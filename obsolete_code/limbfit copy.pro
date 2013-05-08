PRO limbfit, thresh, xpos, ypos, plot=plot, region=region, time=time
;+
;   :Description:
;       Uses the data from makelimbstrips and fits an n-th order polynomial to the limb to find where
;       it crosses the threshold.
;
;   :Params:
;       thresh : out, required, type=float
;           Threshold used to select pixels
;       xpos : out, required, type=float
;           X center
;       ypos : out, required, type=float
;           Y center
;
;   :Keywords:
;       region : in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       plot : in, optional
;           Makes some nice plots
;       time : in, optional
;           Prints the elapsed time
;-

if region eq !null then region = 1

COMMON vblock, wholeimage

; Run the program to get our structures
makelimbstrips, thresh, xstrips, ystrips, region=region, time=time

start = SYSTIME(1,/seconds)

xlen    = 0
xsum    = 0
xnum    = 0   
ylen    = 0
ysum    = 0
ynum    = 0
xarr    = FINDGEN(N_ELEMENTS(xstrips[4].STARTPOINTS))
yarr    = FINDGEN(N_ELEMENTS(ystrips[4].STARTPOINTS))
tx      = FINDGEN(N_ELEMENTS(xstrips[4].STARTPOINTS) * 1000)/100
ylenarr = FINDGEN(N_ELEMENTS(ystrips))
xlenarr = FINDGEN(N_ELEMENTS(xstrips))

;Deal with rows
FOR n=0,N_ELEMENTS(xstrips)-1 DO BEGIN
    ; Using fz_roots instead of SPLINE interpolating. Saving lines and making code more readable

    startresult     = REFORM(POLY_FIT(xarr,xstrips[n].STARTPOINTS, !param.order))
    endresult       = REFORM(POLY_FIT(xarr,xstrips[n].ENDPOINTS, !param.order))

    ; Solving for roots but want to include threshold value
    startresult[0]  -=thresh
    endresult[0]    -=thresh

    IF xstrips[n].BEGINDEX GT 0 THEN BEGIN
        ; Get roots (complex)
        begroots    = FZ_ROOTS(startresult)
        ; Take only roots with no imaginary components
        begusable   = (REAL_PART(begroots))[WHERE(IMAGINARY(begroots) eq 0.)]
        ; Find smallest root (apparently I have to choose the smaller one)
        ; Or i can find the midpoints using the other two roots then take the average of the two,
        ; that way works too, but why would I do that?
        begusable   = (begusable[WHERE(begusable gt 0)])[0]
        stripbeg    = xstrips[n].BEGINDEX + begusable
    ENDIF ELSE BEGIN
        begusable   = 0
        stripbeg    = 0
    ENDELSE

    IF xstrips[n].ENDINDEX GT 0 THEN BEGIN
        endroots    = FZ_ROOTS(endresult)
        endusable   = (REAL_PART(endroots))[WHERE(IMAGINARY(endroots) eq 0.)]
        endusable   = (endusable[WHERE(endusable gt 0)])[0]
        stripend    = xstrips[n].ENDINDEX + endusable
    ENDIF ELSE BEGIN
        endusable   = 0
        stripend    = 0
    ENDELSE

    ; Stick the midpoints in an array to take the mean of later
    xlenarr[n] = MEAN([[stripend],[stripbeg]])
ENDFOR    

FOR n=0,N_ELEMENTS(ystrips)-1 DO BEGIN
    startresult     = REFORM(POLY_FIT(yarr,ystrips[n].STARTPOINTS, !param.order))
    endresult       = REFORM(POLY_FIT(yarr,ystrips[n].ENDPOINTS, !param.order))

    startresult[0]  -=thresh
    endresult[0]    -=thresh

    IF ystrips[n].BEGINDEX GT 0 THEN BEGIN
        begroots    = FZ_ROOTS(startresult)
        begusable   = (REAL_PART(begroots))[WHERE(IMAGINARY(begroots) eq 0.)]
        begusable   = (begusable[WHERE(begusable gt 0)])[0]
        stripbeg    = ystrips[n].BEGINDEX + begusable
    ENDIF ELSE BEGIN
        begusable   = 0
        stripbeg    = 0
    ENDELSE

    IF ystrips[n].ENDINDEX GT 0 THEN BEGIN
        endroots    = FZ_ROOTS(endresult)
        endusable   = (REAL_PART(endroots))[WHERE(IMAGINARY(endroots) eq 0.)]
        endusable   = (endusable[WHERE(endusable gt 0)])[0]
        stripend    = ystrips[n].ENDINDEX + endusable
        
    ENDIF ELSE BEGIN
        endusable   = 0
        stripend    = 0
    ENDELSE

    ylenarr[n] = MEAN([[stripend],[stripbeg]])
ENDFOR    

; Get the midpoint of the chords
xpos = MEAN(xlenarr[WHERE(xlenarr ne 0)]) + (xstrips.xoffset)[0]
ypos = MEAN(ylenarr[WHERE(ylenarr ne 0)]) + (ystrips.yoffset)[0]

IF KEYWORD_SET(plot) THEN BEGIN
    wn = 3
    startresult = POLY_FIT(xarr,xstrips[wn].STARTPOINTS, !param.order)
    endresult = POLY_FIT(xarr,xstrips[wn].ENDPOINTS, !param.order)

    CASE !param.order OF
    1: BEGIN
        xtmp = SPLINE(xarr,startresult[0] + startresult[1]*xarr,tx)
        atmp = SPLINE(xarr,endresult[0] + endresult[1]*xarr,tx)
        END
    2: BEGIN
        xtmp = SPLINE(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2,tx)
        atmp = SPLINE(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2,tx)
        END
    3: BEGIN
        xtmp = SPLINE(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3,tx)
        atmp = SPLINE(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3,tx)
        END
    4: BEGIN
        xtmp = SPLINE(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4,tx)
        atmp = SPLINE(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4,tx)
        END
    5: BEGIN
        xtmp = SPLINE(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5,tx)
        atmp = SPLINE(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5,tx)
        END    
    6: BEGIN
        xtmp = SPLINE(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
                startresult[6]*xarr^6,tx)
        atmp = SPLINE(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
                endresult[6]*xarr^6,tx)
        END
    7: BEGIN
        xtmp = SPLINE(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
                startresult[6]*xarr^6 + startresult[7]*xarr^7,tx)
        atmp = SPLINE(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
                endresult[6]*xarr^6 + endresult[7]*xarr^7,tx)
        END
    ENDCASE

    ; A pretty plot for Nicole
    window,2
    plot,xarr+xstrips[wn].BEGINDEX,xstrips[wn].startpoints,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].BEGINDEX,xtmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','SPLINEd Data'],linestyle=[0,1],/bottom,/right,charsize=2

    window,0
    plot,xarr+xstrips[wn].ENDINDEX,xstrips[wn].ENDPOINTS,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].ENDINDEX,atmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','SPLINEd Data'],linestyle=[0,1],/bottom,/left,charsize=2
ENDIF

finish = SYSTIME(1,/seconds)

IF KEYWORD_SET(time) THEN  print,'Elapsed Time for limbfit: ',STRCOMPRESS(finish-start,/rem),' seconds'
RETURN
END