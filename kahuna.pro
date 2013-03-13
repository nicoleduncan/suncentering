PRO makelimbstrips, thresh, xstrips, ystrips, region=region, time=time
;+
;   :Description:
;       Makes limb strips from full-length strips
;
;   :Params:
;       thresh : out, required, type=float
;           Threshold used to select pixels
;       xstrips : out, required, type=structure
;           Structure containing row strips
;       ystrips : out, required, type=structure
;           Structure containing column strips
;
;   :Keywords:
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       time : in, optional
;           Prints the elapsed time
;-

; IF n_elements(region) EQ 0 THEN region = 1
IF region EQ !null THEN region = 1

COMMON vblock, wholeimage

; Going through and doing a little commenting, think I forgot how this works:

makestrips, thresh, c4xstrips, c4ystrips, region=region, time=time

start = SYSTIME(1,/seconds)

ministrip_side_buffer = byte(!param.ministrip_length)/2 
; have to byte it since we read the ministrip_length as a float

; Contains coordinates of chord enpoints
rowchord_endpoints = fltarr(2,n_elements(c4xstrips))
colchord_endpoints = fltarr(2,n_elements(c4ystrips))
;   Seeing where the array starts to be greater than the thresh
FOR i = 0,n_elements(c4ystrips)-1 DO BEGIN
    col_where = where(c4ystrips[i].ARRAY GT thresh)
    ; beginning of chord
    colchord_endpoints[0,i] = col_where[0]
    ; end of chord
    colchord_endpoints[1,i] = col_where[-1]
ENDFOR

FOR i = 0,n_elements(c4xstrips) -1 DO BEGIN
    row_where = where(c4xstrips[i].ARRAY GT thresh)
    rowchord_endpoints[0,i] = row_where[0]
    rowchord_endpoints[1,i] = row_where[-1]
ENDFOR

; Preallocating the array, replicating it by the number of strips there are
xstrips = REPLICATE({ROWINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:BYTARR(!param.ministrip_length), $
        ENDPOINTS:BYTARR(!param.ministrip_length), $
        xoffset:c4xstrips.xoffset},N_ELEMENTS(c4xstrips))
ystrips = REPLICATE({COLINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:BYTARR(!param.ministrip_length), $
        ENDPOINTS:BYTARR(!param.ministrip_length), $
        yoffset:c4ystrips.yoffset},N_ELEMENTS(c4ystrips))

;Filling out structure with cut-down strip information
FOR i = 0,N_ELEMENTS(c4xstrips) - 1 DO BEGIN
    xstrips[i].ROWINDEX     = c4xstrips[i].ROWINDEX
    ; If there is no strip that cuts through the sun, set things to 0
    IF rowchord_endpoints[0,i] EQ -1 THEN BEGIN
        xstrips[i].STARTPOINTS  = BYTARR(!param.ministrip_length) 
        xstrips[i].BEGINDEX     = 0
    ENDIF ELSE BEGIN
        ; STARTPOINTS is the cut down strip with length = ministrip_length and contains
        ; the indices from rowchord_endpoints[0,i] +/- ministrip_side_buffer
        xstrips[i].STARTPOINTS  = $
        ; IF chord is too long, it tries to crop from outside of image file
            (c4xstrips[i].ARRAY)[rowchord_endpoints[0,i]-ministrip_side_buffer:$
            rowchord_endpoints[0,i]+ministrip_side_buffer]   
        ; BEGINDEX is the index of the strip where it begins. 
        ; e.g., the array is 5 long, starts from index 9 and is centered around index 11
        xstrips[i].BEGINDEX     = FIX(rowchord_endpoints[0,i] - ministrip_side_buffer)
    ENDELSE

    IF rowchord_endpoints[1,i] EQ -1 THEN BEGIN
        xstrips[i].ENDPOINTS    = BYTARR(!param.ministrip_length)
        xstrips[i].ENDINDEX    = 0
    ENDIF ELSE BEGIN
        xstrips[i].ENDPOINTS  = $
            (c4xstrips[i].ARRAY)[rowchord_endpoints[1,i]-ministrip_side_buffer:$
            rowchord_endpoints[1,i]+ministrip_side_buffer]   
        xstrips[i].ENDINDEX     = FIX(rowchord_endpoints[1,i] - ministrip_side_buffer)
    ENDELSE
ENDFOR


FOR k = 0,N_ELEMENTS(c4ystrips) - 1 DO BEGIN
    ystrips[k].COLINDEX     = c4ystrips[k].COLINDEX
    IF colchord_endpoints[0,k] EQ -1 THEN BEGIN
        ystrips[k].STARTPOINTS  = BYTARR(!param.ministrip_length) 
        ystrips[k].BEGINDEX     = 0
    ENDIF ELSE BEGIN 
        ystrips[k].STARTPOINTS  = (c4ystrips[k].ARRAY)[colchord_endpoints[0,k]- $
            ministrip_side_buffer:colchord_endpoints[0,k]+ministrip_side_buffer]
        ystrips[k].BEGINDEX     = FIX(colchord_endpoints[0,k] - ministrip_side_buffer)
    ENDELSE

    IF colchord_endpoints[1,k] EQ -1 THEN BEGIN
        ystrips[k].ENDPOINTS    = BYTARR(!param.ministrip_length) 
        ystrips[k].ENDINDEX     = 0        
    ENDIF ELSE BEGIN
        ystrips[k].ENDPOINTS    = (c4ystrips[k].ARRAY)[colchord_endpoints[1,k]- $
        ministrip_side_buffer:colchord_endpoints[1,k]+ministrip_side_buffer]
        ystrips[k].ENDINDEX     = FIX(colchord_endpoints[1,k] - ministrip_side_buffer) 
    ENDELSE
ENDFOR


finish = SYSTIME(1,/seconds)

IF KEYWORD_SET(time) THEN  print,'Elapsed Time for makelimbstrips: ', $
    STRCOMPRESS(finish-start,/rem),' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************

 
FUNCTION quickmask, input_image, thresh
;+
;   :Description:
;       Finds center of mask where pixels are above a given threshold
;
;   :Params:
;       input_image : in, required, type=byte
;           2D array of pixels to mask with threshold
;       thresh : in, required, type=float
;           Threshold used to select pixels
;-


a = input_image[SORT(input_image)]
niceimage = a[0:(1-!param.elim_perc/100)*(N_ELEMENTS(a)-1)]
; Eliminating the highest 1% of data
IF thresh eq !null then thresh = !param.reg1thresh*MAX(niceimage)
; IF n_elements(thresh) EQ 0 THEN thresh = 0.25*MAX(image)

s = SIZE(input_image,/dimensions)
n_col = s[0]
n_row = s[1]

suncheck = input_image gt thresh

xpos = TOTAL( TOTAL(suncheck, 2) * INDGEN(n_col) ) / TOTAL(suncheck)
ypos = TOTAL( TOTAL(suncheck, 1) * INDGEN(n_row) ) / TOTAL(suncheck)
RETURN, {xpos:xpos,ypos:ypos}
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


FUNCTION whichcropmethod, region
;+
;   :Description:
;       Crops differently according to which region is selected. 
;
;   :Params:
;       region : in, required, type=integer
;           1) main sun
;           2) 50% brightness sun
;           3) 25% brightness sun
;-
COMMON vblock, wholeimage

; crop_box = BYTE(!param.crop_box)
a = wholeimage[SORT(wholeimage)]
niceimage = a[0:(1-!param.elim_perc/100)*(N_ELEMENTS(a)-1)]

thresh = !param.reg1thresh_mult*max(niceimage)
ducks = quickmask(wholeimage,thresh)

image = wholeimage[ducks.xpos- !param.crop_box:ducks.xpos+ !param.crop_box, $
    ducks.ypos- !param.crop_box:ducks.ypos+ !param.crop_box]

mainxpos = ducks.xpos
mainypos = ducks.ypos
xoffset = ducks.xpos- !param.crop_box
yoffset = ducks.ypos- !param.crop_box

IF REGION NE 1 THEN BEGIN
    circscancrop, mainxpos, mainypos, image, thresh, xpos, ypos, xoffset, yoffset, region=region, time=time
ENDIF

; There is a strong fiducial at image[*,53], but it's not on the limb. It's pretty darn close though. 
; Now, need to replicate those conditions

; plot,image[40,*],/nodata

; i=0
; while get_kbrd(0) EQ '' do BEGIN
; oplot, image[*,i]
; wait,.2
; i++
; ENDWHILE
; plot,image[0:20,53]
; stop

; stop
RETURN,{image:image, xoffset:xoffset, yoffset:yoffset, thresh:thresh}
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


PRO makestrips, thresh, xstrips, ystrips, region=region, time=time
;+
;   :Description:
;       Only saves 5 strips centered around the solar diameter to reduce the amount of limb-
;           darkened pixels and to make the polynomial-fitted limbs more-or-less look similar. 
;
;   :Params:
;   thresh : out, required, type=float
;       Threshold used to select pixels
;   xstrips : out, required, type=structure
;       Structure containing row strips
;   ystrips : out, required, type=structure
;       Structure containing column strips
;
;   :Keywords:
;   region : in, required, type=integer, default=1
;       Which sun out of the three to find the center of. Defaults to the brightest sun
;   time : in, optional
;       Prints elapsed time
;-

; IF n_elements(region) EQ 0 THEN region = 1
IF region eq !null then region = 1

COMMON vblock, wholeimage

struct = whichcropmethod(region)
ducks = quickmask(struct.image)
thresh = struct.thresh

start = SYSTIME(1,/seconds)

animage = struct.image
s = size(animage,/dimensions)
length = s[0]
height = s[1]

rowchord_endpoints = fltarr(2,!param.nstrips)
colchord_endpoints = fltarr(2,!param.nstrips)

xstrips = REPLICATE({ROWINDEX:0, ARRAY:bytarr(length), xoffset:struct.xoffset}, !param.nstrips)
ystrips = REPLICATE({COLINDEX:0, ARRAY:bytarr(height), yoffset:struct.yoffset}, !param.nstrips)

FOR i = 0,!param.nstrips - 1 DO BEGIN
    xstrips[i].ROWINDEX = i
    xstrips[i].ARRAY = animage[*, round(ducks.xpos)+(i-!param.nstrips/2)* !param.scan_width]
    ystrips[i].COLINDEX = i
    ystrips[i].ARRAY = animage[round(ducks.ypos)+(i-!param.nstrips/2)* !param.scan_width,*]
ENDFOR

finish = SYSTIME(1,/seconds)
IF KEYWORD_SET(time) THEN  print,'Elapsed Time for makestrips: ', $
    strcompress(finish-start,/rem),' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


PRO circscancrop, mainxpos, mainypos, image, thresh, xpos, ypos, xoffset, yoffset, region=region, $
     time=time
;+
;   :Description: 
;       Quickly finds the center of the main sun, scans in a circle, and locates the two secondary 
;       suns' centers. Crops either of the secondary suns based on what region specified.
;
;   :Params:
;       mainxpos : in, required
;           X position of 100% brightness sun to scan in a circle around
;       mainypos : in, required
;           Y position of 100% brightness sun to scan in a circle around
;       image : out, required
;           Cropped area
;       thresh : out, required, type=float
;           Threshold used in finding center
;       xpos : out, required, type=float
;           Computed X position of center
;       ypos : out, required, type=float
;           Computed Y position of center
;       xoffset : out, required
;           X offset of cropped region's bottom left corner
;       yoffset : out, required
;           Y offset of cropped region's bottom left corner
;
;   :Keywords:
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       time : in, optional
;           Print the elapsed time
;-

COMPILE_OPT idl2 
ON_ERROR,2

COMMON vblock, wholeimage

start = SYSTIME(1,/s)

res = 10.
arr=(findgen(!param.deg_num*res)/res + 90)*!dtor
; only adding 90 so that it starts from 12 o'clock assuming there is
; no dim sun at that location

radius = 129  ; well this is rather arbitrary
r2bit = 2

; The way we have it scanning now is if it doesn't find the aux sun, it scans at a radius interval of 
; 10 so that it looks at the r_orig - interval and r_orig + interval radii. Now, what if the sun isn't there? 

r2 = radius + 10*r2bit
x = radius*cos(arr) + mainxpos
y = radius*sin(arr) + mainypos
x2 = r2*cos(arr) + mainxpos
y2 = r2*sin(arr) + mainypos


loop: BEGIN
    IF !param.file EQ 'dimsun1.fits' THEN radius = BYTE( !param.scan_radius ) 

    ; r2 = radius + 10*r2bit ;10 is an arbitrary number, can be anything, really

    ; x = radius*cos(arr) + mainxpos
    ; y = radius*sin(arr) + mainypos
    ; x2 = r2*cos(arr) + mainxpos
    ; y2 = r2*sin(arr) + mainypos

; stop
    ; Have to use .3 instead of .25 for dimsun2, don't know why

    sorted =  wholeimage[sort(wholeimage)]
    thresh = !param.reg2thresh_mult*max( sorted[0:(1-!param.elim_perc/100)*(n_elements(sorted)-1)] )
    ; ^^
    ; Well this doesn't work.
    
    thresh = !param.reg2thresh_mult*max(wholeimage)
    ; Alright, for some reason, clipping out the top 1% changes the thresh from 53.7 to 64.5
    ; which makes the centerx,centery go from 337,76 (correct)
    ; to
    ; 144,19 (so, so wrong)
    ; now, how to deal with it?

    pri_scan = where(wholeimage[x,y] GT thresh,pri_where)
    aux_scan = where(wholeimage[x2,y2] GT thresh,aux_where)

    ; print,aux_where, ' aux_where before if statement'
    IF aux_where NE 0 THEN BEGIN
    ; stop
        in_inner  = ((where(wholeimage[x,y]     GT thresh))[0])/res - !param.circscan_buffer
        out_inner = ((where(wholeimage[x,y]     GT thresh))[-1])/res + !param.circscan_buffer
        in_outer  = ((where(wholeimage[x2,y2]   GT thresh))[0])/res - !param.circscan_buffer
        out_outer = ((where(wholeimage[x2,y2]   GT thresh))[-1])/res + !param.circscan_buffer
    ENDIF ELSE BEGIN
        r2bit*=-1
        GOTO, loop
    ENDELSE
END

print,res
print,in_inner
print,out_inner

otherloop: BEGIN
    IF REGION EQ 3 THEN BEGIN
        thresh = 0.2*max(wholeimage) ;dimsun2 works if i set the thresh to .2 instead of .15
        ; The other sun is so dim that weird parts are being picked up. How to fix? Is being dim a problem?


        sorted =  wholeimage[sort(wholeimage)]
        thresh = !param.reg3thresh_mult*max( sorted[0:(1-!param.elim_perc/100)*(n_elements(sorted)-1)] )
        ; ^^
        ; Well this doesn't work.
    
        thresh = !param.reg3thresh_mult*max(wholeimage)

        ; check to make sure we're scanning at the right radius
        n_check = where((wholeimage[x2,y2] GT thresh) EQ 1,n_where)

        IF n_where NE 0 THEN BEGIN

            part1 = wholeimage[x[0:in_inner*res],y[0:in_inner*res]]
            part2 = wholeimage[x[out_inner*res:N_ELEMENTS(x)-1],y[out_inner*res:N_ELEMENTS(x)-1]]
            part1b = wholeimage[x2[0:in_outer*res],y2[0:in_outer*res]]
            part2b = wholeimage[x[out_outer*res:N_ELEMENTS(x)-1],y[out_outer*res:N_ELEMENTS(x)-1]]

            in_inner  = ((where([part1,part2]   gt thresh))[0])/res - !param.circscan_buffer
            out_inner = ((where([part1,part2]   gt thresh))[-1])/res + !param.circscan_buffer
            in_outer  = ((where([part1b,part2b] gt thresh))[0])/res - !param.circscan_buffer
            out_outer = ((where([part1b,part2b] gt thresh))[-1])/res + !param.circscan_buffer

        ENDIF ELSE BEGIN
            r2bit*=-1
            GOTO, otherloop
        ENDELSE

        ; Setting this to 0 actually messes up fitting. use only to show what pixels are being circscanned
        ; wholeimage[x[in_inner:out_inner],y[in_inner:out_inner]] = 0
        ; wholeimage[x2[in_outer:out_outer],y2[in_outer:out_outer]] = 0
    ENDIF
END

; print,in_inner
; print,in_outer
; print,out_inner
; print,out_outer

; stop


centerangle = !dtor*(90 + mean([in_inner,out_inner]))
centerx = mainxpos + radius*cos(centerangle)
centery = mainypos + radius*sin(centerangle)

stop
; crop_box = BYTE(!param.crop_box)

image = wholeimage[centerx - !param.crop_box:centerx + !param.crop_box,$
    centery - !param.crop_box:centery + !param.crop_box]
xoffset = centerx- !param.crop_box
yoffset = centery- !param.crop_box

finish = SYSTIME(1,/s)
IF KEYWORD_SET(time) THEN print, 'getstruct took: '+strcompress(finish-start)+$
    ' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


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
        begroots    = fz_roots(startresult)
        ; Take only roots with no imaginary components
        begusable   = (real_part(begroots))[where(imaginary(begroots) eq 0.)]
        ; Find smallest root (apparently I have to choose the smaller one)
        ; Or i can find the midpoints using the other two roots then take the average of the two,
        ; that way works too, but why would I do that?
        begusable   = (begusable[where(begusable gt 0)])[0]
        stripbeg    = xstrips[n].BEGINDEX + begusable
    ENDIF ELSE BEGIN
        begusable   = 0
        stripbeg    = 0
    ENDELSE

    IF xstrips[n].ENDINDEX GT 0 THEN BEGIN
        endroots    = fz_roots(endresult)
        endusable   = (real_part(endroots))[where(imaginary(endroots) eq 0.)]
        endusable   = (endusable[where(endusable gt 0)])[0]
        stripend    = xstrips[n].ENDINDEX + endusable
    ENDIF ELSE BEGIN
        endusable   = 0
        stripend    = 0
    ENDELSE

    ; Stick the midpoints in an array to take the mean of later
    xlenarr[n] = mean([[stripend],[stripbeg]])
ENDFOR    

FOR n=0,n_elements(ystrips)-1 DO BEGIN
    startresult     = reform(poly_fit(yarr,ystrips[n].STARTPOINTS, !param.order))
    endresult       = reform(poly_fit(yarr,ystrips[n].ENDPOINTS, !param.order))

    startresult[0]  -=thresh
    endresult[0]    -=thresh

    IF ystrips[n].BEGINDEX GT 0 THEN BEGIN
        begroots    = fz_roots(startresult)
        begusable   = (real_part(begroots))[where(imaginary(begroots) eq 0.)]
        begusable   = (begusable[where(begusable gt 0)])[0]
        stripbeg    = ystrips[n].BEGINDEX + begusable
    ENDIF ELSE BEGIN
        begusable   = 0
        stripbeg    = 0
    ENDELSE

    IF ystrips[n].ENDINDEX GT 0 THEN BEGIN
        endroots    = fz_roots(endresult)
        endusable   = (real_part(endroots))[where(imaginary(endroots) eq 0.)]
        endusable   = (endusable[where(endusable gt 0)])[0]
        stripend    = ystrips[n].ENDINDEX + endusable
        
    ENDIF ELSE BEGIN
        endusable   = 0
        stripend    = 0
    ENDELSE

    ylenarr[n] = mean([[stripend],[stripbeg]])
ENDFOR    

; Get the midpoint of the chords
xpos = mean(xlenarr[where(xlenarr ne 0)]) + (xstrips.xoffset)[0]
ypos = mean(ylenarr[where(ylenarr ne 0)]) + (ystrips.yoffset)[0]

IF KEYWORD_SET(plot) THEN BEGIN
    wn = 3
    startresult = poly_fit(xarr,xstrips[wn].STARTPOINTS, !param.order)
    endresult = poly_fit(xarr,xstrips[wn].ENDPOINTS, !param.order)

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

IF KEYWORD_SET(time) THEN  print,'Elapsed Time for limbfit: ',strcompress(finish-start,/rem),' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


PRO getstruct, struct, time=time
;+
;   :Description:
;       Finds the centers of a triple-sun image and loads all relevant information
;       including offsets and angles into a new structure.
;
;   :Params:
;       struct : out, required, type=structure
;           Structure containing the centers and cropped images of all 3 suns
;
;   :Keywords:
;       time: in, optional
;           Outputs how much time the program takes
;-
COMPILE_OPT idl2 
ON_ERROR,2

COMMON vblock, wholeimage

start = SYSTIME(1,/s)


center1 = {center1,xpos:0d,ypos:0d,thresh:0d}
center2 = {center2,xpos:0d,ypos:0d,thresh:0d}
center3 = {center3,xpos:0d,ypos:0d,thresh:0d}

limbfit, thresh, xpos, ypos, plot=plot, region=1, time=time
center1.xpos = xpos
center1.ypos = ypos
center1.thresh = thresh

limbfit, thresh, xpos, ypos, plot=plot, region=2, time=time
center2.xpos = xpos
center2.ypos = ypos
center2.thresh = thresh

limbfit, thresh, xpos, ypos, plot=plot, region=3, time=time
center3.xpos = xpos
center3.ypos = ypos
center3.thresh = thresh


theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
    (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

struct = {KAHUNA, center1:center1, center2:center2, center3:center3, $
    theta:theta, offset:offset}
finish = SYSTIME(1,/s)
IF KEYWORD_SET(time) THEN print, 'getstruct took: '+strcompress(finish-start)+$
    ' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


; docformat = 'rst'
;
;+
; NAME: 
;   KAHUNA
;
; PURPOSE:
;   Finds the center of 3 suns in a single image. Currently limited to a .bmp test image. Instead
;   of scanning rows to crop, scans in a circle. Using solar centers, identifies fiducial positions.
;
; :Author:
;   JEREN SUZUKI::
;
;       Space Sciences Laboratory
;       7 Gauss Way
;       Berkeley, CA 94720 USA
;       E-mail: jsuzuki@ssl.berkeley.edu
;-

; PRO kahuna, file, time=time
;+
;   :Description:
;       This version uses limb fitting opposed to masking (tricenter). 
;
;   :Params:
;
;   :Keywords:
;       time: in, optional
;           Outputs how much time the program takes
;
;   :TODO: 
;       Find and ISOLATE fiducials, not just mask them out
;
;       Ignore center if sun is too close to edge (or if when cropping, we cro outside wholeimage)
;
;       Use 25% of median(image)
;
;       Make sure program doesn't freak out when sun isn't in POV
;       
;-
COMPILE_OPT idl2 
ON_ERROR,2

start=SYSTIME(1,/s)

; profiler,/system
; profiler

; DEATH TO THE COMMON BLOCK (or not)
COMMON vblock, wholeimage
file = 'dimsun1.fits'
readcol,'pblock.txt',var,num,format='A,F',delimiter=' '
    for i=0,N_ELEMENTS(var)-1 do (SCOPE_VARFETCH(var[i],/enter,level=0))=num[i]


p = create_struct(var[0],num[0])

;This takes, like, no time.
for i=0,n_elements(var)-2 do begin
    p = create_struct(p,var[i+1],num[i+1])
endfor

p = create_struct(p,'file','dimsun1.fits')

defsysv,'!param',p


; print,'Parameters:'
; for i=0,N_ELEMENTS(var)-1 do print,var[i],num[i],format='(A,A)'

wholeimage = mrdfits(file)

getstruct, struct, time=time

; profiler,/report,data=data
; profiler,/reset,/clear

; print,data[sort(-data.time)],format='(A-20, I7, F12.5, F10.5, I9)'

print,'Main sun x pos:',struct.center1.xpos
print,'Main sun y pos:',struct.center1.ypos
print,'50% sun x pos: ',struct.center2.xpos
print,'50% sun y pos: ',struct.center2.ypos
print,'25% sun x pos: ',struct.center3.xpos
print,'25% sun y pos: ',struct.center3.ypos

; wholeimage2 = wholeimage
; wholeimage3 = wholeimage

; wholeimage[struct.center1.xpos,*]=20
; wholeimage[*,struct.center1.ypos]=20
; wholeimage2[struct.center2.xpos,*]=20
; wholeimage2[*,struct.center2.ypos]=20
; wholeimage3[struct.center3.xpos,*]=20
; wholeimage3[*,struct.center3.ypos]=20

; ; window,0
; ; cgimage,wholeimage,/k,output=strmid(file,0,7)+'_'+'region1.png'
; ; ; window,2
; ; cgimage,wholeimage2,/k,output=strmid(file,0,7)+'_'+'region2.png'
; ; ; window,3
; ; cgimage,wholeimage3,/k,output=strmid(file,0,7)+'_'+'region3.png'

; window,0
; cgimage,wholeimage,/k
; window,2
; cgimage,wholeimage2,/k
; window,3
; cgimage,wholeimage3,/k

; Here we make the assumption that the darker regions are linearly darker so we can just divide by 2 and 4
; Works pretty well

wholeimage = mrdfits(file)
ideal = BYTSCL( READ_TIFF('plots_tables_images/dimsun_ideal.tiff',channels=1) )
crop = wholeimage[struct.center1.xpos-rad:struct.center1.xpos+rad,$
    struct.center1.ypos-rad:struct.center1.ypos+rad]

icrop = ideal[struct.center1.xpos-rad:struct.center1.xpos+rad,$
    struct.center1.ypos-rad:struct.center1.ypos+rad]

p = icrop[sort(icrop)]
idealthresh = !param.idealthresh_mult*max( p[0:(1-!param.elim_perc/100)*(n_elements(p)-1)] )

imask = icrop lt idealthresh
; dim50 = .5*crop
dim50 = wholeimage[struct.center2.xpos-rad:struct.center2.xpos+rad,$
    struct.center2.ypos-rad:struct.center2.ypos+rad]
; dim25 = .25*crop
dim25 = wholeimage[struct.center3.xpos-rad:struct.center3.xpos+rad,$
    struct.center3.ypos-rad:struct.center3.ypos+rad]

; Actually using the wholeimage cropped areas reveal little difference than with the cheating method
; Doesn't really matter which one we use, practically same result


; thresh was -80, now, how do I do quantify this sorcery?
thresh = 0.5*min((SHIFT_DIFF(EMBOSS(crop),dir=3)))

s = SIZE(crop,/dim)
nrow = s[0]
ncol = s[1]

xpb = (SHIFT_DIFF(EMBOSS(crop),dir=3)) lt thresh
ypb = (SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)) lt thresh

;Don't need to display these anymore (or at least for now)

; display,byte(crop),/square,title='100%'
; plot_edges,xpb,thick=6,setcolor=80
; plot_edges,ypb,thick=6,setcolor=255
; -80 is about 3 stddev() above the minimum
; -80 is also about half the minimum of xpb/ypb

; xpb = (SHIFT_DIFF(EMBOSS(dim50),dir=3)) lt thresh/2
; ypb = (SHIFT_DIFF(EMBOSS(dim50, az=90),dir=1)) lt thresh/2

; ; ps_start,filename='plots_tables_images/dim50.eps',/color,/encapsulated,xsize=8,ysize=8,/inches
; display,byte(dim50),/square,title='50% Dim'
; plot_edges,xpb,thick=6,setcolor=80
; plot_edges,ypb,thick=6,setcolor=255
; ; ps_end,resize=100



; ; so, fixing circscancrop to not set parts of wholeimage to 0 messes up the fid finding here because 
; ; the cropped area now includes a part of a fiducial. fixing.
; xpb = (SHIFT_DIFF(EMBOSS(dim25),dir=3)) lt thresh/4
; ypb = (SHIFT_DIFF(EMBOSS(dim25, az=90),dir=1)) lt thresh/4
; ; ps_start,filename='plots_tables_images/dim25.eps',/color,/encapsulated,xsize=8,ysize=8,/inches       
; display,byte(dim25),/square,title='25% Dim'
; plot_edges,xpb,thick=6,setcolor=80
; plot_edges,ypb,thick=6,setcolor=255
; ; ps_end,resize=100


; ; Working with image of blank sun with real fiducials:
; whitecrop = bytarr(s) + 198  ;198 is the mode of the not-fiducial-maskt
; fakesun = imask*crop + whitecrop*(icrop gt idealthresh)
; ; cgsurface,(SHIFT_DIFF(EMBOSS(fakesun),dir=3))
; a = SHIFT_DIFF(EMBOSS(fakesun),dir=3)
; cgimage,a,/k
; cgimage,a*(a gt 10),/k








; ******************************************************************************************
; ******************************************************************************************
; ******************************************************************************************

wn_row = (size(wholeimage,/dim))[0]
wn_col = (size(wholeimage,/dim))[1]
datmask = bytarr(wn_row,wn_col) + 1
datmask[(1/!param.mask_border_perc)*wn_row:(1-(1/!param.mask_border_perc))*wn_row,$
    (1/!param.mask_border_perc)*wn_col:(1-(1/!param.mask_border_perc))*wn_col] = 0


; min_val should be a really low number, the mode of wholeimage is 3
min_val = mode(wholeimage)
if total(datmask*wholeimage) gt n_elements(datmask[where(datmask eq 1)])*min_val then begin
   ; Don't use this image, bro.
endif

; If in the case where we want the above picture still... then....


; ******************************************************************************************
; ******************************************************************************************
; ******************************************************************************************
; #       #     #     #####         #   #
; #       #    # #    #   #         #   #
; # #   # #   #   #   ####          #   #
; #  # #  #   #####   #   #         #   #
; #   #   #  #     #  #    #        #   #

; Trying to make a complete paraeter table - more intensive than I thought


; Current parameters
; scan_width          5
; sundiam             70
; nstrips             5
; order               2
; ministrip_length    13
; sundiam             70
; rad                 20
; scan_radius         149
; crop_box            60
; reg1thresh_mult     .65


; Ghost parameters (in program but not in pblock.txt)
; These variable names do not exist in the program so don't bother searching for them


; reg1thresh          0.25        ; Thresh used when running quickmask on region 1
; circscan_buffer     10          ; After scanning for aux suns in a circle, how pany pixels to step back/forward
;                                 ; from start/stop
; elim_perc           1           ; How many pixels to eliminate from masks to find maximums for thresholds
; mask_border_perc    10          ; What percentage of width/height should we make a NPZ (no pixel zone) when 
;                                 ; deciding whether or not to use image for sun centering
; idealthresh_mult    .25         ; Multiplier of ideal image maximum
; reg2thresh_mult     .3          ; Multiplier of image maximum for circscancrop region 2
; reg3thresh_mult     .2          ; Multiplier of image maximum for circscancrop region 3
; scanflip            10          ; If no sun is found when running circscancrop, scan at -2*scanflip radius units
; deg_num             360         ; Number of elements in circle array - can be 720 to scan at a finer resolution



; Maybe defining system variables would be better for this... I read in a txt file, make a structure of
; n_elements(), name them like struct.'name' = 'val' or something, then call it !params.scan_width or something

; Cleaner than a common block!

;pasting copy at beginning of code so we can actually use this:
; p = create_struct(var[0],num[0])
; 
; for i=0,n_elements(var)-2 do begin
;     p = create_struct(p,var[i+1],num[i+1])
; endfor
; 
; defsysv,'!param',p

; ******************************************************************************************
; ******************************************************************************************
; ******************************************************************************************

; #       #     #     #####         #   ###
; #       #    # #    #   #         #      #
; # #   # #   #   #   ####          #   ###
; #  # #  #   #####   #   #         #      #
; #   #   #  #     #  #    #        #   ###

; Fiducial cropping, let's get this down

; bordermask = bytarr(nrow,ncol) + 1
; bordermask[(2:nrow-2,2:ncol-2] = 0

; min_val = mode(crop)
; if total(bordermask*crop) gt n_elements(bordermask[where(bordermask eq 1)])*min_val then begin
   
;    ; Look at another 2 pixels in in
;     bordermask = bytarr(nrow,ncol) + 1
;     bordermask[(4:nrow-4,4:ncol-4] = 0
;     if total(bordermask*crop) gt n_elements(bordermask[where(bordermask eq 1)])*min_val then begin
;         new_crop = crop[(2:nrow-2,2:ncol-2]
;     endif else begin
;         new_crop = crop[(4:nrow-4,4:ncol-4]
;     endelse
; endif

; Now, this is direction independent, what if we have good fiducials on one edge but not the other?
; We need to look at each edge independently

big = mrdfits(file)
p_crop = big[struct.center1.xpos-rad:struct.center1.xpos+rad,$
    struct.center1.ypos-rad:struct.center1.ypos+rad]


;Not sure why, but the 2d arrays are turning into array[*]

leftedge    = p_crop[0:2,*]
topedge     = p_crop[*,nrow-3:nrow-1]
rightedge   = p_crop[ncol-3:ncol-1,*]
botedge     = p_crop[*,0:2]

if (total(leftedge) gt n_elements(leftedge)*mode(p_crop)) then begin
    ; another 2 pix p_crop check
    if total(p_crop[0:5,*]) gt n_elements(p_crop[0:5,*])*mode(p_crop) then p_cropleft=1 else p_cropleft=2
    ; newleftedge = p_crop[0:2,*]
    ; endif else begin
    ;     newleftedge = p_crop[0:5,*]
    ; endelse
endif else p_cropleft=0

; Now do I do this for all sides?

if (total(topedge) gt n_elements(topedge)*mode(p_crop)) then begin
    if total(p_crop[*,nrow-5:nrow-1]) gt n_elements(p_crop[*,nrow-5:nrow-1])*mode(p_crop) then $
        p_croptop=1 else p_croptop=2
endif else p_croptop=0

if (total(rightedge) gt n_elements(rightedge)*mode(p_crop)) then begin
    if total(p_crop[ncol-5:ncol-1,*]) gt n_elements(p_crop[ncol-5:ncol-1,*])*mode(p_crop) then $
        p_cropright=1 else p_cropright=2
endif else p_cropright=0

if (total(botedge) gt n_elements(botedge)*mode(p_crop)) then begin
    if total(p_crop[*,0:5]) gt n_elements(p_crop[*,0:5])*mode(p_crop) then p_cropbot=1 else p_cropbot=2
endif else p_cropbot=0



newp_crop = p_crop[p_cropleft*3:ncol-1-p_cropright*3,p_cropbot*3:nrow-1-p_croptop*3]

; ******************************************************************************************
; ******************************************************************************************
; ******************************************************************************************

stop

; Dickin' around with convol()
kernel = [[-1,1,-1],[1,1,1],[-1,1,-1]]
cgimage,convol(crop,kernel),output='kerneltest.png',/k

stop

; Testing out with diagonals
wholeimage = BYTSCL( READ_TIFF('plots_tables_images/diag.tiff',channels=1) )
crop = wholeimage[struct.center1.xpos-rad:struct.center1.xpos+rad,$
    struct.center1.ypos-rad:struct.center1.ypos+rad]
cgimage,emboss(crop,az=45),/k



; Big ass kernel is not good
kernel = [[1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1],$
        [-1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1],$
        [-1,-1,1,-1,-1,-1,-1,-1,-1,1,-1,-1],$
        [-1,-1,-1,1,-1,-1,-1,-1,1,-1,-1,-1],$
        [-1,-1,-1,-1,1,-1,-1,1,-1,-1,-1,-1],$
        [-1,-1,-1,-1,-1,1,1,-1,-1,-1,-1,-1],$
        [-1,-1,-1,-1,-1,1,1,-1,-1,-1,-1,-1],$
        [-1,-1,-1,-1,1,-1,-1,1,-1,-1,-1,-1],$
        [-1,-1,-1,1,-1,-1,-1,-1,1,-1,-1,-1],$
        [-1,-1,1,-1,-1,-1,-1,-1,-1,1,-1,-1],$
        [-1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1],$
        [1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1]]
; ******************************************************************************************
; ******************************************************************************************
; ******************************************************************************************

stop
; The sunthetic image has too-nice edges that they end up being edge-detected 
; So I actually didn't anticipate this.

; window,0
; !p.multi=[0,2,1]
; cgimage,xpb*crop,/k
; cgimage,ypb*crop,/k
; !p.multi=0

ind_col = WHERE(xpb eq 1) mod ncol
ind_row = WHERE(ypb eq 1)/nrow


a = MODE(ind_col)
b = MODE(ind_col[WHERE(ind_col ne a)])

c = MODE(ind_row)
d = MODE(ind_row[WHERE(ind_row ne f)])



; Just to make it sorted
xpos = [a,b]
ypos = [c,d]
xpos = xpos[SORT(xpos)]
ypos = ypos[SORT(ypos)]

; Because fiducials are 2 pixels wide 
xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1]]
ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1]]

finish = SYSTIME(1,/s)
IF KEYWORD_SET(time) THEN print, 'merrygotrace took: '+strcompress(finish-start)+$
    ' seconds'

end