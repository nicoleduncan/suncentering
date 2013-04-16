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
rowchord_endpoints = FLTARR(2,N_ELEMENTS(c4xstrips))
colchord_endpoints = FLTARR(2,N_ELEMENTS(c4ystrips))
;   Seeing where the array starts to be greater than the thresh
FOR i = 0,N_ELEMENTS(c4ystrips)-1 DO BEGIN
    col_where = WHERE(c4ystrips[i].ARRAY GT thresh)
    ; beginning of chord
    colchord_endpoints[0,i] = col_where[0]
    ; end of chord
    colchord_endpoints[1,i] = col_where[-1]
ENDFOR

FOR i = 0,N_ELEMENTS(c4xstrips) -1 DO BEGIN
    row_where = WHERE(c4xstrips[i].ARRAY GT thresh)
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

; input_image = FLOAT(input_image)
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


FUNCTION quickfidmask, input_image, thresh
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

input_image = FLOAT(input_image)
a = input_image[SORT(input_image)]
niceimage = a[0:(1-!param.elim_perc/100)*(N_ELEMENTS(a)-1)]
; Eliminating the highest 1% of data
IF thresh eq !null then thresh = !param.reg1thresh*MAX(niceimage)
; IF n_elements(thresh) EQ 0 THEN thresh = 0.25*MAX(image)

s = SIZE(input_image,/dimensions)
n_col = s[0]
n_row = s[1]

suncheck = input_image lt thresh
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
s = SIZE(animage,/dimensions)
length = s[0]
height = s[1]

rowchord_endpoints = FLTARR(2,!param.nstrips)
colchord_endpoints = FLTARR(2,!param.nstrips)

xstrips = REPLICATE({ROWINDEX:0, ARRAY:BYTARR(length), xoffset:struct.xoffset}, !param.nstrips)
ystrips = REPLICATE({COLINDEX:0, ARRAY:BYTARR(height), yoffset:struct.yoffset}, !param.nstrips)

FOR i = 0,!param.nstrips - 1 DO BEGIN
    xstrips[i].ROWINDEX = i
    xstrips[i].ARRAY = animage[*, ROUND(ducks.xpos)+(i-!param.nstrips/2)* !param.scan_width]
    ystrips[i].COLINDEX = i
    ystrips[i].ARRAY = animage[ROUND(ducks.ypos)+(i-!param.nstrips/2)* !param.scan_width,*]
ENDFOR

finish = SYSTIME(1,/seconds)
IF KEYWORD_SET(time) THEN  print,'Elapsed Time for makestrips: ', $
    STRCOMPRESS(finish-start,/rem),' seconds'
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

arr=(FINDGEN(!param.deg_num*!param.res)/!param.res + 90)*!dtor
; only adding 90 so that it starts from 12 o'clock assuming there is
; no dim sun at that location

radius = 129  ; well this is rather arbitrary
r2bit = 2

; The way we have it scanning now is if it doesn't find the aux sun, it scans at a radius interval of 
; 10 so that it looks at the r_orig - interval and r_orig + interval radii. Now, what if the sun isn't there? 

r2 = radius + 20*r2bit              ;20 is an arbitrary number, can be anything, really
x = radius*COS(arr) + mainxpos
y = radius*SIN(arr) + mainypos
x2 = r2*COS(arr)    + mainxpos
y2 = r2*SIN(arr)    + mainypos


loop: BEGIN
    IF !param.file EQ 'dimsun1.fits' THEN radius = BYTE( !param.scan_radius ) 

    ; Have to use .3 instead of .25 for dimsun2, don't know why

    sorted =  wholeimage[sort(wholeimage)]
    thresh = !param.reg2thresh_mult*MAX( sorted[0:(1-!param.elim_perc/100)*(N_ELEMENTS(sorted)-1)] )
    ; ^^
    ; Well this doesn't work.
    
    thresh = !param.reg2thresh_mult*MAX(wholeimage)
    ; Alright, for some reason, clipping out the top 1% changes the thresh from 53.7 to 64.5
    ; which makes the centerx,centery go from 337,76 (correct)
    ; to
    ; 144,19 (so, so wrong)
    ; now, how to deal with it?

    pri_scan = WHERE(wholeimage[x,y] GT thresh,pri_where)
    aux_scan = WHERE(wholeimage[x2,y2] GT thresh,aux_where)

    ; print,aux_where, ' aux_where before if statement'
    IF aux_where NE 0 THEN BEGIN
    ; stop
        in_inner  = ((WHERE(wholeimage[x,y]     GT thresh))[0])/!param.res - !param.circscan_buffer
        out_inner = ((WHERE(wholeimage[x,y]     GT thresh))[-1])/!param.res + !param.circscan_buffer
        in_outer  = ((WHERE(wholeimage[x2,y2]   GT thresh))[0])/!param.res - !param.circscan_buffer
        out_outer = ((WHERE(wholeimage[x2,y2]   GT thresh))[-1])/!param.res + !param.circscan_buffer
    ENDIF ELSE BEGIN
        r2bit*=-1
        GOTO, loop
    ENDELSE
END

otherloop: BEGIN
    IF REGION EQ 3 THEN BEGIN
        thresh = 0.2*MAX(wholeimage) ;dimsun2 works if i set the thresh to .2 instead of .15
        ; The other sun is so dim that weird parts are being picked up. How to fix? Is being dim a problem?

        sorted =  wholeimage[sort(wholeimage)]
        thresh = !param.reg3thresh_mult*MAX( sorted[0:(1-!param.elim_perc/100)*(N_ELEMENTS(sorted)-1)] )
        ; ^^
        ; Well this doesn't work.
    
        thresh = !param.reg3thresh_mult*MAX(wholeimage)

        ; check to make sure we're scanning at the right radius
        n_check = WHERE((wholeimage[x2,y2] GT thresh) EQ 1,n_where)

        IF n_where NE 0 THEN BEGIN
            part1 = wholeimage[x[0:in_inner*!param.res],y[0:in_inner*!param.res]]
            part2 = wholeimage[x[out_inner*!param.res:N_ELEMENTS(x)-1],y[out_inner*!param.res:N_ELEMENTS(x)-1]]
            part1b = wholeimage[x2[0:in_outer*!param.res],y2[0:in_outer*!param.res]]
            part2b = wholeimage[x[out_outer*!param.res:N_ELEMENTS(x)-1],y[out_outer*!param.res:N_ELEMENTS(x)-1]]

            in_inner  = ((WHERE([part1,part2]   gt thresh))[0])/!param.res - !param.circscan_buffer
            out_inner = ((WHERE([part1,part2]   gt thresh))[-1])/!param.res + !param.circscan_buffer
            in_outer  = ((WHERE([part1b,part2b] gt thresh))[0])/!param.res - !param.circscan_buffer
            out_outer = ((WHERE([part1b,part2b] gt thresh))[-1])/!param.res + !param.circscan_buffer

        ENDIF ELSE BEGIN
            r2bit*=-1
            GOTO, otherloop
        ENDELSE

        ; Setting this to 0 actually messes up fitting. use only to show what pixels are being circscanned
        ; wholeimage[x[in_inner:out_inner],y[in_inner:out_inner]] = 0
        ; wholeimage[x2[in_outer:out_outer],y2[in_outer:out_outer]] = 0
    ENDIF
END


centerangle = !dtor*(90 + MEAN([in_inner,out_inner]))
centerx = mainxpos + radius*COS(centerangle)
centery = mainypos + radius*SIN(centerangle)

image = wholeimage[centerx - !param.crop_box:centerx + !param.crop_box,$
    centery - !param.crop_box:centery + !param.crop_box]
xoffset = centerx- !param.crop_box
yoffset = centery- !param.crop_box

finish = SYSTIME(1,/s)
IF KEYWORD_SET(time) THEN print, 'getstruct took: '+STRCOMPRESS(finish-start)+$
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
IF KEYWORD_SET(time) THEN print, 'getstruct took: '+STRCOMPRESS(finish-start)+$
    ' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


function cropme, input, pix

    s = size(input,/d)
    nrow = s[0]
    ncol = s[1]
    output = input[(nrow-1)/2 - pix:(nrow-1)/2 + pix,(ncol-1)/2 - pix:(ncol-1)/2 + pix]

return, output
end

;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************

function bordercheck, input

s = SIZE(input,/dim)
wn_row = s[0]
wn_col = s[1]
datmask = BYTARR(wn_row,wn_col) + 1
datmask[(1/!param.mask_border_perc)*wn_row:(1-(1/!param.mask_border_perc))*wn_row,$
    (1/!param.mask_border_perc)*wn_col:(1-(1/!param.mask_border_perc))*wn_col] = 0

; min_val should be a really low number, the mode of input is 3
min_val = MODE(input)

if TOTAL(datmask*input) gt N_ELEMENTS(datmask[WHERE(datmask eq 1)])*min_val then okaybit=0 else okaybit=1

return, okaybit
end

;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************

function edgefidcheck, input, thresh

bigxpb = SHIFT_DIFF(EMBOSS(input),dir=3) lt thresh
bigypb = SHIFT_DIFF(EMBOSS(input,az=90),dir=1) lt thresh

for p = 1,5 do begin
    tmpcrop = input[10:38+p,8:42]
    xpb = bigxpb[10:38+p,8:42]
    ypb = bigypb[10:38+p,8:42]

    s = size(tmpcrop,/d)
    bordermask = bytarr(s[0],s[1]) + 1
    ; any specific reason we have the border 2 pixels instead of 1?
    bordermask[1:s[0]-2,1:s[1]-2] = 0

    mcrop = bordermask * tmpcrop

    ncol = s[0]
    nrow = s[1]
    ind_col = WHERE(xpb eq 1) mod ncol
    ind_row = WHERE(ypb eq 1)/nrow


    a = MODE(ind_col)
    b = MODE(ind_col[WHERE(ind_col ne a)])

    c = MODE(ind_row)
    d = MODE(ind_row[WHERE(ind_row ne c)])

    ; Just to make it sorted
    xpos = [a,b]
    ypos = [c,d]
    xpos = xpos[SORT(xpos)]
    ypos = ypos[SORT(ypos)]

    xmcrop = bordermask * xpb
    ymcrop = bordermask * ypb

    ind_col = WHERE(xmcrop eq 1) mod ncol
    ind_row = WHERE(ymcrop eq 1)/nrow

    ; if N_ELEMENTS(row_border) eq 1 then row_border = MODE(ind_row)
    ; if N_ELEMENTS(col_border) eq 1 then col_border = MODE(ind_col)

    ; Look at each index, 6 pixels in

    ; If we see a fiducial cut off, either

    ; ignore fiducial
    ; or
    ; crop it out
    ; need to identify whether to use 0:6 or -7:-1
    if WHERE(xmcrop eq 1) eq [-1] then print,'col_slice boo' else begin
        col_slice = FLTARR(N_ELEMENTS(ind_col),nrow)
        for i=0,N_ELEMENTS(ind_col)-1 do begin
            col_slice[i,*] = REFORM(tmpcrop[ind_col[i],*])
            if WHERE(xpb[ind_col[i],*] eq 1) eq [0] then begin
                print,'cropping 0:6'

                ; So this is the part of the program where we need to 
                ; do the smart thing of checking to make sure that the fiducials are being
                ; thresholded correctly, but how do we do that? 

                ; What is somevalue? How do we quantify it?

        ; if N_ELEMENTS(FLOAT(col_slice[i,0:6]) - MODE(tmpcrop) lt somevalue) lt 6 then okaybit=0 else okaybit=1
        ; I think we should use something with derivatives because we know approximately 
        ; how dim the fiducials will get. Instead of replying on pixel values, we rely
        ; on the relative pixel changes which may/may not be more robust

        if N_ELEMENTS(DERIV(DERIV(FLOAT(col_slice[i,0:6])))) gt 0 lt 6 then okb = 0 else okb = 1

        ; This isn't going to work because the threshold of 0 is too high. According to this
        ; current setup, if a fiducial is right on the edge, it'll ALWAYS be bad.

        
        ; I can actually not use parentheses here, is it ok?
        ;Honestly, what's the purpose of doing this "X-Y lt thresh" instead of "X lt thresh"?


        ; The problem is that I'm unable to quantify the fiducials in the way I want
            endif
            if WHERE(xpb[ind_col[i],*] eq 1) eq [-1] then print,'cropping -7:-1'
        endfor
    endelse

    if WHERE(ymcrop eq 1) eq [-1] then print,'row_slice boo' else begin
        row_slice = FLTARR(ncol,N_ELEMENTS(ind_row))
        for i=0,N_ELEMENTS(ind_row)-1 do begin
            row_slice[*,i] = REFORM(tmpcrop[*,ind_row[i]])
            if WHERE(ypb[ind_row[i],*] eq 1) eq [0] then print,'cropping 0:6'
            if WHERE(ypb[ind_row[i],*] eq 1) eq [-1] then print,'cropping -7:-1'
        endfor
    endelse

    ; wait, why is it that if I do something to tmpcrop it wiggs out?
    aa = tmpcrop

    !p.multi=[0,1,3]
    oldcharsize = !p.charsize
    !p.charsize=2
    window,p
    ; ps_start,filename='slice'+strcompress(p,/rem)+'.eps',/encapsulated,/color
    ; range = (float(tmpcrop[11,*]))[0:5]
    range = (FLOAT(tmpcrop[*,20]))[-6:-1]

    plot,range - MODE(aa),psym=-4,title='plain slice from [-6:-1] edge of '+STRCOMPRESS(8,/rem)+':'+$
        STRCOMPRESS(38+p,/rem),xs=3,ys=3
    vline,5-p
    vline,5-p
    plot,DERIV(range),psym=-4,title='1st deriv of slice',xs=3,ys=3
    vline,5-p
    plot,(DERIV(DERIV(range)))[*],psym=-4,title='2nd deriv of slice',xs=3,ys=3
    vline,5-p
    ; ps_end
    !p.multi=0
    !p.charsize=oldcharsize

    ; The right of the vline is where the fiducial is

endfor

; I don't see any patterns... Let's try different patterns. 

; Instead of looking only at edge 6 pixels...?

; What is thi

; cgimage,convol(float(tmpcrop),kernel,/edge_truncate,/center) * (scale_vector(convol(float(tmpcrop),kernel,/edge_truncate,/center)) lt .3),/k

; kernel = [[-.5,1,-.5],[1,1,1],[-.5,1,-.5]]
; kernel = [[0,1,0],[1,1,1],[0,1,0]]
; kernel = [[0,0,1,0,0],[0,0,1,0,0],[1,1,1,1,1],[0,0,1,0,0],[0,0,1,0,0]]
kernel = [[0,0,1,1,0,0],[0,0,1,1,0,0],[1,1,1,1,1,1],[1,1,1,1,1,1],[0,0,1,1,0,0],[0,0,1,1,0,0]]




; fiducial locations
; [24:33,16:27]
; [24:33,0:10]
; [5:15,0:10]
; [5:15,16:27]

first = [24,24,5,5]
second = [33,33,15,15]
third = [16,0,0,16]
fourth = [27,10,10,27]

for hail = 0,3 do begin
    circ = convol(float(tmpcrop),kernel,/edge_truncate,/center)
    crop_circ = circ[first[hail]:second[hail],third[hail]:fourth[hail]]

    ; CONVOL before crop makes the image a little lighter, shape stays the same though

    thresh = .9*max(crop_circ)
    centers=quickfidmask(crop_circ,thresh)
    glorb = tmpcrop[first[hail]:second[hail],third[hail]:fourth[hail]]
    glorb[centers.xpos,*] =.8*max(crop_circ)
    glorb[*,centers.ypos] = .8*max(crop_circ)

    ; omg, we can just convol() it, then find a quickfidmask!
    finefine = INTERPOLATE(crop_circ,FINDGEN((SIZE(crop_circ,/d))[0] *10)/10.,FINDGEN((SIZE(crop_circ,/d))[1] *10)/10.,/grid,cubic=-.5)
    finefine[centers.xpos * 10,*] =.8*max(finefine)
    finefine[*,10*centers.ypos] = .8*max(finefine)

    !p.multi=[0,2,2]
        ; window,20 + hail,xsize=700,ysize=1000
        ps_start,filename='cropcomp'+strcompress(hail,/rem)+'.eps',/encapsulated,/color,xsize=7,ysize=10
        cgimage,glorb,/k
        cgimage,tmpcrop[first[hail]:second[hail],third[hail]:fourth[hail]],/k
        cgimage,finefine,/k,/axes,title='Interpolated circ_crop'
        cgimage,crop_circ,/k,/axes,title='CONVOL() of fiducial'
        ps_end,/png
    !p.multi=0
endfor
; cgimage,glorb,/k,output='glorb.png'
; cgimage,tmpcrop[first[hail]:second[hail],third[hail]:fourth[hail]],/k,output='regularcrop.png'
; cgimage,finefine,/k,/axes,title='Interpolated circ_crop',output='interpcrop.png'
; cgimage,crop_circ,/k,/axes,title='CONVOL() of fiducial',output='convolcrop.png'
; ********
; ********
; So from the result of these plots, we see that using quickfidmask works well for isolated fiducials but not really
; on edge fiducials. 
; ********
; ********

; window,3
; plot,DERIV(float(TS_SMOOTH(reform(range),10) -  range)),psym=-4,title='deriv of ts_smooth(x) - x'
; vline,18
; vline,24
; window,4
; cgimage,SHIFT_DIFF(EMBOSS(tmpcrop),dir=3),/k,title='filter of cropped',/axes
; window,5
; cgimage,(shift_diff(emboss(input),dir=3))[10:43,8:42],/k,title='cropped',/axes
; window,6
; cgimage,SHIFT_DIFF(EMBOSS(input[10:40,8:42],/edge_truncate),dir=3,/edge_truncate),/k,'filter of cropped with edge_truncate',/axes




; So this is actually a good way to do it?
; Not if the fiducial is on the edge, bro.
; if this is okay, then just count how far it is from the edge
stop


return,okaybit
end
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

c = CREATE_STRUCT(var[0],num[0])

;This takes, like, no time.
for i=1,N_ELEMENTS(var)-1 do begin
    c = CREATE_STRUCT(c,var[i],num[i])
endfor

c = CREATE_STRUCT(c,'file','dimsun1.fits')

defsysv,'!param',c

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

crop = wholeimage[struct.center1.xpos-!param.safecrop:struct.center1.xpos+!param.safecrop,$
    struct.center1.ypos-!param.safecrop:struct.center1.ypos+!param.safecrop]
thresh = 0.5*MIN((SHIFT_DIFF(EMBOSS(crop),dir=3)))
borderbit = bordercheck(wholeimage)
edgefidbit = edgefidcheck(crop,thresh)

stop
finish = SYSTIME(1,/s)
IF KEYWORD_SET(time) THEN print, 'merrygotrace took: '+strcompress(finish-start)+$
    ' seconds'
end