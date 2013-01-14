PRO makelimbstrips, thresh, xstrips, ystrips, file, ministrip_length, scan_width, $
    sundiam, nstrips=nstrips, region=region, time=time
;+
;   :Description:
;       Makes limb strips from full-length strips
;
;   :Params:
;       file: in, required, type=string, default='triplesun.bmp'
;           File to be read in
;       ministrip_length: in, required, type=byte, default=13
;           How long the total array of the cut-down strip will be
;       scan_width: in, required, type=integer, default=5
;           Indicates how far apart to scan
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;       thresh : out, required, type=float
;           Threshold used to select pixels
;
;   :Keywords:
;       nstrips : in, optional, type=byte, default=5
;           How many strips to select, centered around the row/col diameter
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       time : in, optional
;           Prints the elapsed time
;
;   :TODO:
;
;   Exactly how much data should be stored in a structure? Since we're interested in saving space,
;   doesn't make sense to repeat any data in the structures.
;-

IF n_elements(file)             EQ 0    THEN file               = 'dimsun1.fits'
IF n_elements(ministrip_length) EQ 0    THEN ministrip_length   = 13
IF n_elements(nstrips)          EQ 0    THEN nstrips            = 5 
IF n_elements(scan_width)       EQ 0    THEN scan_width         = 5 
IF n_elements(sundiam)          EQ 0    THEN sundiam            = 70
IF n_elements(region)           EQ 0    THEN region             = 1

; Going through and doing a little commenting, think I forgot how this works:


makestrips, thresh, c4xstrips, c4ystrips, file, scan_width, sundiam, nstrips=nstrips, $
    region=region, time=time

start = systime(1,/seconds)

ministrip_side_buffer = ministrip_length/2 
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
        STARTPOINTS:bytarr(ministrip_length), $
        ENDPOINTS:bytarr(ministrip_length), $
        xoffset:c4xstrips.xoffset},n_elements(c4xstrips))
ystrips = REPLICATE({COLINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:bytarr(ministrip_length), $
        ENDPOINTS:bytarr(ministrip_length), $
        yoffset:c4ystrips.yoffset},n_elements(c4ystrips))

;Filling out structure with cut-down strip information
FOR i = 0,n_elements(c4xstrips) - 1 DO BEGIN
    xstrips[i].ROWINDEX     = c4xstrips[i].ROWINDEX
    ; If there is no strip that cuts through the sun, set things to 0
    IF rowchord_endpoints[0,i] EQ -1 THEN BEGIN
        xstrips[i].STARTPOINTS  = fltarr(ministrip_length) 
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
        xstrips[i].BEGINDEX     = fix(rowchord_endpoints[0,i] - ministrip_side_buffer)
    ENDELSE
    IF rowchord_endpoints[1,i] EQ -1 THEN BEGIN
        xstrips[i].ENDPOINTS    = fltarr(ministrip_length)
        xstrips[i].ENDINDEX    = 0
    ENDIF ELSE BEGIN
        xstrips[i].ENDPOINTS  = $
            (c4xstrips[i].ARRAY)[rowchord_endpoints[1,i]-ministrip_side_buffer:$
            rowchord_endpoints[1,i]+ministrip_side_buffer]   
        xstrips[i].ENDINDEX     = fix(rowchord_endpoints[1,i] - ministrip_side_buffer)
    ENDELSE
ENDFOR


FOR k = 0,n_elements(c4ystrips) - 1 DO BEGIN
    ystrips[k].COLINDEX     = c4ystrips[k].COLINDEX
    IF colchord_endpoints[0,k] EQ -1 THEN BEGIN
        ystrips[k].STARTPOINTS  = fltarr(ministrip_length) 
        ystrips[k].BEGINDEX     = 0
    ENDIF ELSE BEGIN 
        ystrips[k].STARTPOINTS  = (c4ystrips[k].ARRAY)[colchord_endpoints[0,k]- $
            ministrip_side_buffer:colchord_endpoints[0,k]+ministrip_side_buffer]
        ystrips[k].BEGINDEX     = fix(colchord_endpoints[0,k] - ministrip_side_buffer)
    ENDELSE
    IF colchord_endpoints[1,k] EQ -1 THEN BEGIN
        ystrips[k].ENDPOINTS    = fltarr(ministrip_length) 
        ystrips[k].ENDINDEX     = 0        
    ENDIF ELSE BEGIN

        ystrips[k].ENDPOINTS    = (c4ystrips[k].ARRAY)[colchord_endpoints[1,k]- $
        ministrip_side_buffer:colchord_endpoints[1,k]+ministrip_side_buffer]
        ystrips[k].ENDINDEX     = fix(colchord_endpoints[1,k] - ministrip_side_buffer) 
    ENDELSE
ENDFOR



finish = systime(1,/seconds)

IF keyword_set(time) THEN  print,'Elapsed Time for makelimbstrips: ', $
    strcompress(finish-start,/rem),' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************

 
FUNCTION quickmask, image, thresh

IF N_ELEMENTS(thresh) EQ 0 THEN THRESH = 0.25*max(image)

s = size(image,/dimensions)
n_col = s[0]
n_row = s[1]

suncheck = image gt thresh

xpos = TOTAL( TOTAL(suncheck, 2) * Indgen(n_col) ) / total(suncheck)
ypos = TOTAL( TOTAL(suncheck, 1) * Indgen(n_row) ) / total(suncheck)
RETURN, {xpos:xpos,ypos:ypos}
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


FUNCTION whichcropmethod, file, region, scan_width, sundiam, thresh

COMMON vblock, wholeimage

thresh = 0.65*max(wholeimage)
ducks = quickmask(wholeimage,thresh)

image = wholeimage[ducks.xpos-60:ducks.xpos+60,ducks.ypos-60:ducks.ypos+60]

mainxpos = ducks.xpos
mainypos = ducks.ypos
xoffset = ducks.xpos-60
yoffset = ducks.ypos-60

IF REGION NE 1 THEN BEGIN
    circscancrop, mainxpos, mainypos, file, image, xpos, ypos, xoffset, yoffset, scan_width, $
    sundiam, thresh, region=region, time=time
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

; RETURN, {xpos:xpos, ypos:ypos, image:image, xoffset:xoffset, yoffset:yoffset}
RETURN,{image:image, xoffset:xoffset, yoffset:yoffset}
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


PRO makestrips, thresh, xstrips, ystrips, file, scan_width, sundiam, nstrips=nstrips, $
    region=region, time=time
;+
;   :Description:
;       Only saves 5 strips centered around the solar diameter to reduce the amount of limb-
;           darkened pixels and to make the polynomial-fitted limbs more-or-less look similar. 
;
;   :Params:
;   file: in, required, type=string, default='triplesun.bmp'
;       File to be read in
;   scan_width: in, required, type=integer, default=5
;       Indicates how far apart to scan
;   sundiam : in, required, type=byte, default=70
;       Approximate diameter of sun in pixels. (Based on bmp image)
;   thresh : out, required, type=float
;       Threshold used to select pixels
;   xstrips : out, required, type=structure
;       Structure containing row strips
;   ystrips : out, required, type=structure
;       Structure containing column strips
;
;   :Keywords:
;   nstrips : in, optional, type=byte, default=5
;       How many strips to select, centered around the row/col diameter
;   region : in, required, type=integer, default=1
;       Which sun out of the three to find the center of. Defaults to the brightest sun
;   time : in, optional
;       Prints elapsed time
;-

IF n_elements(file)         EQ 0    THEN file       = 'dimsun1.fits'
IF n_elements(nstrips)      EQ 0    THEN nstrips    = 5
IF n_elements(region)       EQ 0    THEN region     = 1
IF n_elements(scan_width)   EQ 0    THEN scan_width = 5
IF n_elements(sundiam)      EQ 0    THEN sundiam    = 70

struct = whichcropmethod(file, region, scan_width, sundiam, thresh)

ducks = quickmask(struct.image)

start = systime(1,/seconds)

wholeimage = struct.image
s = size(wholeimage,/dimensions)
length = s[0]
height = s[1]

rowchord_endpoints = fltarr(2,nstrips)
colchord_endpoints = fltarr(2,nstrips)

xstrips = REPLICATE({ROWINDEX:0,ARRAY:bytarr(length),xoffset:struct.xoffset},nstrips)
ystrips = REPLICATE({COLINDEX:0,ARRAY:bytarr(height),yoffset:struct.yoffset},nstrips)


FOR i = 0,nstrips - 1 DO BEGIN
    xstrips[i].ROWINDEX = i
    xstrips[i].ARRAY = wholeimage[*, round(ducks.xpos)+(i-nstrips/2)*scan_width]
ENDFOR

FOR k = 0,nstrips - 1 DO BEGIN
    ystrips[k].COLINDEX = k
    ystrips[k].ARRAY = wholeimage[round(ducks.ypos)+(k-nstrips/2)*scan_width,*]
ENDFOR

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print,'Elapsed Time for makestrips: ', $
    strcompress(finish-start,/rem),' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


PRO circscancrop, mainxpos, mainypos, file, image, xpos, ypos, xoffset, yoffset, scan_width, $
    sundiam, thresh, region=region, time=time
;+
;   :Description: 
;       Quickly finds the center of the main sun, scans in a circle, and locates the two secondary 
;       suns' centers. Crops either of the secondary suns based on what region specified.
;
;   :Params:
;       file: in, required, type='string', default='triplesun.bmp'
;           What file to load in
;       xpos : out, required, type=float
;           Computed X position of center
;       ypos : out, required, type=float
;           Computed Y position of center
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sundiam: in, required, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;       thresh : out, required, type=float
;           Threshold used in finding center
;
;   :Keywords:
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       time : in, optional
;           Print the elapsed time
;-

COMPILE_OPT idl2 
on_error,2

COMMON vblock,wholeimage

start = systime(1,/s)

arr=(findgen(360) + 90)*!dtor
; only adding 90 so that it starts from 12 o'clock assuming there is
; no dim sun at that location

radius = 139
r2bit = 2
loop: BEGIN
    IF file EQ 'dimsun1.fits' THEN radius = 149
    ; print,r2bit,' Beginning r2bit'
    ; print,region
    r2 = radius + 10*r2bit ;10 is an arbitrary number, can be anything, really

    x = radius*cos(arr) + mainxpos
    y = radius*sin(arr) + mainypos
    x2 = r2*cos(arr) + mainxpos
    y2 = r2*sin(arr) + mainypos

    ; Have to use .3 instead of .25 for dimsun2, don't know why
    thresh = 0.3*max(wholeimage)

    pri_scan = where((wholeimage[x,y] GT thresh) EQ 1,pri_where)
    aux_scan = where((wholeimage[x2,y2] GT thresh) EQ 1,aux_where)
    ; print,aux_where, ' aux_where before if statement'
    IF aux_where NE 0 THEN BEGIN
        in_inner  = (where((wholeimage[x,y]     GT thresh) EQ 1))[0] - 10
        in_outer  = (where((wholeimage[x2,y2]   GT thresh) EQ 1))[0] - 10
        out_inner = (where((wholeimage[x,y]     GT thresh) EQ 1))[-1] + 10
        out_outer = (where((wholeimage[x2,y2]   GT thresh) EQ 1))[-1] + 10
    ENDIF ELSE BEGIN
        ; print,r2bit,aux_where
        r2bit*=-1
        ; print,r2bit
        GOTO, loop
    ENDELSE
END
r2bit = 2
r2 = radius + 10*r2bit
x2 = r2*cos(arr) + mainxpos
y2 = r2*sin(arr) + mainypos

otherloop: BEGIN
    IF REGION EQ 3 THEN BEGIN
    print,region,' Region'
        thresh = 0.2*max(wholeimage) ;dimsun2 works if i set the thresh to .2 instead of .15
        ; The other sun is so dim that weird parts are being picked up. How to fix? Is being dim a problem?

        wholeimage[x[in_inner:out_inner],y[in_inner:out_inner]] = 0
        wholeimage[x2[in_outer:out_outer],y2[in_outer:out_outer]] = 0
        
        ; check to make sure we're scanning at the right radius
        n_check = where((wholeimage[x2,y2] GT thresh) EQ 1,n_where)

        IF n_where NE 0 THEN BEGIN
            in_inner  = (where((wholeimage[x,y]     GT thresh) EQ 1))[0] - 10
            in_outer  = (where((wholeimage[x2,y2]   GT thresh) EQ 1))[0] - 10
            out_inner = (where((wholeimage[x,y]     GT thresh) EQ 1))[-1] + 10
            out_outer = (where((wholeimage[x2,y2]   GT thresh) EQ 1))[-1] + 10
        ENDIF ELSE BEGIN
            ; print,r2bit,n_where
            ; stop
            r2bit*=-1
            GOTO, otherloop
        ENDELSE

        wholeimage[x[in_inner:out_inner],y[in_inner:out_inner]] = 0
        wholeimage[x2[in_outer:out_outer],y2[in_outer:out_outer]] = 0
    ENDIF

END
centerangle = !dtor*(90 + mean([in_inner,out_inner]))
centerx = mainxpos + radius*cos(centerangle)
centery = mainypos + radius*sin(centerangle)

image = wholeimage[centerx-60:centerx+60,centery-60:centery+60]
xoffset = centerx-60
yoffset = centery-60

finish = systime(1,/s)
IF keyword_set(time) THEN print, 'getstruct took: '+strcompress(finish-start)+$
    ' seconds'
RETURN
END
;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


FUNCTION mainsuncrop, file, scan_width, sundiam, time=time
IF n_elements(file)         EQ 0 THEN file        = 'dimsun1.fits'
IF n_elements(scanwidth)    EQ 0 THEN scan_width  = 5
IF n_elements(sundiam)      EQ 0 THEN sundiam     = 70 ;at it's widest, sun is 61 pixels across

COMPILE_OPT idl2 
on_error,2
COMMON vblock, wholeimage

s = size(wholeimage,/dimensions)
n_col = s[0]
n_row = s[1]

thresh = 0.65*max(wholeimage)
suncheck = wholeimage gt thresh

xpos = TOTAL( TOTAL(suncheck, 2) * Indgen(n_col) ) / total(suncheck)
ypos = TOTAL( TOTAL(suncheck, 1) * Indgen(n_row) ) / total(suncheck)

c2 = wholeimage[xpos-60:xpos+60,ypos-60:ypos+60]
location = {image:c2,xoffset:xpos-60,yoffset:ypos-60}
print,'xpos',xpos
print,'ypos',ypos
finish = systime(1,/seconds)
IF keyword_set(time) THEN  print, 'Elapsed Time for mainsuncrop(): ' + $
    strcompress(finish-start,/remove)+ ' seconds'
RETURN, location

END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


PRO merrygotrimask, image, file, xpos, ypos, xoffset, yoffset, scan_width, sundiam, thresh, $
    time=time
;+
;   :Description:
;       Had to make a new version of comp3 because the old one called scanbox() by default
;
;   :Params:
;       image: out, required, type=array
;           Cropped image
;       file: in, required, type='string', default='triplesun.bmp'
;           What file to load in
;       xpos : out, required, type=float
;           Computed X position of center
;       ypos : out, required, type=float
;           Computed Y position of center
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sundiam: in, required, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;       thresh : out, required, type=float
;           Threshold used in finding center
;
;   :Keywords:
;       time : in, optional
;           Print the elapsed time
;-
COMPILE_OPT idl2 
on_error,2

IF n_elements(file)         EQ 0 THEN file   = 'dimsun1.fits'

; COMMON vblock,wholeimage
struct = mainsuncrop(file, scan_width, sundiam, time=time)
image = struct.image
start = systime(1,/seconds)

ducks = quickmask(image)
xpos = ducks.xpos
ypos = ducks.ypos

xoffset = struct.xoffset
yoffset = struct.yoffset
stop
finish = systime(1,/s)
IF keyword_set(time) THEN  print, 'Elapsed Time for merrygotrimask: ',strcompress(finish-start,/remove),$
    ' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


PRO limbfit, thresh, xpos, ypos, file, ministrip_length, order, scan_width, sundiam, $
    nstrips=nstrips, plot=plot, region=region, time=time
;+
;   :Description:
;   Uses the data from makelimbstrips and fits an n-th order polynomial to the limb to find where
;       it crosses the threshold.
;
;   :Params:
;       file : in, required, type=string, default='triplesun.bmp'
;           File to be read in
;       ministrip_length : in, required, default=9
;           How long the trimmed down strip will be
;       order : in, required, type=integer, default=3
;           What order polynomial to use for POLY_FIT()
;       scan_width : in, required, type=integer, default=5
;           Indicates how far apart to scan
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;       thresh : out, required, type=float
;           Threshold used to select pixels
;       xpos : out, required, type=float
;           X center
;       ypos : out, required, type=float
;           Y center
;
;   :Keywords:
;       nstrips : in, optional, type=byte, default=5
;           How many strips to select, centered around the row/col diameter
;       region : in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       plot : in, optional
;           Makes some nice plots
;       time : in, optional
;           Prints the elapsed time
;-

; Setting default values
IF n_elements(file)                 EQ 0    THEN file              = 'dimsun1.fits'
IF n_elements(ministrip_length)     EQ 0    THEN ministrip_length  = 13
IF n_elements(nstrips)              EQ 0    THEN nstrips           = 5
IF n_elements(order)                EQ 0    THEN order             = 2
IF n_elements(region)               EQ 0    THEN region            = 1
IF n_elements(scan_width)           EQ 0    THEN scan_width        = 5
IF n_elements(sundiam)              EQ 0    THEN sundiam           = 70
; COMMON vblock,image
; Run the program to get our structures
makelimbstrips, thresh, xstrips, ystrips, file, ministrip_length, scan_width, sundiam, $
    nstrips=nstrips, region=region, time=time

start = systime(1,/seconds)

ministrip_side_length = ministrip_length/2
xlen    = 0
xsum    = 0
xnum    = 0   
ylen    = 0
ysum    = 0
ynum    = 0
xarr    = findgen(n_elements(xstrips[4].STARTPOINTS))
yarr    = findgen(n_elements(ystrips[4].STARTPOINTS))
tx      = findgen(n_elements(xstrips[4].STARTPOINTS) * 1000)/100
ylenarr = findgen(n_elements(ystrips))
xlenarr = findgen(n_elements(xstrips))

;Deal with rows
FOR n=0,n_elements(xstrips)-1 DO BEGIN
    ; Using fz_roots instead of spline interpolating. Saving lines and making code more readable


; ; Since there are no limb profiles with fiducials, I'll just make one up

    ; a = float((xstrips[n].STARTPOINTS))
    ; b=a
    ; (a[3])*=.9
    ; (a[4])*=.8
    ; (a[5])*=.45
    ; (a[6])*=.5
    ; (a[7])*=.7
    ; (a[8])*=.9

    ; startresult     = reform(poly_fit(xarr,a,order))
    ; corrstartresult = reform(poly_fit(xarr,b,order))

    ; xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2,tx)
    ; corrxtmp = spline(xarr,corrstartresult[0] + corrstartresult[1]*xarr + corrstartresult[2]*xarr^2,tx)
    ; ; atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2,tx)
    
    ; plot,xarr+xstrips[n].BEGINDEX,a,xs=3,ys=3,title='Limb Profile',$
    ;     xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    ; oplot,tx+xstrips[n].BEGINDEX,xtmp,linestyle=1
    ; oplot,xarr+xstrips[n].BEGINDEX,b,linestyle=4
    ; oplot,tx+xstrips[n].BEGINDEX,corrxtmp,linestyle=5
    ; hline,thresh,linestyle=2
    ; legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/right,charsize=2

;     ; Somewhat hard to make poly_fit fuck up when limbfitting even when I'm forcing fiducials
;     ; Perhaps I don't know what they actually look like?



    startresult     = reform(poly_fit(xarr,xstrips[n].STARTPOINTS,order))
    endresult       = reform(poly_fit(xarr,xstrips[n].ENDPOINTS,order))

    ; xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2,tx)
    ; atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2,tx)
    
    ; window,2
    ; plot,xarr+xstrips[n].BEGINDEX,xstrips[n].startpoints,xs=3,ys=3,title='Limb Profile',$
    ;     xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    ; oplot,tx+xstrips[n].BEGINDEX,xtmp,linestyle=1
    ; hline,thresh,linestyle=2
    ; legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/right,charsize=2

    ; window,0
    ; plot,xarr+xstrips[n].ENDINDEX,xstrips[n].ENDPOINTS,xs=3,ys=3,title='Limb Profile',$
    ;     xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    ; oplot,tx+xstrips[n].ENDINDEX,atmp,linestyle=1
    ; hline,thresh,linestyle=2
    ; legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/left,charsize=2

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
    startresult     = reform(poly_fit(yarr,ystrips[n].STARTPOINTS,order))
    endresult       = reform(poly_fit(yarr,ystrips[n].ENDPOINTS,order))

;     ytmp = spline(yarr,startresult[0] + startresult[1]*yarr + startresult[2]*yarr^2,tx)
;     btmp = spline(yarr,endresult[0] + endresult[1]*yarr + endresult[2]*yarr^2,tx)
    
;     window,3
;     plot,yarr+ystrips[n].BEGINDEX,ystrips[n].startpoints,xs=3,ys=3,title='Limb Profile',$
;         xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(ytmp)]
;     oplot,tx+ystrips[n].BEGINDEX,ytmp,linestyle=1
;     hline,thresh,linestyle=2
;     legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/right,charsize=2

;     window,1
;     plot,yarr+ystrips[n].ENDINDEX,ystrips[n].ENDPOINTS,xs=3,ys=3,title='Limb Profile',$
;         xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(ytmp)]
;     oplot,tx+ystrips[n].ENDINDEX,btmp,linestyle=1
;     hline,thresh,linestyle=2
;     legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/left,charsize=2
; stop
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

IF keyword_set(plot) THEN BEGIN
    wn = 3
    startresult = poly_fit(xarr,xstrips[wn].STARTPOINTS,order)
    endresult = poly_fit(xarr,xstrips[wn].ENDPOINTS,order)

    CASE order OF
    1: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr,tx)
        END
    2: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2,tx)
        END
    3: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3,tx)
        END
    4: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4,tx)
        END
    5: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5,tx)
        END    
    6: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
                startresult[6]*xarr^6,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
                endresult[6]*xarr^6,tx)
        END
    7: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
                startresult[6]*xarr^6 + startresult[7]*xarr^7,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
                endresult[6]*xarr^6 + endresult[7]*xarr^7,tx)
        END
    ENDCASE

    ; A pretty plot for Nicole
    window,2
    ; set_plot,'ps'
    ; device,filename=file+'part1'+'.ps',/encapsulated
    plot,xarr+xstrips[wn].BEGINDEX,xstrips[wn].startpoints,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].BEGINDEX,xtmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/right,charsize=2
    ; device,/close
    ; set_plot,'x'
    window,0
    ; set_plot,'ps'
    ; device,filename=file+'part2'+'.ps',/encapsulated
    plot,xarr+xstrips[wn].ENDINDEX,xstrips[wn].ENDPOINTS,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].ENDINDEX,atmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/left,charsize=2
    ; device,/close
    ; set_plot,'x'
ENDIF

finish = systime(1,/seconds)

IF keyword_set(time) THEN  print,'Elapsed Time for limbfit: ',strcompress(finish-start,/rem),' seconds'
RETURN
END


;**************************************************************************************************
;*                                                                                                *
;**************************************************************************************************


PRO getstruct, file, struct, scan_width, sundiam, time=time
;+
;   :Description:
;       Finds the centers of a triple-sun image and loads all relevant information
;       including offsets and angles into a new structure.
;
;   :Params:
;       file: in, required, type = string, default = 'triplesun.bmp'
;           What file to find 3 centers for
;       struct : out, required, type=structure
;           Structure containing the centers and cropped images of all 3 suns
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;
;   :Keywords:
;       time: in, optional
;           Outputs how much time the program takes
;-
COMPILE_OPT idl2 
on_error,2
; COMMON vblock,image
start = systime(1,/s)

center1 = {center1,xpos:0d,ypos:0d,thresh:0d}
center2 = {center2,xpos:0d,ypos:0d,thresh:0d}
center3 = {center3,xpos:0d,ypos:0d,thresh:0d}

nstrips=5


limbfit, thresh, xpos, ypos, file, ministrip_length, order, scan_width, sundiam, $
    nstrips=nstrips, plot=plot, region=1, time=time
center1.xpos = xpos
center1.ypos = ypos
center1.thresh = thresh

limbfit, thresh, xpos, ypos, file, ministrip_length, order, scan_width, sundiam, $
    nstrips=nstrips, plot=plot, region=2, time=time
center2.xpos = xpos
center2.ypos = ypos
center2.thresh = thresh

limbfit, thresh, xpos, ypos, file, ministrip_length, order, scan_width, sundiam, $
    nstrips=nstrips, plot=plot, region=3, time=time
center3.xpos = xpos
center3.ypos = ypos
center3.thresh = thresh

theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
    (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

struct = {KAHUNA, center1:center1, center2:center2, center3:center3, $
    theta:theta, offset:offset}
finish = systime(1,/s)
IF keyword_set(time) THEN print, 'getstruct took: '+strcompress(finish-start)+$
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
;   MERRYGOTRACE
;
; PURPOSE:
;   Finds the center of 3 suns in a single image. Currently limited to a .bmp test image. Instead
;   of scanning rows to crop, scans in a circle.
;
; :Author:
;   JEREN SUZUKI::
;
;       Space Sciences Laboratory
;       7 Gauss Way
;       Berkeley, CA 94720 USA
;       E-mail: jsuzuki@ssl.berkeley.edu
;-

; PRO merrygotrace, file, scan_width, sundiam, time=time
;+
;   :Description:
;       This version uses limb fitting opposed to masking (tricenter). 
;
;   :Params:
;       file: in, required, type = string, default = 'triplesun.bmp'
;           What file to find 3 centers for
;       scan_width: in, required, type = integer, default = 5
;           How apart the scans are for minicrop(). Overrides defaults in crop().
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;
;   :Keywords:
;       time: in, optional
;           Outputs how much time the program takes
;
;   :TODO: 
;       Find and ISOLATE fiducials, not jsut mask them out
;
;       
;-
COMPILE_OPT idl2 
on_error,2

IF n_elements(file)         EQ 0 THEN   file = 'dimsun3.fits'
IF n_elements(scan_width)   EQ 0 THEN   scan_width = 5
IF n_elements(sundiam)      EQ 0 THEN   sundiam = 70

start=systime(1,/s)

; profiler,/system
; profiler
COMMON vblock, wholeimage
wholeimage = mrdfits(file)

getstruct, file, struct, scan_width, sundiam, time=time

; profiler,/report,data=data
; profiler,/reset

; print,data[sort(-data.time)],format='(A-20, I7, F12.5, F10.5, I9)'

wholeimage2 = wholeimage
wholeimage3 = wholeimage

wholeimage[struct.center1.xpos,*]=20
wholeimage[*,struct.center1.ypos]=20
wholeimage2[struct.center2.xpos,*]=20
wholeimage2[*,struct.center2.ypos]=20
wholeimage3[struct.center3.xpos,*]=20
wholeimage3[*,struct.center3.ypos]=20

print,'Main sun x pos:',struct.center1.xpos
print,'Main sun y pos:',struct.center1.ypos
print,'50% sun x pos:',struct.center2.xpos
print,'50% sun y pos:',struct.center2.ypos
print,'25% sun x pos:',struct.center3.xpos
print,'25% sun y pos:',struct.center3.ypos

; ; window,0
; cgimage,wholeimage,/k,output=strmid(file,0,7)+'_'+'region1.png'
; ; window,2
; cgimage,wholeimage2,/k,output=strmid(file,0,7)+'_'+'region2.png'
; ; window,3
; cgimage,wholeimage3,/k,output=strmid(file,0,7)+'_'+'region3.png'

; window,0
; cgimage,wholeimage,/k
; window,2
; cgimage,wholeimage2,/k
; window,3
; cgimage,wholeimage3,/k


finish = systime(1,/s)
IF keyword_set(time) THEN print, 'merrygotrace took: '+strcompress(finish-start)+$
    ' seconds'

END