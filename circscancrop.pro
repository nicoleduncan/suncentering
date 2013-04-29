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
    ; sorted =  wholeimage[bsort(wholeimage)]
    ; thresh = !param.reg2thresh_mult*MAX(sorted[0:(1-!param.elim_perc/100)*(N_ELEMENTS(sorted)-1)] )
    ; ; ^^
    ; ; Well this doesn't work.
    ; thresh = !param.reg2thresh_mult*MAX(wholeimage)

    thresh = !param.thresh50

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
        ; thresh = 0.2*MAX(wholeimage) ;dimsun2 works if i set the thresh to .2 instead of .15
        ; ; The other sun is so dim that weird parts are being picked up. How to fix? Is being dim a problem?
        ; sorted =  wholeimage[bsort(wholeimage)]
        ; thresh = !param.reg3thresh_mult*MAX( sorted[0:(1-!param.elim_perc/100)*(N_ELEMENTS(sorted)-1)] )
        ; ^^
        ; Well this doesn't work.
        ; print,thresh
        ; thresh = !param.reg3thresh_mult*MAX(wholeimage)
        
        thresh = !param.thresh25

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
        ; stop
    ENDIF
END


centerangle = !dtor*(90 + MEAN([in_inner,out_inner]))
centerx = mainxpos + radius*COS(centerangle)
centery = mainypos + radius*SIN(centerangle)

; This part fails with the bright pixels

image = wholeimage[centerx - !param.crop_box:centerx + !param.crop_box,$
    centery - !param.crop_box:centery + !param.crop_box]
xoffset = centerx- !param.crop_box
yoffset = centery- !param.crop_box

finish = SYSTIME(1,/s)
IF KEYWORD_SET(time) THEN print, 'getstruct took: '+STRCOMPRESS(finish-start)+$
    ' seconds'

; print,'circscancrop center is ',centerx
; print,'circscancrop center is ',centery


RETURN
END