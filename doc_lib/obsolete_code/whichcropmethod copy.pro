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

a = wholeimage[BSORT(wholeimage)]
niceimage = a[0:(1-!param.elim_perc/100)*(N_ELEMENTS(a)-1)]

thresh = !param.reg1thresh_mult*max(niceimage)
thresh = !param.thresh100
ducks = quickmask(wholeimage,thresh)

image = wholeimage[ducks.xpos- !param.crop_box:ducks.xpos+ !param.crop_box, $
    ducks.ypos- !param.crop_box:ducks.ypos+ !param.crop_box]

mainxpos = ducks.xpos
mainypos = ducks.ypos
xoffset = ducks.xpos- !param.crop_box
yoffset = ducks.ypos- !param.crop_box

; print,'main xpos',mainxpos
; print,'main ypos',mainypos
IF REGION NE 1 THEN BEGIN
    circscancrop, mainxpos, mainypos, image, thresh, xpos, ypos, xoffset, yoffset, region=region, time=time
ENDIF

; print,'thresh is: ', thresh
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