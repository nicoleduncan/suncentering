PRO curr, time=time
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
;       Um, let's not (Apr24)
;
;       Make sure program doesn't freak out when sun isn't in POV
;       
;-
COMPILE_OPT idl2
ON_ERROR,1
if keyword_set(time) then tic

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

wholeimage = mrdfits('dottedimage.fits',/silent)
turtle = mrdfits('partial3rd.fits',/silent)
kanga = mrdfits('2partials.fits',/silent)
; inaline = mrdfits('inaline.fits',/silent)

reg12 = mrdfits('1_2.fits',/silent)
reg13 = mrdfits('1_3.fits',/silent)
reg23 = mrdfits('2_3.fits',/silent)

; im = reg12
; newimage = imageprep(im)
; im = newimage
; idedsuns = idsuns(im)
; hailmary = fastercenter(im,idedsuns)
; print,hailmary

; im = reg23
; newimage = imageprep(im)
; im = newimage
; idedsuns = idsuns(im)
; hailmary = fastercenter(im,idedsuns)
; print,hailmary

; im = reg13
; newimage = imageprep(im)
; im = newimage
; idedsuns = idsuns(im)
; hailmary = fastercenter(im,idedsuns)
; print,hailmary

; im = turtle
; newimage = imageprep(im)
; im = newimage
; idedsuns = idsuns(im)
; hailmary = fastercenter(im,idedsuns)
; print,hailmary

; im = kanga
; newimage = imageprep(im)
; im = newimage
; idedsuns = idsuns(im)
; hailmary = fastercenter(im,idedsuns)
; print,hailmary

im = turtle
newimage = imageprep(im)
im = newimage
idedsuns = idsuns(im)
hailmary = fastercenter(im,idedsuns)
print,hailmary

!p.multi=[0,1,n_elements(idedsuns)]
for i = 0,n_elements(idedsuns)-1 do begin
    cgimage,im[hailmary[i].xpos-60:hailmary[i].xpos+60,hailmary[i].ypos-60:hailmary[i].ypos+60],/k
endfor
!p.multi=0
stop

edgefidbit = edgefidcheck(crop,thresh)

; if keyword_set(time) then toc
end