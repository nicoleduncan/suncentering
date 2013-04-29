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

PRO kahuna, file, time=time
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

; wholeimage = mrdfits(file)

; Centers of dottedimage.fits
; wholeimage[200,300] = 255
; wholeimage[202,139] = 255
; wholeimage[87,231] = 255
; wholeimage[401,45] = 255
; wholeimage[23,143] = 255
; wholeimage[34,290] = 255
; wholeimage[420,242] = 255

; Main sun x pos:       210.50238
; Main sun y pos:       154.27054
; 50% sun x pos:        337.80600
; 50% sun y pos:        76.894958
; 25% sun x pos:        78.683426
; 25% sun y pos:        235.11536

wholeimage = mrdfits('dottedimage.fits',/silent)
rabbit = mrdfits('2whole.fits',/silent)
rabbit=rabbit[0,*,*]
turtle = mrdfits('partial3rd.fits',/silent)
ox = mrdfits('2partials.fits',/silent)
inaline = mrdfits('inaline.fits',/silent)
; wholeimage = mrdfits(file)
; mwrfits,wholeimage,'dottedimage.fits',/create
; window,0
; cgimage,rabbit,/k
; window,1
; cgimage,turtle,/k
; window,2
; cgimage,ox,/k

borderbit = bordercheck(wholeimage)

; read_jpeg,'plots_tables_images/2whole.jpeg',lun
; mwrfits,lun,'2whole.fits',/create
; read_jpeg,'plots_tables_images/partial3rd.jpeg',red
; mwrfits,red,'partial3rd.fits',/create
; read_jpeg,'plots_tables_images/2partials.jpeg',stairs
; mwrfits,stairs,'2partials.fits',/create

; stop

; profiler,/report,data=data
; profiler,/reset,/clear

; print,data[sort(-data.time)],format='(A-20, I7, F12.5, F10.5, I9)'


; ****************************
; *******              *******
; *******              *******
; *******              *******
; ****************************

; getstruct, struct, time=time
; print,'Main sun x pos:',struct.center1.xpos
; print,'Main sun y pos:',struct.center1.ypos
; print,'50% sun x pos: ',struct.center2.xpos
; print,'50% sun y pos: ',struct.center2.ypos
; print,'25% sun x pos: ',struct.center3.xpos
; print,'25% sun y pos: ',struct.center3.ypos

; ****************************
; *******              *******
; *******              *******
; *******              *******
; ****************************

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

; crop = wholeimage[struct.center1.xpos-!param.safecrop:struct.center1.xpos+!param.safecrop,$
;     struct.center1.ypos-!param.safecrop:struct.center1.ypos+!param.safecrop]
; thresh = 0.5*MIN((SHIFT_DIFF(EMBOSS(crop),dir=3)))


; threshlist = setthresh(wholeimage)
gooooooooaaallll = fastcenter(wholeimage)
histosmoothed,wholeimage
stop

rabbits = last6pixels(crop,thresh)
turtles = galapagos(crop,thresh)

edgefidbit = edgefidcheck(crop,thresh)
hmmm = barkbark(crop,thresh)
hmmm = scratch(crop,thresh)


stop
finish = SYSTIME(1,/s)
IF KEYWORD_SET(time) THEN print, 'merrygotrace took: '+strcompress(finish-start)+$
    ' seconds'
end