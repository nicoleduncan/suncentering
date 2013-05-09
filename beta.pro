PRO beta, time=time
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
start=SYSTIME(1,/s)

; profiler,/system
; profiler

; DEATH TO THE COMMON BLOCK (or not)
COMMON vblock, w1_w2_p3
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



; How do we run the least number of setbetterpeak? We really only need to run it once
; a=setbetterthresh(wholeimage)
; turn setbetterthresh into a sysvar setting function

wholeimage = mrdfits('dottedimage.fits',/silent)
w1_w2_p3 = mrdfits('partial3rd.fits',/silent)
; takes 1.3s
startimage=w1_w2_p3
defsysvarthresh,startimage

grannysmith = everysun(startimage)

fuji = picksun(startimage, grannysmith)

; how do I pass the right image w/o common blocks?
limbfittedcentroids=centroidwholesuns(fuji)

; testestest = checkimage(startimage)


stop
; ****************************
; *******              *******
; *******              *******
; *******              *******
; ****************************
profiler,/system
profiler
; tic

; toc
profiler,/report
profiler,/reset,/clear
print,'Main sun x pos:',struct.center1.xpos
print,'Main sun y pos:',struct.center1.ypos
print,'50% sun x pos: ',struct.center2.xpos
print,'50% sun y pos: ',struct.center2.ypos
print,'25% sun x pos: ',struct.center3.xpos
print,'25% sun y pos: ',struct.center3.ypos

; ****************************
; *******              *******
; *******              *******
; *******              *******
; ****************************

stop
finish = SYSTIME(1,/s)
IF KEYWORD_SET(time) THEN print, 'merrygotrace took: '+strcompress(finish-start)+$
    ' seconds'
end