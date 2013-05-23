PRO beta
;+
;   :Description:
;      Finds the center of N whole suns and M partial suns using limb-fitting for the whole suns and simple centroiding for the partial suns
;
;   :Params:
;
;   :TODO: 
;       NONE, BRAH
;
;-
COMPILE_OPT idl2
ON_ERROR,1
start=SYSTIME(1,/s)


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

; Load parameters from a txt file and make then system variables
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

; Our list of images to take centers of
wholeimage = mrdfits('dottedimage.fits',/sil)
w1_w2_p3 = mrdfits('partial3rd.fits',/sil)
w1_p2_p3 = mrdfits('2partials.fits',/sil)
reg12 = mrdfits('1_2.fits',/sil)
reg13 = mrdfits('1_3.fits',/sil)
reg23 = mrdfits('2_3.fits',/sil)
w2_p3 = mrdfits('w2_p3.fits',/sil)
p1_w2_w3 = mrdfits('p1_w2_w3.fits',/sil)
p1_w2_p3 = mrdfits('p1_w2_p3.fits',/sil)
p1_p2_w3 = mrdfits('p1_p2_w3.fits',/sil)
w1_p2_w3 = mrdfits('w1_p2_w3.fits',/sil)
p1_w3 = mrdfits('p1_w3.fits',/sil)
p1_w2 = mrdfits('p1_w2.fits',/sil)
w1_p3 = mrdfits('w1_p3.fits',/sil)

; Take your pick of which to center

; startimage=wholeimage
startimage=w1_w2_p3
; startimage=w1_p2_p3
; startimage=reg12
; startimage=reg23
; startimage=reg13
; startimage=w2_p3
; startimage=p1_w2_w3
; startimage=p1_w2_p3
; startimage=p1_p2_w3
; startimage=w1_p2_w3
; startimage=p1_w3
; startimage=p1_w2
startimage=w1_p3

; takes ~.07 s to run everything up to fid_locate
tic

defsysvarthresh,startimage

grannysmith = everysun(startimage)

fuji = picksun(startimage, grannysmith)

limbfittedcentroids=centroidwholesuns(fuji,startimage)

tmpimage = startimage
for i =0,n_elements(limbfittedcentroids)-1 do begin
    tmpimage[limbfittedcentroids[i].limbxpos,*] = 255
    tmpimage[*,limbfittedcentroids[i].limbypos] = 255
endfor
a = fid_locate(startimage,limbfittedcentroids)
toc
cgimage,tmpimage,/k

atmp = startimage
a = fid_locate(startimage,limbfittedcentroids)

; stop
for i = 0,n_elements(a)-1 do begin
    atmp[a[i].x + limbfittedcentroids[0].limbxpos - !param.soldiskr,a[i].y + limbfittedcentroids[0].limbypos - !param.soldiskr]=255
endfor

; acrop = atmp[limbfittedcentroids[0].limbxpos - !param.soldiskr : $
; limbfittedcentroids[0].limbxpos + !param.soldiskr,$
; limbfittedcentroids[0].limbypos - !param.soldiskr : limbfittedcentroids[0].limbypos + !param.soldiskr]
; acrop = acrop[0:-5,0:-4]
; cgimage,acrop,/k,output='notsubpix.png'

; a = corr_fid(startimage,limbfittedcentroids)
; a = crosstest(startimage,limbfittedcentroids)
stop
print,'Main sun x pos:',limbfittedcentroids[0].limbxpos
print,'Main sun y pos:',limbfittedcentroids[0].limbypos
print,'50% sun x pos: ',limbfittedcentroids[1].limbxpos
print,'50% sun y pos: ',limbfittedcentroids[1].limbypos
print,'25% sun x pos: ',limbfittedcentroids[2].limbxpos
print,'25% sun y pos: ',limbfittedcentroids[2].limbypos

stop

end