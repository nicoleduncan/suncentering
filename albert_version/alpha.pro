; docformat = 'rst'
PRO alpha
;+
;   :Description:
;      Finds the center of N whole suns and M partial suns using limb-fitting for the whole suns and simple centroiding for the partial suns
;
;   :Params:
;
;   :TODO: 
;       NONE, BRAH
;
; idldoc,root='/Users/jerensuzuki/Documents/suncentering', output='rbdoc',format_style='rst',/user,/quiet,markup_style='rst'
;-
COMPILE_OPT idl2
ON_ERROR,1

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
wholeimage = mrdfits('../fits_files/dottedimage.fits',/sil)
w1_w2_p3 = mrdfits('../fits_files/partial3rd.fits',/sil)
w1_p2_p3 = mrdfits('../fits_files/2partials.fits',/sil)
reg12 = mrdfits('../fits_files/1_2.fits',/sil)
reg13 = mrdfits('../fits_files/1_3.fits',/sil)
reg23 = mrdfits('../fits_files/2_3.fits',/sil)
w2_p3 = mrdfits('../fits_files/w2_p3.fits',/sil)
p1_w2_w3 = mrdfits('../fits_files/p1_w2_w3.fits',/sil)
p1_w2_p3 = mrdfits('../fits_files/p1_w2_p3.fits',/sil)
p1_p2_w3 = mrdfits('../fits_files/p1_p2_w3.fits',/sil)
w1_p2_w3 = mrdfits('../fits_files/w1_p2_w3.fits',/sil)
p1_w3 = mrdfits('../fits_files/p1_w3.fits',/sil)
p1_w2 = mrdfits('../fits_files/p1_w2.fits',/sil)
w1_p3 = mrdfits('../fits_files/w1_p3.fits',/sil)
albsun = mrdfits('../fits_files/albsun.fits',/sil)
corner = mrdfits('../fits_files/corner.fits',/sil)
corner2 = mrdfits('../fits_files/corner2.fits',/sil)
corner3 = mrdfits('../fits_files/corner3.fits',/sil)
somesun = mrdfits('sun2.fits',/sil)
tritest = mrdfits('tritest.fits',/sil)

; Take your pick of which to center

; startimage=wholeimage
; startimage=w1_w2_p3
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
; startimage=w1_p3
startimage = albsun
startimage = somesun
; startimage = tritest
; a = partialcenter(corner)

profiler
profiler,/system
; takes ~.07 s to run everything up to fid_locate
; takes ~.2 s to run albert's image



defparams

defsysvarthresh,startimage

grannysmith = everysun(startimage)

fuji = picksun(startimage, grannysmith)

limbfittedcentroids=centroidwholesuns(fuji,startimage)

tmpimage = startimage

if n_elements(limbfittedcentroids) gt 1 then begin
    for i =0,n_elements(limbfittedcentroids)-1 do begin
        tmpimage[limbfittedcentroids[i].limbxpos-1:limbfittedcentroids[i].limbxpos+1,*] = 255
        tmpimage[*,limbfittedcentroids[i].limbypos-1:limbfittedcentroids[i].limbypos+1] = 255
    endfor
endif else begin
; stop
    tmpimage[limbfittedcentroids[0].limbxpos-1:limbfittedcentroids[0].limbxpos+1,*] = 255
    tmpimage[*,limbfittedcentroids[0].limbypos-1:limbfittedcentroids[0].limbypos+1] = 255
endelse


bbb = gord_fid(startimage,limbfittedcentroids)

; a = fid_locate(startimage,limbfittedcentroids)

profiler,/report,data=data
profiler,/reset,/clear
print,data[sort(-data.time)],format='(A-20, I7, F12.5, F10.5, I9)'

; window,0
; cgimage,tmpimage,/k

atmp = startimage

bbb = gord_fid(startimage,limbfittedcentroids)


print,bbb
; a = fid_locate(startimage,limbfittedcentroids)

; stop
for i = 0,n_elements(a)-1 do begin
    atmp[a[i].subpx + limbfittedcentroids[0].limbxpos - !param.crop_box,a[i].subpy + limbfittedcentroids[0].limbypos - !param.crop_box]=255
endfor
subsol = atmp[limbfittedcentroids[0].limbxpos-120:limbfittedcentroids[0].limbxpos+120,limbfittedcentroids[0].limbypos-120:limbfittedcentroids[0].limbypos+120]
; window,1
; cgimage,subsol,/k

; albert's numbers
ztmp = startimage
ztmp[674.6796,966-151.0038] = 255
ztmp[796.3074,966-195.0324] = 255                                                  
ztmp[740.4443,966-210.6342] = 255                                                  
ztmp[690.2598,966-226.1973] = 255                                                  
ztmp[643.4235,966-241.8869] = 255                                                  
ztmp[755.8672,966-279.6622] = 255                                                  
ztmp[706.0065,966-295.3022] = 255   

; cgimage,ztmp[limbfittedcentroids.limbxpos-120:limbfittedcentroids.limbxpos+120,limbfittedcentroids.limbypos-120:limbfittedcentroids.limbypos+120],/k


; acrop = atmp[limbfittedcentroids[0].limbxpos - !param.soldiskr : $
; limbfittedcentroids[0].limbxpos + !param.soldiskr,$
; limbfittedcentroids[0].limbypos - !param.soldiskr : limbfittedcentroids[0].limbypos + !param.soldiskr]
; acrop = acrop[0:-5,0:-4]
; cgimage,acrop,/k,output='notsubpix.png'

; gg = startimage[limbfittedcentroids[0].limbxpos-120:limbfittedcentroids[0].limbxpos+120,limbfittedcentroids[0].limbypos-120:limbfittedcentroids[0].limbypos+120]
; cgimage,gg*(gg gt !thresh.reg3),/k

; ctt = cutofftest(startimage,limbfittedcentroids,a)
print,'gord_fid'
tic
aaa = gord_fid(startimage,limbfittedcentroids)
toc
print,'fid_faster'
tic
bbb = fid_faster(startimage,limbfittedcentroids)
toc
print,'fid_locate'
tic
a = fid_locate(startimage,limbfittedcentroids)
toc

; stop
; print,'Main sun x pos:',limbfittedcentroids[0].limbxpos
; print,'Main sun y pos:',limbfittedcentroids[0].limbypos
; print,'50% sun x pos: ',limbfittedcentroids[1].limbxpos
; print,'50% sun y pos: ',limbfittedcentroids[1].limbypos
; print,'25% sun x pos: ',limbfittedcentroids[2].limbxpos
; print,'25% sun y pos: ',limbfittedcentroids[2].limbypos

stop

end