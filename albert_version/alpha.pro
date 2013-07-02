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
wholeimage = MRDFITS('../fits_files/dottedimage.fits',/sil)
w1_w2_p3 = MRDFITS('../fits_files/partial3rd.fits',/sil)
w1_p2_p3 = MRDFITS('../fits_files/2partials.fits',/sil)
reg12 = MRDFITS('../fits_files/1_2.fits',/sil)
reg13 = MRDFITS('../fits_files/1_3.fits',/sil)
reg23 = MRDFITS('../fits_files/2_3.fits',/sil)
w2_p3 = MRDFITS('../fits_files/w2_p3.fits',/sil)
p1_w2_w3 = MRDFITS('../fits_files/p1_w2_w3.fits',/sil)
p1_w2_p3 = MRDFITS('../fits_files/p1_w2_p3.fits',/sil)
p1_p2_w3 = MRDFITS('../fits_files/p1_p2_w3.fits',/sil)
w1_p2_w3 = MRDFITS('../fits_files/w1_p2_w3.fits',/sil)
p1_w3 = MRDFITS('../fits_files/p1_w3.fits',/sil)
p1_w2 = MRDFITS('../fits_files/p1_w2.fits',/sil)
w1_p3 = MRDFITS('../fits_files/w1_p3.fits',/sil)
brightsun = MRDFITS('../fits_files/albsun.fits',/sil)
corner = MRDFITS('../fits_files/corner.fits',/sil)
corner2 = MRDFITS('../fits_files/corner2.fits',/sil)
corner3 = MRDFITS('../fits_files/corner3.fits',/sil)
dimsun = MRDFITS('sun2.fits',/sil)
tritest = MRDFITS('tritest.fits',/sil)

; Take your pick of which to center

startimage=wholeimage
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
; startimage = brightsun   ;no param list for this one
startimage = dimsun
startimage = tritest

; profiler
; profiler,/system

; takes ~.07 s to run albert's triple sun image


tic
defparams, 'pblock_albtritest.txt'
; defparams, 'pblock_albdimsun.txt'
; defparams, 'pblock_orig_small.txt'
toc
; .0005 to here
defsysvarthresh, startimage
toc
; .03 to here
grannysmith = everysun(startimage)
toc
; .065 to here
fuji = picksun_rot(startimage, grannysmith)
toc
; .065 to here
limbfittedcentroids=centroidwholesuns(fuji,startimage)
toc
; .068 to here
bbb = para_fid(startimage,limbfittedcentroids)
; .07 to here
toc
tmpimage = startimage

if N_ELEMENTS(limbfittedcentroids) gt 1 then begin
    for i =0,N_ELEMENTS(limbfittedcentroids)-1 do begin
        tmpimage[limbfittedcentroids[i].limbxpos-1:limbfittedcentroids[i].limbxpos+1,*] = 255
        tmpimage[*,limbfittedcentroids[i].limbypos-1:limbfittedcentroids[i].limbypos+1] = 255
    endfor
endif else begin
    tmpimage[limbfittedcentroids[0].limbxpos-1:limbfittedcentroids[0].limbxpos+1,*] = 255
    tmpimage[*,limbfittedcentroids[0].limbypos-1:limbfittedcentroids[0].limbypos+1] = 255
endelse

; so the rough center is a bit off. Gasp!

; profiler,/report,data=data
; profiler,/reset,/clear
; print,data[sort(-data.time)],format='(A-20, I7, F12.5, F10.5, I9)'

atmp = startimage

; So I have to highlight fiducials

for i = 0,N_ELEMENTS(bbb)-1 do begin
    for j = 0,N_ELEMENTS((*(bbb[i])).fidarr)-1 do begin

        if ((*(bbb[i])).fidarr)[j].subx ne 0 or ((*(bbb[i])).fidarr)[j].suby ne 0 then begin
        atmp[((*(bbb[i])).fidarr)[j].subx + limbfittedcentroids[i].limbxpos - !param.crop_box -1:((*(bbb[i])).fidarr)[j].subx + limbfittedcentroids[i].limbxpos - !param.crop_box+1,((*(bbb[i])).fidarr)[j].suby + limbfittedcentroids[i].limbypos - !param.crop_box-1:((*(bbb[i])).fidarr)[j].suby + limbfittedcentroids[i].limbypos - !param.crop_box+1]=255
        endif
    endfor
endfor

; print,'Main sun x pos:',limbfittedcentroids[0].limbxpos
; print,'Main sun y pos:',limbfittedcentroids[0].limbypos
; print,'50% sun x pos: ',limbfittedcentroids[1].limbxpos
; print,'50% sun y pos: ',limbfittedcentroids[1].limbypos
; print,'25% sun x pos: ',limbfittedcentroids[2].limbxpos
; print,'25% sun y pos: ',limbfittedcentroids[2].limbypos

window,0
cgimage,tmpimage,/k
window,1
cgimage,atmp,/k

ztmp = startimage

best4 = best4(limbfittedcentroids,bbb)

; ztmp[limbfittedcentroids[0].limbxpos-1:limbfittedcentroids[0].limbxpos+1,limbfittedcentroids[0].limbypos-1:limbfittedcentroids[0].limbypos+1]=!red

; a=ztmp[limbfittedcentroids[0].limbxpos- !param.crop_box:limbfittedcentroids[0].limbxpos + !param.crop_box, limbfittedcentroids[0].limbypos - !param.crop_box : limbfittedcentroids[0].limbypos + !param.crop_box]

; for j = 0,n_elements(best4[0].fidarr)-1 do begin
;         a[best4[0].fidarr[j].subx -1:best4[0].fidarr[j].subx +1, best4[0].fidarr[j].suby-1:best4[0].fidarr[j].suby+1]=255
; endfor

; cgimage,a,/k

for i = 0,n_elements(best4)-1 do begin
    for j = 0,n_elements(best4[i].fidarr)-1 do begin
        ztmp[best4[i].fidarr[j].subx + limbfittedcentroids[i].limbxpos - !param.crop_box -1:best4[i].fidarr[j].subx + limbfittedcentroids[i].limbxpos - !param.crop_box+1,$
            best4[i].fidarr[j].suby + limbfittedcentroids[i].limbypos - !param.crop_box-1:$
            best4[i].fidarr[j].suby + limbfittedcentroids[i].limbypos - !param.crop_box+1]=255
    endfor
endfor
window,2
cgimage,ztmp,/k
; cgimage,startimage[limbfittedcentroids[0].limbxpos- !param.crop_box:limbfittedcentroids[0].limbxpos+ !param.crop_box,limbfittedcentroids[0].limbypos- !param.crop_box:limbfittedcentroids[0].limbypos+ !param.crop_box],output='tritest_reg1.eps',/k,/display
; cgimage,startimage[limbfittedcentroids[1].limbxpos- !param.crop_box:limbfittedcentroids[1].limbxpos+ !param.crop_box,limbfittedcentroids[1].limbypos- !param.crop_box:limbfittedcentroids[1].limbypos+ !param.crop_box],output='tritest_reg2.eps',/k,/display
; cgimage,startimage[limbfittedcentroids[2].limbxpos- !param.crop_box:limbfittedcentroids[2].limbxpos+ !param.crop_box,limbfittedcentroids[2].limbypos- !param.crop_box:limbfittedcentroids[2].limbypos+ !param.crop_box],output='tritest_reg3.eps',/k,/display

; aa = startimage

; aa[*,limbfittedcentroids[0].limbypos -20]=255
; aa[*,limbfittedcentroids[0].limbypos -10]=255
; aa[*,limbfittedcentroids[0].limbypos]=255
; aa[*,limbfittedcentroids[0].limbypos +10]=255
; aa[*,limbfittedcentroids[0].limbypos +20]=255

; aa = aa[limbfittedcentroids[0].limbxpos- !param.crop_box:limbfittedcentroids[0].limbxpos+ !param.crop_box,limbfittedcentroids[0].limbypos- !param.crop_box:limbfittedcentroids[0].limbypos+ !param.crop_box]
; cgimage,aa,/k,/display,output='5chords.eps'






f = 25e-3
pix_size = 3.75e-6 * (.914-f)/f
; print,pix_size*1000

; .26mm for 2 pixels thick

; how long?
; 3: .399 mm
; 7: .933
; 9: 1.2
; 11: 1.46
; 15: 2

; how many pixels in our image editing program?

; @ 300 pix/cm
; 3: 12
; 7: 28
; 9: 36
; 11: 43.8
; 15: 60

; so we want our fiducials to be, say, .26x.4mm 

stop

end