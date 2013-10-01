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
w1_w2_p3 = mrdfits('partial3rd.fits',/silent)
w1_p2_p3 = mrdfits('2partials.fits',/silent)
; inaline = mrdfits('inaline.fits',/silent)
reg12 = mrdfits('1_2.fits',/silent)
reg13 = mrdfits('1_3.fits',/silent)
reg23 = mrdfits('2_3.fits',/silent)
w2_p3 = mrdfits('w2_p3.fits',/sil)
p1_w2_w3 = mrdfits('p1_w2_w3.fits',/sil)
p1_w2_p3 = mrdfits('p1_w2_p3.fits',/sil)
p1_p2_w3 = mrdfits('p1_p2_w3.fits',/sil)
w1_p2_w3 = mrdfits('w1_p2_w3.fits',/sil)
p1_w3 = mrdfits('p1_w3.fits',/sil)
p1_w2 = mrdfits('p1_w2.fits',/sil)
w1_p3 = mrdfits('w1_p3.fits',/sil)

names = ['w1_w2_p3',$
    'reg12',$
    'reg23',$
    'reg13',$
    'w1_p2_p3',$
    'wholeimage',$
    'w2_p3',$
    'p1_w2_w3',$
    'p1_w2_p3',$
    'p1_p2_w3',$
    'p1_w3',$
    'p1_w2',$
    'w1_p2_w3',$
    'w1_p3']

imgstr = ptrarr(14,/allocate)
*imgstr[0] = ptr_new(w1_w2_p3,/allocate)
*imgstr[1] = ptr_new(reg12,/allocate)
*imgstr[2] = ptr_new(reg23,/allocate)
*imgstr[3] = ptr_new(reg13,/allocate)
*imgstr[4] = ptr_new(w1_p2_p3,/allocate)
*imgstr[5] = ptr_new(wholeimage,/allocate)
*imgstr[6] = ptr_new(w2_p3,/allocate)
*imgstr[7] = ptr_new(p1_w2_w3,/allocate)
*imgstr[8] = ptr_new(p1_w2_p3,/allocate)
*imgstr[9] = ptr_new(p1_p2_w3,/allocate)
*imgstr[10] = ptr_new(p1_w3,/allocate)
*imgstr[11] = ptr_new(p1_w2,/allocate)
*imgstr[12] = ptr_new(w1_p2_w3,/allocate)
*imgstr[13] = ptr_new(w1_p3,/allocate)

tic
testestest = checkimage(wholeimage)
toc
print,testestest
tic
a=centroidtest(wholeimage)
toc
print,a
stop

    newimage = imageprep(w1_p2_p3)
    im = newimage
    idedsuns = idsuns(im)
    hailmary = fastercenter(im,idedsuns)
    
    
    stop
    print,strcompress(names[7],/rem)
    print,idedsuns
    print,''
    stop
    ; position of 3rd sun is fucked up

    ; It's becaue the y positions of the suns are the same
    ; LOL, how to fix?

    !p.multi=[0,1,n_elements(idedsuns)]
    for i = 0,n_elements(idedsuns)-1 do begin
        cgimage,im[hailmary[i].xpos-60:hailmary[i].xpos+60,hailmary[i].ypos-60:hailmary[i].ypos+60],/k
    endfor


for j=0,13 do begin
    im = *(*imgstr[j])
    newimage = imageprep(im)
    im = newimage
    idedsuns = idsuns(im)
    hailmary = fastercenter(im,idedsuns)
    print,strcompress(names[j],/rem)
    print,idedsuns
    print,''
    !p.multi=[0,1,n_elements(idedsuns)]
    for i = 0,n_elements(idedsuns)-1 do begin
        cgimage,im[hailmary[i].xpos-60:hailmary[i].xpos+60,hailmary[i].ypos-60:hailmary[i].ypos+60],/k
    endfor
    !p.multi=0
    wait,1
endfor





stop

edgefidbit = edgefidcheck(crop,thresh)

; if keyword_set(time) then toc
end