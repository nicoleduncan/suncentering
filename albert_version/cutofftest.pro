FUNCTION cutofftest, inputimage, inputstruct, fidstruct

crop = inputimage[inputstruct.limbxpos - !param.crop_box:inputstruct.limbxpos + !param.crop_box,inputstruct.limbypos - !param.crop_box:inputstruct.limbypos + !param.crop_box]
s = size(crop,/d)
prime = quickmask(crop,inputstruct.thresh)

; loadct,13
; only go up to 3
; maybe go up to 5 col-wide at a time

; ps_start,filename='cutofftestside.eps',/encap,xsize=6,ysize=8
; !p.multi=[0,3,3]
; for i = 0, 8 do begin
;    somecrop = crop[35 + i*10:s[0]-1,0:s[1]-1]
;    somec = quickmask (somecrop,inputstruct.thresh)
;    somecrop[somec.xpos-1:somec.xpos+1,somec.ypos-1:somec.ypos+1]=255
;    somecrop[prime.xpos-(35 + i*10) - 1:prime.xpos-(35+i*10) + 1 ,prime.ypos-1:prime.ypos+1]=180
;    print,prime.xpos-(35 + i*10) - somec.xpos
;    print,prime.ypos - somec.ypos
;    a=where(somecrop gt inputstruct.thresh,n_pix)
;    print,n_pix
;    cgimage,somecrop,/k
; endfor
; ps_end

; print, 'interim'

; ;================================================================================

; ;================================================================================

; ps_start,filename='cutofftestcorner.eps',/encap,xsize=6,ysize=8
; !p.multi=[0,3,3]
; for i = 0, 8 do begin
;    somecrop = crop[35 + i*10:s[0]-1,35 + i*10:s[1]-1]
;    somec = quickmask(somecrop,inputstruct.thresh)
;    somecrop[somec.xpos-1:somec.xpos+1,somec.ypos-1:somec.ypos+1]=255
;    somecrop[prime.xpos-(35 + i*10) - 1:prime.xpos-(35+i*10) + 1 ,prime.ypos-(35+ i*10) -1:prime.ypos-(35+i*10)+1]=180
;    cgimage,somecrop,/k
;    a=where(somecrop gt inputstruct.thresh,n_pix)
;    print,n_pix
;    print,prime.xpos-(35 + i*10) - somec.xpos
;    print,prime.ypos-(35 + i*10) - somec.ypos
; endfor
; ps_end









; ; ps_start,filename='side5col.eps',/encap,xsize=6,ysize=4
; ; !p.multi=[0,3,2]
; for i = 0, 5 do begin
;    somecrop = crop[35 + i*5:s[0]-1,0:s[1]-1]
;    somec = quickmask (somecrop,inputstruct.thresh)
;    somecrop[somec.xpos-1:somec.xpos+1,somec.ypos-1:somec.ypos+1]=255
;    somecrop[prime.xpos-(35 + i*5) - 1:prime.xpos-(35+i*5) + 1 ,prime.ypos-1:prime.ypos+1]=180
;    ; print,prime.xpos-(35 + i*5) - somec.xpos
;    ; print,prime.ypos - somec.ypos
;    a=where(somecrop gt inputstruct.thresh,n_pix)
;    print,n_pix/26597.
;    ; cgimage,somecrop,/k
; endfor
; ; ps_end

; print, 'interim'

; ;================================================================================

; ;================================================================================

; ; ps_start,filename='diag5col.eps',/encap,xsize=6,ysize=4
; ; !p.multi=[0,3,2]
; for i = 0, 5 do begin
;    somecrop = crop[35 + i*5:s[0]-1,35 + i*5:s[1]-1]
;    somec = quickmask(somecrop,inputstruct.thresh)
;    somecrop[somec.xpos-1:somec.xpos+1,somec.ypos-1:somec.ypos+1]=255
;    somecrop[prime.xpos-(35 + i*5) - 1:prime.xpos-(35+i*5) + 1 ,prime.ypos-(35+ i*5) -1:prime.ypos-(35+i*5)+1]=180
;    ; cgimage,somecrop,/k
;    a=where(somecrop gt inputstruct.thresh,n_pix)
;    print,n_pix/26597.
;    ; print,prime.xpos-(35 + i*5) - somec.xpos
;    ; print,prime.ypos-(35 + i*5) - somec.ypos
; endfor
; ; ps_end
; ; What are we looking for in particular here?

; ; plot,crop[*,0]
; ; for i = 0,((size(crop,/dim))[0])-1 do begin
; ; plot,crop[*,i]
; ; wait,.1
; ; endfor


tic
squirtle = float((transpose(crop))[*])
temparr = float(crop[*])
n_col = (size(crop,/dim))[0]

; kernel = [1,1,1,1,1,-1,-1,-1,-1,-1,1,1,1,1,1]
; kernel = [1,1,1,1,1,-1,-1,-1,-1,-1,1,1,1,1,1]
kernel = [1,0,0,0,1]

; we don't want pluses though, we want smooth hills
; kernel = [1,1,1,1,1,0,0,0,0,0,1,1,1,1,1]
; looks good:
; kernel = [1,-1,-1,-1,1]
kernel = [1,0,1]

; for i = 0,n_col-1 do begin

;     ; a=CONVOL(FIX(crop[*,i]),[1,1,1,1,1,-1,-1,-1,-1,-1,1,1,1,1,1],/edge_truncate)
;     ; plot,a
;     ; oplot,crop[*,i]
;     ; vline,where(a eq max(a))
;     ; wait,.1
;     a = temparr[i*n_col:(i+1)*n_col-1]
;     ca = CONVOL(a,kernel,/edge_truncate)
;     if i eq 0 then ta = ca else ta =[ta,ca]
;     ; plot,ca
;     ; oplot,a
; endfor
; cgimage,reform(ta,241,241),/k
; print,fidstruct.subpx
; print,fidstruct.subpy
; um, we can just do this: .0003 s compared to .003 of for-loop compared to .026 of 2D convol. 10x faster? BS.
test = convol(temparr,kernel)
ytest = convol(squirtle,kernel)

im = reform(test,241,241)
yim = reform(ytest,241,241)

tyim = transpose(yim)
toc


tic
fidwidth = 3
fidlength = 15
kernel = fltarr(fidlength,fidlength)
kernel[0:fidwidth-1,*]=1
kernel[*,0:fidwidth-1]=1
kernel = SHIFT(kernel,2*fidwidth,2*fidwidth)

image = CONVOL(FIX(crop),kernel,/edge_truncate)
toc

; .001 to do the x and y things convols. The transposing sucks.

; for i = 0,n_elements(fidstruct)-1 do begin
;     im[fidstruct[i].subpx,fidstruct[i].subpy]=300
;     tyim[fidstruct[i].subpx,fidstruct[i].subpy]=300
; endfor

; somearea = (im*tyim)[34:74,88:128]

somearea = im*tyim
atmp = somearea
somefactor = .5
fidfloor = 8000
s = size(somearea,/d)
threshold = mean(somearea) - somefactor*stddev(somearea)

; this motherfucking for loop takes .02 s, same as other for loop
for i = 2, s[0]-3 do begin
    for j = 2,s[1]-3 do begin
        ; The correlation value at some position
        thisvalue = somearea[i,j]
        ; Check to see if it's less than the surrounding 8 pixels
        if thisvalue lt threshold  and thisvalue gt fidfloor then begin
            if thisvalue lt somearea[i,j+1] and $
            thisvalue lt somearea[i,j-1] and $
            thisvalue lt somearea[i+1,j] and $
            thisvalue lt somearea[i-1,j] and $
            thisvalue lt somearea[i-1,j-1] and $
            thisvalue lt somearea[i-1,j+1] and $
            thisvalue lt somearea[i+1,j-1] and $
            thisvalue lt somearea[i+1,j+1] then begin
                ; Well it's not really picking it up
                ; print,i,j
                ; Look in the 16 pixels around the 8 pixels
                if thisvalue lt somearea[i,j+2] and $
                thisvalue lt somearea[i,j-2] and $
                thisvalue lt somearea[i+2,j] and $
                thisvalue lt somearea[i-2,j] and $
                thisvalue lt somearea[i-2,j-1] and $
                thisvalue lt somearea[i-2,j+1] and $
                thisvalue lt somearea[i-1,j+2] and $
                thisvalue lt somearea[i+1,j+2] and $
                thisvalue lt somearea[i+2,j-1] and $
                thisvalue lt somearea[i+1,j+1] and $
                thisvalue lt somearea[i-1,j-2] and $
                thisvalue lt somearea[i+2,j-2] and $
                thisvalue lt somearea[i-2,j-2] and $
                thisvalue lt somearea[i-2,j+2] and $
                thisvalue lt somearea[i+2,j-2] and $
                thisvalue lt somearea[i+2,j+2] then begin
                    atmp[i,j] = 200000
                endif
            endif
        endif
    endfor
endfor


; Where the fuck did this number come from
; .002 s to run for-loop
; 10x faster than old method


loadct,15
cgimage,atmp,/k

; Looking into alternative method instead of nested for loop

tic
a=localmax(somearea,fidfloor,threshold)
toc

; cgimage,im*tyim,/k

; 54,108
; 101,124
; 117,55
; 151,139
; 167,70








stop

RETURN,1
end