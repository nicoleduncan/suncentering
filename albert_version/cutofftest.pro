FUNCTION cutofftest, inputimage, inputstruct

crop = inputimage[inputstruct.limbxpos - !param.crop_box:inputstruct.limbxpos + !param.crop_box,inputstruct.limbypos - !param.crop_box:inputstruct.limbypos + !param.crop_box]
s = size(crop,/d)
prime = quickmask(crop,inputstruct.thresh)
loadct,13


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









; ps_start,filename='cutofftestside.eps',/encap,xsize=6,ysize=8
!p.multi=[0,3,2]
for i = 0, 5 do begin
   somecrop = crop[35 + i*5:s[0]-1,0:s[1]-1]
   somec = quickmask (somecrop,inputstruct.thresh)
   somecrop[somec.xpos-1:somec.xpos+1,somec.ypos-1:somec.ypos+1]=255
   somecrop[prime.xpos-(35 + i*5) - 1:prime.xpos-(35+i*5) + 1 ,prime.ypos-1:prime.ypos+1]=180
   print,prime.xpos-(35 + i*5) - somec.xpos
   print,prime.ypos - somec.ypos
   a=where(somecrop gt inputstruct.thresh,n_pix)
   print,n_pix
   cgimage,somecrop,/k
endfor
; ps_end

print, 'interim'

;================================================================================

;================================================================================

; ps_start,filename='cutofftestcorner.eps',/encap,xsize=6,ysize=8
!p.multi=[0,3,2]
for i = 0, 5 do begin
   somecrop = crop[35 + i*5:s[0]-1,35 + i*5:s[1]-1]
   somec = quickmask(somecrop,inputstruct.thresh)
   somecrop[somec.xpos-1:somec.xpos+1,somec.ypos-1:somec.ypos+1]=255
   somecrop[prime.xpos-(35 + i*5) - 1:prime.xpos-(35+i*5) + 1 ,prime.ypos-(35+ i*5) -1:prime.ypos-(35+i*5)+1]=180
   cgimage,somecrop,/k
   a=where(somecrop gt inputstruct.thresh,n_pix)
   print,n_pix
   print,prime.xpos-(35 + i*5) - somec.xpos
   print,prime.ypos-(35 + i*5) - somec.ypos
endfor
; ps_end
; What are we looking for in particular here?

stop

RETURN,1
end