FUNCTION localmax, img, floor, ceiling

; .005 to shift 24 times
; This seems super incredibly inefficient
; HAS TO BE a better way

; I tried forlooping over a smaller interval (3400 instead of 58000) but it still took .04 s

; ; outer 8
dx1 = SHIFT(img,-1,-1)
dx2 = SHIFT(img,-1,0)
dx3 = SHIFT(img,-1,1)
dx4 = SHIFT(img,1,-1)
dx5 = SHIFT(img,1,0)
dx6 = SHIFT(img,1,1)
dx7 = SHIFT(img,0,-1)
dx8 = SHIFT(img,0,1)
; outer 16
dx9 = SHIFT(img,-2,-2)
dx10 = SHIFT(img,-2,-1)
dx11 = SHIFT(img,-2,0)
dx12 = SHIFT(img,-2,1)
dx13 = SHIFT(img,-2,2)
dx14 = SHIFT(img,2,-2)
dx15 = SHIFT(img,2,-1)
dx16 = SHIFT(img,2,0)
dx17 = SHIFT(img,2,1)
dx18 = SHIFT(img,2,2)
dx19 = SHIFT(img,0,-2)
dx20 = SHIFT(img,0,2)
dx21 = SHIFT(img,-1,-2)
dx22 = SHIFT(img,-1,2)
dx23 = SHIFT(img,1,-2)
dx24 = SHIFT(img,1,2)

m = (img lt dx1) and (img lt dx2) and (img lt dx3) and (img lt dx4) and $
(img lt dx5) and (img lt dx6) and (img lt dx7) and (img lt dx8) and $
(img lt dx9) and (img lt dx10) and (img lt dx11) and (img lt dx12) and $
(img lt dx13) and (img lt dx14) and (img lt dx15) and (img lt dx16) and $
(img lt dx17) and (img lt dx18) and (img lt dx19) and (img lt dx20) and $
(img lt dx21) and (img lt dx22) and (img lt dx23) and (img lt dx24) and $ 
(img gt floor) and (img lt ceiling)

; n = (img gt floor) and (img lt ceiling)

s = SIZE(img,/dim)
fids = WHERE(m eq 1,n_count)
xpos = fids mod s[0]
ypos = fids/s[1]

; ; 58000 -> 3400, not much of an improvement...
; xx=0
; for i = 0,n_count-1 do begin
;     if img[xpos[i],ypos[i]] lt img[xpos[i]-1,ypos[i]] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+1,ypos[i]] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i],ypos[i]-1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i],ypos[i]+1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+1,ypos[i]+1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]-1,ypos[i]+1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]-1,ypos[i]-1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+1,ypos[i]-1] and $ 

;     img[xpos[i],ypos[i]] lt img[xpos[i]-2,ypos[i]-2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]-2,ypos[i]-1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]-2,ypos[i]] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]-2,ypos[i]+1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]-2,ypos[i]+2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+2,ypos[i]-2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+2,ypos[i]-1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+2,ypos[i]] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+2,ypos[i]+1] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+2,ypos[i]+2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]-1,ypos[i]-2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]-1,ypos[i]+2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i],ypos[i]-2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i],ypos[i]+2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+1,ypos[i]-2] and $
;     img[xpos[i],ypos[i]] lt img[xpos[i]+1,ypos[i]+2] then begin
;         if total(xx) eq 0 then begin
;             xx = xpos[i]
;             yy = ypos[i] 
;         endif else begin
;             xx=[xx,xpos[i]]
;             yy=[yy,ypos[i]]
;         endelse
;     endif
; endfor

subpx = fltarr(n_count,/nozero)
subpy = fltarr(n_count,/nozero)

for k = 0,n_count-1 do begin
    z = img[xpos[k]-1:xpos[k]+1,ypos[k]-1:ypos[k]+1]
    result = paradip(z)
    ; result = parapeak(1/z)
    subpx[k] = xpos[k] + result[0]
    subpy[k] = ypos[k] + result[1]
endfor

RETURN,{x:xpos,y:ypos,subpx:subpx,subpy:subpy}
end