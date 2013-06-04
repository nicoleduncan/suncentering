FUNCTION localmax, img, floor, ceiling

; .01 to shift 24 times
; This seems super incredibly inefficient
; only 2x faster than nested forloop
; HAS TO BE a better way

; outer 8
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

s = SIZE(img,/dim)
fids = WHERE(m eq 1,n_count)
xpos = fids mod s[0]
ypos = fids/s[1]

RETURN,{x:xpos,y:ypos}
end