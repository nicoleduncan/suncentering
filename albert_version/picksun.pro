FUNCTION picksun, inputimage, inputstruct
;+
;   :Description:
;       Decides which suns to ignore. Creates a mask with the bottom corners cut off and then creates a bordermask of the remaining area. Any center positions of suns within this bordermask are deemed partial.
;
;   :Params:
;       inputimage: in, required
;           The raw input image
;
;       inputstruct: in, required, type=structure
;           Structure containing all the solar information
;
;   :Keywords:
;       
;-

s = SIZE(inputimage,/d)
n_col = s[0]
n_row = s[1]
n = !param.triangle_size*s[0]

amask = FLTARR(s) + 1

i = REBIN(INDGEN(n), n, n)           
j = REBIN(TRANSPOSE(INDGEN(n)), n, n)
botleft = ROTATE(i ge j,1)
botright = j ge i

amask[0,0] = botleft
amask[(1 - !param.triangle_size)*s[0],0]=botright
; 1296*966
padding = 100
paddedimage = FLTARR(s+padding*2)
paddedimage[padding,padding]=amask

; ghetto erode:
; pad mask with lots of 0s
; shift mask right, set to 1
right = SHIFT(paddedimage , !param.border_pad,0)
; shift mask left, ditto
left = SHIFT(paddedimage, - !param.border_pad,0)
; shift mask up
up = SHIFT(paddedimage,0 , !param.border_pad)
; shift mask down
down = SHIFT(paddedimage,0, - !param.border_pad)

; This is sketchy, only can do this because we know for exact the shape of the triangle
side = SQRT( ( !param.border_pad^2)/2)

upright = SHIFT(paddedimage,-side,side)
upleft = SHIFT(paddedimage,side,side)

; multiple all masks together
tiny = right*left*up*down*upright*upleft
; unpad
fixedmask = tiny[padding:s[0]+padding-1,padding:s[1]+padding-1]

; take that erode
; cgimage,(amask-fixedmask)*rotate(FINDGEN(s),2),/k

a = fixedmask

; This works, right?

if N_ELEMENTS(inputstruct) eq 1 then begin
    a[inputstruct[0].xpos,inputstruct[0].ypos] = !values.f_nan
    if MEAN(a) eq !values.f_nan then inputstruct[0].partial = 1
endif else begin
    for i = 0,N_ELEMENTS(inputstruct)-1 do begin
        a[inputstruct[i].xpos,inputstruct[i].ypos] = !values.f_nan
        if MEAN(a) eq !values.f_nan then inputstruct[i].partial = 1
        a = fixedmask
    endfor
endelse

RETURN,inputstruct
end