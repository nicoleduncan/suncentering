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









; We're going to use a Gordon-approved method here:


a=fan(fltarr(100)+200,100)
ww = 12
a[ww,ww]=1
b = fltarr(300,300,/nozero)
b[100,100]=a
c=b
b[*,110]=0
b[*,190]=0
b[110,*]=0
b[190,*]=0

b[*,150]=!magenta
window,0
cgimage,b,/k;,/display,output='firstcheck.eps'


b=c
window,1
b = rot(b,-45)
b[*,110]=!red
b[*,190]=!red
b[110,*]=!red
b[190,*]=!red
b[*,100]=!green
b[*,200]=!green
b[100,*]=!green
b[200,*]=!green
cgimage,b,/k;,output='seccheck.eps',/display


xpos = mean(where(b eq 1) mod 300)
ypos = mean(where(b eq 1) / 300)

print,xpos,ypos
x0=100+ww
y0=100+ww
; went from [120,120] to [150,107]

sa = size(a,/dim)
sb = size(b,/dim)

xc = (sb[0])/2.
yc = (sb[1])/2.

r = sqrt((xc-x0)^2 + (yc-y0)^2)

if x0 gt xc then offset = 0
if y0 gt yc and x0 lt xc then offset = 180
if y0 lt yc and x0 lt xc then offset = -180

theta = !radeg*atan((yc-y0),(xc-x0))+ offset

newx = r*cos((theta + 45)*!dtor)
newy = r*sin((theta + 45)*!dtor)
xpos = newx+xc
ypos = newy+yc
print,xpos,ypos

; good job, now to associate it with borders

; this is fine for the normal sides

if y0 lt yc then begin
    if x0 lt 100+.1*sa[0] or x0 gt 100+.9*sa[0] or y0 lt 100+.1*sa[1] or y0 gt 100+.9*sa[1] or xpos lt 100 or xpos gt 200 or ypos lt 100 or ypos gt 200 then begin
    ; Have to do something special for corners. The params will affect how big the triangle is
        ; print,"you're too close to the  normal box edges"
        print,"> > > you're too close to the corner"
    endif
endif














stop
; seriously? 2k times faster?

x0=inputstruct[0].xpos
y0=inputstruct[0].ypos

im = inputimage

im[x0-1:x0+1,y0-1:y0+1] = !red
rim = rot(im,-45)
window,0
cgimage,im,/k
window,1
cgimage,rim,/k

spot = where(rim eq !red)

xx = mean(spot mod s[0])
yy = mean(spot / s[0])

print,"according to rot(), center is at"
print,xx,yy

xc = (s[0])/2.
yc = (s[1])/2.

r = sqrt((xc-x0)^2 + (yc-y0)^2)

if x0 gt xc then offset = 0 else begin
    if y0 gt yc then offset = 180*!dtor
    if y0 lt yc then offset = -180*!dtor
endelse

theta = atan(ABS(yc-y0),ABS(xc-x0))+ offset

twist = 45*!dtor
newx = r*cos((theta + twist))
newy = r*sin((theta + twist))
xpos = newx+xc
ypos = newy+yc


print,x0,y0
print,''
print,'according to my function, center is at'
print,xpos,ypos

if x0 lt .1*s[0] or x0 gt .9*s[0] or y0 lt .1*s[1] or y0 gt .9*s[1] or xpos lt 0 or xpos gt s[0] then print,"no good"










stop




























RETURN,inputstruct
end