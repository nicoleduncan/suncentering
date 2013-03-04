; pro rottest

for i = 1,4 do begin
which=i
case which of
    1 : begin
        xcenter = 210
        ycenter = 153
        wholeimage = BYTSCL(READ_TIFF('plots_tables_images/dimsun1.tiff',channels=1))
        thresh = -80
    end
    2 : begin 
        xcenter = 210
        ycenter = 153
        wholeimage = BYTSCL(READ_TIFF('plots_tables_images/dimsun_ideal.tiff',channels=1))
        thresh = -150
    end
    3 : begin
        xcenter = 210
        ycenter = 153
        wholeimage = BYTSCL(READ_TIFF('plots_tables_images/gauss1pix.tiff',channels=1))
        thresh = -160
    end
    4 : begin
        xcenter = 210
        ycenter = 153
        wholeimage = BYTSCL(READ_TIFF('plots_tables_images/displacement.tiff',channels=1))
        thresh = -160
    end

endcase

rad = 22
; Old value used to be 40, but let's just go ahead and eliminate the ring of the sun 
; since it's proving to be a problem.
crop = wholeimage[xcenter-rad:xcenter+rad,ycenter-rad:ycenter+rad]

rot_1 = ROT(crop,1,/interp)
rot_2 = ROT(crop,2,/interp)
rot_3 = ROT(crop,3,/interp)
rot_4 = ROT(crop,4,/interp)
rot_5 = ROT(crop,5,/interp)

; im= ROT(rot_1,-1,/interp )
im = rot_1
; With dimsun1.tiff, gets really shitty at 2 deg
; with fidsun copy, rot_1 goes from 2 dots to lines... and already tilted.
s = SIZE(im,/dim)
nrow = s[0]
ncol = s[1]

xpb = (SHIFT_DIFF(EMBOSS(im),dir=3)) lt thresh
ypb = (SHIFT_DIFF(EMBOSS(im, az=90),dir=1)) lt thresh

window,i
!p.multi=[0,2,1]
cgimage,xpb*im,/k
cgimage,ypb*im,/k
!p.multi=0

ind_col = WHERE(xpb eq 1) mod ncol
ind_row = where(ypb eq 1)/nrow

; IF WE HAD 3 ROWS/COLS OF FIDUCIALS, WHICH WE DON'T ANYMORE 
; a = mode(ind_col)
; b = ind_col[where(ind_col ne a)]
; c = mode(b)
; d = ind_col[where(ind_col ne a and ind_col ne c)]
; e = mode(d)

; f = mode(ind_row)
; g = ind_row[where(ind_row ne f)]
; h = mode(g)
; i = ind_col[where(ind_col ne f and ind_col ne h)]
; j = mode(i)

; ; Just to make it sorted
; xpos = [a,c,e]
; ypos = [f,h,j]
; xpos = xpos[sort(xpos)]
; ypos = ypos[sort(ypos)]

; ; Because fiducials are 2 pixels wide 
; xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1],xpos[2]-1,xpos[2]]
; ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1],ypos[2]-1,ypos[2]]

a = mode(ind_col)
b = ind_col[WHERE(ind_col ne a)]
c = mode(b)

f = mode(ind_row)
g = ind_row[WHERE(ind_row ne f)]
h = mode(g)

; Just to make it sorted
xpos = [a,c]
ypos = [f,h]
xpos = xpos[SORT(xpos)]
ypos = ypos[SORT(ypos)]

; Because fiducials are 2 pixels wide 
xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1]]
ymask = [ypos[0]-1,ypos[0],ypos[1]-1,xpos[1]]

; In terms of the whole image
rxmask = xpos + xcenter-rad - 0.5
rymask = ypos + ycenter-rad - 0.5

print,rxmask
print,rymask

; cgimage,im,output=STRCOMPRESS(SCOPE_VARNAME(rot_1),/rem)+'_'+STRCOMPRESS(i,/rem)+'.eps',/k

endfor
stop
END