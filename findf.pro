; findf.pro


xpos = 240
ypos = 140

a=READ_TIFF('plots_tables_images/shitsun.tiff',channels=1)


wholeimage = BYTSCL(a)
rad = 40

crop = wholeimage[xpos-rad:xpos+rad,ypos-rad:ypos+rad]

s = SIZE(crop,/dim)
nrow = s[0]
ncol = s[1]

xpb = (SHIFT_DIFF(EMBOSS(crop),dir=3)) lt -150
ypb = (SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)) lt -150

; The sunthetic image has too-nice edges that they end up being edge-detected 
; So I actually didn't anticipate this.

!p.multi=[0,2,1]
cgimage,xpb*crop,/k
cgimage,ypb*crop,/k
!p.multi=0

ind_col = WHERE(xpb eq 1) mod ncol
ind_row = WHERE(ypb eq 1)/nrow


a = mode(ind_col)
b = ind_col[WHERE(ind_col ne a)]
c = mode(b)
d = ind_col[WHERE(ind_col ne a and ind_col ne c)]
e = mode(d)

f = mode(ind_row)
g = ind_row[WHERE(ind_row ne f)]
h = mode(g)
i = ind_col[WHERE(ind_col ne f and ind_col ne h)]
j = mode(i)

; Just to make it sorted
xpos = [a,c,e]
ypos = [f,h,j]
xpos = xpos[SORT(xpos)]
ypos = ypos[SORT(ypos)]

; Because fiducials are 2 pixels wide 
xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1],xpos[2]-1,xpos[2]]
ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1],ypos[2]-1,ypos[2]]

ftest = crop
ftest[a,*] = 100
ftest[c,*] = 100
ftest[e,*] = 100

cgimage,ftest,/k

END