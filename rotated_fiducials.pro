xpos = 240
ypos = 140

a=READ_TIFF('plots_tables_images/5deg_blotchy.tiff')


a=REFORM(a[0,*,*])

wholeimage = BYTSCL(a)
rad = 40

crop = wholeimage[xpos-rad:xpos+rad,ypos-rad:ypos+rad]
cgimage,crop,/k

s = SIZE(crop,/dim)
nrow = s[0]
ncol = s[1]

; Had to take out dir=3 since now it's at an arbitrary angle
xpb = (SHIFT_DIFF(EMBOSS(crop))) lt -150
ypb = (SHIFT_DIFF(EMBOSS(crop, az=90))) lt -150

!p.multi=[0,2,1]
cgimage,xpb*crop,/k
cgimage,ypb*crop,/k
!p.multi=0

ind_col = where(xpb eq 1) mod ncol
ind_row = where(ypb eq 1)/nrow


a = mode(ind_col)
b = ind_col[where(ind_col ne a)]
c = mode(b)
d = ind_col[where(ind_col ne a and ind_col ne c)]
e = mode(d)

f = mode(ind_row)
g = ind_row[where(ind_row ne f)]
h = mode(g)
i = ind_col[where(ind_col ne f and ind_col ne h)]
j = mode(i)

; Just to make it sorted
xpos = [a,c,e]
ypos = [f,h,j]
xpos = xpos[sort(xpos)]
ypos = ypos[sort(ypos)]

; Because fiducials are 2 pixels wide 
xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1],xpos[2]-1,xpos[2]]
ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1],ypos[2]-1,ypos[2]]

stop

END