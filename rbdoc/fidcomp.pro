; Brightest sun
xpos = 210.81349
ypos = 153.94748

xpos = 240
ypos = 140

a=READ_TIFF('obsolete_code/shitsun.tiff')
; a=read_tiff('obsolete_code/circfid.tiff')

; We can have circle fiducials but they are more prone to misalignment.

a=REFORM(a[0,*,*])

; file = 'betterfake.fits'
; wholeimage = mrdfits(file)
wholeimage = BYTSCL(a)
rad = 40
; This is the main sun image
crop = wholeimage[xpos-rad:xpos+rad,ypos-rad:ypos+rad]

; Ani is the outline of the original fiducial positions
ani = FLTARR(43,43)

ani[16,5]=1
ani[17,5]=1
ani[16,6]=1
ani[17,6]=1

ani[36,5]=1
ani[37,5]=1
ani[36,6]=1
ani[37,6]=1

ani[16,25]=1
ani[16,26]=1
ani[17,25]=1
ani[17,26]=1

ani[36,25]=1
ani[37,25]=1
ani[36,26]=1
ani[37,26]=1

;************************************************
;************************************************
;************************************************

s = SIZE(crop,/dim)
nrow = s[0]
ncol = s[1]

xpb = (SHIFT_DIFF(EMBOSS(crop),dir=3)) lt -150
ypb = (SHIFT_DIFF(EMBOSS(crop, az=90),dir=3)) lt -150

; Not using this... yet
xpeaks = xpb*((SHIFT_DIFF(EMBOSS(crop),dir=3)))
ypeaks = ypb*((SHIFT_DIFF(EMBOSS(crop, az=90),dir=3)))

; finish=systime(1,/s)
; print,finish-start
; indices of the rows/columns 
ind_col = where(xpb eq 1) mod ncol
ind_row = where(ypb eq 1)/nrow


; There's a better way to do this:
a = mode(ind_col)
b = ind_col[where(ind_col ne a)]
c = mode(b)

d = mode(ind_row)
e = ind_row[where(ind_row ne d)]
f = mode(e)

; Just to make it sorted
xpos = [a,c]
ypos = [d,f]
xpos = xpos[sort(xpos)]
ypos = ypos[sort(ypos)]

; Because fiducials are 2 pixels wide 
xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1]]
ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1]]

; Super slow way, got to be a better way to do this
emask = fltarr(s)
emask[xmask[0],ymask[0]]=1
emask[xmask[0],ymask[1]]=1
emask[xmask[0],ymask[2]]=1
emask[xmask[0],ymask[3]]=1
emask[xmask[1],ymask[0]]=1
emask[xmask[1],ymask[1]]=1
emask[xmask[1],ymask[2]]=1
emask[xmask[1],ymask[3]]=1
emask[xmask[2],ymask[0]]=1
emask[xmask[2],ymask[1]]=1
emask[xmask[2],ymask[2]]=1
emask[xmask[2],ymask[3]]=1
emask[xmask[3],ymask[0]]=1
emask[xmask[3],ymask[1]]=1
emask[xmask[3],ymask[2]]=1
emask[xmask[3],ymask[3]]=1

; display,bytscl(crop),/square
; plot_edges,emask

!p.multi=[0,3,3]
window,8,xsize=1000,ysize=1000
cgimage,crop,/k,/axes,title='Original',charsize=3
cgimage,EMBOSS(crop),/k,title='EMBOSS Filter',/axes,charsize=3
cgimage,SHIFT_DIFF(EMBOSS(crop),dir=3),/k,title='EMBOSS + SHIFT_DIFF Filter',/axes,charsize=3
cgimage,((SHIFT_DIFF(EMBOSS(crop),dir=3)) lt -130)*(SHIFT_DIFF(EMBOSS(crop),dir=3)),/k,title='Values lt -130',/axes,charsize=3
cgimage,((SHIFT_DIFF(EMBOSS(crop),dir=3)) lt -140)*(SHIFT_DIFF(EMBOSS(crop),dir=3)),/k,title='Values lt -140',/axes,charsize=3
cgimage,((SHIFT_DIFF(EMBOSS(crop),dir=3)) lt -150)*(SHIFT_DIFF(EMBOSS(crop),dir=3)),/k,title='Values lt -150',/axes,charsize=3
cgimage,((SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)) lt -130)*(SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)),/k,title='Values lt -130',/axes,charsize=3
cgimage,((SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)) lt -140)*(SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)),/k,title='Values lt -140',/axes,charsize=3
cgimage,((SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)) lt -150)*(SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)),/k,title='Values lt -150',/axes,charsize=3

!p.multi=0


stop
END