; PRO scrib

; Brightest sun
xpos = 210.81349
ypos = 153.94748

; xpos = 240
; ypos = 140

file = 'dimsun1.fits'
wholeimage = mrdfits(file)

rad = 21
; This is the main sun image
crop = wholeimage[xpos-rad:xpos+rad,ypos-rad:ypos+rad]

; Ani is the outline of the original fiducial positions
ani = fltarr(43,43)

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


; Alright alright, we can use emboss

; One pass with emboss() to find x position and one with emboss(az=90) to find y position
; Using crop or looksnice, doesn't change emboss much either way
;
; shift_diff(emboss(crop)) works too

; display, bytscl((shift_Diff(emboss(crop))) lt -75)*((shift_Diff(emboss(crop))) lt -75) 
; plot_edges,ani

; Just to get dimensions 
s = size(crop,/dim)
nrow = s[0]
ncol = s[1]

; Lines representing the x and y fiducials. Since the fiducials are 2 pixels wide, 
; these lines represent the top right corner of the center box. 
;
; To see, plot with:
; display,bytscl(crop),/square
; plot_Edges,xpb
; plot_edges,ypb

; start=systime(1,/s)
xpb = (shift_Diff(emboss(crop))) lt -75
ypb = (shift_Diff(emboss(crop, az=90))) lt -75

; Not using this... yet
xpeaks = xpb*((shift_Diff(emboss(crop))))
ypeaks = ypb*((shift_Diff(emboss(crop, az=90))))
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


crop[xmask[0],ymask[0]]=250
crop[xmask[0],ymask[1]]=250
crop[xmask[0],ymask[2]]=250
crop[xmask[0],ymask[3]]=250
crop[xmask[1],ymask[0]]=250
crop[xmask[1],ymask[1]]=250
crop[xmask[1],ymask[2]]=250
crop[xmask[1],ymask[3]]=250
crop[xmask[2],ymask[0]]=250
crop[xmask[2],ymask[1]]=250
crop[xmask[2],ymask[2]]=250
crop[xmask[2],ymask[3]]=250
crop[xmask[3],ymask[0]]=250
crop[xmask[3],ymask[1]]=250
crop[xmask[3],ymask[2]]=250
crop[xmask[3],ymask[3]]=250

display,bytscl(crop),/square
plot_edges,xpb,thick=2
plot_edges,ypb,thick=2

; loadct,19

; ps_start,filename='fid.eps',/encapsulated,quiet=1
;    display,bytscl(crop),/square
; ps_end,resize=100

; ps_start,filename='fidoutline.eps',/encapsulated,quiet=1
;    display,bytscl(crop),/square
;    plot_edges,xpb,thick=4
;    plot_edges,ypb,thick=4
; ps_end,resize=100

stop

!p.multi=[0,3,2]
window,8,xsize=1000,ysize=800
cgimage,crop,/k,/axes,title='Original',charsize=3
cgimage,emboss(crop),/k,title='Emboss Filter',/axes,charsize=3
cgimage,shift_Diff(emboss(crop)),/k,title='Emboss + Shift_Diff Filter',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -60)*(shift_Diff(emboss(crop))),/k,title='Values lt -60',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -70)*(shift_Diff(emboss(crop))),/k,title='Values lt -70',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -80)*(shift_Diff(emboss(crop))),/k,title='Values lt -80',/axes,charsize=3
!p.multi=0

stop

;***************************************************************************************************************
;                                                                                                              *
;                                                                                                              *
;                                                                                                              *
;                                                                                                              *
;                                                                                                              *
;                                                                                                              *
;***************************************************************************************************************

; 50% sun
xpos = 338.28275
ypos = 77.370949

file = 'dimsun1.fits'
wholeimage = mrdfits(file)

rad = 21
; This is the main sun image
crop = wholeimage[xpos-rad:xpos+rad,ypos-rad:ypos+rad]

; Ani is the outline of the original fiducial positions
ani = fltarr(43,43)

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

s = size(crop,/dim)
nrow = s[0]
ncol = s[1]

xpb = (shift_Diff(emboss(crop))) lt -75
ypb = (shift_Diff(emboss(crop, az=90))) lt -75

xpeaks = xpb*((shift_Diff(emboss(crop))))
ypeaks = ypb*((shift_Diff(emboss(crop, az=90))))

ind_col = where(xpb eq 1) mod ncol
ind_row = where(ypb eq 1)/nrow

a = mode(ind_col)
b = ind_col[where(ind_col ne a)]
c = mode(b)

d = mode(ind_row)
e = ind_row[where(ind_row ne d)]
f = mode(e)

xpos = [a,c]
ypos = [d,f]
xpos = xpos[sort(xpos)]
ypos = ypos[sort(ypos)]

xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1]]
ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1]]

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

!p.multi=[0,3,2]
window,9,xsize=1000,ysize=800
cgimage,crop,/k,/axes,title='Original',charsize=3
cgimage,emboss(crop),/k,title='Emboss Filter',/axes,charsize=3
cgimage,shift_Diff(emboss(crop)),/k,title='Emboss + Shift_Diff Filter',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -30)*(shift_Diff(emboss(crop))),/k,title='Values lt -30',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -40)*(shift_Diff(emboss(crop))),/k,title='Values lt -40',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -50)*(shift_Diff(emboss(crop))),/k,title='Values lt -50',/axes,charsize=3
!p.multi=0

;***************************************************************************************************************
;                                                                                                              *
;                                                                                                              *
;                                                                                                              *
;                                                                                                              *
;                                                                                                              *
;                                                                                                              *
;***************************************************************************************************************

; 25% sun
xpos = 78.717346
ypos = 235.50647

file = 'dimsun1.fits'
wholeimage = mrdfits(file)

rad = 21
; This is the main sun image
crop = wholeimage[xpos-rad:xpos+rad,ypos-rad:ypos+rad]

; Ani is the outline of the original fiducial positions
ani = fltarr(43,43)

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

s = size(crop,/dim)
nrow = s[0]
ncol = s[1]

xpb = (shift_Diff(emboss(crop))) lt -75
ypb = (shift_Diff(emboss(crop, az=90))) lt -75

xpeaks = xpb*((shift_Diff(emboss(crop))))
ypeaks = ypb*((shift_Diff(emboss(crop, az=90))))

ind_col = where(xpb eq 1) mod ncol
ind_row = where(ypb eq 1)/nrow

a = mode(ind_col)
b = ind_col[where(ind_col ne a)]
c = mode(b)

d = mode(ind_row)
e = ind_row[where(ind_row ne d)]
f = mode(e)

xpos = [a,c]
ypos = [d,f]
xpos = xpos[sort(xpos)]
ypos = ypos[sort(ypos)]

xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1]]
ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1]]

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

!p.multi=[0,3,2]
window,10,xsize=1000,ysize=800
cgimage,crop,/k,/axes,title='Original',charsize=3
cgimage,emboss(crop),/k,title='Emboss Filter',/axes,charsize=3
cgimage,shift_Diff(emboss(crop)),/k,title='Emboss + Shift_Diff Filter',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -10)*(shift_Diff(emboss(crop))),/k,title='Values lt -10',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -15)*(shift_Diff(emboss(crop))),/k,title='Values lt -15',/axes,charsize=3
cgimage,((shift_Diff(emboss(crop))) lt -20)*(shift_Diff(emboss(crop))),/k,title='Values lt -20',/axes,charsize=3
!p.multi=0

stop
; Subtracting the original field from a median'ed field
looksnice = 90>crop-median(crop,6)<60  

strucelem = [[0,0,1,1,0,0],$
            [0,0,1,1,0,0],$
            [1,1,1,1,1,1],$
            [1,1,1,1,1,1],$
            [0,0,1,1,0,0],$
            [0,0,1,1,0,0]]

smelem = [[0,1,1,0],$
         [1,1,1,1],$
         [1,1,1,1],$
         [0,1,1,0]]

; cgimage,morph_close(crop,strucelem,/gray),/k,/window
; Convolution of median'ed difference with a 2d array that looks like a fiducial

; window,10
; !P.Multi=[0,2,2]
; p = [0.02, 0.1, 0.90, 0.90]
; cgimage,looksnice,/k,/axes,title='ORIGINAL',Position=p
; p = [0.02, 0.1, 0.90, 0.90]
; cgimage,morph_close(looksnice*(looksnice lt -10),strucelem,/gray),/k,title='LOOKSNICE LT -10',/axes, Position=p
; p = [0.02, 0.1, 0.90, 0.90]
; cgimage,morph_close(looksnice*(looksnice lt -8),strucelem,/gray),/k,title='LOOKSNICE LT -8',/axes, Position=p
; p = [0.02, 0.1, 0.90, 0.90]
; cgimage,morph_close(looksnice*(looksnice lt -6),strucelem,/gray),/k,title='LOOKSNICE LT -6',/axes, Position=p
; !P.Multi =0

;<---
;Same Plots
;-->
loadct,8
window,9
!P.Multi=[0,2,2]
p = [0.02, 0.1, 0.90, 0.90]
cgimage,looksnice,/k,/axes,title='ORIGINAL',Position=p
p = [0.02, 0.1, 0.90, 0.90]
cgimage,looksnice*(looksnice lt -10),/k,title='LOOKSNICE LT -10',/axes, Position=p
p = [0.02, 0.1, 0.90, 0.90]
cgimage,looksnice*(looksnice lt -8),/k,title='LOOKSNICE LT -8',/axes, Position=p
p = [0.02, 0.1, 0.90, 0.90]
cgimage,looksnice*(looksnice lt -6),/k,title='LOOKSNICE LT -6',/axes, Position=p
!P.Multi =0

; This doesn't even help.
tss = fltarr((size(crop,/dim))[0],(size(crop,/dim))[0])
FOR i = 0,(size(crop,/dim))[0] - 1 DO BEGIN
    tss[*,i] = ts_SMOOTH(crop[*,i],3)
ENDFOR

stop
; Original cropped area
display,crop,/square,title='Original'
; plot_edges outlines the fiducials
plot_edges,ani

; One of the edge-detection filters
display,bytscl(shift_diff(crop)),/square,title='SHIFT_DIFF()'
plot_edges,ani

; Another filter
display,bytscl(laplacian(crop)),/square,title='LAPLACIAN()'
plot_edges,ani


;************************************************
;************************************************
;************************************************

orig_image = wholeimage
 
; Crop the image to focus in on the bridges:
croppedSize = [96, 96]
croppedImage = orig_image[150:(croppedSize[0] - 1) + 200, $
   100:(croppedSize[1] - 1) + 100]
croppedimage = crop   
   ; stop
 
; Display original image.
img01 = IMAGE(croppedImage, $
   TITLE = "Original", $
   LAYOUT = [4, 2, 1], DIMENSIONS = [640, 400])
 
; Apply Roberts filter.
robimage = ROBERTS(croppedImage)
img02 = IMAGE(robimage, $
   TITLE = "Roberts Filter", /CURRENT, $
   LAYOUT = [4, 2, 2])
 
; Apply Sobel filter.
sobimage = SOBEL(croppedImage)
img03 = IMAGE(sobimage, $
   TITLE = "Sobel Filter", /CURRENT, $
   LAYOUT = [4, 2, 3])
 
; Apply Prewitt filter.
prewimage = PREWITT(croppedImage)
img04 = IMAGE(prewimage, $
   TITLE = "Prewitt Filter", /CURRENT, $
   LAYOUT = [4, 2, 4])
 
; Apply SHIFT_DIFF filter.
shiftimage = SHIFT_DIFF(croppedImage)
img05 = IMAGE(shiftimage, $
   TITLE = "SHIFT_DIFF Filter", /CURRENT, $
   LAYOUT = [4, 2, 5])
 
; Apply EDGE_DOG filter.
edgedogimage = EDGE_DOG(croppedImage)
img06 = image(edgedogimage, $
   TITLE = "EDGE_DOG Filter", /CURRENT, $
   LAYOUT = [4,2,6])
 
; Apply Laplacian filter.
lapimage = LAPLACIAN(croppedImage)
img07 = IMAGE(lapimage, $
   TITLE = "Laplacian Filter", /CURRENT, $
   LAYOUT = [4, 2, 7])
 
; Apply EMBOSS filter.
embossimage = EMBOSS(croppedImage)
img08 = IMAGE(embossimage, $
   TITLE = "EMBOSS Filter", /CURRENT, $
   LAYOUT = [4, 2, 8])





END