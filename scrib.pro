; PRO scrib

xpos = 210.81349
ypos = 153.94748

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

s = size(crop,/dim)
nrow = s[0]
ncol = s[1]

xpb = (shift_Diff(emboss(crop))) lt -75
ypb = (shift_Diff(emboss(crop, az=90))) lt -75

xpeaks = xpb*((shift_Diff(emboss(crop))))
ypeaks = ypb*((shift_Diff(emboss(crop, az=90))))

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

xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1]]
ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1]]

; Slow way:
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