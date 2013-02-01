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

; Subtracting the original field from a median'ed field
looksnice = 80>crop-median(crop,6)<120  

strucelem = [[0,0,1,1,0,0],$
            [0,0,1,1,0,0],$
            [1,1,1,1,1,1],$
            [1,1,1,1,1,1],$
            [0,0,1,1,0,0],$
            [0,0,1,1,0,0]]

; cgimage,morph_close(crop,strucelem,/gray),/k,/window
; Convolution of median'ed difference with a 2d array that looks like a fiducial

window,10
!P.Multi=[0,2,2]
p = [0.02, 0.1, 0.90, 0.90]
cgimage,looksnice,/k,/axes,title='ORIGINAL',Position=p
p = [0.02, 0.1, 0.90, 0.90]
cgimage,morph_close(looksnice*(looksnice lt -10),strucelem,/gray),/k,title='LOOKSNICE LT -10',/axes, Position=p
p = [0.02, 0.1, 0.90, 0.90]
cgimage,morph_close(looksnice*(looksnice lt -8),strucelem,/gray),/k,title='LOOKSNICE LT -8',/axes, Position=p
p = [0.02, 0.1, 0.90, 0.90]
cgimage,morph_close(looksnice*(looksnice lt -6),strucelem,/gray),/k,title='LOOKSNICE LT -6',/axes, Position=p
!P.Multi =0
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