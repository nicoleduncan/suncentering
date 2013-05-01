FUNCTION countsuns, input

; radius = 5
; kernel = SHIFT(DIST(2*radius+1), radius, radius) LE radius

; ; Apply the opening operator to the image.
; openImage = MORPH_OPEN(input, kernel, /GRAY)

; mask = openImage GE 20

; blobs = Obj_New('blob_analyzer', openImage, MASK=mask, SCALE=[0.5, 0.5])

; ; Display the original image
; s = Size(input, /DIMENSIONS)
; Window, XSIZE=2*s[0], YSIZE=2*s[1], Title='Blob Analyzer Example'
; LoadCT, 0
; cgImage, input, 0, /TV

; ; Display the opened input beside it.
; cgImage, openImage, 1, /TV,/noerase

; ; Display the blobs we located with LABEL_REGION.
; count = blobs -> NumberOfBlobs()
; blank = BytArr(s[0], s[1])
; FOR j=0,count-1 DO BEGIN
; 	blobIndices = blobs -> GetIndices(j)
; 	blank[blobIndices] = input[blobIndices]
; ENDFOR
; cgImage, blank, 2, /TV,/noerase

; ; Display the original input, with blob outlined and labelled.
; cgImage, input, 3, /TV,/noerase
; FOR j=0,count-1 DO BEGIN
; 	stats = blobs -> GetStats(j, /NoScale)
; 	PLOTS, stats.perimeter_pts[0,*] + s[0], stats.perimeter_pts[1,*], /Device, COLOR=cgColor('dodger blue')
; 	XYOUTS, stats.center[0]+s[0], stats.center[1]-5, /Device, StrTrim(j,2), $
; 	    COLOR=cgColor('red'), ALIGNMENT=0.5, CHARSIZE=0.75
; ENDFOR

; ; Report stats.
; blobs -> ReportStats

; ; Destroy the object.
; Obj_Destroy, blobs
  
; Technically, we only need:
; radius = 5
; kernel = SHIFT(DIST(2*radius+1), radius, radius) LE radius
; openImage = MORPH_OPEN(input, kernel, /GRAY)
; mask = openImage GE 20
; blobs = Obj_New('blob_analyzer', openImage, MASK=mask, SCALE=[0.5, 0.5])
; count = blobs -> NumberOfBlobs()
; ; count, but maybe there's parts of this program I can tear apart?
; Obj_Destroy, blobs



; Welp. it's so fast.

; tic
bimask = input gt 20
s = Size(bimask, /DIMENSIONS)
fixLabelRegionImage = BytArr(s + 2)
; need this because label_region assumes pixels at edge to be 0
fixLabelRegionImage[1,1] = bimask
labelRegionOutput = Label_Region(fixLabelRegionImage)
labelRegionOutput = labelRegionOutput[1:s[0], 1:s[1]]
h = Histogram(labelRegionOutput, MIN=1, REVERSE_INDICES=ri, BINSIZE=1)

; reg1 = ri[ri[0]:ri[1]-1]
; reg2 = ri[ri[2]:ri[3]-1]
; reg3 = ri[ri[4]:ri[5]-1]
; toc

stop
RETURN, n_elements(h ne 1)
END