; docformat = 'rst'

PRO doesntfindanything,xpos,ypos,file=file
;+
; :OBSOLETE:
; :KEYWORDS:
;			file
;
; :DESCRIPTION:
;			Finds the centroid by looking at strips of data
;
; :RETURNS:
;			xpos - x center
;			ypos - y center
;
; :EXAMPLES:
;			comp4,xpos,ypos
;
;-

IF ~keyword_set(file) THEN file = '104533_20120911_153147_254618_0.bin'
; cropped_image = newbox(file='$PWD/sep11_postit/104533_20120912_124300_353097_0.bin')
cropped_image = scanbox(file='Sun_Images_000000.bmp')

start = systime(1,/seconds)

; cgimage,cropped_image,/keep_asp
s = size(cropped_image,/dimensions)
length = s[0]
height = s[1]
; Arbitrary value
scan_width = 10

; Getting some values
col_chords = fltarr(length/scan_width)
row_chords = fltarr(height/scan_width)
thresh = mean(cropped_image)
; thresh = max(cropped_image)/2.

rowchord_endpoints = fltarr(2,height/scan_width)
colchord_endpoints = fltarr(2,length/scan_width)

; Counting the lenGTh of chords where cropped_image > thresh
FOR i = 0,length/scan_width-1 DO BEGIN
	col_where = where(cropped_image[i*scan_width,*] GT thresh,col_num)
	col_chords[i] = col_num
	; beginning of chord
	colchord_endpoints[0,i] = col_where[0]
	; end of chord
	colchord_endpoints[1,i] = col_where[-1]
ENDFOR

FOR i = 0,height/scan_width -1 DO BEGIN
	row_where = where(cropped_image[*,i*scan_width] GT thresh,row_num)
	row_chords[i] = row_num
	; beginning of chord
	rowchord_endpoints[0,i] = row_where[0]
	; end of chord
	rowchord_endpoints[1,i] = row_where[-1]
ENDFOR

rowmeanvalue = mean(rowchord_endpoints[where(rowchord_endpoints ne -1)])
colmeanvalue = mean(colchord_endpoints[where(colchord_endpoints ne -1)])

xpos = rowmeanvalue
ypos = colmeanvalue


; cropped_image[*,fix(ypos)]=0
; cropped_image[fix(xpos),*]=0
; cgimage,cropped_image,/keep_asp

finish = systime(1,/seconds)
print, 'Elapsed Time: ',strcompress(finish-start,/remove),' seconds'
RETURN
END