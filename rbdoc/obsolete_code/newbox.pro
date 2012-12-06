; docformat = 'rst'

FUNCTION newbox,file=file,bmp=bmp
;+
; :OBSOLETE:
; :DESCRIPTION: 
;			Since I kind of fucked up boxit, doing another one.
; :KEYWORDS:
;			file
;			bmp
;
; :EXAMPLES:
;		newbox,file='$PWD/sep11_postit/104533_20120912_124300_353097_0.bin'
;-
;FOR novelty
start = systime(1,/seconds)
IF ~keyword_set(file) THEN file = '104533_20120911_153147_254618_0.bin'

;Setting some parameters
n_row 		= 960
n_col 		= 1280
newi		= 0
newj		= 0
xpos		= 0
ypos		= 0
flipimage 	= bytarr(n_col,n_row)
image 		= bytarr(n_col,n_row)

IF keyword_set(bmp) THEN BEGIN
	; Read the bmp file
	tmpimage = read_bmp(file)
	; Get height AND width
	n_col = (size(tmpimage,/dimensions))[1]
	n_row = (size(tmpimage,/dimensions))[2]
	; tmpimage is of dimensions [3,n_col,n_row] so we need to size it down
	image = bytarr(n_col,n_row)

	; Filling the actual image array with data from tmpimage
	FOR i = 0,n_col-1 DO BEGIN
		FOR j = 0,n_row - 1 DO BEGIN
			image[i,j] = tmpimage[0,i,j]
		ENDFOR
	ENDFOR
	
	;Just some thresholds
	themean = mean(image)
	thresh 	= max(image)/2

	; Using a single FOR loop and MOD FOR practice
	; Counting up the indices where image[x_i,y_i] > thresh
	FOR i=0,n_elements(image)-1 DO BEGIN
		x_i = i MOD n_col
		y_i = i/n_col
		IF image[x_i,y_i] GT thresh THEN BEGIN
			xpos+=x_i
			ypos+=y_i
		ENDIF
	ENDFOR

	; Actually taking the positions
	xpos/=n_elements(image[where(image GT thresh)])
	ypos/=n_elements(image[where(image GT thresh)])

	; Look at where the n_elements per row where image > thresh is maximum
	y_sundiameter = n_elements((image[xpos,*])[where(image[xpos,*] GT thresh)])
	x_sundiameter = n_elements((image[*,ypos])[where(image[*,ypos] GT thresh)])

	; Have to use sundiameter < xpos or ELSE it'll break since xpos-sundiameter will be negative and
	; the below FOR loop will always be true
	sundiameter = (x_sundiameter+y_sundiameter)/2
	sundiameter = x_sundiameter
	; sundiameter = 376 		;xpos is 375, 
	cropped_flipimage = bytarr(2*sundiameter,2*sundiameter)

	;weird, the nested FORloop is faster by .2 seconds. What's the deal.
	FOR j=0,n_row-1 DO BEGIN
		FOR i = 0,n_col-1 DO BEGIN
			IF (i GT xpos - sundiameter) AND $
			(i lt xpos + sundiameter) AND $
			(j GT ypos - sundiameter) AND $
			(j lt ypos + sundiameter) THEN BEGIN
			 	cropped_flipimage[newi,newj] = image[i,j]
				newi+=1 
			ENDIF
			IF newi EQ 2*sundiameter-1 THEN BEGIN
				newj+=1
				newi=0
			ENDIF
		ENDFOR
	ENDFOR

ENDIF ELSE BEGIN
	openr,lun,file,/get_lun
	readu,lun,image
	free_lun,lun
	FOR i=0,n_row-1 DO BEGIN
		flipimage[*,i] = image[*,n_row-1-i]
	ENDFOR

	themean = mean(flipimage)
	thresh 	= max(flipimage)/2

	FOR i=0,n_elements(flipimage)-1 DO BEGIN
		x_i = i MOD n_col
		y_i = i/n_col
		IF flipimage[x_i,y_i] GT thresh THEN BEGIN
			xpos+=x_i
			ypos+=y_i
		ENDIF
	ENDFOR

	xpos/=n_elements(flipimage[where(flipimage GT thresh)])
	ypos/=n_elements(flipimage[where(flipimage GT thresh)])

	y_sundiameter = n_elements((flipimage[xpos,*])[where(flipimage[xpos,*] GT thresh)])
	x_sundiameter = n_elements((flipimage[*,ypos])[where(flipimage[*,ypos] GT thresh)])

	sundiameter = (x_sundiameter+y_sundiameter)/2
	sundiameter = x_sundiameter
	; sundiameter = 376 		;xpos is 375, 
	cropped_flipimage = bytarr(2*sundiameter,2*sundiameter)

	;weird, the nested FORloop is faster by .2 seconds. What's the deal.
	FOR j=0,n_row-1 DO BEGIN
		FOR i = 0,n_col-1 DO BEGIN
			IF (i GT xpos - sundiameter) AND $
			(i lt xpos + sundiameter) AND $
			(j GT ypos - sundiameter) AND $
			(j lt ypos + sundiameter) THEN BEGIN
			 	cropped_flipimage[newi,newj] = flipimage[i,j]
				newi+=1 
			ENDIF
			IF newi EQ 2*sundiameter-1 THEN BEGIN
				newj+=1
				newi=0
			ENDIF
		ENDFOR
	ENDFOR
ENDELSE

; FOR i=0,n_elements(flipimage)-1 DO BEGIN
; 	x_i = i MOD n_col
; 	y_i = i/n_col
; 	IF (x_i GT xpos - sundiameter) AND $
; 		(x_i lt xpos + sundiameter) AND $
; 		(y_i GT ypos - sundiameter) AND $
; 		(y_i lt ypos + sundiameter) THEN BEGIN
; 	 	cropped_flipimage[newi,newj] = flipimage[x_i,y_i]
; 		newi+=1 
; 	ENDIF
; 	IF newi EQ 2*sundiameter-1 THEN BEGIN
; 		newj+=1
; 		newi=0
; 	ENDIF
; ENDFOR

;FOR novelty purposes
finish = systime(1,/seconds)
print, 'Elapsed Time: ' + strcompress(finish-start,/remove)+ ' seconds'
; winDOw,0
; cgimage,flipimage,/keep_asp
; winDOw,1
; cgimage,cropped_flipimage,/keep_asp

; save,cropped_flipimage,filename='cropped.sav',description='Cropped file from '+file,/compress
RETURN,cropped_flipimage
END