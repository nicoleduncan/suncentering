; docformat = 'rst'

PRO boxit, file=file
;+
; :OBSOLETE:
; :DESCRIPTION:
;		Makes a box around the sun (or round piece of paper) with equal sized padding for every scenario.
;
;-

IF ~keyword_set(file) THEN file = '104533_20120911_153147_254618_0.bin'
n_row = 960.
n_col = 1280.
n_pix = n_row*n_col
flipimage 	= bytarr(n_col,n_row)
scan_width = 10.
chord_lenGTh_array = fltarr(n_row)
inter_flipimage = bytarr(n_col,n_row)
;Load image file
openr,lun,file,/get_lun
image=bytarr(n_col,n_row)
readu,lun,image
free_lun,lun

;Find out some useful inFORmation
image_average = total(image)/n_elements(image)
; halfmax = max(image)/2.
halfmax=200 ; this has to be large enough to exclude black ink (which has a value of ~120)

;Flip image around since IDL is backwards
FOR i=0,n_row-1 DO BEGIN
	flipimage[*,i] = image[*,n_row-1-i]
ENDFOR

;Let's scan rows to see when we hit the bottom of the sun (or paper circle)
row_index=0
while n_elements(where(flipimage[*,row_index*scan_width] GT halfmax)) EQ 1 DO BEGIN
	row_index+=1
ENDwhile
print,'Row index ',row_index*scan_width,' picked up either the sun or piece of paper'

;See how long the width of the sun is
FOR i = 0,n_row-1 DO BEGIN
	chord_lenGTh_array[i] = n_elements(where(flipimage[*,i] GT halfmax))
ENDFOR

print,'Sun or piece of paper is ',max(chord_lenGTh_array),' elements long at its widest' 

;How long is this particular chord of the sun/ sun on a piece of paper
chord_width = n_elements(where(flipimage[*,row_index*scan_width] GT image_average))

;How far out is the center of the chord from the center of sun/ sun on a piece of paper
how_far_out_from_center = sqrt((max(chord_lenGTh_array)/2)^2 + (chord_width/2.)^2)

print,'The chord is ',how_far_out_from_center,' from the center'

first_index = (where(flipimage[*,row_index*scan_width]-image_average GT 0))[0]
x_center = first_index + chord_width/2. 
y_center = row_index*scan_width + how_far_out_from_center

print,'apPROx center of sun or piece of paper is ',x_center,',',y_center
;Now we know apPROx center, let's crop around it.
;Using center, crop a box with lenGTh 1.5*max(chord_lenGTh_array) around center

c_fi = bytarr(2*max(chord_lenGTh_array),2*max(chord_lenGTh_array))


newi=0
newj=0
FOR j=0,n_row-1 DO BEGIN
	FOR i = 0,n_col-1 DO BEGIN
		IF (i GT x_center - max(chord_lenGTh_array)) AND $
		(i lt x_center + max(chord_lenGTh_array)) AND $
		(j GT y_center - max(chord_lenGTh_array)) AND $
		(j lt y_center + max(chord_lenGTh_array)) THEN BEGIN
	 	c_fi[newi,newj] = flipimage[i,j]
		newi+=1 
		ENDIF
		IF newi EQ 2*max(chord_lenGTh_array)-1 THEN BEGIN
			newj+=1
			newi=0
		ENDIF
	ENDFOR
ENDFOR


flipimage[fix(x_center),*]=0
flipimage[*,round(y_center)] = 0
flipimage[first_index,*]=150
flipimage[*,row_index*scan_width]=5

; loadct,0 ;want black AND white
winDOw,0

winDOw,1
cgimage,c_fi,/keep_asp
save,c_fi,filename='cropped.sav',description='Cropped file from '+file,/compress
stop
END