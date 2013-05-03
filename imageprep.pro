FUNCTION imageprep, input

; Do something where if it doesn't see anything in the mask, leave alone
s = SIZE(input,/dim)
n_col = s[0]
n_row = s[1]
datmask = BYTARR(n_col,n_row) + 1
; mask_border_perc = % of edge of image to check
datmask[(!param.mask_border_perc/100)*n_col:(1-(!param.mask_border_perc/100))*n_col,$
    (!param.mask_border_perc/100)*n_row:(1-(!param.mask_border_perc/100))*n_row] = 0

; min_val should be a really low number, the mode of input is 3
min_val = MODE(input)
; min_val of dimmest sun
min_val = 30
;	   vvvvvvvvvvvvvvvvvvvvvv
; ---> must do a better check <---
;      ^^^^^^^^^^^^^^^^^^^^^^  


xarr = fan(FINDGEN(n_col),n_row)
yarr = TRANSPOSE(fan(FINDGEN(n_row),n_col))

maskcheck = datmask*input gt min_val

fixLabelRegionImage = BYTARR(s + 2)
fixLabelRegionImage[1,1] = maskcheck
labelRegionOutput = LABEL_REGION(fixLabelRegionImage)
labelRegionOutput = labelRegionOutput[1:s[0], 1:s[1]]

if total(labelregionoutput) eq 0 then return, input else begin
	h = HISTOGRAM(labelRegionOutput, MIN=1, REVERSE_INDICES=ri, BINSIZE=1)
	n_partial = n_elements(where(h ne 1))

	for i = 0,n_partial-1 do begin
		partial = ri[ri[i]:ri[i+1]-1]
		xpos = MEAN(xarr[partial])
		ypos = MEAN(yarr[partial])

		paddedimage = BYTARR(s+100)
		paddedimage[50,50]=input
		;50 wide
		paddedimage[xpos+0:xpos+100,ypos+0:ypos+100]=0
		input = paddedimage[50:s[0]+49,50:s[1]+49]
        ; we're rewriting the padded image
        ; must load again...somehow...
	endfor
	return, input
endelse

; xpos = TOTAL( TOTAL(maskcheck, 2) * INDGEN(n_row) ) / TOTAL(maskcheck)
; ypos = TOTAL( TOTAL(maskcheck, 1) * INDGEN(n_col) ) / TOTAL(maskcheck)
end