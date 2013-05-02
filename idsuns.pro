FUNCTION idsuns, input

bimask = input gt 20
s = Size(bimask, /DIMENSIONS)
fixLabelRegionImage = BytArr(s + 2)
; need this because label_region assumes pixels at edge to be 0
fixLabelRegionImage[1,1] = bimask
labelRegionOutput = Label_Region(fixLabelRegionImage)
labelRegionOutput = labelRegionOutput[1:s[0], 1:s[1]]
h = Histogram(labelRegionOutput, MIN=1, REVERSE_INDICES=ri, BINSIZE=1)

regmax=fltarr(n_elements(h ne 1))
whichregion = fltarr(n_elements(h ne 1))
for i = 0,n_elements(h ne 1)-1 do begin
	somesun = ri[ri[i]:ri[i+1]-1]
	regmax[i] = max((input[somesun])[0:(1-!param.elim_perc/1000)*(N_ELEMENTS((input[somesun]))-1)])

	; Shoudld be a way to do this quickly with logical operators but that requires thinking

	if regmax[i] gt .6*255b then whichregion[i]=1
	if regmax[i] gt .4*255b and regmax[i] lt .6*255b then whichregion[i]=2
	if regmax[i] lt .4*255b then whichregion[i]=3
endfor

; input[reg1] is a sun
; input[reg2] is another sun

; how to identify?

; match with sorted array somehow?
; reg_a_max = max((input[*(array[0])])[0:(1-!param.elim_perc/1000)*(N_ELEMENTS((input[*(array[0])]))-1)])
; well, that's the approx max, looks like reg1?
; reg_b_max = max((input[*(array[1])])[0:(1-!param.elim_perc/1000)*(N_ELEMENTS((input[*(array[1])]))-1)])
; well this looks like reg2, not reg3 
; Was actually using old input, didn't change. Fixed.

; If values are large and small, must be 1,3? NEed to use sorting
; possible=[0,255]

; This is the really iffy part
; if reg_a_max gt .6*max(possible) then reg=1
; if reg_a_max gt .4*max(possible) and reg_a_max lt .6*max(possible) then reg=2
; if reg_a_max lt .4*max(possible) then reg=3

; how to isolate peaks from noise?

; ps_start,filename='reg23.eps',/encap
; 	plot,arr,xr=[1.3e5,n_elements(arr)-1]
; ps_end
; scaled = scale_vector(arr*(arr gt 0),0,1)

; ; if we filter everything above .2...
; ps_start,filename='scaledreg23.eps',/encap
; plot,scaled*(scaled gt .2),xr=[1.3e5,n_elements(arr)-1],psym=4
; ps_end
; Well, this looks promising

; so, let's just use a threshold for now

RETURN, whichregion
END