FUNCTION idsuns, input

bimask = input gt 20
s = Size(bimask, /DIMENSIONS)
fixLabelRegionImage = BytArr(s + 2)
; need this because label_region assumes pixels at edge to be 0
fixLabelRegionImage[1,1] = bimask
labelRegionOutput = Label_Region(fixLabelRegionImage)
labelRegionOutput = labelRegionOutput[1:s[0], 1:s[1]]
h = Histogram(labelRegionOutput, MIN=1, REVERSE_INDICES=ri, BINSIZE=1)
nsuns = n_elements(where(h ne 1))
regmax=fltarr(nsuns)
whichregion = fltarr(nsuns)

for i = 0,nsuns-1 do begin
	ind = where(h ne 1)
	somesun = ri[ri[ind[i]]:ri[ind[i]+1]-1]
	skimmed = (input[somesun])[bsort(input[somesun])]
	regmax[i] = max(skimmed[0:(1-!param.elim_perc/100)*N_ELEMENTS(skimmed)])

	; Shoudld be a way to do this quickly with logical operators but that requires thinking
	; This way is iffy...... iffy...
	if regmax[i] gt .6*255b then whichregion[i]=1
	if regmax[i] gt .4*255b and regmax[i] lt .6*255b then whichregion[i]=2
	if regmax[i] lt .4*255b then whichregion[i]=3
endfor

; ps_start,filename='reg23.eps',/encap
; 	plot,arr,xr=[1.3e5,n_elements(arr)-1]
; ps_end
; scaled = scale_vector(arr*(arr gt 0),0,1)

; ; if we filter everything above .2...
; ps_start,filename='scaledreg23.eps',/encap
; plot,scaled*(scaled gt .2),xr=[1.3e5,n_elements(arr)-1],psym=4
; ps_end

RETURN, whichregion
END