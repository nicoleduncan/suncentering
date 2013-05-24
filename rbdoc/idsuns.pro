FUNCTION idsuns, input
;+
;   :Description:
;       Defines solar regions
;
;   :Params:
;       input: in, required
;           Starting input image
;
;-

; Well, we don't know anything about thresholds but we know they can't be any lower than 20, right? *awkward laugh*
bimask = input gt 20

s = SIZE(bimask, /DIMENSIONS)
labelme = BYTARR(s + 2)
; Need this because label_region assumes pixels at edge to be 0
labelme[1,1] = bimask
fixedoutput = LABEL_REGION(labelme)
fixedoutput = fixedoutput[1:s[0], 1:s[1]]
h = HISTOGRAM(fixedoutput, MIN=1, REVERSE_INDICES=ri, BINSIZE=1)
nsuns = N_ELEMENTS(WHERE(h ne 1))
regmax=FLTARR(nsuns)
whichregion = FLTARR(nsuns)

for i = 0,nsuns-1 do begin
	ind = WHERE(h ne 1)
	somesun = ri[ri[ind[i]]:ri[ind[i]+1]-1]
	skimmed = (input[somesun])[bsort(input[somesun])]
	regmax[i] = MAX(skimmed[0:(1-!param.elim_perc/100)*N_ELEMENTS(skimmed)])
	; Shoudld be a way to do this quickly with logical operators but that requires thinking
	; This way is iffy...... iffy...
	if regmax[i] gt .6*255b then whichregion[i]=1
	if regmax[i] gt .4*255b and regmax[i] lt .6*255b then whichregion[i]=2
	if regmax[i] lt .4*255b then whichregion[i]=3
endfor

RETURN, whichregion
END