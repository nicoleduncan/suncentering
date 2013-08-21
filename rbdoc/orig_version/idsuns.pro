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

; Well, we don't know anything about thresholds but we know they can't be any lower than 15, right? *awkward laugh*

; 15 for albert's images, 30 for old ones
bimask = input gt 30
; bimask = morph_open(bimask,replicate(1,3,3))
; dilate is 3x faster, needed for albert's sun
; bimask = dilate(bimask,replicate(1,3,3))

s = SIZE(bimask, /DIMENSIONS)
labelme = BYTARR(s + 2)
; Need this because label_region assumes pixels at edge to be 0
labelme[1,1] = bimask
fixedoutput = LABEL_REGION(labelme)
fixedoutput = fixedoutput[1:s[0], 1:s[1]]
h = HISTOGRAM(fixedoutput, MIN=1, REVERSE_INDICES=ri, BINSIZE=1)
noth = WHERE(h ne 1)
nsuns = N_ELEMENTS(noth)
whichregion = FLTARR(nsuns)

for i = 0, nsuns-1 do begin
	somesun = ri[ri[noth[i]]:ri[noth[i]+1]-1]
	skimmed = (input[somesun])[SORT(input[somesun])]
	regmax = MAX(skimmed[0:(1-!param.elim_perc/1000)*N_ELEMENTS(skimmed)])
	; Shoudld be a way to do this quickly with logical operators but that requires thinking
	; This way is iffy...... iffy...
	if regmax gt .6*255b then whichregion[i]=1
	if regmax gt .3*255b and regmax lt .6*255b then whichregion[i]=2
	if regmax lt .3*255b then whichregion[i]=3
endfor

RETURN, whichregion
END