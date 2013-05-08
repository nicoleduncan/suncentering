FUNCTION checkimage, input

;Find centers of 3 suns, if any re within 1.1R_sun of edge, eliminate.

s = SIZE(input,/dim)
n_col = s[0]
n_row = s[1]
a=setbetterthresh(input)
min_val = min(a.thresh)*2
; if I set thresh to min, n_suns goes to 5 
maskcheck = input gt min_val

xarr = fan(FINDGEN(n_col),n_row)
yarr = TRANSPOSE(fan(FINDGEN(n_row),n_col))


fixLabelRegionImage = BYTARR(s + 2)
fixLabelRegionImage[1,1] = maskcheck
labelRegionOutput = LABEL_REGION(fixLabelRegionImage)
labelRegionOutput = labelRegionOutput[1:s[0], 1:s[1]]

h = HISTOGRAM(labelRegionOutput, MIN=1, REVERSE_INDICES=ri, BINSIZE=1)
n_suns = n_elements(where(h ne 1))

raisboss = REPLICATE({reg:0.,xpos:0.,ypos:0.,passbit:0},n_suns)

for i = 0,n_suns-1 do begin
    ind = where(h ne 1)
    somesun = ri[ri[ind[i]]:ri[ind[i]+1]-1]
    raisboss[i].xpos = MEAN(xarr[somesun])
    raisboss[i].ypos = MEAN(yarr[somesun])
    skimmed = (input[somesun])[bsort(input[somesun])]
    somesunsmax = max(skimmed[0:(1-!param.elim_perc/100)*N_ELEMENTS(skimmed)])
    ; Shoudld be a way to do this quickly with logical operators but that requires thinking
    ; This way is iffy...... iffy...
    if somesunsmax gt .6*255b then raisboss[i].reg=1
    if somesunsmax gt .4*255b and somesunsmax lt .6*255b then raisboss[i].reg=2
    if somesunsmax lt .4*255b then raisboss[i].reg=3

    if raisboss[i].xpos lt 1.1* !param.sundiam/2. or raisboss[i].xpos gt n_col-1.1* !param.sundiam/2. or $
    raisboss[i].ypos lt 1.1* !param.sundiam/2. or raisboss[i].ypos gt n_row-1.1* !param.sundiam/2. then raisboss[i].passbit = 0 $
    else raisboss[i].passbit=1
endfor

return, raisboss
end