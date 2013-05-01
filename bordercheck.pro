FUNCTION bordercheck, input

s = SIZE(input,/dim)
wn_row = s[0]
wn_col = s[1]
datmask = BYTARR(wn_row,wn_col) + 1
; mask_border_perc = % of edge of image to check
datmask[(!param.mask_border_perc/100)*wn_row:(1-(!param.mask_border_perc/100))*wn_row,$
    (!param.mask_border_perc/100)*wn_col:(1-(!param.mask_border_perc/100))*wn_col] = 0

; min_val should be a really low number, the mode of input is 3
min_val = MODE(input)
; min_val of dimmest sun
min_val = 30
; must do a better check

if TOTAL(datmask*input*(input gt min_val)) gt 0 then okaybit=0 else okaybit=1

return, okaybit
end