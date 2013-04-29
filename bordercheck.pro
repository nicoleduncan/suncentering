FUNCTION bordercheck, input

s = SIZE(input,/dim)
wn_row = s[0]
wn_col = s[1]
datmask = BYTARR(wn_row,wn_col) + 1
datmask[(1/!param.mask_border_perc)*wn_row:(1-(1/!param.mask_border_perc))*wn_row,$
    (1/!param.mask_border_perc)*wn_col:(1-(1/!param.mask_border_perc))*wn_col] = 0

; min_val should be a really low number, the mode of input is 3
min_val = MODE(input)

if TOTAL(datmask*input) gt N_ELEMENTS(datmask[WHERE(datmask eq 1)])*min_val then okaybit=0 else okaybit=1

return, okaybit
end