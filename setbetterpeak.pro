FUNCTION setbetterpeak, input, regarr


; bunch of if statements are ugly
; how to distinguish 1 peak from another? 
; Easier if I just find peaks and worry about identifying them later

n_peaks = n_elements(regarr)
peakarr = fltarr(n_peaks)
sorted = input[bsort(input)]
sorted = sorted[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

s = SIZE(input,/dim)
n_col = s[0]
n_row = s[1]

xarr = fan(FINDGEN(n_col),n_row)
yarr = TRANSPOSE(fan(FINDGEN(n_row),n_col))

xsort = xarr[BSORT(input)]
xsort = xsort[0:(1- !param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]
ysort = yarr[BSORT(input)]
ysort = ysort[0:(1- !param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

smoothed = TS_SMOOTH(sorted, !param.n_smooth, order = !param.smoothorder)
arr = DERIV(TS_SMOOTH(DERIV(smoothed), !param.n_smooth, order = !param.smoothorder))


for i = 0,n_peaks-1 do begin
    if N_ELEMENTS(MAX(arr)) ne 1 then begin 
    maxi = WHERE(arr eq MAX(arr),n_maxi)
    maxi_check=fltarr(maxi)
    tempthresh = .9*MAX(arr)
    for i = 0,n_elements(maxi) do begin
        chunk = arr[maxi[i]-50:maxi[i]+50]
        maxi_check[i] = n_elements(chunk gt tempthresh)
    endfor
    peakarr[i] = maxi[max(maxi_check)]
    endif else peakarr[i] = MEAN(WHERE(arr eq MAX(arr)))

    arr[peakarr[i]-200:peakarr[i]+200]=0
endfor

return,{peakarr:peakarr,xsort:xsort,ysort:ysort}
end