FUNCTION setbetterpeak, input, n_suns

; bunch of if statements are ugly
; how to distinguish 1 peak from another? 
; Easier if I just find peaks and worry about identifying them later

peakarr = fltarr(n_suns)
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


; THESE TAKE SO MUCH TIME

; Dig into this

smoothed = TS_SMOOTH(sorted, !param.n_smooth, order = !param.smoothorder)
; smoothed = SMOOTH(sorted, !param.n_smooth)
; smoothed = TS_SMOOTH(sorted, 200, order = 3)
arr = DERIV(TS_SMOOTH(DERIV(smoothed), !param.n_smooth, order = !param.smoothorder))
; stop
; arr = DERIV(SMOOTH(DERIV(smoothed), !param.n_smooth))
; stop
; plot,arr,psym=3,xr=[1.3e5,n_elements(arr)-1]  


; arr is returning the wrong peaks.... how to fix?
; Change smoothing parameter?

for i = 0,n_suns-1 do begin
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

return,{peakarr:peakarr,xsort:xsort,ysort:ysort,sorted:sorted}
end