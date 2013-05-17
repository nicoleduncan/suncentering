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

; It just doesn't work. Gives the illusion of working if I run it after ts_smooth 
; for some god-knows-what reason

; arr = deriv(smooth(deriv(smooth(sorted, !param.n_smooth,/edge_truncate)), !param.n_smooth,/edge_truncate))
; arr = arr[0:n_elements(arr)*(1 - !param.elim_perc/100)]
; arr[0:( !param.elim_perc/100)*n_elements(arr)]=0

; changed from
smooth = TS_SMOOTH(sorted, !param.n_smooth, order = !param.smoothorder-1)
arr = DERIV(TS_SMOOTH(DERIV(smooth), !param.n_smooth, order = !param.smoothorder-1))

; ps_start,filename='smoothtest.eps',/color,/encap
; plot,sorted,xr=[1.37e5,1.39e5],yr=[48,100]
; loadct,34
; oplot,smooth(sorted,300),color=15
; oplot,ts_smooth(sorted,300,order=2),color=255
; ps_end


; stop
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