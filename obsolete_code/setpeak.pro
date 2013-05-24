FUNCTION setpeak, input

; for i = 1,10 do begin
;     scaled_input = scale_vector(input,0,BYTE(255*i/10.))
    scaled_input = input
    sorted = scaled_input[bsort(scaled_input)]
    sorted = sorted[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

    s = SIZE(scaled_input,/dim)
    n_col = s[0]
    n_row = s[1]

    xarr = fan(FINDGEN(n_col),n_row)
    yarr = TRANSPOSE(fan(FINDGEN(n_row),n_col))

    xsort = xarr[BSORT(scaled_input)]
    xsort = xsort[0:(1- !param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]
    ysort = yarr[BSORT(scaled_input)]
    ysort = ysort[0:(1- !param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

    smoothed = TS_SMOOTH(sorted, !param.n_smooth, order = !param.smoothorder)
    arr = DERIV(TS_SMOOTH(DERIV(smoothed), !param.n_smooth, order = !param.smoothorder))

    if N_ELEMENTS(MAX(arr)) ne 1 then begin 
        maxi = WHERE(arr eq MAX(arr),n_maxi)
        maxi_check=fltarr(maxi)
        tempthresh = .9*MAX(arr)
        for i = 0,n_elements(maxi) do begin
            chunk = arr[maxi[i]-50:maxi[i]+50]
            maxi_check[i] = n_elements(chunk gt tempthresh)
        endfor
        peak = maxi[max(maxi_check)]
    endif else peak_1 = MEAN(WHERE(arr eq MAX(arr)))
    
    arr[peak_1-200:peak_1+200]=0

    if N_ELEMENTS(MAX(arr)) ne 1 then begin 
        maxi = WHERE(arr eq MAX(arr),n_maxi)
        maxi_check=fltarr(maxi)
        tempthresh = .9*MAX(arr)
        for i = 0,n_elements(maxi) do begin
            chunk = arr[maxi[i]-50:maxi[i]+50]
            maxi_check[i] = n_elements(chunk gt tempthresh)
        endfor
        peak = maxi[max(maxi_check)]
    endif else peak_2 = MEAN(WHERE(arr eq MAX(arr)))

    arr[peak_2-200:peak_2+200]=0

    if N_ELEMENTS(MAX(arr)) ne 1 then begin 
        maxi = WHERE(arr eq MAX(arr),n_maxi)
        maxi_check=fltarr(maxi)
        tempthresh = .9*MAX(arr)
        for i = 0,n_elements(maxi) do begin
            chunk = arr[maxi[i]-50:maxi[i]+50]
            maxi_check[i] = n_elements(chunk gt tempthresh)
        endfor
        peak = maxi[max(maxi_check)]
    endif else peak_3  = MEAN(WHERE(arr eq MAX(arr)))

    ; Old way, don't need to do this way anymore
    ; peak_1 = MEAN(WHERE(arr eq MAX(arr)))
    ; arr[peak_1-200:peak_1+200]=0
    ; peak_2 = MEAN(WHERE(arr eq MAX(arr)))
    ; arr[peak_2-200:peak_2+200]=0
    ; peak_3 = MEAN(WHERE(arr eq MAX(arr)))

;     print,i
;     print,'peak 1:',peak_1
;     print,'peak 2:',peak_2
;     print,'peak 3:',peak_3
;     print,''
; endfor

return,{peak_1:peak_1,peak_2:peak_2,peak_3:peak_3,xsort:xsort,ysort:ysort}
end