FUNCTION setthresh, input


; for i = 1,10 do begin

    ; scaled_input = scale_vector(input,0,BYTE(255*i/10.))

    peaks = setpeak(input)

    sorted = scaled_input[bsort(input)]
    sorted = sorted[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

    ; s = SIZE(scaled_input,/dim)
    ; n_col = s[0]
    ; n_row = s[1]

    ; xarr = fan(FINDGEN(n_col),n_row)
    ; yarr = TRANSPOSE(fan(FINDGEN(n_row),n_col))

    ; xsort = xarr[BSORT(scaled_input)]
    ; xsort = xsort[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]
    ; ysort = yarr[BSORT(scaled_input)]
    ; ysort = ysort[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

    ; n_smooth = 100.
    ; smoothed = TS_SMOOTH(sorted,n_smooth,order=3)

    ; ; arr = scale_vector(deriv(ts_smooth(deriv(smoothed),n_smooth,order=3) ),0,1)
    ; arr = DERIV(TS_SMOOTH(DERIV(smoothed),n_smooth,order=3) )
    ; ; plot,arr,xr=[1.3e5,1.44e5]
    ; ; If I do max instead of thresholds, it never fails because, duh there's always a max
    ; ; Looking 100 pixels wide is pretty arbitrary
    ; ; peak_1 = mean(where(arr gt !param.peak1_thresh))


    ; peak_1 = MEAN(WHERE(arr eq MAX(arr)))
    ; ; if i eq 5 then stop
    ; arr[peak_1-200:peak_1+200]=0
    ; ; peak_2 = mean(where(arr gt !param.peak2_thresh))
    
    ; peak_2 = MEAN(WHERE(arr eq MAX(arr)))
    ; arr[peak_2-200:peak_2+200]=0
    ; ; peak_3 = mean(where(arr gt !param.peak3_thresh))
    ; peak_3 = MEAN(WHERE(arr eq MAX(arr)))

    ; ; Add a little more the position? Why?
    ; ; Unless I add a little more, the thresh includes the highest values from the second brightest 
    ; ; Don't like arbitrarily adding stuff though

    thresh100 = sorted[peaks.peak_1]
    thresh50 = sorted[peaks.peak_2]
    thresh25 = sorted[peaks.peak_3]

    ; vline,peak_1
    ; vline,peak_2
    ; vline,peak_3
    ; stop

    ; window,i
    ; cgimage,scaled_input*(scaled_input gt thresh100),/k

    print,'Scale factor ',i
    print,'Reg 1 thresh: ',thresh100
    print,'Reg 2 thresh: ',thresh50
    print,'Reg 3 thresh: ',thresh25
    print,''
    ; stop
; endfor
; stop

; !p.multi=[0,1,3]
; plot,xsort,psym=3,title='X Positions'
; vline,peak_1
; vline,peak_2
; vline,peak_3
; plot,ysort,psym=3,title='Y Positions'
; vline,peak_1
; vline,peak_2
; vline,peak_3
; plot,sorted
; vline,peak_1
; vline,peak_2
; vline,peak_3
; !p.multi=0


return,{thresh100:thresh100,thresh50:thresh50,thresh25:thresh25}
end