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

; changed from
; smooth = TS_SMOOTH(sorted, !param.n_smooth, order = !param.smoothorder-1)
; arr = DERIV(TS_SMOOTH(DERIV(smooth), !param.n_smooth, order = !param.smoothorder-1))

; c = float(sorted-shift(sorted,1))
; expanded = findgen(n_elements(sorted))
; ; toc
; ; c[where(c eq 0)]= !values.f_nan
; ; plot,shift(sorted,1)+shift(sorted,2)+shift(sorted,3)+shift(sorted,4)+shift(sorted,5),psym=3
; plot,c,yr=[0,1.2],psym=3,xr=[1.30e5,1.42e5]
; ; need to pad it so that the mod is okay
; binsize = 200
; padnum = binsize - (n_elements(c) mod binsize)
; ; stop
; c = [fltarr(padnum),c]
; ; stop
; d = reform(c,binsize,n_elements(c)/binsize)
; e = total(d,1)
; for i = 0,n_elements(e)-1 do begin
;     if i eq 0 then vv = fltarr(binsize)+e[i] else vv = [vv,fltarr(binsize)+e[i]]
; endfor
; plot,vv,xr=[1.30e5,1.42e5],psym=3
; vline,141225
; vline,138077
; vline,134314
; Well this doesn't work.
; omfg
; omfg
; o
; m
; f
; g
; good fucking god. 

a = deriv(smooth(float(sorted),200,/edge_truncate))
arr = deriv(smooth(a,200,/edge_truncate))
; toc
; plot,b
; vline,141225
; vline,138077
; vline,134314
; stop
; d= shift(c,1)-c
; plot,d,xr=[1.30e5,1.39e5],psym=-4
; !p.multi=0
; deriv = dy/dx, but dx=same for all cases


; let's filter stuff out in the freq domain?

; fft takes up too much resources
; tic ; .52 s
; a=shift(fft(deriv(sorted)),n_elements(sorted)/2)
; a[where(abs(a) gt .0005)]=0
; b=fft(a,/inverse)
; toc

; Need a faster way to smooth

; ps_start,filename='smoothtest.eps',/color,/encap
; plot,sorted,xr=[1.37e5,1.39e5],yr=[48,100]
; loadct,34
; oplot,smooth(sorted,300),color=15
; oplot,ts_smooth(sorted,300,order=2),color=255
; ps_end

; Here we are again, old friend.

; stop
for i = 0,n_suns-1 do begin
    if N_ELEMENTS(MAX(arr)) ne 1 then begin 
        maxi = WHERE(arr eq MAX(arr),n_maxi)
        maxi_check=FLTARR(maxi)
        tempthresh = .9*MAX(arr)
        for i = 0,N_ELEMENTS(maxi) do begin
            chunk = arr[maxi[i]-50:maxi[i]+50]
            maxi_check[i] = N_ELEMENTS(chunk gt tempthresh)
        endfor
        peakarr[i] = maxi[(maxi_check)]
    endif else peakarr[i] = MEAN(WHERE(arr eq MAX(arr)))
        arr[peakarr[i]-200:peakarr[i]+200]=0
endfor
; stop
return,{peakarr:peakarr,xsort:xsort,ysort:ysort,sorted:sorted}
end