FUNCTION setbetterpeak, input, n_suns
;+
;   :Description:
;       Makes strips using approx centroiding method to make cropped areas
;
;   :Params:
;       input: in, required
;           Starting input image
;
;       n_suns: in, required
;           Number of suns
;
;-

peakarr = FLTARR(n_suns)
beensorted = SORT(input)
sorted = input[beensorted]
sorted = sorted[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

s = SIZE(input,/dim)
n_col = s[0]
n_row = s[1]

xarr = fan(FINDGEN(n_col),n_row)
yarr = TRANSPOSE(fan(FINDGEN(n_row),n_col))

xsort = xarr[beensorted]
xsort = xsort[0:(1- !param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]
ysort = yarr[beensorted]
ysort = ysort[0:(1- !param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

; Originally, used ts_smooth but a simple float() fixed smoothing. Jesus Christ.
; smooth = TS_SMOOTH(sorted, !param.n_smooth, order = !param.smoothorder-1)
; arr = DERIV(TS_SMOOTH(DERIV(smooth), !param.n_smooth, order = !param.smoothorder-1))

a = DERIV(SMOOTH(FLOAT(sorted), !param.n_smooth, /edge_truncate))
arr = DERIV(SMOOTH(a, !param.n_smooth, /edge_truncate))

for i = 0,n_suns-1 do begin
    peakarr[i] = MEAN(WHERE(arr eq MAX(arr)))
    ; so that the next peak is the real peak
    arr[peakarr[i]-200:peakarr[i]+200]=0
endfor

RETURN,{peakarr:peakarr,xsort:xsort,ysort:ysort,sorted:sorted}
end