FUNCTION setbetterpeak, input, nsuns
;+
;   :Description:
;       Returns peaks of 2nd deriv of sorted array to set thresholds for image
;
;   :Params:
;       input: in, required
;           Starting input image
;
;       nsuns: in, required
;           Number of suns
;
;-

; instead of sorting the entire image, let's try getting away with sorting a whole lot less

trimmed = input[where(input gt 1)]
peakarr = FLTARR(nsuns)
beensorted = SORT(trimmed)
sorted = trimmed[beensorted]
sorted = sorted[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]
; .03 to run to here
 ; Now takes .0075

a = DERIV(SMOOTH(FLOAT(sorted), !param.n_smooth, /edge_truncate))
arr = DERIV(SMOOTH(a, !param.n_smooth, /edge_truncate))
; .05 to run to here
; Now takes .0075


; This isn't much faster
; tic
; a = SMOOTH(FLOAT(sorted), !param.n_smooth, /edge_truncate)
; a=[0,a,0]
; da = a-shift(a,1)
; da = da[1:n_elements(da)-2]
; b = SMOOTH(da, !param.n_smooth, /edge_truncate)
; b=[0,b,0]
; db = b-shift(b,1)
; db = db[1:n_elements(db)-2]
; toc

arr = arr[0:-10000]

if nsuns gt 1 then begin
    for i = 0,nsuns-1 do begin
        peakarr[i] = MEAN(WHERE(arr eq MAX(arr)))
        ; so that the next peak is the real peak
        arr=[arr,fltarr(13000)]
        arr[peakarr[i]-13000:peakarr[i]+13000]=0
        arr=arr[0:-13000]
    endfor
endif else begin
    peakarr = MEAN(WHERE(a[0:-10000] eq MAX(a[0:-10000])))
endelse
; .06 to run to here

; plot,sorted
; vline,peakarr
; stop


; ; We actually don't care about this xpos ypos thing. Let's comment it out for later.
; peakarr = FLTARR(nsuns)
; beensorted = SORT(input)
; sorted = input[beensorted]
; sorted = sorted[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

; s = SIZE(input,/dim)
; n_col = s[0]
; n_row = s[1]

; xarr = fan(FINDGEN(n_col),n_row)
; ; yarr = TRANSPOSE(fan(FINDGEN(n_row),n_col))
; ; slightly faster to rotate than transpose
; yarr = ROTATE(fan(FINDGEN(n_row),n_col),1)

; xsort = xarr[beensorted]
; xsort = xsort[0:(1- !param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]
; ysort = yarr[beensorted]
; ysort = ysort[0:(1- !param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

; ; Originally, used ts_smooth but a simple float() fixed smoothing. Jesus Christ.
; ; smooth = TS_SMOOTH(sorted, !param.n_smooth, order = !param.smoothorder-1)
; ; arr = DERIV(TS_SMOOTH(DERIV(smooth), !param.n_smooth, order = !param.smoothorder-1))
; stop
; a = DERIV(SMOOTH(FLOAT(sorted), !param.n_smooth, /edge_truncate))
; arr = DERIV(SMOOTH(a, !param.n_smooth, /edge_truncate))

; ; have to drop the last 10,000 indices since I'm smoothing by 1500
; arr=arr[0:1240000]

; ; The thresholds are letting too many of the region-1 pixels in
; ; stop
; ; plot,arr
; ; vline, 1222214
; ; vline, 1193643
; ; vline,1158722
; ; stop
; if nsuns gt 1 then begin
;     for i = 0,nsuns-1 do begin
;         peakarr[i] = MEAN(WHERE(arr eq MAX(arr)))
;         ; so that the next peak is the real peak
;         arr=[arr,fltarr(10000)]
;         arr[peakarr[i]-10000:peakarr[i]+10000]=0
;         arr=arr[0:-10000]
        
;     endfor
; endif else begin
;     peakarr = MEAN(WHERE(a eq MAX(a)))
; endelse

RETURN,{peakarr:peakarr,sorted:sorted};xsort:xsort,ysort:ysort}
end