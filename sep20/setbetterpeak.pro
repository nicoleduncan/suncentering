FUNCTION setbetterpeak, input, nsuns
;+
;   :Description:
;       Returns peaks of 2nd deriv of sorted array to set thresholds for image
;
;   :Params:
;       input: in, required, type=byte(ndims,2)
;           Starting input image
;
;       nsuns: in, required, type=float
;           Number of suns
;
;-

; instead of sorting the entire image, let's try getting away with sorting a whole lot less

; Either threshold by 1 or use last 25% of array or something
trimmed = input[where(input gt 1)]
peakarr = FLTARR(nsuns)
beensorted = SORT(trimmed)
sorted = trimmed[beensorted]
sorted = sorted[0:(1-!param.elim_perc/100)*(N_ELEMENTS(sorted)-1)]
; .03 to run to here
 ; Now takes .0075


a = DERIV(SMOOTH(FLOAT(sorted), !param.n_smooth, /edge_truncate))
arr = DERIV(SMOOTH(a, !param.n_smooth, /edge_truncate))
; .05 to run to here
; Now takes .0075

; Eliminate the peak on the right end of the array, only needed for Albert's images

if !param.peakfind_buff eq 13000 then arr = arr[0:-10000]

if nsuns gt 1 then begin
    for i = 0,nsuns-1 do begin
        peakarr[i] = MEAN(WHERE(arr eq MAX(arr)))
        print,WHERE(arr eq MAX(arr))
        ; so that the next peak is the real peak
        arr=[arr,fltarr( !param.peakfind_buff)]
        arr[peakarr[i]- !param.peakfind_buff:peakarr[i]+ !param.peakfind_buff]=0
        arr=arr[0:- !param.peakfind_buff]
    endfor
endif else begin
    peakarr = MEAN(WHERE(a[0:- !param.peakfind_buff] eq MAX(a[0:- !param.peakfind_buff])))
endelse


; a = DERIV(SMOOTH(FLOAT(sorted), !param.n_smooth, /edge_truncate))
; arr = DERIV(SMOOTH(a, !param.n_smooth, /edge_truncate))
; ps_start,filename='smoothed_zoom.eps',/encap
; plot,arr,yr=[-4e-6,4e-6],title='2nd derivative of sorted array'
; vline,peakarr,linestyle=1
; ps_end
; ps_start,filename='deriv_1_unsmoothed.eps',/encap
; plot,deriv(float(sorted)),ys=3,title='1st derivative of sorted array'
; ps_end
; ps_start,filename='deriv_2_unsmoothed.eps',/encap
; plot,deriv(deriv(float(sorted))),ys=3,title='2nd derivative of sorted array'
; ps_end
; stop

; .06 to run to here
; Now .008

RETURN,{peakarr:peakarr,sorted:sorted}
end