pro histosmoothed, input

sorted = float(input[bsort(input)])

; u_input only looks good if the positions are so that the aux suns aren't in the same col/row as another sun
; If there's any that do, it's going to be sucks.
; Thus, this is only for looks
u_input = input[uniq(input)]
u_sort = float(u_input[sort(u_input)])

n_col = (size(input,/dim))[0]
n_row = (size(input,/dim))[1]
xarr = fan(findgen(n_col),n_row)
yarr = transpose(fan(findgen(n_row),n_col))

xsort = xarr[bsort(input)]
ysort = yarr[bsort(input)]

; Food for thought, do we ALWAYS WANT TO SKIM?
skimmed = sorted[0:(1-!param.elim_perc/100)*(N_ELEMENTS(sorted)-1)]
arr = histogram(skimmed)
!p.multi=[0,1,2]
plot,arr,yr=[0,300]
vline,25
vline,56
vline,110
plot,DERIV(arr),/ys,yr=[-90,60],title='DERIV(array)'
vline,25
vline,56
vline,110
!p.multi=0
n_smooth = 100.
smoothed = ts_smooth(skimmed,n_smooth,order=3)
reg_smooth = smooth(skimmed,n_smooth,/edge_truncate)
med_smooth = median(skimmed,n_smooth)

; find peak, zero out
; find peak, zero out
arr = scale_vector(deriv( ts_smooth(deriv(smoothed),n_smooth,order=3) ),0,1)
peak_1 = mean(where(arr gt !param.peak1_thresh))
arr[peak_1-100:peak_1+100]=0
peak_2 = mean(where(arr gt !param.peak2_thresh))
arr[peak_2-100:peak_2+100]=0
peak_3 = mean(where(arr gt !param.peak3_thresh))

; Add a little more the position?
thresh100 = skimmed[peak_1];+n_elements(skimmed)*.001]
thresh50 = skimmed[peak_2];+n_elements(skimmed)*.001]
thresh25 = skimmed[peak_3];+n_elements(skimmed)*.001]

print,thresh100
print,thresh50
print,thresh25

; Things I could be doing:
; do the same plots for histogram
; make the saysitall.eps for a bunch of suns in a line
; make program not freak out for 2 suns

; print,thresh100
; print,thresh50
; print,thresh25
stop
end