FUNCTION fastcenter, input

sorted = (input[bsort(input)])
sorted = sorted[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

s = SIZE(input,/dim)

n_col = s[0]
n_row = s[1]

xarr = FAN(FINDGEN(n_col),n_row)
yarr = TRANSPOSE(FAN(FINDGEN(n_row),n_col))

xsort = xarr[BSORT(input)]
xsort = xsort[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]
ysort = yarr[BSORT(input)]
ysort = ysort[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]

n_smooth = 100.
smoothed = TS_SMOOTH(sorted,n_smooth,order=3)

; find peak, zero out
; find peak, zero out

arr = scale_vector(DERIV(TS_SMOOTH(DERIV(smoothed),n_smooth,order=3) ),0,1)
peak_1 = MEAN(WHERE(arr gt !param.peak1_thresh))
arr[peak_1-100:peak_1+100]=0
peak_2 = MEAN(WHERE(arr gt !param.peak2_thresh))
arr[peak_2-100:peak_2+100]=0
peak_3 = MEAN(WHERE(arr gt !param.peak3_thresh))

; peak_1 = 141243
; peak_2 = 138114
; peak_3 = 134328


; I'm going to spend way too much time finding out why this doesn't work completly right

; tic
;     x1 = mean(xsort[peak_1:n_elements(xsort)-1])
;     y1 = mean(ysort[peak_1:n_elements(ysort)-1])
; ; stop
;     a = where(xsort gt (x1 - !param.crop_box) and xsort lt (x1 + !param.crop_box),complement=reg1)
; stop
;     x2 = mean((xsort[peak_2:peak_1])[reg1])
;     y2 = mean((ysort[peak_2:peak_1])[reg1])

;     c = where(xsort gt (x2 - !param.crop_box) and xsort lt (x2 + !param.crop_box),complement=reg2)
;     ;Match is soooooo slow
;     match,reg1,reg2,suba,subb

;     x3 = mean((xsort[peak_3:peak_2])[reg1[suba]])
;     y3 = mean((ysort[peak_3:peak_2])[reg1[suba]])
; toc
; print,'Without altering original arrays'
; print,x1,y1
; print,x2,y2
; print,x3,y3


tic
    x1 = mean(xsort[peak_1:n_elements(xsort)-1])
    y1 = mean(ysort[peak_1:n_elements(ysort)-1])

    xsort[where(xsort gt (x1 - !param.crop_box) and xsort lt (x1 + !param.crop_box))] = 0
    ysort[where(ysort gt (y1 - !param.crop_box) and ysort lt (y1 + !param.crop_box))] = 0

    x2 = mean((xsort[peak_2:peak_1])[where(xsort[peak_2:peak_1] ne 0)])
    y2 = mean((ysort[peak_2:peak_1])[where(ysort[peak_2:peak_1] ne 0)])

    xsort[where(xsort gt (x2 - !param.crop_box) and xsort lt (x2 + !param.crop_box))] = 0
    ysort[where(ysort gt (y2 - !param.crop_box) and ysort lt (y2 + !param.crop_box))] = 0

    ; Where peak_3:2 ne 0 and peak_2:1 ne 0
    x3 = mean((xsort[peak_3:peak_2])[where(xsort[peak_3:peak_2] ne 0)])
    y3 = mean((ysort[peak_3:peak_2])[where(ysort[peak_3:peak_2] ne 0)])

    xsort[where(xsort gt (x3 - !param.crop_box) and xsort lt (x3 + !param.crop_box))] = 0
    ysort[where(ysort gt (y3 - !param.crop_box) and ysort lt (y3 + !param.crop_box))] = 0
toc

print,'Setting array parts to 0'
print,x1,y1
print,x2,y2
print,x3,y3


    ; If we need xsort and ysort back, we NEED to use the other method
    ; xsort = xarr[BSORT(input)]
    ; xsort = xsort[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]
    ; ysort = yarr[BSORT(input)]
    ; ysort = ysort[0:(1-!param.elim_perc/1000)*(N_ELEMENTS(sorted)-1)]


; window,1
; ps_start,filename='betterenough.eps',/encapsulated,xsize=3.5,ysize=3
;     !p.multi=[0,3,3]
;     cgimage,input[210-60:210+60,153-60:153+60],/k
;     cgimage,input[x1-60:x1+60,y1-60:y1+60],/k
;     cgimage,input[x1z-60:x1z+60,y1z-60:y1z+60],/k
    
;     cgimage,input[337-60:337+60,77-60:77+60],/k
;     cgimage,input[x2-60:x2+60,y2-60:y2+60],/k
;     cgimage,input[x2z-60:x2z+60,y2z-60:y2z+60],/k
    
;     cgimage,input[83-60:83+60,232-60:232+60],/k
;     cgimage,input[x3-60:x3+60,y3-60:y3+60],/k
;     cgimage,input[x3z-60:x3z+60,y3z-60:y3z+60],/k
;     !p.multi=0
; ps_end

stop
return,{thresh100:thresh100,thresh50:thresh50,thresh25:thresh25}
end