FUNCTION fastcenter, input


for i = 1,10 do begin
    scaled_input = scale_vector(input,0,BYTE(255*i/10.))
    peaks = setpeak(scaled_input)
    peak_1 = peaks.peak_1
    peak_2 = peaks.peak_2
    peak_3 = peaks.peak_3
    xsort = peaks.xsort
    ysort = peaks.ysort

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
        x1 = MEAN(xsort[peak_1:N_ELEMENTS(xsort)-1])
        y1 = MEAN(ysort[peak_1:N_ELEMENTS(ysort)-1])

        xsort[WHERE(xsort gt (x1 - !param.crop_box) and xsort lt (x1 + !param.crop_box))] = 0
        ysort[WHERE(ysort gt (y1 - !param.crop_box) and ysort lt (y1 + !param.crop_box))] = 0

        x2 = MEAN((xsort[peak_2:peak_1])[WHERE(xsort[peak_2:peak_1] ne 0)])
        y2 = MEAN((ysort[peak_2:peak_1])[WHERE(ysort[peak_2:peak_1] ne 0)])

        xsort[WHERE(xsort gt (x2 - !param.crop_box) and xsort lt (x2 + !param.crop_box))] = 0
        ysort[WHERE(ysort gt (y2 - !param.crop_box) and ysort lt (y2 + !param.crop_box))] = 0

        x3 = MEAN((xsort[peak_3:peak_2])[WHERE(xsort[peak_3:peak_2] ne 0)])
        y3 = MEAN((ysort[peak_3:peak_2])[WHERE(ysort[peak_3:peak_2] ne 0)])

        xsort[WHERE(xsort gt (x3 - !param.crop_box) and xsort lt (x3 + !param.crop_box))] = 0
        ysort[WHERE(ysort gt (y3 - !param.crop_box) and ysort lt (y3 + !param.crop_box))] = 0
    toc

    print,i
    print,x1,y1
    print,x2,y2
    print,x3,y3
    print,''
endfor

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
return,{reg1:[x1,y1]],reg2:[x2,y2],reg3:[x3,y3]}
end