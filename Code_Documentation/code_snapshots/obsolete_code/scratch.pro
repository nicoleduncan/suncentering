FUNCTION scratch, input, thresh

bigxpb = SHIFT_DIFF(EMBOSS(input),dir=3) lt thresh
bigypb = SHIFT_DIFF(EMBOSS(input,az=90),dir=1) lt thresh

for p = 1,6 do begin
    tmpcrop = input[8:40,12-p:42]
    xpb = bigxpb[8:40,12-p:42]
    ypb = bigypb[8:40,12-p:42]
    s = size(tmpcrop,/d)
    bordermask = bytarr(s[0],s[1]) + 1
    ; any specific reason we have the border 2 pixels instead of 1?
    bordermask[1:s[0]-2,1:s[1]-2] = 0

    mcrop = bordermask * tmpcrop

    ncol = s[0]
    nrow = s[1]
    ind_col = WHERE(xpb eq 1) mod ncol
    ind_row = WHERE(ypb eq 1)/nrow


    a = MODE(ind_col)
    b = MODE(ind_col[WHERE(ind_col ne a)])

    c = MODE(ind_row)
    d = MODE(ind_row[WHERE(ind_row ne c)])

    ; Just to make it sorted
    xpos = [a,b]
    ypos = [c,d]
    xpos = xpos[SORT(xpos)]
    ypos = ypos[SORT(ypos)]

    xmcrop = bordermask * xpb
    ymcrop = bordermask * ypb

    ind_col = WHERE(xmcrop eq 1) mod ncol
    ind_row = WHERE(ymcrop eq 1)/nrow

    if WHERE(xmcrop eq 1) eq [-1] then print,'col_slice boo' else begin
        col_slice = FLTARR(N_ELEMENTS(ind_col),nrow)
        for i=0,N_ELEMENTS(ind_col)-1 do begin
            col_slice[i,*] = REFORM(tmpcrop[ind_col[i],*])
            if WHERE(xpb[ind_col[i],*] eq 1) eq [0] then begin
                print,'cropping 0:6'
                if N_ELEMENTS(DERIV(DERIV(FLOAT(col_slice[i,0:6])))) gt 0 lt 6 then okb = 0 else okb = 1
            endif
            if WHERE(xpb[ind_col[i],*] eq 1) eq [-1] then print,'cropping -7:-1'
        endfor
    endelse

    if WHERE(ymcrop eq 1) eq [-1] then print,'row_slice boo' else begin
        row_slice = FLTARR(ncol,N_ELEMENTS(ind_row))
        for i=0,N_ELEMENTS(ind_row)-1 do begin
            row_slice[*,i] = REFORM(tmpcrop[*,ind_row[i]])
            if WHERE(ypb[ind_row[i],*] eq 1) eq [0] then print,'cropping 0:6'
            if WHERE(ypb[ind_row[i],*] eq 1) eq [-1] then print,'cropping -7:-1'
        endfor
    endelse

    !p.multi=[0,1,3]
    oldcharsize = !p.charsize
    !p.charsize=2

    ;Everything to left of vline is fiducial

    window,p
    ; ps_start,filename='anotherslice'+strcompress(p,/rem)+'.eps',/encapsulated,/color
        range = (FLOAT(tmpcrop[12,*]))[0:5]
        plot,range - mode(tmpcrop),psym=-4,title='array - mode(wholeimage) from [0:5] edge of (input[8:40,'+strcompress(12-p,/rem)+':42])[12,*]',xs=3,ys=3
        vline,p-1
        hline,-30    
        plot,DERIV(range),psym=-4,title='1st deriv of slice',xs=3,ys=3
        vline,p-1    
        plot,(DERIV(DERIV(range)))[*],psym=-4,title='2nd deriv of slice',xs=3,ys=3
        vline,p-1
    ; ps_end
    !p.multi=0
    !p.charsize=oldcharsize
endfor

stop
return,okaybit
end