FUNCTION edgefidcheck, input, thresh

for p = 0,5 do begin
    tmpcrop = input[10:38+p,8:42]
    xpb = SHIFT_DIFF(EMBOSS(tmpcrop,az=180,/edge_truncate),dir=4,/edge_truncate) lt thresh
    ypb = SHIFT_DIFF(EMBOSS(tmpcrop,az=90,/edge_truncate),dir=1,/edge_truncate) lt thresh

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

    ; if N_ELEMENTS(row_border) eq 1 then row_border = MODE(ind_row)
    ; if N_ELEMENTS(col_border) eq 1 then col_border = MODE(ind_col)

    ; Look at each index, 6 pixels in

    ; If we see a fiducial cut off, either

    ; ignore fiducial
    ; or
    ; crop it out
    ; need to identify whether to use 0:6 or -7:-1
    if WHERE(xmcrop eq 1) eq [-1] then print,'col_slice boo' else begin
        col_slice = FLTARR(N_ELEMENTS(ind_col),nrow)
        for i=0,N_ELEMENTS(ind_col)-1 do begin
            col_slice[i,*] = REFORM(tmpcrop[ind_col[i],*])
            if WHERE(xpb[ind_col[i],*] eq 1) eq [0] then begin
                print,'cropping 0:6'

                ; So this is the part of the program where we need to 
                ; do the smart thing of checking to make sure that the fiducials are being
                ; thresholded correctly, but how do we do that? 

                ; What is somevalue? How do we quantify it?

        ; if N_ELEMENTS(FLOAT(col_slice[i,0:6]) - MODE(tmpcrop) lt somevalue) lt 6 then okaybit=0 else okaybit=1
        ; I think we should use something with derivatives because we know approximately 
        ; how dim the fiducials will get. Instead of replying on pixel values, we rely
        ; on the relative pixel changes which may/may not be more robust

        if N_ELEMENTS(DERIV(DERIV(FLOAT(col_slice[i,0:6])))) gt 0 lt 6 then okb = 0 else okb = 1

        ; This isn't going to work because the threshold of 0 is too high. According to this
        ; current setup, if a fiducial is right on the edge, it'll ALWAYS be bad.

        
        ; I can actually not use parentheses here, is it ok?
        ;Honestly, what's the purpose of doing this "X-Y lt thresh" instead of "X lt thresh"?


        ; The problem is that I'm unable to quantify the fiducials in the way I want
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

    ; !p.multi=[0,1,3]
    ; oldcharsize = !p.charsize
    ; !p.charsize=2
    ; ; The right of the vline is where the fiducial is
    ; window,p
    ; ; ps_start,filename='betterslice'+strcompress(p,/rem)+'.eps',/encapsulated,/color
    ;     range = (FLOAT(tmpcrop[*,20]))[-6:-1]
    ;     plot,range - mode(tmpcrop),psym=-4,title='array - mode(wholeimage) from [-6:-1] edge of (input[10:'+strcompress(38+p)+',8:42])[*,20]',xs=3,ys=3
    ;     vline,5-p
    ;     hline,-20
    ;     plot,DERIV(range),psym=-4,title='1st deriv of slice',xs=3,ys=3
    ;     vline,5-p
    ;     plot,(DERIV(DERIV(range)))[*],psym=-4,title='2nd deriv of slice',xs=3,ys=3
    ;     vline,5-p
    ; ; ps_end
    ; !p.multi=0
    ; !p.charsize=oldcharsize
    
    ; window,p
    ps_start,filename='fidcheck_newdegree'+strcompress(p,/rem)+'.eps',/encapsulated,/color
        cgimage,tmpcrop,/k,/axes,title='(input[10:'+strcompress(38+p,/rem)+',8:42])[*,20]'
        plot_edges,xpb,thick=3,x0=.5,y0=.5
        plot_edges,ypb,dcolor=220,thick=3,x0=.5,y0=.5
    ps_end,/png,resize=100,/delete

endfor


stop


return,okaybit
end