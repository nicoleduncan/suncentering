FUNCTION crosstest, inputimage, inputstruct

for kk = 0,N_ELEMENTS(inputstruct)-1 do begin
tic
    acrop = inputimage[inputstruct[kk].limbxpos - !param.soldiskr : inputstruct[kk].limbxpos + !param.soldiskr,$
    inputstruct[kk].limbypos - !param.soldiskr : inputstruct[kk].limbypos + !param.soldiskr]

    ; badcrop = acrop[0:-6,0:-4]
    badcrop=acrop

    ; badcrop to remove high pixels
    badcrop[where(badcrop eq max(badcrop))] = mode(badcrop)

    ; this is the worst-case scenario, so how does it hold up?
    ; so thresh is a different thresh altogether, it's the thresh at which the fiducials dip below

    thresh = .3

    ; for i = 1,9 do begin
    ; thresh = .1*i
    a = SCALE_VECTOR(SHIFT_DIFF(EMBOSS(badcrop,/edge_truncate,az=0),dir=3,/edge_truncate),0,1)
    b = SCALE_VECTOR(SHIFT_DIFF(EMBOSS(badcrop,az=90,/edge_truncate),dir=1,/edge_truncate),0,1)

    xpb = a lt thresh
    ypb = b lt thresh

    s = SIZE(badcrop,/d)

    ncol = s[0]
    nrow = s[1]
    ind_col = WHERE(xpb eq 1) mod ncol
    ind_row = WHERE(ypb eq 1)/ncol

    ; Unique addresses
    uniq_col = ind_col[UNIQ(ind_col,SORT(ind_col))]
    uniq_row = ind_row[UNIQ(ind_row,SORT(ind_row))]
    n_col_indices = N_ELEMENTS(uniq_col)
    n_row_indices = N_ELEMENTS(uniq_row)
    xpos = FLTARR(n_col_indices)
    ypos = FLTARR(n_row_indices)
    uniq_col_stack = FLTARR(n_col_indices)
    uniq_row_stack = FLTARR(n_row_indices)

    ; For each unique col/row index, how many indices are there
    for i = 0,n_col_indices-1 do begin
        uniq_col_stack[i] = N_ELEMENTS(WHERE(ind_col eq uniq_col[i] eq 1))
    endfor

    for i = 0,n_row_indices-1 do begin
        uniq_row_stack[i] = N_ELEMENTS(WHERE(ind_row eq uniq_row[i] eq 1))
    endfor

    ; CGIMAGE,badcrop,/k,/axes
    ; PLOT_EDGES,xpb,x0=.5,y0=.5,dcolor=220,thick=3
    ; PLOT_EDGES,ypb,x0=.5,y0=.5,dcolor=0,thick=3
    ; PLOT_EDGES,xpb*ypb,x0=.5,y0=.5,dcolor=100,thick=3

    ; can't incorporate loops because we call max
    for i = 0,n_col_indices-1 do begin
        xpos[i] = uniq_col[(WHERE(uniq_col_stack eq max(uniq_col_stack)))[0]]
    endfor
    for i = 0,n_row_indices-1 do begin
        ypos[i] = uniq_row[(WHERE(uniq_row_stack eq max(uniq_row_stack)))[0]]
    endfor

    cross_col = WHERE(xpb*ypb eq 1) mod ncol
    cross_row = WHERE(xpb*ypb eq 1)/ncol

    ; buffer for SHIFT
    uniq_col = [0,uniq_col,0]
    uniq_col_stack = [0,uniq_col_stack,0]
    uniq_row = [0,uniq_row,0]
    uniq_row_stack = [0,uniq_row_stack,0]


    ; FOR YPOS ONLY
    adjacent = WHERE(uniq_row-SHIFT(uniq_row,1) eq 1,num_adj)
    qq=0
    rr=0
    
    if num_adj gt 0 then begin
        while qq lt num_adj do begin
            ; must redo?
            ; stop
            c = WHERE( [uniq_row_stack[adjacent[qq]-1],uniq_row_stack[adjacent[qq]]] eq $
                MIN([uniq_row_stack[adjacent[qq]-1],uniq_row_stack[adjacent[qq]]]))
            ; look where it's adjacent
            ; find less pupulous one
            ; set the less populous one to 0
            bin_row = (FLTARR(N_ELEMENTS(uniq_row)) +1)
            bin_row[c+adjacent[qq]-1] = 0
            uniq_row = (uniq_row*bin_row)
            uniq_row_stack = (uniq_row_stack*bin_row)
            qq++
        endwhile
    endif else begin
        adjacent = WHERE(uniq_row-SHIFT(uniq_row,1) eq -1)
        while rr lt num_adj do begin
            c = WHERE( [uniq_row_stack[adjacent[rr]-1],uniq_row_stack[adjacent[rr]]] eq $
                MIN([uniq_row_stack[adjacent[rr]-1],uniq_row_stack[adjacent[rr]]]))
            bin_row = (FLTARR(N_ELEMENTS(uniq_row)) +1)
            bin_row[c+adjacent[rr]-1] = 0
            uniq_row = uniq_row*bin_row
            uniq_row_stack = uniq_row_stack*bin_row
            rr++
        endwhile
    endelse

    ; THIS IS FOR XPOS ONLY
    adjacent = WHERE(uniq_col-SHIFT(uniq_col,1) eq 1,num_adj)
    mm=0
    nn=0
    if num_adj gt 0 then begin
        while mm lt num_adj do begin
            c = WHERE( [uniq_col_stack[adjacent[mm]-1],uniq_col_stack[adjacent[mm]]] eq $
                MIN([uniq_col_stack[adjacent[mm]-1],uniq_col_stack[adjacent[mm]]]))
            bin_col = (FLTARR(N_ELEMENTS(uniq_col)) +1)
            bin_col[c+adjacent[mm]-1] = 0
            uniq_col = (uniq_col*bin_col)
            uniq_col_stack = (uniq_col_stack*bin_col)
            mm++
        endwhile
    endif else begin
        adjacent = WHERE(uniq_col-SHIFT(uniq_col,1) eq -1)
        while nn lt num_adj do begin
            c = WHERE( [uniq_col_stack[adjacent[nn]-1],uniq_col_stack[adjacent[nn]]] eq $
                MIN([uniq_col_stack[adjacent[nn]-1],uniq_col_stack[adjacent[nn]]]))
            bin_col = (FLTARR(N_ELEMENTS(uniq_col)) +1)
            bin_col[c+adjacent[nn]-1] = 0
            uniq_col = uniq_col*bin_col
            uniq_col_stack = uniq_col_stack*bin_col
            nn++
        endwhile
    endelse

    uniq_col = (uniq_col*bin_col)[WHERE(uniq_col*bin_col ne 0)]
    uniq_col_stack = (uniq_col_stack*bin_col)[WHERE(uniq_col_stack*bin_col ne 0)]
    uniq_row = (uniq_row*bin_row)[WHERE(uniq_row*bin_row ne 0)]
    uniq_row_stack = (uniq_row_stack*bin_row)[WHERE(uniq_row_stack*bin_row ne 0)]

    xpos = uniq_col
    ypos = uniq_row
    ; stop

    ; shit.

    ; how to deal with pixels part of the overlap mask but not in the fiducial???

    xsuperset = [cross_col,xpos]
    if N_ELEMENTS(xsuperset[UNIQ(xsuperset,SORT(xsuperset))]) gt 0 then xpos = xpos
    ysuperset = [cross_row,ypos]
    if N_ELEMENTS(ysuperset[UNIQ(ysuperset,SORT(ysuperset))]) gt 0 then ypos = ypos

toc
stop
    ; 3 no longer works for startimage=wholeimage
    coords = FLTARR(3,3)

    coords[0,*] = cross_col
    coords[1,*] = cross_row

    places = 0
    for jj=0,2 do begin
        if TOTAL(coords[0,jj] eq xpos) ne 0 and TOTAL(coords[1,jj] eq ypos) ne 0 then begin
            places = [places,coords[0:1,jj]]
        endif
    endfor
    
    places=REFORM(places[WHERE(places ne 0)],2,N_ELEMENTS(places)/2)
endfor



stop
return,places
end
































