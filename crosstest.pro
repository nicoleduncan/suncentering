FUNCTION crosstest, inputimage, inputstruct

for i = 0,N_ELEMENTS(inputstruct)-1 do begin
    acrop = inputimage[inputstruct[0].limbxpos - !param.soldiskr : inputstruct[0].limbxpos + !param.soldiskr,$
    inputstruct[0].limbypos - !param.soldiskr : inputstruct[0].limbypos + !param.soldiskr]

    badcrop = acrop[0:-6,0:-4]

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

    CGIMAGE,badcrop,/k,/axes
    PLOT_EDGES,xpb,x0=.5,y0=.5,dcolor=220,thick=3
    PLOT_EDGES,ypb,x0=.5,y0=.5,dcolor=0,thick=3
    PLOT_EDGES,xpb*ypb,x0=.5,y0=.5,dcolor=100,thick=3

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

    adjacent = WHERE(uniq_row-SHIFT(uniq_row,1) eq 1,num_adj)
    if num_adj ne 1 then adjacent = WHERE(uniq_row-SHIFT(uniq_row,1) eq -1)
    c = WHERE( [uniq_row_stack[adjacent-1],uniq_row_stack[adjacent]] eq $
            MIN([uniq_row_stack[adjacent-1],uniq_row_stack[adjacent]]))
    d = (FLTARR(N_ELEMENTS(uniq_row)) +1)
    d[c+adjacent-1] = 0
    ypos = (uniq_row*d)[WHERE(uniq_row*d ne 0)]


    adjacent = WHERE(uniq_col-SHIFT(uniq_col,1) eq 1,num_adj)
    if num_adj ne 1 then adjacent = WHERE(uniq_col-SHIFT(uniq_col,1) eq -1)
    c = WHERE( [uniq_col_stack[adjacent-1],uniq_col_stack[adjacent]] eq $
            MIN([uniq_col_stack[adjacent-1],uniq_col_stack[adjacent]]))
    d = (FLTARR(N_ELEMENTS(uniq_col)) +1)
    d[c+adjacent-1] = 0
    xpos = (uniq_col*d)[WHERE(uniq_col*d ne 0)]

    ; if xpos and ypos are in cross_col and cross_row, it's OK
    
    ; cross_col = cross_col[UNIQ(cross_col,SORT(cross_col))]
    ; cross_row = cross_row[UNIQ(cross_row,SORT(cross_row))]

    superset = [cross_col,xpos]
    if N_ELEMENTS(superset[UNIQ(superset,SORT(superset))]) gt 0 then xpos = xpos
    superset = [cross_row,ypos]
    if N_ELEMENTS(superset[UNIQ(superset,SORT(superset))]) gt 0 then ypos = ypos

    coords = FLTARR(3,3)
    coords[0,*] = cross_col
    coords[1,*] = cross_row

    ; if any row of chords does not have an xpos or ypos component, eliMINate it

    ; >> print,coords
    ;   16.0000      15.0000
    ;   16.0000      16.0000
    ;   16.0000      36.0000

    ; >> print,xpos
    ;       16.0000
    ; >> print,ypos
    ;       16.0000      36.0000

    ; How to tie these two things together?

    ; Look at one at a time

    ; coords[0,0] -> is it an xpos?
    ; coords[0,1] -> is it a ypos? 
    ; if so, coords[*,0] is OK

    ; go to next one
    ; crosses = REPLICATE({fid:[0,0],good:0},5)
    ; why 5?
    ; count=0
    ; Number of fiducials isn't something we know beforehand, use pointers?
    ; or... empty arrays?
    places = 0
    for jj=0,2 do begin
            ; ; coords[2,jj] = TOTAL(coords[0,jj] eq xpos) ne 0 and $
            ; ;      TOTAL(coords[1,jj] eq ypos) ne 0 ? 1 : 0
            ;  fid[*,jj] = TOTAL(coords[0,jj] eq xpos) ne 0 and $
            ;      TOTAL(coords[1,jj] eq ypos) ne 0 ? coords[0:1,jj] : 0
            ; ; crosses[jj].fid = TOTAL(coords[0,jj] eq xpos) ne 0 and $
            ; ;      TOTAL(coords[1,jj] eq ypos) ne 0 ? coords[0:1,jj] : 0
        if TOTAL(coords[0,jj] eq xpos) ne 0 and TOTAL(coords[1,jj] eq ypos) ne 0 then begin
            ; crosses[count].fid = coords[0:1,jj]
            places = [places,coords[0:1,jj]]
            ; crosses[count].good=1
            ; count++
        endif
    endfor
    
    places=REFORM(places[WHERE(places ne 0)],2,N_ELEMENTS(places)/2)
endfor



stop
return,places
end
































