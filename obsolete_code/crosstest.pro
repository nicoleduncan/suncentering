FUNCTION crosstest, inputimage, inputstruct

for i = 0,n_elements(inputstruct)-1 do begin

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
; xpb = SHIFT_DIFF(EMBOSS(badcrop,/edge_truncate,az=0),dir=3,/edge_truncate) lt thresh
; ypb = SHIFT_DIFF(EMBOSS(badcrop,az=90,/edge_truncate),dir=1,/edge_truncate) lt thresh

xpb = a lt thresh
ypb = b lt thresh

; cgimage,xpb*badcrop,/k

; Remember, we need at least 5 fiducial pixels in image for filter to recognize it

s = SIZE(badcrop,/d)

ncol = s[0]
nrow = s[1]
ind_col = WHERE(xpb eq 1) mod ncol
ind_row = WHERE(ypb eq 1)/ncol

uniq_col = ind_col[uniq(ind_col,sort(ind_col))]
uniq_row = ind_row[uniq(ind_row,sort(ind_row))]
n_col_indices = n_elements(uniq_col)
n_row_indices = n_elements(uniq_row)
xpos = fltarr(n_col_indices)
ypos = fltarr(n_row_indices)
uniq_col_stack = fltarr(n_col_indices)
uniq_row_stack = fltarr(n_row_indices)

for i = 0,n_col_indices-1 do begin
    uniq_col_stack[i] = n_elements(where(ind_col eq uniq_col[i] eq 1))
endfor

for i = 0,n_row_indices-1 do begin
    uniq_row_stack[i] = n_elements(where(ind_row eq uniq_row[i] eq 1))
endfor

CGIMAGE,badcrop,/k,/axes
PLOT_EDGES,xpb,x0=.5,y0=.5,dcolor=220,thick=3
PLOT_EDGES,ypb,x0=.5,y0=.5,dcolor=0,thick=3
PLOT_EDGES,xpb*ypb,x0=.5,y0=.5,dcolor=100,thick=3

; >> print,uniq_col
;           15          16
; >> print,uniq_col_stack
;       1.00000      11.0000
; >> print,uniq_row      
;           15          16          36
; >> print,uniq_row_stack
;       3.00000      8.00000      8.00000

for i = 0,n_col_indices-1 do begin
    xpos[i] = uniq_col[(where(uniq_col_stack eq max(uniq_col_stack)))[0]]
    uniq_col_stack[(where(uniq_col_stack eq max(uniq_col_stack)))[0]] = 0
endfor
for i = 0,n_row_indices-1 do begin
    ypos[i] = uniq_row[(where(uniq_row_stack eq max(uniq_row_stack)))[0]]
    uniq_row_stack[(where(uniq_row_stack eq max(uniq_row_stack)))[0]] = 0
endfor

; >> print,xpos
;       16.0000      15.0000      0.00000
; >> print,ypos
;       16.0000      36.0000      15.0000

; must fix xpos, ypos
; is there a way to look at N fiducials?

cross_col = WHERE(xpb*ypb eq 1) mod ncol
cross_row = WHERE(xpb*ypb eq 1)/ncol

; okay, there are 2 fiducials, we know 2 because

n_x = n_elements(cross_col[uniq(cross_col,sort(cross_col))])
n_y = n_elements(cross_row[uniq(cross_row,sort(cross_row))])

; now here's the kicker, n_y has 3 but only really 2
; how to rule one y out? one of the y is an adjacent pixel to another element in y
; eliminate the less populous one and BAM, 2 y positions
;
; now we have 1 x position and 2 y positions
;
; if an element of xpos is in cross_col, it's true
; if an element of ypos is in cross_row, it's true

; first, figure out which cross_Z is more populous
for i = 0,n_row_indices-1 do begin
    uniq_row_stack[i] = n_elements(where(ind_row eq uniq_row[i] eq 1))
endfor

for i = 0,n_col_indices-1 do begin
    uniq_col_stack[i] = n_elements(where(ind_col eq uniq_col[i] eq 1))
endfor


;Works, as well as starting out one, but in scen1
; uniq_row = [16,15,36]
; uniq_row_stack = [8,3,8]
; scen3 works with scen2
; uniq_row=[10,30,29]
; uniq_row_stack=[8,8,3]
;scen4 same as scen1
; uniq_row = [10,29,30]
; uniq_row_stack = [8,3,8]

; a is for scen1
; a = where(uniq_row-shift(uniq_row,1) eq 1,num_a)
; ; b is for scen2
; ; b = where(uniq_row-shift(uniq_row,1) eq -1,num_b)

; c = where( [uniq_row_stack[a-1],uniq_row_stack[a]] eq $
;     min([uniq_row_stack[a-1],uniq_row_stack[a]]))

; d = (fltarr(n_elements(uniq_row)) +1)
; d[c+a-1] = 0

; print,(uniq_row*d)[where(uniq_row*d ne 0)]

; c = where( [uniq_row_stack[b-1],uniq_row_stack[b]] eq $
;     min([uniq_row_stack[b-1],uniq_row_stack[b]]))
; d = (fltarr(n_elements(uniq_row)) +1)
; d[c+b-1]=0
; print,(uniq_row*d)[where(uniq_row*d ne 0)]




; ; THE DECISION MAKER
; uniq_row = [0,uniq_row,0]
; uniq_row_stack = [0,uniq_row_stack,0]

; a = where(uniq_row-shift(uniq_row,1) eq 1,num_a)
; b = where(uniq_row-shift(uniq_row,1) eq -1,num_b)

; if num_a eq 1 then begin
;     c = where( [uniq_row_stack[a-1],uniq_row_stack[a]] eq $
;         min([uniq_row_stack[a-1],uniq_row_stack[a]]))

;     d = (fltarr(n_elements(uniq_row)) +1)
;     d[c+a-1] = 0
;     xpos = (uniq_row*d)[where(uniq_row*d ne 0)]
; endif else begin
;     c = where( [uniq_row_stack[b-1],uniq_row_stack[b]] eq $
;         min([uniq_row_stack[b-1],uniq_row_stack[b]]))

;     d = (fltarr(n_elements(uniq_row)) +1)
;     d[c+b-1]=0
;     xpos = (uniq_row*d)[where(uniq_row*d ne 0)]
; endelse

; THE DECISION MAKER, assumes we have adjacent row/cols

uniq_col = [0,uniq_col,0]
uniq_col_stack = [0,uniq_col_stack,0]
uniq_row = [0,uniq_row,0]
uniq_row_stack = [0,uniq_row_stack,0]

print,'uniq_row'
print,uniq_row
print,'uniq_row_stack'
print,uniq_row_stack
print,'uniq_col'
print,uniq_col
print,'uniq_col_stack'
print,uniq_col_stack


a = where(uniq_row-shift(uniq_row,1) eq 1,num_a)

if num_a ne 1 then a = where(uniq_row-shift(uniq_row,1) eq -1)

c = where( [uniq_row_stack[a-1],uniq_row_stack[a]] eq $
        min([uniq_row_stack[a-1],uniq_row_stack[a]]))
d = (fltarr(n_elements(uniq_row)) +1)
d[c+a-1] = 0
ypos = (uniq_row*d)[where(uniq_row*d ne 0)]

print,ypos

a = where(uniq_col-shift(uniq_col,1) eq 1,num_a)

if num_a ne 1 then a = where(uniq_col-shift(uniq_col,1) eq -1)

c = where( [uniq_col_stack[a-1],uniq_col_stack[a]] eq $
        min([uniq_col_stack[a-1],uniq_col_stack[a]]))
d = (fltarr(n_elements(uniq_col)) +1)
d[c+a-1] = 0
xpos = (uniq_col*d)[where(uniq_col*d ne 0)]

print,xpos


; if xpos and ypos are in cross_col and cross_row, it's OK
cross_col = cross_col[uniq(cross_col,sort(cross_col))]
cross_row = cross_row[uniq(cross_row,sort(cross_row))]

superset = [cross_col,xpos]
if n_elements(superset[uniq(superset,sort(superset))]) gt 0 then print, "xpos is fine"
superset = [cross_row,ypos]
if n_elements(superset[uniq(superset,sort(superset))]) gt 0 then print, "ypos is fine"



; what do we actually want to return? for an input image and structure, 
; we want x and y fiducial positions

stop






; ; THIS DOESN'T WORK
; uniq_row = [15,14,30]
; uniq_row_stack = [15,1,15]
; ; uniq_row = [30,15,14]
; uniq_row = [0,uniq_row,0]
; uniq_row_stack = [0,uniq_row_stack,0]

; ; figures out where things are adjacent
; a = where(uniq_row-shift(uniq_row,1) eq 1,num_a)
; b = where(uniq_row-shift(uniq_row,1) eq -1,num_b)

; if num_a ne 0 then begin
; ; makes the less populous one zero
; c = where([uniq_row_stack[a-1],uniq_row_stack[a]] eq $
;         min([uniq_row_stack[a-1],uniq_row_stack[a]]))

; notgood = a-1+c
; uniq_row_stack[notgood]=0

; print,((uniq_row_stack ne 0)*uniq_row)[where( (uniq_row_stack ne 0)*uniq_row ne 0)]
; ; This works for a, now for b?
; endif else begin
; d = where([uniq_row_stack[a],uniq_row_stack[a+1]] eq $
;         min([uniq_row_stack[a],uniq_row_stack[a+1]]))

; uniq_row_stack[a+d]=0
; print,((uniq_row_stack ne 0)*uniq_row)[where( (uniq_row_stack ne 0)*uniq_row ne 0)]

; endelse

; how to make program realize
; xpos = 16
; ypos = 16,36

; we can either use the cross-test as a validation thing or as a thing that decides xpos
; i.e.
; get xpos -> check if xpos is in cross
; or
; check is xpos is in cross -> get xpos

stop
; get col/row indices
for i = 0,2 do begin
    ; if i eq 0 then begin
    ;     xpos[i] = MODE(ind_col)
    ;     ypos[i] = MODE(ind_row)
    ; endif else begin
    ;     xpos[i] = MODE(ind_col[WHERE(ind_col ne xpos[i-1])])
    ;     ypos[i] = MODE(ind_row[WHERE(ind_row ne ypos[i-1])])
    ; endelse

    case i of
      0: begin
        xpos[i] = MODE(ind_col)
        ypos[i] = MODE(ind_row)
        end
      1: begin
        xpos[i] = MODE(ind_col[WHERE(ind_col ne xpos[i-1])])
        ypos[i] = MODE(ind_row[WHERE(ind_row ne ypos[i-1])])
      end 
      2: begin
      stop
        xpos[i] = MODE(ind_col[WHERE(ind_col ne xpos[i-1] and ind_col ne xpos[i-2])])
        ypos[i] = MODE(ind_row[WHERE(ind_row ne ypos[i-1] and ind_row ne ypos[i-2])])
      end
    endcase
endfor

xpos = [0,xpos,0]
ypos = [0,ypos,0]



a = where(xpos - shift(xpos,1) eq 1,n_a)
b = where(xpos - shift(xpos,-1) eq 1,n_b)
c = where(ypos - shift(ypos,1) eq 1,n_c)
d = where(ypos - shift(ypos,-1) eq 1,n_d)
stop

; gotta pad with 0s so that we don't get wraparound stuff
; ypos=[0,ypos,0]
; xpos=[0,xpos,0]

; this is getting confusing, fast. Is there a simpler way?


; Here the bad boy decision-maker here
if n_a gt 0 then begin
    xpos = [xpos[0:a-1],xpos[a+1,n_elements(a)-1]]
endif
if n_b gt 0 then begin
; but hold on, xpos is 16,15,16, now what
    badxpos = (xpos[where(xpos - shift(xpos,-1) eq 1):where(xpos - shift(xpos,-1) eq 1)+1])[1]
    wnxpos = xpos[where(xpos ne badxpos)]
endif
if n_c gt 0 then begin
    badypos = (ypos[where(ypos - shift(ypos,1) eq 1)-1:where(ypos - shift(ypos,1) eq 1)])[1]
    wnypos = ypos[where(ypos ne badypos)]
endif
if n_d gt 0 then begin
    badypos = (ypos[where(ypos - shift(ypos,-1) eq 1):where(ypos - shift(ypos,-1) eq 1)+1])[1]
    wnypos = ypos[where(ypos ne badypos)]
endif

if n_a ne 1 and n_b ne 1 then wnxpos = xpos[0:1]
if n_c ne 1 and n_d ne 1 then wnypos = ypos[0:1]

print,wnxpos
print,wnypos




; ps_start,filename='threshtesh_'+strcompress(i,/rem)+'.eps',/color,/encap
CGIMAGE,badcrop,/k,/axes
PLOT_EDGES,xpb,x0=.5,y0=.5,dcolor=220,thick=3
PLOT_EDGES,ypb,x0=.5,y0=.5,dcolor=0,thick=3
PLOT_EDGES,xpb*ypb,x0=.5,y0=.5,dcolor=100,thick=3
; ps_end,resize=100,/png
; endfor

; need a check for where there is a cross
; If fid is cut off, can only scan 1 less than usual
; how to use this?

; for example, the xpos should be 16, not 15

stop

tmpcrop = input[8:40,12-p:42]
xpb = SHIFT_DIFF(EMBOSS(tmpcrop,/edge_truncate,az=0),dir=3,/edge_truncate) lt thresh
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

; if WHERE(xmcrop eq 1) eq [-1] then print,'col_slice boo' else begin
;     col_slice = FLTARR(N_ELEMENTS(ind_col),nrow)
;     for i=0,N_ELEMENTS(ind_col)-1 do begin
;         col_slice[i,*] = REFORM(tmpcrop[ind_col[i],*])
;         if WHERE(xpb[ind_col[i],*] eq 1) eq [0] then begin
;             print,'cropping 0:6'
;             if N_ELEMENTS(DERIV(DERIV(FLOAT(col_slice[i,0:6])))) gt 0 lt 6 then okb = 0 else okb = 1
;         endif
;         if WHERE(xpb[ind_col[i],*] eq 1) eq [-1] then print,'cropping -7:-1'
;     endfor
; endelse

; if WHERE(ymcrop eq 1) eq [-1] then print,'row_slice boo' else begin
;     row_slice = FLTARR(ncol,N_ELEMENTS(ind_row))
;     for i=0,N_ELEMENTS(ind_row)-1 do begin
;         row_slice[*,i] = REFORM(tmpcrop[*,ind_row[i]])
;         if WHERE(ypb[ind_row[i],*] eq 1) eq [0] then print,'cropping 0:6'
;         if WHERE(ypb[ind_row[i],*] eq 1) eq [-1] then print,'cropping -7:-1'
;     endfor
; endelse

endfor

; stop
okaybit=1
return,okaybit
end