FUNCTION morescratch, input, thresh

bigxpb = SHIFT_DIFF(EMBOSS(input),dir=3) lt thresh
bigypb = SHIFT_DIFF(EMBOSS(input,az=90),dir=1) lt thresh

for p = 0,5 do begin
    tmpcrop = input[10:38+p,8:42]
    xpb = bigxpb[10:38+p,8:42]
    ypb = bigypb[10:38+p,8:42]
    
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



print,ind_col
print,ind_row
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

    ; wait, why is it that if I do something to tmpcrop it wiggs out?

    !p.multi=[0,1,3]
    oldcharsize = !p.charsize
    !p.charsize=2
    ; The right of the vline is where the fiducial is
    window,p
    ; ps_start,filename='betterslice'+strcompress(p,/rem)+'.eps',/encapsulated,/color
        range = (FLOAT(tmpcrop[*,20]))[-6:-1]
        plot,range - mode(tmpcrop),psym=-4,title='array - mode(wholeimage) from [-6:-1] edge of (input[10:'+strcompress(38+p)+',8:42])[*,20]',xs=3,ys=3
        vline,5-p
        hline,-20
        plot,DERIV(range),psym=-4,title='1st deriv of slice',xs=3,ys=3
        vline,5-p
        plot,(DERIV(DERIV(range)))[*],psym=-4,title='2nd deriv of slice',xs=3,ys=3
        vline,5-p
    ; ps_end
    !p.multi=0
    !p.charsize=oldcharsize
endfor
; stop
; wait, why does -20 work?
; ??
; ??????



; Instead of looking only at edge 6 pixels...?

; What is this shit

; cgimage,convol(float(tmpcrop),kernel,/edge_truncate,/center) * (scale_vector(convol(float(tmpcrop),kernel,/edge_truncate,/center)) lt .3),/k

; kernel = [[-.5,1,-.5],[1,1,1],[-.5,1,-.5]]
; kernel = [[0,1,0],[1,1,1],[0,1,0]]
; kernel = [[0,0,1,0,0],[0,0,1,0,0],[1,1,1,1,1],[0,0,1,0,0],[0,0,1,0,0]]
kernel = [[0,0,1,1,0,0],[0,0,1,1,0,0],[1,1,1,1,1,1],[1,1,1,1,1,1],[0,0,1,1,0,0],[0,0,1,1,0,0]]




; fiducial locations
; [24:33,16:27]
; [24:33,0:10]
; [5:15,0:10]
; [5:15,16:27]

first = [24,24,5,5]
second = [33,33,15,15]
third = [16,0,0,16]
fourth = [27,10,10,27]

for hail = 0,3 do begin
    circ = convol(float(tmpcrop),kernel,/edge_truncate,/center)
    crop_circ = circ[first[hail]:second[hail],third[hail]:fourth[hail]]

    ; CONVOL before crop makes the image a little lighter, shape stays the same though

    thresh = .85*max(crop_circ)
    centers=quickfidmask(crop_circ,thresh)
    glorb = tmpcrop[first[hail]:second[hail],third[hail]:fourth[hail]]
    ; glorb[centers.xpos,*] =.7*max(crop_circ)
    ; glorb[*,centers.ypos] = .7*max(crop_circ)

    ; omg, we can just convol() it, then find a quickfidmask!
    finefine = INTERPOLATE(crop_circ,FINDGEN((SIZE(crop_circ,/d))[0] *10)/10.,FINDGEN((SIZE(crop_circ,/d))[1] *10)/10.,/grid,cubic=-.5)
    finefine[centers.xpos * 10,*] =.8*max(finefine)
    finefine[*,10*centers.ypos] = .8*max(finefine)

    !p.multi=[0,2,2]
        window,20 + hail,xsize=700,ysize=1000
        ; ps_start,filename='cropcomp'+strcompress(hail,/rem)+'.eps',/encapsulated,/color,xsize=7,ysize=10
        cgimage,glorb,/k
        cgimage,tmpcrop[first[hail]:second[hail],third[hail]:fourth[hail]],/k
        cgimage,finefine,/k,/axes,title='Interpolated circ_crop'
        cgimage,crop_circ,/k,/axes,title='CONVOL() of fiducial'
        ; ps_end,/png
    !p.multi=0
endfor
; cgimage,glorb,/k,output='glorb.png'
; cgimage,tmpcrop[first[hail]:second[hail],third[hail]:fourth[hail]],/k,output='regularcrop.png'
; cgimage,finefine,/k,/axes,title='Interpolated circ_crop',output='interpcrop.png'
; cgimage,crop_circ,/k,/axes,title='CONVOL() of fiducial',output='convolcrop.png'
; ********
; ********
; So from the result of these plots, we see that using quickfidmask works well for isolated fiducials but not really
; on edge fiducials. 
; ********
; ********

; Now we're more or less back to where we started?

; HOW TO DEAL WITH CF

; window,3
; plot,DERIV(float(TS_SMOOTH(reform(range),10) -  range)),psym=-4,title='deriv of ts_smooth(x) - x'
; vline,18
; vline,24
; window,4
; cgimage,SHIFT_DIFF(EMBOSS(tmpcrop),dir=3),/k,title='filter of cropped',/axes
; window,5
; cgimage,(shift_diff(emboss(input),dir=3))[10:43,8:42],/k,title='cropped',/axes
; window,6
; cgimage,SHIFT_DIFF(EMBOSS(input[10:40,8:42],/edge_truncate),dir=3,/edge_truncate),/k,'filter of cropped with edge_truncate',/axes




; So this is actually a good way to do it?
; Not if the fiducial is on the edge, bro.
; if this is okay, then just count how far it is from the edge


a = emboss(float(tmpcrop),az=180,/edge_truncate)
b = emboss(float(tmpcrop),az=0,/edge_truncate)
cgimage,abs(a-b),/k
; not that great of an image >_>








stop


return,okaybit
end