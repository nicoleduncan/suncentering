FUNCTION limbfit, inputstruct, inputimage
;+
;   :Description:
;       Fits 2nd order polynomial to limb strips 
;
;   :Params:
;       inputstruct: in, required
;           Structure containing all the solar information
;
;       inputimage: in, required
;           Starting input image
;
;   :Keywords:
;       
;-

; Run the program to get our structures
a=makelimbstrips(inputstruct,inputimage)

xlen    = 0
xsum    = 0
xnum    = 0   
ylen    = 0
ysum    = 0
ynum    = 0
xarr    = FINDGEN(N_ELEMENTS(a[0].limbxstrips[0].startpoints))
yarr    = FINDGEN(N_ELEMENTS(a[0].limbystrips[0].startpoints))
tx      = FINDGEN(N_ELEMENTS(a[0].limbxstrips[0].startpoints) * 1000)/100
xlenarr = FINDGEN(N_ELEMENTS(a[0].limbxstrips))
ylenarr = FINDGEN(N_ELEMENTS(a[0].limbystrips))

;Deal with rows
for jj = 0,n_elements(a)-1 do begin
    for n=0, !param.nstrips-1 do begin
        ; Using fz_roots instead of SPLINE interpolating. Saving lines and making code more readable

        startresult     = REFORM(POLY_FIT(xarr,a[jj].limbxstrips[n].startpoints, !param.order))
        endresult       = REFORM(POLY_FIT(xarr,a[jj].limbxstrips[n].endpoints, !param.order))

        ; Solving for roots but want to include threshold value
        startresult[0]  -=a[jj].thresh
        endresult[0]    -=a[jj].thresh

        if a[jj].limbxstrips[n].begindex gt 0 then begin
            ; Get roots (complex)
            begroots    = FZ_ROOTS(startresult)
            ; Take only roots with no imaginary components
            begusable   = (REAL_PART(begroots))[WHERE(IMAGINARY(begroots) eq 0.)]
            ; Find smallest root (apparently I have to choose the smaller one)
            ; Or i can find the midpoints using the other two roots then take the average of the two,
            ; that way works too, but why would I do that?
            begusable   = (begusable[WHERE(begusable gt 0)])[0]
            stripbeg    = a[jj].limbxstrips[n].begindex + begusable
        endif else begin
            begusable   = 0
            stripbeg    = 0
        endelse

        if a[jj].limbxstrips[n].endindex gt 0 then begin
            endroots    = FZ_ROOTS(endresult)
            endusable   = (REAL_PART(endroots))[WHERE(IMAGINARY(endroots) eq 0.)]
            endusable   = (endusable[WHERE(endusable gt 0)])[0]
            stripend    = a[jj].limbxstrips[n].endindex + endusable
        endif else begin
            endusable   = 0
            stripend    = 0
        endelse

        ; Stick the midpoints in an array to take the mean of later
        xlenarr[n] = MEAN([[stripend],[stripbeg]])
    endfor    

    for n=0, !param.nstrips-1 do begin
        startresult     = REFORM(POLY_FIT(yarr,a[jj].limbystrips[n].startpoints, !param.order))
        endresult       = REFORM(POLY_FIT(yarr,a[jj].limbystrips[n].endpoints, !param.order))

        startresult[0]  -=a[jj].thresh
        endresult[0]    -=a[jj].thresh

        if a[jj].limbystrips[n].begindex gt 0 then begin
            begroots    = FZ_ROOTS(startresult)
            begusable   = (REAL_PART(begroots))[WHERE(IMAGINARY(begroots) eq 0.)]
            begusable   = (begusable[WHERE(begusable gt 0)])[0]
            stripbeg    = a[jj].limbystrips[n].begindex + begusable
        endif else begin
            begusable   = 0
            stripbeg    = 0
        endelse

        if a[jj].limbystrips[n].endindex gt 0 then begin
            endroots    = FZ_ROOTS(endresult)
            endusable   = (REAL_PART(endroots))[WHERE(IMAGINARY(endroots) eq 0.)]
            endusable   = (endusable[WHERE(endusable gt 0)])[0]
            stripend    = a[jj].limbystrips[n].endindex + endusable
            
        endif else begin
            endusable   = 0
            stripend    = 0
        endelse

        ylenarr[n] = MEAN([[stripend],[stripbeg]])
    endfor    

    ; Get the midpoint of the chords
    a[jj].limbxpos = MEAN(xlenarr[WHERE(xlenarr ne 0)]) + a[jj].xpos - !param.crop_box
    a[jj].limbypos = MEAN(ylenarr[WHERE(ylenarr ne 0)]) + a[jj].ypos - !param.crop_box
endfor

RETURN,a
END