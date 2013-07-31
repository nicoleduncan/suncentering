FUNCTION npixfit, inputstruct, inputimage
;+
;   :Description:
;       Linear fit to limb strips with an arbitrary number of limb pixels
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
a = cyoalimbstrips(inputstruct,inputimage)

xlen    = 0
ylen    = 0
xarr    = FINDGEN(N_ELEMENTS(a[0].limbxstrips[0].startpoints))
yarr    = FINDGEN(N_ELEMENTS(a[0].limbystrips[0].startpoints))
xlenarr = FLTARR(N_ELEMENTS(a[0].limbxstrips),/nozero)
ylenarr = FLTARR(N_ELEMENTS(a[0].limbystrips),/nozero)


; Collapsed version:
for jj = 0,N_ELEMENTS(a)-1 do begin
    for n = 0, !param.nstrips-1 do begin
        ; Fit line to n limb pixels, n/2 above thresh, n/2 below
        
        ; Don't need to reform the linear fit I guess
        ; ~MAY~ Have fucked up, used startpoint for endresult
        x_startresult   = LINFIT(xarr,a[jj].limbxstrips[n].startpoints)
        x_endresult     = LINFIT(xarr,a[jj].limbxstrips[n].endpoints)
        y_startresult   = LINFIT(yarr,a[jj].limbystrips[n].startpoints)
        y_endresult     = LINFIT(yarr,a[jj].limbystrips[n].endpoints)

        ; if a[jj].limbxstrips[n].begindex gt 0 then begin
            x_begusable = (a[jj].thresh - x_startresult[0])/x_startresult[1]
            x_stripbeg =  a[jj].limbxstrips[n].begindex + x_begusable
        ; endif else begin
            ; x_begusable   = !values.f_nan
            ; x_stripbeg    = !values.f_nan
        ; endelse

        ; if a[jj].limbxstrips[n].endindex gt 0 then begin
            x_endusable   = (a[jj].thresh - x_endresult[0])/x_endresult[1]
            x_stripend    =  a[jj].limbxstrips[n].endindex + x_endusable
        ; endif else begin
            ; x_endusable   = !values.f_nan
            ; x_stripend    = !values.f_nan
        ; endelse

        ; Stick the midpoints in an array to take the mean of later
        xlenarr[n] = MEAN([[x_stripend],[x_stripbeg]],/NaN)

    ; endfor    

    ; for n = 0, !param.nstrips-1 do begin
        ; y_startresult = LINFIT(yarr,a[jj].limbystrips[n].startpoints)
        ; y_endresult = LINFIT(yarr,a[jj].limbystrips[n].endpoints)

        ; if a[jj].limbystrips[n].begindex gt 0 then begin
            y_begusable = (a[jj].thresh - y_startresult[0])/y_startresult[1]
            y_stripbeg =  a[jj].limbystrips[n].begindex + y_begusable
        ; endif else begin
            ; y_begusable   = !values.f_nan
            ; y_stripbeg    = !values.f_nan
        ; endelse

        ; if a[jj].limbystrips[n].endindex gt 0 then begin
            y_endusable   = (a[jj].thresh - y_endresult[0])/y_endresult[1]
            y_stripend    =  a[jj].limbystrips[n].endindex + y_endusable
        ; endif else begin
            ; y_endusable   = !values.f_nan
            ; y_stripend    = !values.f_nan
        ; endelse

        ;Quick solution is to take the not-infite means, long-term is to get better lib chords
        ylenarr[n] = MEAN([[y_stripend],[y_stripbeg]],/NaN)
    endfor    

    ; Get the midpoint of the chords
    a[jj].limbxpos = MEAN(xlenarr[WHERE(xlenarr ne 0)],/NaN) + a[jj].xpos - !param.crop_box
    a[jj].limbypos = MEAN(ylenarr[WHERE(ylenarr ne 0)],/NaN) + a[jj].ypos - !param.crop_box
endfor


; ; Deal with rows
; for jj = 0,N_ELEMENTS(a)-1 do begin
;     for n = 0, !param.nstrips-1 do begin
;         ; Fit line to n limb pixels, n/2 above thresh, n/2 below
        
;         ; Don't need to reform the linear fit I guess
;         ; ~MAY~ Have fucked up, used startpoint for endresult
;         startresult = LINFIT(xarr,a[jj].limbxstrips[n].startpoints)
;         endresult = LINFIT(xarr,a[jj].limbxstrips[n].endpoints)

;         if a[jj].limbxstrips[n].begindex gt 0 then begin
;             begusable = (a[jj].thresh - startresult[0])/startresult[1]
;             stripbeg =  a[jj].limbxstrips[n].begindex + begusable
;         endif else begin
;             begusable   = !values.f_nan
;             stripbeg    = !values.f_nan
;         endelse

;         if a[jj].limbxstrips[n].endindex gt 0 then begin
;             endusable   = (a[jj].thresh - endresult[0])/endresult[1]
;             stripend    =  a[jj].limbxstrips[n].endindex + endusable
;         endif else begin
;             endusable   = !values.f_nan
;             stripend    = !values.f_nan
;         endelse

;         ; Stick the midpoints in an array to take the mean of later
;         xlenarr[n] = MEAN([[stripend],[stripbeg]],/NaN)

;     endfor    

;     for n = 0, !param.nstrips-1 do begin
;         startresult = LINFIT(yarr,a[jj].limbystrips[n].startpoints)
;         endresult = LINFIT(yarr,a[jj].limbystrips[n].endpoints)

;         if a[jj].limbystrips[n].begindex gt 0 then begin
;             begusable = (a[jj].thresh - startresult[0])/startresult[1]
;             stripbeg =  a[jj].limbystrips[n].begindex + begusable
;         endif else begin
;             begusable   = !values.f_nan
;             stripbeg    = !values.f_nan
;         endelse

;         if a[jj].limbystrips[n].endindex gt 0 then begin
;             endusable   = (a[jj].thresh - endresult[0])/endresult[1]
;             stripend    =  a[jj].limbystrips[n].endindex + endusable
;         endif else begin
;             endusable   = !values.f_nan
;             stripend    = !values.f_nan
;         endelse

;         ;Quick solution is to take the not-infite means, long-term is to get better lib chords
;         ylenarr[n] = MEAN([[stripend],[stripbeg]],/NaN)
;     endfor    

;     ; Get the midpoint of the chords
;     a[jj].limbxpos = MEAN(xlenarr[WHERE(xlenarr ne 0)],/NaN) + a[jj].xpos - !param.crop_box
;     a[jj].limbypos = MEAN(ylenarr[WHERE(ylenarr ne 0)],/NaN) + a[jj].ypos - !param.crop_box
; endfor

RETURN,a
END