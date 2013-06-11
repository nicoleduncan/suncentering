FUNCTION fourpixfit, inputstruct, inputimage
;+
;   :Description:
;       Linear fit to limb strips 
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
a = makeslimlimbstrips(inputstruct,inputimage)

xlen    = 0
ylen    = 0
xarr    = FINDGEN(N_ELEMENTS(a[0].limbxstrips[0].startpoints))
yarr    = FINDGEN(N_ELEMENTS(a[0].limbystrips[0].startpoints))
xlenarr = FLTARR(N_ELEMENTS(a[0].limbxstrips))
ylenarr = FLTARR(N_ELEMENTS(a[0].limbystrips))

;Deal with rows
for jj = 0,N_ELEMENTS(a)-1 do begin
    for n=0, !param.nstrips-1 do begin
        
        startresult = REFORM(LINFIT(xarr,a[jj].limbxstrips[n].startpoints))
        endresult = REFORM(LINFIT(xarr,a[jj].limbxstrips[n].startpoints))
        
        if a[jj].limbxstrips[n].begindex gt 0 then begin
            begusable = (a[jj].thresh - startresult[0])/startresult[1]
            stripbeg =  a[jj].limbxstrips[n].begindex + begusable
        endif else begin
            begusable   = 0
            stripbeg    = 0
        endelse

        if a[jj].limbxstrips[n].endindex gt 0 then begin
            endusable   = (a[jj].thresh - endresult[0])/endresult[1]
            stripend    =  a[jj].limbxstrips[n].endindex + endusable
        endif else begin
            endusable   = 0
            stripend    = 0
        endelse

        ; Stick the midpoints in an array to take the mean of later
        xlenarr[n] = MEAN([[stripend],[stripbeg]],/NaN)

    endfor    

    for n=0, !param.nstrips-1 do begin
        startresult = REFORM(LINFIT(yarr,a[jj].limbystrips[n].startpoints))
        endresult = REFORM(LINFIT(yarr,a[jj].limbystrips[n].startpoints))

        if a[jj].limbystrips[n].begindex gt 0 then begin
            begusable = (a[jj].thresh - startresult[0])/startresult[1]
            stripbeg =  a[jj].limbystrips[n].begindex + begusable
        endif else begin
            begusable   = 0
            stripbeg    = 0
        endelse

        if a[jj].limbystrips[n].endindex gt 0 then begin
            endusable   = (a[jj].thresh - endresult[0])/endresult[1]
            stripend    =  a[jj].limbystrips[n].endindex + endusable
        endif else begin
            endusable   = 0
            stripend    = 0
        endelse

        ;Quick solution is to take the not-infite means, long-term is to get better lib chords
        ylenarr[n] = MEAN([[stripend],[stripbeg]],/NaN)
    endfor    

    ; Get the midpoint of the chords
    a[jj].limbxpos = MEAN(xlenarr[WHERE(xlenarr ne 0)],/NaN) + a[jj].xpos - !param.crop_box
    a[jj].limbypos = MEAN(ylenarr[WHERE(ylenarr ne 0)],/NaN) + a[jj].ypos - !param.crop_box
endfor

RETURN,a
END