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
        
        x_startresult   = LINFIT(xarr,a[jj].limbxstrips[n].startpoints)
        x_endresult     = LINFIT(xarr,a[jj].limbxstrips[n].endpoints)
        y_startresult   = LINFIT(yarr,a[jj].limbystrips[n].startpoints)
        y_endresult     = LINFIT(yarr,a[jj].limbystrips[n].endpoints)


        x_begusable = (a[jj].thresh - x_startresult[0])/x_startresult[1]
        x_stripbeg =  a[jj].limbxstrips[n].begindex + x_begusable
        x_endusable   = (a[jj].thresh - x_endresult[0])/x_endresult[1]
        x_stripend    =  a[jj].limbxstrips[n].endindex + x_endusable
        ;Quick solution is to take the not-infite means, long-term is to get better limb chords
        xlenarr[n] = MEAN([[x_stripend],[x_stripbeg]],/NaN)


        y_begusable = (a[jj].thresh - y_startresult[0])/y_startresult[1]
        y_stripbeg =  a[jj].limbystrips[n].begindex + y_begusable
        y_endusable   = (a[jj].thresh - y_endresult[0])/y_endresult[1]
        y_stripend    =  a[jj].limbystrips[n].endindex + y_endusable
        ylenarr[n] = MEAN([[y_stripend],[y_stripbeg]],/NaN)
    endfor    

    ; Get the midpoint of the chords
    a[jj].limbxpos = MEAN(xlenarr[WHERE(xlenarr ne 0)],/NaN) + a[jj].xpos - !param.crop_box
    a[jj].limbypos = MEAN(ylenarr[WHERE(ylenarr ne 0)],/NaN) + a[jj].ypos - !param.crop_box
endfor


RETURN,a
END