FUNCTION makeslimlimbstrips, inputstruct, inputimage
;+
;   :Description:
;       Makes limb strips from whole strips
;
;   :Params:
;       inputstruct: in, required
;           Structure containing all the solar information
;
;       inputimage: in, required
;           The raw input image
;
;-

a = makestrips(inputstruct,inputimage)
limbwidth = !param.ministrip_length/2
for jj = 0,N_ELEMENTS(inputstruct)-1 do begin
    FOR i = 0, !param.nstrips - 1 DO BEGIN
        col_where = WHERE((a[jj].ystrips)[i].ARRAY GT a[jj].thresh)
        row_where = WHERE((a[jj].xstrips)[i].ARRAY GT a[jj].thresh)

        a[jj].limbxstrips[i].rowindex = a[jj].xstrips[i].rowindex
        a[jj].limbystrips[i].colindex = a[jj].ystrips[i].colindex

        IF row_where[0] NE -1 THEN BEGIN
            ; startpoints is the cut down strip with length = ministrip_length and contains
            ; the indices from row_where[0] +/- limbwidth
            
            a[jj].limbxstrips[i].startpoints  = $
                (a[jj].xstrips[i].array)[row_where[0] - limbwidth:row_where[0]+1]   
            ; begindex is the index of the strip where it begins. 
            ; e.g., the array is 5 long, starts from index 9 and is centered around index 11
            a[jj].limbxstrips[i].begindex   = FIX(row_where[0] - limbwidth)
        ENDIF
        IF row_where[-1] NE -1 THEN BEGIN
            a[jj].limbxstrips[i].endpoints  = (a[jj].xstrips[i].array)[row_where[-1] - limbwidth:row_where[-1] + 1]   
            a[jj].limbxstrips[i].endindex   = FIX(row_where[-1] - limbwidth)
        ENDIF

        IF col_where[0] NE -1 THEN BEGIN
            a[jj].limbystrips[i].startpoints  = (a[jj].ystrips[i].array)[col_where[0] - limbwidth:col_where[0] + 1]   
            a[jj].limbystrips[i].begindex     = FIX(col_where[0] - limbwidth)
        ENDIF
        IF col_where[-1] NE -1 THEN BEGIN
            a[jj].limbystrips[i].endpoints  = (a[jj].ystrips[i].array)[col_where[-1] - limbwidth:col_where[-1] + 1]   
            a[jj].limbystrips[i].endindex     = FIX(col_where[-1] - limbwidth)
        ENDIF
    ENDFOR
endfor

RETURN,a
END