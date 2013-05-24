FUNCTION makelimbstrips, inputstruct, inputimage
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

; have to byte it since we read the ministrip_length as a float
ministrip_side_buffer = BYTE( !param.ministrip_length)/2 

; Contains coordinates of chord endpoints
rowchord_endpoints = FLTARR(2, !param.nstrips)
colchord_endpoints = FLTARR(2, !param.nstrips)


for jj = 0,N_ELEMENTS(inputstruct)-1 do begin
    FOR i = 0, !param.nstrips - 1 DO BEGIN
        col_where = WHERE((a[jj].ystrips)[i].ARRAY GT a[jj].thresh)
        row_where = WHERE((a[jj].xstrips)[i].ARRAY GT a[jj].thresh)
        colchord_endpoints[0,i] = col_where[0]
        colchord_endpoints[1,i] = col_where[-1]
        rowchord_endpoints[0,i] = row_where[0]
        rowchord_endpoints[1,i] = row_where[-1]
        a[jj].limbxstrips[i].rowindex = a[jj].xstrips[i].rowindex

        IF rowchord_endpoints[0,i] NE -1 THEN BEGIN
            ; startpoints is the cut down strip with length = ministrip_length and contains
            ; the indices from rowchord_endpoints[0,i] +/- ministrip_side_buffer
            a[jj].limbxstrips[i].startpoints  = $
            ; If chord is too long, it tries to crop from outside of image file
                (a[jj].xstrips[i].array)[rowchord_endpoints[0,i]-ministrip_side_buffer:$
                rowchord_endpoints[0,i]+ministrip_side_buffer]   
            ; begindex is the index of the strip where it begins. 
            ; e.g., the array is 5 long, starts from index 9 and is centered around index 11
            a[jj].limbxstrips[i].begindex   = FIX(rowchord_endpoints[0,i] - ministrip_side_buffer)
        ENDIF
        IF rowchord_endpoints[1,i] NE -1 THEN BEGIN
            a[jj].limbxstrips[i].endpoints  = (a[jj].xstrips[i].array)[rowchord_endpoints[1,i]-ministrip_side_buffer:rowchord_endpoints[1,i]+ministrip_side_buffer]   
            a[jj].limbxstrips[i].endindex   = FIX(rowchord_endpoints[1,i] - ministrip_side_buffer)
        ENDIF

        a[jj].limbystrips[i].colindex = a[jj].ystrips[i].colindex
        IF rowchord_endpoints[0,i] NE -1 THEN BEGIN
            a[jj].limbystrips[i].startpoints  = (a[jj].ystrips[i].array)[rowchord_endpoints[0,i]-ministrip_side_buffer:rowchord_endpoints[0,i]+ministrip_side_buffer]   
            a[jj].limbystrips[i].begindex     = FIX(rowchord_endpoints[0,i] - ministrip_side_buffer)
        ENDIF
        IF rowchord_endpoints[1,i] NE -1 THEN BEGIN
            a[jj].limbystrips[i].endpoints  = (a[jj].ystrips[i].array)[rowchord_endpoints[1,i]-ministrip_side_buffer:rowchord_endpoints[1,i]+ministrip_side_buffer]   
            a[jj].limbystrips[i].endindex     = FIX(rowchord_endpoints[1,i] - ministrip_side_buffer)
        ENDIF
    ENDFOR
endfor

RETURN,a
END