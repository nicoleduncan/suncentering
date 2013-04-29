PRO makelimbstrips, thresh, xstrips, ystrips, region=region, time=time
;+
;   :Description:
;       Makes limb strips from full-length strips
;
;   :Params:
;       thresh : out, required, type=float
;           Threshold used to select pixels
;       xstrips : out, required, type=structure
;           Structure containing row strips
;       ystrips : out, required, type=structure
;           Structure containing column strips
;
;   :Keywords:
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       time : in, optional
;           Prints the elapsed time
;-

; IF n_elements(region) EQ 0 THEN region = 1
IF region EQ !null THEN region = 1

COMMON vblock, wholeimage

; Going through and doing a little commenting, think I forgot how this works:

makestrips, thresh, c4xstrips, c4ystrips, region=region, time=time

start = SYSTIME(1,/seconds)

ministrip_side_buffer = byte(!param.ministrip_length)/2 
; have to byte it since we read the ministrip_length as a float

; Contains coordinates of chord enpoints
rowchord_endpoints = FLTARR(2,N_ELEMENTS(c4xstrips))
colchord_endpoints = FLTARR(2,N_ELEMENTS(c4ystrips))
;   Seeing where the array starts to be greater than the thresh
FOR i = 0,N_ELEMENTS(c4ystrips)-1 DO BEGIN
    col_where = WHERE(c4ystrips[i].ARRAY GT thresh)
    ; beginning of chord
    colchord_endpoints[0,i] = col_where[0]
    ; end of chord
    colchord_endpoints[1,i] = col_where[-1]
ENDFOR

FOR i = 0,N_ELEMENTS(c4xstrips) -1 DO BEGIN
    row_where = WHERE(c4xstrips[i].ARRAY GT thresh)
    rowchord_endpoints[0,i] = row_where[0]
    rowchord_endpoints[1,i] = row_where[-1]
ENDFOR

; Preallocating the array, replicating it by the number of strips there are
xstrips = REPLICATE({ROWINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:BYTARR(!param.ministrip_length), $
        ENDPOINTS:BYTARR(!param.ministrip_length), $
        xoffset:c4xstrips.xoffset},N_ELEMENTS(c4xstrips))
ystrips = REPLICATE({COLINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:BYTARR(!param.ministrip_length), $
        ENDPOINTS:BYTARR(!param.ministrip_length), $
        yoffset:c4ystrips.yoffset},N_ELEMENTS(c4ystrips))

;Filling out structure with cut-down strip information
FOR i = 0,N_ELEMENTS(c4xstrips) - 1 DO BEGIN
    xstrips[i].ROWINDEX     = c4xstrips[i].ROWINDEX
    ; If there is no strip that cuts through the sun, set things to 0
    IF rowchord_endpoints[0,i] EQ -1 THEN BEGIN
        xstrips[i].STARTPOINTS  = BYTARR(!param.ministrip_length) 
        xstrips[i].BEGINDEX     = 0
    ENDIF ELSE BEGIN
        ; STARTPOINTS is the cut down strip with length = ministrip_length and contains
        ; the indices from rowchord_endpoints[0,i] +/- ministrip_side_buffer
        xstrips[i].STARTPOINTS  = $
        ; IF chord is too long, it tries to crop from outside of image file
            (c4xstrips[i].ARRAY)[rowchord_endpoints[0,i]-ministrip_side_buffer:$
            rowchord_endpoints[0,i]+ministrip_side_buffer]   
        ; BEGINDEX is the index of the strip where it begins. 
        ; e.g., the array is 5 long, starts from index 9 and is centered around index 11
        xstrips[i].BEGINDEX     = FIX(rowchord_endpoints[0,i] - ministrip_side_buffer)
    ENDELSE

    IF rowchord_endpoints[1,i] EQ -1 THEN BEGIN
        xstrips[i].ENDPOINTS    = BYTARR(!param.ministrip_length)
        xstrips[i].ENDINDEX    = 0
    ENDIF ELSE BEGIN
        xstrips[i].ENDPOINTS  = $
            (c4xstrips[i].ARRAY)[rowchord_endpoints[1,i]-ministrip_side_buffer:$
            rowchord_endpoints[1,i]+ministrip_side_buffer]   
        xstrips[i].ENDINDEX     = FIX(rowchord_endpoints[1,i] - ministrip_side_buffer)
    ENDELSE
ENDFOR


FOR k = 0,N_ELEMENTS(c4ystrips) - 1 DO BEGIN
    ystrips[k].COLINDEX     = c4ystrips[k].COLINDEX
    IF colchord_endpoints[0,k] EQ -1 THEN BEGIN
        ystrips[k].STARTPOINTS  = BYTARR(!param.ministrip_length) 
        ystrips[k].BEGINDEX     = 0
    ENDIF ELSE BEGIN 
        ystrips[k].STARTPOINTS  = (c4ystrips[k].ARRAY)[colchord_endpoints[0,k]- $
            ministrip_side_buffer:colchord_endpoints[0,k]+ministrip_side_buffer]
        ystrips[k].BEGINDEX     = FIX(colchord_endpoints[0,k] - ministrip_side_buffer)
    ENDELSE

    IF colchord_endpoints[1,k] EQ -1 THEN BEGIN
        ystrips[k].ENDPOINTS    = BYTARR(!param.ministrip_length) 
        ystrips[k].ENDINDEX     = 0        
    ENDIF ELSE BEGIN
        ystrips[k].ENDPOINTS    = (c4ystrips[k].ARRAY)[colchord_endpoints[1,k]- $
        ministrip_side_buffer:colchord_endpoints[1,k]+ministrip_side_buffer]
        ystrips[k].ENDINDEX     = FIX(colchord_endpoints[1,k] - ministrip_side_buffer) 
    ENDELSE
ENDFOR


finish = SYSTIME(1,/seconds)

IF KEYWORD_SET(time) THEN  print,'Elapsed Time for makelimbstrips: ', $
    STRCOMPRESS(finish-start,/rem),' seconds'
RETURN
END