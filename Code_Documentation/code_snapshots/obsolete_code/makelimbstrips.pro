PRO makelimbstrips, thresh, xstrips, ystrips, file, ministrip_length, scan_width, sigmavalue, sundiam, $
    nstrips=nstrips, region=region, time=time
;+
;   :Description:
;       Makes limb strips from full-length strips
;
;   :Params:
;       file: in, required, type=string, default='triplesun.bmp'
;           File to be read in
;       ministrip_length: in, required, type=byte, default=13
;           How long the total array of the cut-down strip will be
;       scan_width: in, required, type=integer, default=5
;           Indicates how far apart to scan
;       sigmavalue: in, required, type=integer, default=2
;           Sets the threshold to be::
;   
;           max(image) - sigmavalue*stddev(image)
;
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;       thresh : out, required, type=float
;           Threshold used to select pixels
;
;   :Keywords:
;       nstrips : in, optional, type=byte, default=5
;           How many strips to select, centered around the row/col diameter
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       time : in, optional
;           Prints the elapsed time
;
;   :TODO:
;
;   Exactly how much data should be stored in a structure? Since we're interested in saving space,
;   doesn't make sense to repeat any data in the structures.
;-

IF n_elements(file)             EQ 0    THEN file               = 'triplesun.bmp'
IF n_elements(ministrip_length) EQ 0    THEN ministrip_length   = 9
IF n_elements(nstrips)          EQ 0    THEN nstrips            = 5 
IF n_elements(scan_width)       EQ 0    THEN scan_width         = 5 
IF n_elements(sigmavalue)       EQ 0    THEN sigmavale          = 2
IF n_elements(sundiam)          EQ 0    THEN sundiam            = 70
IF n_elements(region)           EQ 0    THEN region             = 1

makestrips, thresh, c4xstrips, c4ystrips, file, scan_width, sigmavalue, sundiam, nstrips=nstrips, $
    region=region, time=time

start = systime(1,/seconds)

ministrip_side_buffer = ministrip_length/2 
rowchord_endpoints = fltarr(2,n_elements(c4xstrips))
colchord_endpoints = fltarr(2,n_elements(c4ystrips))
;   Seeing where the array starts to be greater than the thresh
FOR i = 0,n_elements(c4ystrips)-1 DO BEGIN
    col_where = where(c4ystrips[i].ARRAY GT thresh)
    ; beginning of chord
    colchord_endpoints[0,i] = col_where[0]
    ; end of chord
    colchord_endpoints[1,i] = col_where[-1]
ENDFOR

FOR i = 0,n_elements(c4xstrips) -1 DO BEGIN
    row_where = where(c4xstrips[i].ARRAY GT thresh)
    rowchord_endpoints[0,i] = row_where[0]
    rowchord_endpoints[1,i] = row_where[-1]
ENDFOR

; Preallocating the array, replicating it by the number of strips there are
xstrips = REPLICATE({ROWINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:bytarr(ministrip_length), $
        ENDPOINTS:bytarr(ministrip_length)},n_elements(c4xstrips))
ystrips = REPLICATE({COLINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:bytarr(ministrip_length), $
        ENDPOINTS:bytarr(ministrip_length)},n_elements(c4ystrips))

;Filling out structure with cut-down strip information
FOR i = 0,n_elements(c4xstrips) - 1 DO BEGIN
    xstrips[i].ROWINDEX     = c4xstrips[i].ROWINDEX
    ; If there is no strip that cuts through the sun, set things to 0
    IF rowchord_endpoints[0,i] EQ -1 THEN BEGIN
        xstrips[i].STARTPOINTS  = fltarr(ministrip_length) 
        xstrips[i].BEGINDEX     = 0
    ENDIF ELSE BEGIN
        ; STARTPOINTS is the cut down strip with length = ministrip_length and contains
        ; the indices from rowchord_endpoints[0,i] +/- ministrip_side_buffer
        xstrips[i].STARTPOINTS  = $
            (c4xstrips[i].ARRAY)[rowchord_endpoints[0,i]-ministrip_side_buffer:$
            rowchord_endpoints[0,i]+ministrip_side_buffer]   
        ; BEGINDEX is the index of the strip where it begins. 
        ; e.g., the array is 5 long, starts from index 9 and is centered around index 11
        xstrips[i].BEGINDEX     = fix(rowchord_endpoints[0,i] - ministrip_side_buffer)
    ENDELSE
    IF rowchord_endpoints[1,i] EQ -1 THEN BEGIN
        xstrips[i].ENDPOINTS    = fltarr(ministrip_length)
        xstrips[i].ENDINDEX    = 0
    ENDIF ELSE BEGIN
        xstrips[i].ENDPOINTS  = $
            (c4xstrips[i].ARRAY)[rowchord_endpoints[1,i]-ministrip_side_buffer:$
            rowchord_endpoints[1,i]+ministrip_side_buffer]   
        xstrips[i].ENDINDEX     = fix(rowchord_endpoints[1,i] - ministrip_side_buffer)
    ENDELSE
ENDFOR

FOR k = 0,n_elements(c4ystrips) - 1 DO BEGIN
    ystrips[k].COLINDEX     = c4ystrips[k].COLINDEX
    IF colchord_endpoints[0,k] EQ -1 THEN BEGIN
        ystrips[k].STARTPOINTS  = fltarr(ministrip_length) 
        ystrips[k].BEGINDEX     = 0
    ENDIF ELSE BEGIN 
        ystrips[k].STARTPOINTS  = (c4ystrips[k].ARRAY)[colchord_endpoints[0,k]- $
            ministrip_side_buffer:colchord_endpoints[0,k]+ministrip_side_buffer]
        ystrips[k].BEGINDEX     = fix(colchord_endpoints[0,k] - ministrip_side_buffer)
    ENDELSE
    IF colchord_endpoints[1,k] EQ -1 THEN BEGIN
        ystrips[k].ENDPOINTS    = fltarr(ministrip_length) 
        ystrips[k].ENDINDEX     = 0        
    ENDIF ELSE BEGIN
        ystrips[k].ENDPOINTS    = (c4ystrips[k].ARRAY)[colchord_endpoints[1,k]- $
        ministrip_side_buffer:colchord_endpoints[1,k]+ministrip_side_buffer]
        ystrips[k].ENDINDEX     = fix(colchord_endpoints[1,k] - ministrip_side_buffer) 
    ENDELSE
ENDFOR

finish = systime(1,/seconds)

IF keyword_set(time) THEN  print,'Elapsed Time for makelimbstrips: ',strcompress(finish-start,/rem),' seconds'
RETURN
END

