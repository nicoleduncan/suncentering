FUNCTION data::crop, file, scan_width, sigmavalue, sundiam, time=time, region=region
;+
;   :Description: 
;       Loads a triple-sun image and crops out selected regions one-by-one.
;
;   :Params:
;       file: in, required, type = string, default = 'triplesun.bmp'
;           What file to find 4 centers for
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sigmavalue : in, required, type = integer, default = 2
;          Sets the threshold to be::
;
;           max(image) - sigmavalue*stddev(image)
;
;       sundiam: in, required, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;
;   :Keywords:
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;
; 
;-
COMPILE_OPT idl2 
on_error,2

IF n_elements(scan_width) EQ 0 THEN scan_width = 5
IF n_elements(sigmavalue) EQ 0 THEN sigmavalue = 2
IF n_elements(sundiam)    EQ 0 THEN sundiam = 70
IF n_elements(region)     EQ 0 THEN region = 1

start  = systime(1,/s)

inputarr=self->read(file)

thresh = max(inputarr) - sigmavalue*stddev(inputarr)
temparr = inputarr * (inputarr GT thresh)

limits = self->minicrop(scan_width,sundiam,temparr,thresh,time=time)

CASE region OF

1:  cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
2:  BEGIN
    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0
    
    ;****************************************************************************************
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(scan_width,sundiam,temparr,thresh,time=time)

    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END

3:  BEGIN
    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0

    ;****************************************************************************************
    ; Step 2: Black out the first dimsum
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(scan_width,sundiam,temparr,thresh,time=time)

    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 3: Crop what's left
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(scan_width,sundiam,temparr,thresh,time=time)

    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END
ENDCASE

self->set,{image:cropped,xoffset:limits.colscan*scan_width,$
    yoffset:limits.rowscan*scan_width}
finish = systime(1,/s)
IF keyword_set(time) THEN print,'Elapsed time for boundaries(): '+strcompress(finish-start,/remove)+' seconds'
RETURN,self
END
