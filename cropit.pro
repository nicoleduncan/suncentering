FUNCTION cropit, inputarr, location, scan_width, sigmavalue, sundiam, region=region, time=time
;+
;   :Description: 
;       Loads a triple-sun image and crops out selected regions one-by-one.
;
;   :Params:
;       inputarr : in, required, type=byte
;           Starting image to crop
;       location : out, required, type=structure
;           Structure containing the cropped image along with the X and Y distances from origin
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
;       time : in, optional
;         Print the elapsed time
;
; :Examples:
;       cropped = cropit(inputarr,scan_width,sigmavalue,sundiam,region=1)
;
;-
COMPILE_OPT idl2 
on_error,2

IF n_elements(scan_width) EQ 0 THEN scan_width = 5
IF n_elements(sigmavalue) EQ 0 THEN sigmavalue = 2
IF n_elements(sundiam)    EQ 0 THEN sundiam = 70
IF n_elements(region)     EQ 0 THEN region = 1

start = systime(1,/s)

thresh = max(inputarr) - sigmavalue*stddev(inputarr)
temparr = inputarr * (inputarr gt thresh)

minicrop, temparr, rowscan, colscan, rowendscan, colendscan, scan_width, sundiam, thresh,time=time

CASE region OF

1: BEGIN
    cropped=inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:$
        rowendscan*scan_width]

    location = {REGION1,image:cropped,xoffset:colscan*scan_width,yoffset:rowscan*scan_width}
    END

2: BEGIN
    inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width] = 0
    
    ;****************************************************************************************

    temparr = inputarr * (inputarr lt thresh)

    minicrop,temparr, rowscan, colscan, rowendscan, colendscan, scan_width,$
        sundiam, thresh,time=time

    cropped=inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:$
        rowendscan*scan_width]
    location = {REGION2,image:cropped,xoffset:colscan*scan_width,yoffset:rowscan*scan_width}
    END

3: BEGIN
    inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 2: Black out the first dimsum
    temparr = inputarr * (inputarr lt thresh)

    minicrop,temparr, rowscan, colscan, rowendscan, colendscan, scan_width,$
        sundiam, thresh,time=time

    inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 3: Crop what's left
    temparr = inputarr * (inputarr lt thresh)

    minicrop,temparr, rowscan, colscan, rowendscan, colendscan, scan_width,$
        sundiam, thresh,time=time

    cropped=inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:$
        rowendscan*scan_width]
    location = {REGION3,image:cropped,xoffset:colscan*scan_width,yoffset:rowscan*scan_width}

    END
ENDCASE
; window,region
; cgimage,cropped,/k
finish = systime(1,/s)
IF keyword_set(time) THEN print,' cropit() took '+strcompress(finish-start,/remove)+' seconds'
RETURN,location
END
