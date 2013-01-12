FUNCTION data::center, file, scan_width, sigmavalue, sundiam, region=region, time=time
;+
;   :Description:
;       Had to make a new version of comp3 because the old one called scanbox() by default
;
;   :Params:
;       file: in, required, type = string, default = 'triplesun.bmp'
;           What file to find 3 centers for
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sigmavalue : in, required, type = integer, default = 2
;          Sets the threshold to be::
;
;           max(image) - sigmavalue*stddev(image)
;
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;
;   :Keywords:
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       time : in, optional
;           Print the elapsed time
;-
COMPILE_OPT idl2 
on_error,2

IF n_elements(scan_width)   EQ 0 THEN scan_width = 5
IF n_elements(sigmavalue)   EQ 0 THEN sigmavalue = 2
IF n_elements(sundiam)      EQ 0 THEN sundiam = 70
IF n_elements(region)       EQ 0 THEN region = 1

start = systime(1,/seconds)

temp = self->crop(file, scan_width,sigmavalue,sundiam,region=region,time=time)
struct = temp->get()
cropped = struct.image
thresh = max(cropped)-sigmavalue*stddev(cropped)

s = size(cropped,/dimensions)
n_col = s[0]
n_row = s[1]

suncheck = cropped gt thresh

xpos = TOTAL( TOTAL(suncheck, 2) * Indgen(n_col) ) / total(suncheck) + struct.xoffset
ypos = TOTAL( TOTAL(suncheck, 1) * Indgen(n_row) ) / total(suncheck) + struct.yoffset

self->set,{xpos:xpos, ypos:ypos, thresh:thresh}

finish = systime(1,/s)
IF keyword_set(time) THEN  print, 'Elapsed Time for center(): ',strcompress(finish-start,/remove),$
    ' seconds'

RETURN,self
END