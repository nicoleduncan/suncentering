FUNCTION tribox, file, scan_width, sigmavalue, sundiam, region=region, time=time
;+
;   :Description: 
;       The triple-sun variant of scanbox(), the goal of this is to provide cropped regions for
;       each centering method to compute the centers of. 
;
;   :Params:
;       file : in, required, type=string, default='triplesun.bmp'
;           File to be read in
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
;           Print the elapsed time
;
;   :Examples:
;       cropped = scanbox(file='triplesun.bmp',/time)
;
;-
COMPILE_OPT idl2 
on_error,2

IF n_elements(file)       EQ 0 THEN file       	= 'triplesun.bmp'
IF n_elements(scanwidth)  EQ 0 THEN scan_width 	= 5
IF n_elements(sigmavalue) EQ 0 THEN sigmavalue 	= 1
IF n_elements(region)     EQ 0 THEN region 	= 2

start = systime(1,/seconds)

IF STRPOS(file, 'tiff') NE -1  THEN BEGIN
    ; Read the tiff file
    tmpimage = read_tiff(file)
    ; Get height AND width
    s = size(tmpimage,/dimensions)
    n_col = s[1]
    n_row = s[2]
    ; Let's use vectors to reize the 3xn_colxn_row array
    image = reform(tmpimage[0,*,*])
    sundiam = max(TOTAL(image gt max(image)/2, 1))+10
ENDIF

IF STRPOS(file, 'bmp') NE -1  THEN BEGIN
    tmpimage = read_bmp(file)
    s = size(tmpimage,/dimensions)
    n_col = s[1]
    n_row = s[2]
    image = reform(tmpimage[0,*,*])
    sundiam = 70 ;at it's widest, sun is 61 pixels across
ENDIF

IF STRPOS(file, 'bin') NE -1  THEN BEGIN
    n_row       = 960
    n_col       = 1280
    ; These values are fixed for the model Prosilica GC 1290, we don't have 
    ; to worry about making this variable
    tmpimage    = bytarr(n_col,n_row)
    image       = bytarr(n_col,n_row)

    openr,lun,file,/get_lun
    readu,lun,image
    free_lun,lun

    FOR i=0,n_row-1 DO BEGIN
        tmpimage[*,i] = image[*,n_row-1-i]
    ENDFOR

    image = flipimage
ENDIF

cropped = cropit(image, location, scan_width, sigmavalue, sundiam, region=region, time=time)

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print, 'Elapsed Time for tribox(): ' + $
    strcompress(finish-start,/remove)+ ' seconds'
RETURN, cropped
END
