PRO makestrips, thresh, xstrips, ystrips, file, scan_width, sigmavalue, sundiam, nstrips=nstrips, $
    region=region, time=time
;+
;   :Description:
;		Only saves 5 strips centered around the solar diameter to reduce the amount of limb-
;       	darkened pixels and to make the polynomial-fitted limbs more-or-less look similar. 
;
;   :Params:
;	file: in, required, type=string, default='triplesun.bmp'
;	    File to be read in
;	scan_width: in, required, type=integer, default=5
;	    Indicates how far apart to scan
;	sigmavalue: in, required, type=integer, default=2
;	    Sets the threshold to be::
;	
;	    max(image) - sigmavalue*stddev(image)
;
;	sundiam : in, required, type=byte, default=70
;	    Approximate diameter of sun in pixels. (Based on bmp image)
;	thresh : out, required, type=float
;	    Threshold used to select pixels
;	xstrips : out, required, type=structure
;	    Structure containing row strips
;	ystrips : out, required, type=structure
;	    Structure containing column strips
;
;   :Keywords:
;	nstrips : in, optional, type=byte, default=5
;	    How many strips to select, centered around the row/col diameter
;	region : in, required, type=integer, default=1
;	    Which sun out of the three to find the center of. Defaults to the brightest sun
;	time : in, optional
;	    Prints elapsed time
;-

IF n_elements(file)			EQ 0    THEN file	    = 'triplesun.bmp'
IF n_elements(nstrips)	    EQ 0    THEN nstrips    = 5
IF n_elements(region)	    EQ 0    THEN region	    = 1
IF n_elements(scan_width)   EQ 0    THEN scan_width = 5
IF n_elements(sigmavalue)   EQ 0    THEN sigmavalue = 2
IF n_elements(sundiam)	    EQ 0    THEN sundiam    = 70

struct = tribox(file, scan_width, sigmavalue, sundiam, region=region, time=time)
;trimask, file, xpos, ypos, scan_width, sigmavalue, sundiam, thresh, region=region, time=time

start = systime(1,/seconds)
cropped_image = struct.image
;xpos-=struct.xoffset
;ypos-=struct.yoffset

thresh = max(cropped_image) - stddev(cropped_image)*sigmavalue 

s = size(cropped_image,/dimensions)
length = s[0]
height = s[1]

xpos = length/2 ;+ struct.xoffset
ypos = height/2 ;+ struct.yoffset

rowchord_endpoints = fltarr(2,nstrips)
colchord_endpoints = fltarr(2,nstrips)

xstrips = REPLICATE({ROWINDEX:0,ARRAY:bytarr(length)},nstrips)
ystrips = REPLICATE({COLINDEX:0,ARRAY:bytarr(height)},nstrips)

FOR i = 0,nstrips - 1 DO BEGIN
    xstrips[i].ROWINDEX = i
    xstrips[i].ARRAY = cropped_image[*, round(xpos)+(i-nstrips/2)*scan_width]
ENDFOR

FOR k = 0,nstrips - 1 DO BEGIN
    ystrips[k].COLINDEX = k
    ystrips[k].ARRAY = cropped_image[round(ypos)+(k-nstrips/2)*scan_width,*]
ENDFOR

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print,'Elapsed Time for makestrips: ',strcompress(finish-start,/rem),' seconds'
RETURN
END


