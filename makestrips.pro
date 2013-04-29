PRO makestrips, thresh, xstrips, ystrips, region=region, time=time
;+
;   :Description:
;       Only saves 5 strips centered around the solar diameter to reduce the amount of limb-
;           darkened pixels and to make the polynomial-fitted limbs more-or-less look similar. 
;
;   :Params:
;   thresh : out, required, type=float
;       Threshold used to select pixels
;   xstrips : out, required, type=structure
;       Structure containing row strips
;   ystrips : out, required, type=structure
;       Structure containing column strips
;
;   :Keywords:
;   region : in, required, type=integer, default=1
;       Which sun out of the three to find the center of. Defaults to the brightest sun
;   time : in, optional
;       Prints elapsed time
;-

; IF n_elements(region) EQ 0 THEN region = 1
IF region eq !null then region = 1

COMMON vblock, wholeimage

struct = whichcropmethod(region)
ducks = quickmask(struct.image)
thresh = struct.thresh

start = SYSTIME(1,/seconds)

animage = struct.image
s = SIZE(animage,/dimensions)
length = s[0]
height = s[1]

rowchord_endpoints = FLTARR(2,!param.nstrips)
colchord_endpoints = FLTARR(2,!param.nstrips)

xstrips = REPLICATE({ROWINDEX:0, ARRAY:BYTARR(length), xoffset:struct.xoffset}, !param.nstrips)
ystrips = REPLICATE({COLINDEX:0, ARRAY:BYTARR(height), yoffset:struct.yoffset}, !param.nstrips)

FOR i = 0,!param.nstrips - 1 DO BEGIN
    xstrips[i].ROWINDEX = i
    xstrips[i].ARRAY = animage[*, ROUND(ducks.xpos)+(i-!param.nstrips/2)* !param.scan_width]
    ystrips[i].COLINDEX = i
    ystrips[i].ARRAY = animage[ROUND(ducks.ypos)+(i-!param.nstrips/2)* !param.scan_width,*]
ENDFOR

finish = SYSTIME(1,/seconds)
IF KEYWORD_SET(time) THEN  print,'Elapsed Time for makestrips: ', $
    STRCOMPRESS(finish-start,/rem),' seconds'
RETURN
END