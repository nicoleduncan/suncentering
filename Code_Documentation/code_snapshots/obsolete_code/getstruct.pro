PRO getstruct, file, struct, scan_width, sigmavalue, sundiam, time=time
;+
;   :Description:
;       Finds the centers of a triple-sun image and loads all relevant information
;       including offsets and angles into a new structure.
;
;   :Params:
;       file: in, required, type = string, default = 'triplesun.bmp'
;           What file to find 3 centers for
;       struct : out, required, type=structure
;           Structure containing the centers and cropped images of all 3 suns
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
;       time: in, optional
;           Outputs how much time the program takes
;-
COMPILE_OPT idl2 
on_error,2

start = systime(1,/s)

center1 = {center1,xpos:0d,ypos:0d,thresh:0d}
center2 = {center2,xpos:0d,ypos:0d,thresh:0d}
center3 = {center3,xpos:0d,ypos:0d,thresh:0d}

nstrips=5

trimask, file, xpos, ypos, scan_width, sigmavalue, sundiam, thresh, region=1, time=time
;limbfit, thresh, xpos, ypos, file, ministrip_length, order, scan_width, sigmavalue, sundiam, nstrips=nstrips, plot=plot, $
;	region=1, time=time
center1.xpos = xpos
center1.ypos = ypos
center1.thresh = thresh
;trimask, file, xpos, ypos, scan_width, sigmavalue, sundiam, thresh, region=2, time=time
limbfit, thresh, xpos, ypos, file, ministrip_length, order, scan_width, sigmavalue, sundiam, nstrips=nstrips, plot=plot, $
	region=2, time=time
center2.xpos = xpos
center2.ypos = ypos
center2.thresh = thresh
;trimask, file, xpos, ypos, scan_width, sigmavalue, sundiam, thresh, region=3, time=time
limbfit, thresh, xpos, ypos, file, ministrip_length, order, scan_width, sigmavalue, sundiam, nstrips=nstrips, plot=plot, $
	region=3, time=time
center3.xpos = xpos
center3.ypos = ypos
center3.thresh = thresh

 print, 'Center 1 X position: ', center1.xpos
 print, 'Center 1 Y position: ', center1.ypos
 print, 'Center 2 X position: ', center2.xpos
 print, 'Center 2 Y position: ', center2.ypos
 print, 'Center 3 X position: ', center3.xpos
 print, 'Center 3 Y position: ', center3.ypos

theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
    (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

struct = {KAHUNA, center1:center1, center2:center2, center3:center3, $
    theta:theta, offset:offset}
finish = systime(1,/s)
IF keyword_set(time) THEN print, 'getstruct took: '+strcompress(finish-start)+$
    ' seconds'
RETURN
END
