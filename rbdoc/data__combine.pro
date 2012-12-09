FUNCTION data::combine, file, scan_width, sigmavalue, sundiam, time=time
;+
;   :Description:
;       Finds the centers of a triple-sun image and loads all relevant information
;       including offsets and angles into a new structure.
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
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;
;   :Keywords:
;       time: in, optional
;           Outputs how much time the program takes
;
;       
;-

COMPILE_OPT idl2 
on_error,2

IF n_elements(scan_width) 	EQ 0 THEN scan_width = 5
IF n_elements(sigmavalue) 	EQ 0 THEN sigmavalue = 2
IF n_elements(sundiam)		EQ 0 THEN sundiam = 70

start=systime(1,/s)

c1temp = self->center(file, scan_width, sigmavalue, sundiam, region=1, time=time)
center1 = c1temp->get()
c2temp = self->center(file, scan_width, sigmavalue, sundiam, region=2, time=time)
center2 = c1temp->get()
c3temp = self->center(file, scan_width, sigmavalue, sundiam, region=3, time=time)
center3 = c1temp->get()

theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
    (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

self->set,{center1:center1, center2:center2, center3:center3, $
    theta:theta, offset:offset}

finish = systime(1,/s)
IF keyword_set(time) THEN print, 'Elapsed time for combine(): '+strcompress(finish-start)+$
    ' seconds'
RETURN,self
END