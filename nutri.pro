
; docformat = 'rst'
;
;+
; NAME: 
;   nutri
;
; PURPOSE:
;   Finds the center of 3 suns in a single image. Currently limited to a .bmp test image. 
;
; :Author:
;   JEREN SUZUKI::
;
;       Space Sciences Laboratory
;       7 Gauss Way
;       Berkeley, CA 94720 USA
;       E-mail: jsuzuki@ssl.berkeley.edu
;-

; PRO nutri,file, scan_width, sigmavalue, time=time

;+
;   :Description:
;       Object-oriented version of tricenter.pro. Easier to use? Not really, but
;		that's probably because it was my first time writing a successful OOP 
;		program.
;
;   :Params:
;       scan_width: in, required, type = integer, default = 5
;           How apart the scans are for minicrop(). Overrides defaults in crop().
;       file: in, required, type = string, default = 'triplesun.bmp'
;           What file to find 4 centers for
;       sigmavalue: in, required, type = integer, default = 2
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

IF n_elements(scan_width)   EQ 0 THEN    scan_width = 5
IF n_elements(file)         EQ 0 THEN    file = 'triplesun.bmp'
IF n_elements(sigmavalue)   EQ 0 THEN    sigmavalue = 2

start=systime(1,/s)

a=obj_new('data')
temp = a->combine(scan_width,sigmavalue,sundiam,time=time)
struct = temp->get()
tmpimage = read_bmp(file) 
s = size(tmpimage,/dimensions)
n_col = s[1]
n_row = s[2]
image = reform(tmpimage[0,*,*])
image2 = image
image3 = image

image[struct.center1.xpos,*]=20
image[*,struct.center1.ypos]=20
image2[struct.center2.xpos,*]=20
image2[*,struct.center2.ypos]=20
image3[struct.center3.xpos,*]=20
image3[*,struct.center3.ypos]=20

; window,0
; cgimage,image,/k
; window,2
; cgimage,image2,/k
; window,3
; cgimage,image3,/k

finish = systime(1,/s)
IF keyword_set(time) THEN print, 'Elapsed time for nutri : '+strcompress(finish-start)+$
    ' seconds'

END