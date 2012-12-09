; docformat = 'rst'
;
;+
; NAME: 
;   TRICENTER
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

;****************************************************************************************

PRO minicrop, temparr, rowscan, colscan, rowendscan, colendscan, scan_width=scan_width,$
        sundiam=sundiam, thresh=thresh,time=time
;+
;   :Description: 
;       Small function to keep code small in cropit. Finds the row where the threshold is crossed
;       and then steps back in the x-direction to find the left side of the cropping box. Skips over
;       the sun by a predetermined value then crops out the sun. 
;   :Keywords:
;       thresh: in, optional, default = max(array) - sigmavalue*stddev(array)
;           Threshold used for masking
;       scan_width: in, optional, default = 5
;           Distance between scan lines when looking for first instance of threshold crossing
;       sundiam: in, optional, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;       time : in, optional
;           Print the elapsed time
;-
COMPILE_OPT idl2 
on_error,2

start = systime(1,/s)
rowscan=0

WHILE total(where(temparr[*,rowscan*scan_width] GT thresh/2)) EQ -1 DO BEGIN
    rowscan++
ENDWHILE
; Doing it this way so that if in the case of 3 suns, if 1 sun is more left than the sun which 
; is the most bottom, the cropping will correctly choose the right sun.
colscan = fix(((where(temparr[*,rowscan*scan_width] GT thresh/2))[0] - sundiam/2 + $
        n_elements(where(temparr[*,rowscan*scan_width] GT thresh/2))/2 )/scan_width)


rowendscan = rowscan + sundiam/scan_width ; Jumping to other side of sun
colendscan = colscan + sundiam/scan_width

;Since the column scanning is rough, have to give the ends a little room.
rowscan     -= 2
colscan     -= 2
rowendscan  += 2
colendscan  += 2

finish = systime(1,/s)
IF keyword_set(time) THEN print,' minicrop took '+strcompress(finish-start,/remove)+' seconds'

RETURN
END

;****************************************************************************************

FUNCTION cropit, location, region=region, inputarr=inputarr, scan_width=scan_width, $
    sundiam=sundiam, sigmavalue=sigmavalue, time=time
;+
; :Description: 
;   Loads a triple-sun image and crops out selected regions one-by-one.
;
; :Keywords:
;   time : in, optional
;       Print the elapsed time
;   scan_width : in, optional, type=integer, default=5
;       How far apart the scanning should be.
;   sigmavalue: in, optional, type=integer, default=1
;       Sets the threshold to be::
;
;           max(image) - sigmavalue*stddev(image)
;
; :Examples:
;       cropped = cropit(region=1,inputarr=image,sundiam=sundiam,scan_width=scan_width,$
;           thresh=thresh)
;
;-

start = systime(1,/s)

thresh = max(inputarr) - sigmavalue*stddev(inputarr)
temparr = inputarr * (inputarr gt thresh)

minicrop,temparr,rowscan,colscan,rowendscan,colendscan,thresh=thresh,scan_width=scan_width,$
    sundiam=sundiam,time=time

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

    minicrop,temparr,rowscan,colscan,rowendscan,colendscan,thresh=thresh,scan_width=scan_width,$
        sundiam=sundiam,time=time

    cropped=inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:$
        rowendscan*scan_width]
    location = {REGION2,image:cropped,xoffset:colscan*scan_width,yoffset:rowscan*scan_width}
    END

3: BEGIN
    inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 2: Black out the first dimsum
    temparr = inputarr * (inputarr lt thresh)

    minicrop,temparr,rowscan,colscan,rowendscan,colendscan,thresh=thresh,scan_width=scan_width,$
        sundiam=sundiam,time=time

    inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 3: Crop what's left
    temparr = inputarr * (inputarr lt thresh)

    minicrop,temparr,rowscan,colscan,rowendscan,colendscan,thresh=thresh,scan_width=scan_width,$
        sundiam=sundiam,time=time

    cropped=inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:$
        rowendscan*scan_width]
    location = {REGION3,image:cropped,xoffset:colscan*scan_width,yoffset:rowscan*scan_width}

    END

ENDCASE
finish = systime(1,/s)
IF keyword_set(time) THEN print,' cropit() took '+strcompress(finish-start,/remove)+' seconds'
RETURN,location
END

;****************************************************************************************

FUNCTION tribox,region=region, file=file,time=time,scan_width=scan_width,sigmavalue=sigmavalue
;+
; :Description: 
;   The triple-sun variant of scanbox(), the goal of this is to provide cropped regions for
;   each centering method to compute the centers of. 
;
; :Keywords:
;   file : in, optional, type=string, default='triplesun.bmp'
;       File to be read in
;   time : in, optional
;       Print the elapsed time
;   sigmavalue: in, optional, type=integer, default=2
;       Sets the threshold to be::
;
;           max(image) - sigmavalue*stddev(image)
;
;   scan_width : in, optional, type=integer, default=5
;       How far apart the scanning should be. Fine tuned compared to scanbox
;
; :Examples:
;       cropped = scanbox(file='triplesun.bmp',/time)
;
;-

IF ~keyword_set(file)       THEN file       = 'triplesun.bmp'
IF ~keyword_set(scanwidth)  THEN scan_width = 5
IF ~keyword_set(sigmavalue) THEN sigmavalue = 1
IF ~keyword_set(region)     THEN region = 2

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

cropped = cropit(region=region,inputarr=image,sundiam=sundiam,scan_width=scan_width,$
        sigmavalue=sigmavalue,time=time)

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print, 'Elapsed Time for tribox(): ' + $
    strcompress(finish-start,/remove)+ ' seconds'
RETURN, cropped
END

;****************************************************************************************

PRO trimask, xpos,ypos,thresh,file=file,time=time,sigmavalue=sigmavalue,region=region
;+
;   :Description:
;       Had to make a new version of comp3 because the old one called scanbox() by default
;
;   :Keywords:
;       file: in, optional, type='string', default='triplesun.bmp'
;           What file to load in
;       time : in, optional
;           Print the elapsed time
;       sigmavalue: in, optional, type=integer, default=2
;           Sets the threshold to be::
;   
;               max(image) - sigmavalue*stddev(image)
;   
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;-

IF ~keyword_set(file)           THEN file   = 'triplesun.bmp'
IF ~keyword_set(sigmavalue)     THEN sigmavalue = 2
IF ~keyword_set(region)         THEN region = 1

struct = tribox(file=file,time=time,region=region,sigmavalue=sigmavalue)
cropped = struct.image

start = systime(1,/seconds)

thresh = max(cropped)-sigmavalue*stddev(cropped)

s = size(cropped,/dimensions)
n_col = s[0]
n_row = s[1]

suncheck = cropped gt thresh

xpos = TOTAL( TOTAL(suncheck, 2) * Indgen(n_col) ) / total(suncheck) + struct.xoffset
ypos = TOTAL( TOTAL(suncheck, 1) * Indgen(n_row) ) / total(suncheck) + struct.yoffset

finish = systime(1,/s)
IF keyword_set(time) THEN  print, 'Elapsed Time for trimask: ',strcompress(finish-start,/remove),$
    ' seconds'
RETURN

END

;****************************************************************************************

PRO getstruct, struct, scan_width=scan_width, file=file,sigmavalue=sigmavalue, time=time
;+
;   :Description:
;       Finds the centers of a triple-sun image and loads all relevant information
;       including offsets and angles into a new structure.
;
;   :Keywords:
;       scan_width: in, optional, type = integer, default = 5
;           How apart the scans are for minicrop(). Overrides defaults in tribox().
;       file: in, optional, type = string, default = 'triplesun.bmp'
;           What file to find 4 centers for
;       time: in, optional
;           Outputs how much time the program takes
;       sigmavalue: in, optional, type = integer, default = 2
;          Sets the threshold to be::
;
;           max(image) - sigmavalue*stddev(image)
;
;       
;-

start=systime(1,/s)

center1 = {center1,xpos:0d,ypos:0d,thresh:0d}
center2 = {center2,xpos:0d,ypos:0d,thresh:0d}
center3 = {center3,xpos:0d,ypos:0d,thresh:0d}

trimask,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file,region=1
center1.xpos = xpos
center1.ypos = ypos
center1.thresh = thresh
trimask,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file,region=2
center2.xpos = xpos
center2.ypos = ypos
center2.thresh = thresh
trimask,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file,region=3
center3.xpos = xpos
center3.ypos = ypos
center3.thresh = thresh

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

;****************************************************************************************

; PRO tricenter,file=file,scan_width=scan_width,time=time,sigmavalue=sigmavalue

;+
; :Description:
;       Finds the pitch and yaw of a triple sun image. What those are I don't know, but will that
;       affect my code writing ability?? Probably not. 
;       
;       Important!:
;       IDL uses the origin to be the top left corner for 2d arrays and a column is indexed from 0
;       starting from the top. You have been WARNED.
; :Keywords:
;   file: in, optional, type=string, default='triplesun.bmp'
;       File to be read in      
;   scan_width: in, optional, type=byte, default = 10   
;       How far apart the scans are
;   time: in, optional
;       Outputs how much time the program takes
;   sigmavalue: in, optional, default=2
;       Sets the threshold to be::
;
;           max(image) - sigmavalue*stddev(image)
;
; :TODO: 
;       Make it use limb-fitting instead of simple masking
;   
;       More generally, does it make sense for each sub program to have defaults?
;
;-

COMPILE_OPT idl2

IF ~keyword_set(scan_width)         THEN    scan_width = 5
IF ~keyword_set(file)               THEN    file = 'triplesun.bmp'
IF ~keyword_set(sigmavalue)         THEN    sigmavalue = 2

start=systime(1,/s)

getstruct, struct, scan_width=scan_width, file=file, sigmavalue=sigmavalue

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
IF keyword_set(time) THEN print, 'tricenter took: '+strcompress(finish-start)+$
    ' seconds'
END

