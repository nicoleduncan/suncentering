FUNCTION data::init
COMPILE_OPT idl2 
on_error,2
;-- allocate memory to pointer when initializing object
 self.ptr=ptr_new(/allocate)  
 RETURN,1
end 

;****************************************************************************************

PRO data::set,value
;-- if data value exists, then insert into pointer location
 if n_elements(value) ne 0 then *(self.ptr)=value
 RETURN 
end

;****************************************************************************************

FUNCTION data::get,value
;-- if data value is stored in object pointer, then copy it out
 if n_elements(*(self.ptr)) ne 0 then value=*(self.ptr)
 RETURN,value
end

;****************************************************************************************

PRO data__define
 void={data,ptr:ptr_new()}
 RETURN 
end

;****************************************************************************************

PRO data::combine,scan_width=scan_width, sigmavalue=sigmavalue, time=time

COMPILE_OPT idl2 
on_error,2

IF ~keyword_set(scan_width) THEN scan_width = 5
IF ~keyword_set(sigmavalue) THEN sigmavalue = 2
; IF self->data::init() EQ 0 THEN RETURN, 0

start=systime(1,/s)

center1 = self->center(sigmavalue=sigmavalue,scan_width=scan_width,region=1)
center2 = self->center(sigmavalue=sigmavalue,scan_width=scan_width,region=2)
center3 = self->center(sigmavalue=sigmavalue,scan_width=scan_width,region=3)

theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
    (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

finish = systime(1,/s)
IF keyword_set(time) THEN print, 'getstruct took: '+strcompress(finish-start)+$
    ' seconds'
; RETURN,{KAHUNA, center1:center1, center2:center2, center3:center3, $
;     theta:theta, offset:offset}
self->set,{KAHUNA, center1:center1, center2:center2, center3:center3, $
    theta:theta, offset:offset}

   RETURN 
END

;********************************************************************************

PRO data::read,file

IF n_elements(file) EQ 0 THEN file='triplesun.bmp'
check=findfile(file,count=count)     ;-- check if file exists
; IF count NE 1 THEN RETURN,0            ;-- bail if not there
tmpimage = read_bmp(file)
s = size(tmpimage,/dimensions)
image = reform(tmpimage[0,*,*])
; RETURN, image
self->set,image
RETURN
END

;********************************************************************************

PRO data::crop,region=region,sundiam=sundiam,scan_width=scan_width,sigmavalue=sigmavalue,time=time

IF ~keyword_set(sundiam)    THEN sundiam = 70
IF ~keyword_set(scan_width) THEN scan_width = 5
IF ~keyword_set(sigmavalue) THEN sigmavalue = 2
IF ~keyword_set(region)     THEN region = 1

; inputarr = self->read(file=file)
inputarr = self->get()

thresh = max(inputarr) - sigmavalue*stddev(inputarr)
temparr = inputarr * (inputarr GT thresh)

limits = self->minicrop(temparr=temparr,sundiam=sundiam,thresh=thresh,scan_width=scan_width)

CASE region OF

1:  cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
2:  BEGIN
    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0
    
    ;****************************************************************************************
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(temparr=temparr,sundiam=sundiam,thresh=thresh,scan_width=scan_width)

    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END

3:  BEGIN
    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0

    ;****************************************************************************************
    ; Step 2: Black out the first dimsum
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(temparr=temparr,sundiam=sundiam,thresh=thresh,scan_width=scan_width)

    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 3: Crop what's left
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(temparr=temparr,sundiam=sundiam,thresh=thresh,scan_width=scan_width)

    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END
ENDCASE
finish = systime(1,/s)
IF keyword_set(time) THEN print,' boundaries() took '+strcompress(finish-start,/remove)+' seconds'
; RETURN,{POSTCROP,image:cropped,xoffset:limits.colscan*scan_width,$
;     yoffset:limits.rowscan*scan_width}
self->set,{POSTCROP,image:cropped,xoffset:limits.colscan*scan_width,$
    yoffset:limits.rowscan*scan_width}
RETURN
END

;********************************************************************************

FUNCTION data::minicrop, scan_width=scan_width, sundiam=sundiam, thresh=thresh, temparr=temparr, time=time
COMPILE_OPT idl2 
on_error,2

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

finish = systime(1,/s)
IF keyword_set(time) THEN print,' minicrop took '+strcompress(finish-start,/remove)+' seconds'
RETURN,{minicropped,rowscan:rowscan-2, colscan:colscan-2, rowendscan:rowendscan+2, colendscan:colendscan+2}
END

;********************************************************************************

pro data::center, region=region, time=time, sigmavalue=sigmavalue, scan_width=scan_width
;+
;   :Description:
;       Had to make a new version of comp3 because the old one called scanbox() by default
;
;   :Keywords:
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

COMPILE_OPT idl2 
on_error,2

IF ~keyword_set(sigmavalue)     THEN sigmavalue = 2
IF ~keyword_set(region)         THEN region = 1
IF ~keyword_set(scan_width)     THEN scan_width = 5

start = systime(1,/seconds)

; struct = self->crop(scan_width=scan_width,region=region)
struct = self->get()
cropped = struct.image

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

; RETURN,{CENTER, xpos:xpos, ypos:ypos, thresh:thresh}
self->set,{CENTER, xpos:xpos, ypos:ypos, thresh:thresh}
RETURN
END

;********************************************************************************

PRO nutri,scan_width=scan_width, file=file,sigmavalue=sigmavalue, time=time
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

COMPILE_OPT idl2 
on_error,2

IF ~keyword_set(scan_width)         THEN    scan_width = 5
IF ~keyword_set(file)               THEN    file = 'triplesun.bmp'
IF ~keyword_set(sigmavalue)         THEN    sigmavalue = 2

start=systime(1,/s)

a=obj_new('data')
struct = a->combine(scan_width=scan_width, sigmavalue=sigmavalue,time=time)
; a->combine

struct = a->get()
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


