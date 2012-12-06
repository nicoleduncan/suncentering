function data::init

;-- allocate memory to pointer when initializing object

 self.ptr=ptr_new(/allocate)  
 return,1

end

;----------------------------------------------------------------

pro data::set,value

;-- if data value exists, then insert into pointer location

 if n_elements(value) ne 0 then *(self.ptr)=value
 return 

end

;----------------------------------------------------------------

function data::get,value

;-- if data value is stored in object pointer, then copy it out

 if n_elements(*(self.ptr)) ne 0 then value=*(self.ptr)

 return,value

end

;------------------------------------------------------------------

pro data__define
 
 void={data,ptr:ptr_new()}
 return 

end

FUNCTION minicrop::init, scan_width=scan_width, sundiam=sundiam, thresh=thresh, temparr=temparr, time=time
COMPILE_OPT idl2 
on_error,2

if self->data::init() eq 0 then return, 0

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
*self.ptr = {minicropped,rowscan:rowscan-2, colscan:colscan-2, rowendscan:rowendscan+2, colendscan:colendscan+2}

finish = systime(1,/s)
IF keyword_set(time) THEN print,' minicrop took '+strcompress(finish-start,/remove)+' seconds'
RETURN,1

END

;****************************************************************************************

PRO minicrop__define
COMPILE_OPT idl2

def = {minicrop, INHERITS data}

END

;****************************************************************************************

FUNCTION boundaries::init, region=region, inputarr=inputarr, scan_width=scan_width, $
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
;       cropped = boundaries(region=1,inputarr=image,sundiam=sundiam,scan_width=scan_width,$
;           thresh=thresh)
;
;-

COMPILE_OPT idl2 
on_error,2

if self->data::init() eq 0 then return, 0

start = systime(1,/s)

thresh = max(inputarr) - sigmavalue*stddev(inputarr)
temparr = inputarr * (inputarr gt thresh)

chunk = obj_new('minicrop',thresh=thresh,scan_width=scan_width,sundiam=sundiam,$
    temparr=temparr,time=time)

CASE region OF

1: BEGIN
    limits = chunk->get()
    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END

2: BEGIN
    limits = chunk->get()

    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
    limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0
    
    ;****************************************************************************************

    temparr = inputarr * (inputarr lt thresh)

    chunk = obj_new('minicrop',thresh=thresh,scan_width=scan_width,sundiam=sundiam,$
        temparr=temparr,time=time)

    limits = chunk->get()

    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END

3: BEGIN
    limits = chunk->get()
    
    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
    limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 2: Black out the first dimsum
    temparr = inputarr * (inputarr lt thresh)

    chunk = obj_new('minicrop',thresh=thresh,scan_width=scan_width,sundiam=sundiam,$
        temparr=temparr,time=time)

    limits = chunk->get()

    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 3: Crop what's left
    temparr = inputarr * (inputarr lt thresh)

    chunk = obj_new('minicrop',thresh=thresh,scan_width=scan_width,sundiam=sundiam,$
        temparr=temparr,time=time)

    limits = chunk->get()

    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END
ENDCASE

*self.ptr = {POSTCROP,image:cropped,xoffset:limits.colscan*scan_width,$
    yoffset:limits.rowscan*scan_width}
finish = systime(1,/s)
IF keyword_set(time) THEN print,' boundaries() took '+strcompress(finish-start,/remove)+' seconds'

RETURN,1
END

;********************************************************************************

PRO boundaries::cleanup

    IF ptr_valid(self.ptr)    THEN ptr_free, self.ptr

END

;********************************************************************************

PRO boundaries__define
    compile_opt idl2
    def={boundaries,INHERITS data}
END

FUNCTION cropit::init, region=region, scan_width=scan_width, $
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

COMPILE_OPT idl2 
on_error,2

if self->data::init() eq 0 then return, 0

IF ~keyword_set(file)       THEN file       = 'triplesun.bmp'
IF ~keyword_set(scanwidth)  THEN scan_width = 5
IF ~keyword_set(sigmavalue) THEN sigmavalue = 2
IF ~keyword_set(region)     THEN region = 1

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

CASE region of
    1: temp=obj_new('boundaries',region=1,inputarr=image,sundiam=sundiam,scan_width=scan_width,$
            sigmavalue=sigmavalue,time=time)
    2: temp=obj_new('boundaries',region=2,inputarr=image,sundiam=sundiam,scan_width=scan_width,$
            sigmavalue=sigmavalue,time=time)
    3: temp=obj_new('boundaries',region=3,inputarr=image,sundiam=sundiam,scan_width=scan_width,$
            sigmavalue=sigmavalue,time=time)
ENDCASE
obj = temp->get() 
*self.ptr = obj
finish = systime(1,/seconds)
IF keyword_set(time) THEN  print, 'Elapsed Time for tribox(): ' + $
    strcompress(finish-start,/remove)+ ' seconds'

RETURN,1
END

;********************************************************************************

PRO cropit::cleanup

    IF ptr_valid(self.ptr)    THEN ptr_free, self.ptr

END

;********************************************************************************

PRO cropit__define
    compile_opt idl2



    def={cropit,INHERITS data}
END

FUNCTION getstruct::init,scan_width=scan_width, file=file,sigmavalue=sigmavalue, time=time
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

if self->data::init() eq 0 then return, 0


start=systime(1,/s)

c1block = obj_new('centerit',time=time,sigmavalue=sigmavalue,file=file,region=1)
center1 = c1block->get()
c2block = obj_new('centerit',time=time,sigmavalue=sigmavalue,file=file,region=2)
center2 = c2block->get()
c3block = obj_new('centerit',time=time,sigmavalue=sigmavalue,file=file,region=3)
center3 = c3block->get()

theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
    (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

*self.ptr = {KAHUNA, center1:center1, center2:center2, center3:center3, $
    theta:theta, offset:offset}
finish = systime(1,/s)
IF keyword_set(time) THEN print, 'getstruct took: '+strcompress(finish-start)+$
    ' seconds'
RETURN,1
END

;********************************************************************************

PRO getstruct::cleanup

    IF ptr_valid(self.ptr)    THEN ptr_free, self.ptr

END

;********************************************************************************

PRO getstruct__define
    compile_opt idl2
    def={getstruct,INHERITS data}
END

PRO nutricenter,scan_width=scan_width, file=file,sigmavalue=sigmavalue, time=time
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

block = obj_new('getstruct',scan_width=scan_width, file=file, sigmavalue=sigmavalue)
struct = block->get()
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

FUNCTION centerit::init,file=file,time=time,sigmavalue=sigmavalue,region=region
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

COMPILE_OPT idl2 
on_error,2

if self->data::init() eq 0 then return, 0

IF ~keyword_set(file)           THEN file   = 'triplesun.bmp'
IF ~keyword_set(sigmavalue)     THEN sigmavalue = 2
IF ~keyword_set(region)         THEN region = 1

start = systime(1,/seconds)

block = obj_new('cropit',region=region, scan_width=scan_width, $
    sundiam=sundiam, sigmavalue=sigmavalue, time=time)
struct = block->get()

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

*self.ptr = {CENTER, xpos:xpos, ypos:ypos, thresh:thresh}
RETURN,1
END

;********************************************************************************

PRO centerit::cleanup

    IF ptr_valid(self.ptr)    THEN ptr_free, self.ptr

END

;********************************************************************************

PRO centerit__define
    compile_opt idl2
    def={centerit,INHERITS data}
END

