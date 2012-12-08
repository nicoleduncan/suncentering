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

FUNCTION data::combine, scan_width, sigmavalue, time=time
;+
;   :Description:
;       Finds the centers of a triple-sun image and loads all relevant information
;       including offsets and angles into a new structure.
;
;   :Params:
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sigmavalue : in, required, type = integer, default = 2
;          Sets the threshold to be::
;
;           max(image) - sigmavalue*stddev(image)
;
;   :Keywords:
;       file: in, optional, type = string, default = 'triplesun.bmp'
;           What file to find 4 centers for
;       time: in, optional
;           Outputs how much time the program takes
;
;       
;-

IF n_elements(scan_width) EQ 0 THEN scan_width = 5
IF n_elements(sigmavalue) EQ 0 THEN sigmavalue = 2

start=systime(1,/s)

c1temp = self->center(scan_width,sigmavalue,region=1,time=time)
center1 = c1temp->get()
c2temp = self->center(scan_width,sigmavalue,region=2,time=time)
center2 = c1temp->get()
c3temp = self->center(scan_width,sigmavalue,region=3,time=time)
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

;********************************************************************************

FUNCTION data::read, file

IF n_elements(file) EQ 0 THEN file='triplesun.bmp'
; check=findfile(file,count=count)     ;-- check if file exists
; IF count NE 1 THEN RETURN,0            ;-- bail if not there
; Useful, but slow.
tmpimage = read_bmp(file)
s = size(tmpimage,/dimensions)
image = reform(tmpimage[0,*,*])
RETURN, image
END

;********************************************************************************

FUNCTION data::crop, scan_width, sigmavalue, sundiam, time=time, region=region
;+
;   :Description: 
;       Loads a triple-sun image and crops out selected regions one-by-one.
;
;   :Params:
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
;       file: in, optional, type = string, default = 'triplesun.bmp'
;           What file to find 4 centers for
;       time: in, optional
;           Outputs how much time the program takes
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;
; 
;-

IF n_elements(sundiam)    EQ 0 THEN sundiam = 70
IF n_elements(scan_width) EQ 0 THEN scan_width = 5
IF n_elements(sigmavalue) EQ 0 THEN sigmavalue = 2
IF n_elements(region)     EQ 0 THEN region = 1

start  = systime(1,/s)

inputarr=self->read(file)

thresh = max(inputarr) - sigmavalue*stddev(inputarr)
temparr = inputarr * (inputarr GT thresh)

limits = self->minicrop(scan_width,sundiam,temparr,thresh,time=time)

CASE region OF

1:  cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
2:  BEGIN
    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0
    
    ;****************************************************************************************
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(scan_width,sundiam,temparr,thresh,time=time)

    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END

3:  BEGIN
    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0

    ;****************************************************************************************
    ; Step 2: Black out the first dimsum
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(scan_width,sundiam,temparr,thresh,time=time)

    inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width] = 0

    ;****************************************************************************************

    ; Step 3: Crop what's left
    temparr = inputarr * (inputarr LT thresh)

    limits = self->minicrop(scan_width,sundiam,temparr,thresh,time=time)

    cropped=inputarr[limits.colscan*scan_width:limits.colendscan*scan_width,$
        limits.rowscan*scan_width:limits.rowendscan*scan_width]
    END
ENDCASE

self->set,{image:cropped,xoffset:limits.colscan*scan_width,$
    yoffset:limits.rowscan*scan_width}
finish = systime(1,/s)
IF keyword_set(time) THEN print,'Elapsed time for boundaries(): '+strcompress(finish-start,/remove)+' seconds'
RETURN,self
END

;********************************************************************************

FUNCTION data::minicrop, scan_width, sundiam, temparr, thresh, time=time
;+
;   :Description:
;       Figures out where the boundaries are depending solely on the threshold.
;
;   :Params:
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;       temparr : in, required, type=byte
;           2D array to check boundary limits of
;   :Keywords:
;       time : in, optional
;           Print the elapsed time
;-

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

finish = systime(1,/s)
IF keyword_set(time) THEN print,' Elapsed time for minicrop(): '+strcompress(finish-start,/remove)+' seconds'
RETURN,{minicropped,rowscan:rowscan-2, colscan:colscan-2, rowendscan:rowendscan+2, colendscan:colendscan+2}
END

;********************************************************************************

FUNCTION data::center, scan_width, sigmavalue, region=region, time=time
;+
;   :Description:
;       Had to make a new version of comp3 because the old one called scanbox() by default
;
;   :Params:
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sigmavalue : in, required, type = integer, default = 2
;          Sets the threshold to be::
;
;           max(image) - sigmavalue*stddev(image)
;
;   :Keywords:
;       region: in, required, type=integer, default=1
;           Which sun out of the three to find the center of. Defaults to the brightest sun
;       time : in, optional
;           Print the elapsed time
;-

IF n_elements(sigmavalue)   EQ 0 THEN sigmavalue = 2
IF n_elements(region)       EQ 0 THEN region = 1
IF n_elements(scan_width)   EQ 0 THEN scan_width = 5
IF n_elements(sundiam)      EQ 0 THEN sundiam = 70

start = systime(1,/seconds)

temp = self->crop(scan_width,sigmavalue,sundiam,region=region,time=time)
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

;********************************************************************************

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
temp = a->combine(scan_width,sigmavalue,time=time)
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


