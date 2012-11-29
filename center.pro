; docformat = 'rst'

;+
; At the end of the day this should find the center of the sun using a variety of (working) methods
;
; Compiled the doc with idldoc,root='/Users/jerensuzuki/Documents/suncentering', $
; output='rbdoc',format_style='rst',/user,/quiet,markup_style='rst'
;
; :Author:
;   JEREN SUZUKI::
;
;       Space Sciences Laboratory
;       7 Gauss Way
;       Berkeley, CA 94720 USA
;       E-mail: jsuzuki@ssl.berkeley.edu
;-

PRO center,file=file,ministrip_length=ministrip_length,scan_width=scan_width,time=time,order=order, $
    plot=plot,sigmavalue=sigmavalue,savstep=savstep,saveonly=saveonly,storestruct=storestruct
;+
; :Description:
;       Finds the centroid using a variety of compression levels
;       Removed the 'vX' suffix because it got confusing keeping track of which numbers went with what
;       
;       Important!:
;       IDL uses the origin to be the top left corner for 2d arrays and a column is indexed from 0
;       starting from the top. You have been WARNED.
; :Keywords:
;   file: in, optional, type=string, default='gauss1pix.tiff'
;       File to be read in      
;   ministrip_length: in, optional, type=byte, default=9
;       How long the cut-down strip is
;   scan_width: in, optional, type=byte, default = 10   
;       How far apart the scans are
;   time: in, optional
;       Prints the elapsed time
;   order: in, optional, type=integer, default=3
;       What order polynomial to use for POLY_FIT()
;   plot: in, optional
;       If set, makes some nice plots.
;   sigmavalue: in, optional, type=integer, default=1
;       Sets the threshold to be:: 
;           
;           max(image) - sigmavalue*stddev(image)
;   savstep: in, required, type=integer, default=4
;       The number of steps to include in the fits file. 
;       savstep = 1: cropped image
;       savstep = 2: long strips
;       savstep = 3: limb strips
;       savstep = 4: center of mask
;   saveonly: in, optional
;       Determines whether or not savstep saves steps leading up to savstep or just savstep
;   storestruct: in, optional
;       Toggles whether or not we want to save data at all
;
;
; :TODO: 
;
;   More Keywords/parameters    
;
;   How strict are we going to be for the outputs of each compression file?
;
;   Deal with fiducials hitting the limb (Does it even matter?)
;
;   Deal with scan_width; scan_width=5 seems to produce some pretty good results.
;
;   I fixed the values being stuck, it was because I added to the index where the image value > thresh,
;   I should've subtracted instead. Note: Don't do that again.
;
;   Threshold value doesn't have a safety in terms of cosmic rays or single pixels with absurdly high
;   values. Got to have some sort of catch here just in case. 
;-

IF ~keyword_set(ministrip_length)   THEN    ministrip_length = 9
IF ~keyword_set(scan_width)         THEN    scan_width = 10
IF ~keyword_set(file)               THEN    file = 'Sun_Images_000000.bmp'
IF ~keyword_set(sigmavalue)         THEN    sigmavalue = 1
IF ~keyword_set(savstep)            THEN    savstep = 4
IF ~keyword_set(order)              THEN    order = 3

comp2,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file
print,'Summing each point and average the X and Y positions:'
print,'X Center is ',xpos
print,'Y Center is ',ypos
print,'Threshold is ', thresh
print,''
xcs = xpos
ycs = ypos

comp3,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file
print,'Looking at just the mask:'
print,'X Center is ',xpos
print,'Y Center is ',ypos
print,'Threshold is ', thresh
print,''
xcsn = xpos
ycsn = ypos

comp6,xpos,ypos,time=time,order=order,scan_width=scan_width,file=file,plot=plot,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length,savstep=savstep,saveonly=saveonly,storestruct=storestruct
print,'Limb-fitting:'
print,'X Center is ',xpos
print,'Y Center is ',ypos
print,''
print,'Difference in X Position from comp2',xcs-xpos
print,'Difference in Y Position from comp2',ycs-ypos
print,'Difference in X Position from comp3',xcsn-xpos
print,'Difference in Y Position from comp3',ycsn-ypos

; restore,'bigstruct.sav'

; So I think I've figured our the source of the problem, it's maybe something to do with FXADDPAR
; not being up to date enough, but I haven't figured out a place to download a more recent 
; version online. 

; Also, I can't use writefits to write structures because that's something I can't do with 
; writefits, unfortunately. There is only the capability to save arrays? Lame.

; So my options are to either find a more recent version of FXADDPAR (if that's even the problem)
; or find another way to save fits files. 

stop
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION scanbox,file=file,time=time,meanthresh=meanthresh,scan_width=scan_width,$
    savstep=savstep,saveonly=saveonly,sigmavalue=sigmavalue,storestruct=storestruct
;+
; :Description: 
;       Boxit was too computation-intensive, this is more a rough boxing program. 
;       Uses knowledge of approx size of sun to only scan until it hits the sun then 
;       moves the crop borders based on the given parameters.
;
; :Keywords:
;   file : in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time : in, optional
;       Print the elapsed time
;   sigmavalue: in, optional, type=integer, default=1
;       Sets the threshold to be::
;
;       max(image) - sigmavalue*stddev(image)

;   scan_width : in, optional, type=integer, default=5
;       How far apart the scanning should be. Fine tuned compared to scanbox
;   savstep: in, required, type=integer, default=4
;       The number of steps to include in the fits file. 
;       savstep = 1: cropped image
;       savstep = 2: long strips
;       savstep = 3: limb strips
;       savstep = 4: center of mask
;   saveonly: in, optional
;       Determines whether or not savstep saves steps leading up to savstep or just savstep
;   storestruct: in, optional
;       Toggles whether or not we want to save data at all
;
; :Examples:
;       cropped = scanbox(file='$PWD/sep11_postit/104533_20120912_124300_353097_0.bin',/time)
;
;-
IF ~keyword_set(file)       THEN file       = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(scanwidth)  THEN scan_width = 5
IF ~keyword_set(sigmavalue) THEN sigmavalue = 1

start = systime(1,/seconds)

; Setting some parameters
rowscan = 0
colscan = 0

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
    ; These values are fixed for the model Prosilica GC 1290, we don't have to worry about making this 
    ; variable
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


; IF keyword_set(sigmavalue) THEN thresh = max(image) - sigmavalue*stddev(image) ELSE $
;     thresh = max(image) - stddev(image)

; Can't use above since it makes the thresh=190 which is way too high. Mean = 7.22, which is
; too low? max(image)/2. = 107 which is reasonable, imo.
thresh = max(image)/2.
; thresh = max(image) - 2*sigmavalue*stddev(image)

;Cmon Jeren you didn't dominate Radio Lab for nothing

; A counter to see how many rows/cols it takes to hit the sun
WHILE n_elements(where(image[*,rowscan*scan_width] GT thresh)) EQ 1 DO BEGIN
    rowscan++
ENDWHILE
WHILE n_elements(where(image[colscan*scan_width,*] GT thresh)) EQ 1 DO BEGIN
    colscan++
ENDWHILE
    
rowendscan = rowscan + sundiam/scan_width ; Jumping to other side of sun
colendscan = colscan + sundiam/scan_width

rowscan     -= 2
colscan     -= 2

cropped_image=image[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width]

;for novelty purposes
finish = systime(1,/seconds)
IF keyword_set(time) THEN  print, 'Elapsed Time for scanbox(): ' + $
    strcompress(finish-start,/remove)+ ' seconds'
; save,cropped_image,filename='cropped_image.sav',/compress
IF n_elements(saveonly) EQ 0 AND keyword_set(storestruct) THEN BEGIN
    bigstruct = {crop:cropped_image}
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
RETURN,cropped_image
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp2,xpos,ypos,thresh,file=file,time=time,plot=plot,sigmavalue=sigmavalue
;+
; :Description:
;           Finds the centroid by summing all values over a certain threshold and averaging them.
;
; :Keywords:
;   file: in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time: in, optional
;       Prints the elapsed time
;   plot: in, optional
;       Makes a nice plot
;   sigmavalue: in, optional, type=integer, default=2
;       Sets the threshold to be::
;
;       max(image) - sigmavalue*stddev(image)
;
;       Having a sigmavalue=2 makes the pixels chosen from the sun more circular than sigmavalue=1
;
;           
; :Params:
;   xpos: out, required,type=float
;       X center
;   ypos: out, required,type=float
;       Y center
;   thresh: out, required,type=float
;       Threshold used to select pixels
;
; :Examples:
;           comp2,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file
;
;-
IF ~keyword_set(file)       THEN file       = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(sigmavalue) THEN sigmavalue = 2

cropped_image = scanbox(file=file,time=time)

start = systime(1,/seconds)

; BEWARE:
; comp2 takes into account all pixels above acertain threshold, including fiducial marks. comp6 
; doesn't deal with what's going on in the non-limb parts of the sun. But in the case of a 
; fiducial mark hitting the limb, what do we do?


; Allocating an array where values are above the threshold
; Funny thing, it's .0003 seconds faster to only run 1 instance of size()
s = size(cropped_image,/dimensions)
n_col = s[0]
n_row = s[1]

; LE TEMP
; **************
; thresh = max(cropped_image)/2
thresh = max(cropped_image)-sigmavalue*stddev(cropped_image)
; **************

;THIS IS BEAUTIFUL
sunonly = cropped_image*(cropped_image gt thresh)
;God damit this isn't fair how fast this is.
sum     = TOTAL(sunonly)
xpos    = TOTAL( TOTAL(sunonly, 2) * Indgen(n_col) ) / sum
ypos    = TOTAL( TOTAL(sunonly, 1) * Indgen(n_row) ) / sum 

IF keyword_set(plot) THEN BEGIN
    cropped_image[*,ypos] = 0
    cropped_image[xpos,*] = 0
    cgimage,cropped_image,/keep_asp
    print,'X center is ',xpos
    print,'Y center is ',ypos
ENDIF

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print, 'Elapsed Time for comp2: ',strcompress(finish-start,/remove),' seconds'
RETURN
END 

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp3,xpos,ypos,thresh,file=file,time=time,plot=plot,sigmavalue=sigmavalue
;+
; :Description:
;           Looks only at the positions of pixels above a threshold and doen't care
;       About their actual values, like in comp2. More "accurate" to find the
;       center than comp2 since this gives all solar pixels equal weighting.
;
; :Keywords:
;   file: in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time: in, optional
;       Prints the elapsed time
;   plot: in, optional
;       Makes a nice plot
;   sigmavalue: in, optional, type=integer, default=2
;       Sets the threshold to be::
;
;       max(image) - sigmavalue*stddev(image)
;
;       Having a sigmavalue=2 makes the pixels chosen from the sun more circular than sigmavalue=1
;
;           
; :Params:
;   xpos: out, required,type=float
;       X center
;   ypos: out, required,type=float
;       Y center
;   thresh: out, required,type=float
;       Threshold used to select pixels
;
; :Examples:
;           comp3,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file
;
;-

IF ~keyword_set(file)   THEN file   = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(sigmavalue)  THEN sigmavalue = 2

cropped_image = scanbox(file=file,time=time)

start = systime(1,/seconds)

thresh = max(cropped_image)-sigmavalue*stddev(cropped_image)

s = size(cropped_image,/dimensions)
n_col = s[0]
n_row = s[1]

suncheck = cropped_image gt thresh

xpos = TOTAL( TOTAL(suncheck, 2) * Indgen(n_col) ) / total(suncheck)
ypos = TOTAL( TOTAL(suncheck, 1) * Indgen(n_row) ) / total(suncheck)

finish = systime(1,/s)
IF keyword_set(time) THEN  print, 'Elapsed Time for comp3: ',strcompress(finish-start,/remove),' seconds'
RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp4, xstrips,ystrips,thresh,file=file,time=time,scan_width=scan_width,sigmavalue=sigmavalue,$
    savstep=savstep,saveonly=saveonly,nstrips=nstrips,storestruct=storestruct
;+
; :Description:
;           Only saves 5 strips centered around the solar diameter to reduce the amount of limb-
;           darkened pixels and to make the polynomial-fitted limbs more-or-less look similar. 
;           Called into comp5.
; :Keywords:
;   file: in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time: in, optional
;       Prints the elapsed time
;   scan_width: in, optional, type=integer, default=10
;       Indicates how far apart to scan
;   sigmavalue: in, optional, type=integer, default=1
;       Sets the threshold to be::
;
;        max(image) - sigmavalue*stddev(image)
;
;   savstep: in, required, type=integer, default=4
;       The number of steps to include in the fits file. 
;       savstep = 1: cropped image
;       savstep = 2: long strips
;       savstep = 3: limb strips
;       savstep = 4: center of mask
;   saveonly: in, optional
;       Determines whether or not savstep saves steps leading up to savstep or just savstep
;   nstrips: in, optional, type=byte, default=5
;       How many strips to select, centered around the row/col diameter
;   storestruct: in, optional
;       Toggles whether or not we want to save data at all
;
; :Params:
;   xstrips: out, required, type=structure 
;       Structure containing row strips
;   ystrips: out, required, type=structure 
;       Structure containing column strips
;   thresh: out, required, type=float
;       Threshold used to select pixels
;
; :Examples:
;           comp4,xstrips,ystrips,thresh,/time,scan_width=10
;
;-

IF ~keyword_set(file)       THEN file       = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(scan_width) THEN scan_width = 10
IF ~keyword_set(nstrips)    THEN nstrips = 5

cropped_image = scanbox(file=file,time=time,savstep=savstep,saveonly=saveonly)
comp3,xpos,ypos,time=time,sigmavalue=sigmavalue,file=file

start = systime(1,/seconds)

IF ~keyword_set(sigmavalue)  THEN sigmavalue = 1
thresh = max(cropped_image) - stddev(cropped_image)*sigmavalue 

s = size(cropped_image,/dimensions)
length = s[0]
height = s[1]

rowchord_endpoints = fltarr(2,nstrips)
colchord_endpoints = fltarr(2,nstrips)

xstrips = REPLICATE({ROWINDEX:0,SCAN_WIDTH:scan_width,ARRAY:bytarr(length)},nstrips)
ystrips = REPLICATE({COLINDEX:0,SCAN_WIDTH:scan_width,ARRAY:bytarr(height)},nstrips)

FOR i = 0,nstrips - 1 DO BEGIN
    xstrips[i].ROWINDEX = i
    xstrips[i].ARRAY = cropped_image[*, round(xpos)+(i-nstrips/2)*scan_width]
ENDFOR

FOR k = 0,nstrips - 1 DO BEGIN
    ystrips[k].COLINDEX = k
    ystrips[k].ARRAY = cropped_image[round(ypos)+(k-nstrips/2)*scan_width,*]
ENDFOR

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print,'Elapsed Time for comp4: ',strcompress(finish-start,/rem),' seconds'
; save,xstrips,ystrips,thresh,filename='comp4strips.sav',/compress
IF savstep GE 2 AND n_elements(saveonly) EQ 0 AND keyword_set(storestruct) THEN BEGIN
    restore,'bigstruct.sav'
    longstrips = {longxstrips:xstrips,longystrips:ystrips,thresh:thresh}
    bigstruct = create_struct(bigstruct,longstrips)
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
IF savstep GE 2 AND n_elements(saveonly) NE 0 AND keyword_set(storestruct) THEN BEGIN
    bigstruct = {longxstrips:xstrips,longystrips:ystrips,thresh:thresh}
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF

RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp5, xstrips,ystrips,thresh,file=file,time=time,ministrip_length=ministrip_length,$
        scan_width=scan_width,sigmavalue=sigmavalue,savstep=savstep,saveonly=saveonly,$
        storestruct=storestruct
;+
; :Description:
;           Calls comp4 to acquire data strips then saves limb strips.
;
; :Keywords:
;   file: in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time: in, optional
;       Prints the elapsed time
;   scan_width: in, optional, type=integer, default=10
;       Indicates how far apart to scan
;   ministrip_length: in, optional, type=byte, default=13
;       How long the total array of the cut-down strip will be
;   sigmavalue: in, optional, type=integer, default=1
;       Sets the threshold to be::
;   
;    max(image) - sigmavalue*stddev(image)
;   
;   savstep: in, required, type=integer, default=4
;       The number of steps to include in the fits file. 
;       savstep = 1: cropped image
;       savstep = 2: long strips
;       savstep = 3: limb strips
;       savstep = 4: center of mask
;   saveonly: in, optional
;       Determines whether or not savstep saves steps leading up to savstep or just savstep
;   storestruct: in, optional
;       Toggles whether or not we want to save data at all
;
; :Params:
;   xstrips: out, required, type=structure
;       Contains the row sctructures of strips
;   ystrips: out, required, type=structure
;       Contains the col sctructures of strips
;   thresh: out, required, type=float
;       Threshold value used 
;
; :Examples:
;           comp5,xstrips,ystrips,thresh
;
; :TODO:
;
;   Exactly how much data should be stored in a structure? Since we're interested in saving space,
;   doesn't make sense to repeat any data in the structures.
;-

IF ~keyword_set(file)               THEN file               = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(scan_width)         THEN scan_width         = 10 
IF ~keyword_set(ministrip_length)   THEN ministrip_length   = 9 

comp4,c4xstrips,c4ystrips,thresh,file=file,time=time,sigmavalue=sigmavalue,scan_width=scan_width,$
    savstep=savstep,saveonly=saveonly

start = systime(1,/seconds)

ministrip_side_buffer = ministrip_length/2 
rowchord_endpoints = fltarr(2,n_elements(c4xstrips))
colchord_endpoints = fltarr(2,n_elements(c4ystrips))

;   Seeing where the array starts to be greater than the thresh
FOR i = 0,n_elements(c4ystrips)-1 DO BEGIN
    col_where = where(c4ystrips[i].ARRAY GT thresh)
    ; beginning of chord
    colchord_endpoints[0,i] = col_where[0]
    ; end of chord
    colchord_endpoints[1,i] = col_where[-1]
ENDFOR

FOR i = 0,n_elements(c4xstrips) -1 DO BEGIN
    row_where = where(c4xstrips[i].ARRAY GT thresh)
    rowchord_endpoints[0,i] = row_where[0]
    rowchord_endpoints[1,i] = row_where[-1]
ENDFOR

; Preallocating the array, replicating it by the number of strips there are
xstrips = REPLICATE({ROWINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:bytarr(ministrip_length), $
        ENDPOINTS:bytarr(ministrip_length)},n_elements(c4xstrips))
ystrips = REPLICATE({COLINDEX:0, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:bytarr(ministrip_length), $
        ENDPOINTS:bytarr(ministrip_length)},n_elements(c4ystrips))


;Filling out structure with cut-down strip information
FOR i = 0,n_elements(c4xstrips) - 1 DO BEGIN
    xstrips[i].ROWINDEX     = c4xstrips[i].ROWINDEX
    ; If there is no strip that cuts through the sun, set things to 0
    IF rowchord_endpoints[0,i] EQ -1 THEN BEGIN
        xstrips[i].STARTPOINTS  = fltarr(ministrip_length) 
        xstrips[i].BEGINDEX     = 0
    ENDIF ELSE BEGIN
        ; STARTPOINTS is the cut down strip with length = ministrip_length and contains
        ; the indices from rowchord_endpoints[0,i] +/- ministrip_side_buffer
        xstrips[i].STARTPOINTS  = $
            (c4xstrips[i].ARRAY)[rowchord_endpoints[0,i]-ministrip_side_buffer: $
            rowchord_endpoints[0,i]+ministrip_side_buffer]   
        ; BEGINDEX is the index of the strip where it begins. 
        ; e.g., the array is 5 long, starts from index 9 and is centered around index 11
        xstrips[i].BEGINDEX     = fix(rowchord_endpoints[0,i] - ministrip_side_buffer)
    ENDELSE
    IF rowchord_endpoints[1,i] EQ -1 THEN BEGIN
        xstrips[i].ENDPOINTS    = fltarr(ministrip_length)
        xstrips[i].ENDINDEX    = 0
    ENDIF ELSE BEGIN
        xstrips[i].ENDPOINTS  = $
            (c4xstrips[i].ARRAY)[rowchord_endpoints[1,i]-ministrip_side_buffer: $
            rowchord_endpoints[1,i]+ministrip_side_buffer]   
        xstrips[i].ENDINDEX     = fix(rowchord_endpoints[1,i] - ministrip_side_buffer)
    ENDELSE
ENDFOR

FOR k = 0,n_elements(c4ystrips) - 1 DO BEGIN
    ystrips[k].COLINDEX     = c4ystrips[k].COLINDEX
    IF colchord_endpoints[0,k] EQ -1 THEN BEGIN
        ystrips[k].STARTPOINTS  = fltarr(ministrip_length) 
        ystrips[k].BEGINDEX     = 0
    ENDIF ELSE BEGIN 
        ystrips[k].STARTPOINTS  = (c4ystrips[k].ARRAY)[colchord_endpoints[0,k]- $
            ministrip_side_buffer:colchord_endpoints[0,k]+ministrip_side_buffer]
        ystrips[k].BEGINDEX     = fix(colchord_endpoints[0,k] - ministrip_side_buffer)
    ENDELSE
    IF colchord_endpoints[1,k] EQ -1 THEN BEGIN
        ystrips[k].ENDPOINTS    = fltarr(ministrip_length) 
        ystrips[k].ENDINDEX     = 0        
    ENDIF ELSE BEGIN
        ystrips[k].ENDPOINTS    = (c4ystrips[k].ARRAY)[colchord_endpoints[1,k]- $
        ministrip_side_buffer:colchord_endpoints[1,k]+ministrip_side_buffer]
        ystrips[k].ENDINDEX     = fix(colchord_endpoints[1,k] - ministrip_side_buffer) 
    ENDELSE
ENDFOR
finish = systime(1,/seconds)

IF keyword_set(time) THEN  print,'Elapsed Time for comp5: ',strcompress(finish-start,/rem),' seconds'
IF savstep GE 3 AND n_elements(saveonly) EQ 0 AND keyword_set(storestruct) THEN BEGIN
    shortstrips = {shortxstrips:xstrips,shortystrips:ystrips,scan_width:scan_width,sigmavalue:sigmavalue,$
        ministrip_length:ministrip_length}
    restore,'bigstruct.sav'
    bigstruct = create_struct(bigstruct,shortstrips)
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
IF savstep GE 3 AND n_elements(saveonly) NE 0 AND keyword_set(storestruct) THEN BEGIN
    bigstruct = {shortxstrips:xstrips,shortystrips:ystrips,scan_width:scan_width,sigmavalue:sigmavalue,$
        ministrip_length:ministrip_length}
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp6,xpos,ypos,file=file,order=order,time=time,scan_width=scan_width,$
    ministrip_length=ministrip_length,plot=plot,sigmavalue=sigmavalue,savstep=savstep,$
    saveonly=saveonly, storestruct=storestruct
;+
; :Description:
;       Uses the data from comp5 and fits an n-th order polynomial to the limb to find where
;       it crosses the threshold. Imports the strips from comp5.
;
; :Keywords:
;   file: in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time: in, optional
;       Prints the elapsed time
;   scan_width: in, optional, type=integer, default=10
;       Ondicates how far apart to scan
;   order: in, optional, type=integer, default=3
;       What order polynomial to use for POLY_FIT()
;   ministrip_length: in, optional, default=9
;       How long the trimmed down strip will be
;   plot: in, optional
;       If set, makes some nice plots.
;   sigmavalue: in, optional, type=integer, default=1
;       Sets the threshold to be::
;
;    max(image) - sigmavalue*stddev(image)
;
;   savstep: in, required, type=integer, default=4
;       The number of steps to include in the fits file. 
;       savstep = 1: cropped image
;       savstep = 2: long strips
;       savstep = 3: limb strips
;       savstep = 4: center of mask
;   saveonly: in, optional
;       Determines whether or not savstep saves steps leading up to savstep or just savstep
;   storestruct: in, optional
;       Toggles whether or not we want to save data at all
;
; :Params:
;   xpos: out, required, type=float
;       X Center
;   ypos: out, required, type=float
;       Y Center
;
; :Examples:
;           comp6,xpos,ypos,order=3     
;
;-

; Setting default values
IF ~keyword_set(file)               THEN file = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(order)              THEN order = 3
IF ~keyword_set(ministrip_length)   THEN ministrip_length = 9

; Run the program to get our structures
comp5,xstrips,ystrips,thresh,file=file,time=time,ministrip_length=ministrip_length,$
    sigmavalue=sigmavalue,scan_width=scan_width,savstep=savstep,saveonly=saveonly

start = systime(1,/seconds)

ministrip_side_length = ministrip_length/2
xlen    = 0
xsum    = 0
xnum    = 0   
ylen    = 0
ysum    = 0
ynum    = 0
xarr    = findgen(n_elements(xstrips[4].STARTPOINTS))
yarr    = findgen(n_elements(ystrips[4].STARTPOINTS))
tx      = findgen(n_elements(xstrips[4].STARTPOINTS) * 1000)/100
ylenarr = findgen(n_elements(ystrips))
xlenarr = findgen(n_elements(xstrips))

;Deal with rows
FOR n=0,n_elements(xstrips)-1 DO BEGIN
    ; Using fz_roots instead of spline interpolating. Saving lines and making code more readable
    startresult     = reform(poly_fit(xarr,xstrips[n].STARTPOINTS,order))
    endresult       = reform(poly_fit(xarr,xstrips[n].ENDPOINTS,order))

    ; Solving for roots but want to include threshold value
    startresult[0]  -=thresh
    endresult[0]    -=thresh

    IF xstrips[n].BEGINDEX GT 0 THEN BEGIN
        ; Get roots (complex)
        begroots    = fz_roots(startresult)
        ; Take only roots with no imaginary components
        begusable   = (real_part(begroots))[where(imaginary(begroots) eq 0.)]
        ; Find smallest root (apparently I have to choose the smaller one)
        ; Or i can find the midpoints using the other two roots then take the average of the two,
        ; that way works too, but why would I do that?
        begusable   = (begusable[where(begusable gt 0)])[0]
        stripbeg    = xstrips[n].BEGINDEX + begusable
    ENDIF ELSE BEGIN
        begusable   = 0
        stripbeg    = 0
    ENDELSE

    IF xstrips[n].ENDINDEX GT 0 THEN BEGIN
        endroots    = fz_roots(endresult)
        endusable   = (real_part(endroots))[where(imaginary(endroots) eq 0.)]
        endusable   = (endusable[where(endusable gt 0)])[0]
        stripend    = xstrips[n].ENDINDEX + endusable
    ENDIF ELSE BEGIN
        endusable   = 0
        stripend    = 0
    ENDELSE

    ; Stick the midpoints in an array to take the mean of later
    xlenarr[n] = mean([[stripend],[stripbeg]])
ENDFOR    

FOR n=0,n_elements(ystrips)-1 DO BEGIN
    startresult     = reform(poly_fit(yarr,ystrips[n].STARTPOINTS,order))
    endresult       = reform(poly_fit(yarr,ystrips[n].ENDPOINTS,order))

    startresult[0]  -=thresh
    endresult[0]    -=thresh

    IF ystrips[n].BEGINDEX GT 0 THEN BEGIN
        begroots    = fz_roots(startresult)
        begusable   = (real_part(begroots))[where(imaginary(begroots) eq 0.)]
        begusable   = (begusable[where(begusable gt 0)])[0]
        stripbeg    = ystrips[n].BEGINDEX + begusable
    ENDIF ELSE BEGIN
        begusable   = 0
        stripbeg    = 0
    ENDELSE

    IF ystrips[n].ENDINDEX GT 0 THEN BEGIN
        endroots    = fz_roots(endresult)
        endusable   = (real_part(endroots))[where(imaginary(endroots) eq 0.)]
        endusable   = (endusable[where(endusable gt 0)])[0]
        stripend    = ystrips[n].ENDINDEX + endusable
        
    ENDIF ELSE BEGIN
        endusable   = 0
        stripend    = 0
    ENDELSE

    ylenarr[n] = mean([[stripend],[stripbeg]])
ENDFOR    

; Get the midpoint of the chords
xpos = mean(xlenarr[where(xlenarr ne 0)])
ypos = mean(ylenarr[where(ylenarr ne 0)])

IF keyword_set(plot) THEN BEGIN
    wn = 3
    startresult = poly_fit(xarr,xstrips[wn].STARTPOINTS,order)
    endresult = poly_fit(xarr,xstrips[wn].ENDPOINTS,order)

    CASE order OF
    1: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr,tx)
        END
    2: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2,tx)
        END
    3: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3,tx)
        END
    4: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4,tx)
        END
    5: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5,tx)
        END    
    6: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
                startresult[6]*xarr^6,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
                endresult[6]*xarr^6,tx)
        END
    7: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
                startresult[6]*xarr^6 + startresult[7]*xarr^7,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
                endresult[6]*xarr^6 + endresult[7]*xarr^7,tx)
        END
    ENDCASE

    ; A pretty plot for Nicole
    window,2
    ; set_plot,'ps'
    ; device,filename=file+'part1'+'.ps',/encapsulated
    plot,xarr+xstrips[wn].BEGINDEX,xstrips[wn].startpoints,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].BEGINDEX,xtmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/right,charsize=2
    ; device,/close
    ; set_plot,'x'
    window,0
    ; set_plot,'ps'
    ; device,filename=file+'part2'+'.ps',/encapsulated
    plot,xarr+xstrips[wn].ENDINDEX,xstrips[wn].ENDPOINTS,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].ENDINDEX,atmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/left,charsize=2
    ; device,/close
    ; set_plot,'x'

    wn = 5
    startresult = poly_fit(xarr,xstrips[wn].STARTPOINTS,order)
    endresult = poly_fit(xarr,xstrips[wn].ENDPOINTS,order)

    CASE order OF
    1: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr,tx)
        END
    2: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2,tx)
        END
    3: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3,tx)
        END
    4: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4,tx)
        END
    5: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5,tx)
        END    
    6: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
                startresult[6]*xarr^6,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
                endresult[6]*xarr^6,tx)
        END
    7: BEGIN
        xtmp = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
                startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
                startresult[6]*xarr^6 + startresult[7]*xarr^7,tx)
        atmp = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
                endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
                endresult[6]*xarr^6 + endresult[7]*xarr^7,tx)
        END
    ENDCASE

    ; A pretty plot for Nicole
    window,3
    ; set_plot,'ps'
    ; device,filename=file+'part1'+'.ps',/encapsulated
    plot,xarr+xstrips[wn].BEGINDEX,xstrips[wn].startpoints,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].BEGINDEX,xtmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/right,charsize=2
    ; device,/close
    ; set_plot,'x'
    window,1
    ; set_plot,'ps'
    ; device,filename=file+'part2'+'.ps',/encapsulated
    plot,xarr+xstrips[wn].ENDINDEX,xstrips[wn].ENDPOINTS,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2;,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].ENDINDEX,atmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/left,charsize=2
    ; device,/close
    ; set_plot,'x'


    
ENDIF
; Just an aside, but plotting adds ~ .1 seconds to bring it up to .22 seconds total

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print,'Elapsed Time for comp6: ',strcompress(finish-start,/rem),' seconds'
; save,xpos,ypos,thresh,sigmavalue,order,file, ministrip_length,scan_width,$
;     filename='comp6results.sav',/compress

; strformatcode = 'a'+strcompress(strlen(file),/rem)

; OPENW,1,'comp6results.dat' 
; PRINTF,1,xpos,ypos,thresh,sigmavalue,order,ministrip_length,scan_width,file, $
;     format='(F7.2,1X,F7.2,1X,F7.2,I,1X,I,1X,I,1X,I,1X,'+strformatcode+')'
; CLOSE,1

; strformatcode = 'a'+strcompress(strlen(file),/rem)
; OPENW,2,'comp6results.txt' 
; PRINTF,2,xpos,ypos,thresh,sigmavalue,order,ministrip_length,scan_width,file, $
;     format='(F7.2,1X,F7.2,1X,F7.2,I,1X,I,1X,I,1X,I,1X,'+strformatcode+')'
; CLOSE,2

IF savstep EQ 4 AND n_elements(saveonly) EQ 0 AND keyword_set(storestruct) THEN BEGIN
    restore,'bigstruct.sav'
    stuff = {xpos:xpos,ypos:ypos,order:order}
    bigstruct = create_struct(stuff,bigstruct)
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
IF savstep EQ 4 AND n_elements(saveonly) NE 0 AND keyword_set(storestruct) THEN BEGIN
    bigstruct = {xpos:xpos,ypos:ypos,order:order}
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF

RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
