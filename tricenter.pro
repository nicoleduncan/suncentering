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

PRO tricenter,file=file,ministrip_length=ministrip_length,scan_width=scan_width,time=time,order=order, $
    plot=plot,sigmavalue=sigmavalue,savstep=savstep,saveonly=saveonly,storestruct=storestruct

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
;-

IF ~keyword_set(ministrip_length)   THEN    ministrip_length = 9
IF ~keyword_set(scan_width)         THEN    scan_width = 10
IF ~keyword_set(file)               THEN    file = 'triplesun.bmp'
IF ~keyword_set(sigmavalue)         THEN    sigmavalue = 1
IF ~keyword_set(savstep)            THEN    savstep = 4
IF ~keyword_set(order)              THEN    order = 3

a=tribox(file=file)

END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION tribox,file=file,time=time,scan_width=scan_width,savstep=savstep,$
    saveonly=saveonly,sigmavalue=sigmavalue,storestruct=storestruct
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
;       cropped = scanbox(file='triplesun.bmp',/time)
;
;-
IF ~keyword_set(file)       THEN file       = 'triplesun.bmp'
IF ~keyword_set(scanwidth)  THEN scan_width = 5
IF ~keyword_set(sigmavalue) THEN sigmavalue = 1

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

; An arbitrary vaue of thresh while I figure out if we should stay with a numeric threshold
thresh =max(image)-2*stddev(image)

thesun = cropit(region=1,inputarr=image,sundiam=sundiam,scan_width=scan_width,thresh=thresh)
dimcrop1 = cropit(region=2,inputarr=image,sundiam=sundiam,scan_width=scan_width,thresh=thresh)
dimcrop2 = cropit(region=3,inputarr=image,sundiam=sundiam,scan_width=scan_width,thresh=thresh)

stop

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

FUNCTION cropit, location, region=region, inputarr=inputarr, scan_width=scan_width, $
    sundiam=sundiam, thresh=thresh

rowscan = 0

CASE region OF

1: BEGIN
    temparr = inputarr * (inputarr gt thresh)

    WHILE total(where(temparr[*,rowscan*scan_width] GT thresh)) EQ -1 DO BEGIN
        rowscan++
    ENDWHILE
    ; Doing it this way so that if in the case of 3 suns, if 1 sun is more left than the sun which is the 
    ; most bottom, the cropping will correctly choose the right sun.
    colscan = fix(((where(temparr[*,rowscan*scan_width] GT thresh))[0] - sundiam/2 + $
            n_elements(where(temparr[*,rowscan*scan_width] GT thresh))/2 )/scan_width)


    rowendscan = rowscan + sundiam/scan_width ; Jumping to other side of sun
    colendscan = colscan + sundiam/scan_width

    rowscan     -= 2
    colscan     -= 2

    ; Since the column scanning is rough, have to give the ends a little room.
    rowendscan  += 2
    colendscan  += 2

    ; Since we care about x and y offsets, sticking this into a structure
    cropped=inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width]
    location = {TRICROP,image:cropped,xoffset:colscan*scan_width,yoffset:rowscan*scan_width}
    RETURN,location
    END

2: BEGIN
    temparr = inputarr * (inputarr lt thresh)

    WHILE total(where(temparr[*,rowscan*scan_width] GT thresh/2)) EQ -1 DO BEGIN
        rowscan++
    ENDWHILE
    ;   Have to exclude thesun somehow?
    colscan = fix(((where(temparr[*,rowscan*scan_width] GT thresh/2))[0] - sundiam/2 + $
            n_elements(where(temparr[*,rowscan*scan_width] GT thresh/2))/2 )/scan_width)

    rowendscan = rowscan + sundiam/scan_width
    colendscan = colscan + sundiam/scan_width

    rowscan     -= 2
    colscan     -= 2
    rowendscan  += 2
    colendscan  += 2

    cropped=inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width]
    location = {TRICROP,image:cropped,xoffset:colscan*scan_width,yoffset:rowscan*scan_width}
    RETURN,location
    END

3: BEGIN
    ; Step 1: Black out thesun
    temparr = inputarr * (inputarr gt thresh)

    WHILE total(where(temparr[*,rowscan*scan_width] GT thresh/2)) EQ -1 DO BEGIN
        rowscan++
    ENDWHILE
    ; Doing it this way so that if in the case of 3 suns, if 1 sun is more left than the sun which is the 
    ; most bottom, the cropping will correctly choose the right sun.
    colscan = ((where(temparr[*,rowscan*scan_width] GT thresh/2))[0] - sundiam/2 + $
            n_elements(where(temparr[*,rowscan*scan_width] GT thresh/2))/2 )/scan_width


    rowendscan = rowscan + sundiam/scan_width ; Jumping to other side of sun
    colendscan = colscan + sundiam/scan_width

    rowscan     -= 2
    colscan     -= 2

    ;Since the column scanning is rough, have to give the ends a little room.
    rowendscan  += 2
    colendscan  += 2

    inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width] = 0

    ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ; Step 2: Black out the first dimsum
    rowscan=0
    temparr = inputarr * (inputarr lt thresh)

    WHILE total(where(temparr[*,rowscan*scan_width] GT thresh/2)) EQ -1 DO BEGIN
        rowscan++
    ENDWHILE

    colscan = ((where(temparr[*,rowscan*scan_width] GT thresh/2))[0] - sundiam/2 + $
            n_elements(where(temparr[*,rowscan*scan_width] GT thresh/2))/2 )/scan_width


    rowendscan = rowscan + sundiam/scan_width
    colendscan = colscan + sundiam/scan_width

    rowscan     -= 2
    colscan     -= 2
    rowendscan  += 2
    colendscan  += 2

    inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width] = 0

    ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ; Step 3: Crop what's left
    rowscan=0
    temparr = inputarr * (inputarr lt thresh)

    WHILE total(where(temparr[*,rowscan*scan_width] GT thresh/2)) EQ -1 DO BEGIN
        rowscan++
    ENDWHILE

    colscan = fix(((where(temparr[*,rowscan*scan_width] GT thresh/2))[0] - sundiam/2 + $
            n_elements(where(temparr[*,rowscan*scan_width] GT thresh/2))/2 )/scan_width)


    rowendscan = rowscan + sundiam/scan_width
    colendscan = colscan + sundiam/scan_width

    rowscan     -= 2
    colscan     -= 2
    rowendscan  += 2
    colendscan  += 2

    cropped=inputarr[colscan*scan_width:colendscan*scan_width,rowscan*scan_width:rowendscan*scan_width]
    location = {TRICROP,image:cropped,xoffset:colscan*scan_width,yoffset:rowscan*scan_width}

    RETURN,location
    END

ENDCASE
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

