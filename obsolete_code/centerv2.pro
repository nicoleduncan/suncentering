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

FUNCTION scanbox,file=file,time=time,meanthresh=meanthresh,scan_width=scan_width,$
    savstep=savstep,saveonly=saveonly,sigmavalue=sigmavalue
;+
; :Description: 
;       Boxit was too computation-intensive, this is more a rough boxing program
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

;   scan_width : in, optional, type=integer, default=20
;       How far apart the scanning should be
;   savstep: in, required, type=integer, default=4
;       The number of steps to include in the fits file. 
;       savstep = 1: cropped image
;       savstep = 2: long strips
;       savstep = 3: limb strips
;       savstep = 4: center of mask
;   saveonly: in, optional
;       Determines whether or not savstep saves steps leading up to savstep or just savstep
;
;
; :Examples:
;       cropped = scanbox(file='$PWD/sep11_postit/104533_20120912_124300_353097_0.bin',/time)
;
;-
; for novelty
start = systime(1,/seconds)
IF ~keyword_set(file)       THEN file       = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(scanwidth)  THEN scan_width = 20

; Setting some parameters
newi    = 0
newj    = 0
xpos    = 0
ypos    = 0

; Using this statement so we don't need a bmp keyword
; Actually, let's use a better switch. 
; tiff, bin, bmp, I need to make the probelm accept any of these no-problemo.
IF STRPOS(file, 'tiff') NE -1  THEN BEGIN
    ; Read the bmp file
    tmpimage = read_tiff(file)
    ; Get height AND width
    n_col = (size(tmpimage,/dimensions))[1]
    n_row = (size(tmpimage,/dimensions))[2]
    
    ; Let's use vectors to reize the 3xn_colxn_row array
    image = reform(tmpimage[0,*,*])

    ; IF keyword_set(sigmavalue) THEN thresh = max(image) - sigmavalue*stddev(image) ELSE $
    ;     thresh = max(image) - stddev(image)
    thresh = mean(image)

    ;Cmon Jeren you didn't dominate Radio Lab for nothing

    ; A counter to see how many rows/cols it takes to hit the sun
    rowscan = 0
    colscan = 0
    WHILE n_elements(where(image[*,rowscan*scan_width] GT thresh)) EQ 1 DO BEGIN
        rowscan+=1
    ENDWHILE
    WHILE n_elements(where(image[colscan*scan_width,*] GT thresh)) EQ 1 DO BEGIN
        colscan+=1
    ENDWHILE

    ; Now using a counter to see how many rows/cols it takes to get off the sun
    rowendscan = rowscan
    colendscan = colscan

    WHILE n_elements(where(image[*,rowendscan*scan_width] GT thresh)) GT 1 DO BEGIN
        rowendscan+=1
    ENDWHILE
    WHILE n_elements(where(image[colendscan*scan_width,*] GT thresh)) GT 1 DO BEGIN
        colendscan+=1
    ENDWHILE    

    ; Giving ourself a little leeway
    ; Gives inconsistent image sizes, must fix.
    rowendscan  += 1
    rowscan     -= 1
    colendscan  += 1
    colscan     -= 1

    ; Preallocating a cropped array that we'll fill with values based on limits
    cropped_image = bytarr((colendscan-colscan)*scan_width,(rowendscan-rowscan)*scan_width)

    ; Is the i,j index between the cropped limits? If so, assign image[i,j] value to cropped_image
    FOR j=0,n_row-1 DO BEGIN
        FOR i = 0,n_col-1 DO BEGIN
            IF (i GT colscan*scan_width) AND $
            (i LT colendscan*scan_width) AND $
            (j GT rowscan*scan_width) AND $
            (j LT rowendscan*scan_width) THEN BEGIN
                cropped_image[newi,newj] = image[i,j]
                newi+=1 
            ENDIF
            IF newi EQ (colendscan-colscan)*scan_width-1 THEN BEGIN
                newj+=1
                newi=0
            ENDIF
        ENDFOR
    ENDFOR
ENDIF 

IF STRPOS(file, 'bmp') NE -1  THEN BEGIN
    ; Read the bmp file
    tmpimage = read_bmp(file)
    ; Get height AND width
    n_col = (size(tmpimage,/dimensions))[1]
    n_row = (size(tmpimage,/dimensions))[2]
    
    ; Let's use vectors to reize the 3xn_colxn_row array
    image = reform(tmpimage[0,*,*])

    ;Just some thresholds
    ; IF keyword_set(sigmavalue) THEN thresh = max(image) - sigmavalue*stddev(image) ELSE $
    ;     thresh = max(image) - stddev(image)
    thresh = mean(image)

    ;Cmon Jeren you didn't dominate Radio Lab for nothing

    ; A counter to see how many rows/cols it takes to hit the sun
    rowscan = 0
    colscan = 0
    WHILE n_elements(where(image[*,rowscan*scan_width] GT thresh)) EQ 1 DO BEGIN
        rowscan+=1
    ENDWHILE
    WHILE n_elements(where(image[colscan*scan_width,*] GT thresh)) EQ 1 DO BEGIN
        colscan+=1
    ENDWHILE

    ; Now using a counter to see how many rows/cols it takes to get off the sun
    rowendscan = rowscan
    colendscan = colscan

    WHILE n_elements(where(image[*,rowendscan*scan_width] GT thresh)) GT 1 DO BEGIN
        rowendscan+=1
    ENDWHILE
    WHILE n_elements(where(image[colendscan*scan_width,*] GT thresh)) GT 1 DO BEGIN
        colendscan+=1
    ENDWHILE    

    rowendscan  += 1
    rowscan     -= 1
    colendscan  += 1
    colscan     -= 1

    ; Preallocating a cropped array that we'll fill with values based on limits
    cropped_image = bytarr((colendscan-colscan)*scan_width,(rowendscan-rowscan)*scan_width)

    ; Is the i,j index between the cropped limits? If so, assign image[i,j] value to cropped_image
    FOR j=0,n_row-1 DO BEGIN
        FOR i = 0,n_col-1 DO BEGIN
            IF (i GT colscan*scan_width) AND $
            (i LT colendscan*scan_width) AND $
            (j GT rowscan*scan_width) AND $
            (j LT rowendscan*scan_width) THEN BEGIN
                cropped_image[newi,newj] = image[i,j]
                newi+=1 
            ENDIF
            IF newi EQ (colendscan-colscan)*scan_width-1 THEN BEGIN
                newj+=1
                newi=0
            ENDIF
        ENDFOR
    ENDFOR
ENDIF

IF STRPOS(file, 'bin') NE -1  THEN BEGIN
    n_row       = 960
    n_col       = 1280
    flipimage   = bytarr(n_col,n_row)
    image       = bytarr(n_col,n_row)

    ; These values are fixed for the model Prosilica GC 1290, we don't have to worry about making this 
    ; variable
    openr,lun,file,/get_lun
    readu,lun,image
    free_lun,lun

    FOR i=0,n_row-1 DO BEGIN
        flipimage[*,i] = image[*,n_row-1-i]
    ENDFOR

    ; Only here so I keep consistent
    image = flipimage

    themean = mean(flipimage)

    ; A counter to see how many rows/cols it takes to hit the sun
    rowscan = 0
    colscan = 0
    WHILE n_elements(where(image[*,rowscan*scan_width] GT thresh)) EQ 1 DO BEGIN
        rowscan+=1
    ENDWHILE
    WHILE n_elements(where(image[colscan*scan_width,*] GT thresh)) EQ 1 DO BEGIN
        colscan+=1
    ENDWHILE

    ; Now using a counter to see how many rows/cols it takes to get off the sun
    rowendscan = rowscan
    colendscan = colscan

    WHILE n_elements(where(image[*,rowendscan*scan_width] GT thresh)) GT 1 DO BEGIN
        rowendscan+=1
    ENDWHILE
    WHILE n_elements(where(image[colendscan*scan_width,*] GT thresh)) GT 1 DO BEGIN
        colendscan+=1
    ENDWHILE    

    rowendscan  += 1
    rowscan     -= 1
    colendscan  += 1
    colscan     -= 1

    ; Preallocating a cropped array that we'll fill with values based on limits
    cropped_image = bytarr((colendscan-colscan)*scan_width,(rowendscan-rowscan)*scan_width)

    ; Is the i,j index between the cropped limits? If so, assign image[i,j] value to cropped_image
    FOR j=0,n_row-1 DO BEGIN
        FOR i = 0,n_col-1 DO BEGIN
            IF (i GT colscan*scan_width) AND $
            (i LT colendscan*scan_width) AND $
            (j GT rowscan*scan_width) AND $
            (j LT rowendscan*scan_width) THEN BEGIN
                cropped_image[newi,newj] = image[i,j]
                newi+=1 
            ENDIF
            IF newi EQ (colendscan-colscan)*scan_width-1 THEN BEGIN
                newj+=1
                newi=0
            ENDIF
        ENDFOR
    ENDFOR
ENDIF

;for novelty purposes
finish = systime(1,/seconds)
IF keyword_set(time) THEN  print, 'Elapsed Time for scanbox(): ' + $
    strcompress(finish-start,/remove)+ ' seconds'
; save,cropped_image,filename='cropped_image.sav',/compress
IF n_elements(saveonly) EQ 0 THEN BEGIN
    bigstruct = {crop:cropped_image}
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
RETURN,cropped_image
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp2,xpos,ypos,thresh,file=file,time=time,plot=plot,sigmavalue=sigmavalue,$
        savstep=savstep,saveonly=saveonly
;+
; :Description:
;           Finds the centroid by summing all values over a certain threshold and averaging them 
;
; :Keywords:
;   file: in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time: in, optional
;       Prints the elapsed time
;   plot: in, optional
;       Makes a nice plot
;   sigmavalue: in, optional, type=integer, default=1
;       Sets the threshold to be::
;
;       max(image) - sigmavalue*stddev(image)
;
;   savstep: in, required, type=integer, default=4
;       The number of steps to include in the fits file. 
;       savstep = 1: cropped image
;       savstep = 2: long strips
;       savstep = 3: limb strips
;       savstep = 4: center of mask
;   saveonly: in, optional
;       Determines whether or not savstep saves steps leading up to savstep or just savstep
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
IF ~keyword_set(file)   THEN file   = '104533_20120911_153147_254618_0.bin'
cropped_image = scanbox(file=file,time=time)
IF keyword_set(sigmavalue)  THEN thresh = max(cropped_image) - stddev(cropped_image)*sigmavalue ELSE $
thresh = max(cropped_image) - stddev(cropped_image)
start = systime(1,/seconds)

; BEWARE:
; comp2 takes into account all pixels above acertain threshold, including fiducial marks. comp6v2 
; doesn't deal with what's going on in the non-limb parts of the sun. But in the case of a 
; fiducial mark hitting the limb, what do we do?


; Allocating an array where values are above the threshold
; Funny thing, it's .0003 seconds faster to only run 1 instance of size()
s = size(cropped_image,/dimensions)
n_col = s[0]
n_row = s[1]
sunonly = bytarr(n_col,n_row)

; Doing the thing I just allocated for
FOR i = 0,n_col-1 DO BEGIN
    FOR j = 0,n_row - 1 DO BEGIN
        IF cropped_image[i,j] GT thresh THEN sunonly[i,j] = cropped_image[i,j]
    ENDFOR
ENDFOR

sum     = Total(sunonly)
xpos    = Total( Total(sunonly, 2) * Indgen(n_col) ) / sum
ypos    = Total( Total(sunonly, 1) * Indgen(n_row) ) / sum 

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

PRO comp4, xstrips,ystrips,thresh,file=file,time=time,scan_width=scan_width,sigmavalue=sigmavalue,$
    savstep=savstep,saveonly=saveonly
;+
; :Description:
;           Only used to save the strips into structures. 
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
;
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
IF ~keyword_set(scan_width) THEN scan_width = 10 ELSE scan_width = scan_width

cropped_image = scanbox(file=file,time=time,savstep=savstep,saveonly=saveonly)
IF keyword_set(sigmavalue)  THEN thresh = max(cropped_image) - stddev(cropped_image)*sigmavalue ELSE $
thresh = max(cropped_image) - stddev(cropped_image)

start = systime(1,/seconds)

; cgimage,cropped_image,/keep_asp
s = size(cropped_image,/dimensions)
length = s[0]
height = s[1]

; Getting some values
col_chords = fltarr(length/scan_width)
row_chords = fltarr(height/scan_width)

rowchord_endpoints = fltarr(2,height/scan_width)
colchord_endpoints = fltarr(2,length/scan_width)

; Counting the lenGTh of chords where cropped_image > thresh
FOR i = 0,length/scan_width-1 DO BEGIN
    col_where = where(cropped_image[i*scan_width,*] GT thresh,col_num)
    col_chords[i] = col_num
    ; beginning of chord
    colchord_endpoints[0,i] = col_where[0]
    ; end of chord
    colchord_endpoints[1,i] = col_where[-1]
ENDFOR

FOR i = 0,height/scan_width -1 DO BEGIN
    row_where = where(cropped_image[*,i*scan_width] GT thresh,row_num)
    row_chords[i] = row_num
    ; beginning of chord
    rowchord_endpoints[0,i] = row_where[0]
    ; end of chord
    rowchord_endpoints[1,i] = row_where[-1]
ENDFOR

xstrips = REPLICATE({ROWINDEX:0,SCAN_WIDTH:scan_width,ARRAY:bytarr(length)},height/scan_width)
ystrips = REPLICATE({COLINDEX:0,SCAN_WIDTH:scan_width,ARRAY:bytarr(height)},length/scan_width)

FOR i = 0,height/scan_width - 1 DO BEGIN
    xstrips[i].ROWINDEX = i
    xstrips[i].ARRAY = cropped_image[*,i*scan_width]
ENDFOR

FOR k = 0,length/scan_width - 1 DO BEGIN
    ystrips[k].COLINDEX = k
    ystrips[k].ARRAY = cropped_image[k*scan_width,*]
ENDFOR

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print,'Elapsed Time for comp4: ',strcompress(finish-start,/rem),' seconds'
; save,xstrips,ystrips,thresh,filename='comp4strips.sav',/compress
IF savstep GE 2 AND n_elements(saveonly) EQ 0 THEN BEGIN
    restore,'bigstruct.sav'
    longstrips = {longxstrips:xstrips,longystrips:ystrips,thresh:thresh}
    bigstruct = create_struct(bigstruct,longstrips)
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
IF savstep GE 2 AND n_elements(saveonly) NE 0 THEN BEGIN
    bigstruct = {longxstrips:xstrips,longystrips:ystrips,thresh:thresh}
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp5v2, xstrips,ystrips,thresh,file=file,time=time,ministrip_length=ministrip_length,$
        scan_width=scan_width,sigmavalue=sigmavalue,savstep=savstep,saveonly=saveonly
;+
; :Description:
;           Only used to save the cut-down strips into structures. Imports strips from 
;           comp4 like a good code should.
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
;
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
;           comp5v2,xstrips,ystrips,thresh
;
; :TODO:
;
;   Exactly how much data should be stored in a structure? Since we're interested in saving space,
;   doesn't make sense to repeat any data in the structures.
;-

IF ~keyword_set(file)       THEN file       = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(scan_width) THEN scan_width = 10 ELSE scan_width = scan_width
IF ~keyword_set(ministrip_length) THEN ministrip_length = 9 ELSE ministrip_length=ministrip_length
ministrip_side_length = ministrip_length/2

start = systime(1,/seconds)

comp4,c4xstrips,c4ystrips,thresh,file=file,time=time,sigmavalue=sigmavalue,scan_width=scan_width,$
    savstep=savstep,saveonly=saveonly

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

; Setting the length of the cut dowen array
IF ~keyword_set(ministrip_length) THEN ministrip_length = 13
ministrip_side_buffer = ministrip_length/2 

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
        ; the indices from rowchord_endpoints[0,i] +/- ministrip_side_length 
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
IF keyword_set(time) THEN  print,'Elapsed Time for comp5v2: ',strcompress(finish-start,/rem),' seconds'
; save,xstrips,ystrips,thresh,scan_width,sigmavalue,ministrip_length,$
;   filename='comp5v2strips.sav',/compress
; mwrfits,xstrips,'test.fits',/create
; Just did a quick sanity check and with a ministrip_length = 23, the compressed savfile is 
; 975 bytes. That's about 46*10 points per strip with 120 strips -> The pdf says about 2 bytes per strip 
; but I'm getting about 8 bytes per strip. 
;
; Using 5 points per strip instead of 23 cuts us down to 656 bytes, which is 5.4 bytes per strip.
; How am I supposed to cut it down to 2?
; Using bytarr instead of fltarr saves me 40 bytes. 
; Using a scan_width of 20 instead of 10 gets me to 536 bytes (4.4 bytes per strip, 2.2 bytes per 
; cut-down strip. Is this the value we want?
;
; Not anymore since I added BEGINDEX and ENDINDEX. Oh well.
IF savstep GE 3 AND n_elements(saveonly) EQ 0 THEN BEGIN
    shortstrips = {shortxstrips:xstrips,shortystrips:ystrips,scan_width:scan_width,sigmavalue:sigmavalue,$
        ministrip_length:ministrip_length}
    restore,'bigstruct.sav'
    bigstruct = create_struct(bigstruct,shortstrips)
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
IF savstep GE 3 AND n_elements(saveonly) NE 0 THEN BEGIN
    bigstruct = {shortxstrips:xstrips,shortystrips:ystrips,scan_width:scan_width,sigmavalue:sigmavalue,$
        ministrip_length:ministrip_length}
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp6v2,xpos,ypos,file=file,order=order,time=time,scan_width=scan_width,$
    ministrip_length=ministrip_length,plot=plot,sigmavalue=sigmavalue,savstep=savstep,saveonly=saveonly
;+
; :Description:
;       Uses the data from comp5v2 and draws a linear/quadratic/cubic/etc. function to find midpoint.
;       Order is the power of the function. Different from comp6 in that this method uses fz_roots()
;       and comp6 uses spline to interpolate where the limb crosses a threshold. Imports the strips
;       from comp5v2.
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
IF ~keyword_set(file)       THEN file = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(order)      THEN order = 3      ;ELSE order = order

xlen    = 0
xsum    = 0
xnum    = 0

ylen    = 0
ysum    = 0
ynum    = 0
; For novelty purposes
start = systime(1,/seconds)

; How many points should we use? Paper says 2-6 so I'll do 5
IF ~keyword_set(ministrip_length) THEN ministrip_length = 9 ; ELSE ministrip_length=ministrip_length
ministrip_side_length = ministrip_length/2

; Run the program to get our structures
comp5v2,xstrips,ystrips,thresh,file=file,time=time,ministrip_length=ministrip_length,$
    sigmavalue=sigmavalue,scan_width=scan_width,savstep=savstep,saveonly=saveonly

xarr    = findgen(n_elements(xstrips[4].STARTPOINTS))
yarr    = findgen(n_elements(ystrips[4].STARTPOINTS))
tx      = findgen(n_elements(xstrips[4].STARTPOINTS) * 1000)/100
ylenarr = findgen(n_elements(ystrips))
xlenarr = findgen(n_elements(xstrips))

;Only needed to run diagnostics
; cropped_image = scanbox(file=file,time=time)

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
        ; print,(cropped_image[*,n*scan_width])[xstrips[n].BEGINDEX + floor(begusable)]
        ; print,(cropped_image[*,n*scan_width])[xstrips[n].BEGINDEX + floor(begusable) + 1]
        ; stop
    ENDIF ELSE BEGIN
        begusable   = 0
        stripbeg    = 0
    ENDELSE

    IF xstrips[n].ENDINDEX GT 0 THEN BEGIN
        endroots    = fz_roots(endresult)
        endusable   = (real_part(endroots))[where(imaginary(endroots) eq 0.)]
        endusable   = (endusable[where(endusable gt 0)])[0]
        stripend    = xstrips[n].ENDINDEX + endusable
        ; print,(cropped_image[*,n*scan_width])[xstrips[n].ENDINDEX + floor(endusable)]
        ; print,(cropped_image[*,n*scan_width])[xstrips[n].ENDINDEX + floor(endusable) + 1]
        ; stop
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
ENDIF
; Just an aside, but plotting adds ~ .1 seconds to bring it up to .22 seconds total

finish = systime(1,/seconds)
IF keyword_set(time) THEN  print,'Elapsed Time for comp6v2: ',strcompress(finish-start,/rem),' seconds'
; save,xpos,ypos,thresh,sigmavalue,order,file, ministrip_length,scan_width,$
;     filename='comp6results.sav',/compress

strformatcode = 'a'+strcompress(strlen(file),/rem)

OPENW,1,'comp6results.dat' 
PRINTF,1,xpos,ypos,thresh,sigmavalue,order,ministrip_length,scan_width,file, $
    format='(F7.2,1X,F7.2,1X,F7.2,I,1X,I,1X,I,1X,I,1X,'+strformatcode+')'
CLOSE,1

strformatcode = 'a'+strcompress(strlen(file),/rem)
OPENW,2,'comp6results.txt' 
PRINTF,2,xpos,ypos,thresh,sigmavalue,order,ministrip_length,scan_width,file, $
    format='(F7.2,1X,F7.2,1X,F7.2,I,1X,I,1X,I,1X,I,1X,'+strformatcode+')'
CLOSE,2

IF savstep EQ 4 AND n_elements(saveonly) EQ 0 THEN BEGIN
    restore,'bigstruct.sav'
    stuff = {xpos:xpos,ypos:ypos,order:order}
    bigstruct = create_struct(stuff,bigstruct)
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF
IF savstep EQ 4 AND n_elements(saveonly) NE 0 THEN BEGIN
    bigstruct = {xpos:xpos,ypos:ypos,order:order}
    save,bigstruct,filename='bigstruct.sav',/compress
ENDIF

RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO cc

cropped_image = scanbox(file='gauss1pix.tiff')
restore,'comp4strips.sav',/v
window,0
plot,ystrips[5].ARRAY,xs=3,ys=3,/nodata
FOR i=0,n_elements(ystrips) - 1 DO BEGIN
    oplot,ystrips[i].ARRAY,color=i+1
    ; print,closest((ystrips[i].ARRAY)[0:59],thresh),closest((ystrips[i].ARRAY)[60:119],thresh)
ENDFOR   

window,1
plot,xstrips[5].ARRAY,xs=3,ys=3,/nodata
FOR i=0,n_elements(xstrips) - 1 DO BEGIN
    oplot,xstrips[i].ARRAY,color=i+1
ENDFOR   

window,2
shade_surf,cropped_image,shades=bytscl(cropped_image)

END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO makesomescatplots
;+
; :Description:
;   Makes a few scatter plots based on varying parameters. Sucks that the values are hardcoded.
;-

xcenter = 107.498
ycenter = 110.496
thresh  = 119.350

a = [105.686,106.016,106.523,106.846]
b = [110.5,110.923,111.306,112.022]

c = [105.190,105.190,106.591,106.417]
d = [110.995,110.019,112.017,111.932]

e = [105.657,105.190,105.886,106.034]
f = [110.5,111.008,111.890,111.903]

g = [105.191,105.681,105.577,105.610]
h = [110.5,110.754,111.133,110.694]

ad = [1.81226,1.48,.974,.652]
bd = [-.00353,-.4267,-.809,-1.52528]

cd = [2.30773,2.30796,.906784,1.08130]
dd = [-.49828,-.522530,-1.52,-1.43504]

ed = [1.84131,2.30773,1.61249,1.46391]
fd = [-.003,-.511787,-1.39378,-1.40653]

gd = [2.30679,1.81726,1.92130,1.88797]
hd = [-.00353,-2.57034,-.636787,-.197037]


ps_start,filename='0pix.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,a,psym=7,xr=[1,4],ys=3,xs=3,yr=[100,115],ytitle='Position',$
    title='X and Y positions with 0 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,b,psym=4,color=6
    hline,xcenter
    hline,ycenter,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='1pix.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,c,psym=7,xr=[1,4],ys=3,xs=3,yr=[100,115],ytitle='Position',$
    title='X and Y positions with 1 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,d,psym=4,color=6
    hline,xcenter
    hline,ycenter,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='2pix.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,e,psym=7,xr=[1,4],ys=3,xs=3,yr=[100,115],ytitle='Position',$
    title='X and Y positions with 2 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,f,psym=4,color=6
    hline,xcenter
    hline,ycenter,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='4pix.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,g,psym=7,xr=[1,4],ys=3,xs=3,yr=[100,115],ytitle='Position',$
    title='X and Y positions with 4 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,h,psym=4,color=6
    hline,xcenter
    hline,ycenter,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ps_start,filename='0pixdev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,ad,psym=7,xr=[1,4],ys=3,xs=3,yr=[-3,3],ytitle='Position',$
    title='X and Y Deviations with 0 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,bd,psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='1pixdev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,cd,psym=7,xr=[1,4],ys=3,xs=3,yr=[-3,3],ytitle='Position',$
    title='X and Y Deviations with 1 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,dd,psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='2pixdev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,ed,psym=7,xr=[1,4],ys=3,xs=3,yr=[-3,3],ytitle='Position',$
    title='X and Y Deviations with 2 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,fd,psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='4pixdev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,gd,psym=7,xr=[1,4],ys=3,xs=3,yr=[-3,3],ytitle='Position',$
    title='X and Y Deviations with 4 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,hd,psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps


END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO msmsp
;+
; :Description:
;   Abbreviated form of "Make Some More Scatter Plots"
;   Making more scat plots now that I've fixed my program and am now moving onto bmp images
;
;-

xc = 49.1981
yc = 45.4614
thresh = 142.208

ministrip_length = 9
scan_width = 10
file = 'Sun_Images_000000.bmp'
sigmavalue = 1

comp6v2,xpos,ypos,order=1,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length
x1=xpos
y1=ypos
comp6v2,xpos,ypos,order=2,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length
x2=xpos
y2=ypos
comp6v2,xpos,ypos,order=3,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length
x3=xpos
y3=ypos
comp6v2,xpos,ypos,order=4,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length
x4=xpos
y4=ypos

ps_start,filename='bmpsundev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,xc-[x1,x2,x3,x4],psym=7,xr=[1,4],ys=3,xs=3,yr=[-1,1],ytitle='Position',$
        title='X and Y Deviations from bmp sun, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,yc-[y1,y2,y3,y4],psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps


ps_start,filename='bmpsun.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,[x1,x2,x3,x4],psym=7,xr=[1,4],ys=3,xs=3,yr=[40,55],ytitle='Position',$
        title='X and Y positions from bmp sun, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,[y1,y2,y3,y4],psym=4,color=6
    hline,xc
    hline,yc,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO centerv2,file=file,ministrip_length=ministrip_length,scan_width=scan_width,time=time,order=order, $
    plot=plot,sigmavalue=sigmavalue,savstep=savstep,saveonly=saveonly
;+
; :Description:
;       Finds the centroid using a variety of compression levels
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

comp6v2,xpos,ypos,time=time,order=order,scan_width=scan_width,file=file,plot=plot,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length,savstep=savstep,saveonly=saveonly
print,'Limb-fitting:'
print,'X Center is ',xpos
print,'Y Center is ',ypos
print,''
print,'Difference in X Position',xcs-xpos
print,'Difference in Y Position',ycs-ypos

; writefits,'test.fits',[xpos,ypos]
; a=readfits('test.fits',header)
restore,'bigstruct.sav'
; stop
; writefits,'bigstruct.fits',[bigstruct]
; hdr = ['Some values','poo']
; mwrfits,(bigstruct.longxstrips)[0].ARRAY,'bigstruct.fits',/create
; writefits,'bigstruct.fits',(bigstruct.longxstrips)[0].ARRAY; b=readfits('bigstruct.fits')
; writefits,'bigstruct.fits',(bigstruct.longxstrips)[0].ARRAY
; writefits,'bigstruct.fits',(bigstruct.longxstrips)[0].ROWINDEX,/append
; writefits,'bigstruct.fits',(bigstruct.longxstrips)[0].SCAN_WIDTH,/append
; c=mrdfits('bigstruct.fits')
; f=readfits('bigstruct.fits')
; help,c
; help,f
; d=mrdfits('test.fits')

; This works, but only because I use readfits instead of mrdfits? Does it have something to 
; do with the syntax of using mwrfits? 
; mwrfits,[xpos,ypos],'e.fits',/create
; e=readfits('e.fits')


; so not even this works
str = {garlic,x:0}
; str = findgen(100) ;if str is an array and not a structure, writefits doesn't complain

writefits,'myfile.fits',str
mwrfits,str,'myfile.fits',/create
a=readfits('myfile.fits')
help,a


; So I think I've figured our the source of the problem, it's maybe something to do with FXADDPAR
; not being up to date enough, but I haven't figured out a place to download a more recent 
; version online. 

; Also, I can't use writefits to write structures because that's something I can't do with 
; writefits, unfortunately. There is only the capability to save arrays? Lame.

; So my options are to either find a more recent version of FXADDPAR (if that's even the problem)
; or find another way to save fits files. 

stop
END
