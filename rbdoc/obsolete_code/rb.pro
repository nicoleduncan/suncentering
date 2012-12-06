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

FUNCTION scanbox,file=file,time=time,meanthresh=meanthresh,scan_width=scan_width
;+
; :Description: 
;       Boxit was too computation-intensive, this is more a rough boxing program
;
; :Keywords:
;   file : in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time : in, optional
;       Print the elapsed time
;   meanthresh : in, optional
;       Use the average of the image as the threshold value. Otherwise, use half of max
;   scan_width : in, optional, type=integer, default=20
;       How far apart the scanning should be
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
; tiff, bin, bmp, should be able to accept any of these no-problemo.
IF STRPOS(file, 'tiff') NE -1  THEN BEGIN
    ; Read the bmp file
    tmpimage = read_tiff(file)
    ; Get height AND width
    n_col = (size(tmpimage,/dimensions))[1]
    n_row = (size(tmpimage,/dimensions))[2]
    
    ; Let's use vectors to reize the 3xn_colxn_row array
    image = reform(tmpimage[0,*,*])

    ;Just some thresholds
    themean = mean(image)
    halfmax = max(image)/2
    IF keyword_set(meanthresh) THEN thresh = themean ELSE thresh = halfmax

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
    ; print,'Picked up sun on ',strcompress(rowscan,/rem),'rd row'
    ; print,'Picked up sun on ',strcompress(colscan,/rem),'rd col'

    ; Now using a counter to see how many rows/cols it takes to get off the sun
    rowendscan = rowscan
    colendscan = colscan

    WHILE n_elements(where(image[*,rowendscan*scan_width] GT thresh)) GT 1 DO BEGIN
        rowendscan+=1
    ENDWHILE
    WHILE n_elements(where(image[colendscan*scan_width,*] GT thresh)) GT 1 DO BEGIN
        colendscan+=1
    ENDWHILE    

    ; print,rowendscan,' more rows until no more Sun'
    ; print,colendscan,' more cols until no more Sun'

    ; image[*,scan_width*findgen(n_row/scan_width)] = 0
    ; image[scan_width*findgen(n_col/scan_width),*] = 0
    ; cgimage,image,/keep_asp

    ; Giving ourself a little leeway
    ; Gives inconsistent image sizes
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
            (i lt colendscan*scan_width) AND $
            (j GT rowscan*scan_width) AND $
            (j lt rowendscan*scan_width) THEN BEGIN
                cropped_image[newi,newj] = image[i,j]
                newi+=1 
            ENDIF
            IF newi EQ (colendscan-colscan)*scan_width-1 THEN BEGIN
                newj+=1
                newi=0
            ENDIF
        ENDFOR
    ENDFOR
    ; And we're done with that.
ENDIF ELSE BEGIN

    ; Haven't touched this since I started with the new bmp file
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
    halfmax = max(flipimage)/2
    IF keyword_set(meanthresh) THEN thresh = themean ELSE thresh = halfmax

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
    ; Gives inconsistent image sizes, need to change
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
            (i lt colendscan*scan_width) AND $
            (j GT rowscan*scan_width) AND $
            (j lt rowendscan*scan_width) THEN BEGIN
                cropped_image[newi,newj] = image[i,j]
                newi+=1 
            ENDIF
            IF newi EQ (colendscan-colscan)*scan_width-1 THEN BEGIN
                newj+=1
                newi=0
            ENDIF
        ENDFOR
    ENDFOR
ENDELSE

;for novelty purposes
finish = systime(1,/seconds)
IF keyword_set(time) THEN  print, 'Elapsed Time for scanbox(): ' + $
    strcompress(finish-start,/remove)+ ' seconds'
RETURN,cropped_image
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp2,xpos,ypos,thresh,file=file,time=time,plot=plot,sigmavalue=sigmavalue
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
;       Sets the threshold to be max(image) - sigmavalue*stddev(image)
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
;           comp2,xpos,ypos,/plot
;
;-
IF ~keyword_set(file)   THEN file   = '104533_20120911_153147_254618_0.bin'
cropped_image = scanbox(file='nogauss.tiff',time=time)
IF keyword_set(sigmavalue)  THEN thresh = max(cropped_image) - stddev(cropped_image)*sigmavalue ELSE $
thresh = max(cropped_image) - stddev(cropped_image)


start = systime(1,/seconds)

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

PRO comp4, xstrips,ystrips,thresh,file=file,time=time,scan_width=scan_width,sigmavalue=sigmavalue
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
;       Sets the threshold to be max(image) - sigmavalue*stddev(image)
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
;           comp4,xstrips,ystrips,thresh,/time,scan_width=10,/thalf
;
;-

IF ~keyword_set(file)       THEN file       = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(scan_width) THEN scan_width = 10 ELSE scan_width = scan_width

cropped_image = scanbox(file='Sun_Images_000000.bmp',time=time)
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
save,xstrips,ystrips,thresh,filename='comp4strips.sav',/compress
; Holy shit this is 2 KB. So much unnecessary data
RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp5, xstrips,ystrips,thresh,file=file,time=time,ministrip_length=ministrip_length,$
        scan_width=scan_width,sigmavalue=sigmavalue
;+
; :Description:
;           Only used to save the cut-down strips into structures. 
;
; :Keywords:
;   file: in, optional, type=string, default='104533_20120911_153147_254618_0.bin'
;       File to be read in
;   time: in, optional
;       Prints the elapsed time
;   scan_width: in, optional, type=integer, default=10
;       Indicates how far apart to scan
;   ministrip_length: in, optional, type=byte, default=23
;       How long the total array of the cut-down strip will be
;   sigmavalue: in, optional, type=integer, default=1
;       Sets the threshold to be max(image) - sigmavalue*stddev(image)
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
;-

IF ~keyword_set(file)       THEN file       = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(scan_width) THEN scan_width = 10 ELSE scan_width = scan_width
; cropped_image = scanbox(file='Sun_Images_000000.bmp',time=time)
cropped_image = scanbox(file=file,time=time)

; Trying a new threshold instead of the mean
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

; Counting the length of chords where cropped_image > thresh
FOR i = 0,length/scan_width-1 DO BEGIN
    col_where = where(cropped_image[i*scan_width,*] GT thresh,col_num)
    col_chords[i] = col_num
    ; beginning of chord
    colchord_endpoints[0,i] = col_where[0]
    ; end of chord
    colchord_endpoints[1,i] = col_where[-1]
    ; if i eq 2 then stop
ENDFOR

FOR i = 0,height/scan_width -1 DO BEGIN
    row_where = where(cropped_image[*,i*scan_width] GT thresh,row_num)
    row_chords[i] = row_num
    ; beginning of chord
    rowchord_endpoints[0,i] = row_where[0]
    ; end of chord
    rowchord_endpoints[1,i] = row_where[-1]
ENDFOR

; Setting the length of the cut dowen array
IF ~keyword_set(ministrip_length) THEN ministrip_length = 23
ministrip_side_buffer = ministrip_length/2 

; Preallocating the array, replicating it by the number of strips there are
xstrips = REPLICATE({ROWINDEX:0,SCAN_WIDTH:scan_width, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:bytarr(ministrip_length), $
        ENDPOINTS:bytarr(ministrip_length)},height/scan_width)
ystrips = REPLICATE({COLINDEX:0,SCAN_WIDTH:scan_width, BEGINDEX:0, ENDINDEX:0, $
        STARTPOINTS:bytarr(ministrip_length), $
        ENDPOINTS:bytarr(ministrip_length)},length/scan_width)

;Filling out structure with cut-down strip information
FOR i = 0,height/scan_width - 1 DO BEGIN
    xstrips[i].ROWINDEX     = i
    ; If there is no strip that cuts through the sun, set things to 0
    IF rowchord_endpoints[0,i] EQ -1 THEN BEGIN
        xstrips[i].STARTPOINTS  = fltarr(ministrip_length) 
        xstrips[i].BEGINDEX     = 0
    ENDIF ELSE BEGIN
        ; STARTPOINTS is the cut down strip with length = ministrip_length and contains
        ; the indices from rowchord_endpoints[0,i] +/- ministrip_side_length 
        xstrips[i].STARTPOINTS  = $
            cropped_image[rowchord_endpoints[0,i]-ministrip_side_buffer: $
            rowchord_endpoints[0,i]+ministrip_side_buffer,i*scan_width]
        ; BEGINDEX is the index of the strip where it begins. 
        ; e.g., the array is 5 long, starts from index 9 and is centered around index 11
        xstrips[i].BEGINDEX     = fix(colchord_endpoints[0,i] - ministrip_side_buffer)  
    ENDELSE
    IF rowchord_endpoints[1,i] EQ -1 THEN BEGIN
        xstrips[i].ENDPOINTS    = fltarr(ministrip_length)
        xstrips[i].ENDINDEX    = 0
    ENDIF ELSE BEGIN
        xstrips[i].ENDPOINTS    = $
            cropped_image[rowchord_endpoints[1,i]-ministrip_side_buffer: $
            rowchord_endpoints[1,i]+ministrip_side_buffer,i*scan_width]
        xstrips[i].ENDINDEX     = fix(colchord_endpoints[1,i] - ministrip_side_buffer)
    ENDELSE
ENDFOR

FOR k = 0,length/scan_width - 1 DO BEGIN
    ystrips[k].COLINDEX     = k
    IF colchord_endpoints[0,k] EQ -1 THEN BEGIN
        ystrips[k].STARTPOINTS  = fltarr(ministrip_length) 
        ystrips[k].BEGINDEX     = 0
    ENDIF ELSE BEGIN 
        ystrips[k].STARTPOINTS  = cropped_image[k*scan_width,colchord_endpoints[0,k]-ministrip_side_buffer: $
            colchord_endpoints[0,k]+ministrip_side_buffer]
        ystrips[k].BEGINDEX     = fix(colchord_endpoints[0,k] - ministrip_side_buffer)
    ENDELSE
    IF colchord_endpoints[1,k] EQ -1 THEN BEGIN
        ystrips[k].ENDPOINTS    = fltarr(ministrip_length) 
        ystrips[k].ENDINDEX     = 0        
    ENDIF ELSE BEGIN
        ystrips[k].ENDPOINTS    = cropped_image[k*scan_width,colchord_endpoints[1,k]-ministrip_side_buffer: $
            colchord_endpoints[1,k]+ministrip_side_buffer]
        ystrips[k].ENDINDEX     = fix(colchord_endpoints[1,k] - ministrip_side_buffer) 
    ENDELSE
ENDFOR

; oops, x and y indices were switched
; array = cropped_image[*,3*scan_width]
; la = xstrips[3].STARTPOINTS
; ra = xstrips[3].ENDPOINTS
; lep = rowchord_endpoints[0,3]
; rep = rowchord_endpoints[1,3]

; stop
finish = systime(1,/seconds)
IF keyword_set(time) THEN  print,'Elapsed Time for comp5: ',strcompress(finish-start,/rem),' seconds'
save,xstrips,ystrips,thresh,filename='comp5strips.sav',/compress
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
RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO comp6,xpos,ypos,file=file,order=order,time=time,scan_width=scan_width,$
    ministrip_length=ministrip_length,plot=plot,sigmavalue=sigmavalue
;+
; :Description:
;       Uses the data from comp5 and draws a linear/quadratic/cubic function to find midpoint.
;       Order is the power of the function.
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
;       Sets the threshold to be max(image) - sigmavalue*stddev(image)
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
IF ~keyword_set(file)   THEN file = '104533_20120911_153147_254618_0.bin'
IF ~keyword_set(order)  THEN order = 3 ELSE order = order

xlen    = 0
xsum    = 0
xnum    = 0

ylen    = 0
ysum    = 0
ynum    = 0
; For novelty purposes
start = systime(1,/seconds)

; How many points should we use? Paper says 2-6 so I'll do 5
IF ~keyword_set(ministrip_length) THEN ministrip_length = 9 ELSE ministrip_length=ministrip_length
ministrip_side_length = ministrip_length/2

; Run the program to get our structures
comp5,xstrips,ystrips,thresh,file=file,time=time,ministrip_length=ministrip_length,sigmavalue=sigmavalue,$
    scan_width=scan_width

xarr = findgen(n_elements(xstrips[4].STARTPOINTS))
yarr = findgen(n_elements(ystrips[4].STARTPOINTS))
; Have to set to be this large or else min(abs()) will be fragmented
tx = findgen(n_elements(xstrips[4].STARTPOINTS) * 1000)/100
ty = findgen(n_elements(ystrips[4].STARTPOINTS) * 1000)/100

;Deal with rows
FOR n=0,n_elements(xstrips)-1 DO BEGIN
    ; Need to run POLY_FIT() to get an equation for SPLINE()
    startresult = poly_fit(xarr,xstrips[n].STARTPOINTS,order)
    endresult   = poly_fit(xarr,xstrips[n].ENDPOINTS,order)

    CASE order OF
    1: BEGIN
        ; Spline to get better interpolated values
        startz  = spline(xarr,startresult[0] + startresult[1]*xarr,tx)
        endz    = spline(xarr,endresult[0] + endresult[1]*xarr,tx)
        END
    2: BEGIN
        startz  = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2,tx)
        endz    = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2,tx)
        END
    3: BEGIN
        startz  = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
            startresult[3]*xarr^3,tx)
        endz    = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
            endresult[3]*xarr^3,tx)
        END
    4: BEGIN
        startz  = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
            startresult[3]*xarr^3 + startresult[4]*xarr^4,tx)
        endz    = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
            endresult[3]*xarr^3 + endresult[4]*xarr^4,tx)
        END
    5: BEGIN
        startz  = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
            startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5,tx)
        endz    = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
            endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5,tx)
        END    
    6: BEGIN
        startresult = reform(startresult)
        endresult = reform(endresult)
        startz  = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
            startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
            startresult[6]*xarr^6,tx)
        endz    = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
            endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
            endresult[6]*xarr^6,tx)
        END
    7: BEGIN
        startresult = reform(startresult)
        endresult = reform(endresult)
        startz  = spline(xarr,startresult[0] + startresult[1]*xarr + startresult[2]*xarr^2 + $
            startresult[3]*xarr^3 + startresult[4]*xarr^4 + startresult[5]*xarr^5 + $
            startresult[6]*xarr^6 + startresult[7]*xarr^7,tx)
        endz    = spline(xarr,endresult[0] + endresult[1]*xarr + endresult[2]*xarr^2 + $
            endresult[3]*xarr^3 + endresult[4]*xarr^4 + endresult[5]*xarr^5 + $
            endresult[6]*xarr^6 + endresult[7]*xarr^7,tx)
        END
    ENDCASE

    ; Find where chord intersects threshold through interpolated values
    a=min(abs(startz-thresh),bindex)
    b=min(abs(endz-thresh),eindex)

    ; Where does the strip begin? Where does it end?
    stripbeg = xstrips[n].BEGINDEX + tx[bindex]
    stripend = xstrips[n].ENDINDEX + tx[eindex]

    ; xlen = (stripend - stripbeg)/2. + stripbeg
    xlen = mean([[stripend],[stripbeg]])
    
; print,xlen

    ; if n eq 3 then stop
    ; It looks like it's correctly indexing the threshold, how else could it be fucking up the xcenter?
    ; print,'1 before',startz[bindex-1]
    ; print,startz[bindex]
    ; print,'1 after',startz[bindex+1]
    ; print,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    ; print,'1 before',endz[eindex-1]
    ; print,endz[eindex]
    ; print,'1 after',endz[eindex+1]

    ;Sum chord midpoints and then finds the average at the end
    IF xlen NE 0. THEN BEGIN
        xsum += xlen 
        xnum += 1
    ENDIF
ENDFOR    

FOR n=0,n_elements(ystrips)-1 DO BEGIN
    startresult = poly_fit(yarr,ystrips[n].STARTPOINTS,order)
    endresult   = poly_fit(yarr,ystrips[n].ENDPOINTS,order)

    CASE order OF
    1: BEGIN
        startz  = spline(yarr,startresult[0] + startresult[1]*yarr,ty)
        begz    = spline(yarr,endresult[0] + endresult[1]*yarr,ty)
        END
    2: BEGIN
        startz  = spline(yarr,startresult[0] + startresult[1]*yarr + startresult[2]*yarr^2,ty)
        begz    = spline(yarr,endresult[0] + endresult[1]*yarr + endresult[2]*yarr^2,ty)
        END
    3: BEGIN
        startz  = spline(yarr,startresult[0] + startresult[1]*yarr + startresult[2]*yarr^2 + $
            startresult[3]*yarr^3,ty)
        begz    = spline(yarr,endresult[0] + endresult[1]*yarr + endresult[2]*yarr^2 + $
            endresult[3]*yarr^3,ty)
        END
    4: BEGIN
        startz  = spline(yarr,startresult[0] + startresult[1]*yarr + startresult[2]*yarr^2 + $
            startresult[3]*yarr^3 + startresult[4]*yarr^4,ty)
        begz    = spline(yarr,endresult[0] + endresult[1]*yarr + endresult[2]*yarr^2 + $
            endresult[3]*yarr^3 + endresult[4]*yarr^4,ty)
        END
    5: BEGIN
        startz  = spline(yarr,startresult[0] + startresult[1]*yarr + startresult[2]*yarr^2 + $
            startresult[3]*yarr^3 + startresult[4]*yarr^4 + startresult[5]*yarr^5,ty)
        begz    = spline(yarr,endresult[0] + endresult[1]*yarr + endresult[2]*yarr^2 + $
            endresult[3]*yarr^3 + endresult[4]*yarr^4 + endresult[5]*yarr^5,ty)
        END    
    6: BEGIN
        startz  = spline(yarr,startresult[0] + startresult[1]*yarr + startresult[2]*yarr^2 + $
            startresult[3]*yarr^3 + startresult[4]*yarr^4 + startresult[5]*yarr^5 + $
            startresult[6]*yarr^6,ty)
        begz    = spline(yarr,endresult[0] + endresult[1]*yarr + endresult[2]*yarr^2 + $
            endresult[3]*yarr^3 + endresult[4]*yarr^4 + endresult[5]*yarr^5 + $
            endresult[6]*yarr^6,ty)
        END
    7: BEGIN
        startz  = spline(yarr,startresult[0] + startresult[1]*yarr + startresult[2]*yarr^2 + $
            startresult[3]*yarr^3 + startresult[4]*yarr^4 + startresult[5]*yarr^5 + $
            startresult[6]*yarr^6 + startresult[7]*yarr^7,ty)
        begz    = spline(yarr,endresult[0] + endresult[1]*yarr + endresult[2]*yarr^2 + $
            endresult[3]*yarr^3 + endresult[4]*yarr^4 + endresult[5]*yarr^5 + $
            endresult[6]*yarr^6 + endresult[7]*yarr^7,ty)
        END
    ENDCASE

    a=min(abs(startz-thresh),bindex)
    a=min(abs(begz-thresh),eindex)

    stripbeg = ystrips[n].BEGINDEX + ty[bindex]
    stripend = ystrips[n].ENDINDEX + ty[eindex]
    ; ylen = (stripend - stripbeg)/2. + stripbeg
    ylen = mean([[stripend],[stripbeg]])

    IF ylen NE 0. THEN BEGIN
        ysum += ylen 
        ynum += 1
    ENDIF
ENDFOR    

; Get the midpoint of the chords
xpos = xsum/xnum
ypos = ysum/ynum

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
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].BEGINDEX,xtmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/right,charsize=2
    ; device,/close
    ; set_plot,'x'
    window,0
    ; set_plot,'ps'
    ; device,filename=file+'part2'+'.ps',/encapsulated
    plot,xarr+xstrips[wn].ENDINDEX,xstrips[wn].ENDPOINTS,xs=3,ys=3,title='Limb Profile',$
        xtitle='Pixel indices of total strip',ytitle='Brightness',psym=-2,yr=[0,1.1*max(xtmp)]
    oplot,tx+xstrips[wn].ENDINDEX,atmp,linestyle=1
    hline,thresh,linestyle=2
    legend,['Actual Data Values','Splined Data'],linestyle=[0,1],/bottom,/left,charsize=2
    ; device,/close
    ; set_plot,'x'
ENDIF
; Just an aside, but plotting adds ~ .18 seconds to bring it up to .36 seconds total

finish = systime(1,/seconds)
; stop
IF keyword_set(time) THEN  print,'Elapsed Time for comp6: ',strcompress(finish-start,/rem),' seconds'
RETURN
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO cc

restore,'comp4strips.sav',/v
plot,ystrips[5].ARRAY,xs=3,ys=3,/nodata
FOR i=0,n_elements(ystrips) - 1 DO BEGIN
    oplot,ystrips[i].ARRAY,color=i
    ; print,closest((ystrips[i].ARRAY)[0:59],thresh),closest((ystrips[i].ARRAY)[60:119],thresh)
    ; wait,1
ENDFOR   

; It's okay that we have non-zero values for 1st,2nd,and last 2 rows because those aren't counted anyways

stop
END
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO rb,file=file,ministrip_length=ministrip_length,scan_width=scan_width,time=time,order=order, $
    plot=plot,sigmavalue=sigmavalue
;+
; :Description:
;       Finds the centroid using a variety of compression levels
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
;       Sets the threshold to be max(image) - sigmavalue*stddev(image)
;
; :TODO: 
;
;   More Keywords/parameters    
;
;   How strict are we going to be for the outputs of each compression file?
;
;       
;-

IF ~keyword_set(ministrip_length)   THEN    ministrip_length = 9
IF ~keyword_set(scan_width)         THEN    scan_width = 10
IF ~keyword_set(file)               THEN    file = 'gauss1pix.tiff'
IF ~keyword_set(sigmavalue)         THEN    sigmavalue = 1

comp2,xpos,ypos,thresh,time=time
print,'X Center is ',xpos
print,'Y Center is ',ypos
print,'Threshold is ', thresh
print,''
xcs = xpos
ycs = ypos
; comp4,xstrips,ystrips,thresh,scan_width=scan_width
; print,'Threshold is ', thresh
; help,xstrips,/str
; print,''
; comp5,xstrips,ystrips,thresh,scan_width=scan_width,ministrip_length=ministrip_length
; print,'Threshold is ', thresh
; help,xstrips,/str
; print,''
comp6,xpos,ypos,time=time,order=order,scan_width=scan_width,file=file,plot=plot,sigmavalue=sigmavalue
print,'X Center is ',xpos
print,'Y Center is ',ypos
print,''
print,xcs-xpos
print,ycs-ypos
; cropped_image = scanbox(file='Sun_Images_000000.bmp',time=time)
; window,0
; cropped_image[*,ypos] = 0
; cropped_image[xpos,*] = 0
; cgimage,cropped_image,/keep_asp


; cropped_image = scanbox(file='Sun_Images_000000.bmp',time=time)
; window,1
; cropped_image[*,ycs] = 0
; cropped_image[xcs,*] = 0
; cgimage,cropped_image,/keep_asp

; cropped_image = scanbox(file=file,time=time)
; window,0
; cropped_image[*,ypos] = 0
; cropped_image[xpos,*] = 0
; cgimage,cropped_image,/keep_asp


; cropped_image = scanbox(file=file,time=time)
; window,1
; cropped_image[*,ycs] = 0
; cropped_image[xcs,*] = 0
; cgimage,cropped_image,/keep_asp



; From the looks of it, comp6 returns a bad center. why? What could cause the program to shift the
; xpos left by 3 indices?
; I tested it on a perfect sun test image and it works fine... something to do with the bmp image,
; I'm sure.
stop
END
