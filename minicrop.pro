PRO minicrop, temparr, rowscan, colscan, rowendscan, colendscan, scan_width,$
        sundiam, thresh,time=time
;+
;   :Description: 
;       Small function to keep code small in cropit. Finds the row where the threshold is crossed
;       and then steps back in the x-direction to find the left side of the cropping box. Skips 
;       over the sun by a predetermined value then crops out the sun. 
;
;   :Params:
;       temparr : in, required, type=byte
;           2D array to check boundary limits of
;       rowscan : out, required, type=integer
;           Where to crop the rows from
;       colscan : out, required, type=integer
;           Where to crop the columns from
;       rowendscan : out, required, type=integer
;           Where to crop the rows to
;       colendscan : out, required, type=integer
;           Where to crop the columns to
;       scan_width : in, required, type=integer, default=5
;           How apart the scans are for minicrop(). 
;       sundiam : in, required, type=byte, default=70
;           Approximate diameter of sun in pixels. (Based on bmp image)
;       temparr : in, required, type=byte
;           2D array to check boundary limits of
;       thresh : in, required, type=float
;           What the minimum threshold will be for scanning
;
;   :Keywords:
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
