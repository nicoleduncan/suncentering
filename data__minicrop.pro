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

finish = systime(1,/s)
IF keyword_set(time) THEN print,' Elapsed time for minicrop(): '+strcompress(finish-start,/remove)+' seconds'
RETURN,{minicropped,rowscan:rowscan-2, colscan:colscan-2, rowendscan:rowendscan+2, colendscan:colendscan+2}
END
