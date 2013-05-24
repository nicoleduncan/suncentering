;+
; PROJECT:
;       HESSI
; NAME:
;   PARADIP
;
; PURPOSE:
;   Given a 3x3 array, does parabolic fits to determine position and value of dip. Different from PARAPEAK in that this computes the fit of the dip and not a peak. I don't like having to normalie my data so I'd rather do it this way.
;
; CATEGORY:
;   IMAGE
;
; CALLING SEQUENCE:
;   Result = paradip (z, err_msg=err_msg)
;
; INPUTS:
;   z - (3,3) array of image values.  Middle value should be highest, corners lowest.
;
; OUTPUT KEYWORDS:
;   err_msg - '' if no error, otherwise contains error message
;
; OUTPUT:
;   Returns a 3-element vector containing the x,y coordinates of the peak (relative to the middle
;     of the central pixel) in units of pixel size and the value at the inferred peak.
;
; PROCEDURE:
;   Given a 3x3 array in which the corner values are lower than any intermediate value,
;   PARADIP assumes that these correspond to the top of a 2-D circular gaussian; does
;   a pair of 1-D parabolic fits and returns the 2-D peak location and value.
;   The 3x3 array supposedly represents the function z at x=0,1,2 and y=0,1,2
;
;   Note that there is considerable redundancy in the data which has not been exploited.
;   An alternate technique would be to use just the 5 values in the '+' shaped configuration.
;
; COMMON BLOCKS:
;   None.
;-

FUNCTION paradip, z, err_msg=err_msg

err_msg = ''

sz = size(z)
if not same_data(sz[0:2], [2L,3,3]) then begin
    err_msg = 'PARADIP: syntax - result = paradip, z, err_msg=err_msg.  z must be [3,3] array.'
    return, [-1,-1,-1]
endif

; first confirm that center point is lowest.  Otherwise can't work.
if min(z[4] - z[[0,1,2,3,5,6,7,8]]) lt 0. then begin

  x       = FLTARR(3)
  zx      = FLTARR(3)

  if product(2.*z[1,*]-z[0,*]-z[2,*]) ne 0. then begin
    x[*]    = 0.5*(z[2,*]-z[0,*]) / (2.*z[1,*]-z[0,*]-z[2,*])
    zx[*]   = z[1,*] + 0.25*x[*] * (z[2,*]-z[0,*])
   ; PRINT, Z
   ; PRINT, X
   ; PRINT, ZX
    xpk     = MEAN(x)
    if (2.*zx[1]-zx[0]-zx[2]) ne 0. then begin
;    print,x,ABS(2*x[1] - x[0] - x[2]) 
        IF ABS(2*x[1] - x[0] - x[2]) LT 1. THEN BEGIN               ; test on row peak locations
            ypk     = 0.5*(zx[2]-zx[0]) / (2.*zx[1]-zx[0]-zx[2])
            zpk     = zx[1] + 0.25*ypk * (zx[2]-zx[0])
            RETURN,[xpk,ypk,zpk]
        ENDIF
    endif
  endif
endif

err_msg = 'PARADIP:  Can not compute parabolic fits to this data. Aborting.'
return, [-1,-1,-1]
END