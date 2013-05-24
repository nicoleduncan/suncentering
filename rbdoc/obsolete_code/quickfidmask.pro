FUNCTION quickfidmask, input_image, thresh
;+
;   :Description:
;       Finds center of mask where pixels are above a given threshold
;
;   :Params:
;       input_image : in, required, type=byte
;           2D array of pixels to mask with threshold
;       thresh : in, required, type=float
;           Threshold used to select pixels
;-

input_image = FLOAT(input_image)
a = input_image[SORT(input_image)]
niceimage = a[0:(1-!param.elim_perc/100)*(N_ELEMENTS(a)-1)]
; Eliminating the highest 1% of data
IF thresh eq !null then thresh = !param.reg1thresh*MAX(niceimage)
; IF n_elements(thresh) EQ 0 THEN thresh = 0.25*MAX(image)

s = SIZE(input_image,/dimensions)
n_col = s[0]
n_row = s[1]

suncheck = input_image lt thresh
xpos = TOTAL( TOTAL(suncheck, 2) * INDGEN(n_col) ) / TOTAL(suncheck)
ypos = TOTAL( TOTAL(suncheck, 1) * INDGEN(n_row) ) / TOTAL(suncheck)

RETURN, {xpos:xpos,ypos:ypos}
END
