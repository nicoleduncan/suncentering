FUNCTION quickmask, input_image, thresh
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

; input_image = FLOAT(input_image)
a = input_image[BSORT(input_image)]

s = SIZE(input_image,/dimensions)
n_col = s[0]
n_row = s[1]

suncheck = input_image gt thresh
xpos = TOTAL( TOTAL(suncheck, 2) * INDGEN(n_col) ) / TOTAL(suncheck)
ypos = TOTAL( TOTAL(suncheck, 1) * INDGEN(n_row) ) / TOTAL(suncheck)

RETURN, {xpos:xpos,ypos:ypos}
END