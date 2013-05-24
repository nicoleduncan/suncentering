FUNCTION betterwhichcropmethod, region
;+
;   :Description:
;       Crops differently according to which region is selected. 
;
;   :Params:
;       region : in, required, type=integer
;           1) main sun
;           2) 50% brightness sun
;           3) 25% brightness sun
;-
COMMON vblock, wholeimage
input = wholeimage

a=setbetterthresh(input)
b=quickmask(input,a.thresh100)
; why am I calling ts_smooth 6 times? Should only be 2
; 1 for each call of limbfit

image = wholeimage[b.xpos - !param.crop_box:b.xpos + !param.crop_box, $
    b.ypos - !param.crop_box:b.ypos + !param.crop_box]
xoffset = b.xpos - !param.crop_box
yoffset = b.ypos - !param.crop_box
thresh = a.thresh100
IF REGION NE 1 THEN BEGIN
    input[b.xpos - !param.crop_box:b.xpos + !param.crop_box,$
        b.ypos - !param.crop_box:b.ypos + !param.crop_box] = 0
    c=quickmask(input,a.thresh50)
    image = wholeimage[c.xpos - !param.crop_box:c.xpos + !param.crop_box, $
    c.ypos - !param.crop_box:c.ypos + !param.crop_box]
    xoffset = c.xpos - !param.crop_box
    yoffset = c.ypos - !param.crop_box
    thresh = a.thresh50
    if REGION EQ 3 THEN BEGIN
        input[c.xpos - !param.crop_box:c.xpos + !param.crop_box,$
            c.ypos - !param.crop_box:c.ypos + !param.crop_box] = 0
        d=quickmask(input,a.thresh25)
        image = wholeimage[d.xpos - !param.crop_box:d.xpos + !param.crop_box, $
            d.ypos - !param.crop_box:d.ypos + !param.crop_box]
        xoffset = d.xpos - !param.crop_box
        yoffset = d.ypos - !param.crop_box
        thresh = a.thresh25
    endif
ENDIF


RETURN,{image:image, xoffset:xoffset, yoffset:yoffset, thresh:thresh}
END