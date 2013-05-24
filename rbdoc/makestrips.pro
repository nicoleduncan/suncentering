FUNCTION makestrips, inputstruct, inputimage
;+
;   :Description:
;       Makes strips using approx centroiding method to make cropped areas
;
;   :Params:
;       inputstruct: in, required
;           Structure containing all the solar information
;
;       inputimage: in, required
;           The raw input image
;
;-

im=inputimage

for i = 0, N_ELEMENTS(inputstruct)-1 do begin
    s = SIZE(inputimage,/dim)
    ; If center is too close to edge, pad it
    if inputstruct[i].xpos + !param.crop_box gt s[0] or $
        inputstruct[i].ypos + !param.crop_box gt s[1] or $
        inputstruct[i].xpos - !param.crop_box lt 0 or $
        inputstruct[i].ypos - !param.crop_box lt 0  then begin
        
        sidepad = !param.crop_box
        paddedimage = BYTARR(s+sidepad*2) + MODE(inputimage)
        paddedimage[sidepad,sidepad]=inputimage

        crop = paddedimage[inputstruct[i].xpos - !param.crop_box + sidepad : inputstruct[i].xpos+ !param.crop_box + sidepad, inputstruct[i].ypos - !param.crop_box + sidepad : inputstruct[i].ypos + !param.crop_box + sidepad]

        for j = 0, !param.nstrips - 1 do begin
            inputstruct[i].xstrips[j].rowindex = j
            inputstruct[i].xstrips[j].array = REFORM(crop[*,( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width])
            inputstruct[i].ystrips[j].colindex = j
            inputstruct[i].ystrips[j].array = REFORM(crop[( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width,*])
        endfor

    endif else begin
        crop = im[inputstruct[i].xpos - !param.crop_box : inputstruct[i].xpos + !param.crop_box,inputstruct[i].ypos - !param.crop_box : inputstruct[i].ypos + !param.crop_box]
        for j = 0, !param.nstrips - 1 do begin
            inputstruct[i].xstrips[j].rowindex = j
            inputstruct[i].xstrips[j].array = REFORM(crop[*,( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width])
            inputstruct[i].ystrips[j].colindex = j
            inputstruct[i].ystrips[j].array = REFORM(crop[( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width,*])
        endfor
    endelse
endfor

RETURN,inputstruct
END