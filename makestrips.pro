FUNCTION makestrips, inputstruct

COMMON vblock, w1_w2_p3
; what do we have to work with here?
; we already have rough centers

; to start, make it work for 1 case

im=w1_w2_p3

for i = 0, N_ELEMENTS(inputstruct)-1 do begin
    crop = im[inputstruct[i].xpos - !param.crop_box : inputstruct[i].xpos + !param.crop_box,$
        inputstruct[i].ypos - !param.crop_box : inputstruct[i].ypos + !param.crop_box]
    for j = 0, !param.nstrips - 1 do begin
        inputstruct[i].xstrips[j].rowindex = j
        inputstruct[i].xstrips[j].array = crop[*,( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width]
        inputstruct[i].ystrips[j].colindex = j
        inputstruct[i].ystrips[j].array = crop[( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width,*]
    endfor
endfor

RETURN,inputstruct
END