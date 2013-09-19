FUNCTION makestrips, inputstruct, inputimage, fixfid
;+
;   :Description:
;       Makes strips using approx centroiding method to make cropped areas
;
;   :Params:
;       inputstruct: in, required,type=structure
;           Structure containing all the solar information
;
;       inputimage: in, required,type=structure
;           The raw input image
;
;       fixfid : in, optional
;           If set, marks the second run of centroidwholesuns and attempts to fix any chord slicing if the chord edge crosses a fiducial.
;
;   :Keywords:
;-

im=inputimage
s = SIZE(inputimage,/dim)

for i = 0, N_ELEMENTS(inputstruct)-1 do begin
    ; If center is too close to edge, pad it
    if inputstruct[i].xpos + !param.crop_box gt s[0] or inputstruct[i].ypos + !param.crop_box gt s[1] or inputstruct[i].xpos - !param.crop_box lt 0 or inputstruct[i].ypos - !param.crop_box lt 0  then begin
        
        paddedimage = BYTARR(s+ !param.crop_box*2) + MODE(inputimage)
        paddedimage[ !param.crop_box, !param.crop_box]=inputimage

        crop = paddedimage[inputstruct[i].xpos : inputstruct[i].xpos + 2* !param.crop_box, inputstruct[i].ypos : inputstruct[i].ypos + 2* !param.crop_box]
        
        for j = 0, !param.nstrips - 1 do begin
        ;This part hasn't been updated to work with chord thicknessses
            inputstruct[i].xstrips[j].rowwhere = j
            ; inputstruct[i].xstrips[j].array = REFORM(crop[*,( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width])
            inputstruct[i].xstrips[j].array = (crop[*,( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width])
            inputstruct[i].ystrips[j].colwhere = j
            ; inputstruct[i].ystrips[j].array = REFORM(crop[( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width,*])
            inputstruct[i].ystrips[j].array = (crop[( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width,*])
        endfor
    endif else begin
    ; Otherwise, crop it normally
        crop = im[inputstruct[i].xpos - !param.crop_box : inputstruct[i].xpos + !param.crop_box,inputstruct[i].ypos - !param.crop_box : inputstruct[i].ypos + !param.crop_box]
        
        for j = 0, !param.nstrips - 1 do begin
        ; stop
            inputstruct[i].xstrips[j].rowwhere = [ !param.crop_box + (j - !param.nstrips/2) * !param.scan_width, !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1]
            ; Using an average of chords now, defined by !param.chord_thickness

            ; inputstruct[i].xstrips[j].array = REFORM(MEAN(crop[*,( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width: !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1],dim=2))
            inputstruct[i].xstrips[j].array = (MEAN(crop[*,( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width: !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1],dim=2))
            
            inputstruct[i].ystrips[j].colwhere = [ !param.crop_box + (j - !param.nstrips/2) * !param.scan_width, !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1]
            ; inputstruct[i].ystrips[j].array = REFORM(MEAN(crop[( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width: !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1,*],dim=1))
            inputstruct[i].ystrips[j].array = (MEAN(crop[( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width: !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1,*],dim=1))
            ; stop
        endfor
    endelse
endfor

if n_elements(fixfid) ne 0 then begin
    ; Check each row for fiducials
    for i = 0, N_ELEMENTS(inputstruct)-1 do begin
        for j = 0, !param.nstrips - 1 do begin
            for k = 0,n_elements((*fixfid[0]).fidarr) - 1 do begin
                    ; !param.fidarm is to make sure that no part of the fiducial is in the part we care about, not even the fiducial arm. 
                if (((*fixfid[0]).fidarr).subx)[k] gt (inputstruct[i].limbxstrips[j].startloc)[0] - !param.fidarm and (((*fixfid[0]).fidarr).subx)[k] lt (inputstruct[i].limbxstrips[j].startloc)[0] + !param.ministrip_length - 1 + !param.fidarm and (((*fixfid[0]).fidarr).suby)[k] gt (inputstruct[i].limbxstrips[j].rowwhere)[0] - !param.fidarm and (((*fixfid[0]).fidarr).suby)[k] lt (inputstruct[i].limbxstrips[j].rowwhere)[1] + !param.fidarm then begin

                    print,(((*fixfid[0]).fidarr).subx)[k]
                    print,' is between'
                    print, (inputstruct[i].limbxstrips[j].startloc)[0]
                    print,'and'
                    print, (inputstruct[i].limbxstrips[j].startloc)[0] + !param.ministrip_length - 1
                    print,' in addition '
                    print,(((*fixfid[0]).fidarr).suby)[k]
                    print,' is between'
                    print, (inputstruct[i].limbxstrips[j].rowwhere)[0]
                    print,'and'
                    print, (inputstruct[i].limbxstrips[j].rowwhere)[1]

                    ; make new chord -> check new chord
                    ; or
                    ; make new chord and include chord check
                    ; or
                    ; make new chord -> don't check since we know there aren't any nearby fiducials

                endif
            endfor
            ; print, inputstruct[i].xstrips[j].rowwhere
            ; print, inputstruct[i].limbxstrips[j].startpoints
        endfor
        ; stop
    endfor
endif
stop
RETURN,inputstruct
end