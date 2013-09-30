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
;       fixfid : in, required, type=structure
;           Strucutre comtaining the fiducial positions to be used for chord checking.
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

            ; Kind of want to also make limb strips here, but feel like I should separate the functions
            inputstruct[i].xstrips[j].array = MEAN(crop[*,( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width: !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1],dim=2) 
            inputstruct[i].ystrips[j].array = MEAN(crop[( !param.crop_box)+(j - !param.nstrips/2) * !param.scan_width: !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1,*],dim=1)

            ; For each long chord, look at where limb occurs and make note of position
            xlimbinfo = micro_makelimbstrips(inputstruct[i].xstrips[j].array,inputstruct[i].thresh)
            ylimbinfo = micro_makelimbstrips(inputstruct[i].ystrips[j].array,inputstruct[i].thresh)

            ; What is the position of the chord relative to the image
            rowpos = [ !param.crop_box + (j - !param.nstrips/2) * !param.scan_width, !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1]
            inputstruct[i].xstrips[j].rowwhere = rowpos
            inputstruct[i].ystrips[j].colwhere = rowpos
            
            ; Just a marker of where the chord is
            inputstruct[i].limbxstrips[j].rowwhere = rowpos
            inputstruct[i].limbystrips[j].colwhere = rowpos

            ; For each fiducial in our list, check to make sure NO part of the fiducial is in the pixels used for limb-fitting. That includes not only the fiducial center pixel, but the four arms that extend outward.
            for k = 0,n_elements((*fixfid[0]).fidarr) - 1 do begin
                if (((*fixfid[0]).fidarr).subx)[k] gt xlimbinfo.startloc[0] - !param.fidarm and (((*fixfid[0]).fidarr).subx)[k] lt xlimbinfo.startloc[0] + !param.ministrip_length - 1 + !param.fidarm and (((*fixfid[0]).fidarr).suby)[k] gt rowpos[0] - !param.fidarm and (((*fixfid[0]).fidarr).suby)[k] lt rowpos[1] + !param.fidarm then xlimbinfo.isitbad=1
                if (((*fixfid[0]).fidarr).suby)[k] gt ylimbinfo.startloc[0] - !param.fidarm and (((*fixfid[0]).fidarr).suby)[k] lt ylimbinfo.startloc[0] + !param.ministrip_length - 1 + !param.fidarm and (((*fixfid[0]).fidarr).subx)[k] gt rowpos[0] - !param.fidarm and (((*fixfid[0]).fidarr).subx)[k] lt rowpos[1] + !param.fidarm then ylimbinfo.isitbad=1
            endfor

            ; Once we look at all fiducials, stick results into our main array
            copy_limb_struct,inputstruct,xlimbinfo,ylimbinfo
        endfor
        for k = 0, !param.nstrips - 1 do begin
            if inputstruct[i].limbxstrips[k].isitbad eq 1 then begin
                ; is the chord one of the first 2? If so, make new chord below. If not, make new chord above.
                downcount = 0
                upcount = !paran.nstrips-1

                ; If the chord is one of the first two, the location of the new chord will be scan_width below the bottom-most chord
                if k lt 2 then begin
                    xlimbinfo = micro_makelimbstrips(MEAN(crop[*,( !param.crop_box)+(downcount - 1 - !param.nstrips/2) * !param.scan_width: !param.crop_box + (downcount - 1  - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1],dim=2),inputstruct[i].thresh)
                    downcount--
                endif else begin
                ; If the chord is elsewhere, the new chord will be scan_width above the top-most chord
                    xlimbinfo = micro_makelimbstrips(MEAN(crop[*,( !param.crop_box)+(upcount - 1 - !param.nstrips/2) * !param.scan_width: !param.crop_box + (upcount - 1  - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1],dim=2),inputstruct[i].thresh)
                    upcount++
                endelse

                ; Repeat the steps above to see if there is a fiducial in the newly drawn chord
                xlimbinfo = micro_makelimbstrips(inputstruct[i].xstrips[k].array,inputstruct[i].thresh)
                ylimbinfo = micro_makelimbstrips(inputstruct[i].ystrips[k].array,inputstruct[i].thresh)

                rowpos = [ !param.crop_box + (j - !param.nstrips/2) * !param.scan_width, !param.crop_box + (j - !param.nstrips/2) * !param.scan_width + !param.chord_thickness - 1]
                inputstruct[i].xstrips[k].rowwhere = rowpos
                inputstruct[i].ystrips[k].colwhere = rowpos
                
                inputstruct[i].limbxstrips[k].rowwhere = rowpos
                inputstruct[i].limbystrips[k].colwhere = rowpos

                for k = 0,n_elements((*fixfid[0]).fidarr) - 1 do begin
                    if (((*fixfid[0]).fidarr).subx)[k] gt xlimbinfo.startloc[0] - !param.fidarm and (((*fixfid[0]).fidarr).subx)[k] lt xlimbinfo.startloc[0] + !param.ministrip_length - 1 + !param.fidarm and (((*fixfid[0]).fidarr).suby)[k] gt rowpos[0] - !param.fidarm and (((*fixfid[0]).fidarr).suby)[k] lt rowpos[1] + !param.fidarm then xlimbinfo.isitbad=1
                    if (((*fixfid[0]).fidarr).suby)[k] gt ylimbinfo.startloc[0] - !param.fidarm and (((*fixfid[0]).fidarr).suby)[k] lt ylimbinfo.startloc[0] + !param.ministrip_length - 1 + !param.fidarm and (((*fixfid[0]).fidarr).subx)[k] gt rowpos[0] - !param.fidarm and (((*fixfid[0]).fidarr).subx)[k] lt rowpos[1] + !param.fidarm then ylimbinfo.isitbad=1
                endfor

                The problem here is that if the new chord has a fiducial, we will have to repeat the process again. Either I write this as a recursive function or 
            endif
        endfor
    endelse
endfor

RETURN,inputstruct
end