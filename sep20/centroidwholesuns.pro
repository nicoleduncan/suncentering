PRO centroidwholesuns, inputstruct, inputimage, limbfittedcentroids, best4, fixfid
;+
;   :Description:
;       Finds the centers of a triple-sun image and loads all relevant information
;       including offsets and angles into a new structure.
;
;   :Params:
;       inputstruct : in, required, type=structure
;           Structure containing the centers and cropped images of all 3 suns
;
;       inputimage : in, required, type=byte(ndims,2)
;           Image that we start out with
;
;       limbfittedcentroids : out, required, type=structure
;           Strucutre comtaining all information
;
;       best4 : out, required, type=structure
;           Structure containing positions of best 4 fiducials
;
;       fixfid : out, required, type=structure
;           Structure containing all fiducial positions
;
;   :Keywords:
;-
COMPILE_OPT idl2
ON_ERROR,1

wholesunstruct = inputstruct[where(inputstruct.partial ne 1)]

;If we wanted to use a 13 pixel limb fit instead of a 4 pixel limb fit
; centers = limbfit(wholesunstruct,inputimage)
; else we use

fixfid = para_fid(inputimage,wholesunstruct)
; We do it this way so that we can make sure the limbs don't include fiducials
limbfittedcentroids = npixfit(wholesunstruct,inputimage,fixfid)
; Have to include this here so I don't pass around fixfid again


; fixfid was centered on xpos,ypos, want it to be centered around limbxpos,limbypos
for i = 0,n_elements(fixfid)-1 do begin
    for j = 0,n_elements((*fixfid[i]).fidarr)-1 do begin
        (*fixfid[i]).fidarr[j].x+=limbfittedcentroids[i].xpos-limbfittedcentroids[i].limbxpos
        (*fixfid[i]).fidarr[j].y+=limbfittedcentroids[i].ypos-limbfittedcentroids[i].limbypos
        (*fixfid[i]).fidarr[j].subx+=limbfittedcentroids[i].xpos-limbfittedcentroids[i].limbxpos
        (*fixfid[i]).fidarr[j].suby+=limbfittedcentroids[i].ypos-limbfittedcentroids[i].limbypos
    endfor
endfor

best4 = best4(limbfittedcentroids,fixfid)

; An artifact of before I got in over my head:
; only usable with 3, this is going to be a fair amount of work so I'll leave it here
; theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
; hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
; offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
;     (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

; struct = {KAHUNA, center1:center1, center2:center2, center3:center3, $
;     theta:theta, offset:offset}

END