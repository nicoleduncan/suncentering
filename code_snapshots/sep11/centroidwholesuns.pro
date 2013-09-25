FUNCTION centroidwholesuns, inputstruct, inputimage, fixfid
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
;       fixfid : in, optional
;           If set, marks the second run of centroidwholesuns and attempts to fix any chord slicing if the chord edge crosses a fiducial.
;
;   :Keywords:
;-
COMPILE_OPT idl2
ON_ERROR,1

wholesunstruct = inputstruct[where(inputstruct.partial ne 1)]

;If we wanted to use a 13 pixel limb fit instead of a 4 pixel limb fit
; centers = limbfit(wholesunstruct,inputimage)
; else we use
centers = npixfit(wholesunstruct,inputimage,fixfid)


; An artifact of before I got in over my head:
; only usable with 3, this is going to be a fair amount of work so I'll leave it here
; theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
; hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
; offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
;     (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

; struct = {KAHUNA, center1:center1, center2:center2, center3:center3, $
;     theta:theta, offset:offset}

RETURN,centers
END