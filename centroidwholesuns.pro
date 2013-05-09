FUNCTION centroidwholesuns, inputstruct
;+
;   :Description:
;       Finds the centers of a triple-sun image and loads all relevant information
;       including offsets and angles into a new structure.
;
;   :Params:
;       struct : out, required, type=structure
;           Structure containing the centers and cropped images of all 3 suns
;
;   :Keywords:
;       time: in, optional
;           Outputs how much time the program takes
;-
COMPILE_OPT idl2
ON_ERROR,1

COMMON vblock, w1_w2_p3

; here we make the eexecutive decision to trim out input structure
; do we keep the parts where it's partial?
wholesunstruct = inputstruct[where(inputstruct.partial ne 1)]

centers = limbfit(wholesunstruct)


; only usable with 3, this is going to be a fair amount of work so I'll leave it here
; theta = !radeg*atan((center3.ypos - center2.ypos)/(center3.xpos - center2.xpos))
; hypot = sqrt((center3.ypos - center2.ypos)^2 + (center3.xpos - center2.xpos)^2)
; offset = ((center1.xpos - center2.xpos)*(center3.ypos - center2.ypos) - $
;     (center1.ypos - center2.ypos)*(center3.xpos - center2.xpos))/hypot

; struct = {KAHUNA, center1:center1, center2:center2, center3:center3, $
;     theta:theta, offset:offset}

RETURN,centers
END