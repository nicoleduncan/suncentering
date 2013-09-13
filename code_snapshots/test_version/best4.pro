FUNCTION best4, inputstruct, fidstruct
;+
;   :Description:
;       Chooses best 4 fiducials (closest to disk center)
;
;   :Params:
;       inputstruct: in, required, type=structure
;           Structure containing all the solar information
;       fidstruct: in, required, type=structure
;           Structure containing all the solar information
;
;   :Keywords:
;       
;-

distances= !null
nsuns = n_elements(inputstruct)
; Since we're only interested in the best -4-, safe to hardcode the 4 elements of this structure
fidarr = REPLICATE({x:0.,y:0.,subx:0.,suby:0.,id:''},4)
best4 = REPLICATE({fidarr:fidarr,reg:0},nsuns)

for i=0,nsuns-1 do begin
    nfid = N_ELEMENTS(((*(fidstruct[i])).fidarr.subx)[where((*(fidstruct[i])).fidarr.subx) ne 0])
    for j = 0,nfid-1 do begin
        ; For each fiducial, find distance to center of sun
        a = howfar([((*(fidstruct[i])).fidarr)[j].subx +  inputstruct[i].limbxpos - !param.crop_box,((*(fidstruct[i])).fidarr)[j].suby +  inputstruct[i].limbypos - !param.crop_box],[inputstruct[i].limbxpos,inputstruct[i].limbypos])
        ; Populate array with distances
        distances=[distances,a]
    endfor
    ; Sort by closest distance, grab lowest 4
    grabfrom = (((*(fidstruct[i])).fidarr)[sort(distances)])[0:3]
    best4[i].reg = inputstruct[i].reg
    for k =0,3 do begin
        best4[i].fidarr[k].x = grabfrom[k].x
        best4[i].fidarr[k].y = grabfrom[k].y
        best4[i].fidarr[k].subx = grabfrom[k].subx
        best4[i].fidarr[k].suby = grabfrom[k].suby
    endfor
    distances= !null
endfor

RETURN,best4
end