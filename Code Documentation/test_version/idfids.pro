function idfids, fidstruct, centerstruct

; So, how do we know which fids are which?

; For each fiducial, assign a combination of distances as a letter. i.e.,

; beta = [A, B, C, D, E, F]
; beta = [0, 0, 0, 0, 0, 0]

; since we know the distances between certain points and which fiducials they belong to, we check if those distances line up with our fiducials

; If the distance between point alpha and beta is l, then we look up the distance l and see which fiducials they correspond to. In the table, l is the distance between fiducials A and D. e go to our matrix and for the beta fiducial, we say that it can be either A or D. Now the array looks like:

; beta = [A, B, C, D, E, F]
; beta = [1, 0, 0, 1, 0, 0]

; Great. Now we look up the distance between beta and phi and find the distance to be d. We look up the distance d and see that it links fiducials D and F. Now the array looks like:

; beta = [A, B, C, D, E, F]
; beta = [1, 0, 0, 2, 0, 1]

; we're incrementing by 1 for each possible fiducial it could be. Technically we can stop at 2 but for robustness we do 3. We look up the distance between beta and omicron and find the distance to be w. w marks the distance between the C and D fiducial. Now the array looks like:

; beta = [A, B, C, D, E, F]
; beta = [1, 0, 1, 3, 0, 0]

; After enough iterations, we deduce beta (which holds no meaning) is fiducial D on the grid. Done.

; THIS IS A REALLY BAD WAY
; THIS IS A REALLY BAD WAY
; THIS IS A REALLY BAD WAY
; THIS IS A REALLY BAD WAY
; THIS IS A REALLY BAD WAY

; ; Assuming that the list is a list of fid chords
; a=posscomb(4)

; ; fidstruct[0].fidarr[0].x,fidstruct[0].fidarr[0].y = 1 fid coord
; n=4
; i = REBIN(LINDGEN(n), n, n)           
; j = REBIN(TRANSPOSE(LINDGEN(n)), n, n)
; mask = i gt j

; dx = fan(fidstruct[0].fidarr.subx,4)
; tdx = transpose(dx)
; x = (dx - tdx)^2

; dy = fan(fidstruct[0].fidarr.suby,4)
; tdy = transpose(dy)
; y = (dy - tdy)^2

; d = sqrt(x + y) * mask

; print,d

; don't do a list of chords, combinations get obscene.








; Assuming that the is a list of fid coords:

; if F1 is a fiducial with positions F1=[x1,y1] and list is a list of known fid positions:

; list =  [x1,y1]
;         [x2,y2]
;         [x3,y3]
;         [x4,y4]
;         etc

; Then the closest fiducial to F1 is found with:

; sqrt((list[0,*] - F1[0])^2+(list[1,*] - F1[1])^2)

; f1=[2,3]



; newx = subx + centerstruct[0].limbxpos - !param.crop_box


readcol,'fidlist.txt',id,xpos,ypos,format='A,F,F',delimiter=' '
fidl = CREATE_STRUCT('id',id[0],'xpos',xpos[0],'ypos',ypos[0]) 
for i=1,N_ELEMENTS(id)-1 do begin
    fidl=[fidl,{id:id[i],xpos:xpos[i],ypos:ypos[i]}]
endfor

flist = [reform(fidl.xpos,1,6),reform(fidl.ypos,1,6)]
ourflist = [reform(fidstruct.fidarr.subx,1,4),reform(fidstruct.fidarr.suby,1,4)]

for i = 0,3 do begin
    f1 = ourflist[*,i]
    a=sqrt((flist[0,*] - F1[0])^2+(flist[1,*] - F1[1])^2)
    fidstruct.fidarr[i].id = (fidl.id)[where(a eq min(a))]
endfor








; so actually

; the list can't be from a point selected from taking a picture since that'll awlays change, the only thing that won't change is the ditance between fiducials. So if I can convert the distance between fiducials to a grid, then we're okay.  


stop
RETURN, fidstruct
end