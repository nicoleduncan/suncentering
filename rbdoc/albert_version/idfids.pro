function idfids, fidstruct

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


; Assuming that the list is a list of fid chords
a=posscomb(4)

; fidstruct[0].fidarr[0].x,fidstruct[0].fidarr[0].y = 1 fid coord
n=4
i = REBIN(LINDGEN(n), n, n)           
j = REBIN(TRANSPOSE(LINDGEN(n)), n, n)
mask = i gt j

dx = fan(fidstruct[0].fidarr.subx,4)
tdx = transpose(dx)
x = (dx - tdx)^2

dy = fan(fidstruct[0].fidarr.suby,4)
tdy = transpose(dy)
y = (dy - tdy)^2

d = sqrt(x + y) * mask

print,d


; Assuming that the is a list of fid coords:







stop
RETURN, 1s
end