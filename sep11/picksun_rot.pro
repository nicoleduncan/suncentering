FUNCTION picksun_rot, inputimage, inputstruct
;+
;   :Description:
;       Decides which suns to ignore. Checks if the center coordinates of the sun are within certain boundaries. To deal with the bottom corners, coordinates are rotated 45 degrees.
;
;   :Params:
;       inputimage: in, required
;           The raw input image
;
;       inputstruct: in, required, type=structure
;           Structure containing all the solar information
;
;   :Keywords:
;
;   :TODO:
;       Correct border checking distance in rotated space
;-

s = SIZE(inputimage,/d)

for i = 0,N_ELEMENTS(inputstruct)-1 do begin
    x0=inputstruct[i].xpos
    y0=inputstruct[i].ypos

    ; im = inputimage

    ; im[x0-1:x0+1,y0-1:y0+1] = !red
    ; rim = rot(im,-45)
    ; window,0
    ; cgimage,im,/k
    ; window,1
    ; cgimage,rim,/k

    ; spot = where(rim eq !red)

    ; xx = mean(spot mod s[0])
    ; yy = mean(spot / s[0])

    ; print,"according to rot(), center is at"
    ; print,xx,yy

    ; Center of rotation
    xc = (s[0])/2.
    yc = (s[1])/2.

    ; distance of sun from center
    r = SQRT((xc-x0)^2 + (yc-y0)^2)

    ; Since we're taking ATAN(), need to make sure we add !pi correctly
    if x0 gt xc then offset = 0 else begin
        if y0 gt yc then offset = 180*!dtor
        if y0 lt yc then offset = -180*!dtor
    endelse

    theta = ATAN(ABS(yc-y0),ABS(xc-x0))+ offset
    ; Now we know polar coordinates of solar center. Now we rotate them by 45 Degrees to see how close to the border they get

    twist = 45*!dtor
    newx = r*COS((theta + twist))
    newy = r*SIN((theta + twist))
    xpos = newx+xc
    ypos = newy+yc


    ; print,x0,y0
    ; print,''
    ; print,'according to my function, center is at'
    ; print,xpos,ypos

    ; If the solar center is too close to the edge in unrotated space or rotated space, it's bad.
    if x0 lt ( !param.bordercheck_perc/100)*s[0] or x0 gt (1 - !param.bordercheck_perc/100)*s[0] or y0 lt ( !param.bordercheck_perc/100)*s[1] or y0 gt (1- !param.bordercheck_perc/100)*s[1] or xpos lt 0 or ypos lt 0 then inputstruct[i].partial = 1
endfor

RETURN,inputstruct
end