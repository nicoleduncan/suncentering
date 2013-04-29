FUNCTION auxcrop;, mainxpos, mainypos, image, thresh, xpos, ypos, xoffset, yoffset, region=region, $
     ;time=time

; input the region


; replacing circscancrop because this is so much simpler. 

; we already know the center of the main sun, crop it out
a = findgen(10,10)
; if center is 5,5
b = fltarr(10,10) + 1
b[4:6,4:6] -= 1
newmask = a*b

; work with this image

; thresh = .6*MAX(newmask[0:(1-!param.elim_perc/100)*(N_ELEMENTS(newmask)-1)])

; but instead of .6 it's a parameter

; shoud do a check where if there are no adjacent pixels near a bright pixel, eliminate it, BUT that would be a mean amount of calculation

; quickmask newmask with the above thresh

; the once we find center, crop out that part too, rinse above steps

; Is it a problem that reg 3 needs the coords of reg 2? Nah old code does the same.

; centerx = mainxpos + radius*COS(centerangle)
; centery = mainypos + radius*SIN(centerangle)

; ; This part fails with the bright pixels

; image = wholeimage[centerx - !param.crop_box:centerx + !param.crop_box,$
;     centery - !param.crop_box:centery + !param.crop_box]
; xoffset = centerx- !param.crop_box
; yoffset = centery- !param.crop_box


; output the crop area
; return, 
end