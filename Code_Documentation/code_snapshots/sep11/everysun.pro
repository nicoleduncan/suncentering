FUNCTION everysun, input
;+
;   :Description:
;       Finds the rough centroiding centers of any and all suns
;
;   :Params:
;       input: in, required,type=byte(ndims,2)
;           The raw input image
;
;   :Keywords:
;       
;-
; Why do I need to do this?
temparr = input
s = SIZE(temparr,/d)
n_col = s[0]
n_row = s[1]
; don't want to run this again, but alas...
idedsuns = idsuns(temparr)
n_suns = N_ELEMENTS(idedsuns)

; Making as much of our structure in one place as possible, the one parameter we can't include here are the number of fiducials we find. Well, at leat in the current iteration. Future versions may have a preallocated szze of, say, 10 fiducials and we only fill up however many we actually need.

; These strips of data span the entire width/length of the cropped solar image.
xstrips = REPLICATE({wholexstrips,rowwhere:BYTARR(2), array:BYTARR(2* !param.crop_box + 1)}, !param.nstrips)
ystrips = REPLICATE({wholeystrips,colwhere:BYTARR(2), array:BYTARR(2* !param.crop_box + 1)}, !param.nstrips)

; There is a pair of strips, the start points as well as the end points, for each strip above.
limbxstrips = REPLICATE({limbxstrips, rowwhere:BYTARR(2), begindex:0, endindex:0, startpoints:BYTARR( !param.ministrip_length), endpoints:BYTARR( !param.ministrip_length), $
    startloc:BYTARR( !param.ministrip_length), endloc:BYTARR( !param.ministrip_length)}, !param.nstrips)
limbystrips = REPLICATE({limbystrips, colwhere:BYTARR(2), begindex:0, endindex:0, startpoints:BYTARR( !param.ministrip_length), endpoints:BYTARR( !param.ministrip_length), $
    startloc:BYTARR( !param.ministrip_length), endloc:BYTARR( !param.ministrip_length)}, !param.nstrips)


; Doesn't matter what we call it, but this is the more-or-less complete structure we fill
asun = REPLICATE({xpos:0.,ypos:0.,reg:0,thresh:0.,partial:0.,xstrips:xstrips,ystrips:ystrips,limbxstrips:limbxstrips,limbystrips:limbystrips,limbxpos:0.,limbypos:0.,npix:0.},n_suns)

for i = 0,n_suns-1 do begin
    asun[i].reg = idedsuns[WHERE(idedsuns eq MIN(idedsuns))]

    ; Match up threshold with region
    case asun[i].reg of
        1: asun[i].thresh = !thresh.reg1
        2: asun[i].thresh = !thresh.reg2
        3: asun[i].thresh = !thresh.reg3
    endcase
    
    ; Use a simple centroiding method to find the center of an image above a threshold    
    gig = quickmask(temparr, asun[i].thresh)
    asun[i].xpos = gig.xpos
    asun[i].ypos = gig.ypos

    ; How many pixels are above the threshold? At one point we wanted to know this    
    asun[i].npix = N_ELEMENTS(temparr[WHERE(temparr gt asun[i].thresh)])


    ; If the cropped region would contain out-of-image pixels, then use a special method. Otherwise, just crop between boundaries
    if gig.xpos - !param.crop_box lt 0 or gig.ypos - !param.crop_box lt 0 or gig.xpos + !param.crop_box gt s[0] or gig.ypos + !param.crop_box gt s[1] then begin

        ; Setting up the positions of all pixels within this cropped box
        xarr = findgen(2 * !param.crop_box + 1) + (gig.xpos - !param.crop_box)
        yarr = findgen(2 * !param.crop_box + 1) + (gig.ypos - !param.crop_box)

        ; Eliminating positions that lie outside the image
        xarr = xarr[where(xarr ge 0 and xarr le s[0])]
        yarr = yarr[where(yarr ge 0 and yarr le s[1])]

        ; Converting from x,y positions into a single index - don't have to use a nested for-loop this way
        loc = fan(xarr) + rebin(reform(floor(yarr)*s[0],1,n_elements(xarr)),n_elements(xarr),n_elements(xarr))
        temparr[loc[*]]=0

        idedsuns[WHERE(idedsuns eq MIN(idedsuns))] = 999
    endif else begin
        temparr[gig.xpos - !param.crop_box:gig.xpos + !param.crop_box, gig.ypos - !param.crop_box:gig.ypos + !param.crop_box]=0
        idedsuns[WHERE(idedsuns eq MIN(idedsuns))] = 999
    endelse


    ; So this was the old way, and it's actually the same speed as my new code. 

    ; Need to pad the image because after each center we find, we zero that part of the image out so that the next maximum in the image corresponds to the next lower region number
    ; How much to pad the edges of the image by. As a result, cropped box cannot exceed sidepad*2+1 in width/height
    ; sidepad = 80
    ; ; Make new array with sidepad
    ; paddedimage = BYTARR(s+sidepad*2)
    ; ; Fill blank array with our array
    ; paddedimage[sidepad,sidepad]=temparr
    ; ; Zero out region 
    ; paddedimage[gig.xpos+sidepad - !param.crop_box:gig.xpos+sidepad + !param.crop_box, gig.ypos+sidepad - !param.crop_box:gig.ypos+sidepad + !param.crop_box]=0
    ; ; Unpad image
    ; temparr = paddedimage[sidepad:s[0]+sidepad-1,sidepad:s[1]+sidepad-1]
    ; ; or set it to 999, that's basically like 0
    ; idedsuns[WHERE(idedsuns eq MIN(idedsuns))] = 999
    
endfor

RETURN,asun
end