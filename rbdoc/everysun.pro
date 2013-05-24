FUNCTION everysun, input
;+
;   :Description:
;       Finds the rough centroiding centers of any and all suns
;
;   :Params:
;       input: in, required
;           The raw input image
;
;   :Keywords:
;       
;-

temparr = input
s = SIZE(temparr,/d)
n_col = s[0]
n_row = s[1]
idedsuns = idsuns(temparr)
n_suns = N_ELEMENTS(idedsuns)

; xstrips = REPLICATE({wholexstrips,rowindex:0, array:BYTARR(n_col)}, !param.nstrips)
; ystrips = REPLICATE({wholeystrips,colindex:0, array:BYTARR(n_row)}, !param.nstrips)

; Making as much of our structure in one place as possible, the one parameter we can't include here are the number of fiducials we find. Well, at leat in the current iteration. Future versions may have a preallocated szze of, say, 10 fiducials and we only fill up however many we actually need.

; These strips of data span the entire width/length of the cropped solar image.
xstrips = REPLICATE({wholexstrips,rowindex:0, array:BYTARR(2* !param.crop_box + 1)}, !param.nstrips)
ystrips = REPLICATE({wholeystrips,colindex:0, array:BYTARR(2* !param.crop_box + 1)}, !param.nstrips)

; There is a pair of strips, the start points as well as the end points, for each strip above.
limbxstrips = REPLICATE({limbxstrips, rowindex:0, begindex:0, endindex:0, $
        startpoints:BYTARR( !param.ministrip_length), $
        endpoints:BYTARR( !param.ministrip_length)}, !param.nstrips)
limbystrips = REPLICATE({limbystrips, colindex:0, begindex:0, endindex:0, $
        startpoints:BYTARR( !param.ministrip_length), $
        endpoints:BYTARR( !param.ministrip_length)}, !param.nstrips)

; Doesn't matter what we call it, but this is the more-or-less complete structure we fill
asun = REPLICATE({xpos:0.,ypos:0.,reg:0,thresh:0.,partial:0.,xstrips:xstrips,ystrips:ystrips,limbxstrips:limbxstrips,limbystrips:limbystrips,limbxpos:0.,limbypos:0.,fidlocations:{blank:0.}},n_suns)

for i = 0,n_suns-1 do BEGIN
    asun[i].reg = idedsuns[WHERE(idedsuns eq MIN(idedsuns))]

    case asun[i].reg of
        1: asun[i].thresh = !thresh.reg1
        2: asun[i].thresh = !thresh.reg2
        3: asun[i].thresh = !thresh.reg3
    endcase
    
    ; Use a simple centroiding method to find the center of an image above a threshold    
    gig = quickmask(temparr, asun[i].thresh)
    asun[i].xpos = gig.xpos
    asun[i].ypos = gig.ypos
    
    ; Need to pad the image because after for each center we find, we zero that part of the image out so that the next maximum in the image corresponds to the next lower region number

    sidepad = 80
    s = SIZE(temparr,/d)
    paddedimage = BYTARR(s+sidepad*2)
    paddedimage[sidepad,sidepad]=temparr
    paddedimage[gig.xpos+sidepad - !param.crop_box:gig.xpos+sidepad + !param.crop_box, gig.ypos+sidepad - !param.crop_box:gig.ypos+sidepad + !param.crop_box]=0
    temparr = paddedimage[sidepad:s[0]+sidepad-1,sidepad:s[1]+sidepad-1]

    ; or set it to 999, that's basically like 0
    idedsuns[WHERE(idedsuns eq MIN(idedsuns))] = 999
endfor

RETURN,asun
end