FUNCTION everysun, input

temparr = input
idedsuns = idsuns(temparr)
n_suns = n_elements(idedsuns)

asun = replicate({xpos:0.,ypos:0.,reg:0,thresh:0.,partial:0.},n_suns)

for i = 0,n_suns-1 do BEGIN
    asun[i].reg = idedsuns[where(idedsuns eq min(idedsuns))]
    case asun[i].reg of
        1: asun[i].thresh = !thresh.reg1
        2: asun[i].thresh = !thresh.reg2
        3: asun[i].thresh = !thresh.reg3
    endcase
    
    gig = quickmask(temparr, asun[i].thresh)
    asun[i].xpos = gig.xpos
    asun[i].ypos = gig.ypos

    sidepad = 80
    s = size(temparr,/d)
    paddedimage = BYTARR(s+sidepad*2)
    paddedimage[sidepad,sidepad]=temparr
    paddedimage[gig.xpos+sidepad - !param.crop_box:gig.xpos+sidepad + !param.crop_box,$
        gig.ypos+sidepad - !param.crop_box:gig.ypos+sidepad + !param.crop_box]=0
    temparr = paddedimage[sidepad:s[0]+sidepad-1,sidepad:s[1]+sidepad-1]

    idedsuns[where(idedsuns eq min(idedsuns))] = 999
endfor

return,asun
end