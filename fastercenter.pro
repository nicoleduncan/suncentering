FUNCTION fastercenter, input, regarr

; regarr = [1,2,3]
peakinfo = setbetterpeak(input,regarr)

; Not true all the time!!!
; peak_1 = (peakinfo.peakarr)[0]
; peak_2 = (peakinfo.peakarr)[1]
; peak_3 = (peakinfo.peakarr)[2]

xsort = peakinfo.xsort
ysort = peakinfo.ysort

peakreg = REPLICATE({reg:0,xpos:0.,ypos:0.},N_ELEMENTS(regarr))

; do we deal with region number here? Probably, right?
; Easiest to find centers then match up locations to regarr, Probably

for i = 0,N_ELEMENTS(peakinfo.peakarr)-1 do begin
    apeak = peakinfo.peakarr[i]
    peakreg[i].reg = regarr[i]

    if i eq 0 then begin
        peakreg[i].xpos = MEAN(xsort[apeak:N_ELEMENTS(xsort)-1])
        peakreg[i].ypos = MEAN(ysort[apeak:N_ELEMENTS(ysort)-1])

        ; why isn't it wrking for regarr=[1,2]?? It is, just a dif image, idiot. Lol.
    endif else begin
        peakreg[i].xpos = MEAN((xsort[apeak:peakinfo.peakarr[i-1]])[WHERE(xsort[apeak:peakinfo.peakarr[i-1]] ne 0)])
        peakreg[i].ypos = MEAN((ysort[apeak:peakinfo.peakarr[i-1]])[WHERE(ysort[apeak:peakinfo.peakarr[i-1]] ne 0)])
    endelse

    xsort[WHERE(xsort gt (peakreg[i].xpos - !param.crop_box) and xsort lt (peakreg[i].xpos + !param.crop_box))] = 0
    ysort[WHERE(ysort gt (peakreg[i].ypos - !param.crop_box) and ysort lt (peakreg[i].ypos + !param.crop_box))] = 0
endfor

return,peakreg
end