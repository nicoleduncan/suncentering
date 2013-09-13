FUNCTION evenfastercenter, input, idedsuns

n_suns = N_ELEMENTS(idedsuns)
peakinfo = setbetterpeak(input,n_suns)

xsort = peakinfo.xsort
ysort = peakinfo.ysort

s = SIZE(input,/dim)
n_col = s[0]
n_row = s[1]
xarr = fan(FINDGEN(n_col),n_row)
yarr = TRANSPOSE(xarr)
; How do we tell apart lined up images?
rot_input = TRANSPOSE(input)
ysort = yarr[bsort(rot_input)]
; or
; xsort already has top 1% removed, can't remake any arrays

; 2 is fine, 3 fucks up

peakreg = REPLICATE({reg:0,xpos:0.,ypos:0.},n_suns)

for i = 0,N_ELEMENTS(peakinfo.peakarr)-1 do begin
    apeak = peakinfo.peakarr[i]
    peakreg[i].reg = idedsuns[i]
    if i eq 0 then begin
        peakreg[i].xpos = MEAN(xsort[apeak:N_ELEMENTS(xsort)-1])
        peakreg[i].ypos = MEAN(ysort[apeak:N_ELEMENTS(ysort)-1])
    endif else begin
    
        peakreg[i].xpos = MEAN((xsort[apeak:peakinfo.peakarr[i-1]])[WHERE(xsort[apeak:peakinfo.peakarr[i-1]] ne 0)])
        peakreg[i].ypos = MEAN((ysort[apeak:peakinfo.peakarr[i-1]])[WHERE(ysort[apeak:peakinfo.peakarr[i-1]] ne 0)])
    endelse
    
    

    stop
    plot,input[WHERE((yarr gt (peakreg[i].ypos - !param.crop_box)) and $
        (yarr lt (peakreg[i].ypos + !param.crop_box)) and $
        (xarr gt (peakreg[i].xpos - !param.crop_box)) and $
        (xarr lt (peakreg[i].xpos + !param.crop_box)))],psym=3
stop
    ; input[peakreg[i].xpos-60:peakreg[i].xpos+60,peakreg[i].ypos-60:peakreg[i].ypos+60]=0
    xsort[WHERE(xsort gt (peakreg[i].xpos - !param.crop_box) and xsort lt (peakreg[i].xpos + !param.crop_box))] = 0
    ysort[WHERE(ysort gt (peakreg[i].ypos - !param.crop_box) and ysort lt (peakreg[i].ypos + !param.crop_box))] = 0
endfor

RETURN,peakreg
end