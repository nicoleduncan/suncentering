PRO defsysvarthresh, input

idedsuns = idsuns(input)
n_suns = n_elements(idedsuns)
peaks = setbetterpeak(input,n_suns)

thresharr = REPLICATE({reg:0,thresh:0.},n_suns)
for i = 0,n_suns-1 do begin
    thresharr[i].reg = (idedsuns[sort(idedsuns)])[i]
    thresharr[i].thresh = (peaks.sorted)[peaks.peakarr[i]]

    if i eq 0 then c = CREATE_STRUCT('reg'+strcompress(thresharr[i].reg,/rem),thresharr[i].thresh) else $
        c = CREATE_STRUCT(c,'reg'+strcompress(thresharr[i].reg,/rem),thresharr[i].thresh)
endfor

defsysv,'!thresh',c
end