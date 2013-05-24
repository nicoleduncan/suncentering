FUNCTION setbetterthresh, input

idedsuns = idsuns(input)
n_suns = n_elements(idedsuns)
peaks = setbetterpeak(input,n_suns)

thresharr = REPLICATE({reg:0,thresh:0.},n_suns)
for i = 0,n_suns-1 do begin
    thresharr[i].reg = (idedsuns[sort(idedsuns)])[i]
    thresharr[i].thresh = (peaks.sorted)[peaks.peakarr[i]]
endfor

return,thresharr
end