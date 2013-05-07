FUNCTION setbetterthresh, input

idedsuns = idsuns(input)
n_suns = n_elements(idedsuns)
peaks = setbetterpeak(input,n_suns)

thresh100 = (peaks.sorted)[peaks.peakarr[0]]
thresh50 = (peaks.sorted)[peaks.peakarr[1]]
thresh25 = (peaks.sorted)[peaks.peakarr[2]]

return,{thresh100:thresh100,thresh50:thresh50,thresh25:thresh25}
end