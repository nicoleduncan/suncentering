function posscomb,n

i = REBIN(LINDGEN(n), n, n)           
j = REBIN(TRANSPOSE(LINDGEN(n)), n, n)
mask = i gt j

mwh = where(mask eq 1)
xpos = mwh mod 4
ypos = mwh / 4

return,{x:xpos,y:ypos}
end