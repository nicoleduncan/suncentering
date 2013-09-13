function partialcenter, inputimage
loadct,15
pcrop = inputimage[0:100,0:100]
qm = quickmask(pcrop,30)
pcrop[qm.xpos,qm.ypos]=255
cgimage,pcrop*(pcrop gt 30),/k
stop
return,1
end