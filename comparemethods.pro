PRO comparemethods
;+
; :Description: 
;       Compare each of our 3 methods to see how different they really are
;
;-

ministrip_length = 9
scan_width = 10
file = 'Sun_Images_000000.bmp'
sigmavalue = 1
savstep = 4
order = 2

plot,[0,1,2],[40.1,40,40.4],psym=7,ystyle=3,/nodata
comp2v2,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file
xcs = xpos
ycs = ypos
hline,xcs,color=4 ; GREEN

comp3,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file
xcsn = xpos
ycsn = ypos
hline,xcsn,color=6 ;RED


FOR i = 0,6 DO BEGIN
    ministrip_length = i*2+3

    comp6v2,xpos,ypos,time=time,order=order,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
        ministrip_length=ministrip_length,savstep=savstep,saveonly=saveonly

    ; plot,[0,1,2],[40.1,40,40.4],psym=7,ystyle=3,/nodata
    ; plot,[0,1,2],[xcs,xcsn,xpos],psym=7,ystyle=3
    ; wait,.5
    hline,xpos,color=2
ENDFOR









stop

END