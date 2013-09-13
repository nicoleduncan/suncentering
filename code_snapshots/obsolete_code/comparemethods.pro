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

comp2,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file
xcs = xpos
ycs = ypos

comp3,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file
xcsn = xpos
ycsn = ypos

set_plot,'ps'

device,filename='comparemethods_xpos.ps',/encapsulated,/color
plot,[0,1,2],[40.1,40,40.4],psym=7,ystyle=3,/nodata

hline,xcs,color=4 ; GREEN
hline,xcsn,color=6 ;PURPLE

FOR i = 0,6 DO BEGIN
    ministrip_length = i*2+3

    comp6,xpos,ypos,time=time,order=order,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
        ministrip_length=ministrip_length,savstep=savstep,saveonly=saveonly

    hline,xpos,color=2
ENDFOR

legend,['Limb-fitted X position','Weighted Average X Position','Mask Center X Position'],$
    lines=[0,0,0],color=[2,4,6],/bottom,/left,charsize=1.2
device,/close

;--------------------------------------------------------------------------------------------

device,filename='comparemethods_ypos.ps',/encapsulated,/color
plot,[0,1,2],[35.6,36,36.6],psym=7,ystyle=3,/nodata

hline,ycs,color=4 ; GREEN
hline,ycsn,color=6 ;PURPLE

FOR i = 0,6 DO BEGIN
    ministrip_length = i*2+3

    comp6,xpos,ypos,time=time,order=order,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
        ministrip_length=ministrip_length,savstep=savstep,saveonly=saveonly

    hline,ypos,color=2
ENDFOR

legend,['Limb-fitted X position','Weighted Average X Position','Mask Center X Position'],$
    lines=[0,0,0],color=[2,4,6],/bottom,/left,charsize=1.2
device,/close

command = 'ps2pdf -dEPSCrop comparemethods_ypos.ps && ps2pdf -dEPSCrop comparemethods_xpos.ps'
remove = 'rm -rf comparemethods*.ps'
spawn,command
spawn,remove

set_plot,'x'

; Needs to be a better way to optimize limb length
; Is there a way to color each limb length or I.... should plot it.

hline,xcs,color=4 ; GREEN
hline,xcsn,color=6 ;PURPLE
xposarr = fltarr(7)
yposarr = fltarr(7)
FOR i = 0,6 DO BEGIN
    ministrip_length = i*2+3

    comp6,xpos,ypos,time=time,order=order,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
        ministrip_length=ministrip_length,savstep=savstep,saveonly=saveonly
    xposarr[i] = xpos
    yposarr[i] = ypos
ENDFOR

!p.multi = [0, 1, 2, 0, 0]
set_plot,'ps'
device,filename='differencesinposition.eps',/encapsulated
    plot,findgen(7)*2+3,xposarr-xcsn,psym=7,xstyle=3,title='X Position',xtitle='Limb Length',$
        ytitle='mask center - limb center'
    plot,findgen(7)*2+3,yposarr-ycsn,psym=7,xstyle=3,title='Y Position',xtitle='Limb Length',$
        ytitle='mask center - limb center'
device,/close

set_plot,'x'
!p.multi=0

spawn,'ps2pdf -dEPSCrop differencesinposition.eps && rm -rf differencesinposition.eps'
stop

END