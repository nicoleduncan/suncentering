FUNCTION gord_fid, inputimage, inputstruct

crop = FLOAT(inputimage[inputstruct.limbxpos - !param.crop_box:inputstruct.limbxpos + !param.crop_box,inputstruct.limbypos - !param.crop_box:inputstruct.limbypos + !param.crop_box])

ysums = total(crop,1) - smooth(total(crop,1),15)
xsums = total(crop,2) - smooth(total(crop,2),15)

yfids = where(ysums le -150,n_yfid)
xfids = where(xsums le -150,n_xfid)
; !p.charsize=1.2
; !p.multi=[0,1,2]
loadct,15
; ps_start,filename='smoothcomp.eps',/color,/encap
; plot,total(crop,1) - smooth(total(crop,1),10),thick=5,/ys,/xs,title='Black = 10 smoothed, Red = 15 smoothed'
; oplot,total(crop,1) - smooth(total(crop,1),15),thick=5,color=6
; hline,-150
; plot,total(crop,1) - smooth(total(crop,1),10),thick=5,/ys,/xs,title='Black = 10 smoothed, Red = 20 smoothed'
; oplot,total(crop,1) - smooth(total(crop,1),20),thick=5,color=100
; hline,-150
; ps_end
; !p.multi=0
; print,xfids
; print,yfids


; associate fid coords with each other
print,xfids
print,yfids

; I can either eliminate adjacent piels (easier in the long run?) or just ignore them (faster)

; construct each pair, look to see if there is a nearby fiducial
; what qualifies a nearby fiducial?
; Probably needs to look at pixel values and do some sort of comparison of surroundig pixels

; 54,108
; 101,124
; 117,55
; 151,139
; 167,70



cgimage,crop,/k
; ; !p.multi=[0,2,2]
; ps_start,filename='smooth_expl.eps',/encap,/color
; plot,total(crop,1)
; ; plot,total(crop,1)
; oplot,smooth(total(crop,1),10),color=20
; ; plot,ysums
; ; hline,-150
; ps_end
; ; !p.multi=0
stop
RETURN,{xfids:xfids,yfids:yfids}
end