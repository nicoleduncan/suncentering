; docformat = 'rst'

;+
; Bunch of code deisgned to make sure the things I write are doing the things I want
;
; Compiled the doc with idldoc,root='/Users/jerensuzuki/Documents/suncentering', $
; output='rbdoc',format_style='rst',/user,/quiet,markup_style='rst'
;
; :Author:
;   JEREN SUZUKI::
;
;       Space Sciences Laboratory
;       7 Gauss Way
;       Berkeley, CA 94720 USA
;       E-mail: jsuzuki@ssl.berkeley.edu
;-

PRO cc

cropped_image = scanboxv2(file = 'Sun_Images_000000.bmp')

restore,'comp4strips.sav',/v
window,0
plot,ystrips[5].ARRAY,xs=3,ys=3,/nodata
FOR i=0,n_elements(ystrips) - 1 DO BEGIN
    oplot,ystrips[i].ARRAY,color=i+1
    ; print,closest((ystrips[i].ARRAY)[0:59],thresh),closest((ystrips[i].ARRAY)[60:119],thresh)
ENDFOR   

window,1
plot,xstrips[5].ARRAY,xs=3,ys=3,/nodata
FOR i=0,n_elements(xstrips) - 1 DO BEGIN
    oplot,xstrips[i].ARRAY,color=i+1
ENDFOR   

window,2
shade_surf,cropped_image,shades=bytscl(cropped_image)

window,3
cgimage,cropped_image,/keep_asp
END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO makesomescatplots
;+
; :Description:
;   Makes a few scatter plots based on varying parameters. Sucks that the values are hardcoded.
;   
;   Also, as of Nov 14, these are all wrong.
;-

xcenter = 107.498
ycenter = 110.496
thresh  = 119.350

a = [105.686,106.016,106.523,106.846]
b = [110.5,110.923,111.306,112.022]

c = [105.190,105.190,106.591,106.417]
d = [110.995,110.019,112.017,111.932]

e = [105.657,105.190,105.886,106.034]
f = [110.5,111.008,111.890,111.903]

g = [105.191,105.681,105.577,105.610]
h = [110.5,110.754,111.133,110.694]

ad = [1.81226,1.48,.974,.652]
bd = [-.00353,-.4267,-.809,-1.52528]

cd = [2.30773,2.30796,.906784,1.08130]
dd = [-.49828,-.522530,-1.52,-1.43504]

ed = [1.84131,2.30773,1.61249,1.46391]
fd = [-.003,-.511787,-1.39378,-1.40653]

gd = [2.30679,1.81726,1.92130,1.88797]
hd = [-.00353,-2.57034,-.636787,-.197037]


ps_start,filename='0pix.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,a,psym=7,xr=[1,4],ys=3,xs=3,yr=[100,115],ytitle='Position',$
    title='X and Y positions with 0 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,b,psym=4,color=6
    hline,xcenter
    hline,ycenter,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='1pix.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,c,psym=7,xr=[1,4],ys=3,xs=3,yr=[100,115],ytitle='Position',$
    title='X and Y positions with 1 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,d,psym=4,color=6
    hline,xcenter
    hline,ycenter,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='2pix.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,e,psym=7,xr=[1,4],ys=3,xs=3,yr=[100,115],ytitle='Position',$
    title='X and Y positions with 2 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,f,psym=4,color=6
    hline,xcenter
    hline,ycenter,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='4pix.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,g,psym=7,xr=[1,4],ys=3,xs=3,yr=[100,115],ytitle='Position',$
    title='X and Y positions with 4 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,h,psym=4,color=6
    hline,xcenter
    hline,ycenter,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ps_start,filename='0pixdev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,ad,psym=7,xr=[1,4],ys=3,xs=3,yr=[-3,3],ytitle='Position',$
    title='X and Y Deviations with 0 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,bd,psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='1pixdev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,cd,psym=7,xr=[1,4],ys=3,xs=3,yr=[-3,3],ytitle='Position',$
    title='X and Y Deviations with 1 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,dd,psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='2pixdev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,ed,psym=7,xr=[1,4],ys=3,xs=3,yr=[-3,3],ytitle='Position',$
    title='X and Y Deviations with 2 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,fd,psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps

ps_start,filename='4pixdev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,gd,psym=7,xr=[1,4],ys=3,xs=3,yr=[-3,3],ytitle='Position',$
    title='X and Y Deviations with 4 pixel blur, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,hd,psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps


END

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PRO scatter
;+
; :Description:
;   Abbreviated form of "Make Some More Scatter Plots"
;   Making more scat plots now that I've fixed my program and am now moving onto bmp images
;
;-



ministrip_length = 9
scan_width = 10
file = 'Sun_Images_000000.bmp'
sigmavalue = 1
savstep = 4

comp2v2,xpos,ypos,thresh,time=time,sigmavalue=sigmavalue,file=file

xc=xpos
yc=ypos

comp6v2,xpos,ypos,order=1,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length,savstep=savstep
x1=xpos
y1=ypos
comp6v2,xpos,ypos,order=2,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length,savstep=savstep
x2=xpos
y2=ypos
comp6v2,xpos,ypos,order=3,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length,savstep=savstep
x3=xpos
y3=ypos
comp6v2,xpos,ypos,order=4,scan_width=scan_width,file=file,sigmavalue=sigmavalue,$
    ministrip_length=ministrip_length,savstep=savstep
x4=xpos
y4=ypos

ps_start,filename='bmpsundev.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,xc-[x1,x2,x3,x4],psym=7,xr=[1,4],ys=3,xs=3,yr=[-1,1],ytitle='Position',$
        title='X and Y Deviations from bmp sun, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,yc-[y1,y2,y3,y4],psym=4,color=6
    hline,0
    legend,['X Deviations   ','Y Deviations   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps


ps_start,filename='bmpsun.ps',/encapsulated,quiet=1,charsize=.8
    plot,findgen(4) + 1,[x1,x2,x3,x4],psym=7,xr=[1,4],ys=3,xs=3,yr=[40,55],ytitle='Position',$
        title='X and Y positions from bmp sun, limb length = 9, scan width = 10,threshold = 1 sigma',$
        xtitle='Order Polynomial',xticks = 3
    oplot,findgen(4) + 1,[y1,y2,y3,y4],psym=4,color=6
    hline,xc
    hline,yc,color=6
    legend,['X Positions   ','Y Positions   '],colors=[0,6],psym=[7,4], /top,/right
ps_end,resize=100,/pdf,/delete_ps
END