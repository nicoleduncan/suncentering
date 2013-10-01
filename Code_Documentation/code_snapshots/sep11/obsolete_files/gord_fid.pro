FUNCTION gord_fid, inputimage, inputstruct
; Takes .01s
crop = FLOAT(inputimage[inputstruct.limbxpos - !param.crop_box:inputstruct.limbxpos + !param.crop_box,inputstruct.limbypos - !param.crop_box:inputstruct.limbypos + !param.crop_box])

yt = total(crop,1)
xt = total(crop,2)

ysums = yt - smooth(yt,15)
xsums = xt - smooth(xt,15)

yfids = where(ysums le -150)
xfids = where(xsums le -150)

; I can either eliminate adjacent piels (easier in the long run?) or just ignore them (faster)

; construct each pair, look to see if there is a nearby fiducial
; what qualifies a nearby fiducial?
; Probably needs to look at pixel values and do some sort of comparison of surroundig pixels

; 54,108
; 101,124
; 117,55
; 151,139
; 167,70

; 10:
; 54  55          70         101         102         116         117         118 136         151         152         166         167
; 54          55          70          71         108         123         124         139 140         155         199

; 15
; 54          55          70         101         102         116 117         118         136         151         152         166 167
; 24          39          54          55          70          71 108         123         124         139         140         155 199         215


; 20:
; 20          21          22          23          24          25          26          54 55          70         101         102         116         117         118         151 152         166         167         216         217         218         219         220
; 19          20          21          22          23          24          54          55 70          71         107         108         109         123         124         139 140         199         214         215         216         218

; Look at each looped coords
; Leftovers:
; x:
; y:
; 24 155 199

; im = crop

; fff = crop

; im[54-1:54+1,108-1:108+1]=255
; im[70-1:70+1, 39-1:39+1]=255
; im[101-1:101+1 , 124-1:124+1]=255
; im[117-1:117+1 , 55-1:55+1]=255
; im[151-1:151+1 , 139-1:139+1]=255
; im[167-1:167+1 , 70-1:70+1]=255
; im[136-1:136+1 , 215-1:215+1]=255
; ; 85 is not on the list. It's not on any list.
; im[86-1:86+1,199-1:199+1]=200
; im[208-1:208+1,155-1:155+1]=200
; im[237-1:237+1,24-1:24+1]=200

; r1 = im[54-15:54+15,108-15:108+15]

; ; how to id w/o convol? it'll take up a lot of time otherwise
; ; The problem is, the old way we found fiducials in the image as a whole
; ; Now we find rough locations but not really since we just have a list of coords where things look different, we'll have to look back at the data itself

; ; Leftover from 20 smoothed
; ; 23 218
; ; 

; fff[54-1:54+1,108-1:108+1]=255
; fff[70-1:70+1, 39-1:39+1]=200
; fff[101-1:101+1 , 124-1:124+1]=255
; fff[117-1:117+1 , 55-1:55+1]=255
; fff[151-1:151+1 , 139-1:139+1]=255
; fff[167-1:167+1 , 70-1:70+1]=255
; fff[136-1:136+1 , 215-1:215+1]=200
; ; 85 is not on the list. It's not on any list.
; fff[86-1:86+1,199-1:199+1]=200
; ; fff[208-1:208+1,155-1:155+1]=200
; fff[237-1:237+1,24-1:24+1]=200
; ; fff[220-1:220+1,86-1:86+1]=200

; aa= [54,55,70,101,102,116,117,118,136,151,152,166,167]
aa=xfids
aa= [0,aa,0]
bb = aa - shift(aa,1)
cc = aa[where(bb ne 1)]
xx = cc[1:-2]

; aa = [24,39,54,55,70,71,108,123,124,139,140,155,199,215]
aa=yfids
aa= [0,aa,0]
bb = aa - shift(aa,1)
cc = aa[where(bb ne 1)]
yy = cc[1:-2]

; Breaks if indices aren't exactly consecutive
somethresh=100
; asd = crop
; tic

fidpos = REPLICATE({fidpos,x:0.,y:0.,subx:0.,suby:0.},n_elements(xx)>n_elements(yy))

; There's a better way to do this, shouldn't need to iterate through each possible combination
k=0

; if distance to fiducial from center of sun is > solar radius then don't use
; tic

length = 31

for i = 0,n_elements(xx)-1 do begin
    for j = 0,n_elements(yy)-1 do begin
        ; To eliminate coords that are just solar pixels and not fiducials (on disk)
        if crop[xx[i],yy[j]] lt 15 then begin

            aa = crop[xx[i]-15:xx[i]+15,yy[j]-15:yy[j]+15]
            ; do 2 1D convols on aa
            ; length = (size(aa,/d))[0]
            
            ; tic
            ; gg = gaussfit(findgen(length),total(aa,1),nterms=5,chisq=achisq)
            ; gg = gaussfit(findgen(length),total(aa,2),nterms=5,chisq=bchisq)
            ; toc

            ; smoothing is like 40X faster
            ; tic

            a=total(aa,1) ; Summing rows to get a y position profile
            b=smooth(a,10)-a
            bw = where(b gt somethresh,n_bw)
            c=total(aa,2)
            d=smooth(a,10)-c
            dw = where(d gt somethresh,n_dw)

            ; !p.multi=[0,1,2]
            ; ps_start,filename='1d1dsums_'+strcompress(i,/rem)+'_'+strcompress(j,/rem)+'.eps',/encap,charsize=1.2
            ; plot,b,title='smooth(array,10)-array'
            ; hline,100,linestyle=1
            ; plot,d,title='smooth(array,10)-array'
            ; hline,100,linestyle=1
            ; ps_end
            ; !p.multi=0

            ; loadct,15
            ; cgimage,aa,/k,output='1d1dcrop_'+strcompress(i,/rem)+'_'+strcompress(j,/rem)+'.eps',/nointerp



            ; stop


            ; if plots have elements > 100 then it's a fid



            
            ; just look at 1D sums again
            ; stop
            ; print,achisq,bchisq
            ; Need a better 'deciding' factor
            ; if achisq gt 500 and bchisq gt 500 then begin
                ; gg = gaussfit(findgen(length),total(aa,2),nterms=5,chisq=bchisq)
            if n_bw ne 0 and n_dw ne 0 then begin
                ; asd[xx[i]-1:xx[i]+1,yy[j]-1:yy[j]+1] = 255 
                ; if bchisq gt 20 then begin
                ; stop
                    fidpos[k].x=xx[i]
                    fidpos[k].y=yy[j]
                    ; fidpos[k].chisq = achisq

                    ; Subpixel fitting with a gaussian, bro.
                    ; makes the program 5X slower. CAN WE DO BETTER??
                    ; Compared to parapeak, 10x slower
                    
                    gg = gaussfit(findgen(length),total(aa,1),gga,nterms=5)
                    hh = gaussfit(findgen(length),total(aa,2),hha,nterms=5)
                    
                    ; [1] is the center term
                    fidpos[k].subx = gga[1] + xx[i]-15
                    fidpos[k].suby = hha[1] + yy[j]-15








                    ; z = fltarr(3,3,/nozero)
                    ; ; b is y position
                    ; ; d is x
                    
                    ; ; sometimes peak isn't the max...
                    ; ; make pairs of rising/falling edges? Not rubust for limb fiducials.

                    ; maxd = where(d eq max(d))
                    ; maxb = where(b eq max(b))
                    ; drr = d[maxd-1:maxd+1]
                    ; brr = b[maxb-1:maxb+1]
                    
                    ; z[0:2] = drr * brr[2]
                    ; z[3:5] = drr * brr[1]
                    ; z[6:8] = drr * brr[0]
                    
                    ; result = parapeak(z)
                    
                    ; ; let's go through and order these correctly, shall we
                    ; ; think I added results in wrong order?
                    ; subx = maxd + result[1] + xx[i] - 15
                    ; suby = maxb + result[0] + yy[j] - 15

                    ; print,''
                    ; print,subx
                    ; print,suby
                    
                    k++
                    if k eq n_elements(xx)>n_elements(yy) then break
                ; endif
            endif
        endif
    endfor
endfor
; toc

; cgimage,asd,/k
; stop




; best5 = (fidpos[reverse(sort(fidpos.chisq))])[0:4]
; so the 5 most correlated aren't the best in reality

; this part is meant to work with a convolution...
; Now what?
; z = crop[xx[i]-1:xx[i]+1,yy[j]-1:yy[j]+1]
; result = paradip(z)
; fidpos[k].subpx = xx[i] + result[0]
; fidpos[k].subpy = yy[j] + result[1]






; ps_start,filename='gaussfitcomp.eps',/encap,xsize=6,ysize=7,/inches
; !p.multi=[0,1,4]
; qq = gaussfit(findgen(n_elements(total(q,1))),total(q,1),nterms=5,chisq=chisq)
; plot,total(q,1),title=textoidl("\chi^2/N=") + strcompress(chisq/length,/rem),/ys,/xs
; oplot,qq,linestyle=3,psym=-4

; qq = gaussfit(findgen(n_elements(total(q,2))),total(q,2),nterms=5,chisq=chisq)
; plot,total(q,2),title=textoidl("\chi^2/N=") + strcompress(chisq/length,/rem),/ys,/xs
; oplot,qq,linestyle=3,psym=-4

; qq = gaussfit(findgen(n_elements(total(ww,1))),total(ww,1),nterms=5,chisq=chisq)
; plot,total(ww,1),title=textoidl("\chi^2/N=") + strcompress(chisq/wl,/rem),/ys,/xs
; oplot,qq,linestyle=3,psym=-4

; qq = gaussfit(findgen(n_elements(total(ww,2))),total(ww,2),nterms=5,chisq=chisq)
; plot,total(ww,2),title=textoidl("\chi^2/N=") + strcompress(chisq/wl,/rem),/ys,/xs
; oplot,qq,linestyle=3,psym=-4
; ps_end
; !p.multi=0



; stop
RETURN,fidpos
end