; PRO bundle

READ_JPEG, 'plots_tables_images/Bundles/DSC02928.jpg', wim

im = reform(wim[0,*,*])

; bc = ((a[1000:2600,1300:1700]) lt 10)
; c = (a[1000:2600,1300:1700])*bc

; mc = 200*morph_close(c,replicate(1,3,3))

; cgimage,bc,/k,/display
; cgimage,shift_diff(bc,direction=1),/k,/display
; cgimage,laplacian(c),/k,/display
; cgimage,mc,/k,/display
; cgimage,200*morph_close(mc,replicate(1,3,3)),/k,/display
; cgimage,200*a[1000:2600,1300:1700]*(a[1000:2600,1300:1700] lt 10),/k,/display
; cgimage,((a[1000:2600,1300:1700]) gt 50)*(a[1000:2600,1300:1700]),/k



; b = (a[1000:2600,1300:1700] lt 10)

; xx = where(total(b,1) gt 1000) ; = rows where more than 1500 pixels are a 'slat'

; d=b
; d[*,xx]=100
; cgimage,d,/k,/display
; cgimage,b*a[1000:2600,1300:1700],/k,/display

; the problem with this is that the slats need to be aligned, when nicole said to draw lines on the edge of the slats, probably didn't mean to mask it?





fftransform = FFT(im[1000:2600,1300:1700],/center)
power = abs(fftransform)^2
scaledpower = alog10(power)

scaledpowerto0 = scaledpower - max(scaledpower)

; s3 = surface(scaledpowerto0)

mask = real_part(scaledpowerto0) gt -7

maskedtransform = fftransform*mask

it = real_part(FFT(maskedtransform,/inverse,/center))

; so doing an inverse FFT to filter out noise is awesome.

; so we get rid of weird artifacts at edges
rim = (im[1000:2600,1300:1700])[15:1585,15:385]
www = fltarr(size(rim,/dim))
nit = it[15:1585,15:385]

; window,0
; cgimage,nit,/k



nitmask = 200*(nit lt 20)
; cgimage,nitmask,/k


begp = !null
endp = !null

for i = 0,(size(nit,/dim))[0] -1 do begin
    a = where(nitmask[i,*] gt 100)
    begp = [ [begp], [a[where(a - shift(a,1) ne 1)]] ]
    endp = [ [endp], [a[where(a - shift(a,-1) ne -1)]] ]
endfor


for i = 0,(size(begp,/dim))[0]-1 do begin
    line = linfit(findgen((size(begp,/dim))[1]),begp[i,*],yfit=begyfit)
    line = linfit(findgen((size(begp,/dim))[1]),endp[i,*],yfit=endyfit)

; ps_start,filename='edgefit0.eps',/encap
; plot,begp[0,*],xs=3,ys=3,title='Distance of edge of slat from edge of image',yr=[24,38]
; oplot,begyfit,color=!red
; legend,['Slat Edge Position','Fitted Edge'],linestyle=[0,0],color=[!black,!red],/bottom,/right
; ps_end

; line = linfit(findgen((size(begp,/dim))[1]),begp[1,*],yfit=begyfit)

; ps_start,filename='edgefit1.eps',/encap
; plot,begp[1,*],xs=3,ys=3,title='Another slat',yr=[72,85]
; oplot,begyfit,color=!red
; legend,['Slat Edge Position','Fitted Edge'],linestyle=[0,0],color=[!black,!red],/bottom,/right
; ps_end

; !p.multi=[0,1,2]
; ps_start,filename='findedge.eps',/encap,xsize=18,ysize=6
; plot,nit[0,*],xs=3
; plot,nitmask[0,*],xs=3
; ps_end
; !p.multi=0
stop
    for j =0,(size(nitmask,/dim))[0]-1 do begin
        nitmask[j,begyfit[j]]=!red
        nitmask[j,endyfit[j]]=!green

        ; To make lines thicker
        ; nitmask[j,begyfit[j]-1:begyfit[j]+1]=!red
        ; nitmask[j,endyfit[j]-1:endyfit[j]+1]=!green

        www[j,begyfit[j]]=!red
        www[j,endyfit[j]]=!green
    endfor
endfor



; This isn't exactly on the edge because it's a fit
cgimage,rim+www,/k
stop


; This IS the edge
aaa = (shift_diff(nit,dir=1) gt 10)*!green
cgimage,aaa+rim,/k




stop
END