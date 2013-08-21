; PRO bundle

; READ_JPEG, 'plots_tables_images/Bundles/DSC02928.jpg', jim
wim = READ_TIFF('plots_tables_images/Bundles/DSC02911.tiff')
im = rotate(reform(wim[0,*,*]),7)

; jim = reform(jim[0,*,*])

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




fftransform = FFT(im[800:2300,1400:1800],/center)
power = abs(fftransform)^2
scaledpower = alog10(power)

scaledpowerto0 = scaledpower - max(scaledpower)

; s3 = surface(scaledpowerto0)

mask = real_part(scaledpowerto0) gt -7

maskedtransform = fftransform*mask

it = real_part(FFT(maskedtransform,/inverse,/center))

; so doing an inverse FFT to filter out noise is awesome.

; so we get rid of weird artifacts at edges
rim = (im[800:2300,1400:1800])[15:1485,15:385]
www = fltarr(size(rim,/dim))
nit = it[15:1485,15:385]

; window,0
; cgimage,nit,/k


nitmask = 200*(nit lt 4000) ;was 20 for jpeg, TIFF uses floats
; nitmask = nit*(nit lt 1.7e4) ;was 20 for jpeg, TIFF uses floats
; cgimage,nitmask,/k


begp = !null
endp = !null

for i = 0,(size(nit,/dim))[0] -1 do begin
    a = where(nitmask[i,*] gt 100)
    ; a = where(nitmask[i,*] gt 5e3)
        ;If dimensions don't match up, have to up the threshold
    begp = [ [begp], [a[where(a - shift(a,1) ne 1)]] ]
    endp = [ [endp], [a[where(a - shift(a,-1) ne -1)]] ]
endfor

; OR ANOTHER WAY

; pawn = shift_diff(nit,dir=1)
; pawn = pawn[1:-2,*]
; stars = shift_diff(nit,dir=6)
; stars = stars[1:-2,*]

; for i = 0,(size(pawn,/dim))[0] -1 do begin
;     a = where(pawn[i,*] gt 5e3)
;     print,i
;     begp = [ [begp], [a[where(a - shift(a,1) ne 1)]] ]
;     b = where(stars[i,*] gt 5e3)
;     endp = [ [endp], [b[where(b - shift(b,-1) ne -1)]] ]
; endfor

for i = 0,(size(begp,/dim))[0]-1 do begin
    line = poly_fit(findgen((size(begp,/dim))[1]),begp[i,*],2,yfit=begyfit)
    line = poly_fit(findgen((size(begp,/dim))[1]),endp[i,*],2,yfit=endyfit)

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
; stop
    for j =0,(size(nitmask,/dim))[0]-1 do begin
        nitmask[j,begyfit[j]]=65535.0
        nitmask[j,endyfit[j]]=65535.0

        ; To make lines thicker
        ; nitmask[j,begyfit[j]-1:begyfit[j]+1]=!red
        ; nitmask[j,endyfit[j]-1:endyfit[j]+1]=!green

        www[j,begyfit[j]]=65535.0
        www[j,endyfit[j]]=65535.0
    endfor
endfor



; This isn't exactly on the edge because it's a fit
cgimage,rim+www,/k

; instead of thresh, use shift_diff?
a = shift_diff(nit,dir=1)*(shift_diff(nit,dir=1) gt 500) + shift_diff(nit,dir=6)*(shift_diff(nit,dir=6) gt 500)
b = shift_diff(nit,dir=1)*(shift_diff(nit,dir=1) gt 750) + shift_diff(nit,dir=6)*(shift_diff(nit,dir=6) gt 750)
c = shift_diff(nit,dir=1)*(shift_diff(nit,dir=1) gt 1000) + shift_diff(nit,dir=6)*(shift_diff(nit,dir=6) gt 1000)
d = shift_diff(nit,dir=1)*(shift_diff(nit,dir=1) gt 2000) + shift_diff(nit,dir=6)*(shift_diff(nit,dir=6) gt 2000)

; a = (shift_diff(nit,dir=1) gt 500) + (shift_diff(nit,dir=6) gt 500)
; b = (shift_diff(nit,dir=1) gt 750) + (shift_diff(nit,dir=6) gt 750)
; c = (shift_diff(nit,dir=1) gt 1000) + (shift_diff(nit,dir=6) gt 1000)
; d = (shift_diff(nit,dir=1) gt 2000) + (shift_diff(nit,dir=6) gt 2000)

cgimage,a,/k

!p.multi=[0,1,3]
plot,a[1,30:100],psym=-7,ys=3
oplot,b[1,30:100],psym=-7,color=!red
plot,a[1,30:100],psym=-7,ys=3
oplot,c[1,30:100],psym=-7,color=!green 
plot,a[1,30:100],psym=-7,ys=3
oplot,d[1,30:100],psym=-7,color=!cyan
!p.multi=0


window,2
plot,float(rim[1,*])-max(rim[1,*])/2,psym=-7           
oplot,(shift_diff(rim,dir=1))[1,*],color=!green,psym=-4

slatstart = !null
slatend = !null
window,3
poo = fltarr(1e5)
k=0
; (size(rim,/dim))[0]-2
for i = 1,1000 do begin
    ; plot,float(rim[i,*])-max(rim[i,*])/2,psym=-7,xr=[20,100]        
    ; oplot,(shift_diff(rim,dir=1))[i,*],color=!green,psym=-4
    a = rim[i,*]
    slats = where(a lt max(a)/2.)

    something = slats[where(slats - shift(slats,1) ne 1)] - 5
    curry = slats[where(slats - shift(slats,-1) ne -1)] + 5

    for j=0,n_elements(something) -1 do begin
        arr = findgen(n_elements(a[something[j]:curry[j]]))
        hooray = gaussfit(arr,abs(float(a[something[j]:curry[j]]) - max(a[something[j]:curry[j]]) ),terms,nterms=3)
        if abs(2*SQRT(2*ALOG(2))*terms[2]) lt 10 then begin
        poo[k] = 2*SQRT(2*ALOG(2))*terms[2]
        stop
        k++
        endif
    endfor

    ; slatstart = [ [slatstart], [slats[where(slats - shift(slats,1) ne 1)]] ]
    ; slatend = [ [slatend], [slats[where(slats - shift(slats,-1) ne -1)]] ]
    ; stop
endfor
poo = poo[0:k]
; cghistoplot,poo,binsize=.02,xr=[0,10]























stop


; This IS the edge
aaa = (shift_diff(nit,dir=1) gt 10)*!green
cgimage,aaa+rim,/k


stop
END