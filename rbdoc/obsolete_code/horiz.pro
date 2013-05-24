; horiz.pro

READ_JPEG,'plots_tables_images/100kft/notilt.jpg',image
redc = reform(image[0,*,*])

spacethresh = 110
mask = redc gt spacethresh
spacemask = (redc lt 100)*redc
rmask = mask*redc
s = size(redc,/dim)
n_col = s[0]
n_row = s[1]

thresh = 60

yspot = where(shift_diff(rmask,dir=1) gt thresh)/n_row
cline = fltarr(n_col)
reg1limb = fltarr(n_col)
reg2limb = fltarr(n_col)

for i = 0,n_col-1 do begin
    cline[i] = mean(where(rmask[i,*] lt thresh))
endfor

height = mean(cline)

; The only thing we care about from the fit is the y intercept 
result=linfit(findgen(n_col),cline)
ama = rmask

for i = 0,n_col-1 do begin
    reg2limb[i] = (where(rmask[i,0:height] gt thresh))[-1]
    reg1limb[i] = (where(rmask[i,height:n_row-1] gt thresh))[0]
    ama[i,reg2limb[i]] = 200
    ama[i,reg1limb[i]+height] = 200
endfor

xarr = findgen(n_col)
tx = findgen(n_col * 100)/100
a=poly_fit(xarr,reg2limb,2)
spl = spline(xarr,a[0] + a[1]*xarr + a[2]*xarr^2,tx)


loadct,0

; window,0
ps_start,filename='earth_limb.eps',/encapsulated
plot,xarr,reg2limb,xs=3,ys=3,psym=3,ytitle='Y-Position',xtitle='X-Position',title='Earth Limb Pixel Position'
oplot,tx,spl,linestyle=1
ps_end

phi = tan((spl[-1]-spl[0])/n_col)*!radeg

print,'Angle from looking at ends of polyfitted indices',phi
print,'Angle from looking at linear fit slope of midpoints of limbs',result[1]*!radeg
print,'^^ Realistically, this should be 0'
; It's a coincidence they are close, right?

; Imagine if the camera was completely level, in that case the slope from linfit=0 but 
; then there could be roll, so phi can be non-zero
; window,1
; cgimage,rmask,/k,output='plots_tables_images/rmask.eps'
; window,2
; cgimage,ama,/k,output='plots_tables_images/ama.eps'

cgimage,spacemask,/k,output='plots_tables_images/spacemask.eps'

window,0
plot,xarr,reg2limb,xs=3,ys=3,psym=4,ytitle='Y-Position',xtitle='X-Position',title='Earth Limb Pixel Position'
oplot,tx,spl,linestyle=1
window,1
cgimage,rmask,/k
window,2
cgimage,ama,/k

;************************************************************************************************
;                                                                                               *
;                                                                                               *
;                                                                                               *
;************************************************************************************************

s = size(spacemask,/dimensions)
n_col = s[0]
n_row = s[1]

xpos = TOTAL( TOTAL(spacemask, 2) * Indgen(n_col) ) / total(spacemask)
ypos = TOTAL( TOTAL(spacemask, 1) * Indgen(n_row) ) / total(spacemask)

spredc = redc
spredc[xpos-1:xpos+1,*]=255
spredc[*,ypos-1:ypos+1]=255

; adding 2 1 pix buffer to make it easier to see

spredc[n_col/2,*] = 100

window,3
cgimage,spredc,/k;';,output='maskcenter.eps'


;************************************************************************************************
;                                                                                               *
;                                                                                               *
;                                                                                               *
;************************************************************************************************



; now going to try some GEOMETRY 

; arclen / (2*R) = asin(half_chord_length/R)

; main_chord_length = theta/alpha * 2 * sin(theta/2.)     solve for theta

; reg2limbend_1 = []
; reg2limbend_2 = []

clength = sqrt((n_col - 0)^2 + (spl[-1] - spl[0])^2)
alpha = crvlength(tx,spl)

; clength = theta/alpha * 2 * sin(theta/2)

; wolfram alpha gives theta = 5.769 i +/- 21.954 j  Taking on the real out, so we're looking at an anfle of 5.76?
theta = 5.76924*!dtor
radius = alpha/theta

anglearr = findgen(1e6)/1e6 * 2*!pi
x = radius*cos(anglearr)
y = radius*sin(anglearr)

; tvcircle,radius,x,y
















stop

end