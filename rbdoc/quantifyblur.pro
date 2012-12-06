PRO quantifyblur
;+
; :Description: 
;       Quantify how many pixels it takes to make transitions.
;
;       Black   ->  1-sigma
;       1-sigma ->  White
;       Black   ->  White
;       over x & y
;-

; Have to compile centerv4 manually. Sux.
; image = scanboxv2(file='Sun_Images_000000.bmp',time=time)
image = scanboxv2(file='gauss2pix.tiff',time=time)

; How do we define minimum of image?
h = histogram(image,OMIN=om,OMAX=oma)
cutarr              = h[0:n_elements(h)/2.]
mostcomm            =  (where(cutarr eq max(cutarr)))[0]
btosigcount         = 0
sigtowhitecount     = 0
FOR i = 0,(size(image,/dimensions))[0]-1 DO BEGIN
    strip           = image[0:((size(image,/dimensions))[0])/2,i]
    ind = where(strip GT max(image)-stddev(image))
    check = strip GT max(image)-stddev(image)
    IF total(check) GT 1 AND median(strip) GT 20 THEN BEGIN ;80 for bmp.
    ; stop
    ; plot,strip
        lowest          = where(abs(strip - mostcomm) EQ min(abs(strip - mostcomm)))
        btosig          = strip[lowest[-1]:ind[0]]
        sigtowhite      = strip[ind[1]:(where((strip eq max(strip)) eq 1))[0]]
        blacktowhite    = [btosig,sigtowhite]
        IF total(btosigcount) eq 0 THEN btosigcount = n_elements(btosig) $
            ELSE btosigcount = [btosigcount,n_elements(btosig)]
        IF total(sigtowhitecount) eq 0 THEN sigtowhitecount = n_elements(sigtowhite) $
            ELSE sigtowhitecount = [sigtowhitecount,n_elements(sigtowhite)]

        ; ; setting things to max to see if we're actually right:
        image[ind[1]:(where((strip eq max(strip)) eq 1))[0],i] = 20
        ; image[lowest[-1]:ind[0],i] = 20
        ; ; We are right, can comment this out now.
    ENDIF
ENDFOR
; window,0
; cghistoplot,btosigcount,binsize=1,title='Black to 1-sigma from max'
; window,2
; cghistoplot,sigtowhitecount,binsize=1,title='1-sigma from max to max'

window,1
cgimage,image,/k
; tmp = image
; tmp[*,8] = 255
; tmp[*,65] =255
; cgimage,tmp,/k


; I'm only measuring linear blur amount, not radial. IS there an easy way to convert?
; Let's not worry about this yet.

; Another problem we're having is that for the bmp image, the sigtowhite is getting t
; oo far into the sun and getting weird frayed esges.

stop
END