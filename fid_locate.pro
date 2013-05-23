FUNCTION fid_locate, inputimage, inputstruct,mcenter=mcenter

acrop = inputimage[inputstruct[0].limbxpos - !param.soldiskr : $
inputstruct[0].limbxpos + !param.soldiskr,$
inputstruct[0].limbypos - !param.soldiskr : inputstruct[0].limbypos + !param.soldiskr]
 
; badcrop = acrop[0:-6,0:-4]       
; badcrop = acrop[0:-5,0:-4]     
badcrop = acrop
badcrop[where(badcrop eq max(badcrop))] = mode(badcrop)

s = size(badcrop,/dim)
; This kernel is no good.
; kernel = [[0,1,0],[1,1,1],[0,1,0]]
; This kernel is no good either.
; kernel = [[0,0,1,0,0],[0,0,1,0,0],[1,1,1,1,1],[0,0,1,0,0],[0,0,1,0,0]]
kernel = [[0,0,1,1,0,0],[0,0,1,1,0,0],[1,1,1,1,1,1],[1,1,1,1,1,1],[0,0,1,1,0,0],[0,0,1,1,0,0]]
image = convol(float(badcrop),kernel,/edge_truncate)

fidthresh = .01
; seems like .01 is a good thresh value. Why? Beats me.
; Also, need 5 fiducial pixels for there to be a fiducial detection

pixelfiducials = indgen(s)
fidlength = 3
fidwidth=2
threshold = mean(image)  + fidthresh*stddev(image)
newthresh = mean(image)  + fidthresh/2*stddev(image)
fidpos = {x:0.,y:0.,subpx:0.,subpy:0.}
basestr = fidpos
ff=0

; loop through:
; i s[0]-1 times
; j s[0]-1 times
; k 10+789+30+809 times

; shave off .0004s by moving these two lines out
xpos = pixelfiducials mod s[0]
ypos = pixelfiducials/s[1]

for i = 1, s[0]-2 do begin
    for j = 1,s[1]-2 do begin
        thisvalue = image[i,j]
        if thisvalue lt threshold then begin
            if thisvalue lt image[i,j+1] and $
            thisvalue lt image[i,j-1] and $
            thisvalue lt image[i+1,j] and $
            thisvalue lt image[i-1,j] then begin
                redundant=0
                for k = 0, N_ELEMENTS(pixelfiducials)-1 do begin
                ; This part is being super-iterated
                    if ABS(xpos[k] - i) lt fidlength*2 and ABS(ypos[k] - j) lt fidlength*2 then begin
                        redundant=1
                        thatvalue = image[xpos[k],ypos[k]]
                        if thisvalue lt thatvalue then begin
                            if ff ne 0 then fidpos = [fidpos,basestr]
                            fidpos[ff].x = i
                            fidpos[ff].y = j
                            ff++
                            break 
                        endif
                    endif
                endfor
            endif
        endif
    endfor
endfor

; let's interp to subpixel values, yo.
; Let's try to do it Albert's way first
; Let's stick with Albert's way

if KEYWORD_SET(mcenter) then begin
; It's already stupid fast as it is.
    for k = 0,n_elements(fidpos)-1 do begin
        caa=0
        cbb=0
        avg=0
        xrange = [fidpos[k].x - fidwidth,fidpos[k].x + fidwidth + 1]
        yrange = [fidpos[k].y - fidwidth,fidpos[k].y + fidwidth + 1]
        for aa = 0,xrange[1]-xrange[0] do begin
            for bb = 0,yrange[1]-yrange[0] do begin
                thisvalue = image[aa+xrange[0],bb+yrange[0]]
                if thisvalue lt newthresh then begin
                    caa += aa*thisvalue
                    cbb += bb*thisvalue
                    avg += thisvalue
                endif
            endfor
        endfor
        fidpos[k].subpx=caa/avg+xrange[0]
        fidpos[k].subpy=cbb/avg+yrange[0]
    endfor
endif else begin


; Let's use parapeak instead of mask centroiding
; It doesn't actually take a long time, only because it needs to compile a few necessary programs.
    for k = 0,n_elements(fidpos)-1 do begin
        z = image[fidpos[k].x-1:fidpos[k].x+1,fidpos[k].y-1:fidpos[k].y+1]
        result = paradip(z)
        ; result = parapeak(1/z)
        fidpos[k].subpx = fidpos[k].x + result[0]
        fidpos[k].subpy = fidpos[k].y + result[1]
    endfor
endelse

; there are small local minima/maxima! Oh boy. Not sure how to deal with this
; atmp = badcrop
; for i =0,n_elements(fidpos)-1 do begin
;     atmp[fidpos[i].x,fidpos[i].y]=255
; endfor

; HOLY SHIT THE LAST 2 ARE SO WRONG

; How to deal with edge fiducials?
; print, fidpos

RETURN,fidpos
END