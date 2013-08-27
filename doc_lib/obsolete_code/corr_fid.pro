FUNCTION corr_fid, inputimage, inputstruct
; tic
acrop = inputimage[inputstruct[0].limbxpos - !param.soldiskr : $
inputstruct[0].limbxpos + !param.soldiskr,$
inputstruct[0].limbypos - !param.soldiskr : inputstruct[0].limbypos + !param.soldiskr]

; badcrop = acrop[0:-6,0:-4]
badcrop = acrop
badcrop[where(badcrop eq max(badcrop))] = mode(badcrop)

s = size(badcrop,/dim)
kernel = [[0,1,0],[1,1,1],[0,1,0]]
kernel = [[0,0,1,0,0],[0,0,1,0,0],[1,1,1,1,1],[0,0,1,0,0],[0,0,1,0,0]]
kernel = [[0,0,1,1,0,0],[0,0,1,1,0,0],[1,1,1,1,1,1],[1,1,1,1,1,1],[0,0,1,1,0,0],[0,0,1,1,0,0]]
; image = c_correlate(badcrop,kernel,findgen(s))
image = convol(float(badcrop),kernel,/edge_Truncate)

; well, c++'s c_corr is like idl's convol()

cgimage,image,/k
fidthresh = .5
pixelfiducials = indgen(s)
fidlength = 3
fidwidth=2
threshold = mean(image)  + fidthresh*stddev(image)
newthresh = mean(image)  + fidthresh/2*stddev(image)
; wait, is their thresh thing so that the image peaks at a 

; oh it does. /facepalm

; start at 1, not 0
; end at -2?

fidpos = replicate({x:0.,y:0.,subpx:0.,subpy:0.},4)
ff=0
; tic
; .002s to do the for-loops out of a total .003s
; This is actually slower than my method but doesn't need complicated functions


; should I make this faster in idl if I can't translate it directly to C++?

; so combining thisvalue lt threshold with the 4 adjacent checks makes it slower. why?
tic

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
        ; print,'maybe a point'
        ; 866 times thisvalue is lt threshold
        ; c++
        ; this condition below filters out a bunch of the stuff
            if thisvalue lt image[i,j+1] and $
            thisvalue lt image[i,j-1] and $
            thisvalue lt image[i+1,j] and $
            thisvalue lt image[i-1,j] then begin
                ; print,'maybe a point' 
                ; 4 times!
                redundant=0
                for k = 0, N_ELEMENTS(pixelfiducials)-1 do begin
                ; This part is being super-iterated
                    if ABS(xpos[k] - i) lt fidlength*2 and $
                    ABS(ypos[k] - j) lt fidlength*2 then begin
                        redundant=1
                        thatvalue = image[xpos[k],ypos[k]]
                        if thisvalue lt thatvalue then begin
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
toc

; let's interp to subpixel values, yo.
; Let's try to do it Albert's way first

; It's already stupid fast as it is.
; tic
; for k = 0,n_elements(fidpos)-1 do begin
;     caa=0
;     cbb=0
;     avg=0
;     xrange = [fidpos[k].x - fidwidth,fidpos[k].x + fidwidth + 1]
;     yrange = [fidpos[k].y - fidwidth,fidpos[k].y + fidwidth + 1]
;     for aa = 0,xrange[1]-xrange[0] do begin
;         for bb = 0,yrange[1]-yrange[0] do begin
;             thisvalue = image[aa+xrange[0],bb+yrange[0]]
;             ; stop
;             if thisvalue lt newthresh then begin
;             ; print,"lawls"
;                 caa += aa*thisvalue
;                 cbb += bb*thisvalue
;                 avg += thisvalue
;             endif
;         endfor
;     endfor
;     fidpos[k].subpx=caa/avg+xrange[0]
;     fidpos[k].subpy=cbb/avg+yrange[0]
; endfor
; toc
; print,fidpos


; Let's use parapeak instead of mask centroiding
; It doesn't actually take a long time, only because it needs to compile a few necessary programs.
; tic
for k = 0,n_elements(fidpos)-1 do begin
    ; Have to do 1/z because convol() makes the highest correlation area a low value 
    ; instead of cross_correlate's high value

    z = 1000/image[fidpos[k].x-1:fidpos[k].x+1,fidpos[k].y-1:fidpos[k].y+1]
    ; the convol values are on the order of 10^3 so we have to normalize it! Makes sense. But this
    ; means we have to normalzie it all the time?
    result = parapeak(z)
    fidpos[k].subpx = fidpos[k].x + z[0]
    fidpos[k].subpy = fidpos[k].y + z[1]
endfor
; toc

; How to deal with edge fiducials?

RETURN,fidpos
END