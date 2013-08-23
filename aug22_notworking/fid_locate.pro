FUNCTION fid_locate, inputimage, inputstruct,mcenter=mcenter
;+
;   :Description:
;       Locates the whole and sub-pixel positions of fiducials in a cropped solar image
;
;   :Params:
;       inputimage: in, required
;           The raw input image
;
;       inputstruct: in, required
;           Structure containing all the solar information
;
;   :Keywords:
;       mcenter: in, optional
;           Finds the center of the local maxima using a binary mask instead of parabolic peak fitting. 
;
;-

badcrop = inputimage[inputstruct[0].limbxpos - !param.soldiskr : inputstruct[0].limbxpos + !param.soldiskr,inputstruct[0].limbypos - !param.soldiskr : inputstruct[0].limbypos + !param.soldiskr]
 
badcrop[WHERE(badcrop eq MAX(badcrop))] = MODE(badcrop)

s = SIZE(badcrop,/dim)
kernel = [[0,0,1,1,0,0],[0,0,1,1,0,0],[1,1,1,1,1,1],[1,1,1,1,1,1],[0,0,1,1,0,0],[0,0,1,1,0,0]]
image = CONVOL(FLOAT(badcrop),kernel,/edge_truncate)

fidthresh = .01
; seems like .01 is a good thresh value. Why? Beats me.
; Also, need 5 fiducial pixels for there to be a fiducial detection

pixelfiducials = INDGEN(s)
fidlength = 3
fidwidth = 2
threshold = MEAN(image) + fidthresh*STDDEV(image)
newthresh = MEAN(image) + fidthresh/2*STDDEV(image)
fidpos = {x:0.,y:0.,subpx:0.,subpy:0.}
basestr = fidpos
ff = 0

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
    for k = 0,N_ELEMENTS(fidpos)-1 do begin
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
    for k = 0,N_ELEMENTS(fidpos)-1 do begin
        z = image[fidpos[k].x-1:fidpos[k].x+1,fidpos[k].y-1:fidpos[k].y+1]
        result = paradip(z)
        ; result = parapeak(1/z)
        fidpos[k].subpx = fidpos[k].x + result[0]
        fidpos[k].subpy = fidpos[k].y + result[1]
    endfor
endelse
; stop
RETURN,fidpos
END