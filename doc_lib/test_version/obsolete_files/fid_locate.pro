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

; badcrop = inputimage[inputstruct[0].limbxpos - !param.soldiskr : inputstruct[0].limbxpos + !param.soldiskr,inputstruct[0].limbypos - !param.soldiskr : inputstruct[0].limbypos + !param.soldiskr]

badcrop = inputimage[inputstruct[0].limbxpos - !param.crop_box : inputstruct[0].limbxpos + !param.crop_box,inputstruct[0].limbypos - !param.crop_box : inputstruct[0].limbypos + !param.crop_box]

badcrop[WHERE(badcrop eq MAX(badcrop))] = MODE(badcrop)

s = SIZE(badcrop,/dim)
; kernel = [$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],$
; [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],$
; [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],$
; [0,0,0,0,0,0,1,1,1,0,0,0,0,0,0]]

fidwidth = 3
fidlength = 15
kernel = fltarr(fidlength,fidlength)
kernel[0:fidwidth-1,*]=1
kernel[*,0:fidwidth-1]=1
kernel = SHIFT(kernel,2*fidwidth,2*fidwidth)

image = CONVOL(FIX(badcrop),kernel,/edge_truncate)

tmp=image
fidthresh = .1

threshold = MEAN(image) + fidthresh*STDDEV(image)
fidfloor = .2*MAX(image)
fidpos = {x:0.,y:0.,subpx:0.,subpy:0.}
basestr = fidpos
ff = 0
n_fid = 5

; pixelfiducials = FINDGEN(s)
; shave off .0004s by moving these two lines out
; xpos = FIX(pixelfiducials mod s[0])
; ypos = FIX(pixelfiducials/s[1])

; shaved off .1 seconds by removing the many abs() statements
; That's a motherfucking 33%


for i = 1, s[0]-2 do begin
    for j = 1,s[1]-2 do begin
        ; The correlation value at some position
        thisvalue = image[i,j]
        ; Check to see if it's less than the surrounding 8 pixels
        if thisvalue lt threshold and thisvalue gt fidfloor then begin
            if thisvalue lt image[i,j+1] and $
            thisvalue lt image[i,j-1] and $
            thisvalue lt image[i+1,j] and $
            thisvalue lt image[i-1,j] and $
            thisvalue lt image[i-1,j-1] and $
            thisvalue lt image[i-1,j+1] and $
            thisvalue lt image[i+1,j-1] and $
            thisvalue lt image[i+1,j+1] then begin
                ; Redundant is to mark if we find something in the next for loop
                redundant = 0
                fidpos[ff].x = i
                fidpos[ff].y = j
                tmp[i,j]=255
                ff++
                ; Only need this if statement because it doesn't like starting at 0
                if n_elements(fidpos) ne 1 then begin
                    for k = 0,n_elements(fidpos) - 1 do begin
                        ; We iterate through previous fiducial positions and if we find one within 2 fiducial lengths of another fiducial we use the stronger correlated one
                        if ABS(fidpos[k].x - i) lt fidlength*2 and ABS(fidpos[k].y - j) lt fidlength*2 then begin
                            ; Hey look it's redundant, something's about to happen
                            redundant = 1
                            ; Correlation value at a position in question
                            thatvalue = image[fidpos[k].x,fidpos[k].y]
                            ; If the peak is contested, give it to the stronger one
                            if thisvalue lt thatvalue then begin
                                fidpos[k].x = i
                                fidpos[k].y = j
                            endif
                        endif
                        ; Get out of here, if there are any contestants it should only happen once
                        break   
                    endfor
                    ; Regardless of whether it was replaced, get out of here
                    if redundant eq 1 then break
                endif
                ; Keep extending fidpos until we have n_fid fiducials
                if n_elements(fidpos) lt n_fid then fidpos = [fidpos,basestr]
            endif
        endif
    endfor
endfor

; stop
; not picking up the fiducials on the VERY edge

; let's interp to subpixel values, yo.
; Let's try to do it Albert's way first
; Let's stick with Albert's way

if KEYWORD_SET(mcenter) then begin
    newthresh = MEAN(image) + fidthresh/2*STDDEV(image)
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