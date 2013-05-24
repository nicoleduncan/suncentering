FUNCTION picksun, inputimage, inputsuns
;+
;   :Description:
;       Decides which suns to ignore. Looks along the border pixels of the image and if 6 consecutive pixels are seen, it finds the closest sun (which will be a partial sun) and marks it as no good
;
;   :Params:
;       inputimage: in, required
;           The raw input image
;
;       inputsuns: in, required
;           Structure containing all the solar information
;
;   :Keywords:
;       
;-

s = SIZE(inputimage,/d)
n_col = s[0]
n_row = s[1]
vec = !Null

; Have to do some stupid strucutre stuff because tags are different
a = fltarr(3)
names = tag_names( !thresh)
if WHERE(names eq 'REG1') ne -1 then a[0] = !thresh.reg1
if WHERE(names eq 'REG2') ne -1 then a[1] = !thresh.reg2
if WHERE(names eq 'REG3') ne -1 then a[2] = !thresh.reg3

; scan in bottom,right,top,left
threshmask = inputimage gt MIN(a)
bottom = threshmask[*,0]
top = threshmask[*,-1]
left = REFORM(threshmask[0,*])
right = REFORM(threshmask[-1,*])

borderarr = [bottom,right,REVERSE(top),REVERSE(left)]

xarr = [FINDGEN(n_col),REPLICATE(n_col-1,n_row),REVERSE(FINDGEN(n_col)),REPLICATE(0,n_row)]
yarr = [REPLICATE(0,n_col),FINDGEN(n_row),REPLICATE(n_row-1,n_col),REVERSE(FINDGEN(n_row))]

; If we have 6 consecutive pixels then it's bad
if TOTAL(borderarr) gt 6 then begin
    for i =0,N_ELEMENTS(borderarr)-1 do begin
        ; Keep track of how many consecutive picels there are
        if borderarr[i+1] + borderarr[i] eq 2 then vec = [vec,i] else vec = !Null
        ; If we get 6 in a row, get out of this for loop
        if N_ELEMENTS(vec) gt 6 then break
    endfor

    if vec ne !Null then begin
        xcenter = MEAN(xarr[vec])
        ycenter = MEAN(yarr[vec])
        ; Pick closest center
        sundist = SQRT((inputsuns.xpos-xcenter)^2 + (inputsuns.ypos-ycenter)^2)
        closest_sun = (inputsuns.reg)[WHERE(sundist eq MIN(sundist))]
        borderarr[vec[0]:vec[0]+70]=0
    endif
    
    ; Set the closest sun to the xcenter,ycenter of the consecutive 6 pixels to nearest center
    inputsuns[WHERE(inputsuns.reg eq closest_sun[0])].partial=1


    ; We have 3 suns so I'm making 1 nested if loop
    if TOTAL(borderarr) gt 6 then begin
        for i =0,N_ELEMENTS(borderarr)-1 do begin
            if borderarr[i+1] + borderarr[i] eq 2 then vec = [vec,i] else vec = !Null
            if N_ELEMENTS(vec) gt 6 then break
        endfor

        if vec ne !Null then begin
            xcenter = MEAN(xarr[vec])
            ycenter = MEAN(yarr[vec])

            ; pick closest center
            sundist = SQRT((inputsuns.xpos-xcenter)^2 + (inputsuns.ypos-ycenter)^2)
            closest_sun = (inputsuns.reg)[WHERE(sundist eq MIN(sundist))]
            borderarr[vec[0]:vec[0]+70]=0
        endif

        inputsuns[WHERE(inputsuns.reg eq closest_sun[0])].partial=1
    endif
endif

RETURN,inputsuns
end