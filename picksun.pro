FUNCTION picksun, inputimage, inputsuns

; Scan outermost pixel, if N_detect > 5, it's NG
; How to associate region to ^ ?

; Whicever centroid is closest, I guess
s = size(inputimage,/d)
n_col = s[0]
n_row = s[1]
vec = !null

; scan in bottom,right,top,left
threshmask = inputimage gt 30
bottom = threshmask[*,0]
top = threshmask[*,-1]
left = reform(threshmask[0,*])
right = reform(threshmask[-1,*])

; right = temp_right[1:-1]
; top = temp_top[0:-2]
; left = temp_left[1:-2]

borderarr = [bottom,right,REVERSE(top),REVERSE(left)]

; xarr = [findgen(n_col),replicate(n_col,n_row-1),findgen(n_col-1),replicate(0,n_row-2)]
; yarr = [replicate(0,n_col),findgen(n_row-1),replicate(n_row,n_col-1),reverse(findgen(n_row-2))]

xarr = [FINDGEN(n_col),REPLICATE(n_col-1,n_row),REVERSE(FINDGEN(n_col)),REPLICATE(0,n_row)]
yarr = [REPLICATE(0,n_col),FINDGEN(n_row),REPLICATE(n_row-1,n_col),REVERSE(FINDGEN(n_row))]
; stop

; multiple suns, that's right...
if total(borderarr) gt 6 then begin
    ; print,'Time to scan'
    for i =0,n_elements(borderarr)-1 do begin
        if borderarr[i+1] + borderarr[i] eq 2 then vec=[vec,i] else vec=!null
        if n_elements(vec) gt 6 then break
    endfor

    if vec ne !null then begin
        xcenter = mean(xarr[vec])
        ycenter = mean(yarr[vec])
        ; pick closest center
        sundist = sqrt((inputsuns.xpos-xcenter)^2 + (inputsuns.ypos-ycenter)^2)
        closest_sun = (inputsuns.reg)[where(sundist eq min(sundist))]
        borderarr[vec[0]:vec[0]+70]=0
    endif
    ; not setting sun to partial




; p1_p2_w3
; stop
    inputsuns[where(inputsuns.reg eq closest_sun[0])].partial=1

; Must be a better way to do this!!!
    if total(borderarr) gt 6 then begin
        ; print,'Time to scan'
        for i =0,n_elements(borderarr)-1 do begin
            if borderarr[i+1] + borderarr[i] eq 2 then vec=[vec,i] else vec=!null
            if n_elements(vec) gt 6 then break
        endfor

        ; xcenter is 0... not good
        ; ycenter is only 97

        if vec ne !null then begin
            xcenter = mean(xarr[vec])
            ycenter = mean(yarr[vec])

            ; pick closest center
            sundist = sqrt((inputsuns.xpos-xcenter)^2 + (inputsuns.ypos-ycenter)^2)
            closest_sun = (inputsuns.reg)[where(sundist eq min(sundist))]
            borderarr[vec[0]:vec[0]+70]=0
        endif

        ; not setting sun to partial
        inputsuns[where(inputsuns.reg eq closest_sun[0])].partial=1
    endif
endif
; inputsuns[where(inputsuns.reg eq closest_sun)].partial=1

return,inputsuns
end