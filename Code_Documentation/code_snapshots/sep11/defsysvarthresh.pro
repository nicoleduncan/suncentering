PRO defsysvarthresh, input
;+
;   :Description:
;       Defines solar thresholds
;
;   :Params:
;       input: in, required,type=byte(ndims,2)
;           Starting input image
;
;-

; Get solar IDs
idedsuns = idsuns(input)
n_suns = N_ELEMENTS(idedsuns)

; Identify peaks in sorted image array so that we can get thresholds
peaks = setbetterpeak(input,n_suns)
thresharr = REPLICATE({reg:0,thresh:0.},n_suns)

for i = 0,n_suns-1 do begin
    ; Grab region number
    thresharr[i].reg = (idedsuns[SORT(idedsuns)])[i]
    ; Grab threshold value
    thresharr[i].thresh = (peaks.sorted)[peaks.peakarr[i]]
    ; Stick them into global variables
    if i eq 0 then c = CREATE_STRUCT('reg'+STRCOMPRESS(thresharr[i].reg,/rem),thresharr[i].thresh) else $
        c = CREATE_STRUCT(c,'reg'+STRCOMPRESS(thresharr[i].reg,/rem),thresharr[i].thresh)
endfor

DEFSYSV,'!thresh',c
end