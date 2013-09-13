pro defparams, file
;+
;   :Description:
;       Defines global parameters
;
;   :Params:
;       file: in, required
;           Text file used to define parameters
;
;-

; Read in file, first column is variable, second is value attacked to variable
; 'A' stands for word, 'F' for float, space deliminted
readcol,file,var,num,format='A,F',delimiter=' '
; Neat trick to dynamically create variables and attach values to them
for i=0,N_ELEMENTS(var)-1 do (SCOPE_VARFETCH(var[i],/enter,level=0))=num[i]

; Stick the first of these variables in a structure
c = CREATE_STRUCT(var[0],num[0])

; Append variables one after another
for i=1,N_ELEMENTS(var)-1 do begin
    c = CREATE_STRUCT(c,var[i],num[i])
endfor

; Load parameters from a txt file and make then system variables
DEFSYSV,'!param',c

END