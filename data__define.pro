FUNCTION data::init
COMPILE_OPT idl2 
on_error,2
;-- allocate memory to pointer when initializing object
 self.ptr=ptr_new(/allocate)  
 RETURN,1
end 

;****************************************************************************************

PRO data::set,value
;-- if data value exists, then insert into pointer location
 if n_elements(value) ne 0 then *(self.ptr)=value
 RETURN 
end

;****************************************************************************************

FUNCTION data::get,value
;-- if data value is stored in object pointer, then copy it out
 if n_elements(*(self.ptr)) ne 0 then value=*(self.ptr)
 RETURN,value
end

;****************************************************************************************

FUNCTION data::read, file

IF n_elements(file) EQ 0 THEN file='triplesun.bmp'
; check=findfile(file,count=count)     ;-- check if file exists
; IF count NE 1 THEN RETURN,0            ;-- bail if not there
; Useful, but slow.
tmpimage = read_bmp(file)
s = size(tmpimage,/dimensions)
image = reform(tmpimage[0,*,*])
RETURN, image
END

;****************************************************************************************

PRO data__define
 void={data,ptr:ptr_new()}
 RETURN 
end