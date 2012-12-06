; so let me put down what I did so far:

str1 = {garlic,bulb:5}
str2 = {lake,hair:50d,www:90,month:'March',cars:str1}
str3 = {tree,dogs:10,struct:str2}

help,str1,/st
help,str2,/st
help,str3,/st

hdr='STRING'
mwrfits,str1,'str1.fits',/create
; mwrfits,str2,'str2.fits',/create
; mwrfits,str3,'str3.fits',/create
print,''
print,''
print,''
help,mrdfits('str1.fits',1)
print,''
; c = mrdfits('str2.fits',1)
help,c
stop
end