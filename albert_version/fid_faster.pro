FUNCTION fid_faster, inputimage, inputstruct
; ONLY 5x faster than fid_locate

; Making somearea is almost 20x faster than fid_locate's 2d convol method but the speed gain is lost in the nested forloop to find the local minima

crop = inputimage[inputstruct.limbxpos - !param.crop_box:inputstruct.limbxpos + !param.crop_box,inputstruct.limbypos - !param.crop_box:inputstruct.limbypos + !param.crop_box]
squirtle = FIX((TRANSPOSE(crop))[*])
temparr = FIX(crop[*])
kernel = [1,1,1,1,1,0,0,0,0,0,1,1,1,1,1]

test = CONVOL(temparr,kernel)
ytest = CONVOL(squirtle,kernel)

im = REFORM(test,1 + 2 * !param.crop_box ,1 + 2 * !param.crop_box )
yim = REFORM(ytest,1 + 2 * !param.crop_box ,1 + 2 * !param.crop_box )

tyim = TRANSPOSE(yim)

somearea = im*tyim
atmp = somearea
somefactor = .5
fidfloor = 8000
s = SIZE(somearea,/d)
threshold = MEAN(somearea) - somefactor*STDDEV(somearea)

a=localmax(somearea,fidfloor,threshold)

RETURN,a
end