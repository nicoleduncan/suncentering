FUNCTION fid_faster, inputimage, inputstruct
; ONLY 7x faster than fid_locate
; takes .008 compared to fid_locate's .06

; Making somearea is almost 20x faster than fid_locate's 2d convol method but the speed gain is lost in the shifting operations

crop = FLOAT(inputimage[inputstruct.limbxpos - !param.crop_box:inputstruct.limbxpos + !param.crop_box,inputstruct.limbypos - !param.crop_box:inputstruct.limbypos + !param.crop_box])
squirtle = (TRANSPOSE(crop))[*]
temparr = crop[*]
kernel = [1,1,1,1,1,0,0,0,0,0,1,1,1,1,1]

test = CONVOL(temparr,kernel)
ytest = CONVOL(squirtle,kernel)

im = REFORM(test,1 + 2 * !param.crop_box ,1 + 2 * !param.crop_box )
yim = REFORM(ytest,1 + 2 * !param.crop_box ,1 + 2 * !param.crop_box )

tyim = TRANSPOSE(yim)

somearea = im*tyim
somefactor = .5
fidfloor = 8000
threshold = MEAN(somearea) - somefactor*STDDEV(somearea)

a=localmax(somearea,fidfloor,threshold)

RETURN,a
end