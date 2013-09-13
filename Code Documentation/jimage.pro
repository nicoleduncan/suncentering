pro jimage, im,_EXTRA=_EXTRA

t = size(im,/dim)
if t[0] gt 1920 then w = 1920/2 else w = t[0]
if t[0] gt 1200 then h = 1200/2 else h = t[0]

cgwindow,wxsize=w,wysize=h,/free
cgimage,im,/axes,/window,/k,background=200,_EXTRA=_EXTRA

end