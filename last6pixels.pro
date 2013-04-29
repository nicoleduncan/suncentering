FUNCTION last6pixels, input, thresh

datcrop = input[18:43,6:31]

; ps_start,filename='datcrop_color.eps',/color,/encapsulated
; s = size(datcrop,/dim)
; maskcrop = bytarr(s[0],s[1])
; taskcrop = bytarr(s[0],s[1])
; baskcrop = bytarr(s[0],s[1])
; askcrop  = bytarr(s[0],s[1])
; maskcrop[2,0:11] =  1
; taskcrop[3,0:11] = 1
; baskcrop[0:11,2] = 1
; askcrop[0:11,3] = 1
; cgimage,datcrop,/k,/axes
; plot_edges,maskcrop,thick=3,x0=.5,y0=.5
; plot_edges,taskcrop,thick=3,x0=.5,y0=.5,dcolor=200
; plot_edges,baskcrop,thick=3,x0=.5,y0=.5,dcolor=220
; plot_edges,askcrop,thick=3,x0=.5,y0=.5,dcolor=150
; ps_end,/png,resize=100
; stop
; cgimage,datcrop,/k,output='datcrop.png'

!p.multi=[0,1,6]

a=0
for z = 0,3 do begin
    ps_start,filename='botleft'+strcompress(z,/rem)+'.eps',/encapsulated,xsize=7,ysize=12
    for i = 0,5 do begin
        case z of
          0:range = (datcrop[*,2])[0+a:11+a]
          1:range = (datcrop[*,3])[0+a:11+a]
          2:range = (datcrop[2,*])[0+a:11+a]
          3:range = (datcrop[3,*])[0+a:11+a]
        endcase
        plot,range - mode(datcrop),psym=-4,title='array - mode(wholeimage) from [0:11] edge of (input['+strcompress(18+a,/rem)+':43,6:31])[*,2]',xs=3,ys=3
        vline,5-i
        hline,-20 
        hline,-10,linestyle=1
        a++
    endfor
    a=0
    ps_end

    ;*****

    ps_start,filename='botright'+strcompress(z,/rem)+'.eps',/encapsulated,xsize=7,ysize=12
    for i = 0,5 do begin
        case z of
          0:range = (datcrop[*,2])[-12-a:-1-a]
          1:range = (datcrop[*,3])[-12-a:-1-a]
          2:range = (datcrop[-2,*])[0+a:11+a]
          3:range = (datcrop[-3,*])[0+a:11+a]
        endcase
        plot,range - mode(datcrop),psym=-4,title='array - mode(wholeimage) from [0:11] edge of (input['+strcompress(18+a,/rem)+':43,6:31])[*,2]',xs=3,ys=3
        if (z eq 0 ) || (z eq 1) then vline,6+i else  vline,5-i
        hline,-20 
        hline,-10,linestyle=1
        a++
    endfor
    a=0
    ps_end
    
    ;*****

    ps_start,filename='topleft'+strcompress(z,/rem)+'.eps',/encapsulated,xsize=7,ysize=12
    for i = 0,5 do begin
        case z of
          0:range = (datcrop[2,*])[-12-a:-1-a]
          1:range = (datcrop[3,*])[-12-a:-1-a]
          2:range = (datcrop[*,-2])[0+a:11+a]
          3:range = (datcrop[*,-3])[0+a:11+a]
        endcase
        plot,range - mode(datcrop),psym=-4,title='array - mode(wholeimage) from [0:11] edge of (input['+strcompress(18+a,/rem)+':43,6:31])[*,2]',xs=3,ys=3
        if (z eq 0 ) || (z eq 1) then vline,6+i else  vline,5-i
        hline,-20 
        hline,-10,linestyle=1
        a++
    endfor
    a=0
    ps_end

    ;*****

    ps_start,filename='topright'+strcompress(z,/rem)+'.eps',/encapsulated,xsize=7,ysize=12
    for i = 0,5 do begin
        case z of
          0:range = (datcrop[-2,*])[-12-a:-1-a]
          1:range = (datcrop[-3,*])[-12-a:-1-a]
          2:range = (datcrop[*,-2])[-12-a:-1-a]
          3:range = (datcrop[*,-3])[-12-a:-1-a]
        endcase
        plot,range - mode(datcrop),psym=-4,title='array - mode(wholeimage) from [0:11] edge of (input['+strcompress(18+a,/rem)+':43,6:31])[*,2]',xs=3,ys=3
        vline,6+i
        hline,-20 
        hline,-10,linestyle=1
        a++
    endfor
    a=0
    ps_end
endfor

; this is all for 1 fiducial



!p.multi=0
stop
end