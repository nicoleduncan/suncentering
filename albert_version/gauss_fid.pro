FUNCTION gauss_fid, inputimage, inputstruct
; 3x faster than fid_faster
; .005 to execute

somethresh=100
k=0
length = 31

; cropped-down image of sun
crop = FLOAT(inputimage[inputstruct.limbxpos - !param.crop_box:inputstruct.limbxpos + !param.crop_box,inputstruct.limbypos - !param.crop_box:inputstruct.limbypos + !param.crop_box])

; plots of x and y totals to identify rows/columns of fiducials
yt = TOTAL(crop,1)
xt = TOTAL(crop,2)

; array of differences between sum profile and smoothed sum profile
ysums = yt - SMOOTH(yt,15)
xsums = xt - SMOOTH(xt,15)

; Identify where it looks like a fiducial
yfids = WHERE(ysums le -150)
xfids = WHERE(xsums le -150)

; Rule out adjacent indices
aa=xfids
aa= [0,aa,0]
bb = aa - SHIFT(aa,1)
cc = aa[WHERE(bb ne 1)]
xx = cc[1:-2]

aa=yfids
aa= [0,aa,0]
bb = aa - SHIFT(aa,1)
cc = aa[WHERE(bb ne 1)]
yy = cc[1:-2]

; Breaks if indices aren't exactly consecutive... need to work on that

fidpos = REPLICATE({fidpos,x:0.,y:0.,subx:0.,suby:0.},N_ELEMENTS(xx)>N_ELEMENTS(yy))

; Loop through each x and y position combination 
for i = 0,N_ELEMENTS(xx)-1 do begin
    for j = 0,N_ELEMENTS(yy)-1 do begin
        ; To eliminate coords that are just solar pixels and not fiducials (on disk)
        if crop[xx[i],yy[j]] lt 15 then begin
            
            aa = crop[xx[i]-15:xx[i]+15,yy[j]-15:yy[j]+15]

            rowsum=TOTAL(aa,1) ; Summing rows to get a y position profile
            ysum=SMOOTH(rowsum,10)-rowsum
            bw = WHERE(ysum gt somethresh,n_bw)

            colsum=TOTAL(aa,2)
            xsum=SMOOTH(colsum,10)-colsum
            dw = WHERE(xsum gt somethresh,n_dw)
            print,xx[i]
            print,yy[j]
            if n_bw ne 0 and n_dw ne 0 then begin
                    fidpos[k].x=xx[i]
                    fidpos[k].y=yy[j]

                    ygauss = GAUSSFIT(FINDGEN(length),rowsum,ycoeff,nterms=5)
                    xgauss = GAUSSFIT(FINDGEN(length),colsum,xcoeff,nterms=5)
                    
                    ; [1] is the center term
                    fidpos[k].subx = ycoeff[1] + xx[i]-15
                    fidpos[k].suby = xcoeff[1] + yy[j]-15

                    k++
                    if k eq N_ELEMENTS(xx)>N_ELEMENTS(yy) then break
            endif
        endif
    endfor
endfor








; 20 possible fiducial candidates, how many have duplciates indices? 14? don't think so.















RETURN,fidpos
end