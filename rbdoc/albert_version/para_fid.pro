FUNCTION para_fid, inputimage, inputstruct
; 3x faster than fid_faster
; .005 to execute if using gaussfit

; 10x faster than fid_faster if using parapeak
; .001 to execute if using parapeak

k=0
z=fltarr(3,3,/nozero)

; cropped-down image of sun
crop = FLOAT(inputimage[inputstruct.limbxpos - !param.crop_box:inputstruct.limbxpos + !param.crop_box,inputstruct.limbypos - !param.crop_box:inputstruct.limbypos + !param.crop_box])

; plots of x and y totals to identify rows/columns of fiducials
yt = TOTAL(crop,1)
xt = TOTAL(crop,2)

; array of differences between sum profile and smoothed sum profile
ysums = yt - SMOOTH(yt, !param.fid_smooth_candidates)
xsums = xt - SMOOTH(xt, !param.fid_smooth_candidates)

; Identify where it looks like a fiducial
yfids = WHERE(ysums le !param.fid_smooth_thresh) ;try little less
xfids = WHERE(xsums le !param.fid_smooth_thresh)

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
        if crop[xx[i],yy[j]] lt !param.disk_brightness then begin
            
            aa = crop[xx[i] - !param.fid_crop_box:xx[i] + !param.fid_crop_box,yy[j] - !param.fid_crop_box:yy[j] + !param.fid_crop_box]

            rowsum=TOTAL(aa,1) ; Summing rows to get a y position profile
            ysum=SMOOTH(rowsum,10)-rowsum   ;The array we're thresholding
            bw = WHERE(ysum gt !param.onedsumthresh,n_bw)

            colsum=TOTAL(aa,2)
            xsum=SMOOTH(colsum,10)-colsum
            dw = WHERE(xsum gt !param.onedsumthresh,n_dw)
            
            ; if there are any array values above a threshold, it's definitely a fiducial
            if n_bw ne 0 and n_dw ne 0 then begin
                    fidpos[k].x=xx[i]
                    fidpos[k].y=yy[j]

                    ; Find peaks
                    maxx = where(xsum eq max(xsum))
                    maxy = where(ysum eq max(ysum))
                    ; Fit a parabola to these
                    xarr = xsum[maxx-1:maxx+1]
                    yarr = ysum[maxy-1:maxy+1]

                    ; Making some fake 3x3 so that we can use parapeak
                    z[0:2] = xarr * yarr[2]
                    z[3:5] = xarr * yarr[1]
                    z[6:8] = xarr * yarr[0]
                    
                    result = parapeak(z)
                    
                    fidpos[k].subx = maxx + result[0] + xx[i] - !param.fid_crop_box
                    fidpos[k].suby = maxy + result[1] + yy[j] - !param.fid_crop_box

                    k++
                    if k eq N_ELEMENTS(xx)>N_ELEMENTS(yy) then break
            endif
        endif
    endfor
endfor

RETURN,fidpos
end