FUNCTION para_fid, inputimage, inputstruct
; 3x faster than fid_faster
; .005 to execute if using gaussfit

; 10x faster than fid_faster if using parapeak
; .001 to execute if using parapeak

fidarr = PTRARR(N_ELEMENTS(inputstruct),/allocate_heap)
for rr = 0,N_ELEMENTS(inputstruct)-1 do begin
    k=0
    z=FLTARR(3,3,/nozero)
    
    ; cropped-down image of sun
    ; crop = FLOAT(inputimage[inputstruct[rr].limbxpos - !param.crop_box:inputstruct[rr].limbxpos + !param.crop_box,inputstruct[rr].limbypos - !param.crop_box:inputstruct[rr].limbypos + !param.crop_box])

    crop = FLOAT(inputimage[inputstruct[rr].limbxpos - 40:inputstruct[rr].limbxpos + 40,inputstruct[rr].limbypos - 40:inputstruct[rr].limbypos + 40])







    ; Do something where the bright pixel is exempt
    a = [where(crop eq max(crop))]

    xa = a mod (size(crop,/dim))[0]
    xy = a / (size(crop,/dim))[0]

    crop[xa,xy]=mean([crop[xa,xy-1],crop[xa,xy+1],crop[xa-1,xy],crop[xa+1,xy]])







    ; plots of x and y totals to identify rows/columns of fiducials
    yt = TOTAL(crop,1)
    xt = TOTAL(crop,2)

    ; array of differences between sum profile and smoothed sum profile
    ysums = yt - SMOOTH(yt, !param.fid_smooth_candidates)
    xsums = xt - SMOOTH(xt, !param.fid_smooth_candidates)

    ; Identify where it looks like a fiducial
    if !param.rough1dsum_thresh eq 1 then begin
        case inputstruct[rr].reg of
           1 : athresh = -500
           2 : athresh = -250
           3 : athresh = -125
        endcase
    endif else begin
        case inputstruct[rr].reg of
           1 : athresh = -100
           2 : athresh = -100
           3 : athresh = -20
        endcase   

        ; Have to rule out the side peaks, good thing they're always the 2 lowest peaks
        wmin = where(xsums eq min(xsums)) 
        xsums[wmin-3:wmin+3]=0
        wmin = where(xsums eq min(xsums)) 
        xsums[wmin-3:wmin+3]=0

        wmin = where(ysums eq min(ysums)) 
        ysums[wmin-3:wmin+3]=0
        wmin = where(ysums eq min(ysums)) 
        ysums[wmin-3:wmin+3]=0         
    endelse
    
    yfids = WHERE(ysums le athresh)
    xfids = WHERE(xsums le athresh)

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

    fidpos = REPLICATE({fidpos,x:0.,y:0.,subx:0.,suby:0.},FACTORIAL(N_ELEMENTS(xx)>N_ELEMENTS(yy)))
    ; tmp = crop
    ; Loop through each x and y position combination 
    for i = 0,N_ELEMENTS(xx)-1 do begin
        for j = 0,N_ELEMENTS(yy)-1 do begin

            ; To eliminate coords that are just solar pixels and not fiducials (on disk)
            if crop[xx[i],yy[j]] lt !param.disk_brightness then begin
            
                aa = crop[xx[i] - !param.fid_crop_box:xx[i] + !param.fid_crop_box,yy[j] - !param.fid_crop_box:yy[j] + !param.fid_crop_box]

                rowsum=TOTAL(aa,1) ; Summing rows to get a y position profile
                if !param.rough1dsum_thresh eq 2 then rowsum = rowsum[where(rowsum gt 1000)]
                ysum=SMOOTH(rowsum, !param.fidcand_smooth)-rowsum   ;The array we're thresholding
                bw = WHERE(ysum gt !param.onedsumthresh,n_bw)

                colsum=TOTAL(aa,2)
                if !param.rough1dsum_thresh eq 2 then colsum = colsum[where(colsum gt 1000)]
                xsum=SMOOTH(colsum, !param.fidcand_smooth)-colsum
                dw = WHERE(xsum gt !param.onedsumthresh,n_dw)

                if n_bw ne 0 and n_dw ne 0 then begin
                        ; tmp[xx[i]-1:xx[i]+1,yy[j]-1:yy[j]+1]=255
                        ; tmp[xx[i],yy[j]]=255
                        ; window,0
                        ; cgimage,aa,/k

                        ; !P.charsize=.7
                        ; window,1
                        ; !p.multi=[0,2,2]
                        ; ps_start,filename='small_image.eps',/encap
                        ; plot,colsum
                        ; oplot,smooth(colsum,5),linestyle=1
                        ; plot,rowsum
                        ; oplot,smooth(rowsum,5),linestyle=1
                        ; plot,xsum
                        ; plot,ysum
                        ; ps_end
                        ; !p.multi=0



                        ; how to deal with false positives? Especially with smoothing artifacts

                        ; smoothing with /edge_X doesn't work
                        ; using col/rowsum above 1k is iffy
                        ; using dif threshold is iffy
                        ; As long as we have 3 though, we should be ok?
; stop
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
                        
                        ; Offset the subpixel location correctly
                        fidpos[k].subx = maxx + result[0] + xx[i] - !param.fid_crop_box
                        fidpos[k].suby = maxy + result[1] + yy[j] - !param.fid_crop_box
                        
                        k++
                endif
                ; if k eq N_ELEMENTS(xx)>N_ELEMENTS(yy) then break                
                if k eq FACTORIAL(N_ELEMENTS(xx)>N_ELEMENTS(yy)) then break
            endif
        endfor
    endfor
    ; stop
    ; window,inputstruct[rr].reg
    ; cgimage,tmp,/k
    *(fidarr[rr])=CREATE_STRUCT('reg',inputstruct[rr].reg,'fidarr',fidpos)  
endfor

RETURN,fidarr
end