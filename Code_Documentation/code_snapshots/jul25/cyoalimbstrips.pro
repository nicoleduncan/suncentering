FUNCTION cyoalimbstrips, inputstruct, inputimage
;+
;   :Description:
;       Makes limb strips from whole strips
;
;   :Params:
;       inputstruct: in, required
;           Structure containing all the solar information
;
;       inputimage: in, required
;           The raw input image
;
;   Fun trivia, the name is an acronym for "Choose Your Own Adventure", meaning this program scales to whatever limb width you want.
;-

a = makestrips(inputstruct,inputimage)
; How many pixels above/below the threhsold we count
limbwidth = BYTE( !param.ministrip_length)/2

for jj = 0,N_ELEMENTS(inputstruct)-1 do begin
    for i = 0, !param.nstrips - 1 do begin
        ; Where our full-length chord is greater than the threshold
        col_where = WHERE((a[jj].ystrips)[i].ARRAY gt a[jj].thresh)
        row_where = WHERE((a[jj].xstrips)[i].ARRAY gt a[jj].thresh)

        a[jj].limbxstrips[i].rowindex = a[jj].xstrips[i].rowindex
        a[jj].limbystrips[i].colindex = a[jj].ystrips[i].colindex

            ; startpoints is the cut down strip with length = ministrip_length and contains
            ; the indices from row_where[0] +/- limbwidth
            
            a[jj].limbxstrips[i].startpoints  = $
                (a[jj].xstrips[i].array)[row_where[0] - limbwidth:row_where[0] + limbwidth - 1]  
            ; begindex is the index of the strip where it begins. 
            a[jj].limbxstrips[i].begindex   = FIX(row_where[0] - limbwidth)

            ; tarr = (a[jj].xstrips[i].array)
            ; limb = replicate(!values.f_nan,241)

            ; limb[row_where[0]-limbwidth:row_where[0]+1] = (a[jj].xstrips[i].array)[row_where[0] - limbwidth:row_where[0]+1]

            ; limb[row_where[-1]-limbwidth+1:row_where[-1]+2] = (a[jj].xstrips[i].array)[row_where[-1] - limbwidth+1:row_where[-1] + 2]
            ; ps_start,filename='redlimbs.eps',/encap,/color
            ; plot,tarr,/ys,title='Chord profile over entire sun'
            ; hline,a[jj].thresh
            ; loadct,2
            ; oplot,limb,thick=6,psym=-4,color=100
            ; ps_end
            ; stop
            a[jj].limbxstrips[i].endpoints  = (a[jj].xstrips[i].array)[row_where[-1] - limbwidth+ 1:row_where[-1] + limbwidth]   
            a[jj].limbxstrips[i].endindex   = FIX(row_where[-1] - limbwidth)
            a[jj].limbystrips[i].startpoints  = (a[jj].ystrips[i].array)[col_where[0] - limbwidth:col_where[0] + limbwidth - 1]   
            a[jj].limbystrips[i].begindex     = FIX(col_where[0] - limbwidth)
            a[jj].limbystrips[i].endpoints  = (a[jj].ystrips[i].array)[col_where[-1] - limbwidth + 1:col_where[-1] + limbwidth]   
            a[jj].limbystrips[i].endindex     = FIX(col_where[-1] - limbwidth)
    endfor
endfor

RETURN,a
end