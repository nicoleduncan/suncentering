FUNCTION micro_makelimbstrips, chord, thresh
;+
;   :Description:
;       Does a quick look at a limb of a chord and returns some positions to compare against fiducial positions
;
;   :Params:
;       chord: in, required, type=byte
;           Chord data we check limb for
;       thresh: in, required, type=float
;           Threshold to judge limb-crossing position
;
;   :Keywords:
;
;-

; How many pixels above/below the threhsold we count
limbwidth = BYTE( !param.ministrip_length)/2
block = {startpoints:BYTARR( !param.ministrip_length),endpoints:BYTARR( !param.ministrip_length),startindex:0,endindex:0,startloc:BYTARR( !param.ministrip_length),endloc:BYTARR( !param.ministrip_length),isitbad:0}

; Where our full-length chord is greater than the threshold
above_thresh = WHERE(chord gt thresh)

; startpoints is the cut down strip with length = ministrip_length and contains
; the indices from row_where[0] +/- limbwidth
block.startpoints   = chord[above_thresh[0] - limbwidth:above_thresh[0] + limbwidth - 1]
block.startindex    = FIX(above_thresh[0] - limbwidth)
block.startloc      = above_thresh[0] - limbwidth
block.endpoints     = chord[above_thresh[-1] - limbwidth+ 1:above_thresh[-1] + limbwidth]   
block.endindex      = FIX(above_thresh[-1] - limbwidth)
block.endloc        = above_thresh[-1] - limbwidth + 1

RETURN,block
end