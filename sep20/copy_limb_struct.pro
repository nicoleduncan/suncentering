PRO copy_limb_struct, mainstruct, xlimbstruct, ylimbstruct
;+
;   :Description:
;       Copy limb struct over from x/ylimbinfo struct into main struct
;
;   :Params:
;       mainstruct: in, required, type=structure
;           inputstruct, holds main information
;       xlimbstruct: in, required, type=structure
;           x limb information
;       ylimbstruct: in, required, type=structure
;           y limb information
;
;   :Keywords:
;
;-

mainstruct.limbxstrips.startindex   = xlimbstruct.startindex
mainstruct.limbxstrips.endindex     = xlimbstruct.endindex
mainstruct.limbxstrips.startpoints  = xlimbstruct.startpoints
mainstruct.limbxstrips.endpoints    = xlimbstruct.endpoints
mainstruct.limbxstrips.startloc     = xlimbstruct.startloc
mainstruct.limbxstrips.endloc       = xlimbstruct.endloc
mainstruct.limbxstrips.isitbad      = xlimbstruct.isitbad

mainstruct.limbystrips.startindex   = ylimbstruct.startindex
mainstruct.limbystrips.endindex     = ylimbstruct.endindex
mainstruct.limbystrips.startpoints  = ylimbstruct.startpoints
mainstruct.limbystrips.endpoints    = ylimbstruct.endpoints
mainstruct.limbystrips.startloc     = ylimbstruct.startloc
mainstruct.limbystrips.endloc       = ylimbstruct.endloc
mainstruct.limbystrips.isitbad      = ylimbstruct.isitbad

end