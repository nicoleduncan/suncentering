;+
; NAME:
; PB_LINES_INTERSECTION
;
; PURPOSE:
; Use this function to find the intersection point of two lines 
; defined as segments (start and endpoints are provided).  For
; now the lines must be coplanar (i.e. 2D).
;
; CALLING SEQUENCE:
; result = PB_LINES_INTERSECTION(LineA, LineB, $
;   [UA = variable], [UB = variable], $
;   [PARALLEL = variable], [COINCIDENT = variable], $
;   [ONSEGMENT = variable], $
;   [ANGLE = angle])
;
; RETURNED VALUE:
; A two element array of the intersection [X, Y].  If the 
; lines are parallel or coincident then the returned 
; value contains NANs and the appropriate output keyword is
; returned as TRUE(non-zero).
;
; ARGUMENTS:
; LINEA and LINEB  4 element arrays that define the start and 
;   end points of the lines in the form [x0,y0,x1,y1] or 
;   [[x0,y0],[x1,y1]]
;
; KEYWORDS:
; ANGLE Set equal to a named variable to retrieve the angle (in radians) between
;   A and B as swept from A to B positive CCW.  The angle is between -pi and +pi 
; UA and UB  The parametric scaling constants calculated from the input
;   points.
; PARALLEL Set this equal to a named variable which is returned
;   as TRUE (non-zero) if the lines are parallel
; COINCIDENT Set this equal to a named variable which is returned
;   as TRUE (non-zero) if the lines are coincident (and parallel, too!)
; ONSEGMENT Set this equal to anamed variable to retrieve a two
;   element flag indicating the point lies within the corresponding 
;   segments.  For example, [1,0] indicates that the point is within
;   the two points defining the A line but it is outside the endpoints 
;   of the B line (but lines A and B still intersect somewhere).
;
; REFERENCE:
; Paul Bourke's Geometry Resources located at
; http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
; http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/
;
; EXAMPLE:
; A = [1.0,1.0,2.0,2.0]
; B = [1.0,2.0,2.0,1.0]
; xy = PB_LINES_INTERSECTION(A,B)
; plot, A[[0,2]],A[[1,3]]
; oplot, B[[0,2]],B[[1,3]]
; plots, xy[0],xy[1],psym = 6
;
; MODIFICATION HISTORY:
; 2007-07-15 BT adapted from description in reference.
; 2012-02-13 BT switched to cg* graphics
;-
;
PRO TESTLINES2, waitInterval

	if (n_elements(waitInterval) EQ 0) then waitInterval = 3
	
	A=[[0.0, 0.0], [5.0, 5.0]] & B = [[5.0, 0.0], [0.0, 5.0]];
    TESTLINES, A, B
    WAIT,waitInterval
    A=[[1.0, 3.0], [9.0, 3.0]] & B = [[0.0, 1.0], [2.0, 1.0]];
    TESTLINES, A, B
    WAIT,waitInterval
    A=[[1.0, 5.0], [6.0, 8.0]] & B = [[0.5, 3.0], [6.0, 4.0]];
    TESTLINES, A, B
    WAIT,waitInterval
    A=[[1.0, 1.0], [3.0, 8.0]] & B = [[0.5, 2.0], [4.0, 7.0]];
    TESTLINES, A, B
    WAIT,waitInterval
    A=[[1.0, 2.0], [3.0, 6.0]] & B = [[2.0, 4.0], [4.0, 8.0]];
    TESTLINES, A, B
    WAIT,waitInterval
    A=[[3.5, 9.0], [3.5, 0.5]] & B = [[3.0, 1.0], [9.0, 1.0]];
    TESTLINES, A, B
    WAIT,waitInterval
    A=[[2.0, 3.0], [7.0, 9.0]] & B = [[1.0, 2.0], [5.0, 7.0]];
END
PRO TESTLINES, A, B

  if n_params() NE 2 then begin
    A= [[1.0,1.0], [2.0,3.0]]
    B =[[4.0, 1.0],[1.5,1.4]]
  endif
  
  XY=PB_LINES_INTERSECTION(A,B, UA = ua, UB = ub, ONSEG = onseg, ANGLE = angle)
  
  cgPLOT, [A[0,*]<B[0,*], A[0,*]>B[0,*]],$
        [A[1,*]<B[1,*], A[1,*]>B[1,*]], $
        PSYM = -4,/NODATA, XSTYLE = 2, YSTYLE = 2
  cgPLOTS, A[0,*],A[1,*],color = cgCOLOR('red')
  cgPLOTS, A[0,1],A[1,1],color = cgCOLOR('red'), PSYM = 4
  cgPLOTS, B[0,*], B[1,*],color = cgCOLOR('blue')
  cgPLOTS, B[0,1], B[1,1],color = cgCOLOR('blue'), PSYM = 5
  cgPLOTS, XY[0],XY[1], PSYM = 6, SYMSIZE = 2, color = cgColor("black")
  
  print, 'XY = ', XY
  PRINT, 'UA = ', UA
  PRINT, 'UB = ', UB
  PRINT, 'ONSEG = ', onseg
  ;PRINT, !RADEG*angle
END


FUNCTION L_I_ANGLE, h1,v1,h2,v2, PI
  ;h1 and h2   horizontal sizes of segments
  ;v1 and v2   vertical sizes of segments
  theta1 = ATAN(v1,h1)
  theta2 = ATAN(v2,h2)
  dtheta = theta2-theta1
  While (dtheta GT PI) do dtheta -= (2*PI)
  While (dtheta LT PI) do dtheta += (2*PI)
RETURN, dtheta
END


FUNCTION PB_LINES_INTERSECTION, AA, BB, $
  UA = ua, UB = ub,$
  PARALLEL = parallel, $
  COINCIDENT = coincident, $
  ONSEGMENT = onSegment,$
  ANGLE = angle, DOUBLE = double
  
  if (SIZE(aa,/TYPE) EQ 5) OR (SIZE(bb,/TYPE) EQ 5) OR KEYWORD_SET(double) then begin
    PI = !DPI
    NAN = !VALUES.D_NAN
    ZERO = 0.0d0
    ONE = 1.0d0
    ANGLE = !VALUES.D_NAN
  endif else begin
    PI = !PI
    NAN = !VALUES.F_NAN
    ZERO = 0.0
    ONE = 1.0
    ANGLE = !VALUES.D_NAN
  endelse
  
  ;LineA or LineB must be [x0,y0,x1,y1]
   
  PARALLEL = 0
  COINCIDENT = 0
  onSegment  = [0B,0B]
  
  ;A = AA[*, BSORT(AA[0,*])]
  ;B = BB[*, BSORT(BB[0,*])]
  
  A = AA
  B = BB
  
  x1 = A[0] & x2 = A[2] & x3 = B[0] & x4 = B[2]
  y1 = A[1] & y2 = A[3] & y3 = B[1] & y4 = B[3]
  
  ;denom = (B[3]-B[1])*(A[2]-A[0]) - (B[2]-B[0])*(A[3]-A[1])
  denom = (y4-y3)*(x2-x1) - (x4-x3)*(y2-y1)
  
  ;ua = (B[2]-B[0])*(A[1]-B[3]) - (B[3]-B[1])*(A[0]-B[2])
  ua = (x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)
  
  ;ub = (A[2]-A[0])*(A[1]-B[3]) - (A[3]-A[1])*(A[0]-B[2])
  ub = (x2-x1)*(y1-y3) - (y2-y1)*(x1-x3)
  
  ;if denom EQ 0 then parallel lines
  if denom EQ 0 then begin
    PARALLEL = 1
    if (PB_POINT_SAME(ua, zero) AND PB_POINT_SAME(ub,zero)) $
     then COINCIDENT = 1
    RETURN, [NAN,NAN]
  endif

  ua = ua/denom
  ub = ub/denom

  onSegment = [ (ua GE ZERO) AND (ua LE ONE), (ub GE ZERO) AND (ub LE ONE)]
  ;onSegment = [ (ua GT ZERO) AND (ua LT ONE), (ub LT ZERO) AND (ub GT -(ONE))]
  x = x1 + ua*(x2-x1)
  y = y1 + ua*(y2-y1)
  
  ;the following is not needed (I think)
;  dxA = PB_POINT_SAME(A[0], A[2]) ? !VALUES.F_NAN : (A[2]-A[0])
;  dxB = PB_POINT_SAME(B[0], B[2]) ? !VALUES.F_NAN : (B[2]-B[0])
;  mA = (A[3]-A[1])/dxA
;  mB = (B[3]-B[1])/dyA

  angle = L_I_ANGLE(AA[2]-AA[0], AA[3]-AA[1], BB[2]-BB[0], BB[3]-BB[1], PI)
  
  RETURN, [[x],[y]]
END