; docformat = 'rst'
;
; PURPOSE:
;   The purpose of this function is to warp an image into a map projection, given
;   latitude and longitude values for each data point. It is similar to MAP_PATCH in
;   IDL.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
;
;+
; The purpose of this function is to warp an image into a map projection, given
; latitude and longitude values for each data point. It is the Coyote Graphics
; equivalent of MAP_PATCH in IDL.
; 
; The program is absolutely dependent on HIST_ND, a program written by JD Smith and included
; in the "public" folder of the Coyote Library.
;   
; :Categories:
;    Graphics, Map Projections
;    
; :Returns:
;     An output 2D grid in which the data points have been warped into the
;     particular map projection at the requested pixel resolution.
;       
; :Params:
;    data:  in, required, type=numerical
;        A 1D vector or 2D image (m x n). Longitude and latitude values must be present
;        (or easily calculated) for each element in the data array. 
;    lons: in, required, type=float
;        If data is a vector, a vector of longitude values corresponding to each
;        data value. Values must be in the range -180 to 360. If data is 2D, either
;        a 1D vector or a 2D array of corresponding longitude values. If data is 2D,
;        and the LONS parameter is missing, a vector of appropriate length scaled into
;        the range -180.0 to 180.0 will be created.
;    lats: in, required, type=float
;        If data is a vector, a vector of latitude values corresponding to each
;        data value. Values must be in the range -90 to 90. If data is 2D, either
;        a 1D vector or a 2D array of corresponding latitude values. If data is 2D,
;        and the LONS parameter is missing, a vector of appropriate length scaled into
;        the range -90.0 to 90.0 will be created.
;               
; :Keywords:
;    map: in, optional, type=object
;       An input map projection object (cgMap). If provided, the data will be gridded into
;       this map projection. If not provided, a map object using a equirectangular map projection
;       with a spherical datum will be used. The XRANGE and YRANGE properties of the map object
;       will be set by the program in the course of doing the gridding if the `SetRange` keyword is
;       set.
;    missing: in, optional, type=varies
;       Missing data in the gridding process will be set to this value.
;    nosetrange: in, optional, type=boolean, default=1
;       If this keyword is set, the XRANGE and YRANGE parameters of the cgMap object will NOT
;       be set to the output X and Y ranges.
;    resolution: in, optional, type=integer
;       A two-element array giving the pixel resolution of the output array in X and Y.
;       The default is a 600x600 array.
;    xrange: out, optional, type=float
;       The output X range in projected meter space (usually associated with the longitude).
;    yrange: out, optional, type=float
;       The output Y range in projected meter space (usually associated with the latitude).
;       
; :Examples:
;    To display a GOES image with map annotations::
;        fileURL = 'http://www.idlcoyote.com/misc/goes_example_data.sav'
;        filename = "goes_example_data.sav"
;        netObject = Obj_New('IDLnetURL')
;        void = netObject -> Get(URL=fileURL, FILENAME=filename)
;        Obj_Destroy, netObject
;        Restore, filename 
;        peru_lat = Temporary(peru_lat) / 10000.
;        peru_lon = Temporary(peru_lon) / 10000.
;        s = Size(peruimage, /DIMENSIONS)
;        centerLat = peru_lat[s[0]/2, s[1]/2]
;        centerLon = peru_lon[s[0]/2, s[1]/2]
;        map = Obj_New('cgMap', 'Albers Equal Area', Ellipsoid='sphere', /OnImage, $
;           STANDARD_PAR1=-19, STANDARD_PAR2=20, CENTER_LAT=centerLat, CENTER_LON=centerLon)
;        warped = cgWarpToMap(peruImage, peru_lon, peru_lat, MAP=map, MISSING=0, $
;            Resolution=[400, 300], /SetRange)
;        cgDisplay, /Free, Title='Warped Image with cgWarpToMap'
;        cgImage, warped, Stretch=2, Position=[0,0,1,1]
;        map -> Draw
;        cgMap_Grid, Map=map, /Label, Color='goldenrod'
;        cgMap_Continents, MAP=map, Color='goldenrod'
;        cgMap_Continents, MAP=map, Color='goldenrod', /Countries
;        
;    Additional examples can be found here: http://www.idlcoyote.com/map_tips/warptomap.php.
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;        David W. Fanning 
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; :History:
;     Modification History::
;        Written by David W. Fanning, 12 Sept 2012.
;        Modifications to accommodate lat/lon arrays that are one-dimensional to go along
;           with 2D data. 13 Sept 2012. DWF.
;        Algorithm completely rewritten to use HIST_ND method. 15 Aug 2013. DWF.
;         
; :Copyright:
;     Copyright (c) 2012-2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgWarpToMap, data, lons, lats, $
   MAP=map, $
   MISSING=missing, $
   NOSETRANGE=nosetrange, $
   RESOLUTION=resolution, $
   XRANGE=xrange, $
   YRANGE=yrange

   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN, data
   ENDIF

   IF N_Params() EQ 0 THEN BEGIN
      Print, 'Calling Syntax: warpedImage = cgWarpImage(image, lons, lats, MAP=map, RESOLUTION=resolution)'
      RETURN, 0
   ENDIF
   
   ; Handle keywords
   IF N_Elements(missing) EQ 0 THEN missing = 0B
   
   ; If no map object, use a Equirectangular grid with a spherical datum.
   IF N_Elements(map) EQ 0 THEN map = Obj_New('cgMap', 'Equirectangular', ELLIPSOID='Sphere')
   
   ; How many dimensions are we dealing with?
   ndims = Size(data, /N_DIMENSIONS)
   datadims = Size(data, /DIMENSIONS)
   
   ; If data is 1D, then lats and lons must be 1D and the same size.
   IF ndims EQ 1 THEN BEGIN
       numElements = N_Elements(data)
       IF (N_Elements(lats) NE numElements) || (N_Elements(lons) NE numElements) THEN BEGIN
           Message, 'The number of latitude and longitude points must be equal to the number of data points.'
       ENDIF
   ENDIF
   
   ; If data is 2D and lats/lons are 1D, then we have to construct a grid.
   IF ndims EQ 2 THEN BEGIN
       IF Size(lats, /N_DIMENSIONS) EQ 1 THEN BEGIN
           lats = Rebin(Reform(lats, 1, datadims[1]), datadims[0], datadims[1])
           modifiedLats = 1
       ENDIF
       IF Size(lons, /N_DIMENSIONS) EQ 1 THEN BEGIN
           lons = Rebin(lons, datadims[0], datadims[1])
           modifiedLons = 1
       ENDIF
       numElements = N_Elements(data)
       IF (N_Elements(lats) NE numElements) || (N_Elements(lons) NE numElements) THEN BEGIN
           Message, 'The number of latitude and longitude points must be equal to the number of data points.'
       ENDIF
   ENDIF

   ; If data is ND, the lats/lons must be 2D and same size as X and Y dimensions of data.
   IF ndims GT 2 THEN BEGIN
       numElements = datadims[0]*datadims[1]
       IF (N_Elements(lats) NE numElements) || (N_Elements(lons) NE numElements) THEN BEGIN
           Message, 'The number of latitude and longitude points must be equal to the number of XY data points.'
       ENDIF
   ENDIF
     
   ; Make sure the longitudes are in the range -180 to 180.
   lons = ((lons + 180) MOD 360) - 180
   
   ; Convert lats/lons to XY projected meter space.
   xy = map -> Forward(lons, lats)

   ; Make sure the resolution is a 2-element array.
   IF N_Elements(resolution) EQ 0 THEN resolution = [600,600]
   IF N_Elements(resolution) EQ 1 THEN resolution = [resolution, resolution] ELSE resolution = [resolution[0:1]]
   
   ; The key to this warping algorithm is performing a 2D histogram on
   ; the lat/lon arrays, can being able to return the indices in the resulting
   ; array. We use JD Smith's HIST_ND routine for this, specifically because
   ; he returns the REVERSE_INDICES. Note that I am using the variable "binsize"
   ; as a return positional parameter in this call. I'm pretty sure JD didn't
   ; intend this, but it is extraordinarily useful.
   result = Hist_ND(xy, binsize, NBINS=resolution, REVERSE_INDICES=ri)  
   
   ; Now we are ready to do the warping of the data. 
   s = Size(result, /Dimensions)
   IF ndims GT 2 THEN BEGIN
       warpedData = Make_Array(DIMENSION=[s[0],s[1],datadims[2]], TYPE=Size(data, /TYPE))
   ENDIF ELSE BEGIN
        warpedData = Make_Array(DIMENSION=s, TYPE=Size(data, /TYPE))
   ENDELSE

   IF ndims GT 2 THEN loopCnt = datadims[2] ELSE loopCnt = 1

   FOR k = 0, loopcnt-1 DO BEGIN
       warp = Make_Array(DIMENSION=s[0:1], TYPE=Size(data, /TYPE)) + missing
       goodIndices = Where(result NE 0, goodCnt)
       thisData = data[*,*,k]
       IF goodCnt GT 0 THEN BEGIN
           FOR j=0L,goodCnt-1 DO BEGIN
               indices = cgReverseIndices(ri, goodIndices[j], COUNT=count)
               CASE count OF
                   0:
                   1: warp[goodIndices[j]] = thisData[indices]
                   ELSE: warp[goodIndices[j]] = Median(thisData[indices])
               ENDCASE
           ENDFOR
       ENDIF
       ;warpedData[0,0,k] = Morph_Close(warp, kernel, /Gray, /Preserve_Type)
       warpedData[0,0,k] = warp
   ENDFOR

   ; Find the appropriate XRANGE and YRANGE for this map projection.
   min_x = Min(xy[0,*])
   min_y = Min(xy[1,*])
   warpSize = Size(warp, /Dimensions)
   xrange = [min_x, min_x + (binsize[0]*warpSize[0])]
   yrange = [min_y, min_y + (binsize[1]*warpSize[1])]
   
   ; If the user asked you to set the range, do it now.
   IF ~Keyword_Set(nosetrange) THEN map -> SetProperty, XRANGE=xrange, YRANGE=yrange
   
   IF Keyword_Set(modifiedLats) THEN lats = Reform(lats[0,*])
   IF Keyword_Set(modifiedLons) THEN lons = lons[*,0]

   ; Return the warped data.
   RETURN, warpedData
END