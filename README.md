suncentering
=============

Overview
-------
Starting with an image of a 100% brightness sun, a 50% and 25% sun, we find the centers of each and align them to background fiducials. 


## INCOMPLETE SOFTWARE

There is one large chunk of the program that is incomplete, and that is the part where once 4 closest fiducials to each solar center are found, how to identify each of them. This has proven to be a worthy adversary of a problem but once it is solvable. 


### Code Synopsis

* Load a starting image
* Load a parameter table
* Calculate necessary parameters 
* Centroid suns in image
* Ignore suns that are either partially cut off the sides of the image or are too close to the image edge
* Align center positions to background fiducials

### Speed

Emphasis is placed on the speed of the code because only the necessary bits will be converted into C++ and installed on the on-board flight computer of the spacecraft. Real-time image analysis must be fast and robust while ground-based analysis (which we will control how much of is offloaded) can be assigned more time. As a result, many functions and tricks used in the code attempt to use as simple tools as possible, eliminating the need for complicated `HISTOGRAM()` functions and the like (although there are a few of those in there, sorry). 

To give a sense of the speed, a 1290 by 960 image with 3 suns takes about .2 seconds from start to finish, from loading the image to calculating the centers of the suns and analyzing the center position from the 4 closest fiducial marks. 

### Code Overview

*WARNING*. These functions may not be used in all versions of the code. for a complete listing, please see doc_lib/index.html

* `alpha/beta/(some greek letter)` -- Loads the image, sets necessary variables, and prints out solar centers. The program that calls all others.
* `defsysvarthresh` -- Defines thresholds to mask the solar regions. Dynamic.
* `everysun` -- Finds the centers of each sun-shaped object
* `picksun` -- Eliminates suns that are cut off the ends of our image
* `limbfit` -- Finds a more accurate center of a whole sun using solar limbs
* `fid_locate` -- Identifies and returns the positions of fiducials within a cropped subsolar region
* `quickmask` -- Finds the center of a mask where pixels are above a certain threshold
* `makestrips` -- Make full-length strips based on approximate solar center
* `centroidwholesuns` -- Finds the centers of a triple-sun image and appends offsets and angles into a new structure. Currently disabled since we don't know exactly how we want to organize the offsets/angles
* `setbetterpeak` -- Finds peaks in 2nd derivative of sorted image data
* `picksun` -- Decides which suns to ignore. Utilizes two bottom corner masks as a "BAD" zone. 
* `picksun_rot` -- An improvement on picksun, uses coordinate rotation instead of masking; much faster thanks to Gordon.
* `para_fid` -- Fits two one-dimensional parabolas (one in each direction) to a fiducial and calculates the intersection for subpixel accuracy
* `npixfit` -- Linear fit to limb strips with an arbitrary number of lmb pixels
* `idsuns` -- Defines solar regions, uses IDL's LABEL_REGION which must be eschewed.
* `defparams` -- Defines global parameters from an external parameter file
* `cyoalimbstrips` -- The most recent iteration of "make_limb_strips.pro"; makes trimmed-down limb strips of data from full-length strips provided from makestrips.
* `best4` -- Chooses the 4 closest fiducials from the center of a given sun.


To-Do
-------
* Horizon sun sensor
* Once 4 closest fiducials to solar centers are found, how to identify which one
* Organize directory, needs to be more obvious what to run


<!-- Analysis programs that fit polynomials to edges of slats in a mask iamge
abundle.pro
hbundle.pro
bundle.pro -->