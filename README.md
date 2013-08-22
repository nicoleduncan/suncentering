suncentering
=============

Overview
-------
Starting with an image of a 100% brightness sun, a 50% and 25% sun, we find the centers of each and align them to background fiducials. 

<!-- ### Code Overview
* `alpha` -- Loads the image, sets necessary variables, and prints out solar centers
* `defsysvarthresh` -- Defines thresholds to mask the solar regions. Dynamic.
* `everysun` -- Finds the centers of each sun-shaped object
* `picksun` -- Eliminates suns that are cut off the ends of our image
* `limbfit` -- Finds a more accurate center of a whole sun using solar limbs
* `fid_locate` -- Identifies and returns the positions of fiducials within a cropped subsolar region -->

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


To-Do
-------
* Horizon sun sensor
* Once 4 closest fiducials to solar centers are found, how to identify which one