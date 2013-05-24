suncentering
=============

Overview
-------
Starting with an image of a 100\% brightness sun, a 50\% a d 25\% sun, we find the centers of each and align them to fiducials marked on each sun. 

### Code Overview
* `beta` -- Loads the image, sets necessary variables, and prints out solar centers
* `defsysvarthresh` -- Defines thresholds to mask the solar regions. Dynamic.
* `everysun` -- Finds the centers of each sun-shaped object
* `picksun` -- Eliminates suns that are cut off the ends of our image
* `limbfit` -- Finds a more accurate center of a whole sun using solar limbs
* `fid_locate` -- Identifies and returns the positions of fiducials within a cropped subsolar region

To-Do
-------
Horizon sun sensor