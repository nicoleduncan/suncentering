suncentering
=============

Overview
-------
Starting with an image of a 100\% brightness sun, a 50\% a d 25\% sun, we find the centers of each and align them to fiducials marked on each sun. 

### Code Overview
* `comp2` -- Finds the center by a simple center-of-mass formula
* `comp3` -- Finds the center by masking all pixels above a threshold and finding the center of mass
* `comp4` -- Saves strips of solar image data
* `comp5` -- Loads complete strips of solar image data and saves only limb-strips
* `comp6` -- Fits an nth-order polynomial to the limb-strips and calculates the center

### * `merrygotrace` 
With the starting position of the 100\% sun, we scan at a constant radius and detect the secondary and tertiary sun. Using geometry, we crop appropriately around the angle and radius of the detected secondary and tertiary sun. 

### Fiducial Finding (`fidf`)
As it currently stands, using two edge-detection filters and calculating the average row/column position of pixels above a threshold. Code breaks if the fiducials are rotated, however. Does it really matter/will this situation ever arise? Looking into this.

To-Do
-------
Horizon sun sensor