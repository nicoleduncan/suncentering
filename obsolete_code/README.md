suncentering
=============

Description
-------
Uses a variety of methods to find the center of a solar image.

### Basic Breakdown
* `comp2` -- Finds the center by a simple center-of-mass formula
* `comp3` -- Finds the center by masking all pixels above a threshold and finding the center of mass
* `comp4` -- Saves strips of solar image data
* `comp5` -- Loads complete strips of solar image data and saves only limb-strips
* `comp6` -- Fits an nth-order polynomial to the limb-strips and calculates the center

To-Do
-------
Find a way to deal with fiducials; haven't really had to deal with them so far.