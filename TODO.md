TO-DO
=============

Overview
-------
A semi-comprehensive list of all the loose ends in the suncentering code. 

High Priority
-------
* Missing program to match fiducial distances from each other to fiducial IDs
* Have to manually tweak bimask in idsuns.pro for image to work on tritest. Relates to graininess of image.
* Organize parameter tables 
* Sup-pixel fitting doesn't work so well in the original, small image. Related to image resolution and lack of pixels
* Find ideal threshold to use when identifying solar regions, must be half the height of a box function and an inflection point as the box function turns more Gaussian. No matter how blurry the image/array gets, it will consistently cross through this threshold.
* Add explanations of things in pblock, the problem is that the way I have it now is each column is either a variable or a value, haven't set the program to ignore anything past those two columns
* For each cropped-down fiducial we are looking at, recrop it down to a square so that the edge of the fiducial just barely extends past the cropped area. Will result in more accurate 1D sums to find fiducial center
* Remove the top .1% pixels in everysun.pro
* Fix para_fid, there is too much bloat in there 

Low Priority
-------
* Use `sort()` magic for initial threshold, but then again we're scientists, not magicians.
* Create program that takes in all possible slat thicknesses and spits out bundles with the least deviation from 13mm 
* Make picksun_rot rotate in both directions and in intervals other than 45 degrees. Need to find a solution if the bottom corners aren't exactly isosceles triangles
* Fix color in abundle.pro, hard to do overlaying red lines on a black and white image without combining the two color tables