#/usr/local/bin/python

import pdb
import numpy as np
import scipy as sp
import pyfits
import pylab
from PIL import Image

xpos = 240
ypos = 140

# Quick way to run:
# execfile('crib.py')

# Let's just do a small thing

hdu = pyfits.open('betterfake.fits')
raw = hdu[0].data


rad=40

crop = raw[ypos-rad:ypos+rad,xpos-rad:xpos+rad]

# aa = Image.fromarray(crop)
# aa.show()