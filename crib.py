#/usr/local/bin/python

import pdb
import numpy as np
import scipy as sp
import pyfits
import pylab
import IPython
from PIL import Image
from scipy import signal as sg

xpos = 240
ypos = 140

# Quick way to run:
# execfile('crib.py')

# Let's just do a small thing

hdu = pyfits.open('betterfake.fits')
raw = hdu[0].data

rad=40

crop = raw[ypos-rad:ypos+rad,xpos-rad:xpos+rad]
kernel = [[1,1,1],[0,0,0],[-1,-1,-1]]
a=sg.convolve2d(crop,kernel,mode='valid')

# b=Image.open(a)
# b.show()
# aa = Image.fromarray(crop)
# a.show()