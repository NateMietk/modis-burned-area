# -*- coding: utf-8 -*-
"""
I might wrap this up into a singular module later, but for now I'd like this
to get all of the data needed to both calculate the burn events and perform
all of the postprocessing.

Created on Fri Jun 21 13:20:45 2019

@author: Travis
"""
import ast
import os
from subprocess import check_output, CalledProcessError
import sys

## Experimenting wit different ways of doing this (temporary)
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')

try:
    tiles = sys.argv[1]
    tiles = ast.literal_eval(tiles)
except:
    tiles = ["h08v04", "h09v04", "h10v04", "h11v04", "h12v04", "h13v04",
             "h08v05", "h09v05", "h10v05", "h11v05", "h12v05", "h08v06",
             "h09v06", "h10v06", "h11v06"]

# Import data download methods
from functions import Data_Getter

# Create data object (you can specify which tiles as an attribute)
data = Data_Getter()
data.tiles = tiles
print(data.tiles)

# Get various shapefiles for attributes
#data.getShapes()

# Get Landcover hdfs - register at https://urs.earthdata.nasa.gov/users/new
#data.landcover_file_root = 'us_lc_mosaic_'
#data.getLandcover()

# Get all of the MODIS burn area hdfs
data.getBurns()
