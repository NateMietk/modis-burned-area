# -*- coding: utf-8 -*-
"""
I might wrap this up into a singular module later, but for now I'd like this
to get all of the data needed to both calculate the burn events and perform
all of the postprocessing.

Created on Fri Jun 21 13:20:45 2019

@author: Travis
"""
import os
from subprocess import check_output, CalledProcessError
import sys

# Experimenting wit different ways of doing this (temporary)
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')

# Import data download methods
# from firedpy import Data_Getter
from functions import Data_Getter

# Create data object (you can specify which tiles as an attribute)
data = Data_Getter()

# Use shapeToTiles to download tiles that intersect with a shapefile
data.shapeToTiles('http://www2.census.gov/geo/tiger/GENZ2016/shp/' +
                  'cb_2016_us_state_20m.zip')

# Set tiles to an empty list to download all available tiles (268 out of 648)
data.tiles = []

# Get various shapefiles for attributes
data.getShapes()

# Get Landcover hdfs - register at https://urs.earthdata.nasa.gov/users/new
data.landcover_file_root = 'world_lc_mosaic_'  # 'us_lc_mosaic_'
data.getLandcover()

# Get all of the MODIS burn area hdfs
data.getBurns()
