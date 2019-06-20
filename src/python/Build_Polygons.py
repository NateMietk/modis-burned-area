# -*- coding: utf-8 -*-
"""
A translation of 'src/r/events_to_polygons.R'

Notes:
    - The python environment hasn't been specifically set, I usually use a
      virtual environment.

Created on Thu Jun 20 09:40:59 2019

@author: Travis
"""
from inspect import currentframe, getframeinfo
import os
import pandas as pd
import sys
from netCDF4 import Dataset

# Set working directory to the repo root and add path to functions
frame = getframeinfo(currentframe()).filename
file_path = os.path.dirname(os.path.abspath(frame))
os.chdir(os.path.join(file_path, '../..'))
sys.path.insert(0, os.path.join(file_path, '../functions'))

# Import functions
from functions import flttn

# Read in the event table and a reference nc file for geometric information
df = pd.read_csv('data/modis_burn_events_00_19.csv')
sample = Dataset('data/bd_numeric_tiles/netcdfs/h08v04.nc')
geom = sample.variables['crs'].geo_transform
res = geom[1]

# Filter columns, center pixel coordinates, and remove repeating pixels
df = df[['id', 'date', 'x', 'y']]
df['x'] = df['x'] + (res/2)
df['y'] = df['y'] + (res/2)

