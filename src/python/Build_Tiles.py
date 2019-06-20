# -*- coding: utf-8 -*-
"""
Build NetCDF files of MODIS tiles from monthly tif files.

Created on Thu Jun 20 11:12:20 2019

@author: Travis
"""
from glob import glob
from inspect import currentframe, getframeinfo
import numpy as np
import os
import sys 

# Set working directory to the repo root and add path to functions
frame = getframeinfo(currentframe()).filename
file_path = os.path.dirname(os.path.abspath(frame))
os.chdir(os.path.join(file_path, '../..'))
sys.path.insert(0, os.path.join(file_path, '../functions'))

# Import functions
from functions import buildNC

# Get File Paths
files = glob('data/bd_numeric_tiles/*tif')
tile_ids = np.unique([f[-10:-4] for f in files])
tile_files = {tid: [f for f in files if f[-10:-4] == tid] for tid in tile_ids}

# Loop through tile ids and build events from all tid in each
for tid in tile_ids:
    print(tid)
    files = tile_files[tid]
    savepath = 'data/bd_numeric_tiles/netcdfs'
    buildNC(files, savepath)
