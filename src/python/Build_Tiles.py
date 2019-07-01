# -*- coding: utf-8 -*-
"""
Build NetCDF files of MODIS tiles from monthly tif files.

Created on Thu Jun 20 11:12:20 2019

@author: Travis
"""
from glob import glob
import numpy as np
import os
from subprocess import check_output, CalledProcessError
import sys 

# Set working directory to the repo root and add path to functions
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')

# Import functions
from functions import buildNC

# Get File Paths
tile_folders = glob('data/rasters/burn_area/hdfs/*')
tile_ids = np.unique([f[-6:] for f in tile_folders])
tile_files = {}
for tid in tile_ids:
    files = glob(os.path.join('data/rasters/burn_area/hdfs', tid, '*hdf'))
    tile_files[tid] = files

# Loop through tile ids and build tiles from all tid in each
for tid in tile_ids:
    files = tile_files[tid]
    savepath = 'data/rasters/burn_area/netcdfs'
    buildNC(files, savepath)
