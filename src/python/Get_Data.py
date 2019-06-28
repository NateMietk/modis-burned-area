# -*- coding: utf-8 -*-
"""
A script to download landcover data from NASA's Land Processes Distributed
Active Archive Center, which is an Earthdata thing. You'll need register for a
username and password, but that's free. Fortunately, there is a tutorial on how
to get this data:
    
    https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+Python

sample citation for later:
    ASTER Mount Gariwang image from 2018 was retrieved from
    https://lpdaac.usgs.gov, maintained by the NASA EOSDIS Land Processes
    Distributed Active Archive Center (LP DAAC) at the USGS Earth Resources
    Observation and Science (EROS) Center, Sioux Falls, South Dakota. 2018,
    https://lpdaac.usgs.gov/resources/data-action/aster-ultimate-2018-
    winter-olympics-observer/.

Created on Fri Jun 21 13:20:45 2019

@author: Travis
"""
from inspect import currentframe, getframeinfo
import os
import sys

# Set working directory to the repo root and add path to functions
frame = getframeinfo(currentframe()).filename
file_path = os.path.dirname(os.path.abspath(frame))
os.chdir(os.path.join(file_path, '../..'))
sys.path.insert(0, 'src/functions')

# Import data download methods
from functions import Data_Getter

# Create data object (you can specify which tiles as an attribute)
data = Data_Getter()

# Get US Boundary and Ecoregion shapefiles
data.getShapes()

# Get Landcover hdfs
data.getLandcover()

# Get MODIS Burn tiles
