#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Take the dates from the original HDF4s, convert them to days since 1970-1-1,
save each as a geotiff. What else needs to happen here?

Created on Sat Jun 29 12:37:06 2019

@author: travis
"""
import datetime as dt
from glob import glob
import numpy as np
import os
from osgeo import gdal
from subprocess import check_output, CalledProcessError
from tqdm import tqdm
import sys

# Experimenting wit different ways of doing this
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')

# We need them in days since 1970-1-1, though I can't find where that's done
def convertDate(julien_day, year):
    '''
    Take the year and julien day and convert it to days since Jan 1 1970.
    '''
    base = dt.datetime(1970, 1, 1)
    date = dt.datetime(year, 1, 1) + dt.timedelta(int(julien_day))
    days = date - base
    return days.days

# Now we need to write this to geotiffs, geometry then
def toRaster(array, trgt, geometry, proj, navalue=-9999):
    """
    Writes a single array to a raster with coordinate system and geometric
    information.

    path = target path
    srs = spatial reference system
    """
    ypixels = array.shape[0]
    xpixels = array.shape[1]
    trgt = trgt.encode('utf-8')
    image = gdal.GetDriverByName("GTiff").Create(trgt, xpixels, ypixels,
                                 1, gdal.GDT_Float32)
    image.SetGeoTransform(geometry)
    image.SetProjection(proj)
    image.GetRasterBand(1).WriteArray(array)
    image.GetRasterBand(1).SetNoDataValue(navalue)

# Let's start with one tile
tiles = glob('data/rasters/burn_area/hdfs/*')
tiles.sort()

# Loop through everything, convert dates, and save to tiff
for t in tiles:
    files = glob(os.path.join(t, '*hdf'))
    files.sort()
    for f in tqdm(files, position=0):
        year = int(f[44:48])
        day = f[48:51]
        ds = gdal.Open(f).GetSubDatasets()[0][0]
        hdf = gdal.Open(ds)
        geometry = hdf.GetGeoTransform()
        proj = hdf.GetProjection()
        data = hdf.GetRasterBand(1)
        array = data.ReadAsArray()

        # This is pretty quick
        locs = np.where(array > 0)
        ys = locs[0]
        xs = locs[1]
        locs = [[ys[i], xs[i]] for i in range(len(xs))]
        for l in locs:
            y = l[0]
            x = l[1]
            array[y, x] = convertDate(array[y, x], year)

        # Save as a geotiff...see if we get the same output
        tile = os.path.basename(t)
        tfolder = os.path.join('data/rasters/burn_area/geotiffs/', tile)
        if not os.path.exists(tfolder):
            os.mkdir(tfolder)
        tfile = tile + '_' + str(year) + day + '.tif'
        trgt = os.path.join(tfolder, tfile)
        toRaster(array, trgt, geometry, proj)

