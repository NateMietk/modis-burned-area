#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Recreation of 'get_landcover_and_calculate_daily_stats.R'

Created on Sat Jul 13 11:18:51 2019

@author: travis
"""
import geopandas as gpd
from glob import glob
import os
from osgeo import gdal
import pandas as pd
import rasterio
from shapely.geometry import Point
from subprocess import check_output, CalledProcessError
import sys
from tqdm import tqdm
from tabula import read_pdf
from urllib.request import urlretrieve
pd.options.mode.chained_assignment = None

# Experimenting with different ways of setting working environment
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')

# Read in a few needed items for attribute building
ignitions = pd.read_csv('data/tables/ignition_lat_longs.csv')
ecoregions = gpd.read_file('data/shapefiles/ecoregion/us_eco_l1_modis.shp')
template = gdal.Open('data/rasters/landcover/mosaics/us_lc_mosaic_2001.tif')
ecoraster = gdal.Open('data/rasters/ecoregion/us_eco_l1.tif')
events = pd.read_csv('data/tables/modis_burn_events_00_19.csv')

# Geometry and Geographic system reference
geom = template.GetGeoTransform()
crs = template.GetProjection()
res = [geom[1], geom[-1]]

# Filter event columns, center pixel coordinates, and remove repeating pixels
events = events[['id', 'date', 'x', 'y']]
events['x'] = events['x'] + (res[0]/2)
events['y'] = events['y'] + (res[1]/2)
events = events.drop_duplicates(['id', 'x', 'y'])
events['year'] = events['date'].apply(lambda x: x[:4]).astype(int)

# Now, for each year, find the landcover classification at each event point
def pquery(p, lc, lc_array):
    row, col = lc.index(p.x, p.y)
    lc_value = lc_array[row, col]
    return lc_value
ll = {}
files = glob('data/rasters/landcover/mosaics/*tif')
files.sort()

# Associate the prior years landcover classifications with each event
for i in tqdm(range(len(files)), position=0):
    year = int(files[i][-8:-4])
    if year == 2001:  # Use the same year for the first available year
        lc = rasterio.open(files[i])
    else:
        lc = rasterio.open(files[i-1])  # Use the prior year for all others
    ydf = events[events['year'] == year]
    lc_array = lc.read(1)
    ydf['geometry'] = ydf[['x', 'y']].apply(lambda x: Point(tuple(x)), axis=1)
    ydf = ydf[['id', 'date', 'geometry']]
    gdf = gpd.GeoDataFrame(ydf, crs=crs, geometry=ydf['geometry'])
    gdf['lc'] = gdf['geometry'].apply(pquery, args=(lc, lc_array))
    ll[year] = gdf

# For years that haven't been published, use the most recently available year
final_year = events['year'].max()
for y in tqdm(range(year + 1, final_year + 1), position=0):
    file = os.path.join('data/rasters/landcover/mosaics/', 'us_lc_mosaic_' +
                        str(year) + '.tif')
    lc = rasterio.open(file)
    lc_array = lc.read(1)
    ydf = events[events['year'] == y]
    ydf['geometry'] = ydf[['x', 'y']].apply(lambda x: Point(tuple(x)), axis=1)
    ydf = ydf[['id', 'date', 'geometry']]
    gdf = gpd.GeoDataFrame(ydf, crs=crs, geometry=ydf['geometry'])
    gdf['lc'] = gdf['geometry'].apply(pquery, args=(lc, lc_array))
    ll[y] = gdf

# Concatenate everything together
ll_list = [ll[y] for y in range(2001, final_year + 1)]
lc_df = pd.concat(ll)

# Getting descriptions (the third table in user guide pdf)
if not os.path.exists('data/tables/MCD12Q1_User_guide.pdf'):
    urlretrieve('https://landweb.modaps.eosdis.nasa.gov/QA_WWW/forPage' +
                '/user_guide/MCD12Q1_User_guide.pdf',
                'data/tables/MCD12Q1_User_guide.pdf')
legend = read_pdf('data/tables/MCD12Q1_User_guide.pdf', spreadsheet=True,
                  pages=7)
