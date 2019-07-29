#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Recreation of 'get_landcover_and_calculate_daily_stats.R'

Created on Sat Jul 13 11:18:51 2019

@author: travis
"""
import datetime as dt
import geopandas as gpd
from glob import glob
import numpy as np
import os
from osgeo import gdal
import pandas as pd
import rasterio
from shapely.geometry import Point
from subprocess import check_output, CalledProcessError
import sys
import time
from tqdm import tqdm
pd.options.mode.chained_assignment = None

# Experimenting with different ways of setting working environment
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')

# Functions
from functions import mode, pquery, toKms

# Start the timer (seconds)
start = time.perf_counter()

# Read in a few needed items for attribute building
ignitions = pd.read_csv('data/tables/ignition_lat_longs.csv')
ecoregions = gpd.read_file('data/shapefiles/ecoregion/us_eco_l3_modis.shp')
template = gdal.Open('data/rasters/landcover/mosaics/us_lc_mosaic_2001.tif')
ecoraster = rasterio.open('data/rasters/ecoregion/us_eco_l3_modis.tif')
conus = gpd.read_file('data/shapefiles/conus_modis.shp')
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
dfd = pd.concat(ll_list)

# Clip by CONUS
dfd = gpd.sjoin(dfd, conus[['STATEFP', 'geometry']], how='left')
dfd = dfd.dropna()

# Now extract the ecoregions for each event
ecoarray = ecoraster.read(1)
dfd['US_L3CODE'] = dfd['geometry'].apply(pquery, args=(ecoraster, ecoarray))

# Getting the landcover legend
if not os.path.exists('data/tables/lc_legend.csv'):
    names = ['Evergreen Needleleaf Forests', 'Evergreen Broadleaf Forests',
             'Deciduous Needleleaf Forests', 'Deciduous Broadleaf Forests',
             'Mixed Forests', 'Closed Shrublands', 'Open Shrublands',
             'Woody Savannas', 'Savannas', 'Grasslands', 'Permanent Wetlands',
             'Croplands', 'Urban and Built-up Lands', 
             'Cropland/Natural Vegetation Mosaics', 'Permanent Snow and Ice',
             'Barren', 'Water Bodies', 'Unclassified']
    codes = [i for i in range(1, 18)]
    codes.append(255)
    legend = pd.DataFrame({'lc': codes, 'lc_name': names})
    legend.to_csv('data/tables/lc_legend.csv', index=False)
ll_legend = pd.read_csv('data/tables/lc_legend.csv')

# Getting the ecoregion legend
eco_refs = pd.read_csv('data/tables/eco_refs.csv')

# Find modal value of lc and eco for each event 
dfe = dfd[['id', 'lc', 'US_L3CODE']].drop_duplicates()
group = dfe.groupby('id')
dfe['lc'] = group['lc'].transform(lambda x: mode(x))
dfe['US_L3CODE'] = group['US_L3CODE'].transform(lambda x: mode(x))
dfe = dfe.drop_duplicates()

# Attach names to landcover and ecoregion codes
dfe = pd.merge(dfe, eco_refs)
dfe = pd.merge(dfe, ll_legend)

# Save file
dfe.to_csv('data/tables/lc_eco_events.csv')

# Next, event/daily level attributes
group = dfd.groupby(['id', 'date'])
dfd['pixels'] = group['id'].transform('count')
dfd['lc'] = group['lc'].transform(lambda x: mode(x))
dfd['US_L3CODE'] = group['US_L3CODE'].transform(lambda x: mode(x))
dfd = dfd[['id', 'date', 'pixels', 'US_L3CODE', 'lc']]
dfd = dfd.drop_duplicates()

# Next, event level attributes
dfd['date'] = dfd['date'].apply(lambda x: dt.datetime.strptime(x, '%Y-%m-%d'))
group = dfd.groupby('id') 
dfd = dfd.sort_values(['id', 'date'])
dfd['cum_pixels'] = group['pixels'].apply(np.cumsum)
dfd['total_pixels'] = group['pixels'].transform('sum')
dfd['ignition_date'] = group['date'].transform('min')
dfd['last_date'] = group['date'].transform('max')

# Now date level calculations will include grouped values
dfd['duration'] = dfd['last_date'] - dfd['ignition_date']
dfd['duration'] = dfd['duration'].apply(lambda x: x.days) + 1
dfd['simple_fsr_pixels'] = dfd['total_pixels'] / dfd['duration']
dfd['simple_fsr_km2'] = dfd['simple_fsr_pixels'].apply(toKms, res=res[0])
dfd['daily_area_km2'] = dfd['pixels'].apply(toKms)
dfd['cum_area_km2'] = dfd['cum_pixels'].apply(toKms)
dfd['total_area_km2'] = dfd['total_pixels'].apply(toKms)
dfd['pct_total_area'] = (dfd['pixels'] / dfd['total_pixels']) * 100
dfd['pct_cum_area'] = (dfd['pixels'] / dfd['cum_pixels']) * 100
dfd['event_day'] = dfd['date'] - dfd['ignition_date']
dfd['event_day'] = dfd['event_day'].apply(lambda x: x.days) + 1
dfd['ratio_area_added_to_average'] = (dfd['daily_area_km2'] / 
                                      dfd['simple_fsr_km2'])
dfd['prior_pixels'] = dfd['cum_pixels'] - dfd['pixels']
dfd['rel_fsr_per_day'] = 0 
dfd['rel_fsr_per_day'][dfd['prior_pixels'] > 0] = (dfd['pixels'] /
                                                   dfd['prior_pixels'])

# Attach names to landcover and ecoregion codes
dfd = pd.merge(dfd, eco_refs)
dfd = pd.merge(dfd, ll_legend)

# Save daily attributes
dfd.to_csv('data/tables/daily_stats_w_landcover.csv', index=False)

# Print the time it took
end = time.perf_counter()
seconds = end - start
minutes = seconds/60
print('Job completed in {} minutes'.format(round(minutes, 2)))