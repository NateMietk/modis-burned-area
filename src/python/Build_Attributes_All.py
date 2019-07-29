#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Building the full event attribute dataset.

Created on Wed Jul 17 22:33:54 2019
@author: travis
"""
import datetime as dt
import geopandas as gpd
from netCDF4 import Dataset
import numpy as np
import os
import pandas as pd
from shapely.geometry import Point, Polygon, MultiPolygon
import shapely.speedups
from subprocess import check_output, CalledProcessError
import sys
import time
pd.options.mode.chained_assignment = None
shapely.speedups.enable()

# Experimenting with different ways of setting working environment
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')

# Functions
from functions import  maxGrowthDate, toAcres, toHa, toKms

# Start the timer (seconds)
start = time.perf_counter()

# Read in the event table and a reference nc file for geometric information
print('Reading classified fire event table...')
df = pd.read_csv('data/tables/modis_burn_events_00_19.csv')
sample = Dataset('data/rasters/burn_area/netcdfs/h08v04.nc')
conus = gpd.read_file('data/shapefiles/conus_modis.shp')
conus = conus.rename(columns={'NAME': 'state'})
crs = sample.variables['crs']
geom = crs.geo_transform
proj4 = crs.proj4
res = [geom[1], geom[-1]]

# Filter columns, center pixel coordinates, and remove repeating pixels
df = df[['id', 'date', 'x', 'y']]
df['x'] = df['x'] + (res[0]/2)
df['y'] = df['y'] + (res[1]/2)
df = df.drop_duplicates(['id', 'x', 'y'])

# Clip by CONUS
#def makePoint(x):
#    return Point(tuple(x))
#df['geometry'] = df[['x', 'y']].apply(makePoint, axis=1)
#gdf = gpd.GeoDataFrame(df, crs=proj4, geometry=df['geometry'])
#df = gpd.sjoin(gdf, conus[['STATEFP', 'geometry']], how='left')  # <---------- This shouldn't be necessary because the ecoregion file is CONUS
#df = df.dropna()

# Now add attributes to event df
print('Calculating event level fire attributes...')
df['pixels'] = df.groupby(['id', 'date'])['id'].transform('count')
group = df.groupby('id')
max_rate_dates = group[['date', 'pixels']].apply(maxGrowthDate)
df['total_pixels'] = group['pixels'].transform('sum')
df['date'] = df['date'].apply(lambda x: dt.datetime.strptime(x, '%Y-%m-%d'))
df['ignition_date'] = group['date'].transform('min')
df['ignition_day'] = df['ignition_date'].apply(
                                 lambda x: dt.datetime.strftime(x, '%j'))
df['ignition_month'] = df['ignition_date'].apply(lambda x: x.month)
df['ignition_year'] = df['ignition_date'].apply(lambda x: x.year)
df['last_date'] = group['date'].transform('max')
df['duration'] = df['last_date'] - df['ignition_date']
df['duration'] = df['duration'].apply(lambda x: x.days + 1)
df['total_area_km2'] = df['total_pixels'].apply(toKms, res=res[0])
df['total_area_acres'] = df['total_pixels'].apply(toAcres, res=res[0])
df['total_area_ha'] = df['total_pixels'].apply(toHa, res=res[0]) 
df['fsr_pixels_per_day'] = df['total_pixels'] / df['duration'] 
df['fsr_km2_per_day'] = df['total_pixels'] / df['duration']
df['fsr_acres_per_day'] = df['total_pixels'] / df['duration']
df['fsr_ha_per_day'] = df['total_pixels'] / df['duration']
df['max_growth_pixels'] = group['pixels'].transform('max')
df['min_growth_pixels'] = group['pixels'].transform('min')
df['mean_growth_pixels'] = group['pixels'].transform('mean') 
df['fsr_km2_per_day'] = df['fsr_km2_per_day'].apply(toKms, res=res[0])
df['fsr_acres_per_day'] = df['fsr_acres_per_day'].apply(toAcres, res=res[0])
df['fsr_ha_per_day'] = df['fsr_ha_per_day'].apply(toHa, res=res[0]) 
df['max_growth_km2'] = df['max_growth_pixels'].apply(toKms, res=res[0])
df['max_growth_acres'] = df['max_growth_pixels'].apply(toAcres, res=res[0])
df['max_growth_ha'] = df['max_growth_pixels'].apply(toHa, res=res[0]) 
df['min_growth_km2'] = df['min_growth_pixels'].apply(toKms, res=res[0])
df['min_growth_acres'] = df['min_growth_pixels'].apply(toAcres, res=res[0])
df['min_growth_ha'] = df['min_growth_pixels'].apply(toHa, res=res[0])
df['mean_growth_km2'] = df['mean_growth_pixels'].apply(toKms, res=res[0])
df['mean_growth_acres'] = df['mean_growth_pixels'].apply(toAcres, res=res[0])
df['mean_growth_ha'] = df['mean_growth_pixels'].apply(toHa, res=res[0])
df['date'] = df['date'].apply(lambda x: x.strftime('%Y-%m-%d'))

df = df[['id', 'total_pixels', 'ignition_date', 'ignition_day',
         'ignition_month', 'ignition_year', 'last_date', 'duration',
         'total_area_km2', 'total_area_acres', 'total_area_ha',
         'fsr_pixels_per_day', 'fsr_km2_per_day', 'fsr_acres_per_day',
         'fsr_ha_per_day', 'max_growth_pixels', 'min_growth_pixels',
         'mean_growth_pixels', 'max_growth_km2', 'max_growth_acres',
         'max_growth_ha', 'min_growth_km2', 'min_growth_acres',
         'min_growth_ha', 'mean_growth_km2', 'mean_growth_acres',
         'mean_growth_ha']]
df = df.drop_duplicates()
df.index = df['id']
df['max_growth_rates'] = max_rate_dates

# Attach names to landcover and ecoregion codes
print('Adding landcover and ecoregion attributes...')
lc = pd.read_csv('data/tables/lc_eco_events.csv')
lc.index = lc['id']
lc = lc[['lc', 'US_L3CODE', 'NA_L3NAME', 'NA_L2NAME', 'NA_L1NAME', 'lc_name']]
ll = pd.read_csv('data/tables/ignition_lat_longs.csv')
ll = ll[['id', 'ignition_state', 'latitude', 'longitude']]
ll.columns = ['id', 'ignition_state', 'ignition_latitude',
              'ignition_longitude']
ll.index=ll['id']
ll = ll[['ignition_state', 'ignition_latitude', 'ignition_longitude']]
df = df.join(lc)
df = df.join(ll)
df = df.dropna()

# Save event level attributes
print("Saving data frame to 'event_attributes.csv'...")
df.to_csv('data/tables/event_attributes.csv', index=False)
df = pd.read_csv('data/tables/event_attributes.csv')
df = df.drop('id', axis=1)
gdf = gpd.read_file('data/shapefiles/modis_event_polygons.gpkg')
gdf = gdf.join(df).dropna()
gdf.to_file('data/shapefiles/events_w_attributes.gpkg', driver='GPKG')

# Print the time it tookgdf.
end = time.perf_counter()
seconds = end - start
minutes = seconds/60
print('Job completed in {} minutes'.format(round(minutes, 2)))