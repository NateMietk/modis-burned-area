# -*- coding: utf-8 -*-
"""
A translation of 'src/r/events_to_polygons.R'

Notes:
    - The python environment hasn't been specifically set, I usually use a
      virtual environment.

Created on Thu Jun 20 09:40:59 2019

@author: Travis
"""
import geopandas as gpd
from inspect import currentframe, getframeinfo
from netCDF4 import Dataset
import os
import pandas as pd
from shapely.geometry import Point, Polygon, MultiPolygon
import sys
import time

# Set working directory to the repo root and add path to functions
frame = getframeinfo(currentframe()).filename
file_path = os.path.dirname(os.path.abspath(frame))
os.chdir(os.path.join(file_path, '../..'))
sys.path.insert(0, 'src/functions')

# Import objects and methods
from functions import cmaps

# Start the timer (seconds)
start = time.perf_counter()

# Read in the event table and a reference nc file for geometric information
df = pd.read_csv('data/modis_burn_events_00_19.csv')
sample = Dataset('data/bd_numeric_tiles/netcdfs/h08v04.nc')
crs = sample.variables['crs']
geom = crs.geo_transform
proj4 = crs.proj4
res = [geom[0], geom[1]]

# Filter columns, center pixel coordinates, and remove repeating pixels
df = df[['id', 'date', 'x', 'y']]
df['x'] = df['x'] + (res[0]/2)
df['y'] = df['y'] - (res[1]/2)
df = df.drop_duplicates(['id', 'x', 'y'])

# We will create a square buffer around each point and then merge these by id
# Each entry in the df gets a point object from the x and y coordinates.
def makePoint(x):
    return Point(tuple(x))
df['geometry'] = df[['x', 'y']].apply(makePoint, axis=1)
df = df[['id', 'date', 'geometry']]
gdf = gpd.GeoDataFrame(df, crs=proj4, geometry=df['geometry'])

# Test output of points
color = cmaps['Diverging']

# Looks good moving on. First create a circle buffer
print("Creating Buffer...")
gdf['geometry'] = gdf.buffer(1 + (res[0]/2))
sample = gdf[gdf['id'] == 3]
sample.plot(column='id', cmap=color[1], legend=True)

# Then create a square envelope around each
gdf['geometry'] = gdf.envelope
sample = gdf[gdf['id'] == 3]
sample.plot(column='id', cmap=color[1], legend=True)

# Now merge these into one by id, using the first date of each event
print("Dissolving Polygons...")
gdf['start_date'] = gdf.groupby('id')['date'].transform('min')
gdf = gdf[['id', 'start_date', 'geometry']]
gdf = gdf.dissolve(by='id', as_index=False)
sample = gdf[gdf['id'] == 3]
sample.plot(column='id', cmap=color[1], legend=True)

# For each geometry, if it is a single, cast as a multi
print("Converting polygons to multipolygons...")
def asMultiPolygon(polygon):
    if type(polygon) == Polygon:
        polygon = MultiPolygon([polygon])
    return polygon
gdf['geometry'] = gdf['geometry'].apply(asMultiPolygon)

# Calculate perimeter length  # <---------------------------------------------- Check this number against the circular polygons (curious)
print('Calculating perimeter lengths...')
gdf['final_perimeter'] = gdf['geometry'].length

# Now save as a geopackage
print('Saving file...')
gdf.to_file('data/modis_event_polygons_test.gpkg', driver='GPKG')  # <--------- Cryptic error here, but it appears to work

# Print the time it took
end = time.perf_counter()
seconds = end - start
minutes = seconds/60
print('Job completed in {} minutes'.format(round(minutes, 2)))
