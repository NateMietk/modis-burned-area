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
from netCDF4 import Dataset
import os
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, Polygon, MultiPolygon
import sys

# Set working directory to the repo root and add path to functions
frame = getframeinfo(currentframe()).filename
file_path = os.path.dirname(os.path.abspath(frame))
os.chdir(os.path.join(file_path, '../..'))
sys.path.insert(0, 'src/functions')

# Import objects and methods
from functions import cmaps

# Read in the event table and a reference nc file for geometric information
df = pd.read_csv('data/modis_burn_events_00_19.csv')
sample = Dataset('data/bd_numeric_tiles/netcdfs/h08v04.nc')
crs = sample.variables['crs']
geom = crs.geo_transform
proj4 = crs.proj4
res = geom[1]

# Filter columns, center pixel coordinates, and remove repeating pixels
df = df[['id', 'date', 'x', 'y']]
df['x'] = df['x'] + (res/2)
df['y'] = df['y'] + (res/2)
df = df.drop_duplicates(['id', 'x', 'y'])

# We will create a square buffer around each point and then merge these by id
# Each entry in the df gets a point object from the x and y coordinates.
df = df.iloc[:10000, :]  # <--------------------------------------------------- Small sample for now, don't forget to remove this line
def makePoint(x):
    return Point(tuple(x))
df['geometry'] = df[['x', 'y']].apply(makePoint, axis=1)
df = df[['id', 'date', 'geometry']]
gdf = gpd.GeoDataFrame(df, crs=proj4, geometry=df['geometry'])

# Test output of points
color = cmaps['Diverging']
gdf.plot(column='id', cmap=color[0], legend=True)

# Looks good moving on. First create a circle buffer
gdf['geometry'] = gdf.buffer(1 + (res/2))
sample = gdf[gdf['id'] == 3]
sample.plot(column='id', cmap=color[1], legend=True)

# Then create a square envelope around each
gdf['geometry'] = gdf.envelope
sample = gdf[gdf['id'] == 3]
sample.plot(column='id', cmap=color[2], legend=True)

# Now merge these into one by id, using the first date of each event
gdf['start_date'] = gdf.groupby('id')['date'].transform('min')
gdf = gdf[['id', 'start_date', 'geometry']]
gdf = gdf.dissolve(by='id', as_index=False)

# For each geometry, if it is a single, cast as a multi
def asMultiPolygon(polygon):
    if type(polygon) == Polygon:
        polygon = MultiPolygon([polygon])
gdf['geometry'] = gdf['geometry'].apply(asMultiPolygon)

# Calculate perimeter length  # <---------------------------------------------- Check this number against the circular polygons (curious)
gdf['final_perimeter'] = gdf['geometry'].length
gdf.plot(column='id', cmap=color[2])

# Now save as a geopackage
gdf.to_file('data/modis_event_polygons_test.gpkg', driver='GPKG')  # <--------- Run the full event list and check against Adams (remember to change summarize to summarise if there's a problem)
