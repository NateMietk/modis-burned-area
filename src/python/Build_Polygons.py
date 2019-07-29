# -*- coding: utf-8 -*-
"""
A translation of 'src/r/events_to_polygons.R'

Notes:
    - Some of the vector manipulations in this step are quite slow. Keep an
      eye out for a stable release of dask-geopandas sometime in late 2019:
          https://github.com/mrocklin/dask-geopandas

Created on Thu Jun 20 09:40:59 2019

@author: Travis
"""
import datetime as dt
import geopandas as gpd
from netCDF4 import Dataset
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

from functions import mode

# Start the timer (seconds)
start = time.perf_counter()

# Read in the event table and a reference nc file for geometric information
print('Reading classified fire event table...')
df = pd.read_csv('data/tables/modis_burn_events_00_19.csv')
sample = Dataset('data/rasters/burn_area/netcdfs/h08v04.nc')
conus = gpd.read_file('data/shapefiles/conus.shp')
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

# Each entry in the df gets a point object from the x and y coordinates.
print('Converting data frame to spatial object...')
def makePoint(x):
    return Point(tuple(x))
df['geometry'] = df[['x', 'y']].apply(makePoint, axis=1)
gdf = df[['id', 'date', 'geometry']]
gdf = gpd.GeoDataFrame(gdf, crs=proj4, geometry=gdf['geometry'])

# Let's go ahead and create the lat lon file here  # <------------------------- Where to put this?
print('Creating WGS84 ignition file...')
wdf = gdf.to_crs(epsg=4326)
wdf['min_date'] = wdf.groupby('id')['date'].transform('min')
wdf = wdf[wdf['date'] == wdf['min_date']]
clip = conus.geometry.unary_union
wdf = wdf[wdf.geometry.intersects(clip)]
lats = wdf['geometry'].apply(lambda p: p.y)
lons = wdf['geometry'].apply(lambda p: p.x)
wdf['latitude'] = lats
wdf['longitude'] = lons
wdf = gpd.sjoin(wdf, conus[['state','STATEFP', 'geometry']], how='left',
                op='intersects')
wdf = wdf[['id', 'date', 'state', 'latitude', 'longitude']]
group = wdf.groupby('id')
wdf['latitude'] = group['latitude'].transform('mean')
wdf['longitude'] = group['longitude'].transform('mean')
wdf['ignition_date'] = group['date'].transform('min')
wdf.to_csv('data/tables/ignition_lat_longs.csv', index=False)

# Why don't developers make mode functions simpler?
wdf['ignition_state'] = group['state'].transform(lambda x: mode(x))
wdf = wdf[['id', 'latitude', 'longitude', 'ignition_date', 'ignition_state']]
wdf = wdf.drop_duplicates()
wdf.to_csv('data/tables/ignition_lat_longs.csv', index=False)

# Now back to MODIS Sinusoidal...create a circle buffer
print("Creating buffer...")
geometry = gdf.buffer(1 + (res[0]/2))
gdf['geometry'] = geometry

# Then create a square envelope around the circle
gdf['geometry'] = gdf.envelope

# Now merge these into one by id, using the first date of each event
print("Dissolving polygons...")
gdf['start_date'] = gdf.groupby('id')['date'].transform('min')
gdf = gdf[['id', 'start_date', 'geometry']]
gdf = gdf.dissolve(by='id', as_index=False)

# For each geometry, if it is a single polygon, cast as a multipolygon
print("Converting polygons to multipolygons...")
def asMultiPolygon(polygon):
    if type(polygon) == Polygon:
        polygon = MultiPolygon([polygon])
    return polygon
gdf['geometry'] = gdf['geometry'].apply(asMultiPolygon)

# Calculate perimeter length
print('Calculating perimeter lengths...')
gdf['final_perimeter'] = gdf['geometry'].length  # <--------------------------- Check accuracy of this, QGIS is slightly different (also check adams)

# Now save as a geopackage  # <------------------------------------------------ Should we also make a shapefile for ESRI users?
print('Saving file...')
gdf.to_file('data/shapefiles/modis_event_polygons.gpkg', driver='GPKG')


# Daily shapefiles?


# Print the time it took
end = time.perf_counter()
seconds = end - start
minutes = seconds/60
print('Job completed in {} minutes'.format(round(minutes, 2)))