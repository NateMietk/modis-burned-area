#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Build shape files, one at a time for now, from MODIS event 
Created on Fri Jun  7 17:35:54 2019

@author: travis
"""
import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.spatial import ConvexHull
from shapely.geometry import Point, Polygon
import time
#from functions import toPolygons

# Event Data Frame
df = pd.read_csv('data/modis_burn_events_00_19.csv')

# Total "burn scar"
def toPolygons(burn_id, df):
    '''
    For each day in a singular event, create a cumulative convex hull
    
    Later, I'd like to add in the event id and capture multiple events. 
    '''
    event = df[df['id'] == burn_id]
    dates = event['date'].drop_duplicates().values
    dates = np.sort(dates)
    point_dict = {}
    polygon_dict = {}
    for i in range(len(dates)):
        edf = event[event['date'] == dates[i]]
        points = [[row['x'], row['y']] for i, row in edf.iterrows()]
        if len(point_dict) == 0:
            point_dict[dates[i]] = points
        else:
            previous_points = point_dict[dates[i-1]]
            points = previous_points + points
            point_dict[dates[i]] = points

    for key, val in point_dict.items():
        if len(val) == 1:
            point = Point(tuple(val[0]))
            shape = point.buffer(50)  # <-------------------------------------- This buffer is a random 50 meters to make point shapes more visible
            polygon = gpd.GeoDataFrame({'date': key, 'geometry': shape},
                                       geometry='geometry', index=[0])
            polygon_dict[key] = polygon
        elif len(val) == 2:
            points = [Point(tuple(v)) for v in val]
            shape = [p.buffer(50) for p in points]
            polygon = gpd.GeoDataFrame({'date': key, 'geometry': shape},
                                       geometry='geometry')
            polygon_dict[key] = polygon
        else:
            val = np.array(val)
            try:
                hull = ConvexHull(val)
            except:
                hull = ConvexHull(val, qhull_options='QJ')  # <---------------- If the points are too planar this jostles them before making the hull
            hull_points = val[hull.vertices]
            shape = Polygon([(row[0], row[1]) for row in hull_points])
            shape = shape.buffer(50)  # <-------------------------------------- Random 50 meters to match point shapes
            polygon = gpd.GeoDataFrame({'date': key, 'geometry': shape},
                                       geometry='geometry', index=[0])
            polygon_dict[key] = polygon
        
    values = [polygon_dict[k].values[0][1] for k, val in polygon_dict.items()] # This could probably be simpler :)
    shp = gpd.GeoDataFrame({'date': dates, 'geometry':values},
                           geometry='geometry')

    return shp

df['year'] = df['date'].apply(lambda x: x[:4])
df = df.sort_values('detections')
df17 = df[df['year'] == '2017']
big_ones = df17['id'][-30000:].drop_duplicates().values

shp = toPolygons(big_ones[1], df)
bounds = shp.geometry.bounds
for i in range(shp.shape[0]):
    shp.loc[[i],'geometry'].plot()
    plt.xlim([bounds.minx.min() - 30000, bounds.maxx.max() + 30000])
    plt.ylim([bounds.miny.min() - 30000, bounds.maxy.max() + 30000])
    plt.title(shp.loc[[i],'date'].values[0])
    time.sleep(.5)
