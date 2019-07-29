#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Creating events, merging tiles and events. I am splitting these steps up for
memory purposes. This will probably require dask.

Created on Sun Jul 28 08:08:36 2019

@author: travis
"""
import datetime as dt
import dask
from glob import glob
import os
import pandas as pd
from subprocess import check_output, CalledProcessError
import sys
import time
from tqdm import tqdm
import warnings
warnings.filterwarnings('ignore')

# Experimenting wit different ways of doing this
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')

# Import functions
from functions import EventGrid, spCheck

# Take savepath from system argument
#savepath = sys.argv[0]
savepath = 'data/tables/modis_burn_events_00_19_world.csv'

# Start the timer (seconds)
start = time.perf_counter()

# We need a few parameters from EventGrid
nc = glob('data/rasters/burn_area/netcdfs/*nc')
builder = EventGrid(nc_path=nc[0], spatial_param=5, temporal_param=11,
                    low_memory=False)
geom = builder.data_set.crs.geo_transform
res = geom[1]
sp_buf = builder.spatial_param * res

# Read in data frames from Build_Events.py
files = glob('data/tables/events/*.csv')

# Concatenate this into a singular data frame
df = pd.concat([pd.read_csv(f) for f in files])

# Recreate unique ids (id + tile perhaps)
def toDays(date, base):
    date = dt.datetime.strptime(date, '%Y-%m-%d')
    delta = (date - base)
    days = delta.days
    return days
base = dt.datetime(1970, 1, 1)
df['id'] = df['tile'] + '_' + df['id'].astype(str)
df['days'] = df['date'].apply(toDays, base=base)

# Cut the edge events out into a separate df
edges = df[df['edge'] == True]
not_edges = df[df['edge'] == False]

# Merge where needed
eids = list(edges['id'].unique())
for iden in tqdm(eids, position=0):
    # check if the file exists.

    # Split, one vs all
    edf = edges[edges['id'] == iden]
    edf2 = edges[edges['id'] != iden]
    days = edf['days']

    # Sometimes these are empty?
    try:
        d1 = min(days) 
        d2 = max(days)
    except:
        pass

    # If there aren't events close enough in time the list will be empty
    edf2 = edf2[(abs(edf2['days'] - d1) < 11) | (abs(edf2['days'] - d2) < 11)]
    eids2 = list(edf2['id'].unique())

    # If there are event close in time, are they close in space?
    for iden2 in eids2:
        edf2 = edges[edges['id'] == iden2]
        ydiffs = [y - edf2['y'].values for y in edf['y'].values]
        xdiffs = [x - edf2['x'].values for x in edf['x'].values]
        ychecks = [spCheck(yds, sp_buf) for yds in ydiffs]
        xchecks = [spCheck(xds, sp_buf) for xds in xdiffs]
        checks = [ychecks[i] * xchecks[i] for i in range(len(ychecks))]
        if any(checks):
            # Merge events! Merge into the earliest event
            d12 = edf2['days'].min()
            if d1 < d12:
                edges['id'][edges['id'] == iden2] = iden
            else:
                edges['id'][edges['id'] == iden] = iden2

# Concatenate edge df back into main df
df = pd.concat([not_edges, edges])

# Reset id values in chronological order
df['first'] = df.groupby('id').days.transform('min')
firsts = df[['id', 'first']].drop_duplicates()
firsts = firsts.sort_values('first')
firsts['new_id'] = range(1, firsts.shape[0] + 1)
idmap = dict(zip(firsts['id'], firsts['new_id']))
df['id'] = df['id'].map(idmap)
df = df.sort_values('id')

# Let's add a duration and detections field
detections = df.groupby('id').size().to_frame('count').reset_index()
detsmap = dict(zip(detections['id'], detections['count']))
df['detections'] = df['id'].map(detsmap)
durations = df.groupby('id').days.apply(lambda x: max(x) - min(x)+1).to_frame()
durmap = dict(zip(durations.index, durations['days']))
df['duration'] = df['id'].map(durmap)

# put these in order
df = df[['id', 'tile', 'date', 'x', 'y', 'duration', 'detections']]

# Finally save
df.to_csv(savepath, index=False)

# Print the time it took
end = time.perf_counter()
seconds = end - start
minutes = seconds/60
print('Job completed in {} minutes'.format(round(minutes, 2)))