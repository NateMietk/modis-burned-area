# -*- coding: utf-8 -*-
"""
MODIS detected fire events since 2000 according to spatial and temporal
grouping parameters. This scripts classifies events by MODIS tile and then
groups them together into a dataframe.

Next steps:
    - work this into a module or import as a function in order to work it into
      a single script for everything.
    - make the save folder an argument.
    - possibly, to speed up the post event builder merge step, only merge with
      neighboring tiles.

Created on Thu June 20 2019

@author: travis
"""
import ast
from collections import OrderedDict
import datetime as dt
from glob import glob
import os
import numpy as np
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
from functions import dateRange, edgeCheck, EventGrid, flttn, spCheck

try:
    low_memory = ast.literal_eval(sys.argv[1])
except:
    low_memory = False

# Start the timer (seconds)
start = time.perf_counter()

# Step #1: Build individual tile events
files = glob('data/rasters/burn_area/netcdfs/*nc')
tile_list = []
df = pd.DataFrame(columns=['id', 'date', 'x', 'y', 'duration', 'edge', 'tile'])
base = dt.datetime(1970, 1, 1)
files.sort()
for file in files:
    tile_id = file[-9:-3]
    print('\n' + tile_id)

    # Create event object
    builder = EventGrid(nc_path=file, spatial_param=5, temporal_param=11,
                        low_memory=False)

    # Classify event perimeters
    perimeters = builder.get_event_perimeters_3d()

    # Remove empty perimeters
    perimeters = [p for p in perimeters if type(p.coords[0]) is not str]
    tile_list.append(perimeters)

    # Extract just the event ID, days, and x,y MODIS coordinates
    plist = [(p.get_event_id(), p.coords) for p in perimeters]

    # Now use dates and event counts to organize event perimeters
    date_list = [[dateRange(p), str(p.coords)] for p in perimeters]

    # Identify if it is an edge case, so either x or y is within 5 cells
    geom = builder.data_set.crs.geo_transform
    res = geom[1]
    sp_buf = builder.spatial_param * res
    maxys = builder.data_set['y'].data[:builder.spatial_param]
    minys = builder.data_set['y'].data[-builder.spatial_param:]
    yedges = list(maxys) + list(minys)
    maxxs = builder.data_set['x'].data[-builder.spatial_param:]
    minxs = builder.data_set['x'].data[:builder.spatial_param]
    xedges = list(maxxs) + list(minxs)

    # Create an empty data frame
    print("Building data frame...")
    events = []
    coords = []
    edges = []
    ys = []
    xs = []
    dates = []
    durations = []
    for p in plist:
        coord = [list(c) for c in p[1]]
        edge = [edgeCheck(yedges, xedges, c, sp_buf) for c in coord]
        if any(edge):
            edge = [True for e in edge]
        event = list(np.repeat(p[0], len(coord)))
        y = [c[0] for c in coord]
        x = [c[1] for c in coord]
        date = [base + dt.timedelta(c[2]) for c in coord]
        duration = (max(date) - min(date)).days + 1
        duration = list(np.repeat(duration, len(coord)))
        events.append(event)
        coords.append(coord)
        edges.append(edge)
        ys.append(y)
        xs.append(x)
        dates.append(date)
        durations.append(duration)

    events = flttn(events)
    coords = flttn(coords)
    edges = flttn(edges)
    ys = flttn(ys)
    xs = flttn(xs)
    dates = flttn(dates)
    durations = flttn(durations)
    edf = pd.DataFrame(OrderedDict({'id': events, 'date': dates, 'x': xs,
                                    'y': ys, 'duration': durations,
                                    'edge': edges, 'tile': tile_id}))
    if not os.path.exists('data/tables/events'):
        os.mkdir('data/tables/events')
    if len(edf) > 0:
        # Perhaps add a more unique identifier to this file name
        edf.to_csv('data/tables/events/' + tile_id + '.csv', index=False)

    # Merge with existing records
    df = pd.concat([df, edf])

# Print the time it took
end = time.perf_counter()
seconds = end - start
minutes = seconds/60
print('Job completed in {} minutes'.format(round(minutes, 2)))
  
# Step #2: Clear memory, read saved dataframes and concatenate
# ...
## save in case of failure
#df.to_csv('data/tables/modis_burn_events_interem.csv', index=False)

# Recreate unique ids (id + tile perhaps)
def toDays(date, base):
    delta = (date - base)
    days = delta.days
    return days
df['id'] = df['tile'] + '_' + df['id'].astype(str)
df['days'] = df['date'].apply(toDays, base=base)

# Cut the edge events out into a separate df
edges = df[df['edge'] == True]
not_edges = df[df['edge'] == False]

# Merge where needed
eids = list(edges['id'].unique())
for iden in tqdm(eids, position=0):
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
df.to_csv('data/tables/modis_events.csv', index=False)


