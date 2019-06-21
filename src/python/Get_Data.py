# -*- coding: utf-8 -*-
"""
A script to download landcover data from NASA's Land Processes Distributed
Active Archive Center.

sample citation:
    ASTER Mount Gariwang image from 2018 was retrieved from
    https://lpdaac.usgs.gov, maintained by the NASA EOSDIS Land Processes
    Distributed Active Archive Center (LP DAAC) at the USGS Earth Resources
    Observation and Science (EROS) Center, Sioux Falls, South Dakota. 2018,
    https://lpdaac.usgs.gov/resources/data-action/aster-ultimate-2018-
    winter-olympics-observer/.

Created on Fri Jun 21 13:20:45 2019

@author: Travis
"""
from bs4 import BeautifulSoup
import os
import urllib.request

# Set working directory to the repo root and add path to functions
frame = getframeinfo(currentframe()).filename
file_path = os.path.dirname(os.path.abspath(frame))
os.chdir(os.path.join(file_path, '../..'))
sys.path.insert(0, 'src/functions')

# Create target directory
if not os.path.exists('/data/landcover'):
    os.mkdir('/data/landcover')


# MODIS tiles
tiles = ["h08v04", "h09v04", "h10v04", "h11v04", "h12v04", "h13v04", "h08v05",
         "h09v05", "h10v05", "h11v05", "h12v05", "h08v06", "h09v06", "h10v06",
         "h11v06"]
years = [str(y) for y in range(2001, 2017)]

# Land cover data from earthdata.nasa.gov
for year in years:
    url = 'https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.006/' + year + '.01.01/'
    r = urllib.request.urlopen(url)
    soup = BeautifulSoup(r, from_encoding=r.info().get_param('charset'))
    links = [link['href'] for link in soup.find_all('a', href=True)]
    links = [l for l in links if 'hdf' in l and l[17:23] in tiles]
    for link in links:
        urllib.request.urlretrieve(url, '/data/landcover')  
