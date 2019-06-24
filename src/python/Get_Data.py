# -*- coding: utf-8 -*-
"""
A script to download landcover data from NASA's Land Processes Distributed
Active Archive Center, which is an Earthdata thing. You'll need register for a
username and password, but that's free. Fortunately, there is a tutorial on how
to get this data:
    
    https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+Python

sample citation for later:
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
from http.cookiejar import CookieJar
from inspect import currentframe, getframeinfo
import os
import sys
import urllib.request as urllib2

# Set working directory to the repo root and add path to functions
frame = getframeinfo(currentframe()).filename
file_path = os.path.dirname(os.path.abspath(frame))
os.chdir(os.path.join(file_path, '../..'))
sys.path.insert(0, 'src/functions')

# Create target directory
if not os.path.exists('data/landcover'):
    os.mkdir('data/landcover')

# MODIS tiles
tiles = ["h08v04", "h09v04", "h10v04", "h11v04", "h12v04", "h13v04", "h08v05",
         "h09v05", "h10v05", "h11v05", "h12v05", "h08v06", "h09v06", "h10v06",
         "h11v06"]
years = [str(y) for y in range(2001, 2017)]

# Access
username = input('Enter NASA Earthdata User Name: ')
password = input('Enter NASA Earthdata Password: ')
pw_manager = urllib2.HTTPPasswordMgrWithDefaultRealm()
pw_manager.add_password(None, 'https://urs.earthdata.nasa.gov',
                        username, password)
cookiejar = CookieJar()
opener = urllib2.build_opener(urllib2.HTTPBasicAuthHandler(pw_manager),
                              urllib2.HTTPCookieProcessor(cookiejar))
urllib2.install_opener(opener)

# Land cover data from earthdata.nasa.gov
for year in years:
    print('Retrieving landcover data for ' + year)
    url = 'https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.006/' + year + '.01.01/'
    r = urllib2.urlopen(url)
    soup = BeautifulSoup(r, from_encoding=r.info().get_param('charset'),
                         features="lxml")
    filenames = [link['href'] for link in soup.find_all('a', href=True)]
    filenames = [f for f in filenames if 'hdf' in f and f[17:23] in tiles]
    links = [url + l for l in filenames]
    for i in range(len(links)):
        request = urllib2.Request(links[i])
        with open('data/landcover/' + filenames[i], 'wb') as file:
            response = urllib2.urlopen(request).read()
            file.write(response)
