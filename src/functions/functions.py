# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
import datetime as dt
import ftplib
import geopandas as gpd
from glob import glob
from http.cookiejar import CookieJar
from netCDF4 import Dataset
import numpy as np
import os
from osgeo import gdal, ogr, osr
import pandas as pd
import rasterio
from rasterio.merge import merge
from subprocess import check_output, CalledProcessError
import sys
from tqdm import tqdm
import urllib.request as urllib2
import yaml

# Supress depreciation warning for dask when importing xarray (temporary)
yaml.warnings({'YAMLLoadWarning': False})
import xarray as xr

# Set working directory here for now (temporary for convenience)
try:
    gitroot = check_output('git rev-parse --show-toplevel', shell=True)
    gitroot = gitroot.decode('utf-8').strip()
    os.chdir(gitroot)
    sys.path.insert(0, 'src/functions')
except CalledProcessError:
    raise IOError('This is not a git repository, using current directory.')


# Variables and objects
cmaps = {'Perceptually Uniform Sequential': ['viridis', 'plasma', 'inferno',
                                             'magma', 'cividis'],
         'Sequential': ['Greys', 'Purples', 'Blues', 'Greens', 'Oranges', 
                        'Reds', 'YlOrBr', 'YlOrRd', 'OrRd', 'PuRd', 'RdPu',
                        'BuPu', 'GnBu', 'PuBu', 'YlGnBu', 'PuBuGn', 'BuGn',
                        'YlGn'],
         'Sequential (2)': ['binary', 'gist_yarg', 'gist_gray', 'gray', 'bone',
                            'pink', 'spring', 'summer', 'autumn', 'winter',
                            'cool', 'Wistia', 'hot', 'afmhot', 'gist_heat',
                            'copper'],
         'Diverging': ['PiYG', 'PRGn', 'BrBG', 'PuOr', 'RdGy', 'RdBu',
                       'RdYlBu', 'RdYlGn', 'Spectral', 'coolwarm', 'bwr',
                       'seismic'],
         'Cyclic': ['twilight', 'twilight_shifted', 'hsv'],
         'Qualitative': ['Pastel1', 'Pastel2', 'Paired', 'Accent', 'Dark2',
                         'Set1', 'Set2', 'Set3', 'tab10', 'tab20', 'tab20b',
                         'tab20c'],
         'Miscellaneous': ['flag', 'prism', 'ocean', 'gist_earth', 'terrain',
                           'gist_stern', 'gnuplot', 'gnuplot2', 'CMRmap',
                           'cubehelix', 'brg', 'gist_rainbow', 'rainbow',
                           'jet', 'nipy_spectral', 'gist_ncar']}

# Functions
def convertDates(array, year):
    '''
    Convert everyday in an array to days since Jan 1 1970
    '''
    def convertDate(julien_day, year):
        base = dt.datetime(1970, 1, 1)
        date = dt.datetime(year, 1, 1) + dt.timedelta(int(julien_day))
        days = date - base
        return days.days

    # Loop through each position with data and convert
    locs = np.where(array > 0)
    ys = locs[0]
    xs = locs[1]
    locs = [[ys[i], xs[i]] for i in range(len(xs))]
    for l in locs:
        y = l[0]
        x = l[1]
        array[y, x] = convertDate(array[y, x], year)

    return array


def dateRange(perimeter):
    '''
    Converts days in a perimeter object since Jan 1 1970 to date strings 
    '''
    if len(perimeter.coords) > 0:
        base = dt.datetime(1970, 1, 1)
        days = [p[2] for p in perimeter.coords]
        day1 = (base + dt.timedelta(days=int(min(days)))).strftime('%Y-%m-%d')
    else:
        day1 = 'N/A'
    return day1


def mergeChecker(new_coords, full_list, temporal_param, radius):
    '''
    This uses a radius for the spatial window, but the original event
    classifier (get_event_perimeters_3d) uses a square. I ran out of time to
    fix this, but we could either uses a point in polygon approach in the post
    event classifier merge or a radius approach in the event classifier.

    This is not currently being used to merge events.
    '''
    t1 = np.min([c[2] for c in new_coords]) - temporal_param
    t2 = np.max([c[2] for c in new_coords]) + temporal_param
    for i in range(len(full_list)):
        old_event = full_list[i]
        old_coords = old_event[1]
        old_times = [c[2] for c in old_coords]
        time_checks = [t for t in old_times if t >= t1 and t <= t2]

        if len(time_checks) > 0:
            for coord in new_coords:
            # a coord is just one y, x, time coordinate
            # Check if the time coordinate is within an old event
                # Now find if there are any coords close enough
                radii = []
                new_y = coord[0]
                new_x = coord[1]
                for oc in old_coords:
                    old_y = oc[0]
                    old_x = oc[1]
                    dy = abs(old_y - new_y)
                    dx = abs(old_x - new_x)
                    r = np.sqrt((dy ** 2) + (dx ** 2))
                    radii.append(r)
                check = [r for r in radii if r <= radius]
                if any(check):
                    return i, True
                else:
                    return i, False
            else:
                return i, False


def edgeCheck(yedges, xedges, coord, sp_buffer):
    '''
    Let's identify edge cases to make merging events quicker later
    '''
    y = coord[0]
    x = coord[1]
    if y in yedges:
        edge = True
    elif x in xedges:
        edge = True
    else:
        edge = False
    return edge


def flttn(lst):
    '''
    Just a quick way to flatten lists of lists
    '''
    lst = [l for sl in lst for l in sl]
    return lst


def rasterize(src, dst, attribute, transform, crs, all_touch=False,
              na=-9999):
    '''
    It seems to be unreasonably involved to do this in Python compared to
    the command line.
    '''
    resolution = transform[1]

    # Open shapefile, retrieve the layer
    src_data = ogr.Open(src)
    layer = src_data.GetLayer()
    extent = layer.GetExtent()

    # Create the target raster layer
    xmin, xmax, ymin, ymax = extent
    cols = int((xmax - xmin)/resolution)
    rows = int((ymax - ymin)/resolution)
    trgt = gdal.GetDriverByName('GTiff').Create(dst, cols, rows, 1,
                               gdal.GDT_Float32)
    trgt.SetGeoTransform((xmin, resolution, 0, ymax, 0, -resolution))

    # Add crs
    refs = osr.SpatialReference()
    refs.ImportFromWkt(crs)
    trgt.SetProjection(refs.ExportToWkt())

    # Set no value
    band = trgt.GetRasterBand(1)
    band.SetNoDataValue(na)

    # Set options
    if all_touch is True:
        ops = ['-at', 'ATTRIBUTE=' + attribute]
    else:
        ops = ['ATTRIBUTE=' + attribute]

    # Finally rasterize
    gdal.RasterizeLayer(trgt, [1], layer, options=ops)

    # Close target an source rasters
    del trgt
    del src_data


def spCheck(diffs, sp_buf):
    '''
    Quick function to check if events land within the spatial.
    '''
    checks = [e for e in diffs if abs(e) < sp_buf]
    if any(checks):
        check = True
    else:
        check = False
    return check


def toDays(date, base):
    '''
    Convert dates to days since a base date
    '''
    if type(date) is str:
        date = dt.datetime.strptime(date, '%Y-%m-%d')
        delta = (date - base)
        days = delta.days
    return days


def toRaster(array, trgt, geometry, proj, navalue=-9999):
    """
    Writes a single array to a raster with coordinate system and
    geometric information.

    trgt = target path
    proj = spatial reference system
    geom = geographic transformation
    """
    ypixels = array.shape[0]
    xpixels = array.shape[1]
    trgt = trgt.encode('utf-8')
    image = gdal.GetDriverByName("GTiff").Create(trgt, xpixels, ypixels, 1,
                                                 gdal.GDT_Float32)
    image.SetGeoTransform(geometry)
    image.SetProjection(proj)
    image.GetRasterBand(1).WriteArray(array)
    image.GetRasterBand(1).SetNoDataValue(navalue)


# Classes
class Data_Getter():
    def __init__(self):
        self.date = dt.datetime.today().strftime('%m-%d-%Y')
        self.createPaths()
        self.cpus = os.cpu_count()
        self.modis_template_path = 'data/rasters/mosaic_template.tif'
        self.landcover_path = 'data/rasters/landcover'
        self.landcover_mosaic_path = 'data/rasters/landcover/mosaics'
        self.nc_path = 'data/rasters/burn_area/netcdfs'
        self.hdf_path = 'data/rasters/burn_area/hdfs'
        self.tiles = ["h08v04", "h09v04", "h10v04", "h11v04", "h12v04",
                      "h13v04", "h08v05", "h09v05", "h10v05", "h11v05",
                      "h12v05", "h08v06", "h09v06", "h10v06", "h11v06"]

    def createPaths(self):
        folders = ['data', 'data/rasters', 'data/shapefiles', 'data/tables',   # Set these to the path attributes?
                   'data/rasters/burn_area', 'data/rasters/burn_area/hdfs',
                   'data/rasters/landcover', 'data/rasters/ecoregion']
        for f in folders:
            if not os.path.exists(f):
                os.mkdir(f)

    def getBurns(self):
        '''
        This will download the MODIS burn event data set tiles and create a
        singular mosaic to use as a template file for coordinate reference
        information and geometries.

        User manual:
            http://modis-fire.umd.edu/files/MODIS_C6_BA_User_Guide_1.2.pdf

        FTP:
            ftp://fire:burnt@fuoco.geog.umd.edu/gfed4/MCD64A1/C6/

        This doesn't have a way to deal with missed downloads just yet.
        '''
        # Pull out paths and variables
        hdf_path = self.hdf_path
        tiles = self.tiles

        # Download original hdf files
        def downloadBA(file):
            missing = []
            tile = file[17:23]
            folder = os.path.join(hdf_path, tile)
            if not os.path.exists(folder):
                os.mkdir(folder)
            trgt = os.path.join(folder, file)
            if not os.path.exists(trgt):
                try:
                    with open(trgt, 'wb') as dst:
                        ftp.retrbinary('RETR %s' % file, dst.write, 102400)
                except ftplib.all_errors as e:
                    missing.append(file)
                    print('FTP Transfer Error: ', e)
            return missing

        # Get only the specified tiles
        ftp = ftplib.FTP('fuoco.geog.umd.edu')
        ftp.login('fire', 'burnt')
        missings = []
        for tile in tiles:
            print("Downloading " + tile)
            ftp_folder =  '/MCD64A1/C6/' + tile
            ftp.cwd(ftp_folder)
            hdfs = ftp.nlst()
            hdfs = [h for h in hdfs if '.hdf' in h]
            for h in tqdm(hdfs, position=0):
                missing = downloadBA(h)
                if len(missing) > 0:
                    missings = missings + missing
        ftp.quit()

        # How to best handle this?
        print('Missed Files: \n' + str(missings))

        # Should I go ahead and build the netcdfs here?
        # Get File Paths
        tile_folders = glob(os.path.join(hdf_path, '*'))
        tile_ids = np.unique([f[-6:] for f in tile_folders])
        tile_files = {}
        for tid in tile_ids:
            files = glob(os.path.join(hdf_path, tid, '*hdf'))
            tile_files[tid] = files

        # Merge one year into a reference mosaic
        if not os.path.exists(self.modis_template_path):
            folders = glob(os.path.join(self.hdf_path, '*'))
            file_groups = [glob(os.path.join(f, '*hdf')) for f in folders]
            for f in file_groups:
                f.sort()
            files = [f[0] for f in file_groups]
            dss = [rasterio.open(f).subdatasets[0] for f in files]
            tiles = [rasterio.open(d) for d in dss]
            mosaic, transform = merge(tiles)
            crs = tiles[0].meta.copy()
            crs.update({'driver': 'GTIFF',
                        'height': mosaic.shape[1],
                        'width': mosaic.shape[2],
                        'transform': transform})
            with rasterio.open(self.modis_template_path, 'w', **crs) as dest:
                dest.write(mosaic)

        # Build one netcdf per tile
        for tid in tile_ids:
            files = tile_files[tid]
            self.buildNCs(files)


    def getLandcover(self):
        """
        A method to download and process landcover data from NASA's Land
        Processes Distributed Active Archive Center, which is an Earthdata
        thing. You'll need register for a username and password, but that's
        free. Fortunately, there is a tutorial on how to get this data:
            
        https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+
        Python
        
        sample citation for later:
            ASTER Mount Gariwang image from 2018 was retrieved from
            https://lpdaac.usgs.gov, maintained by the NASA EOSDIS Land
            Processes Distributed Active Archive Center (LP DAAC) at the USGS
            Earth Resources Observation and Science (EROS) Center, Sioux Falls,
            South Dakota. 2018, https://lpdaac.usgs.gov/resources/data-action/
            aster-ultimate-2018-winter-olympics-observer/.
        """
        # Pull paths and variables out
        landcover_path = self.landcover_path
        tiles = self.tiles

        # We need years in strings  <------------------------------------------ Parameterize this
        years = [str(y) for y in range(2001, 2017)]

        # Access
        username = 'travissius'  # input('Enter NASA Earthdata User Name: ') <- Mine for developing
        password = 'tobehere1.'  # input('Enter NASA Earthdata Password: ') <-- Temporary, still building the script
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
            url = ('https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.006/' + year +
                   '.01.01/')
            r = urllib2.urlopen(url)
            soup = BeautifulSoup(r,
                                 from_encoding=r.info().get_param('charset'),
                                 features="lxml")
            names = [link['href'] for link in soup.find_all('a', href=True)]
            names = [f for f in names if'hdf' in f and f[17:23] in tiles]
            links = [url + l for l in names]
            for i in range(len(links)):
                if not os.path.exists(os.path.join(landcover_path, year)):
                    os.mkdir(os.path.join(landcover_path, year))
                path = os.path.join(landcover_path, year, names[i])
                if not os.path.exists(path):
                    request = urllib2.Request(links[i])
                    with open(path, 'wb') as file:
                        response = urllib2.urlopen(request).read()
                        file.write(response)

        # Now process these tiles into yearly geotiffs. (Landcover_process.r)
        if not os.path.exists(self.landcover_mosaic_path):
            os.mkdir(self.landcover_mosaic_path)
        for year in years:
            print('Stitching together landcover tiles for year ' + year)
            lc_tiles = glob(os.path.join(self.landcover_path, year, "*hdf"))
            dss = [rasterio.open(f).subdatasets[0] for f in lc_tiles]
            tiles = [rasterio.open(d) for d in dss]
            mosaic, transform = merge(tiles)
            crs = tiles[0].meta.copy()
            crs.update({'driver': 'GTIFF',
                        'height': mosaic.shape[1],
                        'width': mosaic.shape[2],
                        'transform': transform})
            file = 'us_lc_mosaic_' + year + '.tif'
            path = os.path.join(self.landcover_mosaic_path, file)
            with rasterio.open(path, 'w', **crs) as dest:
                dest.write(mosaic)

    def getShapes(self):
        '''
        Just to grab some basic shapefiles needed for calculating statistics.
        '''
        if not os.path.exists('data/shapefiles'):
            os.mkdir('data/shapefiles')

        # Variables
        conus_states = ['WV', 'FL', 'IL', 'MN', 'MD', 'RI', 'ID', 'NH', 'NC',
                        'VT', 'CT', 'DE', 'NM', 'CA', 'NJ', 'WI', 'OR', 'NE',
                        'PA', 'WA', 'LA', 'GA', 'AL', 'UT', 'OH', 'TX', 'CO',
                        'SC', 'OK', 'TN', 'WY', 'ND', 'KY', 'VI', 'ME', 'NY',
                        'NV', 'MI', 'AR', 'MS', 'MO', 'MT', 'KS', 'IN', 'SD',
                        'MA', 'VA', 'DC', 'IA', 'AZ']
        modis_crs = ('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 ' +
                     '+b=6371007.181 +units=m +no_defs')

        # MODIS Sinusoial World Grid
        if not os.path.exists('data/shapefiles/modis-world_grid.shp'):
            src = ('http://book.ecosens.org/wp-content/uploads/2016/06/' +
                   'modis_grid.zip')
            modis = gpd.read_file(src)
            modis.to_file('data/shapefiles/modis-world_grid.shp')

        # Contiguous United States - WGS84
        if not os.path.exists('data/shapefiles/conus.shp'):
            print("Downloading US state shapfile from the Census Bureau...")
            usa = gpd.read_file('http://www2.census.gov/geo/tiger/GENZ2016/' +
                                'shp/cb_2016_us_state_20m.zip')
            conus = usa[usa['STUSPS'].isin(conus_states)]
            conus.to_file('data/shapefiles/conus.shp')

        # Contiguous United States - MODIS Sinusoidal
        if not os.path.exists('data/shapefiles/conus_modis.shp'):
            print("Reprojecting state shapefile to MODIS Sinusoidal...")
            conus = gpd.read_file('data/shapefiles/conus.shp')
            modis_conus = conus.to_crs(modis_crs)
            modis_conus.to_file('data/shapefiles/conus_modis.shp')

        # Omernick Level IV Ecoregions - USGS North American Albers
        if not os.path.exists('data/shapefiles/us_eco_l4.shp'):
            print("Downloading Omernick Level IV Ecoregions from the USGS...")
            eco_l4 = gpd.read_file('ftp://ftp.epa.gov/wed/ecoregions/us/' +
                                   'us_eco_l4.zip')
            eco_l4.crs = {'init': 'epsg:4326'}
            eco_l4.to_file('data/shapefiles/us_eco_l4.shp')

        # Omernick Level IV Ecoregions - MODIS Sinusoidal
        if not os.path.exists('data/shapefiles/us_eco_l4_modis.shp'):
            print("Reprojecting ecoregions to MODIS Sinusoidal...")
            eco_l4 = gpd.read_file('data/shapefiles/us_eco_l4.shp')
            eco_l4_modis = eco_l4.to_crs(modis_crs)
            eco_l4_modis.to_file('data/shapefiles/us_eco_l4_modis.shp')

    def buildNCs(self, files):
        '''
        Take in a time series of files for the MODIS burn detection dataset and
        create a singular netcdf file.
        '''
        savepath = self.nc_path

        # Check that the target folder exists, agian.
        if not os.path.exists(savepath):
            os.mkdir(savepath)
    
        # Set file names
        files.sort()
        names = [os.path.split(f)[-1] for f in files]
        names = [f.split('.')[2] + '_' + f.split('.')[1][1:] for f in names]
        tile_id = names[0].split('_')[0]
        print("Building netcdf for tile " + tile_id)
        file_name = os.path.join(savepath, tile_id + '.nc')
    
        # Use a sample to get geography information and geometries
        sample = files[0]
    #    raster = gdal.Open(sample)  # <--------------------------------------- Using hdfs from now on
    #    geom = raster.GetGeoTransform()
    #    proj = raster.GetProjection()
        ds = gdal.Open(sample).GetSubDatasets()[0][0]
        hdf = gdal.Open(ds)
        geom = hdf.GetGeoTransform()
        proj = hdf.GetProjection()
        data = hdf.GetRasterBand(1)
        crs = osr.SpatialReference()
    
        # Get the proj4 string usign the WKT
        crs.ImportFromWkt(proj)
        proj4 = crs.ExportToProj4()
    
        # Use one tif (one array) for spatial attributes
        array = data.ReadAsArray()
        ny, nx = array.shape
        xs = np.arange(nx) * geom[1] + geom[0]
        ys = np.arange(ny) * geom[5] + geom[3]
    
        # Todays date for attributes
        todays_date = dt.datetime.today()
        today = np.datetime64(todays_date)
    
        # Create Dataset
        nco = Dataset(file_name, mode='w', format='NETCDF4', clobber=True)
    
        # Dimensions
        nco.createDimension('y', ny)
        nco.createDimension('x', nx)
        nco.createDimension('time', None)
    
        # Variables
        y = nco.createVariable('y',  np.float64, ('y',))
        x = nco.createVariable('x',  np.float64, ('x',))
        times = nco.createVariable('time', np.int64, ('time',))
        variable = nco.createVariable('value', np.int16, ('time', 'y', 'x'),
                                      fill_value=-9999, zlib=True)
        variable.standard_name = 'day'
        variable.long_name = 'Burn Days'
    
        # Appending the CRS information - <------------------------------------ Not overly confident with this, got it from here: "https://cf-trac.llnl.gov/trac/ticket/77"
        crs = nco.createVariable('crs', 'c')
        variable.setncattr('grid_mapping', 'crs')
        crs.spatial_ref = proj4
        crs.proj4 = proj4
        crs.geo_transform = geom
        crs.grid_mapping_name = "sinusoidal"
        crs.false_easting = 0.0
        crs.false_northing = 0.0
        crs.longitude_of_central_meridian = 0.0
        crs.longitude_of_prime_meridian = 0.0
        crs.semi_major_axis = 6371007.181
        crs.inverse_flattening = 0.0
    
        # Coordinate attributes
        x.standard_name = "projection_x_coordinate"
        x.long_name = "x coordinate of projection"
        x.units = "m"
        y.standard_name = "projection_y_coordinate"
        y.long_name = "y coordinate of projection"
        y.units = "m"
    
        # Other attributes
        nco.title = "Burn Days"
        nco.subtitle = "Burn Days Detection by MODIS since 1970."
        nco.description = "The day that a fire is detected."
        nco.date = pd.to_datetime(str(today)).strftime("%Y-%m-%d")
        nco.projection = "MODIS Sinusoidal"
        nco.Conventions = "CF-1.6"
    
        # Variable Attrs
        times.units = 'days since 1970-01-01'
        times.standard_name = 'time'
        times.calendar = 'gregorian'
        datestrings = [f[-7:] for f in names]
        dates = []
        for d in datestrings:
            year = dt.datetime(year=int(d[:4]), month=1, day=1)
            date = year + dt.timedelta(int(d[4:]))
            dates.append(date)
        deltas = [d - dt.datetime(1970, 1, 1) for d in dates]
        days = np.array([d.days for d in deltas])
    
        # Write dimension data
        x[:] = xs
        y[:] = ys
        times[:] = days
    
        # One file a time, write the arrays
        tidx = 0
        for f in tqdm(files):
    #        array = gdal.Open(f).ReadAsArray()  # <--------------------------- Use HDFs from now on, convert to days since 1970 here.
            ds = gdal.Open(f).GetSubDatasets()[0][0]
            hdf = gdal.Open(ds)
            data = hdf.GetRasterBand(1)
            array = data.ReadAsArray()
            year = int(f[44:48])
            array = convertDates(array, year)

            try:
                variable[tidx, :, :] = array
            except:
                print(f + ": failed, probably had wrong dimensions, " +
                      "inserting a blank array in its place.")
                blank = np.zeros((ny, nx))
                variable[tidx, :, :] = blank
            tidx += 1
    
        # Done
        nco.close()

    def buildTiffs(self):
        '''
        This will take the original HDF4 files retrieved from get Burns,
        convert the data (in julien days) to days since 1970 and save them as
        individual tiffs in 'data/rasters/burn_area/geotiffs'
        '''
        # Let's start with one tile
        tiles = self.tiles
        tile_files = glob(os.path.join(self.hdf_path, '*'))
        tile_files.sort()
        
        # Loop through everything, convert dates, and save to tiff
        for t in tile_files:
            files = glob(os.path.join(t, '*hdf'))
            files.sort()
            for f in tqdm(files, position=0):
                if not os.path.exists(f):
                    # Get data and attributes
                    year = int(f[44:48])
                    day = f[48:51]
                    ds = gdal.Open(f).GetSubDatasets()[0][0]
                    hdf = gdal.Open(ds)
                    geometry = hdf.GetGeoTransform()
                    proj = hdf.GetProjection()
                    data = hdf.GetRasterBand(1)
                    array = data.ReadAsArray()

                   # Convert dates in array
                    array = convertDates(array, year)
            
                    # Save as a geotiff...see if we get the same output
                    tile = os.path.basename(t)
                    tfolder = os.path.join('data/rasters/burn_area/geotiffs/',
                                           tile)
                    if not os.path.exists(tfolder):
                        os.mkdir(tfolder)
                    tfile = tile + '_' + str(year) + day + '.tif'
                    trgt = os.path.join(tfolder, tfile)
                    toRaster(array, trgt, geometry, proj)

        # Merge one year into a reference mosaic
        if not os.path.exists(self.modis_template_path):
            folder = 'data/rasters/burn_area/geotiffs'
            folders = glob(os.path.join(folder, '*'))
            file_groups = [glob(os.path.join( f, '*tif')) for f in folders]
            for f in file_groups:
                f.sort()
            files = [f[0] for f in file_groups]
            tiles = [rasterio.open(f) for f in files]
            mosaic, transform = merge(tiles)
            crs = tiles[0].meta.copy()
            crs.update({'driver': 'GTIFF',
                        'height': mosaic.shape[1],
                        'width': mosaic.shape[2],
                        'transform': transform})
            with rasterio.open(self.modis_template_path, 'w', **crs) as dest:
                dest.write(mosaic)      


class calculateStatistics:
    def __init__(self, ecoregion_level=1):
        self.tiles = ["h08v04", "h09v04", "h10v04", "h11v04", "h12v04",
                      "h13v04", "h08v05", "h09v05", "h10v05", "h11v05",
                      "h12v05", "h08v06", "h09v06", "h10v06", "h11v06"]
        self.event_file = 'data/modis_event_polygons.gpkg'
        self.template_path = 'data/rasters/mosaic_template.tif'
        self.lc_path = 'data/landcover'
        self.ecoregion_level = ecoregion_level
        self.ecoregion_dict = {1: {'code': 'NA_L1CODE',
                                   'name': 'NA_L1NAME'},
                               2: {'code': 'NA_L2CODE',
                                   'name': 'NA_L2NAME'},
                               3: {'code': 'NA_L3CODE',
                                   'name': 'NA_L3NAME'}}

    def calculateEcoregions(self):
        template_path = self.template_path
        ecoregion_level = self.ecoregion_level
        ecoregion_dict = self.ecoregion_dict
        code = ecoregion_dict[ecoregion_level]['code']
        lc_path = self.lc_path

        # Get the template modis mosaic 
        if not os.path.exists(self.template_path):
            files = glob('data/bd_numeric_tiles/*tif')
            files = [f for f in files if '2001_001' in f]
            tiles = [rasterio.open(f) for f in files]
            mosaic, transform = merge(tiles)
            crs = tiles[0].meta.copy()
            crs.update({'driver': 'GTIFF',
                        'height': mosaic.shape[1],
                        'width': mosaic.shape[2],
                        'transform': transform})
            with rasterio.open(template_path, 'w', **crs) as dest:
                dest.write(mosaic)
        template = gdal.Open(template_path)

        # Rasterize Ecoregion according to the template crs and geometry
        src = 'data/shapefiles/us_eco_l3_modis.shp'
        dst = 'data/rasters/landcover.tif'
        transform = template.GetGeoTransform()
        crs = template.GetProjection()
        if not os.path.exists(dst):
            rasterize(src, dst, code, transform, crs)
        eco_raster = gdal.Open(dst)

        # Now read in the event file
        event_df = gpd.read_file(self.event_file)

        # Now I need landcover raster mosaics  # <----------------------------- Left off here


class EventPerimeter:
    def __init__(self, event_id, coord_list=[]):
        self.event_id = event_id
        self.merge_id = np.nan
        self.coords = []
        self.coords = self.add_coordinates(coord_list)

    def add_coordinates(self,coord_list):
        for coord in coord_list:
            self.coords.append(coord)
        return self.coords

    def print_perimeter(self):
        print(self.event_id)
        print("coordinate list:")
        for c in self.coords:
            print(c)

    def get_event_id(self):
        return self.event_id

    def get_merge_id(self):
        return self.merge_id

    def get_coords(self):
        return self.coords


class EventGrid:
    '''
    So this is set up to handle either a time series of full mosaics or
    individual tiles. If you use the full mosaic without a rather large amount
    of RAM it will be incredibly slow.

    Use Build_From_Tiles.py for the tiled netcdfs to build a csv with each
    classified event.
    '''
    def __init__(self, nc_path=('/data/rasters/burn_area/netcdfs/'),
                 spatial_param=5, temporal_param=11, area_unit='Unknown',
                 time_unit='days since 1970-01-01', low_memory=True):
        self.nc_path = nc_path
        self.spatial_param = spatial_param
        self.temporal_param = temporal_param
        self.area_unit = area_unit
        self.time_unit = time_unit
        self.event_grid = {}
        self.next_event_id = 1
        self.input_array = self.get_input_xarray()
        self.low_memory = low_memory

    def get_event_stats(self, event_type='Fire'):
        id_list = []
        mergeid_list = []
        coords_list = []
        detections = []
        start = []
        end = []
        peak = []
        for p in range(0, len(self.event_perimeters)):
            id_list.append(self.event_perimeters[p].get_event_id())
            mergeid_list.append(self.event_perimeters[p].get_merge_id())
            coords_list.append(self.event_perimeters[p].get_coords())
            coords = self.event_perimeters[p].get_coords()
            if len(coords):
                z = [x for _, _, x in coords]
            detections.append(z)
            s = min(z)
            start.append(s)
            e = max(z)
            end.append(e)
            p = max(set(z), key = z.count)
            peak.append(p)

        # create zipped list
        zippedList =  list(zip(id_list, mergeid_list, coords_list,
                               detections, start, end, peak))
        df = pd.DataFrame(zippedList, columns=['event_id' , 'merge_id',
                                               'coords','detections','start',
                                               'end','peak'])
        df.set_index('event_id',inplace=True)
        df['duration'] = df.end - df.start + 1
        if event_type == 'Fire':
            df.columns = df.columns.str.replace('detections', 'burn_days')
            df.columns = df.columns.str.replace('start', 'start_day')
            df.columns = df.columns.str.replace('end', 'end_day')
            df.columns = df.columns.str.replace('peak', 'max_day')
            print(df.head())
            df.to_csv('../out/burns_0521.csv')  # <---------------------------- Possible path confusion, do we use this any more?

    def get_input_xarray(self):
        burns = xr.open_dataset(self.nc_path)
        self.data_set = burns
        self.coordinates = burns.coords
        input_array = burns.value

        return input_array

    def add_event_grid(self, event_id, new_pts):
        for p in new_pts:
            entry = {p : event_id}
            self.event_grid.update(entry)

    def merge_perimeters(self, perimeters, event_id, obsolete_id):
        # set the merge id in the obsolete id
        perimeters[obsolete_id-1].merge_id = event_id
        new_pts = perimeters[obsolete_id-1].coords

        # update the event_grid and add points to event_id perimeter
        for p in new_pts:
            self.event_grid[p] = event_id
        perimeters[event_id-1].add_coordinates(new_pts)

        # set old perimeter to null
        merge_notice = 'Merged with event {}'.format(event_id)
        perimeters[obsolete_id-1].coords = [merge_notice, new_pts]

        return perimeters

    def get_spatial_window(self, y, x, array_dims):
        '''
        Pull in the spatial window around a detected event and determine its
        shape and the position of the original point within it. Finding this
        origin point is related to another time saving step in the event
        classification procedure.
        '''
        top = max(0, y - self.spatial_param)
        bottom = min(array_dims[0], y + self.spatial_param)
        left = max(0, x - self.spatial_param)
        right = min(array_dims[1], x + self.spatial_param)

        #  Derive full xarray coordinates from just the window for speed
        ydim = array_dims[0]
        xdim = array_dims[1]

        # Expand the spatial dimension
        tps = [i for i in range(self.spatial_param)]

        # There are four edge cases
        x_edges_1 = [0 + t for t in tps]
        x_edges_2 = [xdim - t for t in tps]
        y_edges_1 = [0 + t for t in tps]
        y_edges_2 = [ydim - t for t in tps]

        # Get the full y, x coords of the origin, and window coords of date
        if y in y_edges_1:
            ycenter = y
            oy = 0
        elif y in y_edges_2:
            ycenter = y - ydim
            oy = y - self.spatial_param
        else:
            ycenter = self.spatial_param - 1
            oy = y - self.spatial_param
        if x in x_edges_1:
            xcenter = x
            ox = 0
        elif x in x_edges_2:
            xcenter = x - xdim
            ox = x - self.spatial_param
        else:
            xcenter = self.spatial_param - 1
            ox = x - self.spatial_param
        center = [ycenter, xcenter]
        origin = [oy, ox]
        return top, bottom, left, right, center, origin

    def print_perimeters(self):
        for p in range(0, len(self.event_perimeters)):
            e = self.event_perimeters[p].event_id
            m = self.event_perimeters[p].merge_id
            c = self.event_perimeters[p].coords
            print("EVENT ID: {}, MERGE ID: {}, COORDS: {}".format(e, m, c))

    def print_event_grid(self):
        print(self.event_grid)

    def get_availables(self):
        '''
        To save time, avoid checking cells with no events at any time step.
        Create a mask of max values at each point. If the maximum at a cell is
        less than or equal to zero there were no values and it will not be
        checked in the event classification step. 

        If the computer has enough memory, this option could be combined
        with the event_perimeter method to read this data in once.
        '''
        burns = xr.open_dataset(self.nc_path, chunks=1000)
        array = burns.value
        mask = array.max(dim='time').compute().values
        burns.close()
        locs = np.where(mask > 0)
        available_pairs = []
        for i in range(len(locs[0])):
            available_pairs.append([locs[0][i], locs[1][i]])

        return available_pairs

    def get_event_perimeters_3d(self):
        '''
        Iterate through each cell in the 3D MODIS Burn Date tile and group it
        into fire events using the space-time window.        
        '''
        print("\nFiltering out cells with no events...")
        if self.low_memory:
            available_pairs = self.get_availables()
            arr = self.input_array
        else:
            arr = self.data_set.value
            mask = arr.max(dim='time').compute().values
            locs = np.where(mask > 0)
            available_pairs = []
            for i in range(len(locs[0])):
                available_pairs.append([locs[0][i], locs[1][i]])

        # This is to check the window positions
        nz, ny, nx = self.input_array.shape
        dims = [ny, nx]
        perimeters = []

        # traverse spatially, processing each burn day
        print("Building event perimeters...\n")
        for pair in tqdm(available_pairs, position=0):
            # Separate coordinates
            y, x = pair

            # get the spatial window
            [top, bottom, left,
             right, center, origin] = self.get_spatial_window(y, x, dims)
            cy, cx = center

            # what if we pull in the window?
            window = arr[:, top:bottom+1, left:right+1].data  # <-------------- If it is slow, this is the problem

            # The center of the window is the target burn day
            center_burn = window[:, cy, cx]
            center_burn = center_burn[center_burn > 0]

            # Loop through each event in the window and identify neighbors
            for burn in center_burn:
                new_pts = []
                curr_event_ids = []

                # Now we can get the values and position right away
                diff = abs(burn - window)
                val_locs = np.where(diff <= self.temporal_param)
                y_locs = val_locs[1]
                x_locs = val_locs[2]
                oy, ox = origin

                # Get the actual x,y positions from the window coordinates
                vals = window[val_locs]
                ys = [oy + yl for yl in y_locs]
                xs = [ox + xl for xl in x_locs]

                # Now get the geographic coordinates from tile positions
                all_ys = self.coordinates['y'].data
                all_xs = self.coordinates['x'].data
                ys = all_ys[ys]
                xs = all_xs[xs]

                # Now check if this point is in the event_grid yet
                for i in range(len(vals)):
                    curr_pt = (float(ys[i]), float(xs[i]), float(vals[i]))

                    # already assigned to an event
                    if (curr_pt in self.event_grid):
                        if self.event_grid[curr_pt] not in curr_event_ids:
                            curr_event_ids.append(self.event_grid[curr_pt])
                    else:
                        new_pts.append(curr_pt)

                # If this is a new event
                if len(curr_event_ids)==0:
                    # create a new perimeter object
                    perimeter = EventPerimeter(self.next_event_id, new_pts)

                    # append to perimeters list
                    perimeters.append(perimeter)

                    # add points to the grid
                    self.add_event_grid(self.next_event_id, new_pts)

                    # increment the event ID
                    self.next_event_id += 1

                # If all points part of same existing event
                elif len(curr_event_ids) == 1:
                    event_id = curr_event_ids[0]
                    if len(new_pts):
                        perimeters[event_id - 1].add_coordinates(new_pts)
                        self.add_event_grid(event_id, new_pts)

                # events overlap
                else:
                    perimeters = self.merge_perimeters(perimeters,
                                                       curr_event_ids[0],
                                                       curr_event_ids[1])

        return perimeters

