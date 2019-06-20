import datetime as dt
from netCDF4 import Dataset
import numpy as np
import os
from osgeo import gdal, osr
import pandas as pd
from tqdm import tqdm
import yaml

# Supress depreciation warning for dask when importing xarray (temporary)
yaml.warnings({'YAMLLoadWarning': False})
import xarray as xr

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
def buildNC(files, savepath):
    '''
    Take in a list of files for the full mosaic of the MODIS burn detection
    dataset and create a singular netcdf file.
    '''
    # Set file names
    files.sort()
    names = [os.path.split(f)[-1] for f in files]
    names = [f.split('.')[0] for f in names]
    tile_id = files[0][-10:-4]
    print("Building tile " + tile_id)
    file_name = os.path.join(savepath, tile_id + '.nc')

    # Use a sample to get geography information and geometries
    sample = files[0]
    raster = gdal.Open(sample)
    geom = raster.GetGeoTransform()
    proj = raster.GetProjection()
    crs = osr.SpatialReference()

    # There may be an issue with transformations with this proj4 string
    # if so try '+proj=sinu +R=6371007.181 +nadgrids=@null +wktext'
    crs.ImportFromWkt(proj)
    proj4 = crs.ExportToProj4()

    # Use one tif (one array) for spatial attributes
    array = raster.ReadAsArray()
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
    y = nco.createVariable('y',  'f4', ('y',))
    x = nco.createVariable('x',  'f4', ('x',))
    times = nco.createVariable('time', 'f4', ('time',))
    variable = nco.createVariable('value', 'f4', ('time', 'y', 'x'),
                                  fill_value=-9999, zlib=True)
    variable.standard_name = 'day'
    variable.long_name = 'Burn Days'

    # Appending the CRS information - "https://cf-trac.llnl.gov/trac/ticket/77"
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
    datestrings = [f[-15:-7] for f in names]
    dates = []
    for d in datestrings:
        year = dt.datetime(year=int(d[:4]), month=1, day=1)
        date = year + dt.timedelta(int(d[5:]))
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
        array = gdal.Open(f).ReadAsArray()
        try:
            variable[tidx, :, :] = array
        except:
            print(f + " failed, probably had wrong dimensions, inserting a " +
                  "blank array in its place.")
            blank = np.zeros((ny, nx))
            variable[tidx, :, :] = blank
        tidx += 1

    # Done
    nco.close()


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


# Classes 
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

    Use Build_From_Mosaic.py for the full mosaic netcdfs and
    Build_From_Tiles.py for the tiled netcdfs to build a csv with each
    classified event.
    '''
    def __init__(self, nc_path=('/data/netcdfs/'), spatial_param=5,  # <------- Create nc_path if not present, where would this occur?
                 temporal_param=11, area_unit='Unknown', time_unit='day'):
        self.nc_path = nc_path
        self.spatial_param = spatial_param
        self.temporal_param = temporal_param
        self.area_unit = area_unit
        self.time_unit = time_unit
        self.event_grid = {}
        self.next_event_id = 1
        self.input_array = self.get_input_xarray()

    def get_event_stats(self,event_type='Fire'):
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
        try:
            mask = self.input_array.max(dim='time')
        except:
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
        print("Filtering out cells with no events...")
        available_pairs = self.get_availables()

        # This is to check the window positions
        nz, ny, nx = self.input_array.shape
        dims = [ny, nx]
        arr = self.input_array
        perimeters = []

        # traverse spatially processing each burn day
        print("Building event perimeters...")
        for pair in tqdm(available_pairs, position=0):
            # Separate coordinates
            y, x = pair

            # get the spatial window
            [top, bottom, left,
             right, center, origin] = self.get_spatial_window(y, x, dims)
            cy, cx = center

            # what if we pull in the window?
            window = arr[:, top:bottom+1, left:right+1].data

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

