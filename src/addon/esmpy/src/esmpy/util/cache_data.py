import os
import esmpy
import site
# import inspect


# inspect returns local module directory by default (not installed directory)
# DATA_DIR = os.environ.get('ESMPY_DATA_DIR', os.path.join(os.path.dirname(inspect.getfile(esmpy)),'data'))
DATA_DIR = os.environ.get('ESMPY_DATA_DIR', os.path.join(site.getsitepackages()[0], "esmpy/data"))
DATA_URL = 'http://data.earthsystemmodeling.org/download/data/'
# DATA_URL = 'https://raw.github.com/esmf-org/esmf-test-data/main/grids/'



# If fname doesn't exist, retrieve it from the remote server via http.
def cache_data_file(fname):
    import sys
    if sys.version_info[0] >= 3:
        from urllib.request import urlopen, URLError
    else:
        from urllib2 import urlopen, URLError

    from shutil import copyfileobj

    status_ok = True
    if not os.path.exists(fname):
        url = os.path.join(DATA_URL, os.path.basename(fname))
        print('Retrieving ' + url + '...')
        try:
            req = urlopen(url)
        except URLError:
            print('Error opening %s' % url)
            status_ok = False
        else:
            try:
                with open(fname, 'wb') as fp:
                    copyfileobj(req, fp)
            except:
                status_ok = False
    return status_ok

def download_example_data():
    # Filenames to download.
    datafilelist = ["aggregAtlanticESTOFS.nc",
                    "GRIDSPEC_ACCESS1.nc",
                    "ll1deg_grid.nc",
                    "ll2.5deg_grid.nc",
                    "mpas_uniform_10242_dual_counterclockwise.nc",
                    "so_Omon_GISS-E2.nc",
                    "T42_grid.nc",
                    ]

    wget = True
    if 'ESMPY_DATA_DIR' in os.environ:
        wget = False
    else:
        print ('Data directory: {}'.format(DATA_DIR))

    # Create data subdirectory if it doesn't exist.
    if not os.path.exists(DATA_DIR):
        os.mkdir(DATA_DIR)
    
    if wget:
        # Download each test file.
        for fname in datafilelist:
            # Retrieve the data files needed for the test cases from the remote server.
            status_ok = cache_data_file(os.path.join(DATA_DIR, fname))
            if not status_ok:
                raise IOError("Error downloading '{}'".format(fname))

def download_unittest_data():
    # Filenames to download.
    datafilelist = ["ne4np4-pentagons.nc",
                    "ne4np4-esmf.nc",
                    "ne30np4-t2.nc",
                    "ll2.5deg_grid.nc",
                    "T42_grid.nc",
                    "gridspec1Dcoords.nc",
                    ]

    wget = True
    if 'ESMPY_DATA_DIR' in os.environ:
        wget = False
    else:
        print ('Data directory: {}'.format(DATA_DIR))

    # Create data subdirectory if it doesn't exist.
    if not os.path.exists(DATA_DIR):
        os.mkdir(DATA_DIR)
    
    if wget:
        # Download each test file.
        for fname in datafilelist:
            # Retrieve the data files needed for the test cases from the remote server.
            status_ok = cache_data_file(os.path.join(DATA_DIR, fname))
            if not status_ok:
                raise IOError("Error downloading '{}'".format(fname))
