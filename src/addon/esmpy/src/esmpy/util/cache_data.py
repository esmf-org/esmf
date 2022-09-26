import os
import inspect
import esmpy

DATA_DIR = os.environ.get('ESMPY_DATA_DIR', os.path.join(os.path.dirname(inspect.getfile(esmpy)),'data'))
DATA_URL = 'http://data.earthsystemmodeling.org/download/data/'

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
