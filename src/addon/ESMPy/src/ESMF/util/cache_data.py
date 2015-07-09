import os

DATA_URL_ROOT = 'http://www.earthsystemmodeling.org/download/data'

# If fname doesn't exist, retrieve it from the remote server via http.
def cache_data_file(fname, DATA_URL_ROOT=DATA_URL_ROOT):
    from urllib2 import urlopen, URLError
    from shutil import copyfileobj
    status_ok = True
    if not os.path.exists(fname):
        url = os.path.join(DATA_URL_ROOT, os.path.basename(fname))
        print 'Retrieving ' + url + '...\n'
        try:
            req = urlopen(url)
        except URLError:
            print 'Error opening %s' % url
            status_ok = False
        else:
            try:
                with open(fname, 'wb') as fp:
                    copyfileobj(req, fp)
            except Exception:
                print 'Error writing to %s' % fname
                status_ok = False
    return status_ok