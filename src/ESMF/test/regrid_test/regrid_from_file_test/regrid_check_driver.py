# $Id$

"""
Reads each line of a control file where each line corresponds to one test
case.    Parses each line and calls a test subroutine that creates meshes from
source and destination NetCDF files, creates an analytic field across the
source mesh, regrids the source mesh to the grid of the destination mesh,
and compares the analytic field of the resulting regridded mesh to that of the
source mesh.
"""

import sys
import os
import subprocess
import urllib
import traceback
import getopt

try:
    import ESMF
except:
    raise ImportError('The ESMF library cannot be found!')
import re
from regrid_check import mesh_check

TEST_REGRID_DIR = 'src/ESMF/test/regrid_test/regrid_from_file_test/'
CONTROL_FNAME = 'regrid_test_data.txt'
DATA_SUBDIR = os.path.join(TEST_REGRID_DIR,'data/')
DATA_URL_ROOT = 'http://www.earthsystemmodeling.org/download/data'

# If fname doesn't exist, retrieve it from the remote server via http.
def cache_data_file (fname):
    if not os.path.exists(fname):
        url = os.path.join(DATA_URL_ROOT, os.path.basename(fname))
        print 'Retrieving ' + url + '...\n'
        urllib.urlretrieve(url, fname)

# Main program: Start up ESMF and run regrid test for each line of options
# read from a control file.    Retrieve data files for each test from a remote
# server if they do not exist locally.
def main():
    try:
        # parse options
        opts, args = getopt.getopt(sys.argv[1:], "", ["dryrun"])
    except getopt.GetoptError as err:
        # print help information and exit:
        print str(err)
        usage()
        sys.exit(2)
    dryrun = False
    for o, a in opts:
        if o == "--dryrun":
            dryrun = True
        else:
            assert False, "unhandled option"


    wget_ok = True
    if not dryrun:
        # Start up ESMF.
        esmp = ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

        petCount = esmp.petCount

    # Create data subdirectory if it doesn't exist.
    if not os.path.exists(DATA_SUBDIR):
        os.mkdir(DATA_SUBDIR)

    # For each test case line from the control file parse the line and call
    # the test subroutine.
    for line in open(os.path.join(TEST_REGRID_DIR, CONTROL_FNAME), 'r'):
        if line[0] != '#' and re.match('(\s*.+\s*:){3}', line):
            (src_fname, dst_fname, regrid_method, options,
             max_err_str) = re.split('\s*:\s*', line)
            max_err = float(max_err_str)
            if not dryrun:
                test_str = 'Regrid %s to %s as %s with %s and max_err = %f' % \
                (src_fname, dst_fname, regrid_method, options,
                 max_err)

                print '\n' + test_str + ' - START\n'

            src_fname_full = os.path.join(DATA_SUBDIR, src_fname)
            dst_fname_full = os.path.join(DATA_SUBDIR, dst_fname)

            # run the data file retrieval and regridding through try/except
            correct = False
            try:
                cache_data_file (src_fname_full)
                cache_data_file (dst_fname_full)
            except:
                print "wget ERROR:\n"
                traceback.print_exc(file=sys.stdout)
                wget_ok = False
            else:
                if not dryrun:
                    try:
                        correct = mesh_check(src_fname_full, dst_fname_full,
                                             regrid_method, options, max_err)
                    except:
                        print "Regridding ERROR:\n"
                        traceback.print_exc(file=sys.stdout)

            if not dryrun:
                skip = False
                for i in range(petCount):
                    for line in open("PET"+str(i)+".ESMF_LogFile"):
                        if "ESMF_NETCDF not defined when lib was compiled" in line or \
                            "File format is not supported" in line:
                            # set skip
                            skip = True

                # print the file
                print '\n***NOTE*** The log files must be deleted in this test case, they are printed below for future reference\n'
                if skip:
                    for line in open("PET"+str(i)+".ESMF_LogFile"):
                        print line

                # clean the log files
                for i in range(petCount):
                    os.system("echo ' ' > PET"+str(i)+".ESMF_LogFile")
                    
                print '\n' + test_str + ' - FINISH\n'

                if skip:
                    print 'RESULT: SKIP\n\n'
                elif correct:
                    print 'RESULT: PASS\n\n'
                else:
                    print 'RESULT: FAIL\n\n'

    if dryrun:
        if wget_ok:
            print 'RESULT: PASS - wget ok\n\n'
        else:
            print 'RESULT: FAIL - wget error\n\n'
    return 0

if __name__ == '__main__':
    sys.exit(main())
