# $Id$
#===============================================================================
#                                                        remap.py
# 
# This is a driver python script for the ESMF_RegridWeightGen application
# CAUTION: this is extremely outdated
#===============================================================================

# ================================================================================
# ================================================================================
# 
# Dependencies
# ------------
# 
# 
# 
# ESMF must be built and installed for this python module to run properly.
# If ESMF is not on your system you can download, build and install it by 
# following the instructions in the "MORE HELP" section below.
# 
# The ESMF library must be built with NetCDF, and LAPACK support to enable
# full functionality with this module.
# 
# If ESMF is already installed on the system you are using, you 
# should only have to set the ESMFMKFILE environment variable.
# This environment variable should be pointed to the esmf.mk file in the ESMF
# installation.
# 
# The PYTHONPATH environment variable must also be set to the top-level
# ESMP directory, then this module is accessed with: 
# 
#    from src import remap
# 
# ================================================================================
# ================================================================================
# 
# Use
# ---
# 
# There are three required input parameters to this python interface, and one 
# optional parameter.    These are listed below, with descriptions and
# default values if applicable:
# 
# srcgrid - The source grid, in NetCDF format, to be used in the remapping
# weight generation.
# 
# dstgrid - The destination grid, in NetCDF format, to be used in the remapping
# weight generation.
# 
# weights - The name of the NetCDF file into which the remapping weights will be
# written.    
# 
# method - The interpolation method to be used to generate the weights.    The 
# default method is "bilinear", other methods are "patch" and "conserve".    The
# "patch" option specifies a patch recovery based finite element interpolation, 
# and "conserve" specifies and area weighted first order locally conservative
# method.
# 
# Some examples of how to use this interface are:
# 
# remap(source, destination, myweightsfile)
# 
# remap(source, destination, myweightsfile, conserve)
# 
# ================================================================================
# ================================================================================
# 
# More Help
# ------------
# 
# Details about the ESMF_RegridWeightGen application that is driven by the
# interface can be found in the ESMF Reference Manual:
# http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_5_2_0r/ESMF_refdoc/node3.html
# 
# Information about the ESMF project can be found at the ESMF web site:
#                     http://www.earthsystemmodeling.org
#     
# Software documentation for the last public release is at:
#                     http://www.earthsystemmodeling.org -> Users -> Documentation
#     
# Software documentation for all releases is at:
#                     http://www.earthsystemmodeling.org -> Download -> View All Releases
#     
# The ESMF library source code is also available for download at:
#                     http://sourceforge.net/projects/esmf
#     
# Please contact esmf_support@ucar.edu with any questions or problems.
#     
# ================================================================================

def remap(srcgrid, dstgrid, weights, method='bilinear'):
    import os
    import re
    import sys
        
    # current running executable directory
    RUNDIR = os.getcwd()
    
    #TODO is this in the esmf.mk?
    # find out how many procs to use on this machine
    if os.environ.get('ESMF_NUM_PROCS'):
        NUM_PROCS = os.environ.get('ESMF_NUM_PROCS')
    else:
        print "ESMF_NUM_PROCS not defined in user environment, using default ESMF_NUM_PROCS=1"
        NUM_PROCS = "1"
    
    # read the esmf.mk and get the location of the executable and the OS for 
    # this system
    if os.environ.get('ESMFMKFILE'):
        esmfmkfile = open(os.environ.get('ESMFMKFILE'))
    else:
        print "ESMFMKFILE is not defined!"
        sys.exit
    
    for line in esmfmkfile:
        if re.search(".*ESMF_APPSDIR.*", line) != None:
            ap_match = line 
    
    esmfmkfile.close()
    
    # clean up the executable name string for proper usage
    ap_match = ap_match.split("=")[1]
    APP = ap_match.strip()+"/ESMF_RegridWeightGen"
    
    # TODO: use 'bilinear' instead of 'b' for filename
    # TODO: this could be a dict
    options = ''
    file = ''
    # methods
    if method == 'bilinear':
        options = ''
        file = 'b'
    elif method == 'patch':
        options = '-m patch'
        file = 'p'
    elif method == 'conserve':
        options = '-m conserve'
        file = 'c'
    else:
        print 'Method: '+method+' is not supported!'
        sys.exit
    
    # TODO: what is this cruft?
    #weights = weights.split('.')[0]
    #weights = weights+"_"+file+".nc"
    #srcgrid = "/export/shared/grids/ll1deg_grid.nc"
    #dstgrid = "/export/shared/grids/ll2.5deg_grid.nc"
    #options = ""
    #weights = "srcgrid_to_dstgrid_b.nc"
    
    return os.system("mpirun -np "+NUM_PROCS+" "+APP+" "+options+\
                     " -s "+srcgrid+" -d "+dstgrid+" -w "+weights+" > RegridWeightGen.out")
