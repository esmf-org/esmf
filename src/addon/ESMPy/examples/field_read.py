# This example demonstrates how to regrid between a GRIDSPEC grid and a tripole grid, both grids use masking.
# The grid files are required, they can be retrieved from the ESMF data repository:
#   wget http://www.earthsystemmodeling.org/download/data/tasmax_day_CanCM4_decadal2010_r2i1p1_20110101-20201231.nc
#   wget http://www.earthsystemmodeling.org/download/data/ll1deg_grid.nc

import ESMF

# create a manager object with multiprocessor logging in debug mode
ESMF.Manager(logkind=ESMF.LogKind.MULTI, debug=True)

datafile = "data/tasmax_day_CanCM4_decadal2010_r2i1p1_20110101-20201231.nc"
gridfile = "data/ll1deg_grid.nc"

# Create a  grid from a GRIDSPEC formatted file
srcgrid = ESMF.Grid(filename=datafile, filetype=ESMF.FileFormat.GRIDSPEC)

# create a field on the center stagger locations of the source grid
srcfield = ESMF.Field(srcgrid, 'srcfield', staggerloc=ESMF.StaggerLoc.CENTER)

srcfield.read(filename=datafile, variable="tasmax", timeslice=1)

# create a tripole grid
dstgrid = ESMF.Grid(filename=gridfile, filetype=ESMF.FileFormat.SCRIP)

# create fields on the center stagger locations of the tripole grid
dstfield = ESMF.Field(dstgrid, 'dstfield', meshloc=ESMF.StaggerLoc.CENTER)
dstfield.data[...] = 1e20

# create an object to regrid data from the source to the destination field
regrid = ESMF.Regrid(srcfield, dstfield,
                     regrid_method=ESMF.RegridMethod.BILINEAR,
                     unmapped_action=ESMF.UnmappedAction.ERROR)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield)

# output the results from one processor only
if ESMF.local_pet() is 0: print "ESMPy Field Data Regridding Example Finished Successfully"