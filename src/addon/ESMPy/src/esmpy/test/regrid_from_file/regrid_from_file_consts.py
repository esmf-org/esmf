"""
Constants required for the regrid from file tests.
"""
import os
try:
    import esmpy
except:
    raise ImportError('The ESMF library cannot be found!')
#
TEST_REGRID_DIR = 'src/esmpy/test/regrid_from_file/'
CONTROL_FNAME = 'regrid_test_data.txt'
DATA_SUBDIR = os.path.join(TEST_REGRID_DIR,'data/')
DATA_URL_ROOT = 'http://data.earthsystemmodeling.org/download/data/'
UNINITVAL = 422397696.
EPSILON = 1E-30
#
regrid_method_map = {"bilinear" : esmpy.RegridMethod.BILINEAR,
                     "patch" : esmpy.RegridMethod.PATCH,
                     "conserve" : esmpy.RegridMethod.CONSERVE,
                     "neareststod" : esmpy.RegridMethod.NEAREST_STOD,
                     "nearestdtos" : esmpy.RegridMethod.NEAREST_DTOS}
file_type_map = {"VTK" : esmpy.FileFormat.VTK,
                 "SCRIP" : esmpy.FileFormat.SCRIP,
                 "ESMF" : esmpy.FileFormat.ESMFMESH,
                 "ESMFMESH" : esmpy.FileFormat.ESMFMESH,
                 "ESMFGRID" : esmpy.FileFormat.ESMFGRID,
                 "UGRID" : esmpy.FileFormat.UGRID,
                 "GRIDSPEC" : esmpy.FileFormat.GRIDSPEC}
pole_method_map = {"none" : esmpy.PoleMethod.NONE,
                   "all" : esmpy.PoleMethod.ALLAVG,
                   "teeth" : esmpy.PoleMethod.TEETH}
