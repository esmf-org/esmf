"""
Constants required for the regrid from file tests.
"""
import os
try:
    import ESMF
except:
    raise ImportError('The ESMF library cannot be found!')
#
TEST_REGRID_DIR = 'src/ESMF/test/regrid_from_file/'
CONTROL_FNAME = 'regrid_test_data.txt'
DATA_SUBDIR = os.path.join(TEST_REGRID_DIR,'data/')
DATA_URL_ROOT = 'http://www.earthsystemmodeling.org/download/data'
UNINITVAL = 422397696.
EPSILON = 1E-30
#
regrid_method_map = {"bilinear" : ESMF.RegridMethod.BILINEAR,
                     "patch" : ESMF.RegridMethod.PATCH,
                     "conserve" : ESMF.RegridMethod.CONSERVE,
                     "neareststod" : ESMF.RegridMethod.NEAREST_STOD,
                     "nearestdtos" : ESMF.RegridMethod.NEAREST_DTOS}
file_type_map = {"VTK" : ESMF.FileFormat.VTK,
                 "SCRIP" : ESMF.FileFormat.SCRIP,
                 "ESMF" : ESMF.FileFormat.ESMFMESH,
                 "ESMFMESH" : ESMF.FileFormat.ESMFMESH,
                 "ESMFGRID" : ESMF.FileFormat.ESMFGRID,
                 "UGRID" : ESMF.FileFormat.UGRID,
                 "GRIDSPEC" : ESMF.FileFormat.GRIDSPEC}
pole_method_map = {"none" : ESMF.PoleMethod.NONE,
                   "all" : ESMF.PoleMethod.ALLAVG,
                   "teeth" : ESMF.PoleMethod.TEETH}
