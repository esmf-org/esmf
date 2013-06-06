FileFormat
==========

The Grid and Mesh objects may be created
from specifications in a NetCDF data file.  This flag indicates the
format of the data file.

Values are:

    VTK = 1
        Use VTK file format.
    SCRIP = 2
        Use SCRIP file format.
    ESMFMESH = 3
        Use ESMF unstructured grid file format.
    ESMFGRID = 4
        Use ESMF structured grid file format.
    UGRID = 5
        Use CF-convention unstructured grid file format.
    GRIDSPEC = 6
        Use a single tile grid file conforming with the proposed CF-GRIDSPEC conventions.
