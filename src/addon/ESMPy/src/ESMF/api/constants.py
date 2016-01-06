# $Id$

import numpy as np

from ESMF.util.enum import IntEnum

# error message
_errmsg = 'Please check the log files (named "*ESMF_LogFile").'
_ESMP_SUCCESS = 0
# ESMF TYPEKINDS mapped to Python types
_ESMF2PythonType = (0,0,0, np.int32, np.int64, np.float32, np.float64)
_Python2ESMFType = {\
                    int:3,
                    np.int32:3,
                    np.int64:4,
                    float:5,
                    np.float32:5,
                    np.float64:6,
                    }

#### CONSTANTS ################################################################

# weird ESMF stuff for ESMC_Initialize optional argument passing prototype
_ESMP_ArgLast = 11235813
_ESMP_ArgBaseID = 1
_ESMP_InitArgLogKindFlagID = _ESMP_ArgBaseID + 3

# ESMF ABI
_ESMF_ABI = None
(_ESMF_ABI_64,_ESMF_ABI_32) = (64, 32)

# ESMF OS
_ESMF_OS = None
(_ESMF_OS_DARWIN,
 _ESMF_OS_LINUX,
 _ESMF_OS_UNICOS) = (-5,-4,-3)

# ESMF_NETCDF
_ESMF_NETCDF = False

# ESMF_COMM
_ESMF_COMM = None
_ESMF_COMM_MPIUNI = -1

# ESMF_VERSION
_ESMF_VERSION = None

# ESMF_MPIRUN
_ESMF_MPIRUN = "mpirun"

#### NAMED CONSTANTS ##########################################################

# CoordSys
class CoordSys(IntEnum):
    """
    This flag indicates the coordinate system of a Grid. This value is
    useful both to indicate to other users the type of the coordinates,
    but also to control how the coordinates are interpreted in
    regridding methods (e.g. Regrid()).
    """
    INVALID = -2
    UNINIT = -1
    CART = 0
    """
    Cartesian coordinate system. In this system, the Cartesian
    coordinates are mapped to the Grid coordinate dimensions in the
    following order: x,y,z. (e.g. using coord_dim=1 in
    Grid.get_coords() references the y dimension)
    """
    SPH_DEG = 1
    """
    Spherical coordinates in degrees. In this system, the spherical
    coordinates are mapped to the Grid coordinate dimensions in the
    following order: longitude, latitude, radius. (E.g. using
    coord_dim=1 in Grid.get_coords() references the latitude dimension)
    Note, however, that Regrid() currently just supports longitude and
    latitude (i.e. with this system, only Grids of dimension 2 are
    supported in the regridding).
    """
    SPH_RAD = 2
    """
    Spherical coordinates in radians. In this system, the spherical
    coordinates are mapped to the Grid coordinate dimensions in the
    following order: longitude, latitude, radius. (E.g. using
    coord_dim=1 in Grid.get_coords() references the latitude dimension)
    Note, however, that Regrid() currently just supports longitude and
    latitude (i.e. with this system, only Grids of dimension 2 are
    supported in the regridding).
    """

# DecompFlag
class DecompFlag(IntEnum):
    """
    This flag indicates how object are distributed across the available
    computational resources.
    """
    DEFAULT = 0
    """
    Use default decomposition behavior. Currently equal to 'BALANCED'.
    """
    BALANCED = 1
    """
    Decompose elements as balanced as possible. The maximum
    difference in number of elements per DE is 1, with the extra elements on
    the lower DEs.
    """
    RESTFIRST = 2
    """
    Divide elements over DEs. Assign the rest of this division to the first DE.
    """
    RESTLAST = 3
    """
    Divide elements over DEs. Assign the rest of this division to the last DE.
    """
    CYCLIC = 4
    """
    Decompose elements cyclically across DEs.
    """

# FileFormat
class FileFormat(IntEnum):
    """
    The Grid and Mesh objects may be created from specifications in a NetCDF
    data file.  This flag indicates the format of the data file.
    """
    UNDEFINED = 0
    VTK = 1
    """
    Use VTK file format.
    """
    SCRIP = 2
    """
    Use SCRIP file format.
    """
    ESMFMESH = 3
    """
    Use ESMF unstructured grid file format.
    """
    ESMFGRID = 4
    """
    Use ESMF structured grid file format.
    """
    UGRID = 5
    """
    Use CF UGRID unstructured grid file format.
    """
    GRIDSPEC = 6
    """
    Use a single tile grid file conforming with the proposed
    CF GRIDSPEC conventions.
    """

# GridItem
class GridItem(IntEnum):
    """
    The Grid can contain other kinds of data besides coordinates. This
    data is referred to as Grid "items". Some items may be used
    for calculations involving the Grid.
    """
    INVALID = -2
    UNINIT = -1
    MASK = 0
    """
    A grid item to represent a mask.
    """
    AREA = 1
    """
    A grid item to represent an area field for conservative regridding.
    """

# LineType
class LineType(IntEnum):
    """
    This argument controls the path of the line which connects two points on
    the surface of the sphere. This in turn controls the path along which
    distances are calculated and the shape of the edges that make up a cell.
    Both of these quantities can influence how interpolation weights are
    calculated. As would be expected, this argument is only applicable with
    grids which lie on the surface of a sphere.
    """
    CART = 0
    """
    Cartesian line. When this option is specified distances are calculated in a
    straight line through the 3D Cartesian space in which the sphere is
    embedded. Cells are approximated by 3D planes bounded by 3D Cartesian lines
    between their corner vertices. When calculating regrid weights, this line
    type is currently the default for the following all regrid methods except
    for conservative.
    """
    GREAT_CIRCLE = 1
    """
    Great circle line. When this option is specified distances are calculated
    along a great circle path (the shortest distance between two points on a
    sphere surface). Cells are bounded by great circle paths between their
    corner vertices. When calculating regrid weights, this line type is
    currently the default for the conservative regrid method.
    """

# LogKind
class LogKind(IntEnum):
    """
    This flag is used to specify how much logging should be done.
    """
    MULTI = 2
    """
    Use multiple log files -- one per PET.
    """
    NONE = 3
    """
    Do not issue messages to a log file.
    """

# MeshElemType
class MeshElemType(IntEnum):
    """
    A Mesh can be constructed from a combination of different elements.
    The type of elements that can be used in a Mesh depends on the
    parametric dimension of the Mesh, which is set during Mesh
    creation. The following are the valid Mesh element types for each
    valid Mesh parametric dimension (2D or 3D).
    """
    TRI = 3
    """
    2D triangular elements with 3 sides.
    """
    QUAD = 4
    """
    2D quadrilateral elements with 4 sides.
    """
    TETRA = 10
    """
    3D tetrahedral elements with 4 faces.
    """
    HEX = 12
    """
    3D hexahedral elements with 6 faces.
    """

# MeshLoc
class MeshLoc(IntEnum):
    """
    The Mesh location used to hold Field data.
    """
    NODE = 0
    """
    The nodes of the Mesh.
    """
    ELEMENT = 1
    """
    The elements of the Mesh.
    """

# NormType
class NormType(IntEnum):
    """
    When doing conservative regridding, this option allows the user
    to select the type of normalization used when producing the weights.
    """
    DSTAREA = 0
    """
    Destination area normalization. Here the weights are calculated by dividing
    the area of overlap of the source and destination cells by the area of the
    entire destination cell. In other words, the weight is the fraction of the
    entire destination cell which overlaps with the given source cell.
    """
    FRACAREA = 1
    """
    Fraction area normalization. Here in addition to the weight calculation
    done for destination area normalization the weights are also divided by the
    fraction that the destination cell overlaps with the entire source grid. In
    other words, the weight is the fraction of just the part of the destination
    cell that overlaps with the entire source mesh.
    """

# PoleMethod
class PoleMethod(IntEnum):
    """
    Indicates which type of artificial pole to construct on the source
    Grid for regridding.
    """
    NONE = 0
    """
    No pole. Destination points which lie above the top or below the bottom row
    of the source Grid won't be mapped.
    """
    ALLAVG = 1
    """
    Construct an artificial pole placed in the center of the top (or bottom)
    row of nodes, but projected onto the sphere formed by the rest of the grid.
    The value at this pole is the average of all the source values surrounding
    the pole.
    """
    NPNTAVG = 2
    """
    Construct an artificial pole placed in the center of the top (or bottom)
    row of nodes, but projected onto the sphere formed by the rest of the grid.
    The value at this pole is the average of the N source nodes next to the pole
    and surrounding the destination point (i.e. the value may differ for each
    destination point). Here N is set by using the regridPoleNPnts parameter
    and ranges from 1 to the number of nodes around the pole. This option is
    useful for interpolating values which may be zeroed out by averaging around
    the entire pole (e.g. vector components).
    """
    TEETH = 3
    """
    No new pole point is constructed, instead the holes at the poles are filled
    by constructing triangles across the top and bottom row of the source Grid.
    This can be useful because no averaging occurs, however, because the top and
    bottom of the sphere are now flat, for a big enough mismatch between the
    size of the destination and source pole holes, some destination points may
    still not be able to be mapped to the source Grid.
    """

# Region
class Region(IntEnum):
    """
    Specify various regions in the data layout of a Field object.
    """
    TOTAL = 0
    """
    An operation applies to every element in the selected domain.
    """
    SELECT = 1
    """
    An operation applies to a select portion of the domain. One use of this is
    to specify that the portions of a Field that are not mapped in a regridding
    operation should retain their original value (as opposed to being
    initialized to 0).
    """
    EMPTY = 2
    """
    An operation does not apply any element in the domain.
    """

# RegridMethod
class RegridMethod(IntEnum):
    """
    Specify which interpolation method to use during regridding.
    """
    BILINEAR = 0
    """
    Bilinear interpolation. Destination value is a linear combination of the
    source values in the cell which contains the destination point. The weights
    for the linear combination are based on the distance of the destination
    point from each source value.
    """
    PATCH = 1
    """
    Higher-order patch recovery interpolation. Destination value is a weighted
    average of 2D polynomial patches constructed from cells surrounding the
    source cell which contains the destination point. This method typically
    results in better approximations to values and derivatives than bilinear.
    However, because of its larger stencil, it also results in a much larger
    interpolation matrix than the bilinear method.
    """
    CONSERVE = 2
    """
    First order conservative interpolation. Value of a destination cell is the
    weighted sum of the values of the source cells that it overlaps. The
    weights are determined by the amount the source cell overlaps the
    destination cell. This method will typically give less accurate
    approximations to values than the other interpolation methods, however, it
    will do a much better job preserving the integral of the value between the
    source and destination. This method requires the corner coordinate values
    to be provided in the Grid, and it currently only works for Fields created
    on the Grid center stagger (or the Mesh element location).
    """
    NEAREST_STOD = 3
    """
    Nearest neighbor interpolation where each destination point is mapped to
    the closest source point. A given source point may go to multiple
    destination points, but no destination point will receive input from more
    than one source point.
    """
    NEAREST_DTOS = 4
    """
    Nearest neighbor interpolation where each destination point is mapped to
    the closest source point. A given source point may go to multiple
    destination points, but no destination point will receive input from more
    than one source point.
    """

# StaggerLoc
class StaggerLoc(IntEnum):
    """
    In the Grid class, data can be located at different positions in a
    Grid cell. When setting or retrieving coordinate data the stagger
    location is specified to tell the Grid method from where in the
    cell to get the data.
    """
    INVALID = -2
    UNINIT = -1
    CENTER = 0
    """
    2D: Cell centered stagger location.
    """
    EDGE1 = 1
    """
    2D: First dimension edge stagger location.
    """
    EDGE2 = 2
    """
    2D: Second dimension edge stagger location.
    """
    CORNER = 3,
    """
    2D: Cell corner stagger location.
    """
    CENTER_VCENTER = 0
    """
    3D: Vertically centered stagger location.
    """
    EDGE1_VCENTER = 1
    """
    3D: Vertically centered first dimension edge stagger location.
    """
    EDGE2_VCENTER = 2
    """
    3D: Vertically centered second dimension edge stagger location.
    """
    CORNER_VCENTER = 3
    """
    3D: Vertically centered corner stagger location.
    """
    CENTER_VFACE = 4
    """
    3D: Centered stagger location of the top and bottom cell faces.
    """
    EDGE1_VFACE = 5
    """
    3D: First dimension edge stagger location of the top and bottom cell faces.
    """
    EDGE2_VFACE = 6
    """
    3D: Second dimension edge stagger location of the top and bottom cell faces.
    """
    CORNER_VFACE = 7
    """
    3D: Corner stagger location of the top and bottom cell faces.
    """

# TypeKind
class TypeKind(IntEnum):
    """
    This is used to indicate the type and kind of ESMPy types to the
    underlying ESMF library routines.
    """
    I4 = 3
    """
    A four byte integer, equivalent to numpy.int32.
    """
    I8 = 4
    """
    An eight byte integer, equivalent to numpy.int64.
    """
    R4 = 5
    """
    A four byte real, equivalent to numpy.float32.
    """
    R8 = 6
    """
    An eight byte real, equivalent to numpy.float64.
    """

# UnmappedAction
class UnmappedAction(IntEnum):
    """
    This is used to indicate what action to take with respect to unmapped destination
    points and the entries of the sparse matrix that correspond to
    these points.
    """
    ERROR = 0
    """
    Unmapped points result in an error code return.
    """
    IGNORE = 1
    """
    Unmapped points are ignored.
    """
