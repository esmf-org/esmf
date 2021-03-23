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
_ESMF_MPIRUN = None
_ESMF_MPIRUN_NP = None

#### NAMED CONSTANTS ##########################################################

# CoordSys
class CoordSys(IntEnum):
    """
    This flag indicates the coordinate system of a :class:`~ESMF.api.grid.Grid`. This value is
    useful both to indicate to other users the type of the coordinates,
    but also to control how the coordinates are interpreted in
    regridding methods (e.g. :class:`~ESMF.api.regrid.Regrid`).
    """
    INVALID = -2
    UNINIT = -1
    CART = 0
    """
    Cartesian coordinate system. In this system, the Cartesian
    coordinates are mapped to the :class:`~ESMF.api.grid.Grid` coordinate dimensions in the
    following order: x, y, z. (e.g. using coord_dim=1 in
    :class:`~ESMF.api.grid.Grid.get_coords()` references the y dimension)
    """
    SPH_DEG = 1
    """
    Spherical coordinates in degrees. In this system, the spherical
    coordinates are mapped to the :class:`~ESMF.api.grid.Grid` coordinate dimensions in the
    following order: longitude, latitude, radius. (E.g. using
    coord_dim=1 in :class:`~ESMF.api.grid.:class:`~ESMF.api.grid.Grid.get_coords()`` references the latitude dimension).
    """
    SPH_RAD = 2
    """
    Spherical coordinates in radians. In this system, the spherical
    coordinates are mapped to the :class:`~ESMF.api.grid.Grid` coordinate dimensions in the
    following order: longitude, latitude, radius. (E.g. using
    coord_dim=1 in :class:`~ESMF.api.grid.Grid.get_coords()` references the latitude dimension).
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

# ExtrapMethod
class ExtrapMethod(IntEnum):
    """
    Specify which extrapolation method to use on unmapped destination points after 
    regridding.
    """
    NONE = 0
    """
    Indicates that no extrapolation should be done.
    """
    NEAREST_STOD = 1
    """
    Inverse distance weighted average. 
    Here the value of a destination point is the weighted average of the 
    closest N source points. The weight is the reciprocal of the distance of 
    the source point from the destination point raised to a power P. All the
    weights contributing to one destination point are normalized so that they 
    sum to 1.0. The user can choose N and P when using this method, but 
    defaults are also provided.
    """
    NEAREST_IDAVG = 2
    """
    Nearest source to destination. 
    Here each destination point is mapped to the closest source point. A given 
    source point may go to multiple destination points, but no destination 
    point will receive input from more than one source point.
    """
    CREEP_FILL = 3
    """
    Creep fill extrapolation.
    Here unmapped destination points are filled by repeatedly moving data from
    mapped locations to neighboring unmapped locations. The data filled into a 
    new location is the average of its already filled neighbors' values. This 
    process is repeated for a user specified number of levels (e.g. in Regrid() 
    this is specified via the extrap_num_levels parameter). This extrapolation 
    method is not supported with conservative regrid methods.
    """

# FileFormat
class FileFormat(IntEnum):
    """
    The :class:`~ESMF.api.grid.Grid` and :class:`~ESMF.api.mesh.Mesh` objects 
    may be created from specifications in a NetCDF data file.  This flag 
    indicates the format of the data file.
    """
    UNDEFINED = 0
    VTK = 1
    """
    Use the VTK file format.
    """
    SCRIP = 2
    """
    Use the :ref:`SCRIP <scrip>` file format.
    """
    ESMFMESH = 3
    """
    Use the :ref:`ESMFMESH <esmfmesh>` unstructured grid file format.
    """
    ESMFGRID = 4
    """
    Use the ESMF structured grid file format.
    """
    UGRID = 5
    """
    Use the :ref:`UGRID <ugrid>` unstructured grid file format.
    """
    GRIDSPEC = 6
    """
    Use the :ref:`UGRID single tile grid file format based on CF V1.6 conventions (a.k.a GRIDSPEC) <gridspec>`.
    """

# FileMode
class FileMode(IntEnum):
    """
    Specify which mode to use when writing a weight file.
    """
    BASIC = 0
    """
    Indicates that only the factorList and factorIndexList should be written.
    """
    WITHAUX = 1
    """
    Indicates that grid center coordinates should also be written.
    """

# GridItem
class GridItem(IntEnum):
    """
    The :class:`~ESMF.api.grid.Grid` can contain other kinds of data besides coordinates. This
    data is referred to as :class:`~ESMF.api.grid.Grid` "items". Some items may be used
    for calculations involving the :class:`~ESMF.api.grid.Grid`.
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
    A :class:`~ESMF.api.mesh.Mesh` can be constructed from a combination of different elements.
    The type of elements that can be used in a :class:`~ESMF.api.mesh.Mesh` depends on the
    parametric dimension of the :class:`~ESMF.api.mesh.Mesh`, which is set during :class:`~ESMF.api.mesh.Mesh`
    creation. The following are the valid :class:`~ESMF.api.mesh.Mesh` element types for each
    valid :class:`~ESMF.api.mesh.Mesh` parametric dimension (2D or 3D).
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
    The :class:`~ESMF.api.mesh.Mesh` location used to hold :class:`~ESMF.api.field.Field` data.
    """
    NODE = 0
    """
    The nodes of the :class:`~ESMF.api.mesh.Mesh`.
    """
    ELEMENT = 1
    """
    The elements of the :class:`~ESMF.api.mesh.Mesh`.
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

# PoleKind
class PoleKind(IntEnum):
    """
    This type describes the type of connection that occurs at the pole of a  
    :class:`~ESMF.api.grid.Grid`.
    """
    NONE = 0
    """
    No connection at pole.
    """
    MONOPOLE = 1
    """
    This edge is connected to itself. Given that the edge is n elements long, 
    then element i is connected to element i+n/2.
    """
    BIPOLE = 2
    """
    This edge is connected to itself. Given that the edge is n elements long, element i is connected to element n-i-1.
    """

# PoleMethod
class PoleMethod(IntEnum):
    """
    Indicates which type of artificial pole to construct on the source
    :class:`~ESMF.api.grid.Grid` for regridding.
    """
    NONE = 0
    """
    No pole. Destination points which lie above the top or below the bottom row
    of the source :class:`~ESMF.api.grid.Grid` won't be mapped.
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
    by constructing triangles across the top and bottom row of the source :class:`~ESMF.api.grid.Grid`.
    This can be useful because no averaging occurs, however, because the top and
    bottom of the sphere are now flat, for a big enough mismatch between the
    size of the destination and source pole holes, some destination points may
    still not be able to be mapped to the source :class:`~ESMF.api.grid.Grid`.
    """

# Region
class Reduce(IntEnum):
    """
    Indicates reduce operation.
    """
    SUM = 1
    """
    Use arithmetic sum to add all data elements.
    """
    MIN = 2
    """
    Determine the minimum of all data elements.
    """
    MAX = 3
    """
    Determine the maximum of all data elements.
    """

# Region
class Region(IntEnum):
    """
    Specify various regions in the data layout of a :class:`~ESMF.api.field.Field` object.
    """
    TOTAL = 0
    """
    An operation applies to every element in the selected domain.
    """
    SELECT = 1
    """
    An operation applies to a select portion of the domain. One use of this is
    to specify that the portions of a :class:`~ESMF.api.field.Field` that are not mapped in a regridding
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
    source values in the cell which contains the destination point. The 
    weights for the linear combination are based on the distance of destination 
    point from each source value. 
    """
    PATCH = 1
    """
    Higher-order patch recovery interpolation. Destination value is a weighted 
    average of 2D polynomial patches constructed from cells surrounding the 
    source cell which contains the destination point. This method typically 
    results in better approximations to values and derivatives than bilinear. 
    However, because of its larger stencil, it also results in a much larger 
    interpolation matrix (and thus routeHandle) than the bilinear. 
    """
    CONSERVE = 2
    """
    First-order conservative interpolation. The main purpose of this method is 
    to preserve the integral of the field across the interpolation from source 
    to destination. In this method the value across each source cell is treated 
    as a constant, so it will typically have a larger interpolation error than 
    the bilinear or patch methods.  The value of a destination cell 
    is calculated as the weighted sum of the values of the source cells that it 
    overlaps. The weights are determined by the amount the source cell overlaps 
    the destination cell. This method requires corner coordinate values to be 
    provided in the :class:`~ESMF.api.grid.Grid`. It currently only works for 
    :class:`Fields <ESMF.api.field.Field>`s created on the 
    :class:`~ESMF.api.grid.Grid` center stagger or 
    the :class:`~ESMF.api.mesh.Mesh` element location. 
    """
    NEAREST_STOD = 3
    """
    In this version of nearest neighbor interpolation each destination point is 
    mapped to the closest source point. A given source point may go to multiple 
    destination points, but no destination point will receive input from more 
    than one source point. 
    """
    NEAREST_DTOS = 4
    """
    In this version of nearest neighbor interpolation each source point is 
    mapped to the closest destination point. A given destination point may 
    receive input from multiple source points, but no source point will go to 
    more than one destination point. 
    """
    CONSERVE_2ND = 5
    """
    Second-order conservative interpolation. This method's main purpose is to 
    preserve the integral of the field across the interpolation from source to 
    destination. The difference between the first and second-order conservative 
    methods is that the second-order takes the source gradient into account, so 
    it yields a smoother destination field that typically better matches the 
    source field. This difference between the first and second-order methods is 
    particularly apparent when going from a coarse source grid to a finer 
    destination grid. Another difference is that the second-order method does 
    not guarantee that after regridding the range of values in the destination 
    field is within the range of values in the source field. For example, if the 
    mininum value in the source field is 0.0, then it's possible that after 
    regridding with the second-order method, the destination field will contain 
    values less than 0.0. This method requires corner coordinate values to be 
    provided in the :class:`~ESMF.api.grid.Grid`. It currently only works for 
    :class:`Fields <ESMF.api.field.Field>`s created on the 
    :class:`~ESMF.api.grid.Grid` center stagger or 
    the :class:`~ESMF.api.mesh.Mesh` element location. 
    """

# StaggerLoc
class StaggerLoc(IntEnum):
    """
    In the :class:`~ESMF.api.grid.Grid` class, data can be located at different positions in a
    :class:`~ESMF.api.grid.Grid` cell. When setting or retrieving coordinate data the stagger
    location is specified to tell the :class:`~ESMF.api.grid.Grid` method from where in the
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
