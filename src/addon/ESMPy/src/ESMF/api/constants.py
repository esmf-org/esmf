# $Id$

import numpy as np

from collections import namedtuple

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

# ESMF_COMM
_ESMF_VERSION = None

#### NAMED CONSTANTS ##########################################################

# CoordSys
class _CoordSys(namedtuple('CoordSys', ['INVALID', 'UNINIT', 'CART', 'SPH_DEG',
                                   'SPH_RAD'])):
    """
    This flag indicates the coordinate system of a Grid. This value is
    useful both to indicate to other users the type of the coordinates,
    but also to control how the coordinates are interpreted in
    regridding methods (e.g. Regrid()).\n
    Values are:\n
        INVALID = -2\n
        UNINIT = -1\n
        CART = 0\n
            Cartesian coordinate system. In this system, the Cartesian
            coordinates are mapped to the Grid coordinate dimensions in the
            following order: x,y,z. (e.g. using coord_dim=1 in
            Grid.get_coords() references the y dimension)\n
        SPH_DEG = 1\n
            Spherical coordinates in degrees. In this system, the spherical
            coordinates are mapped to the Grid coordinate dimensions in the
            following order: longitude, latitude, radius. (E.g. using
            coord_dim=1 in Grid.get_coords() references the latitude dimension)
            Note, however, that Regrid() currently just supports longitude and
            latitude (i.e. with this system, only Grids of dimension 2 are
            supported in the regridding).\n
        SPH_RAD = 2\n
            Spherical coordinates in radians. In this system, the spherical
            coordinates are mapped to the Grid coordinate dimensions in the
            following order: longitude, latitude, radius. (E.g. using
            coord_dim=1 in Grid.get_coords() references the latitude dimension)
            Note, however, that Regrid() currently just supports longitude and
            latitude (i.e. with this system, only Grids of dimension 2 are
            supported in the regridding).\n
    """
    pass

CoordSys = _CoordSys(INVALID=-2, UNINIT=-1, CART=0, SPH_DEG=1, SPH_RAD=2)

# DecompFlag
class _DecompFlag(namedtuple('DecompFlag', ['DEFAULT', 'BALANCED', 'RESTFIRST', 'RESTLAST',
                                       'CYCLIC'])):
    """
    This flag indicates how object are distributed across the available computational
    resources.\n
    Values are:\n
        DEFAULT = 0\n
            Use default decomposition behavior. Currently equal to 'BALANCED'.\n
        BALANCED = 1\n
            Decompose elements as balanced as possible. The maximum
            difference in number of elements per DE is 1, with the extra elements on
            the lower DEs.\n
        RESTFIRST = 2\n
            Divide elements over DEs. Assign the rest of this division to the first DE.\n
        RESTLAST = 3\n
            Divide elements over DEs. Assign the rest of this division to the last DE.\n
        CYCLIC = 4\n
            Decompose elements cyclically across DEs.\n
    """
    pass

DecompFlag = _DecompFlag(DEFAULT=0, BALANCED=1, RESTFIRST=2, RESTLAST=3, CYCLIC=4)

# FileFormat
class _FileFormat(namedtuple('FileFormat', ['UNDEFINED', 'VTK', 'SCRIP', 'ESMFMESH',
                                       'ESMFGRID', 'UGRID', 'GRIDSPEC'])):
    """
    The Grid and Mesh objects may be created from specifications in a NetCDF
    data file.  This flag indicates the format of the data file.\n
    Values are:\n
        VTK = 1\n
            Use VTK file format.\n
        SCRIP = 2\n
            Use SCRIP file format.\n
        ESMFMESH = 3\n
            Use ESMF unstructured grid file format.\n
        ESMFGRID = 4\n
            Use ESMF structured grid file format.\n
         UGRID = 5\n
            Use CF UGRID unstructured grid file format.\n
        GRIDSPEC = 6\n
            Use a single tile grid file conforming with the proposed
            CF GRIDSPEC conventions.\n
    """
    pass

FileFormat = _FileFormat(UNDEFINED=0, VTK=1, SCRIP=2, ESMFMESH=3, ESMFGRID=4,
                        UGRID=5, GRIDSPEC=6)

# GridItem
class _GridItem(namedtuple('GridItem', ['INVALID', 'UNINIT', 'MASK', 'AREA'])):
    """
    The Grid can contain other kinds of data besides coordinates. This
    data is referred to as Grid "items". Some items may be used
    for calculations involving the Grid.\n
    Values are:\n
        INVALID = -2\n
        UNINIT = -1\n
        MASK = 0\n
            A grid item to represent a mask.\n
        AREA = 1\n
            A grid item to represent an area field for conservative regridding.\n
    """
    pass

GridItem = _GridItem(INVALID=-2, UNINIT=-1, MASK=0, AREA=1)

# LineType
class _LineType(namedtuple('LineType', ['CART', 'GREAT_CIRCLE'])):
    """
    This argument controls the path of the line which connects two points on the surface of
    the sphere. This in turn controls the path along which distances are calculated and the
    shape of the edges that make up a cell. Both of these quantities can influence how
    interpolation weights are calculated.  As would be expected, this argument is only
    applicable with grids which lie on the surface of a sphere. \n
    Values are:\n
        CART = 0\n
            Cartesian line. When this option is specified distances are calculated in a straight line through the
            3D Cartesian space in which the sphere is embedded. Cells are approximated by 3D planes bounded by 3D
            Cartesian lines between their corner vertices. When calculating regrid weights, this line type is
            currently the default for the following all regrid methods except for conservative.\n
        GREAT_CIRCLE = 1\n
            Great circle line. When this option is specified distances are calculated along a great circle path
            (the shortest distance between two points on a sphere surface). Cells are bounded by great circle
            paths between their corner vertices. When calculating regrid weights, this line type is currently the
            default for the conservative regrid method.\n
    """
    pass

LineType = _LineType(CART=0, GREAT_CIRCLE=1)

# LogKind
class _LogKind(namedtuple('LogKind', ['MULTI', 'NONE'])):
    """
    This flag is used to specify how much logging should be done.\n
    Values are:\n
        MULTI = 2\n
            Use multiple log files -- one per PET.\n
        NONE = 3\n
            Do not issue messages to a log file.\n
    """
    pass

LogKind = _LogKind(MULTI=2, NONE=3)

# MeshElemType
class _MeshElemType(namedtuple('MeshElemType', ['TRI', 'QUAD', 'TETRA', 'HEX'])):
    """
    A Mesh can be constructed from a combination of different elements.
    The type of elements that can be used in a Mesh depends on the
    parametric dimension of the Mesh, which is set during Mesh
    creation. The following are the valid Mesh element types for each
    valid Mesh parametric dimension (2D or 3D).\n
    Values are:\n
        TRI = 3\n
            2D triangular elements with 3 sides.\n
        QUAD = 4\n
            2D quadrilateral elements with 4 sides.\n
        TETRA = 10\n
            3D tetrahedral elements with 4 faces.\n
        HEX = 12\n
            3D hexahedral elements with 6 faces.\n
    """
    pass

MeshElemType = _MeshElemType(TRI=3, QUAD=4, TETRA=10, HEX=12)

# MeshLoc
class _MeshLoc(namedtuple('MeshLoc', ['NODE', 'ELEMENT'])):
    """
    The Mesh location used to hold Field data.\n
    Values are:\n
        NODE = 1\n
            The nodes of the Mesh.\n
        ELEMENT = 2\n
            The elements of the Mesh.\n
    """
    pass

MeshLoc = _MeshLoc(NODE=1, ELEMENT=2)

# NormType
class _NormType(namedtuple('NormType', ['DSTAREA', 'FRACAREA'])):
    """
    When doing conservative regridding, this option allows the user
    to select the type of normalization used when producing the weights.\n
    Values are:\n
        DSTAREA = 0\n
            Destination area normalization. Here the weights are calculated by dividing the area
            of overlap of the source and destination cells by the area of the entire destination
            cell. In other words, the weight is the fraction of the entire destination cell which
            overlaps with the given source cell.\n
        FRACAREA = 1\n
            Fraction area normalization. Here in addition to the weight calculation done for
            destination area normalization the weights are also divided
            by the fraction that the destination cell overlaps with the entire source grid. In
            other words, the weight is the fraction of just the part of the destination cell that
            overlaps with the entire source mesh.\n
    """
    pass

NormType = _NormType(DSTAREA=0, FRACAREA=1)

# PoleMethod
class _PoleMethod(namedtuple('PoleMethod', ['NONE', 'ALLAVG', 'NPNTAVG', 'TEETH'])):
    """
    Indicates which type of artificial pole to construct on the source
    Grid for regridding.\n
    Values are:\n
        NONE = 0\n
            No pole. Destination points which lie above the top or
            below the bottom row of the source Grid won't be mapped.\n
        ALLAVG = 1\n
            Construct an artificial pole placed in the center of the
            top (or bottom) row of nodes, but projected onto the
            sphere formed by the rest of the grid. The value at this
            pole is the average of all the source values surrounding
            the pole.\n
        NPNTAVG = 2\n
            Construct an artificial pole placed in the center of the
            top (or bottom) row of nodes, but projected onto the
            sphere formed by the rest of the grid. The value at this
            pole is the average of the N source nodes next to the pole
            and surrounding the destination point (i.e. the value may
            differ for each destination point). Here N is set by using
            the regridPoleNPnts parameter and ranges from 1 to
            the number of nodes around the pole. This option is useful
            for interpolating values which may be zeroed out by
            averaging around the entire pole (e.g. vector components).\n
        TEETH = 3\n
            No new pole point is constructed, instead the holes at the
            poles are filled by constructing triangles across the top
            and bottom row of the source Grid. This can be useful
            because no averaging occurs, however, because the top and
            bottom of the sphere are now flat, for a big enough
            mismatch between the size of the destination and source
            pole holes, some destination points may still not be able
            to be mapped to the source Grid.\n
    """
    pass

PoleMethod = _PoleMethod(NONE=0, ALLAVG=1, NPNTAVG=2, TEETH=3)

# Region
class _Region(namedtuple('Region', ['TOTAL', 'SELECT', 'EMPTY'])):
    """
    Specify various regions in the data layout of a Field object.\n
    Values are:\n
        TOTAL = 0\n
            An operation applies to every element in the selected domain.\n
        SELECT = 1\n
            An operation applies to a select portion of the domain. One
            use of this is to specify that the portions of a Field that are not
            mapped in a regridding operation should retain their original value
            (as opposed to being initialized to 0).\n
        EMPTY = 2\n
            An operation does not apply any element in the domain.\n
    """
    pass

Region = _Region(TOTAL=0, SELECT=1, EMPTY=2)

# RegridMethod
class _RegridMethod(namedtuple('RegridMethod', ['BILINEAR', 'PATCH', 'CONSERVE',
                                           'NEAREST_STOD', 'NEAREST_DTOS'])):
    """
    Specify which interpolation method to use during regridding.\n
    Values are:\n
        BILINEAR = 0\n
            Bilinear interpolation. Destination value is a linear combination
            of the source values in the cell which contains the destination
            point. The weights for the linear combination are based on the
            distance of the destination point from each source value.\n
        PATCH = 1\n
            Higher-order patch recovery interpolation. Destination value is a
            weighted average of 2D polynomial patches constructed from cells
            surrounding the source cell which contains the destination point.
            This method typically results in better approximations to values
            and derivatives than bilinear.  However, because of its larger
            stencil, it also results in a much larger interpolation matrix than
            the bilinear method.\n
        CONSERVE = 2\n
            First order conservative interpolation. Value of a destination cell
            is the weighted sum of the values of the source cells that it
            overlaps. The weights are determined by the amount the source cell
            overlaps the destination cell. This method will typically give less
            accurate approximations to values than the other interpolation
            methods, however, it will do a much better job preserving the
            integral of the value between the source and destination. This
            method requires the corner coordinate values to be provided in the
            Grid, and it currently only works for Fields created on the Grid
            center stagger (or the Mesh element location).\n
        NEAREST_STOD = 3\n
            Nearest neighbor interpolation where each destination
            point is mapped to the closest source point. A given
            source point may go to multiple destination points, but
            no destination point will receive input from more than
            one source point.\n
        NEAREST_DTOS = 4\n
            Nearest neighbor interpolation where each destination
            point is mapped to the closest source point. A given
            source point may go to multiple destination points, but no
            destination point will receive input from more than one
            source point.\n
    """
    pass

RegridMethod = _RegridMethod(BILINEAR=0, PATCH=1, CONSERVE=2, NEAREST_STOD=3, NEAREST_DTOS=4)

# StaggerLoc
class _StaggerLoc(namedtuple('StaggerLoc', ['INVALID', 'UNINIT', 'CENTER', 'EDGE1',
                                       'EDGE2', 'CORNER',
                                       'CENTER_VCENTER', 'EDGE1_VCENTER',
                                       'EDGE2_VCENTER', 'CORNER_VCENTER',
                                       'CENTER_VFACE', 'EDGE1_VFACE',
                                       'EDGE2_VFACE', 'CORNER_VFACE'])):
    """
    In the Grid class, data can be located at different positions in a
    Grid cell. When setting or retrieving coordinate data the stagger
    location is specified to tell the Grid method from where in the
    cell to get the data.\n
    Values are:\n
        2D STAGGER LOCATIONS:\n
            INVALID = -2\n
            UNINIT = -1\n
            CENTER = 0\n
                Cell centered stagger location.\n
            EDGE1 = 1\n
                First dimension edge stagger location.\n
            EDGE2 = 2\n
                Second dimension edge stagger location.\n
            CORNER = 3\n
                Cell corner stagger location.\n
        3D STAGGER LOCATIONS:\n
            CENTER_VCENTER = 0\n
                Vertically centered stagger location.\n
            EDGE1_VCENTER = 1\n
                Vertically centered first dimension edge stagger location.\n
            EDGE2_VCENTER = 2\n
                Vertically centered second dimension edge stagger location.\n
            CORNER_VCENTER = 3\n
                Vertically centered corner stagger location.\n
            CENTER_VFACE = 4\n
                Centered stagger location of the top and bottom cell faces.\n
            EDGE1_VFACE = 5\n
                First dimension edge stagger location of the top and bottom cell faces.\n
            EDGE2_VFACE = 6\n
                Second dimension edge stagger location of the top and bottom cell faces.\n
            CORNER_VFACE = 7\n
                Corner stagger location of the top and bottom cell faces.\n
    """
    pass

StaggerLoc = _StaggerLoc(INVALID=-2, UNINIT=-1, CENTER=0, EDGE1=1,
                        EDGE2=2, CORNER=3,
                        CENTER_VCENTER=0, EDGE1_VCENTER=1,
                        EDGE2_VCENTER=2, CORNER_VCENTER=3,
                        CENTER_VFACE=4, EDGE1_VFACE=5,
                        EDGE2_VFACE=6, CORNER_VFACE=7)

# TypeKind
class _TypeKind(namedtuple('TypeKind', ['I4', 'I8', 'R4', 'R8'])):
    """
    This is used to indicate the type and kind of ESMPy types to the
    underlying ESMF library routines.
    Values are:\n
        I4 = 3\n
            A four byte integer, equivalent to numpy.int32.\n
        I8 = 4\n
            An eight byte integer, equivalent to numpy.int64.\n
        R4 = 5\n
            A four byte real, equivalent to numpy.float32.\n
        R8 = 6\n
            An eight byte real, equivalent to numpy.float64.\n
    """
    pass

TypeKind = _TypeKind(I4=3, I8=4, R4=5, R8=6)

# UnmappedAction
class _UnmappedAction(namedtuple('UnmappedAction', ['ERROR', 'IGNORE'])):
    """
    This is used to indicate what action to take with respect to unmapped destination
    points and the entries of the sparse matrix that correspond to
    these points.\n
    Values are:\n
        ERROR = 0\n
            Unmapped points result in an error code return.\n
        IGNORE = 1\n
            Unmapped points are ignored.\n
    """
    pass

UnmappedAction = _UnmappedAction(ERROR=0, IGNORE=1)
