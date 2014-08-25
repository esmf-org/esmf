# $Id$

import numpy as np

from collections import namedtuple

# error message
_errmsg = 'Please check the log files (named "*ESMF_LogFile").'
_ESMP_SUCCESS = 0
# ESMF TYPEKINDS mapped to Python types
_ESMF2PythonType = (0,0,0, np.int32, np.int64, np.float32, np.float64)

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
CoordSys = namedtuple('CoordSys', ['INVALID', 'UNINIT', 'CART', 'SPH_DEG', 
                                   'SPH_RAD'])
CoordSys = CoordSys(INVALID=-2, UNINIT=-1, CART=0, SPH_DEG=1, SPH_RAD=2)

# This is a dummy class object used for documentation purposes only
class _CoordSys(object):
    '''
    This flag indicates the coordinate system of a Grid. This value is 
    useful both to indicate to other users the type of the coordinates, 
    but also to control how the coordinates are interpreted in 
    regridding methods (e.g. Regrid()).
    Values are:
        INVALID = -2
        UNINIT = -1
        CART = 0
            Cartesian coordinate system. In this system, the Cartesian 
            coordinates are mapped to the Grid coordinate dimensions in the 
            following order: x,y,z. (e.g. using coord_dim=1 in 
            Grid.get_coords() references the y dimension)
        SPH_DEG = 1
            Spherical coordinates in degrees. In this system, the spherical
            coordinates are mapped to the Grid coordinate dimensions in the 
            following order: longitude, latitude, radius. (E.g. using 
            coord_dim=1 in Grid.get_coords() references the latitude dimension) 
            Note, however, that Regrid() currently just supports longitude and 
            latitude (i.e. with this system, only Grids of dimension 2 are 
            supported in the regridding).
        SPH_RAD = 2
            Spherical coordinates in radians. In this system, the spherical
            coordinates are mapped to the Grid coordinate dimensions in the 
            following order: longitude, latitude, radius. (E.g. using 
            coord_dim=1 in Grid.get_coords() references the latitude dimension) 
            Note, however, that Regrid() currently just supports longitude and 
            latitude (i.e. with this system, only Grids of dimension 2 are 
            supported in the regridding).
    '''

# DecompFlag
DecompFlag = namedtuple('DecompFlag', ['DEFAULT', 'BALANCED', 'RESTFIRST', 'RESTLAST', 
                                       'CYCLIC'])
DecompFlag = DecompFlag(DEFAULT=0, BALANCED=1, RESTFIRST=2, RESTLAST=3, CYCLIC=4)

# This is a dummy class object used for documentation purposes only
class _DecompFlag(object):
    '''
    This flag indicates how DistGrid elements are decomposed over DEs.
    Values are:
        DEFAULT = 0
            Use default decomposition behavior. Currently equal to 'BALANCED'.
        BALANCED = 1
            Decompose elements as balanced as possible across DEs. The maximum 
            difference in number of elements per DE is 1, with the extra elements on
            the lower DEs.
        RESTFIRST = 2
            Divide elements over DEs. Assign the rest of this division to the first DE.
        RESTLAST = 3
            Divide elements over DEs. Assign the rest of this division to the last DE.
        CYCLIC = 4
            Decompose elements cyclically across DEs.
    '''

# FileFormat
FileFormat = namedtuple('FileFormat', ['UNDEFINED', 'VTK', 'SCRIP', 'ESMFMESH',
                                       'ESMFGRID', 'UGRID', 'GRIDSPEC'])
FileFormat = FileFormat(UNDEFINED=0, VTK=1, SCRIP=2, ESMFMESH=3, ESMFGRID=4,
                        UGRID=5, GRIDSPEC=6)

# This is a dummy class object used for documentation purposes only
class _FileFormat(object):
    '''
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
            Use a single tile grid file conforming with the proposed
        CF-GRIDSPEC conventions.
    '''


# GridItem
GridItem = namedtuple('GridItem', ['INVALID', 'UNINIT', 'MASK', 'AREA'])
GridItem = GridItem(INVALID=-2, UNINIT=-1, MASK=0, AREA=1)

# This is a dummy class object used for documentation purposes only
class _GridItem(object):
    '''
    The Grid can contain other kinds of data besides coordinates. This 
    data is referred to as Grid ``items''. Some items may be used 
    for calculations involving the Grid.
    Values are:
        INVALID = -2
        UNINIT = -1
        MASK = 0
            A grid item to represent a mask
        AREA = 1
            A grid item to represent an area field for conservative regridding
    '''

# LogKind
LogKind = namedtuple('LogKind', ['MULTI', 'NONE'])
LogKind = LogKind(MULTI=2, NONE=3)

# This is a dummy class object used for documentation purposes only
class _LogKind(object):
    '''
    This flag is used to specify how much logging should be done.
    Values are:
        MULTI = 2
            Use multiple log files -- one per PET.
        NONE = 3
            Do not issue messages to a log file.
    '''

# MeshElemType
MeshElemType = namedtuple('MeshElemType', ['TRI', 'QUAD', 'TETRA', 'HEX'])
MeshElemType = MeshElemType(TRI=5, QUAD=9, TETRA=10, HEX=12)

# This is a dummy class object used for documentation purposes only
class _MeshElemType(object):
    '''
    An Mesh can be constructed from a combination of different elements.
    The type of elements that can be used in a Mesh depends on the 
    parametric dimension of the Mesh, which is set during Mesh 
    creation. The following are the valid Mesh element types for each 
    valid Mesh parametric dimension (2D or 3D).
    Values are:
        TRI = 5
            2D triangular elements with 3 sides
        QUAD = 9
            2D quadrilateral elements with 4 sides
        TETRA = 10
            3D tetrahedral elements with 4 faces
        HEX = 12
            3D hexahedral elements with 6 faces
    '''

# MeshLoc
MeshLoc = namedtuple('MeshLoc', ['NODE', 'ELEMENT'])
MeshLoc = MeshLoc(NODE=1, ELEMENT=2)

# This is a dummy class object used for documentation purposes only
class _MeshLoc(object):
    '''
    The Mesh location used to hold Field data.
    Values are:
        NODE = 1
            The nodes of the Mesh
        ELEMENT = 2
            The elements of the Mesh
    '''

# Region
Region = namedtuple('Region', ['TOTAL', 'SELECT', 'EMPTY'])
Region = Region(TOTAL=0, SELECT=1, EMPTY=2)

# This is a dummy class object used for documentation purposes only
class _Region(object):
    '''
    Specify various regions in the data layout of a Field object.
    Values are:
        TOTAL = 0
            An operation applies to every element in the selected domain
        SELECT = 1
            An operation applies to a select portion of the domain
        EMPTY = 2
            An operation does not apply any element in the domain
    '''

# RegridMethod
RegridMethod = namedtuple('RegridMethod', ['BILINEAR', 'PATCH', 'CONSERVE', 
                                           'NEAREST_STOD', 'NEAREST_DTOS'])
RegridMethod = RegridMethod(BILINEAR=0, PATCH=1, CONSERVE=2, NEAREST_STOD=3, NEAREST_DTOS=4)

# This is a dummy class object used for documentation purposes only
class _RegridMethod(object):
    '''
    Specify which interpolation method to use during regridding.
    Values are:
        BILINEAR = 0
            Bilinear interpolation. Destination value is a linear combination
            of the source values in the cell which contains the destination 
            point. The weights for the linear combination are based on the 
            distance of the destination point from each source value.
        PATCH = 1
            Higher-order patch recovery interpolation. Destination value is a
            weighted average of 2D polynomial patches constructed from cells 
            surrounding the source cell which contains the destination point. 
            This method typically results in better approximations to values 
            and derivatives than bilinear.  However, because of its larger 
            stencil, it also results in a much larger interpolation matrix than 
            the bilinear method.
        CONSERVE = 2
            First order conservative interpolation. Value of a destination cell
            is the weighted sum of the values of the source cells that it 
            overlaps. The weights are determined by the amount the source cell 
            overlaps the destination cell. This method will typically give less 
            accurate approximations to values than the other interpolation 
            methods, however, it will do a much better job preserving the 
            integral of the value between the source and destination. This
            method requires the corner coordinate values to be provided in the 
            Grid, and it currently only works for Fields created on the Grid 
            center stagger (or the Mesh element location).
        NEAREST_STOD = 3
            Nearest neighbor interpolation where each destination
            point is mapped to the closest source point. A given
            source point may go to multiple destination points, but
            no destination point will receive input from more than
            one source point.
        NEAREST_DTOS = 4
            Nearest neighbor interpolation where each destination
            point is mapped to the closest source point. A given
            source point may go to multiple destination points, but no
            destination point will receive input from more than one
            source point.
    '''

# StaggerLoc
StaggerLoc = namedtuple('StaggerLoc', ['INVALID', 'UNINIT', 'CENTER', 'EDGE1',
                                       'EDGE2', 'CORNER',
                                       'CENTER_VCENTER', 'EDGE1_VCENTER',
                                       'EDGE2_VCENTER', 'CORNER_VCENTER',
                                       'CENTER_VFACE', 'EDGE1_VFACE',
                                       'EDGE2_VFACE', 'CORNER_VFACE'])
StaggerLoc = StaggerLoc(INVALID=-2, UNINIT=-1, CENTER=0, EDGE1=1,
                        EDGE2=2, CORNER=3,
                        CENTER_VCENTER=0, EDGE1_VCENTER=1,
                        EDGE2_VCENTER=2, CORNER_VCENTER=3,
                        CENTER_VFACE=4, EDGE1_VFACE=5,
                        EDGE2_VFACE=6, CORNER_VFACE=7)

# This is a dummy class object used for documentation purposes only
class _StaggerLoc(object):
    '''
    In the Grid class, data can be located at different positions in a 
    Grid cell. When setting or retrieving coordinate data the stagger 
    location is specified to tell the Grid method from where in the 
    cell to get the data.
    Values are:
        2D STAGGER LOCATIONS:
            INVALID = -2
            UNINIT = -1
            CENTER = 0
                Cell centered stagger location
            EDGE1 = 1
                First dimension (latitude) edge stagger location
            EDGE2 = 2
                Second dimension (longitude) edge stagger location
            CORNER = 3
                Cell corner stagger location
        3D STAGGER LOCATIONS:
            CENTER_VCENTER = 0
                Vertically centered stagger location
            EDGE1_VCENTER = 1
                Vertically centered first dimension (latitude) edge stagger location
            EDGE2_VCENTER = 2
                Vertically centered second dimension (longitude) edge stagger location
            CORNER_VCENTER = 3
                Vertically centered corner stagger location
            CENTER_VFACE = 4
                Centered stagger location of the top and bottom cell faces
            EDGE1_VFACE = 5
                First dimension (latitude) edge stagger location of the top and bottom cell faces
            EDGE2_VFACE = 6
                Second dimension (longitude) edge stagger location of the top and bottom cell faces
            CORNER_VFACE = 7
                Corner stagger location of the top and bottom cell faces
    '''

# TypeKind
TypeKind = namedtuple('TypeKind', ['I4', 'I8', 'R4', 'R8'])
TypeKind = TypeKind(I4=3, I8=4, R4=5, R8=6)

# This is a dummy class object used for documentation purposes only
class _TypeKind(object):
    '''
    Named constants used to indicate type and kind combinations 
    supported by the ESMPy interfaces.
    Values are:
        I4 = 3
            Four byte integer
        I8 = 4
            Eight byte integer
        R4 = 5
            Four byte real
        R8 = 6
            Eight byte real
    '''

# UnmappedAction
UnmappedAction = namedtuple('UnmappedAction', ['ERROR', 'IGNORE'])
UnmappedAction = UnmappedAction(ERROR=0, IGNORE=1)

# This is a dummy class object used for documentation purposes only
class _UnmappedAction(object):
    '''
    Indicates what action to take with respect to unmapped destination 
    points and the entries of the sparse matrix that correspond to 
    these points.
    Values are:
        ERROR = 0
            Unmapped points result in an error code return
        IGNORE = 1
            Unmapped points are ignored
    '''

# PoleMethod
PoleMethod = namedtuple('PoleMethod', ['NONE', 'ALLAVG', 'NPNTAVG', 'TEETH'])
PoleMethod = PoleMethod(NONE=0, ALLAVG=1, NPNTAVG=2, TEETH=3)

# This is a dummy class object used for documentation purposes only
class _PoleMethod(object):
    '''
    Indicates which type of artificial pole to construct on the source 
    Grid for regridding..
    Values are:
        NONE = 0
            No pole. Destination points which lie above the top or
            below the bottom row of the source Grid won't be mapped.
        ALLAVG = 1
            Construct an artificial pole placed in the center of the
            top (or bottom) row of nodes, but projected onto the
            sphere formed by the rest of the grid. The value at this
            pole is the average of all the source values surrounding
            the pole.
        NPNTAVG = 2
            Construct an artificial pole placed in the center of the
            top (or bottom) row of nodes, but projected onto the
            sphere formed by the rest of the grid. The value at this
            pole is the average of the N source nodes next to the pole
            and surrounding the destination point (i.e. the value may
            differ for each destination point). Here N is set by using
            the regridPoleNPnts parameter and ranges from 1 to
            the number of nodes around the pole. This option is useful
            for interpolating values which may be zeroed out by
            averaging around the entire pole (e.g. vector components).
        TEETH = 3
            No new pole point is constructed, instead the holes at the
            poles are filled by constructing triangles across the top
            and bottom row of the source Grid. This can be useful
            because no averaging occurs, however, because the top and
            bottom of the sphere are now flat, for a big enough
            mismatch between the size of the destination and source
            pole holes, some destination points may still not be able
            to be mapped to the source Grid.
    '''

