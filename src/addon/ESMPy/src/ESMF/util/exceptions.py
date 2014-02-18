# $Id$

"""
exceptions
"""

#### EXCEPTIONS #########################################################

class ESMPyException(Exception):
    """Base class for errors in the ESMPy package."""
    pass

class RequiredArgs(ESMPyException):
    """Required arguments were not specified."""
    pass

class NetCDFMissing(ESMPyException):
    """ESMF was not built with the NetCDF package."""
    pass

class ESMPyWarning(Warning):
    """Base class for warnings in the ESMPy package"""
    pass

class MethodNotImplemented(ESMPyException):
    """Raised when an unimplemented method is called."""
    pass

class GridException(ESMPyException):
    """Base class for errors in the Grid class."""
    pass

class GridItemNotSupported(GridException):
    """Raised when an undefined Grid item is specified."""
    pass

class GridBoundsNotCreated(GridException):
    """Raised when the Grid bounds are accessed before they are 
       created."""
    pass

class GridItemAlreadyLinked(GridException):
    """Raised when an attempt is made to re-link a Grid item."""
    pass

class GridCoordsAlreadyLinked(GridException):
    """Raised when an attempt is made to re-link the coordinates of a 
       Grid."""
    pass

class GridSingleStaggerloc(GridException):
    """Raised if a staggerloc argument is passed as a list or a tuple
       where it can only be a single value."""
    pass

class GridArgumentError(GridException):
    """Raised if a Grid optional argument is used incorrectly."""
    pass

class GridWarning(ESMPyWarning):
    """Base class for warnings in the Grid class."""
    pass

class GridArgumentWarning(GridWarning):
    """Warning given for using extraneous Grid optional arguments"""
    pass

class MeshException(ESMPyException):
    """Base class for errors in the Mesh class."""
    pass

class MeshLocationNotSupported(MeshException):
    """Raised when an undefined Mesh location is specified."""
    pass

class MeshArgumentError(MeshException):
    """Raised if a Mesh optional argument is used incorrectly."""
    pass

class MeshWarning(ESMPyWarning):
    """Base class for warnings in the Mesh class."""
    pass

class MeshArgumentWarning(MeshWarning):
    """Warning given for using extraneous Mesh optional arguments"""
    pass

class FieldException(ESMPyException):
    """Base class for errors in the Field class."""
    pass

class FieldDOError(FieldException):
    """Raised when an attempt is made to build a Field on an undefined 
       discretization object (e.g. not a Grid or a Mesh)"""
    pass

class FieldWarning(ESMPyWarning):
    """Base class for warnings in the Field class."""
    pass

class FieldDestroyDisabled(FieldWarning):
    """Warning given for using a disabled version of ESMP_FieldDestroy
       to avoid a neverending segfault resulting from clashing 
       deallocations from ESMF and Python"""
    pass

class TestException(ESMPyException):
    """Base class for errors in the ESMPy testing."""
    pass

class TestGridWriteBeforeCoords(TestException):
    """Raised when an attempt is made to write a Grid before c
       coordinates have been specified"""
    pass
