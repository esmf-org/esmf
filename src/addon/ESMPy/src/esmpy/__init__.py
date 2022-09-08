# $Id$

"""
ESMPy is a Python interface to the Earth System Modeling Framework (ESMF) 
regridding utility. ESMF is software for
building and coupling weather, climate, and related models. ESMF has a robust,
parallel and scalable remapping package, used to generate remapping weights.
It can handle a wide variety of grids and options: logically rectangular grids,
unstructured meshes and sets of unconnected points; regional or global grids;
2D or 3D; and pole and masking options. ESMF also has capabilities to read grid
information from NetCDF files in a variety of formats, including the
`Climate and Forecast (CF) V1.6 (a.k.a GridSpec) <http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html>`_,
`UGRID <https://github.com/ugrid-conventions/ugrid-conventions>`_,
ESMF Unstructured and SCRIP conventions. Information on these last two conventions
can be found in the
`ESMF Reference Manual <http://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/>`_.

ESMPy provides a :class:`~esmpy.api.grid.Grid` to represent single-tile logically
rectangular coordinate data, a :class:`~esmpy.api.mesh.Mesh` for unstructured 
coordinates, and a :class:`~esmpy.api.locstream.LocStream` for collections of
unconnected points like observational data streams.
ESMPy supports bilinear, nearest neighbor, higher order patch recovery, 
first-order conservative and second-order conservative regridding. There is 
also an option to ignore unmapped destination points, mask out points on either
the source or destination, choose straight line or great circle paths when using
spherical coordinates and extrapolate data to points outside of the destination
domain. Regridding on the sphere takes place in 3D Cartesian space, so the pole
problem is not an issue as it commonly is with many Earth system grid remapping
softwares. :class:`~esmpy.api.grid.Grid` and :class:`~esmpy.api.mesh.Mesh` 
objects can be created in 2D or 3D space, and 3D conservative regridding is 
fully supported.

Regridding, also called remapping or interpolation, is the process of changing
the grid underneath field data values while preserving the qualities of the
original data. Different kinds of transformations are appropriate for
different problems. Regridding may be needed when communicating data between
Earth system modeling components such as land and atmosphere, or between
different data sets to support analysis or visualization.

Regridding can be broken into two stages. The first stage is generation of an
interpolation weight matrix that describes how points in the source grid
contribute to points in the destination grid. The second stage is the
multiplication of values on the source grid by the interpolation weight matrix
to produce the appropriate values on the destination grid. ESMPy provides
access to both stages through two separate interfaces.

There are many different interpolation methods, suitable for different problems.
In ESMPy, the basic bilinear option is a two dimensional variant of linear 
interpolation. The higher order patch recovery is a second degree polynomial 
regridding method, which uses a least squares algorithm to calculate the 
polynomial. This method gives better derivatives in the resulting destination 
field than the bilinear. There are two nearest-neighbor methods which map the 
points from one grid to the nearest corresponding point on the other grid, from
either source to destination or vice versa. These are useful for extrapolation 
or categorical fields. 

The first-order conservative regridding is a method designed to preserve the 
integral of the field across the interpolation from source to destination.  It 
uses the proportion of the area of the overlapping source and destination cells
to determine appropriate weights. The second-order conservative method also 
preserves the integral, but uses the source gradient to give a smoother result 
than the first-order conservative. All of these methods can be broken down to a
simple sparse matrix multiplication operation between interpolation weights and
data values.
"""

#### SET UP SOME INFO #########################################################

import esmpy.api.constants as constants

import sys

msg = ""

if (sys.version_info >= (3,8)):
    # this requires Python 3.8 or higher
    import importlib.metadata as ilm
    
    msg = ilm.metadata("ESMPy")

else:
    # pre Python 3.8, not sure how far yet
    from pkg_resources import get_distribution
    try: 
        pkgInfo = get_distribution('ESMPy').get_metadata('METADATA')
    except:
        try:
            pkgInfo = get_distribution('ESMPy').get_metadata('PKG-INFO')
        except:
            raise ImportError("Could not find METADATA or PKG-INFO for ESMPy")
    
    # parse it using email.Parser
    from email import message_from_string
    msg = message_from_string(pkgInfo)

# set the private metadata
__name__ = msg["Name"]
__version__ = msg["Version"]
constants._ESMPY_VERSION = __version__ # required to check against ESMF_VERSION in loadESMF
__license__ = msg["License"]
__email__ = msg["Maintainer-email"]
__description__ = msg["Summary"]
__requires__ = msg["Requires-Dist"]
__requires_python__ = msg["Requires-Python"]
# these don't seem to work with setuptools pyproject.toml
__author__ = msg["Author"]
__homepage__ = msg["Home-page"]
__obsoletes__ = msg["obsoletes"]

#### IMPORT LIBRARIES #########################################################

from esmpy.api.esmpymanager import *
from esmpy.api.grid import *
from esmpy.api.mesh import *
from esmpy.api.locstream import *
from esmpy.api.field import *
from esmpy.api.regrid import *
from esmpy.api.constants import *
from esmpy.util.helpers import *
