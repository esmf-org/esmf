# $Id$

'''
ESMPy is a Python interface to the Earth System Modeling Framework (ESMF) 
regridding utility.  ESMF is software for
building and coupling weather, climate, and related models.  ESMF has a robust,
parallel and scalable remapping package, used to generate remapping weights.
It can handle a wide variety of grids and options:  logically rectangular grids
and unstructured meshes; regional or global grids; 2D or 3D; and pole and
masking options.

ESMPy supports a single-tile logically rectangular discretization type called
Grid and an unstructured discretization type called Mesh (ESMF also
supports observational data streams).
ESMPy supports bilinear, finite element patch recovery and first-order
conservative regridding.  There is also an option to ignore unmapped
destination points and mask out points on either the source or destination.
Regridding on the sphere takes place in 3D Cartesian space, so the pole
problem is not an issue as it commonly is with many Earth system grid remapping
softwares.  Grid and Mesh objects can be created in 2D or 3D space, and 3D
first-order conservative regridding is fully supported.  Future plans for ESMPy
involve the incorporation of observational data streams.

Regridding, also called remapping or interpolation, is the process of changing
the grid underneath field data values while preserving the qualities of the
original data.  Different kinds of transformations are appropriate for
different problems. Regridding may be needed when communicating data between
Earth system model components such as land and atmosphere, or between
different data sets to support analysis or visualization.

Regridding can be broken into two stages. The first stage is generation of an
interpolation weight matrix that describes how points in the source grid
contribute to points in the destination grid. The second stage is the
multiplication of values on the source grid by the interpolation weight matrix
to produce the appropriate values on the destination grid.  ESMP provides
access to both stages through two separate interfaces.

There are many different interpolation methods, suitable for different
problems.  In ESMPy, the basic bilinear option is a two dimensional variant of
linear interpolation.  The higher order patch recovery is a second degree
polynomial regridding method, which uses a least squares algorithm to
calculate the polynomial.  The first-order conservative regridding is a
variant of a constant method which compares the proportions of overlapping
source and destination cells to determine appropriate weights.  All of these
methods can be broken down to a simple sparse matrix multiplication operation
between interpolation weights and data values.
'''

__name__ = "ESMF"
__description__ = "ESMF Python interface"
__author__ = "University Corporation for Atmospheric Research, \
              Massachusetts Institute of Technology, \
              Geophysical Fluid Dynamics Laboratory, \
              University of Michigan, \
              National Centers for Environmental Prediction, \
              Los Alamos National Laboratory, \
              Argonne National Laboratory, \
              NASA Goddard Space Flight Center"
__license__ = "University of Illinois-NCSA"
__release__ = "620_02b"

#### IMPORT LIBRARIES #########################################################

from api.manager import *
from api.grid import *
from api.mesh import *
from api.field import *
from api.regrid import *

# for testing
from util.decorators import expected_failure
[node, element] = [0, 1]
