.. ESMPy documentation master file, created by
   sphinx-quickstart on Tue Dec  6 12:05:22 2011.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. toctree::

#####
ESMPy
#####

Welcome to ESMPy - The ESMF Python Interface!

The `ESMPy home page <http://www.earthsystemcog.org/projects/esmpy/>`_ has all of the
latest information on the ESMPy project including release notes, known
bugs, supported platforms, and download information.

Please see the `ESMF home page <http://www.earthsystemmodeling.org>`_
for more information on ESMF in general.

`Fast Parallel Grid Remapping for Unstructured and Structured Grids
<http://www.earthsystemmodeling.org/presentations/pres_1109_see_regridding.ppt>`_
gives a nice overview of the ESMF remapping functionality.

The `ESMF Regridding Status
<http://www.earthsystemmodeling.org/esmf_releases/last/regridding_status.html>`_
page gives a good overview of the functionality that is available through
various interfaces to ESMF regridding.

The `ESMF_RegridWeightGen application
<http://www.earthsystemmodeling.org/esmf_releases/last/ESMF_refdoc/node3.html#SECTION03020000000000000000>`_
is a command-line version of the functionality that is available through ESMPy.

Please contact esmf_support@list.woc.noaa.gov with any questions or problems.

============
Introduction
============

.. automodule:: ESMF

================
Getting the code
================

The ESMF User's Guide contains information on building and installing ESMF.
The ESMF Reference Manual contains information on the architecture of ESMF,
example code, and details of the API (Application Programming Interface).

Instructions on how to download the ESMPy code can be found at the `ESMPy Download page
<http://www.earthsystemcog.org/projects/esmpy/releases>`_.

------------
Requirements
------------

The following packages are *required* to work with ESMPy:

* `ESMF installation <http://www.earthsystemmodeling.org/esmf_releases/last/ESMF_usrdoc>`_
* `python <http://python.org/>`_
* `numpy <http://www.numpy.org/>`_
* ctypes (included with numpy)

The following packages are *optional*:

* mpi4py - python bindings to MPI, needed to run the parallel regridding tests
* ESMF installation with NetCDF - required to create grids and meshes from file
  - NetCDF must be built as a shared library for ESMPy installation to succeed

============
Installation
============

Installation of ESMPy requires a pointer to a file named esmf.mk inside of an
ESMF installation.  This file resides in a directory which looks like:

<ESMF_INSTALL_DIR>/lib/lib<g<or>O>/<platform>/esmf.mk

If the ESMFMKFILE flag is set when building ESMPy then it will not need to be
referenced again.  If not, an environment variable of the same name must be set
with the path to the esmf.mk file EVERY time that a new shell is initiated.

The ESMPy build can be installed in a custom location using the
--prefix, --home, or --install-base flags to the install command.  If this
is done, then this location needs to be added to the PYTHONPATH environment
variable in the user's shell EVERY time that a new shell is initiated.  If a
customized install location is not specified, ESMPy will be installed in the 
standard Python package installation directory on that particular machine.

Note: The ESMPy build does not have to be installed to be used.  The 
PYTHONPATH environment variable can simply be pointed to the directory 
containing the ESMF module (esmfcontrib-ESMPy/src from a default git clone) 
after the build command.

As usual, any command followed by --help should print out some information
on what options are available.

An installation of ESMPy in the default location for Python packages can be done 
with the following command issued from the top level ESMPy directory:

- default Python package installation:

    python setup.py build --ESMFMKFILE=<DIR_TO_esmf.mk> install

- custom install location:

    python setup.py build --ESMFMKFILE=<DIR_TO_esmf.mk>

    python setup.py install --prefix=<custom_install_location>

    setenv PYTHONPATH <custom_install_location>/lib/*/site_packages

Please contact esmf_support@list.woc.noaa.gov with any questions or problems.


---
Use
---

To use this package in an external program, import it with:

    import ESMF

----------
Validation
----------

The setup.py file can be used to run all of the ESMPy tests, like this:

    python setup.py test

or:

    python setup.py test_regrid

    python setup.py test_regrid_from_file

    python setup.py test_all

    python setup.py test_regrid_parallel

NOTE: The regrid_from_file tests can take up a lot of memory and bandwidth.  
The "test_regrid_from_file_dryrun" command will simply download the test 
files without actually running them (allowing the stress on the machine to
be applied to bandwidth first, and then memory).

Alternatively, if the nose package is available, the tests can be run with:

    nosetests

Individual tests can be run with nose using the following format:

    nosetests <file>:<test>

e.g.  

    nosetests src/ESMF/test/unit_test.py:field_regrid_test

===========
Limitations
===========

ESMPy doesn't include many aspects of ESMF, including components, array
interfaces, time management, etc.  The limitations listed here are relative
to ESMF offline and integrated regridding capabilities.

- There is no FieldBundle class, only single Fields
- There is no support for tripole or multi-tile Grids
- ESMPy cannot use an ESMF installation that is built with external LAPACK 
  support.

Testing related:

- Only tested with gfortran on Darwin and Linux platforms


================
Field regridding
================

------------------
Regridding methods
------------------

The following three sections describe the regridding methods that are
available in ESMPy.

~~~~~~~~
Bilinear
~~~~~~~~

In 2D, ESMPy supports bilinear regridding between any combination of the
following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of any combination of triangles and
  quadrilaterals (e.g. rectangles)

In 3D, ESMPy supports bilinear regridding between any combination of the
following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of hexahedrons (e.g. cubes)

To use the bilinear method the user may created their Fields on any stagger
location for Grids or the node location (MeshLoc.NODE) for Meshes. For
Grids, the stagger location upon which the Field was built must contain
coordinates.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
Higher order patch recovery
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In 2D, ESMPy supports patch regridding between any combination of the following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of any combination of triangles and
  quadrilaterals (e.g. rectangles)

Patch regridding is currently not supported in 3D.

To use the patch method the user may created their Fields on any stagger
location for Grids or the node location (MeshLoc.NODE) for Meshes. For
Grids, the stagger location upon which the Field was built must contain
coordinates.

See references [1] and [2] for more information.

~~~~~~~~~~~~~~~~~~~~~~~~
First-order conservative
~~~~~~~~~~~~~~~~~~~~~~~~

In 2D, ESMPy supports first-order conservative regridding between any
combination of the following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of any combination of triangles and
  quadrilaterals (e.g. rectangles)

In 3D, ESMPy supports first-order conservative regridding between any
combination of the following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of hexahedrons (e.g. cubes) and tetrahedras.

To use the first-order conservative method the user must have created their
Fields on the center stagger location (StaggerLoc.CENTER in 2D or
StaggerLoc.CENTER_VCENTER in 3D) for Grids or the element location
(MeshLoc.ELEMENT) for Meshes. For Grids, the corner stagger location
(StaggerLoc.CORNER in 2D or StaggerLoc.CORNER_VFACE in 3D) must
contain coordinates describing the outer perimeter of the Grid cells.

See reference [3] for more information.


=================================
File Formats for Grids and Meshes
=================================

ESMPy supports Grids and Meshes created from a variety of NetCDF file
formats: SCRIP, ESMF, GRIDSPEC, and UGRID.  Each of these formats is
briefly described below.

SCRIP - the file format used by the SCRIP [4] package, grid files that 
work with that package should also work here.  SCRIP format files are 
capable of storing either 2D logically rectangular grids or 2D 
unstructured grids.  More information can be found in the ESMF reference
manual section on the `SCRIP Grid File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03024000000000000000>`_.

ESMF - a custom unstructured grid file format for describing meshes. 
This format is more compatible than the SCRIP format with the methods 
used to create a Mesh object, so less conversion needs to be done to 
create a Mesh. The ESMF format is thus more efficient than SCRIP when 
used with ESMPy.  More information can be found in the ESMF reference
manual section on the `ESMF Unstructured Grid File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03025000000000000000>`_.

GRIDSPEC - an extension to the Climate and Forecast (CF) metadata 
conventions for the representation of gridded data for Earth System 
Models.  ESMPy supports NetCDF files that follow the CF GRIDSPEC 
convention to support logically rectangular lat/lon grids.  More 
information can be found in the ESMF reference manual section on the 
`CF Convention GRIDSPEC File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03026000000000000000>`_.

UGRID - an extension to the CF metadata 
conventions for the unstructured grid data model.  ESMPy support 
NetCDF files that follow the CF UGRID convention for unstructured grids.
More information can be found in the ESMF reference manual section on 
the `CF Convention UGRID File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03027000000000000000>`_.


===============================
Create a Grid or Mesh From File
===============================

ESMPy can create Grid or Mesh objects from NetCDF files in a variety
of formats.  A Mesh can be created from files in SCRIP, ESMF, and UGRID
formats.  Grid files can be in SCRIP and GRIDSPEC format.

When creating a Mesh from a SCRIP format file, there are a number of
options to control the output Mesh. The data is located at the center
of the grid cell in a SCRIP grid; whereas the data is located at the
corner of a cell in an ESMF Mesh object. Therefore, we create a Mesh
object by default by constructing a "dual" mesh using the coordinates
in the file. If the user wishes to not construct the dual mesh, the
optional argument 'convert_to_dual' may be used to control this
behavior. When 'convert_to_dual' is set to False, the Mesh constructed
from the file will not be the dual. This is necessary when the Mesh is
part of a conservative regridding operation, so the 
weights are properly generated for the cell centers in the file.

A Mesh may also be created with boolean flags to specify whether or not to
add an area property to the Mesh 'add_user_area', or to add a mask
'add_mask' held by the NetCDF variable indicated in the optional argument, 
'varname'.

A number of optional boolean arguments are also supported to create a
structured Grid from a file.  These include 'is_sphere' to indicate whether
the grid is spherical or regional, 'add_corner_stagger' to add the corner
stagger information to the Grid for conservative regridding,
'add_user_area' to specify whether to read in the cell area from the
NetCDF file or to calculate them, and 'add_mask'
to add a mask held by the NetCDF variable indicated in optional
argument, 'varname'.   

-------
Masking
-------

Masking is the process whereby parts of a grid can be marked to be
ignored during an operation, such as regridding.  Masking can be
used on a source grid to indicate that certain portions of the grid
should not be used to generate regridded data.  This is useful, for
example, if a portion of a source grid contains unusable values.
Masking can also be used on a destination grid to indicate that the
portion of the field built on that part of the grid should not
receive regridded data.  This is useful, for example, when part of
the grid isn't being used (e.g. the land portion of an ocean grid).

ESMPy currently supports masking for Fields built on structured 
Grids and element masking for Fields built on unstructured Meshes.  
A Grid mask is initialized by setting mask values in the
Numpy Array returned from the Grid.get_item() call using the 'item'
variable.  A Mesh mask is initialized by passing mask values into
the Mesh.add_elements() call using the 'element_mask' variable.  The 
Field mask can then be setup by indicating the values to use for
the mask in the 'mask_vals' variable of the Field constructor.  However,
the Field mask does not need to be setup to mask values in the
regridding operation.  Regrid masking is handled by passing the
mask values into the 'src_mask_values' or 'dst_mask_values' 
variables of the Regrid constructor.  For example, if 
'dst_mask_values' is set to (/1,2/), then any location
in the Grid or Mesh that has a value of 1 or 2 will be masked.

Masking behavior differs slightly between regridding methods. For
non-conservative regridding methods (e.g. bilinear or high-order
patch), masking is done on points. For these methods, masking a
destination point means that the point won't participate in
regridding (e.g. won't receive an interpolated value). For these methods,
masking a source point means that the entire source cell using
that point is masked out. In other words, if any corner point
making up a source cell is masked then the whole cell is masked. For
conservative regridding methods (e.g. first-order conservative)
masking is done on cells. Masking a destination cell means that
the cell won't participate in regridding (e.g. won't receive an
interpolated value). Similarly, masking a source cell means that the
cell won't participate in regridding (e.g. won't contribute to
interpolation).  For any type of interpolation method (conservative or
non-conservative) the masking is set on the location upon
which the Fields passed into the regridding call are built.
For example, if Fields built on StaggerLoc.CENTER are
passed into the Regrid() call then the masking
should also be set in StaggerLoc.CENTER.

---------------------
Spherical coordinates
---------------------

In the case that the Grid is on a sphere (coord_sys=CoordSys.SPH_DEG or
CoordSys.SPH_RAD) then the coordinates given in the Grid are interpreted
as latitude and longitude values. The coordinates can either be in degrees or
radians as indicated by the 'coord_sys' flag set during Grid creation. As is
true with many global models, this application currently assumes the latitude
and longitude refer to positions on a perfect sphere, as opposed to a more
complex and accurate representation of the earth's true shape such as would be
used in a GIS system.

---------------
Unmapped points
---------------

If a destination point cannot be mapped to a location in the source grid, the
user has two options. The user may ignore those destination points that cannot
be mapped by setting the 'unmapped_action' argument to UnmappedAction.IGNORE.
The user also has the option to return
an error if unmapped destination points exist. This is the default behavior,
so the user can either not set the 'unmapped_action' argument or the user can set
it to UnmappedAction.ERROR. At this point ESMPy does not support
extrapolation to destination points outside the unmasked source Field.


=========
Interface
=========

-------
Classes
-------

============================ ==================================================
Class                        Description
============================ ==================================================
:class:`Manager`             A manager class to initialize and finalize ESMF
:class:`Field`               A data field built on a Grid or Mesh
:class:`Grid`                A structured grid for coordinate representation
:class:`Mesh`                An unstructured grid for coordinate representation
:class:`Regrid`              The regridding utility
============================ ==================================================

---------------
Named constants
---------------

===============================================  ==============================
Named constants                                  Description
===============================================  ==============================
:download:`CoordSys <CoordSys.rst>`              Specify the coordinate system of a Grid
:download:`DecompFlag <DecompFlag.rst>`          Specify how DistGrid elements are decomposed over DEs
:download:`FileFormat <FileFormat.rst>`          Specify the format of a data file
:download:`GridItem <GridItem.rst>`              Specify a mask or area item on a Grid
:download:`LogKind <LogKind.rst>`                Specify how much logging should be done
:download:`MeshElemType <MeshElemType.rst>`      Specify the type of the Mesh elements
:download:`MeshLoc <MeshLoc.rst>`                Specify a nodal or elemental Mesh
:download:`Region <Region.rst>`                  Specify various regions in the data layout of
:download:`RegridMethod <RegridMethod.rst>`      Specify which interpolation method to use regridding              
:download:`StaggerLoc <StaggerLoc.rst>`          Specify the position for data in a Grid cell     
:download:`TypeKind <TypeKind.rst>`              Specify the type and kind of data
:download:`UnmappedAction <UnmappedAction.rst>`  Specify which action to take with respect to unmapped destination points
:download:`PoleMethod <PoleMethod.rst>`          Specify  which type of artificial pole to construct on the source Grid for regridding
===============================================  ==============================


=========
Tutorials
=========

The following tutorials will show you how to build all of the pieces
necessary to regrid data between Fields built on Grids.


----------------
Create a 2D Grid
----------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/grid_regridding_utilities.py
        :pyobject: grid_create

----------------
Create a 3D Grid
----------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/grid_regridding_utilities.py
        :pyobject: grid_create_3d

----------------------
Create a periodic Grid
----------------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/grid_regridding_utilities.py
        :pyobject: grid_create_periodic

-----------------------
Create a Grid From File
-----------------------

    .. literalinclude:: /../src/ESMF/test/unit_test.py
        :pyobject: grid_create_from_file

-----------------------
Create a 5 element Mesh
-----------------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/mesh_regridding_utilities.py
        :pyobject: mesh_create_5

-----------------------
Create a Mesh From File
-----------------------

    .. literalinclude:: /../src/ESMF/test/unit_test.py
        :pyobject: mesh_create_from_file

--------------
Create a Field
--------------

    .. literalinclude:: ../src/ESMF/test/unit_test.py
        :pyobject: create_field


----------------------------
Initialize an analytic Field
----------------------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/grid_regridding_utilities.py
        :pyobject: initialize_field_grid_periodic


--------------------
Run ESMPy regridding
--------------------

    .. literalinclude:: ../src/ESMF/test/unit_test.py
        :pyobject: run_regridding


------------------
Compute Field mass
------------------

  .. literalinclude:: ../src/ESMF/test/regrid_test/grid_regridding_utilities.py
    :pyobject: compute_mass_grid


.. toctree::
    :maxdepth: 2

==========
References
==========

[1] Khoei S.A. Gharehbaghi A, R.
The superconvergent patch recovery technique and data transfer operators in 3d
plasticity problems.
Finite Elements in Analysis and Design, 43(8), 2007.

[2] K.C. Hung H. Gu, Z. Zong.
A modified superconvergent patch recovery method and its application to large
deformation problems.
Finite Elements in Analysis and Design, 40(5-6), 2004.

[3] D. Ramshaw.
Conservative rezoning algorithm for generalized two-dimensional meshes.
Journal of Computational Physics, 59, 1985.

[4] Jones, P.W. 
SCRIP: A Spherical Coordinate Remapping and Interpolation Package. 
http://www.acl.lanl.gov/climate/software/SCRIP/. 
Los Alamos National Laboratory Software Release LACC 98-45.

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

