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

* mpi4py - python bindings to MPI

================
Installation
================

Installation of ESMPy requires a pointer to a file named esmf.mk inside of an
ESMF installation.  This file resides in a directory which looks like:

<ESMF_INSTALL_DIR>/lib/lib<g|O>/<platform>/esmf.mk

If the ESMFMKFILE flag is set when building ESMPy then it will not need to be
referenced again.  If not, an environment variable of the same name must be set
with the path to the esmf.mk file EVERY time that a new shell is initiated.

The ESMPy installation can be built in a custom location using the
--prefix, --home, or --install-base flags to the install command.  If this
is done, then this location needs to be added to the PYTHONPATH environment
variable in the user's shell EVERY time that a new shell is initiated.  If a
customized build location is not specified, ESMPy will be built in the standard
Python package installation directory on that particular machine.

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

python setup.py test_all 

to also run the regrid_from_file tests.

NOTE: The regrid tests can take up a lot of memory and bandwidth.

Alternatively, if the nose package is available, the test can be run with:

nosetests

Individual tests can be run with nose using the following format:

nosetests <file>:<test>

e.g.  nosetests src/ESMF/test/unit_test.py:field_regrid_test

===========
Limitations
===========

ESMPy doesn't include many aspects of ESMF, including components, array
interfaces, time management, etc.  The limitations listed here are relative
to ESMF offline and integrated regridding capabilities.

* There is no FieldBundle class, only single Fields
* There is no support for tripole or multi-tile Grids

* ESMPy cannot use an ESMF installation that is built with external LAPACK support.

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

-------
Masking
-------

Masking is the process whereby parts of a grid can be marked to be
ignored during an operation, such as regridding.  Masking can be
used on a source grid to indicate that certain portions of the grid
should not be used to generate regridded data.  This is useful, for
example, if a portion of source grid contains unusable values.
Masking can also be used on a destination grid to indicate that the
portion of the field built on that part of the Grid should not
receive regridded data.  This is useful, for example, when part of
the grid isn't being used (e.g. the land portion of an ocean grid).

ESMPy currently supports masking for Fields built on
structured Grids and element masking for Fields built on
unstructured Meshes. The user may mask out points in the source
Field or destination Field or both. To do masking the user sets
mask information in the Grid or Mesh upon which the Fields passed into the
Regrid() call
are built. The 'srcMaskValues' and 'dstMaskValues' arguments to that
call can then be used to specify which values in that mask
information indicate that a location should be masked out. For
example, if 'dstMaskValues' is set to (/1,2/), then any location that
has a value of 1 or 2 in the mask information of the Grid or Mesh
upon which the destination Field is built will be masked out.

Masking behavior differs slightly between regridding methods. For
non-conservative regridding methods (e.g. bilinear or high-order
patch), masking is done on points. For these methods, masking a
destination point means that the point won't participate in
regridding (e.g. won't receive an interpolated value). For these methods,
masking a source point means that the entire source cell using
that point is masked out. In other words, if any corner point
making up a source cell is masked then the cell is masked. For
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
be mapped by setting the 'unmappedaction' argument to UnmappedAction.IGNORE.
The user also has the option to return
an error if unmapped destination points exist. This is the default behavior,
so the user can either not set the 'unmappedaction' argument or the user can set
it to UnmappedAction.ERROR. At this point ESMPy does not support
extrapolation to destination points outside the unmasked source Field.


============
Interface
============

-------
Classes
-------

============================ ==================================================
Class                        Description
============================ ==================================================
:class:`Manager`             A manager to initialize and finalize ESMF
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
===============================================  ==============================


=========
Tutorials
=========

The following tutorials will show you how to build all of the pieces
necessary to regrid data between Fields built on Grids.


-------------
Create a Grid
-------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/field_regridding_test.py
        :pyobject: create_grid


--------------
Create a Field
--------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/field_regridding_test.py
        :pyobject: create_field


-----------------------
Build an analytic Field
-----------------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/field_regridding_test.py
        :pyobject: build_analyticfield


--------------------
Run ESMPy regridding
--------------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/field_regridding_test.py
        :pyobject: run_regridding


------------
Compute mass
------------

  .. literalinclude:: ../src/ESMF/test/regrid_test/field_regridding_test.py
    :pyobject: compute_mass


---------------------------------------
Compare Field results to exact solution
---------------------------------------

    .. literalinclude:: ../src/ESMF/test/regrid_test/field_regridding_test.py
        :pyobject: compare_fields


----------------
Field regridding
----------------

.. automodule:: ../src/ESMF/test/regrid_test/field_regridding_test

The code for this tutorial can be downloaded `here <http://esmfcontrib.cvs.sourceforge.net/viewvc/esmfcontrib/python/ESMPy/tutorial/field_regridding.py?view=log>`_.

~~~~~~~~~~~~~~~
regrid_main()
~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/regrid_test/field_regridding_test.py
        :pyobject: regrid_main

Running this tutorial will yield the following results:

    ::

        Welcome to the Field regridding tutorial!
        PASS
          Total error = 1.66715782175
          Max error   = 0.0617374010089
          Min error   = 0.00345860933766
          Csrv error  = 4.62592314758e-16
          srcmass     = 23.0400304899
          dstmass     = 23.0400304899
        
        Thanks for using the Field regridding tutorial.


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



* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

