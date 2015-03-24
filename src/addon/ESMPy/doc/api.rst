==========
Python API
==========

-------
Classes
-------

ESMPy uses a Field object to represent data variables built on an
underlying spatial discretization, which is represented by a Grid or Mesh.
Regridding between Fields is accomplished with the Regrid class.  All of these
classes are explained in more detail in the sections provided by the links in
the following table.

=======================================  ==================================================
Class                                    Description
=======================================  ==================================================
:class:`~ESMF.api.esmpymanager.Manager`  A manager class to initialize and finalize ESMF
:class:`~ESMF.api.field.Field`           A data field built on a Grid or Mesh
:class:`~ESMF.api.grid.Grid`             A structured grid for coordinate representation
:class:`~ESMF.api.mesh.Mesh`             An unstructured grid for coordinate representation
:class:`~ESMF.api.regrid.Regrid`         The regridding utility
=======================================  ==================================================


---------------
Named constants
---------------

ESMPy follows the ESMF convention of using "named constants" to represent the
available options for parameters that expect a variety of specific inputs.  The
following table lists the available named constants and provides links to pages
that further explain the available values.

===============================================  ==============================
Named constants                                  Description
===============================================  ==============================
:class:`~ESMF.api.constants._CoordSys`           Specify the coordinate system of a Grid
:class:`~ESMF.api.constants._DecompFlag`         Specify how DistGrid elements are decomposed over DEs
:class:`~ESMF.api.constants._FileFormat`         Specify the format of a data file
:class:`~ESMF.api.constants._GridItem`           Specify a mask or area item on a Grid
:class:`~ESMF.api.constants._LogKind`            Specify how much logging should be done
:class:`~ESMF.api.constants._MeshElemType`       Specify the type of the Mesh elements
:class:`~ESMF.api.constants._MeshLoc`            Specify a nodal or elemental Mesh
:class:`~ESMF.api.constants._Region`             Specify various regions in the data layout of
:class:`~ESMF.api.constants._RegridMethod`       Specify which interpolation method to use regridding
:class:`~ESMF.api.constants._StaggerLoc`         Specify the position for data in a Grid cell
:class:`~ESMF.api.constants._TypeKind`           Specify the type and kind of data
:class:`~ESMF.api.constants._UnmappedAction`     Specify which action to take with respect to unmapped destination points
:class:`~ESMF.api.constants._PoleMethod`         Specify  which type of artificial pole to construct on the source Grid for regridding
===============================================  ==============================



-------------------------------
Create a Grid or Mesh From File
-------------------------------

~~~~~~~~~~~~
File formats
~~~~~~~~~~~~

ESMPy can create Grid or Mesh objects from NetCDF files in a variety
of formats.  A Mesh can be created from files in SCRIP, ESMF, and UGRID
formats.  Grid files can be in SCRIP and GRIDSPEC format.

+++++
SCRIP
+++++

This file format is used by the SCRIP [4] package, grid files that
work with that package should also work here.  SCRIP format files are
capable of storing either 2D logically rectangular grids or 2D
unstructured grids.  More information can be found in the ESMF reference
manual section on the `SCRIP Grid File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03024000000000000000>`_.

++++
ESMF
++++

ESMF has custom unstructured grid file format for describing meshes.
This format is more compatible than the SCRIP format with the methods
used to create a Mesh object, so less conversion needs to be done to
create a Mesh. The ESMF format is thus more efficient than SCRIP when
used with ESMPy.  More information can be found in the ESMF reference
manual section on the `ESMF Unstructured Grid File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03025000000000000000>`_.

++++++++
GRIDSPEC
++++++++

GRIDSPEC is an extension to the Climate and Forecast (CF) metadata
conventions for the representation of gridded data for Earth System
Models.  ESMPy supports NetCDF files that follow the CF GRIDSPEC
convention to support logically rectangular lat/lon grids.  More
information can be found in the ESMF reference manual section on the
`CF Convention GRIDSPEC File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03026000000000000000>`_.

+++++
UGRID
+++++

UGRID is an extension to the CF metadata
conventions for the unstructured grid data model.  ESMPy support
NetCDF files that follow the CF UGRID convention for unstructured grids.
More information can be found in the ESMF reference manual section on
the `CF Convention UGRID File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03027000000000000000>`_.

~~~~~~~~~~~~~~~~
Meshes From File
~~~~~~~~~~~~~~~~

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
'varname'.  These argument are only valid for UGRID formatted files.

~~~~~~~~~~~~~~~
Grids From File
~~~~~~~~~~~~~~~

A number of optional boolean arguments are also supported to create a
structured Grid from a file.  These include 'is_sphere' to indicate whether
the grid is spherical or regional, 'add_corner_stagger' to add the corner
stagger information to the Grid for conservative regridding, and
'add_user_area' to specify whether to read in the cell area from the
NetCDF file or to calculate them.  Also, for GRIDSPEC formmated files
there is the 'add_mask' optional argument
to add a mask held by the NetCDF variable indicated in optional
argument, 'varname', and the 'coord_names' argument to specify the longitude
and latitude variable names in GRIDSPEC file containing multiple sets of
coordinates.


----------
Regridding
----------

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
the mask in the 'mask_values' variable of the Field constructor.  However,
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


