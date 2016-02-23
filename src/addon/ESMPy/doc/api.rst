==========
Python API
==========

-------
Classes
-------

ESMPy uses a Field object to represent data variables built on an
underlying spatial discretization, which is represented by a Grid, Mesh or LocStream.
Regridding between Fields is accomplished with the Regrid class.  All of these
classes are explained in more detail in the sections provided by the links in
the following table.

=======================================  ==============================================================================
Class                                    Description
=======================================  ==============================================================================
:class:`~ESMF.api.esmpymanager.Manager`  A manager class to initialize and finalize ESMF
:class:`~ESMF.api.field.Field`           A data field built on a Grid, Mesh, or LocStream
:class:`~ESMF.api.grid.Grid`             A class to represent a logically rectangular grid
:class:`~ESMF.api.mesh.Mesh`             A calss to represent an unstructured grid
:class:`~ESMF.api.locstream.LocStream`   A class to represent observational data as a collection of disconnected points
:class:`~ESMF.api.regrid.Regrid`         The regridding utility
=======================================  ==============================================================================


---------------
Named constants
---------------

ESMPy follows the ESMF convention of using "named constants" to represent the
available options for parameters that expect a variety of specific inputs.  The
following table lists the available named constants and provides links to pages
that further explain the available values.

=========================================================== ==============================
Named constants                                             Description
=========================================================== ==============================
:class:`CoordSys<ESMF.api.constants.CoordSys>`              Specify the coordinate system of a Grid
:class:`FileFormat<ESMF.api.constants.FileFormat>`          Specify the format of a data file
:class:`GridItem<ESMF.api.constants.GridItem>`              Specify a mask or area item on a Grid
:class:`LineType<ESMF.api.constants.LineType>`              Specify the type of line that connects two points on a sphere
:class:`LogKind<ESMF.api.constants.LogKind>`                Specify how much logging should be done
:class:`MeshElemType<ESMF.api.constants.MeshElemType>`      Specify the type of the Mesh elements
:class:`MeshLoc<ESMF.api.constants.MeshLoc>`                Specify a nodal or elemental Mesh
:class:`NormType<ESMF.api.constants.NormType>`              Specify the type of normalization to use for conservative regridding weights
:class:`PoleMethod<ESMF.api.constants.PoleMethod>`          Specify which type of artificial pole to construct on the source Grid for regridding
:class:`Region<ESMF.api.constants.Region>`                  Specify various regions in the data layout of
:class:`RegridMethod<ESMF.api.constants.RegridMethod>`      Specify which interpolation method to use regridding
:class:`StaggerLoc<ESMF.api.constants.StaggerLoc>`          Specify the position for data in a Grid cell
:class:`TypeKind<ESMF.api.constants.TypeKind>`              Specify the type and kind of data
:class:`UnmappedAction<ESMF.api.constants.UnmappedAction>`  Specify which action to take with respect to unmapped destination points
=========================================================== ==============================



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

This file format is used by the SCRIP :cite:`ref:SCRIP`, package, grid files that
work with that package should also work here.  SCRIP format files are
capable of storing either 2D logically rectangular grids or 2D
unstructured grids.  More information can be found in the ESMF reference
manual section on the `SCRIP Grid File Format <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node3.html#SECTION03024000000000000000>`_.

++++
ESMF
++++

ESMF has a custom unstructured grid file format for describing meshes.
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
of the grid cell in a SCRIP grid. Therefore, when the Mesh will be
part of a conservative regridding operation, the 'convert_to_dual'
flag must be set to True to properly generate coordinates at the the
cell corners.

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

The following sections describe the regridding methods that are available in ESMPy.

~~~~~~~~
Bilinear
~~~~~~~~

Bilinear interpolation (RegridMethod.BILINEAR) calculates the value for the destination point as a combination of
multiple linear
interpolations, one for each dimension of the Grid. Note that for ease of use, the term bilinear interpolation is used
for 3D interpolation in ESMF as well, although it should more properly be referred to as trilinear interpolation.

In 2D, ESMPy supports bilinear regridding between any combination of the
following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of polygons with any number of sides
- A set of disconnected points (LocStream) may be the destination of the regridding

In 3D, ESMPy supports bilinear regridding between any combination of the
following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of hexahedrons (e.g. cubes)
- A set of disconnected points (LocStream) may be the destination of the regridding

Restrictions:

- Cells which contain enough identical corners to collapse to a line or point are currently ignored
- Self-intersecting cells (e.g. a cell twisted into a bow tie) are not supported
- On a spherical grid, cells which contain an edge which extends more than half way around the sphere are not supported

To use the bilinear method the user must create their Fields on any stagger
location for Grids (e.g. StaggerLoc.CENTER) or any Mesh location (e.g. MeshLoc.NODE) for Meshes. For
either a Grid or a Mesh, the stagger location upon which the Field was built must contain
coordinates.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
Higher order patch recovery
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Patch (or higher-order) interpolation (RegridMethod.PATCH) is the ESMF version of a technique called
*patch recovery* commonly used in finite element modeling :cite:`PatchInterp1`, :cite:`PatchInterp2`.
It typically results in better approximations to values and derivatives when
compared to bilinear interpolation. Patch interpolation works by constructing multiple polynomial patches to represent
the data in a source cell. For 2D grids, these polynomials are currently 2nd degree 2D polynomials. One patch is
constructed for each corner of the source cell, and the patch is constructed by doing a least squares fit through the
data in the cells surrounding the corner. The interpolated value at the destination point is then a weighted average
of the values of the patches at that point. The patch method has a larger stencil than the bilinear, for this reason
the patch weight matrix can be correspondingly larger than the bilinear matrix (e.g. for a quadrilateral grid the
patch matrix is around 4x the size of the bilinear matrix). This can be an issue when performing a regrid operation
close to the memory limit on a machine.

In 2D, ESMPy supports patch regridding between any combination of the following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of polygons with any number of sides
- A set of disconnected points (LocStream) may be the destination of the regridding

Patch regridding is currently not supported in 3D.

Restrictions:

- Cells which contain enough identical corners to collapse to a line or point are currently ignored
- Self-intersecting cells (e.g. a cell twisted into a bow tie) are not supported
- On a spherical grid, cells which contain an edge which extends more than half way around the sphere are not supported

To use the patch method the user must create their Fields on any stagger
location for Grids (e.g. StaggerLoc.CENTER) or any Mesh location (e.g. MeshLoc.NODE) for Meshes. For
either a Grid or a Mesh, the stagger location upon which the Field was built must contain
coordinates.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nearest source to destination
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In nearest source to destination interpolation (RegridMethod.NEAREST_STOD) each destination point is mapped to the
closest source point. A given source point may map to multiple destination points, but no destination point will
receive input from more than one source point. If two points are equally close, then the point with the smallest
sequence index is arbitrarily used (i.e. the point which would have the smallest index in the weight matrix).

In 2D, ESMPy supports destination to source regridding between any combination of the
following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of polygons with any number of sides
- A set of disconnected points (LocStream)

In 3D, ESMPy supports nearest destination to source regridding between any combination of the
following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of hexahedrons (e.g. cubes) and tetrahedrals
- A set of disconnected points (LocStream)

Restrictions:

None

To use the nearest source to destination method the user must create their Fields on any stagger
location for Grids (e.g. StaggerLoc.CENTER) or any Mesh location (e.g. MeshLoc.NODE) for Meshes. For
either a Grid or a Mesh, the stagger location upon which the Field was built must contain
coordinates.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nearest destination to source
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In nearest destination to source interpolation (RegridMethod.NEAREST_DTOS) each source point is mapped
to the closest destination point. A given destination point may receive input from multiple source points, but no
source point will map to more than one destination point. If two points are equally close, then the point with the
smallest sequence index is arbitrarily used (i.e. the point which would have the smallest index in the weight matrix).
Note, that with this method the unmapped destination point detection currently doesn't work, so no error will be
returned even if there are destination points that don't map to any source point.

In 2D, ESMPy supports nearest source to destination regridding between any combination of the
following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of polygons with any number of sides
- A set of disconnected points (LocStream)

In 3D, ESMPy supports nearest source to destination regridding between any combination of the
following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of hexahedrons (e.g. cubes) and tetrahedrals
- A set of disconnected points (LocStream)

Restrictions:

None

To use the nearest destination to source method the user must create their Fields on any stagger
location for Grids (e.g. StaggerLoc.CENTER) or any Mesh location (e.g. MeshLoc.NODE) for Meshes. For
either a Grid or a Mesh, the stagger location upon which the Field was built must contain
coordinates.

~~~~~~~~~~~~~~~~~~~~~~~~
First-order conservative
~~~~~~~~~~~~~~~~~~~~~~~~

First-order conservative interpolation (RegridMethod.CONSERVE) :cite:`ConservativeOrder1` is also available
as a regridding method.
This method will typically have a larger local interpolation error than the previous two methods, but will do a
much better job of preserving the value of the integral of data between the source and destination grid.
In this method the value across each source cell is treated as a constant.
The weights for a particular destination cell are the area of intersection of each source cell with the destination
cell divided by the area of the destination cell.
For Cartesian grids, the area of a grid cell is the typical Cartesian area.
For grids on a sphere, cell areas are calculated by connecting the corner coordinates of each grid cell with
great circles. If the user doesn't specify cell areas in the involved Grids or Meshes, then the conservation will
hold for the areas as calculated by ESMF.  This means the following equation will hold::

    sum-over-all-source-cells(Vsi*Asi) = sum-over-all-destination-cells(Vdj*A'dj),

where V is the variable being regridded and A' is the area of a cell as calculated by ESMF.

The subscripts s and d refer to source and destination values, and the i and j are the source and destination
grid cell indices (flattening the arrays to 1 dimension).
If the user does specify the areas in the Grid or Mesh, then the conservation will be adjusted to work for the
areas provided by the user. This means the following equation will hold::

    sum-over-all-source-cells(Vsi*Asi) = sum-over-all-destination-cells(Vdj*Adj),

where A is the area of a cell as provided by the user.

The user should be aware that because of the conservation relationship between the source and destination fields,
the more the total source area differs from the total destination area the more the values of the source field
will differ from the corresponding values of the destination field, likely giving a higher interpolation error.
It is best to have the total source and destination areas the same (this will automatically be true if no user
areas are specified). For source and destination grids that only partially overlap, the overlapping regions of
the source and destination should be the same.

Note that for grids on a sphere the conservative interpolation assumes great circle edges to cells.
This means that the edges of a cell won't necessarily be the same as a straight line in latitude longitude.
For small edges, this difference will be small, but for long edges it could be significant.
This means if the user expects cell edges as straight lines in latitude longitude space, they should avoid using
one large cell with long edges to compute an average over a region (e.g. over an ocean basin).
The user should also avoid using cells that contain one edge that runs half way or more around the earth,
because the regrid weight calculation assumes the edge follows the shorter great circle path.
Also, there isn't a unique great circle edge defined between points on the exact opposite side of the earth
from one another (antipodal points). However, the user can work around both of these problemS by breaking the
long edge into two smaller edges by inserting an extra node, or by breaking the large target grid cells into
two or more smaller grid cells. This allows the application to resolve the ambiguity in edge direction.

It is important to note that by default (i.e. using destination area normalization) conservative regridding
doesn't normalize the interpolation weights by the destination fraction. This means that for a destination grid
which only partially overlaps the source grid the destination field that is output from the regrid operation
should be divided by the corresponding destination fraction to yield the true interpolated values for cells which
are only partially covered by the source grid. The fraction also needs to be included when computing the total
source and destination integrals. (To include the fraction in the conservative weights, the user can specify
the fraction area normalization type. This can be done by specifying normType=NormType.FRACAREA when creating
the Regrid object.)

For weights generated using destination area normalization (either by not specifying any normalization type or
by specifying normType=NormType.DSTAREA), if a destination field extends outside the unmasked source field,
then the values of the cells which extend partway outside the unmasked source field are decreased by the
fraction they extend outside. To correct these values, the destination field (dst_field) resulting from the
Regrid call can be divided by the destination fraction dst_frac. The following pseudocode demonstrates how to do this::


    for each destination element i
       if (dst_frac(i) not equal to 0.0) then
          dst_field(i)=dst_field(i)/dst_frac(i)
       end if
    end for

For weights generated using destination area normalization (either by not specifying any normalization type or
by specifying normType=NormType.DSTAREA), the following pseudo-code shows how to compute the total destination
integral (dst_total) given the destination field values (dst_field),
the destination area (dst_area), and the destination fraction (dst_frac).
As shown in the previous paragraph, it also shows how to adjust the destination field (dst_field)
by the fraction (dst_frac)::


    dst_total=0.0
    for each destination element i
       if (dst_frac(i) not equal to 0.0) then
          dst_total=dst_total+dst_field(i)*dst_area(i)
          dst_field(i)=dst_field(i)/dst_frac(i)
          ! If mass computed here after dst_field adjust, would need to be:
          ! dst_total=dst_total+dst_field(i)*dst_area(i)*dst_frac(i)
       end if
    end for

For weights generated using fraction area normalization (by specifying normType=NormType.FRACAREA),
no adjustment of the destination field is necessary. The following pseudo-code shows how to compute the total
destination integral (dst_total) given the destination field values (dst_field), the destination area (dst_area),
and the destination fraction (dst_frac)::

    dst_total=0.0
    for each destination element i
         dst_total=dst_total+dst_field(i)*dst_area(i)*dst_frac(i)
    end for

For both normalization types, the following pseudo-code shows how to compute the total source integral (src_total)
given the source field values (src_field), the source area (src_area), and the source fraction (src_frac)::

    src_total=0.0
    for each source element i
       src_total=src_total+src_field(i)*src_area(i)*src_frac(i)
    end for


In 2D, ESMPy supports first-order conservative regridding between any
combination of the following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of polygons with any number of sides

In 3D, ESMPy supports first-order conservative regridding between any
combination of the following:

- Structured Grids composed of a single logically rectangular patch
- Unstructured Meshes composed of hexahedrons (e.g. cubes) and tetrahedrals.

Restrictions:

- Cells which contain enough identical corners to collapse to a line or point are currently ignored
- Self-intersecting cells (e.g. a cell twisted into a bow tie) are not supported
- On a spherical grid, cells which contain an edge which extends more than half way around the sphere are not supported

To use the first-order conservative method the user must create their
Fields on the center stagger location (StaggerLoc.CENTER in 2D or
StaggerLoc.CENTER_VCENTER in 3D) for Grids or the element location
(MeshLoc.ELEMENT) for Meshes. For Grids, the corner stagger location
(StaggerLoc.CORNER in 2D or StaggerLoc.CORNER_VFACE in 3D) must
contain coordinates describing the outer perimeter of the Grid cells.

-------
Masking
-------

Masking is the process whereby parts of a Grid, Mesh or LocStream can be marked to be ignored
during an operation, such as when they are used in regridding. Masking can be used on a Field
created from a regridding source to indicate that certain portions should not be used to generate
regridded data. This is useful, for example, if a portion of the source contains unusable values.
Masking can also be used on a Field created from a regridding destination to indicate that a certain
portion should not receive regridded data. This is useful, for example, when part of the destination
isn't being used (e.g. the land portion of an ocean grid).

The user may mask out points in the source Field or destination Field or both. To do masking the user
sets mask information in the Grid, Mesh, or LocStream upon
which the Fields passed into the Regrid call are built. The src_mask_values and
dst_mask_values arguments to that call can then be used to specify which values in that mask information
indicate that a location should be masked out. For example, if dstMaskValues is set to (/1,2/), then any
location that has a value of 1 or 2 in the mask information of the Grid, Mesh or LocStream upon which
the destination Field is built will be masked out.

Masking behavior differs slightly between regridding methods. For non-conservative regridding methods
(e.g. bilinear or high-order patch), masking is done on points. For these methods, masking a destination
point means that that point won't participate in regridding (e.g. won't be interpolated to). For these
methods, masking a source point means that the entire source cell using that point is masked out.
In other words, if any corner point making up a source cell is masked then the cell is masked.
For conservative regridding methods (e.g. first-order conservative) masking is done on cells.
Masking a destination cell means that the cell won't participate in regridding (e.g. won't be
interpolated to). Similarly, masking a source cell means that the cell won't participate in regridding
(e.g. won't be interpolated from). For any type of interpolation method (conservative or non-conservative)
the masking is set on the location upon which the Fields passed into the regridding call are built.
For example, if Fields built on StaggerLoc.CENTER are passed into the ESMF_FieldRegridStore()
call then the masking should also be set on StaggerLoc.CENTER.

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

--------------------------
Numpy Slicing and Indexing
--------------------------

Numpy arrays are used to represent Grid and Mesh coordinates and Field data,
among other things.  Standard numpy conventions for array indexing
and slicing can be expected.  There are some exceptions when it comes to fancy
indexing, index arrays, and multi-dimensional slicing.  Significant effort has
been put into raising exceptions where inappropriate indexing or slicing
operations are attempted.

It is very important to remember that all indexing
and slicing operations apply ONLY to the ESMPy level objects, and these operations
do not propagate down to the lower-level Fortran- and C-based representations
of the ESMF objects.  One example of where this could come up is when passing
a Field slice into regridding.  The entire original Field will still be run
through the ESMF regridding engine, and only the appropriate portion of
the Field slice will be updated with the regridded values.