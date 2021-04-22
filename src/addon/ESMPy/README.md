# Earth System Modeling Framework Python Interface (ESMPy)

> Copyright 2002-2021, University Corporation for Atmospheric Research, Massachusetts Institute of Technology, Geophysical Fluid Dynamics Laboratory, University of Michigan, National Centers for Environmental Prediction, Los Alamos National Laboratory, Argonne National Laboratory, NASA Goddard Space Flight Center. Licensed under the University of Illinois-NCSA License.

 * [ESMPy Documentation](http://earthsystemmodeling.org/esmpy_doc/release/latest/html/)
   * [Installation](http://earthsystemmodeling.org/esmpy_doc/release/latest/html/install.html)
   * [Tutorials](http://earthsystemmodeling.org/esmpy_doc/release/latest/html/examples.html)
 * Please contact <esmf_support@ucar.edu> with any questions or problems.

## Software Overview

ESMPy is a Python interface to the Earth System Modeling Framework (ESMF) regridding utility. ESMF is software for building and coupling weather, climate, and related models. ESMF has a robust, parallel and scalable remapping package, used to generate remapping weights. It can handle a wide variety of grids and options: logically rectangular grids and unstructured meshes; regional or global grids; 2D or 3D; and pole and masking options. ESMF also has capabilities to read grid information from NetCDF files in a variety of formats, including the Climate and Forecast (CF) V1.6 (a.k.a. GridSpec), UGRID, ESMF Unstructured and SCRIP conventions..

ESMPy provides a Grid to represent single-tile logically rectangular coordinate data, a Mesh for unstructured coordinates, and a LocStream for collections of unconnected points like observational data streams. ESMPy supports bilinear, nearest neighbor, higher order patch recovery, first-order conservative and second-order conservative regridding. There is also an option to ignore unmapped destination points, mask out points on either the source or destination, choose straight line or great circle paths when using spherical coordinates, create artificial poles and bipoles, and extrapolate data to points outside of the destination domain. Regridding on the sphere takes place in 3D Cartesian space, so the pole problem is not an issue as it commonly is with many Earth system grid remapping softwares.

## Regridding Overview

Regridding, also called remapping or interpolation, is the process of changing the grid underneath field data values while preserving the qualities of the original data. Different kinds of transformations are appropriate for different problems. Regridding may be needed when communicating data between Earth system modeling components such as land and atmosphere, or between different data sets to support analysis or visualization.

Regridding can be broken into two stages. The first stage is generation of an interpolation weight matrix that describes how points in the source grid contribute to points in the destination grid. The second stage is the multiplication of values on the source grid by the interpolation weight matrix to produce the appropriate values on the destination grid. ESMPy provides access to both stages through two separate interfaces.

There are many different interpolation methods, suitable for different problems. In ESMPy, the basic bilinear option is a two dimensional variant of linear interpolation. The higher order patch recovery is a second degree polynomial regridding method, which uses a least squares algorithm to calculate the polynomial. This method gives better derivatives in the resulting destination field than the bilinear method. There are two nearest-neighbor methods which map the points from one grid to the nearest corresponding point on the other grid, from either source to destination or vice versa. These are useful for extrapolation or categorical fields. 

The first-order conservative regridding is a method designed to preserve the integral of the field across the interpolation from source to destination. It uses the proportion of the area of the overlapping source and destination cells to determine appropriate weights. The second-order conservative method also preserves the integral, but uses the source gradient to give a smoother result than the first-order conservative. All of these methods can be broken down to a simple sparse matrix multiplication operation between interpolation weights and data values.
