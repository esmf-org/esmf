[![ESMF Doc Build](https://github.com/esmf-org/esmf/actions/workflows/build-esmf-docs.yml/badge.svg)](https://github.com/esmf-org/esmf/actions/workflows/build-esmf-docs.yml)
[![ESMPy Doc Build](https://github.com/esmf-org/esmf/actions/workflows/build-esmpy-docs.yml/badge.svg)](https://github.com/esmf-org/esmf/actions/workflows/build-esmpy-docs.yml)

# Earth System Modeling Framework (ESMF)

>Copyright (c) 2002-2022 University Corporation for Atmospheric Research, Massachusetts Institute of Technology, Geophysical Fluid Dynamics Laboratory, University of Michigan, National Centers for Environmental Prediction, Los Alamos National Laboratory, Argonne National Laboratory, NASA Goddard Space Flight Center. All rights reserved.

Hello and welcome to ESMF.

 * The [ESMF User's Guide](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/) contains information on building and installing ESMF.
 * The [ESMF Reference Manual](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc/) contains information on the architecture of ESMF,
   example code, and details of the API (Application Programming Interface).
 * Please contact <esmf_support@ucar.edu> with any questions or problems.

## Downloading ESMF

The ESMF library source code is available for download at GitHub:
 * https://github.com/esmf-org/esmf/releases

## Building ESMF

To build ESMF from source:
 * Consult the [Building ESMF](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/node10.html) section of the [ESMF User's Guide](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/).
 * For testing ESMF, see [Testing makefile targets](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/node11.html).
 
## Pre-built ESMF
 
Pre-built binaries for ESMF and ESMPy are available through a number of channels:
 * [Docker Hub](https://hub.docker.com/): Under [ESMF Organization](https://hub.docker.com/u/esmf). To install locally, run:
   ```
   docker run -it --rm esmf/esmf-build-release:latest
   ```
   Replace `latest` in the above command with a valid version, like `8.4.0`, in order to access a specific ESMF version.
   
 * [Anaconda Conda-Forge](https://anaconda.org/conda-forge/): Under [conda-forge/esmpy](https://anaconda.org/conda-forge/esmpy). To install locally (_note Windows is not supported_), run:
   ```
   conda create -n esmpy -c conda-forge esmpy
   ```

## Running ESMF Command Line Tools

 * To use the ESMF command line tools follow the instructions located in [Using Bundled ESMF Command Line Tools](https://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/node9.html).

## Linking to ESMF

 * To build your application against an ESMF installation follow the instructions located in [Compiling and Linking User Code against an ESMF Installation](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/node7.html).

## Add-On Packages

Several add-on packages are included with ESMF under [`./src/addon`](https://github.com/esmf-org/esmf/tree/master/src/addon):  
 * [ESMPy](src/addon/esmpy) - Python interface to ESMF.
 * [ESMX](src/addon/ESMX) - A layer providing the Earth System Model eXecutable.
   The ESMX layer is built on top of ESMF and NUOPC.
 * [NUOPC](src/addon/NUOPC) - Interoperability layer developed under the National Unified Operational Prediction Capability (NUOPC) program.

See each addon package for a specific README file.

## More Help

Information about the ESMF project can be found at the ESMF web site:
 * https://www.earthsystemmodeling.org/

Software documentation for the last release:
 * http://earthsystemmodeling.org/doc/

Software documentation for all releases:
 * http://earthsystemmodeling.org/static/releases.html
 
 Support:
 * http://earthsystemmodeling.org/support/
