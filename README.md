[![ESMF](https://circleci.com/gh/esmf-org/esmf.svg?style=svg)](http://www.earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc/)  

# Earth System Modeling Framework (ESMF)  

>Copyright (c) 2002-2021 University Corporation for Atmospheric Research, Massachusetts Institute of Technology, Geophysical Fluid Dynamics Laboratory, University of Michigan, National Centers for Environmental Prediction, Los Alamos National Laboratory, Argonne National Laboratory, NASA Goddard Space Flight Center. All rights reserved.

Hello and welcome to ESMF.

 * The [ESMF User's Guide](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/) contains information on building and installing ESMF.
 * The [ESMF Reference Manual](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc/) contains information on the architecture of ESMF,
   example code, and details of the API (Application Programming Interface).
 * Please contact <esmf_support@ucar.edu> with any questions or problems.

## Downloading ESMF

 * The ESMF library source code is also available for download at:
    * https://github.com/esmf-org/esmf/releases

## Building ESMF

 * To build ESMF from source consult the [Building ESMF](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/node9.html) section of the [ESMF User's Guide](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/).
 * For testing ESMF, see [Testing makefile targets](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/node11.html).
 * Pre-built binaries for ESMF and ESMPy are also available on `conda-forge` (_note Windows is not supported_):
```
conda create -c conda-forge -n <env> esmf esmpy
```

## Linking to ESMF

 * To link your application to an ESMF installation follow instructions located in [Compiling and Linking User Code against an ESMF Installation](http://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/node7.html).

## Add-On Packages

Several add-on packages are included with ESMF under [`./src/addon`](https://github.com/esmf-org/esmf/tree/master/src/addon):  
 * [ESMPy](src/addon/ESMPy/README.md) - Python interface to ESMF.
 * [NUOPC](src/addon/NUOPC/README) - Interoperability layer developed under the National Unified Operational Prediction Capability (NUOPC) program.

See each addon package for a specific README file.

## More Help

Information about the ESMF project can be found at the ESMF web site:
 * https://www.earthsystemmodeling.org/

Software documentation for the last public release is at:
 * http://earthsystemmodeling.org/doc/ -> Users -> User Docs

Software documentation for all releases is at:
 * http://earthsystemmodeling.org/static/releases.html
