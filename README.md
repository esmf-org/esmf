[![Build Status](https://travis-ci.org/esmf-org/esmf.svg?branch=master)](https://travis-ci.org/esmf-org/esmf)  

# Earth System Modeling Framework (ESMF)  

>Copyright (c) 2002-2020 University Corporation for Atmospheric Research, Massachusetts Institute of Technology, Geophysical Fluid Dynamics Laboratory, University of Michigan, National Centers for Environmental Prediction, Los Alamos National Laboratory, Argonne National Laboratory, NASA Goddard Space Flight Center. All rights reserved.

Hello and welcome to ESMF.

 * The [ESMF User's Guide](http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc/node3.html) contains information on building and installing ESMF.
 * The [ESMF Reference Manual](http://www.earthsystemmodeling.org/esmf_releases/last_built/ESMF_refdoc/) contains information on the architecture of ESMF,
   example code, and details of the API (Application Programming Interface).
 * Please contact <esmf_support@ucar.edu> with any questions or problems.

## Downloading ESMF

 * The ESMF library source code is also available for download at:
    * https://github.com/esmf-org/esmf/releases

## Building ESMF

 * To build ESMF from source consult the [Building ESMF](http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc/node6.html#SECTION00064000000000000000) section of the [ESMF User's Guide](http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc/node3.html).
 * For testing ESMF, see [Testing makefile targets](http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc/node6.html#SECTION00064500000000000000).
 * Pre-built binaries for ESMF and ESMPy are also available on `conda-forge`:
```
conda install -c conda-forge -n <env> esmf esmpy
```

## Linking to ESMF

 * To link your application to an ESMF installation follow instructions located in [Compiling and Linking User Code against an ESMF Installation](http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc/node7.html).

## Add-On Packages

Several add-on packages are included with ESMF under [`./src/addon`](https://github.com/esmf-org/esmf/tree/master/src/addon):  
 * `ESMPy` - Python interface to ESMF.
 * `MAPL` - Usability layer developed under the NASA Modeling Analysis and Prediction (MAP) program.
 * `NUOPC` - Interoperability layer developed under the National Unified Operational Prediction Capability (NUOPC) program.

See each addon package for a specific README file.

## More Help

Information about the ESMF project can be found at the ESMF web site:
 * https://www.earthsystemcog.org/

Software documentation for the last public release is at:
 * https://www.earthsystemcog.org/ -> Users -> User Docs

Software documentation for all releases is at:
 * https://www.earthsystemcog.org/ -> Software -> Download/Releases

Demos, system tests, and use test cases, demonstrating how ESMF can be used in realistic situations are available at:
 * https://www.earthsystemcog.org/projects/esmf/code_examples/

Contributions from ESMF users are available at:
 * http://sourceforge.net/projects/esmfcontrib
