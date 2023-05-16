# The ESMF build_config directory

> Earth System Modeling Framework
> 
> Copyright (c) 2002-2023 University Corporation for Atmospheric Research, Massachusetts Institute of Technology, Geophysical Fluid Dynamics Laboratory, University of Michigan, National Centers for Environmental Prediction, Los Alamos National Laboratory, Argonne National Laboratory, NASA Goddard Space Flight Center. All rights reserved.

The files in this directory contain per-platform makefile information which is 
included by the ESMF build system.

## Naming Conventions

In `$ESMF_DIR/build_config`, each compiler/platform combination has
a separate subdirectory which follows a 3-part naming convention:

The first part is the system name as it is set in `ESMF_OS`.

The second part is the compiler name for those platforms which support
compilers from different vendors.  For those systems which come with a
single vendor-supplied compiler, the compiler name is 'default'.  
The environment variable `ESMF_COMPILER` is used to select the compiler.

The last part of the name is the site-specific information. The 'default'
directories contain files which are always read for the given 
architecture/compiler combination. Then, in addition, if the environment
variable `ESMF_SITE` has a value, the corresponding directory will be 
searched after the default directory, for overrides and additional settings 
of directory names, values, flags, and other custom information.

## File Descriptions

Each default directory contains the following files:

- `README.md`: Configuration specific information.

- `build_rules.mk`: Makefile fragment containing configuration specific settings.

- `ESMC_Conf.h`: C/C++ preprocessor definitions for this platform.

- `ESMF_Conf.inc`: Fortran and C/C++ definitions in a format parseable by both languages.         

 
