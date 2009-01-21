#if 0
$Id: ESMF_Conf.inc,v 1.5.2.2 2009/01/21 21:25:18 cdeluca Exp $

Earth System Modeling Framework
Copyright 2002-2009, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if 0
In the ESMF_Config.F90 source file, if EOL is defined
as achar(10) (which is the newline character), 
the file cannot be compiled (it will not parse). 
If a bug fix for this is released, the next #define can
be removed.
#endif

#define ESMF_HAS_ACHAR_BUG 1

