#if 0
$Id: ESMF.h,v 1.4 2004/05/14 08:12:55 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.

main include file which includes all others
#endif

#if 0
Most of the previous contents of this file have been moved into the
base class fortan file, so the definitions of parameters will compile
into the base module.  But for macros needed by the framework, or
compile time defines, can be here.  THIS FILE WILL NOT BE INCLUDED BY
USER CODE, SO WE CANNOT WRITE USER EXAMPLE CODE THAT DEPENDS ON ANYTHING
INCLUDED HERE.  Our tests are an exception, since they need the test
macros in order to provide uniform error messages and exit codes.
#endif

#include "ESMF_Macros.inc"
#include "ESMF_Conf.inc"
#include "ESMF_Version.inc"
#include "ESMF_ErrMsgs.inc"

