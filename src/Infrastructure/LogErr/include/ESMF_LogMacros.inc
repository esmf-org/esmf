#if 0
 $Id: ESMF_LogMacros.inc,v 1.10 2004/05/14 08:14:47 cpboulder Exp $

 Earth System Modeling Framework
 Copyright 2002-2003, University Corporation for Atmospheric Research,
 Massachusetts Institute of Technology, Geophysical Fluid Dynamics
 Laboratory, University of Michigan, National Centers for Environmental
 Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
 NASA Goddard Space Flight Center.
 Licensed under the GPL.
 ------------------------------------------------------------------------------

 This file is included in framework source code and will give filename and
 line numbers where messages were generated from.  For user code in which
 no preprocessing step is preferred, the routines will report the messages
 but no line or file information.  If user code wants to generate this,
 they will have to include this file and use a preprocessing step.

#endif

#include "ESMF_ErrConstants.inc"
#include "ESMF_LogConstants.inc"

#define ESMF_LogWrite(msg,logtype) \
ESMF_LogWrite(msg,logtype,__LINE__,__FILE__,ESMF_METHOD)

#define ESMF_LogFoundError(rc,msg,logtype) \
ESMF_LogFoundError(rc,msg,logtype,__LINE__,__FILE__,ESMF_METHOD)

#define ESMF_LogFoundAllocError(rc,msg) \
ESMF_LogFoundAllocError(rc,msg,__LINE__,__FILE__,ESMF_METHOD)

#define ESMC_LogWrite(msg,logtype) \
ESMC_LogWrite(msg,logtype,__LINE__,__FILE__,ESMC_METHOD)

#define ESMC_LogFoundError(rc,msg,logtype) \
ESMC_LogFoundError(rc,msg,logtype,__LINE__,__FILE__,ESMC_METHOD)

#define ESMC_LogAllocError() \
ESMC_LogAllocError(__LINE__,__FILE__,ESMC_METHOD)
