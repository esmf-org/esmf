#if 0
 $Id: ESMF_LogMacros.inc,v 1.11 2004/05/14 23:31:56 eschwab Exp $

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

#define ESMF_LogFoundError(status,rc) \
ESMF_LogFoundError(status,__LINE__,__FILE__,ESMF_METHOD,rc)

#define ESMF_LogMsgFoundError(status,msg,rc) \
ESMF_LogMsgFoundError(status,msg,__LINE__,__FILE__,ESMF_METHOD,rc)

#define ESMF_LogFoundAllocError(status,rc) \
ESMF_LogFoundAllocError(status,__LINE__,__FILE__,ESMF_METHOD,rc)

#define ESMF_LogMsgFoundAllocError(status,msg,rc) \
ESMF_LogMsgFoundAllocError(status,msg,__LINE__,__FILE__,ESMF_METHOD,rc)


#define ESMC_LogWrite(msg,logtype) \
ESMC_LogWrite(msg,logtype,__LINE__,__FILE__,ESMC_METHOD)

#define ESMC_LogFoundError(status,rc) \
ESMC_LogFoundError(status,__LINE__,__FILE__,ESMC_METHOD,rc)

#define ESMC_LogMsgFoundError(status,msg,rc) \
ESMC_LogMsgFoundError(status,msg,__LINE__,__FILE__,ESMC_METHOD,rc)

#define ESMC_LogAllocError() \
ESMC_LogAllocError(__LINE__,__FILE__,ESMC_METHOD)

#define ESMC_LogMsgAllocError(msg) \
ESMC_LogMsgAllocError(msg,__LINE__,__FILE__,ESMC_METHOD)
