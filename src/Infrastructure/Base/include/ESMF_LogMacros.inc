#if 0
 $Id: ESMF_LogMacros.inc,v 1.2 2004/05/21 09:23:42 nscollins Exp $

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

#ifndef ESMF_LOGMACROS_INC
#define ESMF_LOGMACROS_INC

#define ESMF_CONTEXT  __LINE__,ESMF_FILENAME,ESMF_METHOD


#ifndef ESMF_FILENAME
#define ESMF_FILENAME __FILE__
#endif

#define EM_LogFoundAllocError(status,rc) \
ESMF_LogFoundAllocError(status,__LINE__,ESMF_FILENAME_,ESMF_METHOD,rc)

#define EM_LogFoundError(status,rc) \
ESMF_LogFoundError(status,__LINE__,ESMF_FILENAME_,ESMF_METHOD,rc)

#define EM_LogMsgFoundAllocError(status,msg,rc) \
ESMF_LogMsgFoundAllocError(status,msg,__LINE__,ESMF_FILENAME_,ESMF_METHOD,rc)

#define EM_LogMsgFoundError(status,msg,rc) \
ESMF_LogMsgFoundError(status,msg,__LINE__,ESMF_FILENAME_,ESMF_METHOD,rc)

#define EM_LogWrite(msg,logtype) \
ESMF_LogWrite(msg,logtype,__LINE__,ESMF_FILENAME_,ESMF_METHOD)



#define ESMC_LogAllocError(rc) \
ESMC_LogAllocError(__LINE__,__FILE__,ESMC_METHOD,rc)

#define ESMC_LogFoundError(status,rc) \
ESMC_LogFoundError(status,__LINE__,__FILE__,ESMC_METHOD,rc)

#define ESMC_LogMsgAllocError(msg,rc) \
ESMC_LogMsgAllocError(msg,__LINE__,__FILE__,ESMC_METHOD,rc)

#define ESMC_LogMsgFoundError(status,msg,rc) \
ESMC_LogMsgFoundError(status,msg,__LINE__,__FILE__,ESMC_METHOD,rc)

#define ESMC_LogWrite(msg,logtype) \
ESMC_LogWrite(msg,logtype,__LINE__,__FILE__,ESMC_METHOD)

#endif
