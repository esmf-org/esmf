#if 0
 $Id: ESMF_LogMacros.inc,v 1.3 2003/04/15 20:21:37 nscollins Exp $

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

#define ESMF_LogErrMsg(anErr,err,msg) \
ESMF_LogErrMsg_LN(anErr,err,__LINE__,__FILE__,__DIR__,msg)

#define ESMC_LogErrMsg(err,msg) \
ESMC_LogErrMsg_LN(err,__LINE__,__FILE__,__DIR__,msg)

#define ESMF_LogErr(anErr,err) \
ESMF_LogErr_LN(anErr,err,__LINE__,__FILE__,__DIR__)

#define ESMC_LogErr(err) \
ESMC_LogErr_LN(err,__LINE__,__FILE__,__DIR__)

#define ESMF_LogWarnMsg(anErr,err,msg) \
ESMF_LogWarnMsg_LN(anErr,err,__LINE__,__FILE__,__DIR__,msg)

#define ESMC_LogWarnMsg(err,msg) \
ESMC_LogWarnMsg_LN(err,__LINE__,__FILE__,__DIR__,msg)

#define ESMF_LogWarn(anErr,err) \
ESMF_LogWarn_LN(anErr,err,__LINE__,__FILE__,__DIR__)

#define ESMC_LogWarn(err) \
ESMC_LogWarn_LN(err,__LINE__,__FILE__,__DIR__)

