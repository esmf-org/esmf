#if 0
$Id: ESMF_InitMacros.inc,v 1.4 2006/11/20 03:24:56 oehmke Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.

Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.

These lines prevent this file from being read more than once if it
ends up being included multiple times.
#endif

#ifndef ESMF_INITMACROS_INC
#define ESMF_INITMACROS_INC


#if 0
  The following are the macros for the initialization standardization. 
They are in general used to hide the fact that some compilers dont allow 
automatic initialization of types.  
#endif



#ifdef ESMF_INITMACROS_ON

#define ESMF_INIT_DEFINED 76838410
#define ESMF_INIT_CREATED 82949521
#define ESMF_INIT_DELETED 12594838

#ifdef ESMF_NO_INITIALIZERS
#define ESMF_INIT_DECLARE integer (ESMF_KIND_I8) :: isInit
#define ESMF_INIT_CHECK_SHALLOW(type,var) \
             if (type##GetInitVal(var) .ne. ESMF_INIT_DEFINED) call \
                 type##Init(var)
#else
#define ESMF_INIT_DECLARE integer (ESMF_KIND_I8) :: isInit = ESMF_INIT_DEFINED
#define ESMF_INIT_CHECK_SHALLOW(var,type)
#endif

#define ESMF_INIT_SET_DEFINED(var) var%isInit=ESMF_INIT_DEFINED
#define ESMF_INIT_SET_CREATED(var) var%isInit=ESMF_INIT_CREATED
#define ESMF_INIT_SET_DELETED(var) var%isInit=ESMF_INIT_DELETED

#define ESMF_INIT_CHECK_DEEP(type,var, rc)                               \
    if (ESMF_LogMsgFoundError(ESMF_InitCheckDeep(type##GetInitVal(var)), \
        ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

#else


#define ESMF_INIT_DEFINED 76838410
#define ESMF_INIT_CREATED 82949521
#define ESMF_INIT_DELETED 12594838

#ifdef ESMF_NO_INITIALIZERS
#define ESMF_INIT_DECLARE integer (ESMF_KIND_I8) :: isInit
#define ESMF_INIT_CHECK_SHALLOW(type,var) 
#else
#define ESMF_INIT_DECLARE integer (ESMF_KIND_I8) :: isInit = ESMF_INIT_DEFINED
#define ESMF_INIT_CHECK_SHALLOW(var,type)
#endif

#define ESMF_INIT_SET_DEFINED(var) 
#define ESMF_INIT_SET_CREATED(var) 
#define ESMF_INIT_SET_DELETED(var) 
#define ESMF_INIT_CHECK_DEEP(type, var, rc)

#endif



#endif 

