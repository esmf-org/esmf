#if 0
$Id: ESMF_InitMacros.inc,v 1.31.2.2 2009/01/21 21:25:24 cdeluca Exp $

Earth System Modeling Framework
Copyright 2002-2009, University Corporation for Atmospheric Research,
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


#define ESMF_INIT_TYPE integer (ESMF_KIND_I8)

#if 0
 To detect uninitialized objects on compilers which do not support
 automatic structure initialization ESMF uses the small probablility
 of a given non-pathological (e.g. not all 0s, etc) bit string 
 showing up in memory. The following are some random bit strings
 (stored in 8 byte integers) used for this detection. 
 (For more information about the detection process please see Section 5.10 of the 
 developers guide.)

  ESMF_INIT_DEFINED  marks if a shallow object has been defined by ESMF. 
  ESMF_INIT_CREATED  marks if a deep object has been allocated
  ESMF_INIT_DELETED  marks if a deep object has been deallocated. 
#endif
#define ESMF_INIT_DEFINED 76838410
#define ESMF_INIT_CREATED 82949521
#define ESMF_INIT_DELETED 12594838

#define ESMF_INITMACROS_ON
#ifdef ESMF_INITMACROS_ON

#define ESMF_INIT_GET(var) var%isInit

#ifdef ESMF_NO_INITIALIZERS
#define ESMF_INIT_DECLARE  ESMF_INIT_TYPE :: isInit
#define ESMF_INIT_CHECK_SHALLOW(fget,finit,var) \
             if (fget(var) .ne. ESMF_INIT_DEFINED) call \
                 finit(var)
#else
#define ESMF_INIT_DECLARE ESMF_INIT_TYPE :: isInit = ESMF_INIT_DEFINED
#define ESMF_INIT_CHECK_SHALLOW(fget,finit,var)
#endif

#define ESMF_INIT_SET_DEFINED(var) var%isInit=ESMF_INIT_DEFINED
#define ESMF_INIT_SET_CREATED(var) var%isInit=ESMF_INIT_CREATED
#define ESMF_INIT_SET_DELETED(var) var%isInit=ESMF_INIT_DELETED
#define ESMF_INIT_COPY(dvar,svar)  dvar%isInit=svar%isInit


#define ESMF_INIT_CHECK_DEEP(fget,var, rc) \
if (ESMF_IMErr(fget(var),ESMF_CONTEXT,rc=rc)) return

#define ESMF_INIT_CHECK_DEEP_SHORT(fget,var, rc) \
if (ESMF_IMErr(fget(var),__LINE__,ESMF_FILENAME,"?",rc=rc)) return

#define ESMF_INIT_C_PAD   char isInit[8]; 

#else

#define ESMF_INIT_GET(var) 0

#ifdef ESMF_NO_INITIALIZERS
#define ESMF_INIT_DECLARE 
#define ESMF_INIT_CHECK_SHALLOW(fget,finit,var) 
#else
#define ESMF_INIT_DECLARE 
#define ESMF_INIT_CHECK_SHALLOW(fget,finit,var)
#endif

#define ESMF_INIT_SET_DEFINED(var) 
#define ESMF_INIT_SET_CREATED(var) 
#define ESMF_INIT_SET_DELETED(var) 
#define ESMF_INIT_COPY(dvar,svar)
#define ESMF_INIT_CHECK_DEEP(fget, var, rc)
#define ESMF_INIT_CHECK_DEEP_SHORT(fget,var, rc) 

#define ESMF_INIT_C_PAD 

#endif



#endif 

