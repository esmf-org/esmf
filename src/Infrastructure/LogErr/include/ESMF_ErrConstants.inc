#if 0
$Id: ESMF_ErrConstants.inc,v 1.5 2004/05/14 09:33:57 nscollins Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.


ESMF Error include file of constants for both FORTRAN or C++.
Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.


#endif

#if !defined(__DIR__)
#define __DIR__ "unknowndirectory"
#endif 

#if 0
!BOP
 ESMF\_FATAL : Fatal error
!EOP
#endif
#define ESMF_FATAL                2
#if 0
!BOP
 ESMF\_WARNING : Non-Fatal error
!EOP
#endif
#define ESMF_WARNING              3
#if 0
!BOP
 ESMF\_RC\_RETURN : Return on error
!EOP
#endif
#define ESMF_RC_RETURN           6
#if 0
!BOP
 ESMF\_RC\_REPORT : Print a detailed error report.
!EOP
#endif
#define ESMF_RC_REPORT           7
#if 0
!BOP
 ESMF\_WARNINGS\_FATAL : Execution will terminate on warnings.
!EOP
#endif
#define ESMF_WARNINGS_FATAL       false
#if 0
!BOP
 ESMF\_WARNINGS\_NOT\_FATAL : Execution will not terminate on warnings
!EOP
#endif
#define ESMF_WARNINGS_NOT_FATAL   true


#if 0
!BOP
  These are the generic error codes.  The user is free to add
  additional messages within the source code.
!EOP
#endif

#if 0
!BOP
 ESMF\_RC\_MEM : Unable to allocate requested memory
!EOP
#endif
#define ESMF_RC_MEM             55
#if 0
!BOP
 ESMF\_RC\_SUP : No support for requested operation
!EOP
#endif
#define ESMF_RC_SUP             56
#if 0
!BOP
 ESMF\_RC\_SIG : Signal received
!EOP
#endif
#define ESMF_RC_SIG             59
#if 0
!BOP
 ESMF\_RC\_FP : Floating point exception
!EOP
#endif
#define ESMF_RC_FP              72
#if 0
!BOP
 ESMF\_RC\_COR : Corrupted ESMF object detected
!EOP
#endif
#define ESMF_RC_COR             74
#if 0
!BOP
 ESMF\_RC\_LIB : Error in library called by ESMF
!EOP
#endif
#define ESMF_RC_LIB             76
#if 0
!BOP
 ESMF\_RC\_PLIB : ESMF generated inconsistent data
!EOP
#endif
#define ESMF_RC_PLIB            77
#if 0
!BOP
 ESMF\_RC\_MEMC : Memory corrupted
!EOP
#endif
#define ESMF_RC_MEMC            78
#if 0
!BOP
 ESMF\_RC\_BUSY : Resource is busy
!EOP
#endif
#define ESMF_RC_BUSY            91
#if 0
!BOP
 ESMF\_RC\_SYS : System call error
!EOP
#endif
#define ESMF_RC_SYS             92
#if 0
!BOP
 ESMF\_RC\_ARG\_SIZ : Non-comforming object sizes used in operation
!EOP
#endif
#define ESMF_RC_ARG_SIZ         60
#if 0
!BOP
 ESMF\_RC\_ARG\_IDN : Two arguments not allowed to be the same
!EOP
#endif
#define ESMF_RC_ARG_IDN         61
#if 0
!BOP
 ESMF\_RC\_ARG\_WRONG : Wrong argument
!EOP
#endif
#define ESMF_RC_ARG_WRONG       62
#if 0
!BOP
 ESMF\_RC\_ARG\_CORRUPT : Null or corrupeted ESMF object sent in as argument
!EOP
#endif
#define ESMF_RC_ARG_CORRUPT     64
#if 0
!BOP
 ESMF\_RC\_ARG\_OUTOFRANGE : Input argument out of range
!EOP
#endif
#define ESMF_RC_ARG_OUTOFRANGE  63
#if 0
!BOP
 ESMF\_RC\_ARG\_BADPTR : Invalid pointer argument
!EOP
#endif
#define ESMF_RC_ARG_BADPTR      68
#if 0
!BOP
 ESMF\_RC\_ARG\_NOTSAMETYPE : Two arguments must be same object type
!EOP
#endif
#define ESMF_RC_ARG_NOTSAMETYPE 69
#if 0
!BOP
 ESMF\_RC\_ARG\_NOTSAMECOMM : Two arguments must have the same communicators
!EOP
#endif
#define ESMF_RC_ARG_NOTSAMECOMM 80
#if 0
!BOP
 ESMF\_RC\_ARG\_WRONGSTATE : Object in argument is in wrong state
!EOP
#endif
#define ESMF_RC_ARG_WRONGSTATE  73
#if 0
!BOP
 ESMF\_RC\_ARG\_INCOMP : Arguments are incompatible
!EOP
#endif
#define ESMF_RC_ARG_INCOMP      75
#if 0
!BOP
 ESMF\_RC\_FILE\_OPEN : Unable to open file
!EOP
#endif
#define ESMF_RC_FILE_OPEN       65
#if 0
!BOP
 ESMF\_RC\_FILE\_READ : Unable to read from file
!EOP
#endif
#define ESMF_RC_FILE_READ       66
#if 0
!BOP
 ESMF\_RC\_FILE\_WRITE : Unable to write to file
!EOP
#endif
#define ESMF_RC_FILE_WRITE      67
#if 0
!BOP
 ESMF\_RC\_FILE\_UNEXPECTED : Unexpected data in file
!EOP
#endif
#define ESMF_RC_FILE_UNEXPECTED 79
#if 0
!BOP
 ESMF\_RC\_FILE\_CLOSE : Unable to close file
!EOP
#endif
#define ESMF_RC_FILE_CLOSE      81
#if 0
!BOP
 ESMF\_RC\_INIT : Init method not called
!EOP
#endif
#define ESMF_RC_INIT            82
#if 0
!BOP
 ESMF\_RC\_FILE\_ACTIVE : Instrumented region  is still active
!EOP
#endif
#define ESMF_RC_FILE_ACTIVE     90
