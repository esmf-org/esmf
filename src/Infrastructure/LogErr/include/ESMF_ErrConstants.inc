#if 0
$Id: ESMF_ErrConstants.inc,v 1.1 2003/03/19 16:01:10 shep_smith Exp $

Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.


Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.


!BOP
-------------------------------------------------------------------------

!DESCRIPTION:

ESMF Error include file of constants for both FORTRAN or C++.

-------------------------------------------------------------------------
!EOP
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
 ESMF\_SINGLE\_ERR\_FILE : Send errors to single file for all PE's
!EOP
#endif
#define ESMF_SINGLE_ERR_FILE      4
#if 0
!BOP
 ESMF\_MULT\_ERR\_FILE : All PE's write to seperate files.
!EOP
#endif
#define ESMF_MULT_ERR_FILE        5
#if 0
!BOP
 ESMF\_ERR\_RETURN : Return on error
!EOP
#endif
#define ESMF_ERR_RETURN           6
#if 0
!BOP
 ESMF\_ERR\_REPORT : Print a detailed error report.
!EOP
#endif
#define ESMF_ERR_REPORT           7
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
---------------------------------------------------------------------------
  These are the generic error codes.  The user is free to add
  additional messages within the source code.
---------------------------------------------------------------------------
!EOP
#endif

#if 0
!BOP
 ESMF\_ERR\_MEM : Unable to allocate requested memory
!EOP
#endif
#define ESMF_ERR_MEM             55
#if 0
!BOP
 ESMF\_ERR\_SUP : No support for requested operation
!EOP
#endif
#define ESMF_ERR_SUP             56
#if 0
!BOP
 ESMF\_ERR\_SIG : Signal received
!EOP
#endif
#define ESMF_ERR_SIG             59
#if 0
!BOP
 ESMF\_ERR\_FP : Floating point exception
!EOP
#endif
#define ESMF_ERR_FP              72
#if 0
!BOP
 ESMF\_ERR\_COR : Corrupted ESMF object detected
!EOP
#endif
#define ESMF_ERR_COR             74
#if 0
!BOP
 ESMF\_ERR\_LIB : Error in library called by ESMF
!EOP
#endif
#define ESMF_ERR_LIB             76
#if 0
!BOP
 ESMF\_ERR\_PLIB : ESMF generated inconsistent data
!EOP
#endif
#define ESMF_ERR_PLIB            77
#if 0
!BOP
 ESMF\_ERR\_MEMC : Memory corrupted
!EOP
#endif
#define ESMF_ERR_MEMC            78
#if 0
!BOP
 ESMF\_ERR\_BUSY : Resource is busy
!EOP
#endif
#define ESMF_ERR_BUSY            91
#if 0
!BOP
 ESMF\_ERR\_SYS : System call error
!EOP
#endif
#define ESMF_ERR_SYS             92
#if 0
!BOP
 ESMF\_ERR\_ARG\_SIZ : Non-comforming object sizes used in operation
!EOP
#endif
#define ESMF_ERR_ARG_SIZ         60
#if 0
!BOP
 ESMF\_ERR\_ARG\_IDN : Two arguments not allowed to be the same
!EOP
#endif
#define ESMF_ERR_ARG_IDN         61
#if 0
!BOP
 ESMF\_ERR\_ARG\_WRONG : Wrong argument
!EOP
#endif
#define ESMF_ERR_ARG_WRONG       62
#if 0
!BOP
 ESMF\_ERR\_ARG\_CORRUPT : Null or corrupeted ESMF object sent in as argument
!EOP
#endif
#define ESMF_ERR_ARG_CORRUPT     64
#if 0
!BOP
 ESMF\_ERR\_ARG\_OUTOFRANGE : Input argument out of range
!EOP
#endif
#define ESMF_ERR_ARG_OUTOFRANGE  63
#if 0
!BOP
 ESMF\_ERR\_ARG\_BADPTR : Invalid pointer argument
!EOP
#endif
#define ESMF_ERR_ARG_BADPTR      68
#if 0
!BOP
 ESMF\_ERR\_ARG\_NOTSAMETYPE : Two arguments must be same object type
!EOP
#endif
#define ESMF_ERR_ARG_NOTSAMETYPE 69
#if 0
!BOP
 ESMF\_ERR\_ARG\_NOTSAMECOMM : Two arguments must have the same communicators
!EOP
#endif
#define ESMF_ERR_ARG_NOTSAMECOMM 80
#if 0
!BOP
 ESMF\_ERR\_ARG\_WRONGSTATE : Object in argument is in wrong state
!EOP
#endif
#define ESMF_ERR_ARG_WRONGSTATE  73
#if 0
!BOP
 ESMF\_ERR\_ARG\_INCOMP : Arguments are incompatible
!EOP
#endif
#define ESMF_ERR_ARG_INCOMP      75
#if 0
!BOP
 ESMF\_ERR\_FILE\_OPEN : Unable to open file
!EOP
#endif
#define ESMF_ERR_FILE_OPEN       65
#if 0
!BOP
 ESMF\_ERR\_FILE\_READ : Unable to read from file
!EOP
#endif
#define ESMF_ERR_FILE_READ       66
#if 0
!BOP
 ESMF\_ERR\_FILE\_WRITE : Unable to write to file
!EOP
#endif
#define ESMF_ERR_FILE_WRITE      67
#if 0
!BOP
 ESMF\_ERR\_FILE\_UNEXPECTED : Unexpected data in file
!EOP
#endif
#define ESMF_ERR_FILE_UNEXPECTED 79
#if 0
!BOP
 ESMF\_ERR\_FILE\_CLOSE : Unable to close file
!EOP
#endif
#define ESMF_ERR_FILE_CLOSE      81
#if 0
!BOP
 ESMF\_ERR\_INIT : Init method not called
!EOP
#endif
#define ESMF_ERR_INIT            82
#if 0
!BOP
 ESMF\_ERR\_FILE\_ACTIVE : Instrumented region  is still active
!EOP
#endif
#define ESMF_ERR_FILE_ACTIVE     90
