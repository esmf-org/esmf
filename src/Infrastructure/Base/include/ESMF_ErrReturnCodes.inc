#if 0
$Id: ESMF_ErrReturnCodes.inc,v 1.2 2004/05/20 11:40:05 nscollins Exp $

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

#ifndef ESMF_ERRRETURNCODES_INC
#define ESMF_ERRRETURNCODES_INC

#if 0
Special macro for use in ESM[F,C]_LogMsgFoundError(), e.g.
  ESMF_LogMsgFoundError(rcToCheck, ESMF_ERR_PASSTHRU. rcToReturn)
#endif
#define ESMF_ERR_PASSTHRU   "Internal subroutine call returned Error"

#if 0
Return codes are defined here; corresponding error messages are defined in
ESMC_LogErrMsgs.C
#endif

#if 0
======================
F90 Return codes 1-500
======================
#endif

#define  ESMF_RC_OBJ_BAD          1 
#define  ESMF_RC_OBJ_INIT         2
#define  ESMF_RC_OBJ_CREATE       3
#define  ESMF_RC_OBJ_COR          4
#define  ESMF_RC_OBJ_WRONGSTATE   5

#define  ESMF_RC_ARG_BAD          6
#define  ESMF_RC_ARG_RANK         7
#define  ESMF_RC_ARG_SIZE         8
#define  ESMF_RC_ARG_VALUE        9
#define  ESMF_RC_ARG_DUP         10
#define  ESMF_RC_ARG_SAMETYPE    11
#define  ESMF_RC_ARG_SAMECOMM    12
#define  ESMF_RC_ARG_INCOMP      13
#define  ESMF_RC_ARG_CORRUPT     14
#define  ESMF_RC_ARG_WRONG       15
#define  ESMF_RC_ARG_OUTOFRANGE  16
#define  ESMF_RC_ARG_OPT         17
 
#define  ESMF_RC_NOT_IMPL        18

#define  ESMF_RC_FILE_OPEN       19
#define  ESMF_RC_FILE_CREATE     20
#define  ESMF_RC_FILE_READ       21
#define  ESMF_RC_FILE_WRITE      22
#define  ESMF_RC_FILE_UNEXPECTED 23
#define  ESMF_RC_FILE_CLOSE      24
#define  ESMF_RC_FILE_ACTIVE     25

#define  ESMF_RC_PTR_NULL        26
#define  ESMF_RC_PTR_BAD         27
#define  ESMF_RC_PTR_NOTALLOC    28
#define  ESMF_RC_PTR_ISALLOC     29
#define  ESMF_RC_MEM             30
#define  ESMF_RC_MEMC            31

#define  ESMF_RC_DUP_NAME        32
#define  ESMF_RC_LONG_NAME       33
#define  ESMF_RC_LONG_STR        34
#define  ESMF_RC_COPY_FAIL       35
#define  ESMF_RC_DIV_ZERO        36
#define  ESMF_RC_CANNOT_GET      37
#define  ESMF_RC_CANNOT_SET      38
#define  ESMF_RC_NOT_FOUND       39
#define  ESMF_RC_NOT_VALID       40

#define  ESMF_RC_INTNRL_LIST     41
#define  ESMF_RC_INTNRL_INCONS   42
#define  ESMF_RC_INTNRL_BAD      43

#define  ESMF_RC_SYS             44
#define  ESMF_RC_BUSY            45
#define  ESMF_RC_LIB             46

#if 0
=========================
C++ Return codes 501-1000
=========================
#endif

#define  ESMC_RC_OBJ_BAD         501 
#define  ESMC_RC_OBJ_INIT        502
#define  ESMC_RC_OBJ_CREATE      503
#define  ESMC_RC_OBJ_COR         504
#define  ESMC_RC_OBJ_WRONGSTATE  505

#define  ESMC_RC_ARG_BAD         506
#define  ESMC_RC_ARG_RANK        507
#define  ESMC_RC_ARG_SIZE        508
#define  ESMC_RC_ARG_VALUE       509
#define  ESMC_RC_ARG_DUP         510
#define  ESMC_RC_ARG_SAMETYPE    511
#define  ESMC_RC_ARG_SAMECOMM    512
#define  ESMC_RC_ARG_INCOMP      513
#define  ESMC_RC_ARG_CORRUPT     514
#define  ESMC_RC_ARG_WRONG       515
#define  ESMC_RC_ARG_OUTOFRANGE  516
#define  ESMC_RC_ARG_OPT         517
 
#define  ESMC_RC_NOT_IMPL        518

#define  ESMC_RC_FILE_OPEN       519
#define  ESMC_RC_FILE_CREATE     520
#define  ESMC_RC_FILE_READ       521
#define  ESMC_RC_FILE_WRITE      522
#define  ESMC_RC_FILE_UNEXPECTED 523
#define  ESMC_RC_FILE_CLOSE      524
#define  ESMC_RC_FILE_ACTIVE     525

#define  ESMC_RC_PTR_NULL        526
#define  ESMC_RC_PTR_BAD         527
#define  ESMC_RC_PTR_NOTALLOC    528
#define  ESMC_RC_PTR_ISALLOC     529
#define  ESMC_RC_MEM             530
#define  ESMC_RC_MEMC            531

#define  ESMC_RC_DUP_NAME        532
#define  ESMC_RC_LONG_NAME       533
#define  ESMC_RC_LONG_STR        534
#define  ESMC_RC_COPY_FAIL       535
#define  ESMC_RC_DIV_ZERO        536
#define  ESMC_RC_CANNOT_GET      537
#define  ESMC_RC_CANNOT_SET      538
#define  ESMC_RC_NOT_FOUND       539
#define  ESMC_RC_NOT_VALID       540

#define  ESMC_RC_INTNRL_LIST     541
#define  ESMC_RC_INTNRL_INCONS   542
#define  ESMC_RC_INTNRL_BAD      543

#define  ESMC_RC_SYS             544
#define  ESMC_RC_BUSY            545
#define  ESMC_RC_LIB             546

#endif
