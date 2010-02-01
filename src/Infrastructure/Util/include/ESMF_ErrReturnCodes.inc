#if 0
$Id: ESMF_ErrReturnCodes.inc,v 1.10.2.3 2010/02/01 20:52:25 svasquez Exp $

Earth System Modeling Framework
Copyright 2002-2010, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.


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
These are here for convenient reference and possible future definition;
they are currently defined in ESMF_UtilTypes.F90 and ESMC_Macros.h
//BOP
\begin{verbatim}
=====================================================
Success/Failure Return codes for both Fortran and C++
=====================================================

#define  ESMF_SUCCESS             0
#define  ESMF_FAILURE            -1
#endif

#if 0
=====================================
Fortran Symmetric Return Codes 1-500
=====================================
#endif

#define  ESMF_RC_OBJ_BAD            1 
#define  ESMF_RC_OBJ_INIT           2
#define  ESMF_RC_OBJ_CREATE         3
#define  ESMF_RC_OBJ_COR            4
#define  ESMF_RC_OBJ_WRONG          5
#define  ESMF_RC_ARG_BAD            6
#define  ESMF_RC_ARG_RANK           7
#define  ESMF_RC_ARG_SIZE           8
#define  ESMF_RC_ARG_VALUE          9
#define  ESMF_RC_ARG_DUP           10
#define  ESMF_RC_ARG_SAMETYPE      11
#define  ESMF_RC_ARG_SAMECOMM      12
#define  ESMF_RC_ARG_INCOMP        13
#define  ESMF_RC_ARG_CORRUPT       14
#define  ESMF_RC_ARG_WRONG         15
#define  ESMF_RC_ARG_OUTOFRANGE    16
#define  ESMF_RC_ARG_OPT           17
#define  ESMF_RC_NOT_IMPL          18
#define  ESMF_RC_FILE_OPEN         19
#define  ESMF_RC_FILE_CREATE       20
#define  ESMF_RC_FILE_READ         21
#define  ESMF_RC_FILE_WRITE        22
#define  ESMF_RC_FILE_UNEXPECTED   23
#define  ESMF_RC_FILE_CLOSE        24
#define  ESMF_RC_FILE_ACTIVE       25
#define  ESMF_RC_PTR_NULL          26
#define  ESMF_RC_PTR_BAD           27
#define  ESMF_RC_PTR_NOTALLOC      28
#define  ESMF_RC_PTR_ISALLOC       29
#define  ESMF_RC_MEM               30
#define  ESMF_RC_MEMC              31
#define  ESMF_RC_DUP_NAME          32
#define  ESMF_RC_LONG_NAME         33
#define  ESMF_RC_LONG_STR          34
#define  ESMF_RC_COPY_FAIL         35
#define  ESMF_RC_DIV_ZERO          36
#define  ESMF_RC_CANNOT_GET        37
#define  ESMF_RC_CANNOT_SET        38
#define  ESMF_RC_NOT_FOUND         39
#define  ESMF_RC_NOT_VALID         40
#define  ESMF_RC_INTNRL_LIST       41
#define  ESMF_RC_INTNRL_INCONS     42
#define  ESMF_RC_INTNRL_BAD        43
#define  ESMF_RC_SYS               44
#define  ESMF_RC_BUSY              45
#define  ESMF_RC_LIB               46
#define  ESMF_RC_ATTR_UNUSED       47
#define  ESMF_RC_OBJ_NOT_CREATED   48
#define  ESMF_RC_OBJ_DELETED       49
#define  ESMF_RC_NOT_SET           50
#define  ESMF_RC_VAL_WRONG         51
#define  ESMF_RC_VAL_ERRBOUND      52  
#define  ESMF_RC_VAL_OUTOFRANGE    53
#define  ESMF_RC_OBJ_UNINIT        54

#if 0
54-500 reserved for future Fortran symmetric return code definitions
#endif

#if 0
=====================================
C++ Symmetric Return Codes 501-999
=====================================
#endif

#define  ESMC_RC_OBJ_BAD          501 
#define  ESMC_RC_OBJ_INIT         502
#define  ESMC_RC_OBJ_CREATE       503
#define  ESMC_RC_OBJ_COR          504
#define  ESMC_RC_OBJ_WRONG        505
#define  ESMC_RC_ARG_BAD          506
#define  ESMC_RC_ARG_RANK         507
#define  ESMC_RC_ARG_SIZE         508
#define  ESMC_RC_ARG_VALUE        509
#define  ESMC_RC_ARG_DUP          510
#define  ESMC_RC_ARG_SAMETYPE     511
#define  ESMC_RC_ARG_SAMECOMM     512
#define  ESMC_RC_ARG_INCOMP       513
#define  ESMC_RC_ARG_CORRUPT      514
#define  ESMC_RC_ARG_WRONG        515
#define  ESMC_RC_ARG_OUTOFRANGE   516
#define  ESMC_RC_ARG_OPT          517
#define  ESMC_RC_NOT_IMPL         518
#define  ESMC_RC_FILE_OPEN        519
#define  ESMC_RC_FILE_CREATE      520
#define  ESMC_RC_FILE_READ        521
#define  ESMC_RC_FILE_WRITE       522
#define  ESMC_RC_FILE_UNEXPECTED  523
#define  ESMC_RC_FILE_CLOSE       524
#define  ESMC_RC_FILE_ACTIVE      525
#define  ESMC_RC_PTR_NULL         526
#define  ESMC_RC_PTR_BAD          527
#define  ESMC_RC_PTR_NOTALLOC     528
#define  ESMC_RC_PTR_ISALLOC      529
#define  ESMC_RC_MEM              530
#define  ESMC_RC_MEMC             531
#define  ESMC_RC_DUP_NAME         532
#define  ESMC_RC_LONG_NAME        533
#define  ESMC_RC_LONG_STR         534
#define  ESMC_RC_COPY_FAIL        535
#define  ESMC_RC_DIV_ZERO         536
#define  ESMC_RC_CANNOT_GET       537
#define  ESMC_RC_CANNOT_SET       538
#define  ESMC_RC_NOT_FOUND        539
#define  ESMC_RC_NOT_VALID        540
#define  ESMC_RC_INTNRL_LIST      541
#define  ESMC_RC_INTNRL_INCONS    542
#define  ESMC_RC_INTNRL_BAD       543
#define  ESMC_RC_SYS              544
#define  ESMC_RC_BUSY             545
#define  ESMC_RC_LIB              546
#define  ESMC_RC_ATTR_UNUSED      547
#define  ESMC_RC_OBJ_NOT_CREATED  548
#define  ESMC_RC_OBJ_DELETED      549
#define  ESMC_RC_NOT_SET          550
#define  ESMC_RC_VAL_WRONG        551
#define  ESMC_RC_VAL_ERRBOUND     552
#define  ESMC_RC_VAL_OUTOFRANGE   553
#define  ESMC_RC_OBJ_UNINIT       554

#if 0
554-999 reserved for future C++ symmetric return code definitions
#endif

#if 0
=====================================
C++ Non-symmetric Return Codes 1000
=====================================
#endif

#define  ESMC_RC_OPTARG_BAD      1000

#if 0
\end{verbatim}
//EOP
#endif

#endif
