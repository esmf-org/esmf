#if 0
$Id$

  Earth System Modeling Framework
  Copyright 2002-2020, University Corporation for Atmospheric Research,
  Massachusetts Institute of Technology, Geophysical Fluid Dynamics
  Laboratory, University of Michigan, National Centers for Environmental
  Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
  NASA Goddard Space Flight Center.
  Licensed under the University of Illinois-NCSA License.
  ----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
   This file is part of the pure C public ESMC API
  -----------------------------------------------------------------------------
#endif

#ifndef ESMC_RETURNCODES_H
#define ESMC_RETURNCODES_H

#if 0
//BOP
\begin{verbatim}

=====================================
Fortran Symmetric Return Codes 1-500
=====================================

 ESMF_SUCCESS               0
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
#define  ESMF_RC_MEM_ALLOCATE      31
#define  ESMF_RC_MEM_DEALLOCATE    32
#define  ESMF_RC_MEMC              33
#define  ESMF_RC_DUP_NAME          34
#define  ESMF_RC_LONG_NAME         35
#define  ESMF_RC_LONG_STR          36
#define  ESMF_RC_COPY_FAIL         37
#define  ESMF_RC_DIV_ZERO          38
#define  ESMF_RC_CANNOT_GET        39
#define  ESMF_RC_CANNOT_SET        40
#define  ESMF_RC_NOT_FOUND         41
#define  ESMF_RC_NOT_VALID         42
#define  ESMF_RC_INTNRL_LIST       43
#define  ESMF_RC_INTNRL_INCONS     44
#define  ESMF_RC_INTNRL_BAD        45
#define  ESMF_RC_SYS               46
#define  ESMF_RC_BUSY              47
#define  ESMF_RC_LIB               48
#define  ESMF_RC_LIB_NOT_PRESENT   49
#define  ESMF_RC_ATTR_UNUSED       50
#define  ESMF_RC_OBJ_NOT_CREATED   51
#define  ESMF_RC_OBJ_DELETED       52
#define  ESMF_RC_NOT_SET           53
#define  ESMF_RC_VAL_WRONG         54
#define  ESMF_RC_VAL_ERRBOUND      55
#define  ESMF_RC_VAL_OUTOFRANGE    56
#define  ESMF_RC_ATTR_NOTSET       57
#define  ESMF_RC_ATTR_WRONGTYPE    58
#define  ESMF_RC_ATTR_ITEMSOFF     59
#define  ESMF_RC_ATTR_LINK         60
#define  ESMF_RC_BUFFER_SHORT      61
#define  ESMF_RC_TIMEOUT           62
#define  ESMF_RC_FILE_EXISTS       63
#define  ESMF_RC_FILE_NOTDIR       64
#define  ESMF_RC_MOAB_ERROR        65
#define  ESMF_RC_NOOP              66
#define  ESMF_RC_NETCDF_ERROR      67

#if 0
68-499 reserved for future Fortran symmetric return code definitions
#endif

#if 0
=====================================
C/C++ Symmetric Return Codes 501-999
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
#define  ESMC_RC_MEM_ALLOCATE     531
#define  ESMC_RC_MEM_DEALLOCATE   532
#define  ESMC_RC_MEMC             533
#define  ESMC_RC_DUP_NAME         534
#define  ESMC_RC_LONG_NAME        535
#define  ESMC_RC_LONG_STR         536
#define  ESMC_RC_COPY_FAIL        537
#define  ESMC_RC_DIV_ZERO         538
#define  ESMC_RC_CANNOT_GET       539
#define  ESMC_RC_CANNOT_SET       540
#define  ESMC_RC_NOT_FOUND        541
#define  ESMC_RC_NOT_VALID        542
#define  ESMC_RC_INTNRL_LIST      543
#define  ESMC_RC_INTNRL_INCONS    544
#define  ESMC_RC_INTNRL_BAD       545
#define  ESMC_RC_SYS              546
#define  ESMC_RC_BUSY             547
#define  ESMC_RC_LIB              548
#define  ESMC_RC_LIB_NOT_PRESENT  549
#define  ESMC_RC_ATTR_UNUSED      550
#define  ESMC_RC_OBJ_NOT_CREATED  551
#define  ESMC_RC_OBJ_DELETED      552
#define  ESMC_RC_NOT_SET          553
#define  ESMC_RC_VAL_WRONG        554
#define  ESMC_RC_VAL_ERRBOUND     555
#define  ESMC_RC_VAL_OUTOFRANGE   556
#define  ESMC_RC_ATTR_NOTSET      557
#define  ESMC_RC_ATTR_WRONGTYPE   558
#define  ESMC_RC_ATTR_ITEMSOFF    559
#define  ESMC_RC_ATTR_LINK        560
#define  ESMC_RC_BUFFER_SHORT     561
#define  ESMC_RC_TIMEOUT          562
#define  ESMC_RC_FILE_EXISTS      563
#define  ESMC_RC_FILE_NOTDIR      564
#define  ESMC_RC_MOAB_ERROR       565
#define  ESMC_RC_NOOP             566
#define  ESMC_RC_NETCDF_ERROR     567

#if 0
568-999 reserved for future C/C++ symmetric return code definitions
#endif

#if 0
=====================================
C/C++ Non-symmetric Return Codes 1000
=====================================
#endif

#define  ESMC_RC_OPTARG_BAD      1000

#if 0
\end{verbatim}
//EOP
#endif

#endif
