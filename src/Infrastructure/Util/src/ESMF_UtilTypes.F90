! $Id: ESMF_UtilTypes.F90,v 1.56.2.16 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#define ESMF_FILENAME "ESMF_UtilTypes.F90"

!
! ESMF UtilTypes Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.
!BOP

!EOP

!------------------------------------------------------------------------------
! module definition

      module ESMF_UtilTypesMod
 
#include "ESMF.h"

!BOPI
! !MODULE: ESMF_UtilTypesMod - General utility/generic derived types and parms
!
! !DESCRIPTION:
!
! Common derived types and parameters which are not specifically associated
! with any one class.  Generally this must be updated strictly in sync with
! the corresponding ESMC_Util.h include file and/or ESMC_Util.C source file.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      ! inherit from ESMF base class
!     use ESMF_UtilTypesMod
 !!  use ESMF_InitMacrosMod Commented out to prevent circular dependency
 !!                         this is possible because since all the checks
 !!                         in this module are shallow - Bob 1/9/2007.

      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMC_Macros.h
!
!EOPI
!BOPI

!    !PUBLIC TYPES:
!    !Global integer parameters

      integer, parameter :: ESMF_SUCCESS = 0, ESMF_FAILURE = -1
      integer, parameter :: ESMF_MAXSTR = 128
! TODO:FIELDINTEGRATION Adjust MAXGRIDDIM
      integer, parameter :: ESMF_MAXDIM = 7, &
                            ESMF_MAXDECOMPDIM = 3, &
                            ESMF_MAXIGRIDDIM = 3, &
                            ESMF_MAXGRIDDIM = 3
     
!EOPI

      integer, parameter :: ESMF_MAJOR_VERSION = 3
      integer, parameter :: ESMF_MINOR_VERSION = 1
      integer, parameter :: ESMF_REVISION      = 0
      integer, parameter :: ESMF_PATCHLEVEL    = 2
      character(80), parameter :: ESMF_VERSION_STRING = "3.1.0rp2"

!------------------------------------------------------------------------------
!
!    ! General object status, useful for any object

!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMCI_Util.h

      type ESMF_Status
      sequence
      private
          integer :: status
      end type

      type(ESMF_Status), parameter :: ESMF_STATUS_UNINIT = ESMF_Status(1), &
                                      ESMF_STATUS_READY = ESMF_Status(2), &
                                      ESMF_STATUS_UNALLOCATED = ESMF_Status(3), &
                                      ESMF_STATUS_ALLOCATED = ESMF_Status(4), &
                                      ESMF_STATUS_BUSY = ESMF_Status(5), &
                                      ESMF_STATUS_INVALID = ESMF_Status(6), &
                                      ESMF_STATUS_NOT_READY = ESMF_Status(7)
 
!------------------------------------------------------------------------------
!
!    ! Generic pointer, large enough to hold a pointer on any architecture,
!    ! but not useful directly in fortran.  Expected to be used where a
!    ! pointer generated in C++ needs to be stored on the Fortran side.

!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMC_Util.h

      type ESMF_Pointer
      sequence
      !private
#ifdef ESMF_IS_32BIT_MACHINE
          integer*4 :: ptr
#else
          integer*8 :: ptr
#endif
      end type

      type(ESMF_Pointer), parameter :: ESMF_NULL_POINTER = ESMF_Pointer(0), &
                                       ESMF_BAD_POINTER = ESMF_Pointer(-1)


!------------------------------------------------------------------------------
!
!    ! Where we can use a derived type, the compiler will help do 
!    ! typechecking.  For those places where the compiler refuses to allow
!    ! anything but an Integer data type, use the second set of constants.

!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMC_Util.h

      type ESMF_TypeKind
      sequence
      ! TODO: can this be made private now?
      !!private
        integer :: dkind
      end type

      ! these work well for internal ESMF use, arguments, etc
      type(ESMF_TypeKind), parameter :: &
#ifndef ESMF_NO_INTEGER_1_BYTE 
                   ESMF_TYPEKIND_I1 = ESMF_TypeKind(1), &
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
                   ESMF_TYPEKIND_I2 = ESMF_TypeKind(2), &
#endif
                   ESMF_TYPEKIND_I4 = ESMF_TypeKind(3), &
                   ESMF_TYPEKIND_I8 = ESMF_TypeKind(4), &
                   ESMF_TYPEKIND_R4 = ESMF_TypeKind(5), &
                   ESMF_TYPEKIND_R8 = ESMF_TypeKind(6), &
                   ESMF_C8 = ESMF_TypeKind(7), &
                   ESMF_C16 = ESMF_TypeKind(8), &
                   ESMF_TYPEKIND_LOGICAL = ESMF_TypeKind(9), &
                   ESMF_TYPEKIND_CHARACTER = ESMF_TypeKind(10), &
                   ESMF_TYPEKIND_I  = ESMF_TypeKind(90), &
                   ESMF_TYPEKIND_R  = ESMF_TypeKind(91), &
                   ESMF_NOKIND = ESMF_TypeKind(99)

      ! these are the only Fortran kind parameters supported
      ! by ESMF.

      integer, parameter :: &
#ifndef ESMF_NO_INTEGER_1_BYTE 
                   ESMF_KIND_I1 = selected_int_kind(2), &
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
                   ESMF_KIND_I2 = selected_int_kind(4), &
#endif
                   ESMF_KIND_I4 = selected_int_kind(9), &
#ifndef ESMF_NEC_KIND_I8
                   ESMF_KIND_I8 = selected_int_kind(18), &
#else
                   ESMF_KIND_I8 = selected_int_kind(15), &
#endif
                   ESMF_KIND_R4 = selected_real_kind(3,25), &
                   ESMF_KIND_R8 = selected_real_kind(6,45), &
                   ESMF_KIND_C8 = selected_real_kind(3,25), &
                   ESMF_KIND_C16 = selected_real_kind(6,45)

      integer :: defaultIntegerDummy    ! Needed to define ESMF_KIND_I
      real    :: defaultRealDummy       ! Needed to define ESMF_KIND_R
      integer, parameter :: &
                   ESMF_KIND_I = kind(defaultIntegerDummy), &
                   ESMF_KIND_R = kind(defaultRealDummy)

!------------------------------------------------------------------------------
!
!    ! Dummy structure which must just be big enough to hold the values.
!    ! actual data values will always be accessed on the C++ side.

      type ESMF_DataValue
      sequence
      private
          integer :: pad
      end type

!------------------------------------------------------------------------------
!
!    ! Integer object type id, one for each ESMF Object type 
!    ! plus a string "official" object name.   Keep this in sync
!    ! with the C++ version!!

      type ESMF_ObjectID
      sequence
      !private
          integer :: objectID
          character (len=32) :: objectName
#if 0 
          ESMF_INIT_DECLARE
#endif
      end type

      ! these work well for internal ESMF use, arguments, etc
      type(ESMF_ObjectID), parameter :: &
         ESMF_ID_BASE = ESMF_ObjectID(1, "ESMF_Base"), &
         ESMF_ID_IOSPEC = ESMF_ObjectID(2, "ESMF_IOSpec"), &
         ESMF_ID_LOGERR = ESMF_ObjectID(3, "ESMF_LogErr"), &
         ESMF_ID_TIME = ESMF_ObjectID(4, "ESMF_Time"), &
         ESMF_ID_CALENDAR = ESMF_ObjectID(5, "ESMF_Calendar"), &
         ESMF_ID_TIMEINTERVAL = ESMF_ObjectID(6, "ESMF_TimeInterval"), &
         ESMF_ID_ALARM = ESMF_ObjectID(7, "ESMF_Alarm"), &
         ESMF_ID_CLOCK = ESMF_ObjectID(8, "ESMF_Clock"), &
         ESMF_ID_ARRAYSPEC = ESMF_ObjectID(9, "ESMF_ArraySpec"), &
         ESMF_ID_LOCALARRAY = ESMF_ObjectID(10, "ESMF_LocalArray"), &
         ESMF_ID_ARRAYBUNDLE = ESMF_ObjectID(11, "ESMF_ArrayBundle"), &
         ESMF_ID_VM = ESMF_ObjectID(12, "ESMF_VM"), &
         ESMF_ID_DELAYOUT = ESMF_ObjectID(13, "ESMF_DELayout"), &
         ESMF_ID_CONFIG = ESMF_ObjectID(14, "ESMF_Config"), &
         ESMF_ID_ARRAY = ESMF_ObjectID(16, "ESMF_Array"), &
         ESMF_ID_INTERNDG = ESMF_ObjectID(17, "ESMF_InternDG"), &
         ESMF_ID_COMMTABLE = ESMF_ObjectID(21, "ESMF_CommTable"), &
         ESMF_ID_ROUTETABLE = ESMF_ObjectID(22, "ESMF_RouteTable"), &
         ESMF_ID_ROUTE = ESMF_ObjectID(23, "ESMF_Route"), &
         ESMF_ID_ROUTEHANDLE = ESMF_ObjectID(24, "ESMF_RouteHandle"), &
         ESMF_ID_FIELDDATAMAP = ESMF_ObjectID(25, "ESMF_FieldDataMap"), &
         ESMF_ID_FIELD = ESMF_ObjectID(26, "ESMF_Field"), &
         ESMF_ID_FIELDBUNDLE = ESMF_ObjectID(28, "ESMF_FieldBundle"), &
         ESMF_ID_TRANSFORMVALUES = ESMF_ObjectID(29, "ESMF_TransformValues"), &
         ESMF_ID_REGRID = ESMF_ObjectID(30, "ESMF_Regrid"), &
         ESMF_ID_TRANSFORM = ESMF_ObjectID(31, "ESMF_Transform"), &
         ESMF_ID_STATE = ESMF_ObjectID(32, "ESMF_State"), &
         ESMF_ID_GRIDCOMPONENT = ESMF_ObjectID(33, "ESMF_GridComponent"), &
         ESMF_ID_CPLCOMPONENT = ESMF_ObjectID(34, "ESMF_CplComponent"), &
         ESMF_ID_COMPONENT = ESMF_ObjectID(35, "ESMF_Component"), &
         ESMF_ID_INTERNARRAY = ESMF_ObjectID(36, "ESMF_InternArray"), &
         ESMF_ID_NONE = ESMF_ObjectID(99, "ESMF_None")


!------------------------------------------------------------------------------
!
      type ESMF_AxisIndex
      sequence
          integer :: min
          integer :: max
          integer :: stride
          integer :: pad    ! insure F90/C++ memory alignment
          ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!
      ! possible new type, unused for now.
      type ESMF_Domain
      sequence
          integer :: DE
          integer :: rank
          ! TODO:  add an element for size in points (memory)
          type (ESMF_AxisIndex) :: ai(ESMF_MAXDIM)
          ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!
      ! in the regrid code if we want to send only parts of the
      ! data array, here is a structure which allows lists of blocks
      ! to be described in a single container object.
      type ESMF_DomainList
      sequence
          integer :: num_domains     ! number of domains stored
          integer :: current_size    ! size of buffer, used in linked list
          integer :: total_points    ! total size of domain (number of points)
          integer :: pad_for_64bit   ! unused
          type(ESMF_Domain), dimension(:), pointer :: domains
          ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!
      ! For logically rectangular igridded data, are the index numbers being
      ! computed/retrieved/exchanged relative to an origin of (0,0) on our
      ! local chunk (local), or are they global index numbers relative to 
      ! all index numbers across the entire igrid (global!). 
      type ESMF_LocalGlobalFlag
      sequence
          integer :: value
      end type
   
      type(ESMF_LocalGlobalFlag), parameter :: &
                                    ESMF_LOCAL  = ESMF_LocalGlobalFlag(1), &
                                    ESMF_GLOBAL = ESMF_LocalGlobalFlag(2)

!------------------------------------------------------------------------------
!
      ! Once a large igrid of data is decomposed into various local chunks
      ! there are several different "item counts" or "domains" of interest.  
      ! They include:
      !
      ! exclusive - cells which are far enough inside the local chunks that
      !  their values are never sent to other chunks as part of a halo update.
      !
      ! computational - cells which are the responsibility of the local 
      !  processor to update.
      !
      ! total - computational plus the halo region; all cells which can be
      !  read by the local processor.
      !
      ! allocated - total plus cells which are never read or written, but 
      !  may be allocated to ensure memory address alignment or even memory 
      !  block allocations even when the number of cells differs per chunk.
      !
      ! (TODO: allocated is not currently supported but has been requested.)
      !
      ! (See the ESMF Reference Manual Glossary if you want the precise
      !  definitions of what these are.  This is not the official blessed
      !  legal guide...)
      !
      type ESMF_DomainTypeFlag
      sequence
          integer :: value
      end type
   
      type(ESMF_DomainTypeFlag), parameter :: &
                     ESMF_DOMAIN_EXCLUSIVE        = ESMF_DomainTypeFlag(1), &
                     ESMF_DOMAIN_COMPUTATIONAL    = ESMF_DomainTypeFlag(2), &
                     ESMF_DOMAIN_TOTAL            = ESMF_DomainTypeFlag(3), &
                     ESMF_DOMAIN_ALLOCATED        = ESMF_DomainTypeFlag(4), &
                     ESMF_DOMAIN_OLDEXCLUSIVE     = ESMF_DomainTypeFlag(5), &
                     ESMF_DOMAIN_OLDCOMPUTATIONAL = ESMF_DomainTypeFlag(6), &
                     ESMF_DOMAIN_OLDTOTAL         = ESMF_DomainTypeFlag(7)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
!     ! Typed true/false values which are not compiler dependent, so that
!     ! when crossing the Fortran/C++ language boundary with logical values we
!     ! have a consistent interpretation.  In C/C++ 0=false, 1=true, but this
!     ! is not defined for Fortran and different compilers use different values
!     ! for booleans.

!     ! WARNING: must match corresponding values in ../include/ESMC_Util.h

      type ESMF_Logical
      sequence
      private
          integer :: value
      end type

      type(ESMF_Logical), parameter :: ESMF_TRUE     = ESMF_Logical(1), &
                                       ESMF_FALSE    = ESMF_Logical(2)

!------------------------------------------------------------------------------
!
!     ! Typed reduction operations

!     ! WARNING: must match corresponding values in ../include/ESMC_Util.h

      type ESMF_ReduceFlag
      sequence
      private
          integer :: value
      end type

      type(ESMF_ReduceFlag), parameter ::  ESMF_SUM   = ESMF_ReduceFlag(1), &
                                          ESMF_MIN   = ESMF_ReduceFlag(2), &
                                          ESMF_MAX   = ESMF_ReduceFlag(3)
                                     
!------------------------------------------------------------------------------
!
!     ! Typed blocking/non-blocking flag

      type ESMF_BlockingFlag
      sequence
      private
          integer :: value
      end type

      type(ESMF_BlockingFlag), parameter:: &
        ESMF_BLOCKING     = ESMF_BlockingFlag(1), &
        ESMF_VASBLOCKING  = ESMF_BlockingFlag(2), &
        ESMF_NONBLOCKING  = ESMF_BlockingFlag(3)

!------------------------------------------------------------------------------
!
!     ! Typed context flag

      type ESMF_ContextFlag
      sequence
      private
          integer :: value
      end type

      type(ESMF_ContextFlag), parameter:: &
        ESMF_CHILD_IN_NEW_VM     = ESMF_ContextFlag(1), &
        ESMF_CHILD_IN_PARENT_VM  = ESMF_ContextFlag(2)

!------------------------------------------------------------------------------
!
!     ! Typed termination flag

      type ESMF_TerminationFlag
      sequence
      private
          integer :: value
      end type

      type(ESMF_TerminationFlag), parameter:: &
        ESMF_FINAL        = ESMF_TerminationFlag(1), &
        ESMF_KEEPMPI      = ESMF_TerminationFlag(2), &
        ESMF_ABORT        = ESMF_TerminationFlag(3)

!------------------------------------------------------------------------------
!
!     ! Typed DE pinning flag

      type ESMF_DePinFlag
      sequence
      private
          integer :: value
      end type

      type(ESMF_DePinFlag), parameter:: &
        ESMF_DE_PIN_PET        = ESMF_DePinFlag(1), &
        ESMF_DE_PIN_VAS        = ESMF_DePinFlag(2)

!------------------------------------------------------------------------------
!
!     ! Direction type

      type ESMF_Direction
      sequence
      private
          integer :: value
      end type

      type(ESMF_Direction), parameter:: &
        ESMF_MODE_FORWARD = ESMF_Direction(1), &
        ESMF_MODE_REVERSE = ESMF_Direction(2)

!------------------------------------------------------------------------------
!     ! ESMF_IndexFlag
!
!     ! Interface flag for setting index bounds

      type ESMF_IndexFlag
      sequence
      private
        integer :: i_type
      end type

      type(ESMF_IndexFlag), parameter ::  &
                               ESMF_INDEX_DELOCAL  = ESMF_IndexFlag(0), &
                               ESMF_INDEX_GLOBAL = ESMF_IndexFlag(1), &
                               ESMF_INDEX_USER = ESMF_IndexFlag(2)

!------------------------------------------------------------------------------
!     ! ESMF_RegionFlag
!
!     ! Interface flag for setting index bounds

      type ESMF_RegionFlag
      sequence
      private
        integer :: i_type
      end type

      type(ESMF_RegionFlag), parameter ::  &
        ESMF_REGION_TOTAL = ESMF_RegionFlag(0), &
        ESMF_REGION_SELECT = ESMF_RegionFlag(1), &
        ESMF_REGION_EMPTY = ESMF_RegionFlag(2)

!------------------------------------------------------------------------------
!BOPI
!
! !PUBLIC TYPES:

      public ESMF_STATUS_UNINIT, ESMF_STATUS_READY, &
             ESMF_STATUS_UNALLOCATED, ESMF_STATUS_ALLOCATED, &
             ESMF_STATUS_BUSY, ESMF_STATUS_INVALID

#ifndef ESMF_NO_INTEGER_1_BYTE 
      public ESMF_TYPEKIND_I1
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
      public ESMF_TYPEKIND_I2
#endif

      public ESMF_TYPEKIND_I4, ESMF_TYPEKIND_I8, & 
             ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8, &
             ESMF_C8, ESMF_C16, &
             ESMF_TYPEKIND_LOGICAL, ESMF_TYPEKIND_CHARACTER, &
             ESMF_KIND_I, ESMF_KIND_R, &
             ESMF_NOKIND

#ifndef ESMF_NO_INTEGER_1_BYTE 
      public ESMF_KIND_I1
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
      public ESMF_KIND_I2
#endif
      public ESMF_KIND_I4, ESMF_KIND_I8, & 
             ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_C8, ESMF_KIND_C16

      public ESMF_NULL_POINTER, ESMF_BAD_POINTER

      public ESMF_Logical, ESMF_TRUE, ESMF_FALSE

      public ESMF_Direction, ESMF_MODE_FORWARD, ESMF_MODE_REVERSE

      public ESMF_IndexFlag
      public ESMF_INDEX_DELOCAL, ESMF_INDEX_GLOBAL, ESMF_INDEX_USER
      public ESMF_RegionFlag, &
             ESMF_REGION_TOTAL, ESMF_REGION_SELECT, ESMF_REGION_EMPTY

      public ESMF_ReduceFlag, ESMF_SUM, ESMF_MIN, ESMF_MAX
      public ESMF_BlockingFlag, ESMF_BLOCKING, ESMF_VASBLOCKING, &
             ESMF_NONBLOCKING
      public ESMF_ContextFlag, ESMF_CHILD_IN_NEW_VM, ESMF_CHILD_IN_PARENT_VM
      public ESMF_TerminationFlag, ESMF_FINAL, ESMF_KEEPMPI, ESMF_ABORT
      public ESMF_DePinFlag, ESMF_DE_PIN_PET, ESMF_DE_PIN_VAS

      public ESMF_FAILURE, ESMF_SUCCESS
      public ESMF_MAXSTR
! TODO:FIELDINTEGRATION Adjust MAXGRIDDIM
      public ESMF_MAXDIM, ESMF_MAXDECOMPDIM, ESMF_MAXIGRIDDIM, ESMF_MAXGRIDDIM
     
      public ESMF_MAJOR_VERSION, ESMF_MINOR_VERSION
      public ESMF_REVISION, ESMF_PATCHLEVEL
      public ESMF_VERSION_STRING 

      public ESMF_ObjectID

#if 0 
      public ESMF_ObjectIDGetInit, ESMF_ObjectIDInit, ESMF_ObjectIDValidate
#endif
      public ESMF_ID_NONE
      public ESMF_ID_BASE, ESMF_ID_IOSPEC, ESMF_ID_LOGERR, ESMF_ID_TIME
      public ESMF_ID_CALENDAR, ESMF_ID_TIMEINTERVAL, ESMF_ID_ALARM
      public ESMF_ID_CLOCK, ESMF_ID_ARRAYSPEC, ESMF_ID_LOCALARRAY
      public ESMF_ID_ARRAYBUNDLE, ESMF_ID_VM, ESMF_ID_DELAYOUT
      public ESMF_ID_CONFIG, ESMF_ID_ARRAY
      public ESMF_ID_INTERNARRAY, ESMF_ID_INTERNDG
      public ESMF_ID_COMMTABLE, ESMF_ID_ROUTETABLE, ESMF_ID_ROUTE
      public ESMF_ID_ROUTEHANDLE, ESMF_ID_FIELDDATAMAP, ESMF_ID_FIELD
      public ESMF_ID_FIELDBUNDLE, ESMF_ID_TRANSFORMVALUES
      public ESMF_ID_REGRID, ESMF_ID_TRANSFORM, ESMF_ID_STATE
      public ESMF_ID_GRIDCOMPONENT, ESMF_ID_CPLCOMPONENT, ESMF_ID_COMPONENT

      public ESMF_LOCAL, ESMF_GLOBAL

      public ESMF_DOMAIN_EXCLUSIVE, ESMF_DOMAIN_COMPUTATIONAL
      public ESMF_DOMAIN_TOTAL, ESMF_DOMAIN_ALLOCATED
      public ESMF_DOMAIN_OLDEXCLUSIVE, ESMF_DOMAIN_OLDCOMPUTATIONAL
      public ESMF_DOMAIN_OLDTOTAL

      public ESMF_Status, ESMF_Pointer, ESMF_TypeKind
      public ESMF_DataValue
      public ESMF_Domain, ESMF_DomainGetInit, ESMF_DomainInit, &
             ESMF_DomainValidate
      public ESMF_DomainList, ESMF_DomainListGetInit, ESMF_DomainListInit, & 
             ESMF_DomainListValidate
      public ESMF_AxisIndex, ESMF_AxisIndexGetInit, ESMF_AxisIndexInit, &
             ESMF_AxisIndexValidate
      public ESMF_LocalGlobalFlag, ESMF_DomainTypeFlag

      public ESMF_PointerPrint
      
!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!

!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare 
!  them as if they were simple integers.
 

interface operator (.eq.)
  module procedure ESMF_sfeq
  module procedure ESMF_dkeq
  module procedure ESMF_pteq
  module procedure ESMF_tfeq
  module procedure ESMF_aieq
  module procedure ESMF_bfeq
  module procedure ESMF_ctfeq
  module procedure ESMF_tnfeq
  module procedure ESMF_freq
  module procedure ESMF_lgeq
  module procedure ESMF_dmeq
  module procedure ESMF_ifeq
  module procedure ESMF_rfeq
end interface

interface operator (.ne.)
  module procedure ESMF_sfne
  module procedure ESMF_dkne
  module procedure ESMF_ptne
  module procedure ESMF_tfne
  module procedure ESMF_aine
  module procedure ESMF_bfne
  module procedure ESMF_ctfne
  module procedure ESMF_tnfne
  module procedure ESMF_frne
  module procedure ESMF_lgne
  module procedure ESMF_dmne
end interface

interface assignment (=)
  module procedure ESMF_bfas
  module procedure ESMF_dkas
  module procedure ESMF_tfas
  module procedure ESMF_ptas
  module procedure ESMF_ptas2
end interface  


!------------------------------------------------------------------------------

      contains

#if 0 
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ObjectIDGetInit"
!BOPI
! !IROUTINE:  ESMF_ObjectIDGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_ObjectIDGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_ObjectID), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_ObjectIDGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt domain}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ObjectID} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_ObjectIDGetInit = ESMF_INIT_GET(s)
       else
         ESMF_ObjectIDGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_ObjectIDGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ObjectIDInit"
!BOPI
! !IROUTINE:  ESMF_ObjectIDInit - Initialize ObjectID

! !INTERFACE:
    subroutine ESMF_ObjectIDInit(s)
!
! !ARGUMENTS:
       type(ESMF_ObjectID) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt ObjectID}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ObjectID} of which being initialized.
!     \end{description}
!
!EOPI
       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_ObjectIDInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ObjectIDValidate"
!BOPI
! !IROUTINE:  ESMF_ObjectIDValidate - Check validity of a ObjectID 

! !INTERFACE:
    subroutine ESMF_ObjectIDValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_ObjectID), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt ObjectID} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ObjectID} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt ObjectID}
!           is valid.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_ObjectIDGetInit,ESMF_ObjectIDInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_ObjectIDValidate

#endif

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AxisIndexGetInit"
!BOPI
! !IROUTINE:  ESMF_AxisIndexGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_AxisIndexGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_AxisIndex), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_AxisIndexGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt AxisIndex}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_AxisIndex} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_AxisIndexGetInit = ESMF_INIT_GET(s)
       else
         ESMF_AxisIndexGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_AxisIndexGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AxisIndexInit"
!BOPI
! !IROUTINE:  ESMF_AxisIndexInit - Initialize AxisIndex 

! !INTERFACE:
    subroutine ESMF_AxisIndexInit(s)
!
! !ARGUMENTS:
       type(ESMF_AxisIndex) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt AxisIndex}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_AxisIndex} of which being initialized.
!     \end{description}
!
!EOPI
       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_AxisIndexInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AxisIndexValidate"
!BOPI
! !IROUTINE:  ESMF_AxisIndexValidate - Check validity of a AxisIndex 

! !INTERFACE:
    subroutine ESMF_AxisIndexValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_AxisIndex), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt AxisIndex} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_AxisIndex} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt AxisIndex}
!           is valid.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_AxisIndexValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainGetInit"
!BOPI
! !IROUTINE:  ESMF_DomainGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_DomainGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_Domain), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_DomainGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt domain}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Domain} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_DomainGetInit = ESMF_INIT_GET(s)
       else
         ESMF_DomainGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_DomainGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainInit"
!BOPI
! !IROUTINE:  ESMF_DomainInit - Initialize Domain

! !INTERFACE:
    subroutine ESMF_DomainInit(s)
!
! !ARGUMENTS:
       type(ESMF_Domain) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt domain}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Domain} of which being initialized.
!     \end{description}
!
!EOPI
       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_DomainInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainValidate"
!BOPI
! !IROUTINE:  ESMF_DomainValidate - Check validity of a Domain

! !INTERFACE:
    subroutine ESMF_DomainValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_Domain), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt Domain} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Domain} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt domain}
!           is valid.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_DomainGetInit,ESMF_DomainInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_DomainValidate

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListGetInit"
!BOPI
! !IROUTINE:  ESMF_DomainListGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_DomainListGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_DomainList), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_DomainListGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt domainlist}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_DomainList} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_DomainListGetInit = ESMF_INIT_GET(s)
       else
         ESMF_DomainListGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_DomainListGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListInit"
!BOPI
! !IROUTINE:  ESMF_DomainListInit - Initialize DomainList

! !INTERFACE:
    subroutine ESMF_DomainListInit(s)
!
! !ARGUMENTS:
       type(ESMF_DomainList) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt DomainList}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_DomainList} of which being initialized.
!     \end{description}
!
!EOPI
       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_DomainListInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DomainListValidate"
!BOPI
! !IROUTINE:  ESMF_DomainListValidate - Check validity of a DomainList

! !INTERFACE:
    subroutine ESMF_DomainListValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_DomainList), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt DomainList} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_DomainList} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt DomainList}
!           is valid.
!     \end{description}
!
!EOPI

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_DomainListValidate


!------------------------------------------------------------------------------
! function to compare two ESMF_Status flags to see if they're the same or not

function ESMF_sfeq(sf1, sf2)
 logical ESMF_sfeq
 type(ESMF_Status), intent(in) :: sf1, sf2

 ESMF_sfeq = (sf1%status .eq. sf2%status)
end function

function ESMF_sfne(sf1, sf2)
 logical ESMF_sfne
 type(ESMF_Status), intent(in) :: sf1, sf2

 ESMF_sfne = (sf1%status .ne. sf2%status)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_TypeKinds to see if they're the same or not

function ESMF_dkeq(dk1, dk2)
 logical ESMF_dkeq
 type(ESMF_TypeKind), intent(in) :: dk1, dk2

 ESMF_dkeq = (dk1%dkind .eq. dk2%dkind)
end function

function ESMF_dkne(dk1, dk2)
 logical ESMF_dkne
 type(ESMF_TypeKind), intent(in) :: dk1, dk2

 ESMF_dkne = (dk1%dkind .ne. dk2%dkind)
end function

subroutine ESMF_dkas(intval, dkval)
 integer, intent(out) :: intval
 type(ESMF_TypeKind), intent(in) :: dkval

 intval = dkval%dkind
end subroutine


!------------------------------------------------------------------------------
! function to compare two ESMF_BlockingFlags

subroutine ESMF_bfas(bf1, bf2)
 type(ESMF_BlockingFlag), intent(out) :: bf1
 type(ESMF_BlockingFlag), intent(in)  :: bf2

 bf1%value = bf2%value
end subroutine

function ESMF_bfeq(bf1, bf2)
 logical ESMF_bfeq
 type(ESMF_BlockingFlag), intent(in) :: bf1, bf2

 ESMF_bfeq = (bf1%value .eq. bf2%value)
end function

function ESMF_bfne(bf1, bf2)
 logical ESMF_bfne
 type(ESMF_BlockingFlag), intent(in) :: bf1, bf2

 ESMF_bfne = (bf1%value .ne. bf2%value)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_ContextFlags

function ESMF_ctfeq(ctf1, ctf2)
 logical ESMF_ctfeq
 type(ESMF_ContextFlag), intent(in) :: ctf1, ctf2

 ESMF_ctfeq = (ctf1%value .eq. ctf2%value)
end function

function ESMF_ctfne(ctf1, ctf2)
 logical ESMF_ctfne
 type(ESMF_ContextFlag), intent(in) :: ctf1, ctf2

 ESMF_ctfne = (ctf1%value .ne. ctf2%value)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_TerminationFlags

function ESMF_tnfeq(tnf1, tnf2)
 logical ESMF_tnfeq
 type(ESMF_TerminationFlag), intent(in) :: tnf1, tnf2

 ESMF_tnfeq = (tnf1%value .eq. tnf2%value)
end function

function ESMF_tnfne(tnf1, tnf2)
 logical ESMF_tnfne
 type(ESMF_TerminationFlag), intent(in) :: tnf1, tnf2

 ESMF_tnfne = (tnf1%value .ne. tnf2%value)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_Pointers to see if they're the same or not

function ESMF_pteq(pt1, pt2)
 logical ESMF_pteq
 type(ESMF_Pointer), intent(in) :: pt1, pt2

 ESMF_pteq = (pt1%ptr .eq. pt2%ptr)
end function

function ESMF_ptne(pt1, pt2)
 logical ESMF_ptne
 type(ESMF_Pointer), intent(in) :: pt1, pt2

 ESMF_ptne = (pt1%ptr .ne. pt2%ptr)
end function

subroutine ESMF_ptas(ptval, intval)
 type(ESMF_Pointer), intent(out) :: ptval
 integer, intent(in) :: intval

 ptval%ptr = intval
end subroutine

subroutine ESMF_ptas2(ptval2, ptval)
 type(ESMF_Pointer), intent(out) :: ptval2
 type(ESMF_Pointer), intent(in) :: ptval

 ptval2%ptr = ptval%ptr
end subroutine

!------------------------------------------------------------------------------
! function to compare two ESMF_Logicals to see if they're the same or not
! also assignment to real f90 logical 

function ESMF_tfeq(tf1, tf2)
 logical ESMF_tfeq
 type(ESMF_Logical), intent(in) :: tf1, tf2

 ESMF_tfeq = (tf1%value .eq. tf2%value)
end function

function ESMF_tfne(tf1, tf2)
 logical ESMF_tfne
 type(ESMF_Logical), intent(in) :: tf1, tf2

 ESMF_tfne = (tf1%value .ne. tf2%value)
end function

subroutine ESMF_tfas(lval, tfval)
 logical, intent(out) :: lval
 type(ESMF_Logical), intent(in) :: tfval

 lval = (tfval%value .eq. 1)    ! this must match initializer
end subroutine

!------------------------------------------------------------------------------
! function to compare two ESMF_AxisIndex to see if they're the same or not

function ESMF_aieq(ai1, ai2)
 logical ESMF_aieq
 type(ESMF_AxisIndex), intent(in) :: ai1, ai2

 ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,ai1)
 ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,ai2)

 ESMF_aieq = ((ai1%min .eq. ai2%min) .and. &
              (ai1%max .eq. ai2%max) .and. &
              (ai1%stride .eq. ai2%stride))

end function

function ESMF_aine(ai1, ai2)
 logical ESMF_aine
 type(ESMF_AxisIndex), intent(in) :: ai1, ai2

 ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,ai1)
 ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,ai2)

 ESMF_aine = ((ai1%min .ne. ai2%min) .or. &
              (ai1%max .ne. ai2%max) .or. &
              (ai1%stride .ne. ai2%stride))

end function

!------------------------------------------------------------------------------
! function to compare two ESMF_Direction types

function ESMF_freq(fr1, fr2)
 logical ESMF_freq
 type(ESMF_Direction), intent(in) :: fr1, fr2

 ESMF_freq = (fr1%value .eq. fr2%value)
end function

function ESMF_frne(fr1, fr2)
 logical ESMF_frne
 type(ESMF_Direction), intent(in) :: fr1, fr2

 ESMF_frne = (fr1%value .ne. fr2%value)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_LocalGlobalFlag types

function ESMF_lgeq(lg1, lg2)
 logical ESMF_lgeq
 type(ESMF_LocalGlobalFlag), intent(in) :: lg1, lg2

 ESMF_lgeq = (lg1%value .eq. lg2%value)
end function

function ESMF_lgne(lg1, lg2)
 logical ESMF_lgne
 type(ESMF_LocalGlobalFlag), intent(in) :: lg1, lg2

 ESMF_lgne = (lg1%value .ne. lg2%value)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_DomainTypeFlag types

function ESMF_dmeq(dm1, dm2)
 logical ESMF_dmeq
 type(ESMF_DomainTypeFlag), intent(in) :: dm1, dm2

 ESMF_dmeq = (dm1%value .eq. dm2%value)
end function

function ESMF_dmne(dm1, dm2)
 logical ESMF_dmne
 type(ESMF_DomainTypeFlag), intent(in) :: dm1, dm2

 ESMF_dmne = (dm1%value .ne. dm2%value)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_IndexFlag types

function ESMF_ifeq(if1, if2)
  logical ESMF_ifeq
  type(ESMF_IndexFlag), intent(in) :: if1, if2

  ESMF_ifeq = (if1%i_type .eq. if2%i_type)
end function


!------------------------------------------------------------------------------
! function to compare two ESMF_RegionFlag types

function ESMF_rfeq(rf1, rf2)
  logical ESMF_rfeq
  type(ESMF_RegionFlag), intent(in) :: rf1, rf2

  ESMF_rfeq = (rf1%i_type .eq. rf2%i_type)
end function


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! subroutine to print the corresponding C pointer of ESMF_Pointer object

subroutine ESMF_PointerPrint(ptr)
 type(ESMF_Pointer), intent(in) :: ptr

!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 

  call c_pointerprint(ptr)
end subroutine


      end module ESMF_UtilTypesMod
