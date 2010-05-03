! $Id: ESMF_UtilTypes.F90,v 1.85.2.6 2010/05/03 13:54:45 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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

      integer, parameter :: ESMF_MAJOR_VERSION = 4
      integer, parameter :: ESMF_MINOR_VERSION = 0
      integer, parameter :: ESMF_REVISION      = 0
      integer, parameter :: ESMF_PATCHLEVEL    = 2
      character(*), parameter :: ESMF_VERSION_STRING = "4.0.0rp2"

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
#if (ESMC_POINTER_SIZE == 4)
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
         ESMF_ID_COMMTABLE = ESMF_ObjectID(21, "ESMF_CommTable"), &
         ESMF_ID_ROUTETABLE = ESMF_ObjectID(22, "ESMF_RouteTable"), &
         ESMF_ID_ROUTE = ESMF_ObjectID(23, "ESMF_Route"), &
         ESMF_ID_ROUTEHANDLE = ESMF_ObjectID(24, "ESMF_RouteHandle"), &
         ESMF_ID_FIELDDATAMAP = ESMF_ObjectID(25, "ESMF_FieldDataMap"), &
         ESMF_ID_FIELD = ESMF_ObjectID(26, "ESMF_Field"), &
         ESMF_ID_FIELDBUNDLE = ESMF_ObjectID(28, "ESMF_FieldBundle"), &
         ESMF_ID_GEOMBASE = ESMF_ObjectID(29, "ESMF_GeomBase"), &
         ESMF_ID_REGRID = ESMF_ObjectID(30, "ESMF_Regrid"), &
         ESMF_ID_LOCSTREAM = ESMF_ObjectID(31, "ESMF_Locstream"), &
         ESMF_ID_STATE = ESMF_ObjectID(32, "ESMF_State"), &
         ESMF_ID_GRIDCOMPONENT = ESMF_ObjectID(33, "ESMF_GridComponent"), &
         ESMF_ID_CPLCOMPONENT = ESMF_ObjectID(34, "ESMF_CplComponent"), &
         ESMF_ID_COMPONENT = ESMF_ObjectID(35, "ESMF_Component"), &
         ESMF_ID_NONE = ESMF_ObjectID(99, "ESMF_None")

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
!     ! Typed inquire-only flag

!     ! WARNING: must match corresponding values in ../include/ESMC_Util.h

      type ESMF_InquireFlag
      sequence
      private
          type(ESMF_Logical) :: flag
      end type

      type(ESMF_InquireFlag), parameter :: ESMF_INQUIREONLY = ESMF_InquireFlag(ESMF_TRUE), &
                                           ESMF_NOINQUIRE   = ESMF_InquireFlag(ESMF_FALSE)

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
!     ! ESMF_AttWriteFlag
!
!     ! Interface flag for Attribute write methods

      type ESMF_AttWriteFlag
      sequence
      !private
        integer :: value
      end type

      type(ESMF_AttWriteFlag), parameter ::  &
        ESMF_ATTWRITE_TAB = ESMF_AttWriteFlag(0), &
        ESMF_ATTWRITE_XML = ESMF_AttWriteFlag(1)

!------------------------------------------------------------------------------
!     ! ESMF_AttPackNestFlag
!
!     ! Interface flag for Attribute package nesting

      type ESMF_AttPackNestFlag
      sequence
      !private
        integer :: value
      end type

      type(ESMF_AttPackNestFlag), parameter ::  &
        ESMF_ATTPACKNEST_OFF = ESMF_AttPackNestFlag(0), &
        ESMF_ATTPACKNEST_ON = ESMF_AttPackNestFlag(1)

!------------------------------------------------------------------------------
!     ! ESMF_AttReconcileFlag
!
!     ! Interface flag for Attribute reconcile

      type ESMF_AttReconcileFlag
      sequence
      !private
        integer :: value
      end type

      type(ESMF_AttReconcileFlag), parameter ::  &
        ESMF_ATTRECONCILE_OFF = ESMF_AttReconcileFlag(0), &
        ESMF_ATTRECONCILE_ON = ESMF_AttReconcileFlag(1)

!------------------------------------------------------------------------------
!     ! ESMF_AttCopyFlag
!
!     ! Interface flag for Attribute copy

      type ESMF_AttCopyFlag
      sequence
      !private
        integer :: value
      end type

      type(ESMF_AttCopyFlag), parameter ::  &
        ESMF_ATTCOPY_HYBRID = ESMF_AttCopyFlag(0), &
        ESMF_ATTCOPY_REFERENCE = ESMF_AttCopyFlag(1), &
        ESMF_ATTCOPY_VALUE = ESMF_AttCopyFlag(2)

!------------------------------------------------------------------------------
!     ! ESMF_AttGetCountFlag
!
!     ! Interface flag for Attribute copy

      type ESMF_AttGetCountFlag
      sequence
      !private
        integer :: value
      end type

      type(ESMF_AttGetCountFlag), parameter ::  &
        ESMF_ATTGETCOUNT_ATTRIBUTE = ESMF_AttGetCountFlag(0), &
        ESMF_ATTGETCOUNT_ATTPACK = ESMF_AttGetCountFlag(1), &
        ESMF_ATTGETCOUNT_ATTLINK = ESMF_AttGetCountFlag(2), &
        ESMF_ATTGETCOUNT_TOTAL = ESMF_AttGetCountFlag(3)

!------------------------------------------------------------------------------
!     ! ESMF_AttTreeFlag
!
!     ! Interface flag for Attribute tree

      type ESMF_AttTreeFlag
      sequence
      !private
        integer :: value
      end type

      type(ESMF_AttTreeFlag), parameter ::  &
        ESMF_ATTTREE_OFF = ESMF_AttTreeFlag(0), &
        ESMF_ATTTREE_ON = ESMF_AttTreeFlag(1)

      ! What to do when a point can't be mapped
      type ESMF_UnmappedAction
      sequence
!  private
         integer :: unmappedaction
      end type

      type(ESMF_UnmappedAction), parameter :: &
           ESMF_UNMAPPEDACTION_ERROR    = ESMF_UnmappedAction(0), &
           ESMF_UNMAPPEDACTION_IGNORE   = ESMF_UnmappedAction(1)


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

      public ESMF_InquireFlag
      public ESMF_INQUIREONLY, ESMF_NOINQUIRE

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
      public ESMF_AttCopyFlag, ESMF_ATTCOPY_HYBRID, ESMF_ATTCOPY_REFERENCE, &
                               ESMF_ATTCOPY_VALUE
      public ESMF_AttGetCountFlag, ESMF_ATTGETCOUNT_ATTRIBUTE, ESMF_ATTGETCOUNT_ATTPACK, &
                                   ESMF_ATTGETCOUNT_ATTLINK, ESMF_ATTGETCOUNT_TOTAL
      public ESMF_AttPackNestFlag, ESMF_ATTPACKNEST_OFF, ESMF_ATTPACKNEST_ON
      public ESMF_AttReconcileFlag, ESMF_ATTRECONCILE_OFF, ESMF_ATTRECONCILE_ON
      public ESMF_AttTreeFlag, ESMF_ATTTREE_OFF, ESMF_ATTTREE_ON
      public ESMF_AttWriteFlag, ESMF_ATTWRITE_TAB, ESMF_ATTWRITE_XML

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
      public ESMF_ID_COMMTABLE, ESMF_ID_ROUTETABLE, ESMF_ID_ROUTE
      public ESMF_ID_ROUTEHANDLE, ESMF_ID_FIELDDATAMAP, ESMF_ID_FIELD
      public ESMF_ID_FIELDBUNDLE, ESMF_ID_GEOMBASE
      public ESMF_ID_REGRID, ESMF_ID_LOCSTREAM, ESMF_ID_STATE
      public ESMF_ID_GRIDCOMPONENT, ESMF_ID_CPLCOMPONENT, ESMF_ID_COMPONENT

      public ESMF_Status, ESMF_Pointer, ESMF_TypeKind
      public ESMF_DataValue

      public ESMF_PointerPrint
      
       public ESMF_UnmappedAction, ESMF_UNMAPPEDACTION_ERROR, &
                                   ESMF_UNMAPPEDACTION_IGNORE

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
  module procedure ESMF_bfeq
  module procedure ESMF_ctfeq
  module procedure ESMF_tnfeq
  module procedure ESMF_freq
  module procedure ESMF_ifeq
  module procedure ESMF_rfeq
  module procedure ESMF_unmappedActioneq
end interface

interface operator (.ne.)
  module procedure ESMF_sfne
  module procedure ESMF_dkne
  module procedure ESMF_ptne
  module procedure ESMF_tfne
  module procedure ESMF_bfne
  module procedure ESMF_ctfne
  module procedure ESMF_tnfne
  module procedure ESMF_frne
  module procedure ESMF_unmappedActionne
end interface

interface assignment (=)
  module procedure ESMF_bfas
  module procedure ESMF_dkas
  module procedure ESMF_tfas
  module procedure ESMF_tfas_v
  module procedure ESMF_tfas2
  module procedure ESMF_tfas2_v
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

subroutine ESMF_tfas_v(lval, tfval)
 logical, intent(out) :: lval(:)
 type(ESMF_Logical), intent(in) :: tfval(:)

 lval = (tfval%value .eq. 1)    ! this must match initializer
end subroutine

subroutine ESMF_tfas2 (tfval, lval)
 type(ESMF_Logical), intent(out) :: tfval
 logical, intent(in) :: lval

 tfval = merge (ESMF_TRUE, ESMF_FALSE, lval)
end subroutine

subroutine ESMF_tfas2_v (tfval, lval)
 type(ESMF_Logical), intent(out) :: tfval(:)
 logical, intent(in) :: lval(:)

 tfval = merge (ESMF_TRUE, ESMF_FALSE, lval)
end subroutine

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

  call c_pointerprint(ptr)
end subroutine

!------------------------------------------------------------------------------
! function to compare two ESMF_UNMAPPEDACTION types

function ESMF_unmappedActioneq(uma1, uma2)
 logical ESMF_unmappedActioneq
 type(ESMF_UNMAPPEDACTION), intent(in) :: uma1, uma2

 ESMF_unmappedActioneq = (uma1%unmappedaction .eq. uma2%unmappedaction)
end function

function ESMF_unmappedActionne(uma1, uma2)
 logical ESMF_unmappedActionne
 type(ESMF_UNMAPPEDACTION), intent(in) :: uma1, uma2

 ESMF_unmappedActionne = (uma1%unmappedaction .ne. uma2%unmappedaction)
end function


      end module ESMF_UtilTypesMod
