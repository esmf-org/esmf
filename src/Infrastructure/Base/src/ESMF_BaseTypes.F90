! $Id: ESMF_BaseTypes.F90,v 1.12 2004/12/15 17:19:21 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
#define ESMF_FILENAME "ESMF_BaseTypes.F90"

!
! ESMF BaseTypes Module
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

      module ESMF_BaseTypesMod
 
#include "ESMF.h"

!BOPI
! !MODULE: ESMF_BaseMod - Base class for all ESMF classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation 
!  in the ../src dir.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
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
      integer, parameter :: ESMF_MAXDIM = 7, &
                            ESMF_MAXDECOMPDIM = 3, &
                            ESMF_MAXGRIDDIM = 3
     
!EOPI

      integer, parameter :: ESMF_MAJOR_VERSION = 2
      integer, parameter :: ESMF_MINOR_VERSION = 1
      integer, parameter :: ESMF_REVISION      = 0
      integer, parameter :: ESMF_PATCHLEVEL    = 0
      character(8), parameter :: ESMF_VERSION_STRING = "2.1.0"

!------------------------------------------------------------------------------
!
!    ! General object status, useful for any object

!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMC_Base.h

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
                                      ESMF_STATUS_INVALID = ESMF_Status(6)
 
!------------------------------------------------------------------------------
!
!    ! Generic pointer, large enough to hold a pointer on any architecture,
!    ! but not useful directly in fortran.  Expected to be used where a
!    ! pointer generated in C++ needs to be stored on the Fortran side.

!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMC_Base.h

      type ESMF_Pointer
      sequence
      !private
#ifdef S32
          integer*4 :: ptr
#else
          integer*8 :: ptr
#endif
      end type

      type(ESMF_Pointer), parameter :: ESMF_NULL_POINTER = ESMF_Pointer(0), &
                                       ESMF_BAD_POINTER = ESMF_Pointer(-1)


!------------------------------------------------------------------------------
!
!    ! Data type - does not specify size (*4, *8, short, long) since KIND
!    ! takes care of that.

!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMC_Base.h

      type ESMF_DataType
      sequence
      ! TODO: can this be made private now?
      !!private
          integer :: dtype
      end type

      type(ESMF_DataType), parameter :: ESMF_DATA_INTEGER = ESMF_DataType(1), &
                                        ESMF_DATA_REAL = ESMF_DataType(2), &
                                        ESMF_DATA_LOGICAL = ESMF_DataType(3), &
                                        ESMF_DATA_CHARACTER = ESMF_DataType(4), &
                                        ESMF_DATA_COMPLEX = ESMF_DataType(5)

!------------------------------------------------------------------------------
!
!    ! Where we can use a derived type, the compiler will help do 
!    ! typechecking.  For those places where the compiler refuses to allow
!    ! anything but an Integer data type, use the second set of constants.

!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMC_Base.h

      type ESMF_DataKind
      sequence
      ! TODO: can this be made private now?
      !!private
        integer :: dkind
      end type

      ! these work well for internal ESMF use, arguments, etc
      type(ESMF_DataKind), parameter :: &
                   ESMF_I1 = ESMF_DataKind(1), &
                   ESMF_I2 = ESMF_DataKind(2), &
                   ESMF_I4 = ESMF_DataKind(3), &
                   ESMF_I8 = ESMF_DataKind(4), &
                   ESMF_R4 = ESMF_DataKind(5), &
                   ESMF_R8 = ESMF_DataKind(6), &
                   ESMF_C8 = ESMF_DataKind(7), &
                   ESMF_C16 = ESMF_DataKind(8), &
                   ESMF_NOKIND = ESMF_DataKind(99)

      ! these work where you have to declare an array or something that
      ! the compiler needs to have a fixed 'kind' for.
      integer, parameter :: &
                   ESMF_KIND_I1 = selected_int_kind(2), &
                   ESMF_KIND_I2 = selected_int_kind(4), &
                   ESMF_KIND_I4 = selected_int_kind(9), &
                   ESMF_KIND_I8 = selected_int_kind(18), &
                   ESMF_KIND_R4 = selected_real_kind(3,25), &
                   ESMF_KIND_R8 = selected_real_kind(6,45), &
                   ESMF_KIND_C8 = selected_real_kind(3,25), &
                   ESMF_KIND_C16 = selected_real_kind(6,45)

!------------------------------------------------------------------------------
!
!    ! Dummy structure which must just be big enough to hold the values.
!    ! actual data values will always be accessed on the C++ side.

      type ESMF_DataValue
      sequence
      private
          type(ESMF_DataType) :: dt
          integer :: items
          type(ESMF_Pointer) :: value
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
         ESMF_ID_ARRAYDATAMAP = ESMF_ObjectID(11, "ESMF_ArrayDataMap"), &
         ESMF_ID_VM = ESMF_ObjectID(12, "ESMF_VM"), &
         ESMF_ID_DELAYOUT = ESMF_ObjectID(13, "ESMF_DELayout"), &
         ESMF_ID_CONFIG = ESMF_ObjectID(14, "ESMF_Config"), &
         ESMF_ID_PERFPROF = ESMF_ObjectID(15, "ESMF_PerfProf"), &
         ESMF_ID_ARRAY = ESMF_ObjectID(16, "ESMF_Array"), &
         ESMF_ID_DISTGRID = ESMF_ObjectID(17, "ESMF_DistGrid"), &
         ESMF_ID_PHYSGRID = ESMF_ObjectID(18, "ESMF_PhysGrid"), &
         ESMF_ID_GRID = ESMF_ObjectID(19, "ESMF_Grid"), &
         ESMF_ID_EXCHANGEPACKET = ESMF_ObjectID(20, "ESMF_ExchangePacket"), &
         ESMF_ID_COMMTABLE = ESMF_ObjectID(21, "ESMF_CommTable"), &
         ESMF_ID_ROUTETABLE = ESMF_ObjectID(22, "ESMF_RouteTable"), &
         ESMF_ID_ROUTE = ESMF_ObjectID(23, "ESMF_Route"), &
         ESMF_ID_ROUTEHANDLE = ESMF_ObjectID(24, "ESMF_RouteHandle"), &
         ESMF_ID_FIELDDATAMAP = ESMF_ObjectID(25, "ESMF_FieldDataMap"), &
         ESMF_ID_FIELD = ESMF_ObjectID(26, "ESMF_Field"), &
         ESMF_ID_BUNDLEDATAMAP = ESMF_ObjectID(27, "ESMF_BundleDataMap"), &
         ESMF_ID_BUNDLE = ESMF_ObjectID(28, "ESMF_Bundle"), &
         ESMF_ID_TRANSFORMVALUES = ESMF_ObjectID(29, "ESMF_TransformValues"), &
         ESMF_ID_REGRID = ESMF_ObjectID(30, "ESMF_Regrid"), &
         ESMF_ID_TRANSFORM = ESMF_ObjectID(31, "ESMF_Transform"), &
         ESMF_ID_STATE = ESMF_ObjectID(32, "ESMF_State"), &
         ESMF_ID_GRIDCOMPONENT = ESMF_ObjectID(33, "ESMF_GridComponent"), &
         ESMF_ID_CPLCOMPONENT = ESMF_ObjectID(34, "ESMF_CplComponent"), &
         ESMF_ID_COMPONENT = ESMF_ObjectID(35, "ESMF_Component"), &
         ESMF_ID_NONE = ESMF_ObjectID(99, "ESMF_None")

!------------------------------------------------------------------------------
!
!    ! Dummy structure which must just be big enough to hold the values.
!    ! actual data values will always be accessed on the C++ side.

      type ESMF_Attribute
      sequence
      private
          character (len=ESMF_MAXSTR) :: attr_name
          type (ESMF_DataValue) :: attr_value
      end type

!------------------------------------------------------------------------------
!
      type ESMF_AxisIndex
      sequence
          integer :: min
          integer :: max
          integer :: stride
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
      end type

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
!     ! Typed true/false values which are not compiler dependent, so that
!     ! when crossing the Fortran/C++ language boundary with logical values we
!     ! have a consistent interpretation.  In C/C++ 0=false, 1=true, but this
!     ! is not defined for Fortran and different compilers use different values
!     ! for booleans.

!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h

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

!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h

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
        ESMF_NONBLOCKING  = ESMF_BlockingFlag(2)

!------------------------------------------------------------------------------
!
      ! Contains pointer to real Base object which is defined in C++

      type ESMF_Base
      sequence
      !private
#ifndef ESMF_NO_INITIALIZERS
         type(ESMF_Pointer) :: this = ESMF_NULL_POINTER
#else
         type(ESMF_Pointer) :: this
#endif
     end type

!------------------------------------------------------------------------------
!BOPI
!
! !PUBLIC TYPES:

      public ESMF_STATUS_UNINIT, ESMF_STATUS_READY, &
             ESMF_STATUS_UNALLOCATED, ESMF_STATUS_ALLOCATED, &
             ESMF_STATUS_BUSY, ESMF_STATUS_INVALID

      public ESMF_DATA_INTEGER, ESMF_DATA_REAL, &
             ESMF_DATA_LOGICAL, ESMF_DATA_CHARACTER, ESMF_DATA_COMPLEX

      public ESMF_I1, ESMF_I2, ESMF_I4, ESMF_I8, & 
             ESMF_R4, ESMF_R8, ESMF_C8, ESMF_C16, ESMF_NOKIND

      public ESMF_KIND_I1, ESMF_KIND_I2, ESMF_KIND_I4, ESMF_KIND_I8, & 
             ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_C8, ESMF_KIND_C16

      public ESMF_NULL_POINTER, ESMF_BAD_POINTER

      public ESMF_Logical, ESMF_TRUE, ESMF_FALSE

      public ESMF_ReduceFlag, ESMF_SUM, ESMF_MIN, ESMF_MAX
      public ESMF_BlockingFlag, ESMF_BLOCKING, ESMF_NONBLOCKING

      public ESMF_FAILURE, ESMF_SUCCESS
      public ESMF_MAXSTR
      public ESMF_MAXDIM, ESMF_MAXDECOMPDIM, ESMF_MAXGRIDDIM
     
      public ESMF_MAJOR_VERSION, ESMF_MINOR_VERSION
      public ESMF_REVISION, ESMF_PATCHLEVEL
      public ESMF_VERSION_STRING 

      public ESMF_ObjectID, ESMF_ID_NONE
      public ESMF_ID_BASE, ESMF_ID_IOSPEC, ESMF_ID_LOGERR, ESMF_ID_TIME
      public ESMF_ID_CALENDAR, ESMF_ID_TIMEINTERVAL, ESMF_ID_ALARM
      public ESMF_ID_CLOCK, ESMF_ID_ARRAYSPEC, ESMF_ID_LOCALARRAY
      public ESMF_ID_ARRAYDATAMAP, ESMF_ID_VM, ESMF_ID_DELAYOUT
      public ESMF_ID_CONFIG, ESMF_ID_PERFPROF, ESMF_ID_ARRAY, ESMF_ID_DISTGRID
      public ESMF_ID_PHYSGRID, ESMF_ID_GRID, ESMF_ID_EXCHANGEPACKET
      public ESMF_ID_COMMTABLE, ESMF_ID_ROUTETABLE, ESMF_ID_ROUTE
      public ESMF_ID_ROUTEHANDLE, ESMF_ID_FIELDDATAMAP, ESMF_ID_FIELD
      public ESMF_ID_BUNDLEDATAMAP, ESMF_ID_BUNDLE, ESMF_ID_TRANSFORMVALUES
      public ESMF_ID_REGRID, ESMF_ID_TRANSFORM, ESMF_ID_STATE
      public ESMF_ID_GRIDCOMPONENT, ESMF_ID_CPLCOMPONENT, ESMF_ID_COMPONENT

      public ESMF_Base
      public ESMF_Status, ESMF_Pointer, ESMF_DataType, ESMF_DataKind
      public ESMF_DataValue, ESMF_Attribute
      public ESMF_Domain, ESMF_DomainList
      public ESMF_AxisIndex

      public ESMF_PointerPrint
      
!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!

!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare 
!  them as if they were simple integers.
 

interface operator (.eq.)
 module procedure ESMF_sfeq
 module procedure ESMF_dteq
 module procedure ESMF_dkeq
 module procedure ESMF_pteq
 module procedure ESMF_tfeq
 module procedure ESMF_aieq
 module procedure ESMF_bfeq
end interface

interface operator (.ne.)
 module procedure ESMF_sfne
 module procedure ESMF_dtne
 module procedure ESMF_dkne
 module procedure ESMF_ptne
 module procedure ESMF_tfne
 module procedure ESMF_aine
 module procedure ESMF_bfne
end interface

interface assignment (=)
 module procedure ESMF_dtas
 module procedure ESMF_dkas
 module procedure ESMF_tfas
 module procedure ESMF_ptas
 module procedure ESMF_ptas2
end interface

!------------------------------------------------------------------------------

      contains

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
! function to compare two ESMF_DataTypes to see if they're the same or not

function ESMF_dteq(dt1, dt2)
 logical ESMF_dteq
 type(ESMF_DataType), intent(in) :: dt1, dt2

 ESMF_dteq = (dt1%dtype .eq. dt2%dtype)
end function

function ESMF_dtne(dt1, dt2)
 logical ESMF_dtne
 type(ESMF_DataType), intent(in) :: dt1, dt2

 ESMF_dtne = (dt1%dtype .ne. dt2%dtype)
end function

subroutine ESMF_dtas(intval, dtval)
 integer, intent(out) :: intval
 type(ESMF_DataType), intent(in) :: dtval

 intval = dtval%dtype
end subroutine

!------------------------------------------------------------------------------
! function to compare two ESMF_DataKinds to see if they're the same or not

function ESMF_dkeq(dk1, dk2)
 logical ESMF_dkeq
 type(ESMF_DataKind), intent(in) :: dk1, dk2

 ESMF_dkeq = (dk1%dkind .eq. dk2%dkind)
end function

function ESMF_dkne(dk1, dk2)
 logical ESMF_dkne
 type(ESMF_DataKind), intent(in) :: dk1, dk2

 ESMF_dkne = (dk1%dkind .ne. dk2%dkind)
end function

subroutine ESMF_dkas(intval, dkval)
 integer, intent(out) :: intval
 type(ESMF_DataKind), intent(in) :: dkval

 intval = dkval%dkind
end subroutine


!------------------------------------------------------------------------------
! function to compare two ESMF_BlockingFlags

function ESMF_bfeq(bf1, bf2)
 logical ESMF_bfeq
 type(ESMF_BlockingFlag), intent(in) :: bf1, bf2

! if (bf1%value .eq. bf2%value) then
!   ESMF_bfeq = .true.
! else 
!   ESMF_bfeq = .false.
! endif
 ESMF_bfeq = (bf1%value .eq. bf2%value)
end function

function ESMF_bfne(bf1, bf2)
 logical ESMF_bfne
 type(ESMF_BlockingFlag), intent(in) :: bf1, bf2

 ESMF_bfne = (bf1%value .ne. bf2%value)
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

 ESMF_aieq = ((ai1%min .eq. ai2%min) .and. &
              (ai1%max .eq. ai2%max) .and. &
              (ai1%stride .eq. ai2%stride))

end function

function ESMF_aine(ai1, ai2)
 logical ESMF_aine
 type(ESMF_AxisIndex), intent(in) :: ai1, ai2

 ESMF_aine = ((ai1%min .ne. ai2%min) .or. &
              (ai1%max .ne. ai2%max) .or. &
              (ai1%stride .ne. ai2%stride))

end function

!------------------------------------------------------------------------------
! subroutine to print the corresponding C pointer of ESMF_Pointer object

subroutine ESMF_PointerPrint(ptr)
 type(ESMF_Pointer), intent(in) :: ptr

  call c_pointerprint(ptr)
end subroutine


      end module ESMF_BaseTypesMod
