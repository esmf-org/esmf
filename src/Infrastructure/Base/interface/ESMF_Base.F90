! $Id: ESMF_Base.F90,v 1.90 2004/03/19 15:13:47 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Base Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! module definition

      module ESMF_BaseMod
 
#include "ESMF.h"

!BOPI
! !MODULE: ESMF_BaseMod - Base class for all ESMF classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation in the ../src dir.
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

      integer, parameter :: ESMF_MAJOR_VERSION = 1
      integer, parameter :: ESMF_MINOR_VERSION = 0
      integer, parameter :: ESMF_REVISION      = 6
      integer, parameter :: ESMF_PATCHLEVEL    = 0
      character(8), parameter :: ESMF_VERSION_STRING = "1.0.6"

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

      type(ESMF_Status), parameter :: ESMF_STATE_UNINIT = ESMF_Status(1), &
                                      ESMF_STATE_READY = ESMF_Status(2), &
                                      ESMF_STATE_UNALLOCATED = ESMF_Status(3), &
                                      ESMF_STATE_ALLOCATED = ESMF_Status(4), &
                                      ESMF_STATE_BUSY = ESMF_Status(5), &
                                      ESMF_STATE_INVALID = ESMF_Status(6)
 
!------------------------------------------------------------------------------
!
!    ! Generic pointer, large enough to hold a pointer on any architecture,
!    ! but not useful directly in fortran.  Expected to be used where a
!    ! pointer generated in C++ needs to be stored on the F90 side.

!     ! WARNING: 
!     !  constants MUST match corresponding values in ../include/ESMC_Base.h

      type ESMF_Pointer
      sequence
      private
          integer*8 :: ptr
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
                                        ESMF_DATA_CHARACTER = ESMF_DataType(4)

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
                   ESMF_C16 = ESMF_DataKind(8)

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
      ! possible new type
      type ESMF_Domain
      sequence
          integer :: DE
          integer :: rank
          ! TODO:  add an element for size in points (memory)
          type (ESMF_AxisIndex) :: ai(ESMF_MAXDIM)
      end type

!------------------------------------------------------------------------------
!
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
!     ! when crossing the F90/C++ language boundary with logical values we
!     ! have a consistent interpretation.  In C/C++ 0=false, 1=true, but this
!     ! is not defined for F90 and different compilers use different values
!     ! for booleans.

!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h

      type ESMF_Logical
      sequence
      private
          integer :: value
      end type

      type(ESMF_Logical), parameter :: ESMF_TRUE     = ESMF_Logical(1), &
                                       ESMF_FALSE    = ESMF_Logical(2)

!#ifdef ESMF_ENABLE_VM
!------------------------------------------------------------------------------
!
!     ! Typed reduction operations

!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h

      type ESMF_newOp
      sequence
      private
          integer :: value
      end type

      type(ESMF_newOp), parameter :: ESMF_newSUM   = ESMF_newOp(1), &
                                     ESMF_newMIN   = ESMF_newOp(2), &
                                     ESMF_newMAX   = ESMF_newOp(3)
!#endif
                                     
!------------------------------------------------------------------------------
!
      ! Contains pointer to real Base object which is defined in C++

      type ESMF_Base
      sequence
      private
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

      public ESMF_STATE_UNINIT, ESMF_STATE_READY, &
             ESMF_STATE_UNALLOCATED, ESMF_STATE_ALLOCATED, &
             ESMF_STATE_BUSY, ESMF_STATE_INVALID

      public ESMF_DATA_INTEGER, ESMF_DATA_REAL, &
             ESMF_DATA_LOGICAL, ESMF_DATA_CHARACTER

      public ESMF_I1, ESMF_I2, ESMF_I4, ESMF_I8, & 
             ESMF_R4, ESMF_R8, ESMF_C8, ESMF_C16

      public ESMF_KIND_I1, ESMF_KIND_I2, ESMF_KIND_I4, ESMF_KIND_I8, & 
             ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_C8, ESMF_KIND_C16

      public ESMF_NULL_POINTER, ESMF_BAD_POINTER

      public ESMF_Logical, ESMF_TRUE, ESMF_FALSE

!#ifdef ESMF_ENABLE_VM
      public ESMF_newOp, ESMF_newSUM, ESMF_newMIN, ESMF_newMAX
!#endif

      public ESMF_FAILURE, ESMF_SUCCESS
      public ESMF_MAXSTR
      public ESMF_MAXDIM, ESMF_MAXDECOMPDIM, ESMF_MAXGRIDDIM
     
      public ESMF_MAJOR_VERSION, ESMF_MINOR_VERSION
      public ESMF_REVISION, ESMF_PATCHLEVEL
      public ESMF_VERSION_STRING 

      public ESMF_Base
      public ESMF_Status, ESMF_Pointer, ESMF_DataType, ESMF_DataKind
      public ESMF_DataValue, ESMF_Attribute
      public ESMF_Domain, ESMF_DomainList
      public ESMF_AxisIndex

!EOPI
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
! !PUBLIC MEMBER FUNCTIONS:
!
!   Base class methods
       public ESMF_BaseCreate
       public ESMF_BaseDestroy
   
!      public ESMF_BaseGetInstCount

!      public ESMF_BaseSetID
!      public ESMF_BaseGetID

!      public ESMF_BaseSetRefCount
!      public ESMF_BaseGetRefCount

!      public ESMF_BaseSetStatus
!      public ESMF_BaseGetStatus

       public ESMF_BasePrint

!   Virtual methods to be defined by derived classes
!      public ESMF_Read
!      public ESMF_Write
!      public ESMF_Validate
!      public ESMF_Print

!  Attribute methods
      public ESMF_AttributeSet
      public ESMF_AttributeGet
      public ESMF_AttributeGetCount
      public ESMF_AttributeGetbyNumber
      public ESMF_AttributeGetNameList
      public ESMF_AttributeSetList
      public ESMF_AttributeGetList
      public ESMF_AttributeSetObjectList
      public ESMF_AttributeGetObjectList
      public ESMF_AttributeCopy
      public ESMF_AttributeCopyAll

! DomainList methods
      public ESMF_DomainListCreate
      public ESMF_DomainListDestroy
      public ESMF_DomainListPrint
      public ESMF_DomainListAdd
 
! AxisIndex methods
      public ESMF_AxisIndexSet
      public ESMF_AxisIndexGet

!  Misc methods
      public ESMF_SetName
      public ESMF_GetName
      public ESMF_SetPointer
      public ESMF_SetNullPointer
      public ESMF_GetPointer

!  Print methods for calling by higher level print functions
!  (they have little formatting other than the actual values)
      public ESMF_StatusString, ESMF_DataTypeString
      public ESMF_DataKindString, ESMF_LogicalString

!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!
!

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_DomainListAdd

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DomainListAdd2d
         module procedure ESMF_DomainListAdd3d
         module procedure ESMF_DomainListAddObj
!

! !DESCRIPTION:
!     These functions are meant to ease the task of creating multidimensional
!     domains.
!
!EOPI
      end interface 


!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Base.F90,v 1.90 2004/03/19 15:13:47 theurich Exp $'
!------------------------------------------------------------------------------
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
end interface

interface operator (.ne.)
 module procedure ESMF_sfne
 module procedure ESMF_dtne
 module procedure ESMF_dkne
 module procedure ESMF_ptne
 module procedure ESMF_tfne
 module procedure ESMF_aine
end interface

interface assignment (=)
 module procedure ESMF_dtas
 module procedure ESMF_dkas
 module procedure ESMF_tfas
 module procedure ESMF_ptas
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
!------------------------------------------------------------------------------
! 
! Base methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_BaseCreate - Create and initialize a Base object
!
! !INTERFACE:
      subroutine ESMF_BaseCreate(base, superclass, name, nattr, rc)
!
! !ARGUMENTS:
      type(ESMF_Base) :: base                 
      character(len=*), intent(in) :: superclass
      character(len=*), intent(in), optional :: name
      integer, intent(in), optional :: nattr 
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Set initial state on a Base object.
!
!     \begin{description}
!     \item [base]
!           An {\tt ESMF\_Base} derived type.  It is expected that all 
!           specialized derived types will include an {\tt ESMF\_Base} 
!           object as the first entry.
!     \item [superclass]
!           The name of the superclass, e.g. {\tt Grid}, {\tt Array}.
!           This sets the scope for unique object names.
!     \item [{[name]}]
!           If given, the unique name for this object.  If not given,
!           a unique name will be generated.  
!     \item [{[nattr]}]
!           If given, the initial number of attributes to allocate space for.
!           Additional attributes can be added at any time, but it will be
!           more efficient if space is allocated at create time.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI

      logical :: rcpresent                          ! Return code present   
      integer :: status, allocNAttrs

!     ! Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      allocNAttrs = 0   ! default value, overwrite if argument specified
      if (present(nattr)) allocNAttrs = nattr

      if (present(name)) then
          call c_ESMC_BaseCreate(base, superclass, name, allocNattrs, status)
      else
          !!call c_ESMC_BaseCreate(base, superclass, ESMF_NULL_POINTER, &
          call c_ESMC_BaseCreate(base, superclass, "", allocNattrs, status)
      endif

      if (rcpresent) rc = status

      end subroutine ESMF_BaseCreate

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_BaseDestroy - Release resources from a Base object
!
! !INTERFACE:
      subroutine ESMF_BaseDestroy(base, rc)
!
! !ARGUMENTS:
      type(ESMF_Base) :: base                 
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Release resources held by a Base object.
!
!     \begin{description}
!     \item [base]
!           An {\tt ESMF\_Base} derived type to be deleted.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      logical :: rcpresent                          ! Return code present   
      integer :: status

!     ! Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      call c_ESMC_BaseDestroy(base, status)

      if (rcpresent) rc = status

      end subroutine ESMF_BaseDestroy

!-------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_AttributeSet - set attribute on an ESMF type
!
! !INTERFACE:
      subroutine ESMF_AttributeSet(base, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: base
      character (len = *), intent(in) :: name 
      type(ESMF_DataValue), intent(in) :: value 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.
!
!     The arguments are:
!     \begin{description}
!
!     \item[base]
!       Any ESMF type.
!
!     \item[name]
!       The name of the attribute to set.
!
!     \item[value]
!       The value of the attribute.
!
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      integer :: status 

      status = ESMF_FAILURE
      !call c_ESMC_AttributeSet(base, name, value, status) 
      if (present(rc)) rc = status

      end subroutine ESMF_AttributeSet


!-------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_AttributeGet - get attribute from an ESMF type
!
! !INTERFACE:
      subroutine ESMF_AttributeGet(base, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: base 
      character (len = *), intent(in) :: name 
      type(ESMF_DataValue), intent(out) :: value 
      integer, intent(out), optional :: rc 

!
! !DESCRIPTION: Get a (name,value) pair with any type in the system.
!
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[name]
!       The name of the attribute to get.
!     \item[value]
!       The value of the attribute.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3

      integer :: status 

      status = ESMF_FAILURE
      !call c_ESMC_AttributeGet(base, name, value, status) 
      if (present(rc)) rc = status

      end subroutine ESMF_AttributeGet


!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AttributeGetCount - get an ESMF object's number of attributes
!
! !INTERFACE:
      subroutine ESMF_AttributeGetCount(anytype, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Returns number of attributes present.
!
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[count]
!       The number of attributes.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:  FLD1.7.5

      end subroutine ESMF_AttributeGetCount


!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AttributeGetbyNumber - get an ESMF object's attribute by number
!
! !INTERFACE:
      subroutine ESMF_AttributeGetbyNumber(anytype, number, name, type, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype
      integer, intent(in) :: number
      character (len = *), intent(in) :: name
      type(ESMF_DataType), intent(out) :: type
      type(ESMF_DataValue), intent(out) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[number]
!       The attribute number.
!     \item[name]
!       The attribute name.
!     \item[type]
!       The attribute datatype.
!     \item[type]
!       The attribute value.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!
!EOPI
! !REQUIREMENTS: 

      end subroutine ESMF_AttributeGetbyNumber


!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AttributeGetNameList - get an ESMF object's attribute name list
!
! !INTERFACE:
      subroutine ESMF_AttributeGetNameList(anytype, count, namelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype
      integer, intent(out) :: count
      character (len = *), dimension (:), intent(inout) :: namelist
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[count]
!       The number of attributes.
!     \item[namelist]
!       The list of attribute names.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD1.7.3

      !TODO: when code added here, change (inout) for namelist to just out.
      ! absoft compiler was unhappy.

      end subroutine ESMF_AttributeGetNameList


!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AttributeSetList - set an ESMF object's attributes 
!
! !INTERFACE:
      subroutine ESMF_AttributeSetList(anytype, namelist, valuelist, rc)

!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype
      character (len = *), dimension (:), intent(in) :: namelist
      type(ESMF_DataValue), dimension (:), intent(in) :: valuelist
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[namelist]
!       The list of attribute names.
!     \item[valuelist]
!       The list of attribute values.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  (none.  added for completeness)

      end subroutine ESMF_AttributeSetList


!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AttributeGetList - get an ESMF object's attributes
!
! !INTERFACE:
      subroutine ESMF_AttributeGetList(anytype, namelist, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype
      character (len = *), dimension (:), intent(in) :: namelist
      type(ESMF_DataType), dimension (:), intent(out) :: typelist
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.
!
!
!     The arguments are:
!     \begin{description}
!     \item[anytype]
!       Any ESMF type.
!     \item[namelist]
!       The list of attribute names.
!     \item[typelist]
!       The list of attribute types.
!     \item[valuelist]
!       The list of attribute values.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD1.7.4

      end subroutine ESMF_AttributeGetList


!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AttributeSetObjectList - set an attribute on multiple ESMF objects 
!
! !INTERFACE:
      subroutine ESMF_AttributeSetObjectList(anytypelist, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist
      character (len = *), intent(in) :: name
      type(ESMF_DataValue), dimension (:), intent(in) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.
!
!     The arguments are:
!     \begin{description}
!     \item[anytypelist]
!       A list of any ESMF types.
!     \item[name]
!       The attribute name.
!     \item[typelist]
!       The attribute value.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine ESMF_AttributeSetObjectList


!-------------------------------------------------------------------------
!BOPI
!
!
! !IROUTINE:  ESMF_AttributeGetObjectList - get an attribute from multiple ESMF objects 
!
! !INTERFACE:
      subroutine ESMF_AttributeGetObjectList(anytypelist, name, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist
      character (len = *), intent(in) :: name
      type(ESMF_DataType), dimension (:), intent(out) :: typelist
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.
!
!     The arguments are:
!     \begin{description}
!     \item[anytypelist]
!       The list of any ESMF types.
!     \item[name]
!       The attribute name.
!     \item[typelist]
!       The list of all possible data types.
!     \item[valuelist]
!       The list of attribute values.
!     \item[{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine ESMF_AttributeGetObjectList


!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
      subroutine ESMF_AttributeCopy(name, source, destination, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_Base), intent(in) :: source
      type(ESMF_Base), intent(in) :: destination
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>
! 
!     The arguments are:
!     \begin{description}
!     \item[name]
!       The attribute name.
!     \item[source]
!       The source ESMF object.
!     \item[destination]
!       The destination ESMF object.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD1.5.4

      end subroutine ESMF_AttributeCopy


!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AttributeCopyAll - copy attributes between two objects

!
! !INTERFACE:
      subroutine ESMF_AttributeCopyAll(source, destination, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: source
      type(ESMF_Base), intent(in) :: destination
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)
!
!
!     The arguments are:
!     \begin{description}
!     \item[source]
!       The source ESMF object.
!     \item[destination]
!       The destination ESMF object.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD1.5.4

      end subroutine ESMF_AttributeCopyAll

!-------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_SetName - set the name of this object
!
! !INTERFACE:
      subroutine ESMF_SetName(base, name, namespace, rc)
!
! !ARGUMENTS:
      type(ESMF_Base) :: base                 
      character (len = *), intent(in), optional :: name   
      character (len = *), intent(in), optional :: namespace
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Associate a name with any object in the system.
!
!     \begin{description}
!     \item [base]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [{[name]}]
!           Object name.  An error will be returned if a duplicate name 
!           is specified.  If a name is not given a unique name will be
!           generated and can be queried by the {\tt ESMF\_GetName} routine.
!     \item [{[namespace]}]
!           Object namespace (e.g. "Application", "Component", "Grid", etc).
!           If given, the name will be checked that it is unique within
!           this namespace.  If not given, the generated name will be 
!           unique within this namespace.  If namespace is not specified,
!           a default "global" namespace will be assumed and the same rules
!           for names will be followed.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! 
!EOPI
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
      logical :: rcpresent                          ! Return code present   
      integer :: status

      ! Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: remove this once everyone is initializing their Base objects.
      ! cheat for old code for now.
      if (base%this .eq. ESMF_NULL_POINTER) then
          call ESMF_BaseCreate(base, namespace, name, 0, status)
          if (rcpresent) rc = status
          return
      endif
      ! end cheat

      call c_ESMC_SetName(base, namespace, name, status)

      if (rcpresent) rc = status

      end subroutine ESMF_SetName

!-------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_GetName - get the name of this object
!
! !INTERFACE:
      subroutine ESMF_GetName(base, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: base
      character (len = *), intent(out) :: name
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Return the name of any type in the system.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[name]
!       The name of the ESMF type.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
      integer :: status

      call c_ESMC_GetF90Name(base, name, status)
      if (present(rc)) rc = status

      end subroutine ESMF_GetName


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Print routine
!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_BasePrint - call into C++ code to print base object

!
! !INTERFACE:
      subroutine ESMF_BasePrint(base, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: base
      character(len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!  Interface to call through to C++ and print base object values.
!
!     The arguments are:
!     \begin{description}
!     \item[base]
!       Any ESMF type.
!     \item[options]
!       Print options.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:  FLD1.5.4
      integer :: status
      character(len=ESMF_MAXSTR) :: opts

      if (present(options)) then
          opts = options
      else
          opts = ''
      endif

      if (base%this .eq. ESMF_NULL_POINTER) then
         print *, " Uninitialized Base object"
         return
      endif

      call c_ESMC_BasePrint(base, opts, status)
      if (present(rc)) rc = status

      end subroutine ESMF_BasePrint

!=========================================================================
! Domain List routines.
!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_DomainListCreate - create domain list

!
! !INTERFACE:
      function ESMF_DomainListCreate(num_domains)
! !RETURN VALUE:
      type(ESMF_DomainList) :: ESMF_DomainListCreate
!
! !ARGUMENTS:
      integer :: num_domains

!
! !DESCRIPTION:
! Create a domain list.  Initializes the array of domains.  Preallocates
! some storage, hopefully enough.
!
!     The arguments are:
!     \begin{description}
!     \item[num_domains]
!	A suggestion on the number of domains the object will hold.
!     \end{description}
!
!EOPI
      integer :: status
      type(ESMF_Domain), dimension(:), pointer :: domains

! Allocate an array of domains of specified size
      allocate(domains(num_domains), stat=status)

! Initialize values and attach domains to the list
      ESMF_DomainListCreate%num_domains  = num_domains
      ESMF_DomainListCreate%current_size = num_domains
      ESMF_DomainListCreate%total_points = 0
      ESMF_DomainListCreate%domains      => domains

      end function ESMF_DomainListCreate

!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_DomainListDestroy - destroy domain list

!
! !INTERFACE:
      subroutine ESMF_DomainListDestroy(domainlist)
!
! !ARGUMENTS:
      type(ESMF_DomainList) :: domainlist
!
! !DESCRIPTION:
! Deallocate memory used by creation routine.
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       A list of domains to destroy.
!     \end{description}
!
!EOPI
      integer :: status

      deallocate(domainlist%domains, stat=status)

      end subroutine ESMF_DomainListDestroy

!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_DomainListPrint - print domain list

!
! !INTERFACE:
      subroutine ESMF_DomainListPrint(domainlist)
!
! !ARGUMENTS:
      type(ESMF_DomainList) :: domainlist
!
! !DESCRIPTION:
! Dump the contents of a domain list to screen, i.e. for 
! debugging during development.
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       A list of domains to print.
!     \end{description}
!
!EOPI
      integer :: status, i, j
      integer :: min, max, stride

      print *, "DomainListPrint"
      print *, "Number stored domains:", domainlist%num_domains
      print *, "Total points:", domainlist%total_points

! Now loop through domains and print them out

      do i=1, domainlist%num_domains
         print *, '***Domain.  Rank:', domainlist%domains(i)%rank
         do j=1, domainlist%domains(i)%rank
	    call ESMF_AxisIndexGet(domainlist%domains(i)%ai(j), min, max, stride)
	    print *, '   axis:min,max,stride3:', min, max, stride
         enddo
      enddo

      end subroutine ESMF_DomainListPrint

!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_DomainListAdd2d - Add a 2d domainlist

!
! !INTERFACE:
      subroutine ESMF_DomainListAdd2d(domainlist, &
                             min1, max1, stride1, &
                             min2, max2, stride2)
!
! !ARGUMENTS:
     type(ESMF_DomainList), intent(inout) :: domainlist
     integer :: min1
     integer :: max1
     integer :: stride1
     integer :: min2
     integer :: max2
     integer :: stride2
!
! !DESCRIPTION:
!    Convenience function for adding a 2d domain.  Avoids the 
!    unnecessary hassle of creating a domain, etc...
!
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       The ESMF\_DomainList.
!     \item[min1]
!	Minimimun in first direction.
!     \item[max1]
!	Maximum in first direction.
!     \item[stride1]
!	Stride in first direction.
!     \item[min2]
!	Minimimun in second direction.
!     \item[max2]
!	Maximimun in second direction.
!     \item[stride2]
!	Stride in second direction.
!     \end{description}
!
!EOPI
      type(ESMF_Domain) :: newdomain          ! temp variable to use
      
      newdomain%rank = 2
      call ESMF_AxisIndexSet(newdomain%ai(1), min1, max1, stride1)
      call ESMF_AxisIndexSet(newdomain%ai(2), min2, max2, stride2)

      call ESMF_DomainListAdd(domainlist, newdomain)

      end subroutine ESMF_DomainListAdd2d

!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_DomainListAdd3d - Add a 3d domainlist

!
! !INTERFACE:
      subroutine ESMF_DomainListAdd3d(domainlist, &
                            min1, max1, stride1, &
                            min2, max2, stride2, &
                            min3, max3, stride3)
!
! !ARGUMENTS:
     type(ESMF_DomainList), intent(inout) :: domainlist
     integer :: min1
     integer :: max1
     integer :: stride1
     integer :: min2
     integer :: max2 
     integer :: stride2
     integer :: min3
     integer :: max3
     integer :: stride3
!
! !DESCRIPTION:
!    Convenience function for adding a 3d domain.  Avoids the 
!    unnecessary hassle of creating a domain, etc...
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       The ESMF\_DomainList.
!     \item[min1]
!       Minimimun in first direction.
!     \item[max1]
!       Maximum in first direction.
!     \item[stride1]
!       Stride in first direction.
!     \item[min2]
!       Minimimun in second direction.
!     \item[max2]
!       Maximimun in second direction.
!     \item[stride2]
!       Stride in second direction.
!     \item[min3]
!       Minimimun in third direction.
!     \item[max3]
!       Maximimun in third direction.
!     \item[stride3]
!       Stride in third direction.
!     \end{description}
!
!EOPI
      type(ESMF_Domain) :: newdomain          ! temp variable to use
      
      newdomain%rank = 3
      call ESMF_AxisIndexSet(newdomain%ai(1), min1, max1, stride1)
      call ESMF_AxisIndexSet(newdomain%ai(2), min2, max2, stride2)
      call ESMF_AxisIndexSet(newdomain%ai(3), min3, max3, stride3)

      call ESMF_DomainListAdd(domainlist,newdomain)

      end subroutine ESMF_DomainListAdd3d

!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_DomainListAddObj - Add a domain object 

!
! !INTERFACE:
      subroutine ESMF_DomainListAddObj(domainlist, newdomain)
!
! !ARGUMENTS:
      type(ESMF_DomainList), intent(inout) :: domainlist
      type(ESMF_Domain), intent(in) :: newdomain
!
! !DESCRIPTION:
!   The other add routines should end by using this call.  It takes care of
!   the memory management issues, i.e. it reallocs the list if it has grown
!   too large. 
!
!     The arguments are:
!     \begin{description}
!     \item[domainlist]
!       The ESMF\_DomainList.
!     \item[newdomain]
!       The ESMF\_Domain to add to the list.
!     \end{description}
!
!EOPI
      type(ESMF_Domain), dimension(:), allocatable, target :: temp_domains
      integer :: new_size         ! New number of domains to alloc
      integer :: status, i
      
! One way or another we are going to add the domain, so increment counter
      domainlist%num_domains = domainlist%num_domains + 1

! Check to see if we have room to add this object in the current list
! (Fortran equivalent of a linked list:)

      if (domainlist%num_domains  .gt. domainlist%current_size) then

! The strategy is debatable, but simply double the number of domains
      new_size = domainlist%current_size * 2
      allocate(temp_domains(new_size), stat=status)

! Copy over the old domains
      do i=1, domainlist%current_size
         temp_domains(i) = domainlist%domains(i)
      enddo

! Deallocate the old list and point to the new one
      deallocate(domainlist%domains)

      domainlist%domains => temp_domains
      domainlist%current_size = new_size
          
      endif

! Now add the new domain

      domainlist%domains(domainlist%num_domains) = newdomain

      end subroutine ESMF_DomainListAddObj

!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AxisIndexSet - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine ESMF_AxisIndexSet(ai, min, max, stride, rc)
!
! !ARGUMENTS:
      type(ESMF_AxisIndex), intent(inout) :: ai
      integer, intent(in) :: min, max, stride
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set the contents of an AxisIndex type.
!
!     The arguments are:
!     \begin{description}
!     \item[ai]
!       The ESMF\_AxisIndex.
!     \item[min]
!       The minimimun.
!     \item[max]
!       The maximum.
!     \item[stride]
!       The stride.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      ai%min = min
      ai%max = max
      ai%stride = stride

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AxisIndexSet

!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_AxisIndexGet - get contents of an AxisIndex object
!
! !INTERFACE:
      subroutine ESMF_AxisIndexGet(ai, min, max, stride, rc)
!
! !ARGUMENTS:
      type(ESMF_AxisIndex), intent(in) :: ai
      integer, intent(out), optional :: min, max, stride
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Get the contents of an AxisIndex type.
!
!     The arguments are:
!     \begin{description}
!     \item[ai]
!       The ESMF\_AxisIndex.
!     \item[min]
!       The minimimun.
!     \item[max]
!       The maximum.
!     \item[stride]
!       The stride.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      if (present(max)) min = ai%min
      if (present(max)) max = ai%max
      if (present(stride)) stride = ai%stride

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AxisIndexGet

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_SetPointer - set an opaque value

!
! !INTERFACE:
      subroutine ESMF_SetPointer(ptype, contents, rc)
!
! !ARGUMENTS:
      type(ESMF_Pointer) :: ptype 
      integer*8, intent(in) :: contents
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.
!
!     The arguments are:
!     \begin{description}
!     \item[ptype]
!       ESMF\_Pointer.
!     \item[contents]
!       The contents to set.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      ptype%ptr = contents
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetPointer

!-------------------------------------------------------------------------
!BOPI
!
! !IROUTINE:  ESMF_SetNullPointer - set an opaque value

!
! !INTERFACE:
      subroutine ESMF_SetNullPointer(ptype, rc)
!
! !ARGUMENTS:
      type(ESMF_Pointer) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.
!
!     The arguments are:
!     \begin{description}
!     \item[ptype]
!       ESMF\_Pointer.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!
!EOPI

      integer*8, parameter :: nullp = 0

      ptype%ptr = nullp
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetNullPointer
!------------------------------------------------------------------------- 
!BOPI
!  !IROUTINE:  ESMF_GetPointer - get an opaque value 
!  
! !INTERFACE: 
      function ESMF_GetPointer(ptype, rc) 
!
! !RETURN VALUE:
      integer*8 :: ESMF_GetPointer

! !ARGUMENTS:
      type(ESMF_Pointer), intent(in) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Get the contents of an opaque pointer type.
!
!     The arguments are:
!     \begin{description}
!     \item[ptype]
!       ESMF\_Pointer.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      ESMF_GetPointer = ptype%ptr
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GetPointer

!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
!BOPI 
!  !IROUTINE:  ESMF_StatusString - Return status as a string
!  
! !INTERFACE: 
      subroutine ESMF_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a status variable as a string.
!
!
!     The arguments are:
!     \begin{description}
!     \item[status]
!       The ESMF\_Status of a string.
!     \item[string]
!       The status string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:

      if (status .eq. ESMF_STATE_UNINIT) string = "Uninitialized"
      if (status .eq. ESMF_STATE_READY) string = "Ready"
      if (status .eq. ESMF_STATE_UNALLOCATED) string = "Unallocated"
      if (status .eq. ESMF_STATE_ALLOCATED) string = "Allocated"
      if (status .eq. ESMF_STATE_BUSY) string = "Busy"
      if (status .eq. ESMF_STATE_INVALID) string = "Invalid"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StatusString

!------------------------------------------------------------------------- 
!BOPI 
!  !IROUTINE:  ESMF_DataTypeString - Return DataType as a string
!  
! !INTERFACE: 
      subroutine ESMF_DataTypeString(datatype, string, rc)
!
! !ARGUMENTS:
      type(ESMF_DataType), intent(in) :: datatype
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a datatype variable as a string.
!
!
!     The arguments are:
!     \begin{description}
!     \item[datatype]
!       The ESMF\_DataType of a string.
!     \item[string]
!       The status string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      if (datatype .eq. ESMF_DATA_INTEGER) string = "Integer"
      if (datatype .eq. ESMF_DATA_REAL) string = "Real"
      if (datatype .eq. ESMF_DATA_LOGICAL) string = "Logical"
      if (datatype .eq. ESMF_DATA_CHARACTER) string = "Character"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DataTypeString

!------------------------------------------------------------------------- 
!BOPI 
!  !IROUTINE:  ESMF_DataKindString - Return DataKind as a string
!  
! !INTERFACE: 
      subroutine ESMF_DataKindString(datakind, string, rc)
!
! !ARGUMENTS:
      type(ESMF_DataKind), intent(in) :: datakind
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a datakind variable as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[datakind]
!       The ESMF\_DataKind of a string.
!     \item[string]
!       The status string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:

      if (datakind .eq. ESMF_I1)  string = "Integer*1"
      if (datakind .eq. ESMF_I2)  string = "Integer*2"
      if (datakind .eq. ESMF_I4)  string = "Integer*4"
      if (datakind .eq. ESMF_I8)  string = "Integer*8"
      if (datakind .eq. ESMF_R4)  string = "Real*4"
      if (datakind .eq. ESMF_R8)  string = "Real*8"
      if (datakind .eq. ESMF_C8)  string = "Complex*8"
      if (datakind .eq. ESMF_C16) string = "Complex*16"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DataKindString

!------------------------------------------------------------------------- 
!BOPI 
!  !IROUTINE:  ESMF_LogicalString - Return Logical as a string
!  
! !INTERFACE: 
      subroutine ESMF_LogicalString(tf, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Logical), intent(in) :: tf
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a tf variable as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[tf]
!       The ESMF\_Logical of a string.
!     \item[string]
!       The status string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:

      if (tf .eq. ESMF_TRUE)  string = "True"
      if (tf .eq. ESMF_FALSE) string = "False"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LogicalString

!-------------------------------------------------------------------------
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module ESMF_BaseMod
