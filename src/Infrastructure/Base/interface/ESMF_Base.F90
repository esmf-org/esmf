! $Id: ESMF_Base.F90,v 1.16 2002/12/13 20:24:21 nscollins Exp $
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

#include <ESMF_Macros.inc>

!------------------------------------------------------------------------------
!
!
!------------------------------------------------------------------------------
! module definition

      module ESMF_BaseMod
 
!BOP
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
 
      type ESMF_Pointer
      sequence
      private
          integer*8 :: ptr
      end type

      type ESMF_DataType
      sequence
      private
          integer :: dtype
      end type

      type(ESMF_DataType), parameter :: ESMF_DATA_INTEGER = ESMF_DataType(1), &
                                        ESMF_DATA_REAL = ESMF_DataType(2), &
                                        ESMF_DATA_LOGICAL = ESMF_DataType(3), &
                                        ESMF_DATA_CHARACTER = ESMF_DataType(4)

      type ESMF_DataKind
      sequence
      private
        integer :: dkind
      end type

      type(ESMF_DataKind), parameter :: ESMF_KIND_4 = ESMF_DataKind(4), &
                                        ESMF_KIND_8 = ESMF_DataKind(8)

      type ESMF_DataValue
      sequence
      private
          type(ESMF_DataType) :: dt
          integer :: rank
          ! how do you do values of all types here ?
          ! in C++ i'd do a union w/ overloaded access funcs
          integer :: vi
          !integer, dimension (:), pointer :: vip
          !real :: vr
          !real, dimension (:), pointer :: vrp
          !logical :: vl
          !logical, pointer :: vlp
          !character (len=ESMF_MAXSTR) :: vc
          !character, pointer :: vcp
      end type

      type ESMF_Attribute
      sequence
      private
          character (len=ESMF_MAXSTR) :: attr_name
          type (ESMF_DataType) :: attr_type
          type (ESMF_DataValue) :: attr_value
      end type

      type ESMF_BasePointer
      sequence
      private
          integer*8 :: base_ptr
      end type

      type ESMF_Base
      sequence
      private
         integer :: inst_count
         integer :: ID
         integer :: ref_count
         type (ESMF_Status) :: base_status
         character (len=ESMF_MAXSTR) :: name
     end type

! !PUBLIC TYPES:

      public ESMF_STATE_UNINIT, ESMF_STATE_READY, &
             ESMF_STATE_UNALLOCATED, ESMF_STATE_ALLOCATED, &
             ESMF_STATE_BUSY, ESMF_STATE_INVALID

      public ESMF_DATA_INTEGER, ESMF_DATA_REAL, &
             ESMF_DATA_LOGICAL, ESMF_DATA_CHARACTER

      public ESMF_KIND_4, ESMF_KIND_8

      public ESMF_Status, ESMF_DataType, ESMF_DataKind, ESMF_DataValue 
      public ESMF_Attribute, ESMF_BasePointer, ESMF_Base, ESMF_Pointer

! !PUBLIC MEMBER FUNCTIONS:
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
!   Base class methods
!      public ESMF_BaseInit
!      public ESMF_BaseCreate
!     no need for create, is there?  maybe for attribs if not inline?
!      public ESMF_BaseDestroy
!      public ESMF_BaseConstruct 
!      public ESMF_BaseDestruct
!      public ESMF_BaseGetConfig
!      public ESMF_BaseSetConfig

!      public ESMF_BaseGetInstCount

!      public ESMF_BaseSetID
!      public ESMF_BaseGetID

!      public ESMF_BaseSetRefCount
!      public ESMF_BaseGetRefCount

!      public ESMF_BaseSetStatus
!      public ESMF_BaseGetStatus

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
 
!  Misc methods
      public ESMF_SetName
      public ESMF_GetName
      public ESMF_SetPointer
      public ESMF_SetNullPointer
      public ESMF_GetPointer

!  Overloaded = operator functions
      public ESMF_sfeq, ESMF_dteq, ESMF_dkeq
      public ESMF_sfne, ESMF_dtne, ESMF_dkne
!
!
!EOP

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Base.F90,v 1.16 2002/12/13 20:24:21 nscollins Exp $'
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare them as if
!  they were simple integers

interface operator (.eq.)
 module procedure ESMF_sfeq
 module procedure ESMF_dteq
 module procedure ESMF_dkeq
end interface

interface operator (.ne.)
 module procedure ESMF_sfne
 module procedure ESMF_dtne
 module procedure ESMF_dkne
end interface

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



!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_SetName - set the name of this object
!
! !INTERFACE:
      subroutine ESMF_SetName(anytype, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Base) :: anytype                         ! any ESMF object/type
      character (len = *), intent(in) :: name            ! object/type name
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Associate a name with any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      anytype%name = name
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetName

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_GetName - set the name of this object
!
! !INTERFACE:
      subroutine ESMF_GetName(anytype, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF object/type
      character (len = *), intent(out) :: name           ! object/type name
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Return the name of any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      name = anytype%name
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GetName


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeSet - set attribute on an ESMF type
!
! !INTERFACE:
      subroutine ESMF_AttributeSet(anytype, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataValue), intent(in) :: value              ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      end subroutine ESMF_AttributeSet


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeGet - get attribute from an ESMF type
!
! !INTERFACE:
      subroutine ESMF_AttributeGet(anytype, name, type, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype           ! any ESMF type
      character (len = *), intent(in) :: name          ! attribute name
      type(ESMF_DataType), intent(out) :: type             ! all possible data types
      type(ESMF_DataValue), intent(out) :: value           ! attribute value
      integer, intent(out), optional :: rc             ! return code

!
! !DESCRIPTION:

!
!EOP
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3

      end subroutine ESMF_AttributeGet


!-------------------------------------------------------------------------
!BOP
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
!EOP
! !REQUIREMENTS:  FLD1.7.5

      end subroutine ESMF_AttributeGetCount


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeGetbyNumber - get an ESMF object's attribute by num ber
!
! !INTERFACE:
      subroutine ESMF_AttributeGetbyNumber(anytype, number, name, type, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(in) :: number                      ! attribute number
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataType), intent(out) :: type               ! all possible data types
      type(ESMF_DataValue), intent(out) :: value             ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!EOP
! !REQUIREMENTS: 

      end subroutine ESMF_AttributeGetbyNumber


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMF_AttributeGetNameList - get an ESMF object's attribute name list
!
! !INTERFACE:
      subroutine ESMF_AttributeGetNameList(anytype, count, namelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      character (len = *), dimension (:), intent(out) :: namelist   ! attribute names
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.

!
!EOP
! !REQUIREMENTS:  FLD1.7.3

      end subroutine ESMF_AttributeGetNameList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeSetList - set an ESMF object's attributes 
!
! !INTERFACE:
      subroutine ESMF_AttributeSetList(anytype, namelist, valuelist, rc)

!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(ESMF_DataValue), dimension (:), intent(in) :: valuelist      ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!EOP
! !REQUIREMENTS:  (none.  added for completeness)

      end subroutine ESMF_AttributeSetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeGetList - get an ESMF object's attributes
!
! !INTERFACE:
      subroutine ESMF_AttributeGetList(anytype, namelist, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(ESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.

!
!EOP
! !REQUIREMENTS:  FLD1.7.4

      end subroutine ESMF_AttributeGetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeSetObjectList - set an attribute on multiple ESMF objects 
!
! !INTERFACE:
      subroutine ESMF_AttributeSetObjectList(anytypelist, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any ESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataValue), dimension (:), intent(in) :: value          ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine ESMF_AttributeSetObjectList


!-------------------------------------------------------------------------
!BOP
!
!
! !IROUTINE:  ESMF_AttributeGetObjectList - get an attribute from multiple ESMF objects 
!
! !INTERFACE:
      subroutine ESMF_AttributeGetObjectList(anytypelist, name, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any ESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine ESMF_AttributeGetObjectList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
      subroutine ESMF_AttributeCopy(name, source, destination, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_Base), intent(in) :: source              ! any ESMF type
      type(ESMF_Base), intent(in) :: destination         ! any ESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>


!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine ESMF_AttributeCopy


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects

!
! !INTERFACE:
      subroutine ESMF_AttributeCopyAll(source, destination, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: source              ! any ESMF type
      type(ESMF_Base), intent(in) :: destination         ! any ESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)

!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine ESMF_AttributeCopyAll

!=========================================================================
!BOP
!
!IROUTINE:  ESMC_SetPointer - set an opaque value

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
!EOP
! !REQUIREMENTS:
      ptype%ptr = contents
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetPointer

!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_SetNullPointer - set an opaque value

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
!EOP
! !REQUIREMENTS:
      integer*8 :: nullp = 0

      ptype%ptr = nullp
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetNullPointer
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_GetPointer - get an opaque value

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
!EOP
! !REQUIREMENTS:
      ESMF_GetPointer = ptype%ptr
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GetPointer

!-------------------------------------------------------------------------
!BOP
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module ESMF_BaseMod
