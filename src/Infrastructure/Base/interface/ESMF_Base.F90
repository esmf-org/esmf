! $Id: ESMF_Base.F90,v 1.5 2002/10/23 20:01:31 eschwab Exp $
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
                                      ESMF_STATE_ALLOCATED = ESMF_Status(2), &
                                      ESMF_STATE_BUSY = ESMF_Status(3), &
                                      ESMF_STATE_INVALID = ESMF_Status(4)
 
      type ESMF_DataType
      sequence
      private
          integer :: dtype
      end type

      type(ESMF_DataType), parameter :: ESMF_DATA_INTEGER = ESMF_DataType(1), &
                                        ESMF_DATA_REAL = ESMF_DataType(2), &
                                        ESMF_DATA_LOGICAL = ESMF_DataType(3), &
                                        ESMF_DATA_CHARACTER = ESMF_DataType(4)
      type ESMF_DataValue
      sequence
      private
          type(ESMF_DataType) :: dt
          integer :: rank
          ! how do you do values of all types here ?
          ! in C++ i'd do a union w/ overloaded access funcs
          integer :: vi
          integer, dimension (:), pointer :: vip
          real :: vr
          real, dimension (:), pointer :: vrp
          logical :: vl
          logical, pointer :: vlp
          character (len=ESMF_MAXSTR) :: vc
          character, pointer :: vcp
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
     end type

! !PUBLIC TYPES:

      public ESMF_STATE_UNINIT, ESMF_STATE_ALLOCATED, &
             ESMF_STATE_BUSY, ESMF_STATE_INVALID

      public ESMF_DATA_INTEGER, ESMF_DATA_REAL, &
             ESMF_DATA_LOGICAL, ESMF_DATA_CHARACTER

      public ESMF_Status, ESMF_DataType, ESMF_DataValue 
      public ESMF_Attribute, ESMF_BasePointer, ESMF_Base

! !PUBLIC MEMBER FUNCTIONS:
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
!   Virtual methods to be defined by derived classes
!      public ESMF_Init
!      public ESMF_Create
!     no need for create, is there?  maybe for attribs if not inline?
!      public ESMF_Destroy
!      public ESMF_Construct 
!      public ESMF_Destruct
!      public ESMF_GetConfig
!      public ESMF_SetConfig
!      public ESMF_Read
!      public ESMF_Write
!      public ESMF_Validate
!      public ESMF_Print

!   Base class methods
!      public ESMF_BaseGetInstCount
!      public ESMF_BaseSetInstCount

!      public ESMF_BaseGetID
!      public ESMF_BaseSetID

!      public ESMF_BaseGetRefCount
!      public ESMF_BaseSetRefCount

!      public ESMF_BaseGetStatus
!      public ESMF_BaseSetStatus

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
!
!
!EOP

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: ESMF_Base.F90,v 1.5 2002/10/23 20:01:31 eschwab Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------

      contains

!BOP
! !INTERFACE:
      subroutine ESMF_AttributeSet(anytype, name, value, rc)
!
! !PARAMETERS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataValue), intent(in) :: value              ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.

!
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
!EOP

      end subroutine ESMF_AttributeSet


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeGet(anytype, name, type, value, rc)
!
! !PARAMETERS:
      type(ESMF_Base), intent(in) :: anytype           ! any ESMF type
      character (len = *), intent(in) :: name          ! attribute name
      type(ESMF_DataType), intent(out) :: type             ! all possible data types
      type(ESMF_DataValue), intent(out) :: value           ! attribute value
      integer, intent(out), optional :: rc             ! return code

!
! !DESCRIPTION:

!
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3
!EOP

      end subroutine ESMF_AttributeGet


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeGetCount(anytype, count, rc)
!
! !PARAMETERS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Returns number of attributes present.

!
! !REQUIREMENTS:  FLD1.7.5
!EOP

      end subroutine ESMF_AttributeGetCount


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeGetbyNumber(anytype, number, name, type, value, rc)
!
! !PARAMETERS:
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
! !REQUIREMENTS: 
!EOP

      end subroutine ESMF_AttributeGetbyNumber


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeGetNameList(anytype, count, namelist, rc)
!
! !PARAMETERS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      character (len = *), dimension (:), intent(out) :: namelist   ! attribute names
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.

!
! !REQUIREMENTS:  FLD1.7.3
!EOP

      end subroutine ESMF_AttributeGetNameList


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeSetList(anytype, namelist, valuelist, rc)

!
! !PARAMETERS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(ESMF_DataValue), dimension (:), intent(in) :: valuelist      ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
! !REQUIREMENTS:  (none.  added for completeness)
!EOP

      end subroutine ESMF_AttributeSetList


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeGetList(anytype, namelist, typelist, valuelist, rc)
!
! !PARAMETERS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(ESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.

!
! !REQUIREMENTS:  FLD1.7.4
!EOP

      end subroutine ESMF_AttributeGetList


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeSetObjectList(anytypelist, name, value, rc)
!
! !PARAMETERS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any ESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataValue), dimension (:), intent(in) :: value          ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.

!
! !REQUIREMENTS:  FLD1.5.5 (pri 2)
!EOP

      end subroutine ESMF_AttributeSetObjectList


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeGetObjectList(anytypelist, name, typelist, valuelist, rc)
!
! !PARAMETERS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any ESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.

!
! !REQUIREMENTS:  FLD1.5.5 (pri 2)
!EOP

      end subroutine ESMF_AttributeGetObjectList


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeCopy(name, source, destination, rc)
!
! !PARAMETERS:
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
! !REQUIREMENTS:  FLD1.5.4
!EOP

      end subroutine ESMF_AttributeCopy


!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_AttributeCopyAll(source, destination, rc)
!
! !PARAMETERS:
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
! !REQUIREMENTS:  FLD1.5.4
!EOP

      end subroutine ESMF_AttributeCopyAll
!-------------------------------------------------------------------------
!BOP
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module ESMF_BaseMod

