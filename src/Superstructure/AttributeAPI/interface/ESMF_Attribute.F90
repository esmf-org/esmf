! $Id: ESMF_Attribute.F90,v 1.1.2.1 2008/04/28 06:01:34 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Attribute.F90"
!==============================================================================
!
! ESMF Attribute Module
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

module ESMF_AttributeMod
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_AttributeMod - Attribute class
!
! !DESCRIPTION:
! The code in this file overloads Attribute types defined in
! other classes with shared, simple public interfaces.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_FieldBundleMod
  use ESMF_FieldMod
  use ESMF_StateMod


  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The Attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
! !PUBLIC MEMBER FUNCTIONS:
!

!  Attribute methods
      public ESMF_AttributeSet
      public ESMF_AttributeGet
      public ESMF_AttributeGetCount
      public ESMF_AttributeGetInfo

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Attribute.F90,v 1.1.2.1 2008/04/28 06:01:34 cdeluca Exp $'
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGet - Get an Attribute
!
! !INTERFACE:
      interface ESMF_AttributeGet 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleGetI4Attr
        module procedure ESMF_FieldBundleGetI4ListAttr
        module procedure ESMF_FieldBundleGetI8Attr
        module procedure ESMF_FieldBundleGetI8ListAttr
        module procedure ESMF_FieldBundleGetR4Attr
        module procedure ESMF_FieldBundleGetR4ListAttr
        module procedure ESMF_FieldBundleGetR8Attr
        module procedure ESMF_FieldBundleGetR8ListAttr
        module procedure ESMF_FieldBundleGetLogAttr
        module procedure ESMF_FieldBundleGetLogListAttr
        module procedure ESMF_FieldBundleGetCharAttr

        module procedure ESMF_FieldGetInt4Attr
        module procedure ESMF_FieldGetInt4ListAttr
        module procedure ESMF_FieldGetInt8Attr
        module procedure ESMF_FieldGetInt8ListAttr
        module procedure ESMF_FieldGetReal4Attr
        module procedure ESMF_FieldGetReal4ListAttr
        module procedure ESMF_FieldGetReal8Attr
        module procedure ESMF_FieldGetReal8ListAttr
        module procedure ESMF_FieldGetLogicalAttr
        module procedure ESMF_FieldGetLogicalListAttr
        module procedure ESMF_FieldGetCharAttr

        module procedure ESMF_StateGetInt4Attr
        module procedure ESMF_StateGetInt4ListAttr
        module procedure ESMF_StateGetInt8Attr
        module procedure ESMF_StateGetInt8ListAttr
        module procedure ESMF_StateGetReal4Attr
        module procedure ESMF_StateGetReal4ListAttr
        module procedure ESMF_StateGetReal8Attr
        module procedure ESMF_StateGetReal8ListAttr
        module procedure ESMF_StateGetLogicalAttr
        module procedure ESMF_StateGetLogicalListAttr
        module procedure ESMF_StateGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGetCount - Get number of Attributes
!
! !INTERFACE:
      interface ESMF_AttributeGetCount 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleGetAttCount
        module procedure ESMF_FieldGetAttributeCount
        module procedure ESMF_StateGetAttributeCount

! !DESCRIPTION:
!     This interface provides a single entry point for methods that get
!     total Attribute count.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGetInfo - Query Attribute information
!
! !INTERFACE:
      interface ESMF_AttributeGetInfo 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleGetAttByName
        module procedure ESMF_FieldBundleGetAttByNum

        module procedure ESMF_FieldGetAttrInfoByName
        module procedure ESMF_FieldGetAttrInfoByNum

        module procedure ESMF_StateGetAttrInfoByName
        module procedure ESMF_StateGetAttrInfoByNum


! !DESCRIPTION:
!     This interface provides a single entry point for methods that get
!     information about Attributes.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an attribute
!
! !INTERFACE:
      interface ESMF_AttributeSet
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleSetI4Attr
        module procedure ESMF_FieldBundleSetI4ListAttr
        module procedure ESMF_FieldBundleSetI8Attr
        module procedure ESMF_FieldBundleSetI8ListAttr
        module procedure ESMF_FieldBundleSetR4Attr
        module procedure ESMF_FieldBundleSetR4ListAttr
        module procedure ESMF_FieldBundleSetR8Attr
        module procedure ESMF_FieldBundleSetR8ListAttr
        module procedure ESMF_FieldBundleSetLogAttr
        module procedure ESMF_FieldBundleSetLogListAttr
        module procedure ESMF_FieldBundleSetCharAttr

        module procedure ESMF_FieldSetInt4Attr
        module procedure ESMF_FieldSetInt4ListAttr
        module procedure ESMF_FieldSetInt8Attr
        module procedure ESMF_FieldSetInt8ListAttr
        module procedure ESMF_FieldSetReal4Attr
        module procedure ESMF_FieldSetReal4ListAttr
        module procedure ESMF_FieldSetReal8Attr
        module procedure ESMF_FieldSetReal8ListAttr
        module procedure ESMF_FieldSetLogicalAttr
        module procedure ESMF_FieldSetLogicalListAttr
        module procedure ESMF_FieldSetCharAttr

        module procedure ESMF_StateSetInt4Attr
        module procedure ESMF_StateSetInt4ListAttr
        module procedure ESMF_StateSetInt8Attr
        module procedure ESMF_StateSetInt8ListAttr
        module procedure ESMF_StateSetReal4Attr
        module procedure ESMF_StateSetReal4ListAttr
        module procedure ESMF_StateSetReal8Attr
        module procedure ESMF_StateSetReal8ListAttr
        module procedure ESMF_StateSetLogicalAttr
        module procedure ESMF_StateSetLogicalListAttr
        module procedure ESMF_StateSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to FieldBundles and other objects.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------

! This method is here to prevent the compiler from complaining about no
! methods in the module.

      subroutine ESMF_BlankRoutine(rc)
      integer, intent(out), optional :: rc

      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      end subroutine ESMF_BlankRoutine

!------------------------------------------------------------------------------

end module ESMF_AttributeMod

