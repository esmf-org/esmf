! $Id: ESMF_Attribute.F90,v 1.16 2008/07/12 18:58:20 rokuingh Exp $
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
! !MODULE: ESMF_AttributeMod - Attribute API
!
! !DESCRIPTION:
!
! The code in this file implements the Attribute defined type
!  and functions.  This is an
!  interface to the actual C++ Attribute class implementation 
!  in the ../../Infrastructure/Attribute dir.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_BaseMod
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_ArrayMod
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_GridCompMod
  use ESMF_FieldMod
  use ESMF_FieldBundleMod
  use ESMF_GridMod
  use ESMF_StateTypesMod
  use ESMF_StateVaMod
  use ESMF_StateMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_Attribute
!
!------------------------------------------------------------------------------

  type ESMF_Attribute
  sequence
    character(len=ESMF_MAXSTR)  :: attr_name
    type(ESMF_DataValue)        :: attr_value
  end type

!------------------------------------------------------------------------------
! ! ESMF_AttWriteFlag
!
!------------------------------------------------------------------------------
  type ESMF_AttWriteFlag
  sequence
     integer :: attwriteflag
  end type

  type(ESMF_AttWriteFlag), parameter :: &
                      ESMF_ATTWRITEFLAG_TAB=ESMF_AttWriteFlag(0), &
                      ESMF_ATTWRITEFLAG_XML=ESMF_AttWriteFlag(1)

!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!     The following routines apply to {\tt ESMF\_Array}, {\tt ESMF\_Field},
!     {\tt ESMF\_FieldBundle}, {\tt ESMF\_Grid}, and {\tt ESMF\_State}.  
!
! !PUBLIC MEMBER FUNCTIONS:
!
!   Classes
      public ESMF_Attribute
      public ESMF_AttWriteFlag, ESMF_ATTWRITEFLAG_TAB, ESMF_ATTWRITEFLAG_XML
      
!  Attribute methods
      public ESMF_AttributeAdd
      public ESMF_AttributeCopy
      public ESMF_AttributeGet
      public ESMF_AttributeSet
      public ESMF_AttributeWrite
        
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Attribute.F90,v 1.16 2008/07/12 18:58:20 rokuingh Exp $'
!------------------------------------------------------------------------------
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeAdd  - Create Attribute packages
!
! !INTERFACE:
      interface ESMF_AttributeAdd
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttAddPack
        module procedure ESMF_ArrayAttAddPackCustom
        module procedure ESMF_CplCompAttAddPack
        module procedure ESMF_CplCompAttAddPackCustom
        module procedure ESMF_GridCompAttAddPack
        module procedure ESMF_GridCompAttAddPackCustom
        module procedure ESMF_FieldAttAddPack
        module procedure ESMF_FieldAttAddPackCustom
        module procedure ESMF_FBundleAttAddPack
        module procedure ESMF_FBundleAttAddPackCustom
        module procedure ESMF_GridAttAddPack
        module procedure ESMF_GridAttAddPackCustom
        module procedure ESMF_StateAttAddPack
        module procedure ESMF_StateAttAddPackCustom
        
! !DESCRIPTION:
!     This interface provides a single entry point for methods that create
!     an Attribute package.
 
!EOPI
      end interface

!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeCopy  - Copy an Attribute or hierarchy
!
! !INTERFACE:
      interface ESMF_AttributeCopy
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_StateAttributeCopyAll
        
! !DESCRIPTION:
!     This interface provides a single entry point for methods that copy
!     an Attribute or Attribute hierarchy.
 
!EOPI
      end interface

!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGet  - Get Attributes, Attribute packages, count, 
!              info, and type
!
! !INTERFACE:
      interface ESMF_AttributeGet
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttGetInt4
        module procedure ESMF_ArrayAttGetInt4List
        module procedure ESMF_ArrayAttGetInt8
        module procedure ESMF_ArrayAttGetInt8List
        module procedure ESMF_ArrayAttGetReal4
        module procedure ESMF_ArrayAttGetReal4List
        module procedure ESMF_ArrayAttGetReal8
        module procedure ESMF_ArrayAttGetReal8List
        module procedure ESMF_ArrayAttGetLogical
        module procedure ESMF_ArrayAttGetLogicalList
        module procedure ESMF_ArrayAttGetChar
        module procedure ESMF_ArrayAttGetInfoByName
        module procedure ESMF_ArrayAttGetInfoByNum
        module procedure ESMF_ArrayAttGetCount
        module procedure ESMF_CplCompAttGetInt4
        module procedure ESMF_CplCompAttGetInt4List
        module procedure ESMF_CplCompAttGetInt8
        module procedure ESMF_CplCompAttGetInt8List
        module procedure ESMF_CplCompAttGetReal4
        module procedure ESMF_CplCompAttGetReal4List
        module procedure ESMF_CplCompAttGetReal8
        module procedure ESMF_CplCompAttGetReal8List
        module procedure ESMF_CplCompAttGetLogical
        module procedure ESMF_CplCompAttGetLogicalList
        module procedure ESMF_CplCompAttGetChar
        module procedure ESMF_CplCompAttGetInfoByName
        module procedure ESMF_CplCompAttGetInfoByNum
        module procedure ESMF_CplCompAttGetCount
        module procedure ESMF_GridCompAttGetInt4
        module procedure ESMF_GridCompAttGetInt4List
        module procedure ESMF_GridCompAttGetInt8
        module procedure ESMF_GridCompAttGetInt8List
        module procedure ESMF_GridCompAttGetReal4
        module procedure ESMF_GridCompAttGetReal4List
        module procedure ESMF_GridCompAttGetReal8
        module procedure ESMF_GridCompAttGetReal8List
        module procedure ESMF_GridCompAttGetLogical
        module procedure ESMF_GridCompAttGetLogicalList
        module procedure ESMF_GridCompAttGetChar
        module procedure ESMF_GridCompAttGetInfoByName
        module procedure ESMF_GridCompAttGetInfoByNum
        module procedure ESMF_GridCompAttGetCount
        module procedure ESMF_FieldAttGetInt4
        module procedure ESMF_FieldAttGetInt4List
        module procedure ESMF_FieldAttGetInt8
        module procedure ESMF_FieldAttGetInt8List
        module procedure ESMF_FieldAttGetReal4
        module procedure ESMF_FieldAttGetReal4List
        module procedure ESMF_FieldAttGetReal8
        module procedure ESMF_FieldAttGetReal8List
        module procedure ESMF_FieldAttGetLogical
        module procedure ESMF_FieldAttGetLogicalList
        module procedure ESMF_FieldAttGetChar
        module procedure ESMF_FieldAttGetInfoByName
        module procedure ESMF_FieldAttGetInfoByNum
        module procedure ESMF_FieldAttGetCount
        module procedure ESMF_FBundleAttGetInt4
        module procedure ESMF_FBundleAttGetInt4List
        module procedure ESMF_FBundleAttGetInt8
        module procedure ESMF_FBundleAttGetInt8List
        module procedure ESMF_FBundleAttGetReal4
        module procedure ESMF_FBundleAttGetReal4List
        module procedure ESMF_FBundleAttGetReal8
        module procedure ESMF_FBundleAttGetReal8List
        module procedure ESMF_FBundleAttGetLogical
        module procedure ESMF_FBundleAttGetLogicalList
        module procedure ESMF_FBundleAttGetChar
        module procedure ESMF_FBundleAttGetInfoByName
        module procedure ESMF_FBundleAttGetInfoByNum
        module procedure ESMF_FBundleAttGetCount
        module procedure ESMF_GridAttGetInt4
        module procedure ESMF_GridAttGetInt4List
        module procedure ESMF_GridAttGetInt8
        module procedure ESMF_GridAttGetInt8List
        module procedure ESMF_GridAttGetReal4
        module procedure ESMF_GridAttGetReal4List
        module procedure ESMF_GridAttGetReal8
        module procedure ESMF_GridAttGetReal8List
        module procedure ESMF_GridAttGetLogical
        module procedure ESMF_GridAttGetLogicalList
        module procedure ESMF_GridAttGetChar
        module procedure ESMF_GridAttGetInfoByName
        module procedure ESMF_GridAttGetInfoByNum
        module procedure ESMF_GridAttGetCount
        module procedure ESMF_StateAttGetInt4
        module procedure ESMF_StateAttGetInt4List
        module procedure ESMF_StateAttGetInt8
        module procedure ESMF_StateAttGetInt8List
        module procedure ESMF_StateAttGetReal4
        module procedure ESMF_StateAttGetReal4List
        module procedure ESMF_StateAttGetReal8
        module procedure ESMF_StateAttGetReal8List
        module procedure ESMF_StateAttGetLogical
        module procedure ESMF_StateAttGetLogicalList
        module procedure ESMF_StateAttGetChar
        module procedure ESMF_StateAttGetInfoByName
        module procedure ESMF_StateAttGetInfoByNum
        module procedure ESMF_StateAttGetCount

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     Attributes.
 
!EOPI
      end interface

!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeSet  - Set Attributes and Attribute packages
!
! !INTERFACE:
      interface ESMF_AttributeSet 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttSetInt4
        module procedure ESMF_ArrayAttSetInt4List
        module procedure ESMF_ArrayAttSetInt8
        module procedure ESMF_ArrayAttSetInt8List
        module procedure ESMF_ArrayAttSetReal4
        module procedure ESMF_ArrayAttSetReal4List
        module procedure ESMF_ArrayAttSetReal8
        module procedure ESMF_ArrayAttSetReal8List
        module procedure ESMF_ArrayAttSetLogical
        module procedure ESMF_ArrayAttSetLogicalList
        module procedure ESMF_ArrayAttSetChar
        module procedure ESMF_CplCompAttSetInt4
        module procedure ESMF_CplCompAttSetInt4List
        module procedure ESMF_CplCompAttSetInt8
        module procedure ESMF_CplCompAttSetInt8List
        module procedure ESMF_CplCompAttSetReal4
        module procedure ESMF_CplCompAttSetReal4List
        module procedure ESMF_CplCompAttSetReal8
        module procedure ESMF_CplCompAttSetReal8List
        module procedure ESMF_CplCompAttSetLogical
        module procedure ESMF_CplCompAttSetLogicalList
        module procedure ESMF_CplCompAttSetChar
        module procedure ESMF_CplCompAttSetLinkCplComp
        module procedure ESMF_CplCompAttSetLinkGridComp
        module procedure ESMF_CplCompAttSetLinkState
        module procedure ESMF_GridCompAttSetInt4
        module procedure ESMF_GridCompAttSetInt4List
        module procedure ESMF_GridCompAttSetInt8
        module procedure ESMF_GridCompAttSetInt8List
        module procedure ESMF_GridCompAttSetReal4
        module procedure ESMF_GridCompAttSetReal4List
        module procedure ESMF_GridCompAttSetReal8
        module procedure ESMF_GridCompAttSetReal8List
        module procedure ESMF_GridCompAttSetLogical
        module procedure ESMF_GridCompAttSetLogicalList
        module procedure ESMF_GridCompAttSetChar
        module procedure ESMF_GridCompAttSetLinkCplComp
        module procedure ESMF_GridCompAttSetLinkGridComp
        module procedure ESMF_GridCompAttSetLinkState
        module procedure ESMF_FieldAttSetInt4
        module procedure ESMF_FieldAttSetInt4List
        module procedure ESMF_FieldAttSetInt8
        module procedure ESMF_FieldAttSetInt8List
        module procedure ESMF_FieldAttSetReal4
        module procedure ESMF_FieldAttSetReal4List
        module procedure ESMF_FieldAttSetReal8
        module procedure ESMF_FieldAttSetReal8List
        module procedure ESMF_FieldAttSetLogical
        module procedure ESMF_FieldAttSetLogicalList
        module procedure ESMF_FieldAttSetChar
        module procedure ESMF_FBundleAttSetInt4
        module procedure ESMF_FBundleAttSetInt4List
        module procedure ESMF_FBundleAttSetInt8
        module procedure ESMF_FBundleAttSetInt8List
        module procedure ESMF_FBundleAttSetReal4
        module procedure ESMF_FBundleAttSetReal4List
        module procedure ESMF_FBundleAttSetReal8
        module procedure ESMF_FBundleAttSetReal8List
        module procedure ESMF_FBundleAttSetLogical
        module procedure ESMF_FBundleAttSetLogicalList
        module procedure ESMF_FBundleAttSetChar
        module procedure ESMF_FBundleAttSetLinkField
        module procedure ESMF_GridAttSetInt4
        module procedure ESMF_GridAttSetInt4List
        module procedure ESMF_GridAttSetInt8
        module procedure ESMF_GridAttSetInt8List
        module procedure ESMF_GridAttSetReal4
        module procedure ESMF_GridAttSetReal4List
        module procedure ESMF_GridAttSetReal8
        module procedure ESMF_GridAttSetReal8List
        module procedure ESMF_GridAttSetLogical
        module procedure ESMF_GridAttSetLogicalList
        module procedure ESMF_GridAttSetChar
        module procedure ESMF_StateAttSetInt4
        module procedure ESMF_StateAttSetInt4List
        module procedure ESMF_StateAttSetInt8
        module procedure ESMF_StateAttSetInt8List
        module procedure ESMF_StateAttSetReal4
        module procedure ESMF_StateAttSetReal4List
        module procedure ESMF_StateAttSetReal8
        module procedure ESMF_StateAttSetReal8List
        module procedure ESMF_StateAttSetLogical
        module procedure ESMF_StateAttSetLogicalList
        module procedure ESMF_StateAttSetChar
        module procedure ESMF_StateAttSetLinkFB
        module procedure ESMF_StateAttSetLinkField
        module procedure ESMF_StateAttSetLinkState

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     Attributes and Attribute packages, and link Attribute hierarchies.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeWrite  - Write an Attribute or Attribute Package
!
! !INTERFACE:
      interface ESMF_AttributeWrite
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttWrite
        module procedure ESMF_CplCompAttWrite
        module procedure ESMF_GridCompAttWrite
        module procedure ESMF_FieldAttWrite
        module procedure ESMF_FBundleAttWrite
        module procedure ESMF_GridAttWrite
        module procedure ESMF_StateAttWrite

! !DESCRIPTION:
!     This interface provides a single entry point for methods that write
!     an Attribute or Attribute package.
 
!EOPI
      end interface

!
!------------------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeAdd  - Add an Attribute package
!
! !INTERFACE:
!     ! Private name; call using ESMF_AttributeAdd() 
!     subroutine ESMF_AttAddPack(<object>, convention, purpose, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values  
!     character(ESMF_MAXSTR), intent(in) :: convention
!     character(ESMF_MAXSTR), intent(in) :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Adds an Attribute package to <object> with four Attributes named 
!     {\tt shortname}, {\tt longname}, {\tt units}, and {\tt coordinates}.
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_CplComp), intent(inout) :: comp  
!     \item type(ESMF\_GridComp), intent(inout) :: comp  
!     \item type(ESMF\_Field), intent(inout) :: field  
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle 
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           An {\tt ESMF} object.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeAdd  - Add an Attribute package
!
! !INTERFACE:
!     ! Private name; call using ESMF_AttributeAdd() 
!     subroutine ESMF_AttAddPackCustom(<object>, convention, purpose, &
!     attrList, count, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values  
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     character (ESMF_MAXSTR), dimension(:), intent(in) :: attrList
!     integer, intent(in) :: count   
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Adds a customized Attribute package to <object>.
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_CplComp), intent(inout) :: comp  
!     \item type(ESMF\_GridComp), intent(inout) :: comp  
!     \item type(ESMF\_Field), intent(inout) :: field  
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle 
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           An {\tt ESMF} object.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [attrList]
!           The list of Attribute names to specify the custom Attribute package.
!     \item [count]
!           The number of Attributes to add to the custom Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeCopy - Copy an Attribute or Attribute hierarchy
!
! !INTERFACE:
!     ! Private name; call using ESMF_AttributeCopy() 
!     subroutine ESMF_AttributeCopyAll(<object1>, <object2>, rc)
!
! !ARGUMENTS:
!     <object1>, see below for supported values  
!     <object2>, see below for supported values 
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Copies an Attribute hierarchy from <object1> to <object2>.
!     Supported values for <object1> are:
!     \begin{description}
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!     Supported values for <object2> are:
!     \begin{description}
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [<object1>]
!           An {\tt ESMF} object.
!     \item [<object2>]
!           An {\tt ESMF} object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet  - Retrieve an Attribute
!
! !INTERFACE:
!     subroutine ESMF_AttributeGet(<object>, name, count, <value argument>, &
!                            <defaultvalue argument>, convention, purpose, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values  
!     character (len = *), intent(in) :: name
!     integer, intent(in), optional :: count
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an Attribute from the <object>, or from the Attribute package
!     specified by {\tt convention} and {\tt purpose}.  A default value 
!     argument may be given if a return code is not desired when the 
!     Attribute is not found.
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_CplComp), intent(inout) :: comp  
!     \item type(ESMF\_GridComp), intent(inout) :: comp  
!     \item type(ESMF\_Field), intent(inout) :: field  
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle 
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!     Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(out) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(out) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(out) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(out) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(out) :: valueList
!     \item type(ESMF\_Logical), intent(out) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(out) :: valueList
!     \item character (len = *), intent(out), value
!     \end{description}
!     Supported values for <defaultvalue argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: defaultvalue
!     \item integer(ESMF\_KIND\_I8), intent(out) :: defaultvalue
!     \item real (ESMF\_KIND\_R4), intent(out) :: defaultvalue
!     \item real (ESMF\_KIND\_R8), intent(out) :: defaultvalue
!     \item type(ESMF\_Logical), intent(out) :: defaultvalue
!     \item character (len = *), intent(out), defaultvalue
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           An {\tt ESMF} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of items in a multi-valued Attribute.
!     \item [<value argument>]
!           The value of the named Attribute.
!     \item [<defaultvalue argument>]
!           The default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet - Get the Attribute count
!
! !INTERFACE:
!     subroutine ESMF_AttributeGetCount(<object>, count, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values  
!     integer, intent(out) :: count
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns the Attribute count for <object>
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_CplComp), intent(inout) :: comp  
!     \item type(ESMF\_GridComp), intent(inout) :: comp  
!     \item type(ESMF\_Field), intent(inout) :: field  
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle 
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           An {\tt ESMF} object.
!     \item [count] 
!           The Attribute count for <object>.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet - Get Attribute info by name
!
! !INTERFACE:
!     subroutine ESMF_AttributeGetInfoByName(<object>, name, typekind, count, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values  
!     character (len = *), intent(in) :: name
!     type(ESMF_TypeKind), intent(out), optional :: typekind
!     integer, intent(out), optional :: count   
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns information associated with the named Attribute, 
!     including {\tt typekind} and {\tt count}.
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_CplComp), intent(inout) :: comp  
!     \item type(ESMF\_GridComp), intent(inout) :: comp  
!     \item type(ESMF\_Field), intent(inout) :: field  
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle 
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           An {\tt ESMF} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeGet - Get Attribute info by index number
!
! !INTERFACE:
!     subroutine ESMF_AttributeGetInfoByNum(<object>, attributeIndex, name, &
!       typekind, count, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values  
!     integer, intent(in) :: attributeIndex
!     character (len = *), intent(in) :: name
!     type(ESMF_TypeKind), intent(out), optional :: typekind
!     integer, intent(out), optional :: count   
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns information associated with the indexed Attribute, 
!     including {\tt name}, {\tt typekind} and {\tt count}.
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_CplComp), intent(inout) :: comp  
!     \item type(ESMF\_GridComp), intent(inout) :: comp  
!     \item type(ESMF\_Field), intent(inout) :: field  
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle 
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           An {\tt ESMF} object.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeSet - Set an Attribute
!
! !INTERFACE:
!     subroutine ESMF_AttributeSet(<object>, name, count, <value argument>, &
!                            <defaultvalue argument>, convention, purpose, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values  
!     character (len = *), intent(in) :: name
!     integer, intent(in), optional :: count
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an Attribute to <object>, or sets an Attribute on an 
!     Attribute package.
!     The Attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList}, with a {\tt count}, and a {\tt convention} and {\tt purpose}.
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_CplComp), intent(inout) :: comp  
!     \item type(ESMF\_GridComp), intent(inout) :: comp  
!     \item type(ESMF\_Field), intent(inout) :: field  
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle 
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(in) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(in) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(in) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(in) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(in) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(in) :: valueList
!     \item type(ESMF\_Logical), intent(in) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(in) :: valueList
!     \item character (len = *), intent(in), value
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           An {\tt ESMF} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [count]
!           The number of items in a multi-valued Attribute.
!     \item [<value argument>]
!           The value of the Attribute to set.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!-------------------------------------------------------------------------
!  ARRAY
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttAddPack"
!BOPI
! !IROUTINE: ESMF_ArrayAttAddPack - Create an array Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_ArrayAttAddPack(array, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the Attribute package for the {\tt array}.
!     The Attribute package defines the convention, purpose, and object type 
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      fconvention = convention
      fpurpose = purpose
      fobject = 'array'

      name1 = 'shortname'
      name2 = 'longname'
      name3 = 'units'
      name4 = 'dimensions'

      call c_ESMC_AttPackCreate(array, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(array, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(array, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(array, name4, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttAddPack

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_ArrayAttAddPackCustom - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_ArrayAttAddPackCustom(array, convention, purpose, &
      attrList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      character (len = *), dimension(:), intent(in) :: attrList
      integer, intent(in) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up a custom Attribute package for the {\tt array}, or adds to an 
!     existing Attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif

      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif

      fobject = 'array'
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(array, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttAddPackCustom

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetInt4(array, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer Attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetInt4List(array, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list Attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetInt8(array, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer Attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetInt8List(array, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list Attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetReal4(array, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetReal4List(array, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from an {\tt ESMF\_Array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetReal8(array, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetReal8List(array, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from an {\tt ESMF\_Array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetLogical(array, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical Attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetLogicalList(array, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list Attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The logical values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetChar"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetChar(array, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character Attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [defaultvalue]
!           The character default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackGetChar(array, name, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetChar(array, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetCount"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query the number of Attributes
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet() 
      subroutine ESMF_ArrayAttGetCount(array, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of Attributes associated with the given {\tt array} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [count]
!           The number of Attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_AttributeGetCount(array, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetInfoByName"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Array Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetInfoByName(array, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named Attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_AttributeGetInfoName(array, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetInfoByNum"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Array Attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_ArrayAttGetInfoByNum(array, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed Attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localItemcount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_AttributeGetInfoNum(array, attributeIndex, &
        localName, localTk, localItemcount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetInfoByNum
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetInt4(array, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer Attribute to the {\tt array}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetInt4List(array, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list Attribute to the {\tt array}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetInt8(array, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer Attribute to the {\tt array}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetInt8List(array, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list Attribute to the {\tt array}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetReal4(array, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real Attribute to the {\tt array}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetReal4List(array, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list Attribute to the {\tt array}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetReal8(array, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real Attribute to the {\tt array}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetReal8List(array, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list Attribute to the {\tt array}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_TypeKind) :: tk

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetLogical(array, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical Attribute to the {\tt array}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The logical true/false value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetLogicalList(array, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list Attribute to the {\tt array}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetChar"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_ArrayAttSetChar(array, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character Attribute to the {\tt array}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The character value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_AttPackSetChar(array, name, value, &
        ESMF_TYPEKIND_CHARACTER, fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetChar(array, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttSetChar

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttWrite"
!BOPI
! !IROUTINE: ESMF_AttributeWrite - Print the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeWrite()
      subroutine ESMF_ArrayAttWrite(array, convention, purpose, &
        attwriteflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      type(ESMF_AttWriteFlag), intent(in), optional :: attwriteflag
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Print the Attribute package for the {\tt array}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attwriteflag]
!      Flag to determine the format for writing the Attributes.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_AttWriteFlag) :: writeflag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call ESMF_ArrayValidate(array, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      if (present(attwriteflag)) then
        writeflag = attwriteflag
      else
        writeflag = ESMF_ATTWRITEFLAG_TAB
      endif
      
      if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_TAB%attwriteflag) then
        call c_ESMC_AttPackWrite(array, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_XML%attwriteflag) then
        call c_ESMC_AttPackWrite(array, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttWrite

!-------------------------------------------------------------------------
!  CplComp
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttAddPack"
!BOPI
! !IROUTINE: ESMF_CplCompAttAddPack - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_CplCompAttAddPack(comp, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the Attribute package for the {\tt comp}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!      An {\tt ESMF\_CplComp} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4, name5, name6, name7
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      fconvention = convention
      fpurpose = purpose
      fobject = 'comp'

      name1 = 'discipline'
      name2 = 'physical domain'
      name3 = 'agency'
      name4 = 'institution'
      name5 = 'author'
      name6 = 'coding language'
      name7 = 'model component framework'

      call c_ESMC_AttPackCreate(comp%compp%base, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name4, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name5, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name6, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name7, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttAddPack

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_CplCompAttAddPackCustom - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_CplCompAttAddPackCustom(comp, convention, purpose, &
      attrList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      character (len = *), dimension(:), intent(in) :: attrList
      integer, intent(in) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up a custom Attribute package for the {\tt comp}, or adds to an 
!     existing Attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!      An {\tt ESMF\_CplComp} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)
      

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif

      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif

      fobject = 'comp'
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(comp%compp%base, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttAddPackCustom

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetInt4(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer Attribute from the {\tt comp}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetInt4List(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetInt8(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer Attribute from the {\tt comp}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetInt8List(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetReal4(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from the {\tt comp}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetReal4List(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from an {\tt ESMF\_CplComp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetReal8(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from the {\tt comp}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetReal8List(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from an {\tt ESMF\_CplComp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetLogical(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetLogicalList(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The logical values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetChar"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetChar(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [defaultvalue]
!           The character default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetChar(comp%compp%base, name, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetChar(comp%compp%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetCount"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query the number of Attributes
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet() 
      subroutine ESMF_CplCompAttGetCount(comp, count, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of Attributes associated with the given {\tt comp} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [count]
!           The number of Attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      call c_ESMC_AttributeGetCount(comp%compp%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetInfoByName"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Comp Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetInfoByName(comp, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named Attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      call c_ESMC_AttributeGetInfoName(comp%compp%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttGetInfoByNum"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Comp Attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_CplCompAttGetInfoByNum(comp, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed Attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localItemcount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      call c_ESMC_AttributeGetInfoNum(comp%compp%base, attributeIndex, &
        localName, localTk, localItemcount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttGetInfoByNum
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetInt4(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer Attribute to the {\tt comp}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetInt4List(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetInt8(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer Attribute to the {\tt comp}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetInt8List(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetReal4(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real Attribute to the {\tt comp}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetReal4List(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetReal8(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real Attribute to the {\tt comp}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetReal8List(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_TypeKind) :: tk

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetLogical(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The logical true/false value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetLogicalList(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetChar"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetChar(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_CplComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The character value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetChar(comp%compp%base, name, value, &
        ESMF_TYPEKIND_CHARACTER, fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetChar(comp%compp%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetChar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeSet - Link a CplComp Attribute hierarchy with the
!                               {\bf hierarchy of CplComp, GridComp, or State}
!
! !INTERFACE:
!      ! Private name; call using ESMF_AttributeSet()
!      subroutine ESMF_CplCompAttSetLink(comp1, <object>, rc)
!
! !ARGUMENTS:
!      type(ESMF\_CplComp), intent(inout) :: comp1  
!      <object>, see below for supported values
!      integer, intent(out), optional :: rc   
!
!
! !DESCRIPTION:
!     Attaches a {\tt CplComp} Attribute hierarchy to the hierarchy of
!     a {\tt CplComp}, {\tt GridComp}, or {\tt State},. 
!     Supported values for the <object> are:
!     \begin{description}
!     \item type(ESMF\_CplComp), intent(inout) :: comp2
!     \item type(ESMF\_GridComp), intent(inout) :: comp2
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [comp1]
!       An {\tt ESMF\_CplComp} object.
!     \item [<object>]
!       The object with which to link hierarchies.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetLinkCplComp"
!BOP
! !IROUTINE: ESMF_AttributeSet - Link an CplComp Attribute hierarchy to a CplComp
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetLinkCplComp(comp1, comp2, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp1
      type(ESMF_CplComp), intent(inout)  :: comp2
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a CplComp to a CplComp in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [comp1]
!      An {\tt ESMF\_CplComp} object.
!     \item [comp2]
!      An {\tt ESMF\_CplComp} object.
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp1,rc)

      call ESMF_CplCompValidate(comp1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp2,rc)

      call ESMF_CplCompValidate(comp2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(comp1%compp%base, comp2%compp%base, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetLinkCplComp

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetLinkGridComp"
!BOP
! !IROUTINE: ESMF_AttributeSet - Link an CplComp Attribute hierarchy to a GridComp
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetLinkGridComp(comp1, comp2, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp1
      type(ESMF_GridComp), intent(inout)  :: comp2
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a CplComp to a GridComp in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [comp1]
!      An {\tt ESMF\_CplComp} object.
!     \item [comp2]
!      An {\tt ESMF\_GridComp} object.
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp1,rc)

      call ESMF_CplCompValidate(comp1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp2,rc)

      call ESMF_GridCompValidate(comp2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(comp1%compp%base, comp2%compp%base, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetLinkGridComp

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttSetLinkState"
!BOP
! !IROUTINE: ESMF_AttributeSet - Link an CplComp Attribute hierarchy to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_CplCompAttSetLinkState(comp1, state, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp1
      type(ESMF_State), intent(inout)  :: state
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a comp to a Field in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [comp1]
!      An {\tt ESMF\_CplComp} object.
!     \item [state]
!      An {\tt ESMF\_State} object.
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp1,rc)

      call ESMF_CplCompValidate(comp1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(comp1%compp%base, state%statep%base, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttSetLinkState

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompAttWrite"
!BOPI
! !IROUTINE: ESMF_CplCompAttWrite - Print the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeWrite()
      subroutine ESMF_CplCompAttWrite(comp, convention, purpose, &
        attwriteflag, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      type(ESMF_AttWriteFlag), intent(in), optional :: attwriteflag
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Write the Attribute package for the {\tt comp}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!      An {\tt ESMF\_CplComp} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attwriteflag]
!      Flag to determine the format for writing the Attributes.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_AttWriteFlag) :: writeflag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp,rc)

      call ESMF_CplCompValidate(comp, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      if (present(attwriteflag)) then
        writeflag = attwriteflag
      else
        writeflag = ESMF_ATTWRITEFLAG_TAB
      endif
      
      if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_TAB%attwriteflag) then
        call c_ESMC_AttPackWrite(comp%compp%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_XML%attwriteflag) then
        call c_ESMC_AttPackWrite(comp%compp%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompAttWrite

!-------------------------------------------------------------------------
!  GridComp
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttAddPack"
!BOPI
! !IROUTINE: ESMF_GridCompAttAddPack - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_GridCompAttAddPack(comp, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the Attribute package for the {\tt comp}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!      An {\tt ESMF\_GridComp} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4, name5, name6, name7
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      fconvention = convention
      fpurpose = purpose
      fobject = 'comp'

      name1 = 'discipline'
      name2 = 'physical domain'
      name3 = 'agency'
      name4 = 'institution'
      name5 = 'author'
      name6 = 'coding language'
      name7 = 'model component framework'

      call c_ESMC_AttPackCreate(comp%compp%base, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name4, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name5, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name6, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(comp%compp%base, name7, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttAddPack

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_GridCompAttAddPackCustom - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_GridCompAttAddPackCustom(comp, convention, purpose, &
      attrList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      character (len = *), dimension(:), intent(in) :: attrList
      integer, intent(in) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up a custom Attribute package for the {\tt comp}, or adds to an 
!     existing Attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!      An {\tt ESMF\_GridComp} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif

      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif

      fobject = 'comp'
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(comp%compp%base, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttAddPackCustom

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetInt4(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer Attribute from the {\tt comp}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetInt4List(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetInt8(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer Attribute from the {\tt comp}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetInt8List(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetReal4(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from the {\tt comp}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetReal4List(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from an {\tt ESMF\_GridComp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetReal8(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from the {\tt comp}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetReal8List(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from an {\tt ESMF\_GridComp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetLogical(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetLogicalList(comp, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The logical values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetChar"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetChar(comp, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character Attribute from the {\tt comp}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [defaultvalue]
!           The character default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackGetChar(comp%compp%base, name, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetChar(comp%compp%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetCount"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query the number of Attributes
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet() 
      subroutine ESMF_GridCompAttGetCount(comp, count, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of Attributes associated with the given {\tt comp} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [count]
!           The number of Attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      call c_ESMC_AttributeGetCount(comp%compp%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetInfoByName"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Comp Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetInfoByName(comp, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named Attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      call c_ESMC_AttributeGetInfoName(comp%compp%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttGetInfoByNum"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Comp Attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridCompAttGetInfoByNum(comp, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed Attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localItemcount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      call c_ESMC_AttributeGetInfoNum(comp%compp%base, attributeIndex, &
        localName, localTk, localItemcount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttGetInfoByNum
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetInt4(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer Attribute to the {\tt comp}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetInt4List(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetInt8(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer Attribute to the {\tt comp}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetInt8List(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetReal4(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real Attribute to the {\tt comp}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetReal4List(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetReal8(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real Attribute to the {\tt comp}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetReal8List(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_TypeKind) :: tk

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetLogical(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The logical true/false value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetLogicalList(comp, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(comp%compp%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetChar"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetChar(comp, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character Attribute to the {\tt comp}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!           An {\tt ESMF\_GridComp} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The character value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      call c_ESMC_AttPackSetChar(comp%compp%base, name, value, &
        ESMF_TYPEKIND_CHARACTER, fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetChar(comp%compp%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetChar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeSet - Link a GridComp Attribute hierarchy with the
!                               {\bf hierarchy of CplComp, GridComp, or State}
!
! !INTERFACE:
!      ! Private name; call using ESMF_AttributeSet()
!      subroutine ESMF_GridCompAttSetLink(comp1, <object>, rc)
!
! !ARGUMENTS:
!      type(ESMF\_GridComp), intent(inout) :: comp1  
!      <object>, see below for supported values
!      integer, intent(out), optional :: rc   
!
!
! !DESCRIPTION:
!     Attaches a {\tt GridComp} Attribute hierarchy to the hierarchy of
!     a {\tt CplComp}, {\tt GridComp}, or {\tt State},. 
!     Supported values for the <object> are:
!     \begin{description}
!     \item type(ESMF\_CplComp), intent(inout) :: comp2
!     \item type(ESMF\_GridComp), intent(inout) :: comp2
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [comp1]
!       An {\tt ESMF\_GridComp} object.
!     \item [<object>]
!       The object with which to link hierarchies.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetLinkCplComp"
!BOP
! !IROUTINE: ESMF_AttributeSet - Link an GridComp Attribute hierarchy to a CplComp
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetLinkCplComp(comp1, comp2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp1
      type(ESMF_CplComp), intent(inout)  :: comp2
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a GridComp to a CplComp in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [comp1]
!      An {\tt ESMF\_GridComp} object.
!     \item [comp2]
!      An {\tt ESMF\_CplComp} object.
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp1,rc)

      call ESMF_GridCompValidate(comp1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,comp2,rc)

      call ESMF_CplCompValidate(comp2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(comp1%compp%base, comp2%compp%base, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetLinkCplComp

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetLinkGridComp"
!BOP
! !IROUTINE: ESMF_AttributeSet - Link an GridComp Attribute hierarchy to a GridComp
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetLinkGridComp(comp1, comp2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp1
      type(ESMF_GridComp), intent(inout)  :: comp2
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a GridComp to a GridComp in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [comp1]
!      An {\tt ESMF\_GridComp} object.
!     \item [comp2]
!      An {\tt ESMF\_GridComp} object.
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp1,rc)

      call ESMF_GridCompValidate(comp1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp2,rc)

      call ESMF_GridCompValidate(comp2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(comp1%compp%base, comp2%compp%base, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetLinkGridComp

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttSetLinkState"
!BOP
! !IROUTINE: ESMF_AttributeSet - Link an GridComp Attribute hierarchy to a State
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridCompAttSetLinkState(comp1, state, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp1
      type(ESMF_State), intent(inout)  :: state
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a comp to a Field in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [comp1]
!      An {\tt ESMF\_GridComp} object.
!     \item [state]
!      An {\tt ESMF\_State} object.
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp1,rc)

      call ESMF_GridCompValidate(comp1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(comp1%compp%base, state%statep%base, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttSetLinkState

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompAttWrite"
!BOPI
! !IROUTINE: ESMF_GridCompAttWrite - Print the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeWrite()
      subroutine ESMF_GridCompAttWrite(comp, convention, purpose, &
        attwriteflag, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: comp  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      type(ESMF_AttWriteFlag), intent(in), optional :: attwriteflag
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Write the Attribute package for the {\tt comp}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [comp]
!      An {\tt ESMF\_GridComp} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attwriteflag]
!      Flag to determine the format for writing the Attributes.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_AttWriteFlag) :: writeflag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,comp,rc)

      call ESMF_GridCompValidate(comp, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'comp'

      if (present(attwriteflag)) then
        writeflag = attwriteflag
      else
        writeflag = ESMF_ATTWRITEFLAG_TAB
      endif
      
      if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_TAB%attwriteflag) then
        call c_ESMC_AttPackWrite(comp%compp%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_XML%attwriteflag) then
        call c_ESMC_AttPackWrite(comp%compp%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridCompAttWrite

!-------------------------------------------------------------------------
!  FIELD
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttAddPack"
!BOPI
! !IROUTINE: ESMF_FieldAttAddPack - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_FieldAttAddPack(field, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the Attribute package for the {\tt field}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!      An {\tt ESMF\_Field} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      fconvention = convention
      fpurpose = purpose
      fobject = 'field'

      name1 = 'shortname'
      name2 = 'longname'
      name3 = 'units'
      name4 = 'dimensions'

      call c_ESMC_AttPackCreate(field%ftypep%base, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(field%ftypep%base, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(field%ftypep%base, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(field%ftypep%base, name4, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttAddPack

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_FieldAttAddPackCustom - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_FieldAttAddPackCustom(field, convention, purpose, &
      attrList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      character (len = *), dimension(:), intent(in) :: attrList
      integer, intent(in) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up a custom Attribute package for the {\tt field}, or adds to an 
!     existing Attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!      An {\tt ESMF\_Field} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif

      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif

      fobject = 'field'
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(field%ftypep%base, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttAddPackCustom

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetInt4(field, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer Attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetInt4List(field, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list Attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetInt8(field, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer Attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetInt8List(field, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list Attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetReal4(field, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetReal4List(field, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetReal8(field, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetReal8List(field, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetLogical(field, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical Attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetLogicalList(field, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list Attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The logical values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetChar"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetChar(field, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character Attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [defaultvalue]
!           The character default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackGetChar(field%ftypep%base, name, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetChar(field%ftypep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetCount"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query the number of Attributes
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet() 
      subroutine ESMF_FieldAttGetCount(field, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of Attributes associated with the given {\tt field} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [count]
!           The number of Attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_AttributeGetCount(field%ftypep%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetInfoByName"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Field Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetInfoByName(field, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named Attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_AttributeGetInfoName(field%ftypep%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetInfoByNum"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Field Attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FieldAttGetInfoByNum(field, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed Attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localItemcount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call c_ESMC_AttributeGetInfoNum(field%ftypep%base, attributeIndex, &
        localName, localTk, localItemcount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetInfoByNum
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetInt4(field, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer Attribute to the {\tt field}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetInt4List(field, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list Attribute to the {\tt field}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetInt8(field, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer Attribute to the {\tt field}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetInt8List(field, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list Attribute to the {\tt field}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetReal4(field, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real Attribute to the {\tt field}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetReal4List(field, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list Attribute to the {\tt field}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetReal8(field, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real Attribute to the {\tt field}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetReal8List(field, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list Attribute to the {\tt field}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_TypeKind) :: tk

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetLogical(field, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical Attribute to the {\tt field}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The logical true/false value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetLogicalList(field, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list Attribute to the {\tt field}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(field%ftypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetChar"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldAttSetChar(field, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character Attribute to the {\tt field}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The character value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      call c_ESMC_AttPackSetChar(field%ftypep%base, name, value, &
        ESMF_TYPEKIND_CHARACTER, fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetChar(field%ftypep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttSetChar

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttWrite"
!BOPI
! !IROUTINE: ESMF_AttributeWrite - Print the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeWrite()
      subroutine ESMF_FieldAttWrite(field, convention, purpose, &
        attwriteflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      type(ESMF_AttWriteFlag), intent(in), optional :: attwriteflag
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Write the Attribute package for the {\tt field}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!      An {\tt ESMF\_Field} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attwriteflag]
!      Flag to determine the format for writing the Attributes.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_AttWriteFlag) :: writeflag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      call ESMF_FieldValidate(field, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'

      if (present(attwriteflag)) then
        writeflag = attwriteflag
      else
        writeflag = ESMF_ATTWRITEFLAG_TAB
      endif
      
      if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_TAB%attwriteflag) then
        call c_ESMC_AttPackWrite(field%ftypep%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_XML%attwriteflag) then
        call c_ESMC_AttPackWrite(field%ftypep%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttWrite

!-------------------------------------------------------------------------
!  FIELDBUNDLE
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttAddPack"
!BOPI
! !IROUTINE: ESMF_FBundleAttAddPack - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_FBundleAttAddPack(fieldbundle, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the Attribute package for the {\tt fieldbundle}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!      An {\tt ESMF\_FieldBundle} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      fconvention = convention
      fpurpose = purpose
      fobject = 'fieldbundle'

      name1 = 'shortname'
      name2 = 'longname'
      name3 = 'units'
      name4 = 'dimensions'

      call c_ESMC_AttPackCreate(fieldbundle%btypep%base, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(fieldbundle%btypep%base, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(fieldbundle%btypep%base, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(fieldbundle%btypep%base, name4, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttAddPack

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_FBundleAttAddPackCustom - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_FBundleAttAddPackCustom(fieldbundle, convention, purpose, &
      attrList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      character (len = *), dimension(:), intent(in) :: attrList
      integer, intent(in) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up a custom Attribute package for the {\tt fieldbundle}, or adds to an 
!     existing Attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!      An {\tt ESMF\_FieldBundle} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif

      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif

      fobject = 'fieldbundle'
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(fieldbundle%btypep%base, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttAddPackCustom

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetInt4(fieldbundle, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer Attribute from the {\tt fieldbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetInt4List(fieldbundle, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list Attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetInt8(fieldbundle, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer Attribute from the {\tt fieldbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetInt8List(fieldbundle, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list Attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetReal4(fieldbundle, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from the {\tt fieldbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetReal4List(fieldbundle, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from an {\tt ESMF\_FieldBundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetReal8(fieldbundle, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from the {\tt fieldbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetReal8List(fieldbundle, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from an {\tt ESMF\_FieldBundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetLogical(fieldbundle, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical Attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetLogicalList(fieldbundle, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list Attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The logical values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetChar"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetChar(fieldbundle, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character Attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [defaultvalue]
!           The character default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackGetChar(fieldbundle%btypep%base, name, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetChar(fieldbundle%btypep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetCount"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query the number of Attributes
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet() 
      subroutine ESMF_FBundleAttGetCount(fieldbundle, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of Attributes associated with the given {\tt fieldbundle} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [count]
!           The number of Attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      call c_ESMC_AttributeGetCount(fieldbundle%btypep%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetInfoByName"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query FBundle Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetInfoByName(fieldbundle, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named Attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      call c_ESMC_AttributeGetInfoName(fieldbundle%btypep%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttGetInfoByNum"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query FBundle Attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_FBundleAttGetInfoByNum(fieldbundle, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed Attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localItemcount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      call c_ESMC_AttributeGetInfoNum(fieldbundle%btypep%base, attributeIndex, &
        localName, localTk, localItemcount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttGetInfoByNum
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetInt4(fieldbundle, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer Attribute to the {\tt fieldbundle}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetInt4List(fieldbundle, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list Attribute to the {\tt fieldbundle}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetInt8(fieldbundle, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer Attribute to the {\tt fieldbundle}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetInt8List(fieldbundle, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list Attribute to the {\tt fieldbundle}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetReal4(fieldbundle, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real Attribute to the {\tt fieldbundle}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetReal4List(fieldbundle, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list Attribute to the {\tt fieldbundle}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetReal8(fieldbundle, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real Attribute to the {\tt fieldbundle}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetReal8List(fieldbundle, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list Attribute to the {\tt fieldbundle}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_TypeKind) :: tk

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetLogical(fieldbundle, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical Attribute to the {\tt fieldbundle}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The logical true/false value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetLogicalList(fieldbundle, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list Attribute to the {\tt fieldbundle}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(fieldbundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetChar"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetChar(fieldbundle, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character Attribute to the {\tt fieldbundle}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The character value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      call c_ESMC_AttPackSetChar(fieldbundle%btypep%base, name, value, &
        ESMF_TYPEKIND_CHARACTER, fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetChar(fieldbundle%btypep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetChar

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttSetLinkField"
!BOP
! !IROUTINE: ESMF_AttributeSet - Link an FBundle Attribute hierarchy to a Field
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FBundleAttSetLinkField(fieldbundle, field, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle
      type(ESMF_Field), intent(inout)  :: field
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a FieldBundle to a Field in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!      An {\tt ESMF\_FieldBundle} object.
!     \item [field]
!      An {\tt ESMF\_Field} object.
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      call ESMF_FieldBundleValidate(fieldbundle, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(fieldbundle%btypep%base, field%ftypep%base, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttSetLinkField

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FBundleAttWrite"
!BOPI
! !IROUTINE: ESMF_FBundleAttWrite - Print the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeWrite()
      subroutine ESMF_FBundleAttWrite(fieldbundle, convention, purpose, &
        attwriteflag, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      type(ESMF_AttWriteFlag), intent(in), optional :: attwriteflag
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Write the Attribute package for the {\tt fieldbundle}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!      An {\tt ESMF\_FieldBundle} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attwriteflag]
!      Flag to determine the format for writing the Attributes.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_AttWriteFlag) :: writeflag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      call ESMF_FieldBundleValidate(fieldbundle, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'fieldbundle'

      if (present(attwriteflag)) then
        writeflag = attwriteflag
      else
        writeflag = ESMF_ATTWRITEFLAG_TAB
      endif
      
      if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_TAB%attwriteflag) then
        call c_ESMC_AttPackWrite(fieldbundle%btypep%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_XML%attwriteflag) then
        call c_ESMC_AttPackWrite(fieldbundle%btypep%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FBundleAttWrite

!-------------------------------------------------------------------------
!  GRID
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttAddPack"
!BOPI
! !IROUTINE: ESMF_GridAttAddPack - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_GridAttAddPack(grid, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the Attribute package for the {\tt grid}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!      An {\tt ESMF\_Grid} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      fconvention = convention
      fpurpose = purpose
      fobject = 'grid'

      name1 = 'shortname'
      name2 = 'longname'
      name3 = 'units'
      name4 = 'dimensions'

      call c_ESMC_AttPackCreate(grid, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(grid, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(grid, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(grid, name4, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttAddPack

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_GridAttAddPackCustom - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_GridAttAddPackCustom(grid, convention, purpose, &
      attrList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      character (len = *), dimension(:), intent(in) :: attrList
      integer, intent(in) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up a custom Attribute package for the {\tt grid}, or adds to an 
!     existing Attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!      An {\tt ESMF\_Grid} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif

      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif

      fobject = 'grid'
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(grid, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttAddPackCustom

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetInt4(grid, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer Attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetInt4List(grid, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list Attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetInt8(grid, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer Attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetInt8List(grid, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list Attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetReal4(grid, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetReal4List(grid, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from an {\tt ESMF\_Grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetReal8(grid, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetReal8List(grid, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from an {\tt ESMF\_Grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetLogical(grid, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical Attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetLogicalList(grid, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list Attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The logical values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetChar"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetChar(grid, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character Attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [defaultvalue]
!           The character default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackGetChar(grid, name, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetChar(grid, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetCount"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query the number of Attributes
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet() 
      subroutine ESMF_GridAttGetCount(grid, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of Attributes associated with the given {\tt grid} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [count]
!           The number of Attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      call c_ESMC_AttributeGetCount(grid, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetInfoByName"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Grid Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetInfoByName(grid, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named Attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      call c_ESMC_AttributeGetInfoName(grid, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetInfoByNum"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query Grid Attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_GridAttGetInfoByNum(grid, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed Attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localItemcount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      call c_ESMC_AttributeGetInfoNum(grid, attributeIndex, &
        localName, localTk, localItemcount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInfoByNum
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetInt4(grid, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer Attribute to the {\tt grid}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetInt4List(grid, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list Attribute to the {\tt grid}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetInt8(grid, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer Attribute to the {\tt grid}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetInt8List(grid, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list Attribute to the {\tt grid}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetReal4(grid, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real Attribute to the {\tt grid}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetReal4List(grid, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list Attribute to the {\tt grid}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetReal8(grid, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real Attribute to the {\tt grid}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetReal8List(grid, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list Attribute to the {\tt grid}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_TypeKind) :: tk

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetLogical(grid, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical Attribute to the {\tt grid}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The logical true/false value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetLogicalList(grid, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list Attribute to the {\tt grid}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetChar"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_GridAttSetChar(grid, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character Attribute to the {\tt grid}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The character value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      call c_ESMC_AttPackSetChar(grid, name, value, &
        ESMF_TYPEKIND_CHARACTER, fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetChar(grid, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttSetChar

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttWrite"
!BOPI
! !IROUTINE: ESMF_GridAttWrite - Print the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeWrite()
      subroutine ESMF_GridAttWrite(grid, convention, purpose, &
        attwriteflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      type(ESMF_AttWriteFlag), intent(in), optional :: attwriteflag
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Write the Attribute package for the {\tt grid}.
!     The Attribute package defines the convention, purpose, and object type
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!      An {\tt ESMF\_Grid} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attwriteflag]
!      Flag to determine the format for writing the Attributes.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_AttWriteFlag) :: writeflag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'grid'

      if (present(attwriteflag)) then
        writeflag = attwriteflag
      else
        writeflag = ESMF_ATTWRITEFLAG_TAB
      endif
      
      if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_TAB%attwriteflag) then
        call c_ESMC_AttPackWrite(grid, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_XML%attwriteflag) then
        call c_ESMC_AttPackWrite(grid, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttWrite

!-------------------------------------------------------------------------
!  STATE
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttAddPack"
!BOPI
! !IROUTINE: ESMF_StateAttAddPack - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_StateAttAddPack(state, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the Attribute package for the {\tt state}.
!     The Attribute package defines the convention, purpose, and object type 
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      fconvention = convention
      fpurpose = purpose
      fobject = 'state'

      name1 = 'shortname'
      name2 = 'longname'
      name3 = 'organization'
      name4 = 'discipline'

      call c_ESMC_AttPackCreate(state%statep%base, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(state%statep%base, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(state%statep%base, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(state%statep%base, name4, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttAddPack

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttributeCopyAll"
!BOPI
! !IROUTINE: ESMF_StateAttributeCopyAll - Copy an Attribute hierarchy between states
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeCopy()
      subroutine ESMF_StateAttributeCopyAll(state1, state2, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state1  
      type(ESMF_State), intent(inout) :: state2 
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Copy all Attributes in one {\tt state} hierarchy to another.
!
!     The arguments are:
!     \begin{description}
!     \item [state1]
!      An {\tt ESMF\_State} object.
!     \item [state2]
!      An {\tt ESMF\_State} object.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state1,rc)
      
      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state2,rc)

      call ESMF_StateValidate(state1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      
      call ESMF_StateValidate(state2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeCopyAll(state1%statep%base, state2%statep%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttributeCopyAll

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_StateAttAddPackCustom - Create the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeAdd()
      subroutine ESMF_StateAttAddPackCustom(state, convention, purpose, &
      attrList, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      character (len = *), dimension(:), intent(in) :: attrList
      integer, intent(in) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up a custom Attribute package for the {\tt state}, or adds to an 
!     existing Attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined Attributes
!     \item [count]
!      The count of the number of Attributes in a user specified Attribute package
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif

      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif

      fobject = 'state'
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(state%statep%base, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttAddPackCustom

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetInt4(state, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer Attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetInt4List(state, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list Attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeGet  - Retrieve an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetInt8(state, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer Attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetInt8List(state, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list Attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The integer values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetReal4(state, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetReal4List(state, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real Attribute from an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetReal8(state, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetReal8List(state, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real Attribute from an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The real values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetLogical(state, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical Attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetLogicalList(state, name, count, valueList, &
        defaultvalue, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list Attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the Attribute.
!     \item [valueList]
!           The logical values of the named Attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          valueList = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetChar"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetChar(state, name, value, defaultvalue, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character Attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [defaultvalue]
!           The character default value of the named Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackGetChar(state%statep%base, name, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if
                                
      else
      
      call c_ESMC_AttributeGetChar(state%statep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetCount"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query the number of Attributes
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet() 
      subroutine ESMF_StateAttGetCount(state, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of Attributes associated with the given {\tt state} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [count]
!           The number of Attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call c_ESMC_AttributeGetCount(state%statep%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetInfoByName"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query State Attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetInfoByName(state, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named Attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call c_ESMC_AttributeGetInfoName(state%statep%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetInfoByNum"

!BOPI
! !IROUTINE: ESMF_AttributeGet - Query State Attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGet()
      subroutine ESMF_StateAttGetInfoByNum(state, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed Attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [attributeIndex]
!           The index number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [{[typekind]}]
!           The typekind of the Attribute.
!     \item [{[count]}]
!           Returns the number of items in this Attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localItemcount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call c_ESMC_AttributeGetInfoNum(state%statep%base, attributeIndex, &
        localName, localTk, localItemcount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetInfoByNum
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetInt4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetInt4(state, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer Attribute to the {\tt state}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetInt4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetInt4List(state, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list Attribute to the {\tt state}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetInt8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetInt8(state, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer Attribute to the {\tt state}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The integer value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetInt8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetInt8List(state, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list Attribute to the {\tt state}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetReal4"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetReal4(state, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real Attribute to the {\tt state}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetReal4List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a 4-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetReal4List(state, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list Attribute to the {\tt state}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetReal8"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetReal8(state, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real Attribute to the {\tt state}.
!      The Attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The real value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetReal8List"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set an 8-byte real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetReal8List(state, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list Attribute to the {\tt state}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_TypeKind) :: tk

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetLogical"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetLogical(state, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical Attribute to the {\tt state}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The logical true/false value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetLogicalList"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetLogicalList(state, name, count, valueList, &
        convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list Attribute to the {\tt state}.
!     The Attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, &
        fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetChar"

!BOPI
! !IROUTINE: ESMF_AttributeSet - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetChar(state, name, value, convention, &
        purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character Attribute to the {\tt state}.
!     The Attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the Attribute to add.
!     \item [value]
!           The character value of the Attribute to add.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      if (present(convention) .OR. present(purpose)) then
      
      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'state'

      call c_ESMC_AttPackSetChar(state%statep%base, name, value, &
        ESMF_TYPEKIND_CHARACTER, fconvention, fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
      else
      
      call c_ESMC_AttributeSetChar(state%statep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetChar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeSet - Link a State Attribute hierarchy with the
!                               {\bf hierarchy of Bundle, Field, or State}
!
! !INTERFACE:
!      ! Private name; call using ESMF_AttributeSet()
!      subroutine ESMF_StateAttributeSetLink(state, <object>, rc)
!
! !ARGUMENTS:
!      type(ESMF\_State), intent(inout) :: state  
!      <object>, see below for supported values
!      integer, intent(out), optional :: rc   
!
!
! !DESCRIPTION:
!     Attaches a {\tt State} Attribute hierarchy to the hierarchy of
!     a {\tt Fieldbundle}, {\tt Field}, or another {\tt State},. 
!     Supported values for the <object> are:
!     \begin{description}
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [<object>]
!       The object with which to link hierarchies.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetLinkFB"
!BOPI
! !IROUTINE: ESMF_AttributeSet - Link a State to a FieldBundle in an Attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetLinkFB(state, bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_FieldBundle), intent(inout)  :: bundle
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a state to a FieldBundle in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [bundle]
!      An {\tt ESMF\_FieldBundle} object.
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(state%statep%base, bundle%btypep%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetLinkFB

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetLinkField"
!BOPI
! !IROUTINE: ESMF_AttributeSet - Link a State to a Field in an Attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetLinkField(state, field, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout)  :: field
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a State to a Field in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [field]
!      An {\tt ESMF\_Field} object.
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(state%statep%base, field%ftypep%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetLinkField

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetLinkState"
!BOPI
! !IROUTINE: ESMF_AttributeSet - Link a State to a State in an Attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_StateAttSetLinkState(state1, state2, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state1
      type(ESMF_State), intent(inout)  :: state2
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a State to a State in an Attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [state1]
!      An {\tt ESMF\_State} object.
!     \item [state2]
!      An {\tt ESMF\_State} object.
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state1,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state2,rc)

      call ESMF_StateValidate(state1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
                                  
      call ESMF_StateValidate(state2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetLink(state1%statep%base, state2%statep%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttSetLinkState

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttWrite"
!BOPI
! !IROUTINE: ESMF_StateAttWrite - Print the Attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeWrite()
      subroutine ESMF_StateAttWrite(state, convention, purpose, &
        attwriteflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      type(ESMF_AttWriteFlag), intent(in), optional :: attwriteflag
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Write the Attribute package for the {\tt state}.
!     The Attribute package defines the convention, purpose, and object type 
!     of the associated Attributes.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [convention]
!      The convention of the Attribute package.
!     \item [purpose]
!      The purpose of the Attribute package.
!     \item [attwriteflag]
!      Flag to determine the format for writing the Attributes.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject
      type(ESMF_AttWriteFlag) :: writeflag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'field'
      
      if (present(attwriteflag)) then
        writeflag = attwriteflag
      else
        writeflag = ESMF_ATTWRITEFLAG_TAB
      endif
      
      if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_TAB%attwriteflag) then
        call c_ESMC_AttPackWrite(state%statep%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      else if (writeflag%attwriteflag .eq. ESMF_ATTWRITEFLAG_XML%attwriteflag) then
        call c_ESMC_AttPackWrite(state%statep%base, fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttWrite
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AttributeWrite  - Write an Attribute package
!
! !INTERFACE:
!     subroutine ESMF_AttributeWrite(<object>, convention, purpose, rc)
!
! !ARGUMENTS:
!     <object>, see below for supported values  
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Write the Attribute package for object.  The Attribute package defines 
!     the convention, purpose, and object type of the associated Attributes.
!     Supported values for <object> are:
!     \begin{description}
!     \item type(ESMF\_Array), intent(inout) :: array
!     \item type(ESMF\_CplComp), intent(inout) :: comp  
!     \item type(ESMF\_GridComp), intent(inout) :: comp  
!     \item type(ESMF\_Field), intent(inout) :: field  
!     \item type(ESMF\_FieldBundle), intent(inout) :: fbundle 
!     \item type(ESMF\_Grid), intent(inout) :: grid
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [<object>]
!           An {\tt ESMF} object.
!     \item [convention]
!           The convention of the Attribute package.
!     \item [purpose]
!           The purpose of the Attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
!------------------------------------------------------------------------------

end module ESMF_AttributeMod

