! $Id: ESMF_Attribute.F90,v 1.2 2008/04/28 06:38:19 rokuingh Exp $
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
  use ESMF_FieldMod
  use ESMF_FieldBundleMod
  use ESMF_GridMod
  use ESMF_StateMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
!    ! Dummy structure which must just be big enough to hold the values.
!    ! actual data values will always be accessed on the C++ side.

  type ESMF_Attribute
  sequence
  private
    character(len=ESMF_MAXSTR)  :: attr_name
    type(ESMF_DataValue)        :: attr_value
  end type


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
               '$Id: ESMF_Attribute.F90,v 1.2 2008/04/28 06:38:19 rokuingh Exp $'
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
        module procedure ESMF_FieldAttAddPack
        module procedure ESMF_FieldAttAddPackCustom
        module procedure ESMF_FieldBundleAttAddPack
        module procedure ESMF_FieldBundleAttAddPackCustom
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
        module procedure ESMF_FieldBundleAttGetInt4
        module procedure ESMF_FieldBundleAttGetInt4List
        module procedure ESMF_FieldBundleAttGetInt8
        module procedure ESMF_FieldBundleAttGetInt8List
        module procedure ESMF_FieldBundleAttGetReal4
        module procedure ESMF_FieldBundleAttGetReal4List
        module procedure ESMF_FieldBundleAttGetReal8
        module procedure ESMF_FieldBundleAttGetReal8List
        module procedure ESMF_FieldBundleAttGetLogical
        module procedure ESMF_FieldBundleAttGetLogicalList
        module procedure ESMF_FieldBundleAttGetChar
        module procedure ESMF_FieldBundleAttGetInfoByName
        module procedure ESMF_FieldBundleAttGetInfoByNum
        module procedure ESMF_FieldBundleAttGetCount
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
        module procedure ESMF_FieldBundleAttSetInt4
        module procedure ESMF_FieldBundleAttSetInt4List
        module procedure ESMF_FieldBundleAttSetInt8
        module procedure ESMF_FieldBundleAttSetInt8List
        module procedure ESMF_FieldBundleAttSetReal4
        module procedure ESMF_FieldBundleAttSetReal4List
        module procedure ESMF_FieldBundleAttSetReal8
        module procedure ESMF_FieldBundleAttSetReal8List
        module procedure ESMF_FieldBundleAttSetLogical
        module procedure ESMF_FieldBundleAttSetLogicalList
        module procedure ESMF_FieldBundleAttSetChar
        module procedure ESMF_FieldBundleAttSetLinkField
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
        module procedure ESMF_FieldAttWrite
        module procedure ESMF_FieldBundleAttWrite
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

!-------------------------------------------------------------------------
!  ARRAY
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttAddPack"
!BOPI
! !IROUTINE: ESMF_ArrayAttAddPack - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttAdd()
      subroutine ESMF_ArrayAttAddPack(array, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt array}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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

      name1 = 'longname'
      name2 = 'shortname'
      name3 = 'units'
      name4 = 'coordinates'

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
! !IROUTINE: ESMF_ArrayAttAddPackCustom - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttAdd()
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
!     Sets up a custom attribute package for the {\tt array}, or adds to an existing 
!     attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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
!BOP
! !IROUTINE: ESMF_ArrayAttGet  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_ArrayAttGet(array, name, <value argument>, &
!                            <defaultvalue argument>, convention, purpose, rc)
!
! !ARGUMENTS:
!     type(ESMF_Array), intent(inout) :: array  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an attribute from the {\tt array}.
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
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [<defaultvalue argument>]
!           The default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttGetInt4"

!BOPI
! !IROUTINE: ESMF_ArrayAttGet  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!     Returns an integer attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns a 4-byte integer list attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!     Returns an 8-byte integer attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns an 8-byte integer list attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns a 4-byte real attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns a 4-byte real attribute from an {\tt ESMF\_Array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns an 8-byte real attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns an 8-byte real attribute from an {\tt ESMF\_Array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns a logical attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns a logical list attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!      Returns a character attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [defaultvalue]
!           The character default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

!BOP
! !IROUTINE: ESMF_ArrayAttGet - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_ArrayAttGetCount(array, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of attributes associated with the given {\tt array} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

!BOP
! !IROUTINE: ESMF_ArrayAttGet - Query Array attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
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
!     Returns information associated with the named attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

!BOP
! !IROUTINE: ESMF_ArrayAttGet - Query Array attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttGet()
      subroutine ESMF_ArrayAttGetInfoByNum(array, attributeIndex, name, &
        typekind, itemcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: itemcount   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[itemcount]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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
      if (present(itemcount)) itemcount = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttGetInfoByNum
      
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayAttSet - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_ArrayAttSet(array, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Array), intent(inout) :: array  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt array}, or set an attribute on an 
!     attribute package.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList} and a {\tt convention} and {\tt purpose}.
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
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttSetInt4"

!BOPI
! !IROUTINE: ESMF_ArrayAttSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!      Attaches a 4-byte integer attribute to the {\tt array}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!     Attaches a 4-byte integer list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!      Attaches an 8-byte integer attribute to the {\tt array}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!     Attaches an 8-byte integer list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!      Attaches a 4-byte real attribute to the {\tt array}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!     Attaches a 4-byte real list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!      Attaches an 8-byte real attribute to the {\tt array}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!     Attaches an 8-byte real list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!     Attaches a logical attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!     Attaches a logical list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_ArrayAttSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttSet()
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
!      Attaches a character attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
!BOP
! !IROUTINE: ESMF_ArrayAttWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttWrite()
      subroutine ESMF_ArrayAttWrite(array, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Print the attribute package for the {\tt array}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

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

      call c_ESMC_AttPackWrite(array, fconvention, &
        fpurpose, fobject, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttWrite

!-------------------------------------------------------------------------
!  FIELD
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttAddPack"
!BOPI
! !IROUTINE: ESMF_FieldAttAddPack - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttAdd()
      subroutine ESMF_FieldAttAddPack(field, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt field}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!      An {\tt ESMF\_Field} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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

      name1 = 'longname'
      name2 = 'shortname'
      name3 = 'units'
      name4 = 'coordinates'

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
! !IROUTINE: ESMF_FieldAttAddPackCustom - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttAdd()
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
!     Sets up a custom attribute package for the {\tt field}, or adds to an existing 
!     attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!      An {\tt ESMF\_Field} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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
!BOP
! !IROUTINE: ESMF_FieldAttGet  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_FieldAttGet(field, name, <value argument>, &
!                            <defaultvalue argument>, convention, purpose, rc)
!
! !ARGUMENTS:
!     type(ESMF_Field), intent(inout) :: field  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an attribute from the {\tt field}.
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
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [<defaultvalue argument>]
!           The default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttGetInt4"

!BOPI
! !IROUTINE: ESMF_FieldAttGet  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!     Returns an integer attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns a 4-byte integer list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!     Returns an 8-byte integer attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns an 8-byte integer list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns a 4-byte real attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns a 4-byte real attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns an 8-byte real attribute from the {\tt field}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns an 8-byte real attribute from an {\tt ESMF\_Field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns a logical attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns a logical list attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!      Returns a character attribute from the {\tt field}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [defaultvalue]
!           The character default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

!BOP
! !IROUTINE: ESMF_FieldAttGet - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_FieldAttGetCount(field, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of attributes associated with the given {\tt field} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

!BOP
! !IROUTINE: ESMF_FieldAttGet - Query Field attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
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
!     Returns information associated with the named attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

!BOP
! !IROUTINE: ESMF_FieldAttGet - Query Field attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttGet()
      subroutine ESMF_FieldAttGetInfoByNum(field, attributeIndex, name, &
        typekind, itemcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: itemcount   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[itemcount]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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
      if (present(itemcount)) itemcount = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttGetInfoByNum
      
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldAttSet - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_FieldAttSet(field, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Field), intent(inout) :: field  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt field}, or set an attribute on an 
!     attribute package.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList} and a {\tt convention} and {\tt purpose}.
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
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldAttSetInt4"

!BOPI
! !IROUTINE: ESMF_FieldAttSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!      Attaches a 4-byte integer attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!     Attaches a 4-byte integer list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!      Attaches an 8-byte integer attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!     Attaches an 8-byte integer list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!      Attaches a 4-byte real attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!     Attaches a 4-byte real list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!      Attaches an 8-byte real attribute to the {\tt field}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!     Attaches an 8-byte real list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!     Attaches a logical attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!     Attaches a logical list attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_FieldAttSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttSet()
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
!      Attaches a character attribute to the {\tt field}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
!BOP
! !IROUTINE: ESMF_FieldAttWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttWrite()
      subroutine ESMF_FieldAttWrite(field, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Print the attribute package for the {\tt field}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!      An {\tt ESMF\_Field} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

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

      call c_ESMC_AttPackWrite(field%ftypep%base, fconvention, &
        fpurpose, fobject, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAttWrite

!-------------------------------------------------------------------------
!  FIELDBUNDLE
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttAddPack"
!BOPI
! !IROUTINE: ESMF_FieldBundleAttAddPack - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttAdd()
      subroutine ESMF_FieldBundleAttAddPack(fieldbundle, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt fieldbundle}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!      An {\tt ESMF\_FieldBundle} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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

      name1 = 'longname'
      name2 = 'shortname'
      name3 = 'units'
      name4 = 'coordinates'

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

      end subroutine ESMF_FieldBundleAttAddPack

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_FieldBundleAttAddPackCustom - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttAdd()
      subroutine ESMF_FieldBundleAttAddPackCustom(fieldbundle, convention, purpose, &
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
!     Sets up a custom attribute package for the {\tt fieldbundle}, or adds to an existing 
!     attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!      An {\tt ESMF\_FieldBundle} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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

      end subroutine ESMF_FieldBundleAttAddPackCustom

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldBundleAttGet  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_FieldBundleAttGet(fieldbundle, name, <value argument>, &
!                            <defaultvalue argument>, convention, purpose, rc)
!
! !ARGUMENTS:
!     type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an attribute from the {\tt fieldbundle}.
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
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [<defaultvalue argument>]
!           The default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetInt4"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetInt4(fieldbundle, name, value, defaultvalue, &
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
!     Returns an integer attribute from the {\tt fieldbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetInt4List"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetInt4List(fieldbundle, name, count, valueList, &
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
!      Returns a 4-byte integer list attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetInt8"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetInt8(fieldbundle, name, value, defaultvalue, &
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
!     Returns an 8-byte integer attribute from the {\tt fieldbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetInt8List"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetInt8List(fieldbundle, name, count, valueList, &
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
!      Returns an 8-byte integer list attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetReal4"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetReal4(fieldbundle, name, value, defaultvalue, &
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
!      Returns a 4-byte real attribute from the {\tt fieldbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetReal4List"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetReal4List(fieldbundle, name, count, valueList, &
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
!      Returns a 4-byte real attribute from an {\tt ESMF\_FieldBundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetReal8"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetReal8(fieldbundle, name, value, defaultvalue, &
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
!      Returns an 8-byte real attribute from the {\tt fieldbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetReal8List"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetReal8List(fieldbundle, name, count, valueList, &
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
!      Returns an 8-byte real attribute from an {\tt ESMF\_FieldBundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetLogical"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetLogical(fieldbundle, name, value, defaultvalue, &
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
!      Returns a logical attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetLogicalList"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetLogicalList(fieldbundle, name, count, valueList, &
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
!      Returns a logical list attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetChar"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetChar(fieldbundle, name, value, defaultvalue, &
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
!      Returns a character attribute from the {\tt fieldbundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [defaultvalue]
!           The character default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetCount"

!BOP
! !IROUTINE: ESMF_FieldBundleAttGet - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_FieldBundleAttGetCount(fieldbundle, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of attributes associated with the given {\tt fieldbundle} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

      end subroutine ESMF_FieldBundleAttGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetInfoByName"

!BOP
! !IROUTINE: ESMF_FieldBundleAttGet - Query FieldBundle attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetInfoByName(fieldbundle, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

      end subroutine ESMF_FieldBundleAttGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttGetInfoByNum"

!BOP
! !IROUTINE: ESMF_FieldBundleAttGet - Query FieldBundle attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttGet()
      subroutine ESMF_FieldBundleAttGetInfoByNum(fieldbundle, attributeIndex, name, &
        typekind, itemcount, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: itemcount   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[itemcount]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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
      if (present(itemcount)) itemcount = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleAttGetInfoByNum
      
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldBundleAttSet - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_FieldBundleAttSet(fieldbundle, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt fieldbundle}, or set an attribute on an 
!     attribute package.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList} and a {\tt convention} and {\tt purpose}.
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
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetInt4"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetInt4(fieldbundle, name, value, convention, &
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
!      Attaches a 4-byte integer attribute to the {\tt fieldbundle}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetInt4List"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetInt4List(fieldbundle, name, count, valueList, &
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
!     Attaches a 4-byte integer list attribute to the {\tt fieldbundle}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetInt8"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetInt8(fieldbundle, name, value, convention, &
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
!      Attaches an 8-byte integer attribute to the {\tt fieldbundle}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetInt8List"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetInt8List(fieldbundle, name, count, valueList, &
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
!     Attaches an 8-byte integer list attribute to the {\tt fieldbundle}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetReal4"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetReal4(fieldbundle, name, value, convention, &
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
!      Attaches a 4-byte real attribute to the {\tt fieldbundle}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetReal4List"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetReal4List(fieldbundle, name, count, valueList, &
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
!     Attaches a 4-byte real list attribute to the {\tt fieldbundle}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetReal8"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetReal8(fieldbundle, name, value, convention, &
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
!      Attaches an 8-byte real attribute to the {\tt fieldbundle}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetReal8List"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetReal8List(fieldbundle, name, count, valueList, &
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
!     Attaches an 8-byte real list attribute to the {\tt fieldbundle}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetLogical"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetLogical(fieldbundle, name, value, convention, &
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
!     Attaches a logical attribute to the {\tt fieldbundle}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetLogicalList"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetLogicalList(fieldbundle, name, count, valueList, &
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
!     Attaches a logical list attribute to the {\tt fieldbundle}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetChar"

!BOPI
! !IROUTINE: ESMF_FieldBundleAttSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttSet()
      subroutine ESMF_FieldBundleAttSetChar(fieldbundle, name, value, convention, &
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
!      Attaches a character attribute to the {\tt fieldbundle}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

      end subroutine ESMF_FieldBundleAttSetChar

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttSetLinkField"
!BOPI
! !IROUTINE: ESMF_FieldBundleAttSetLinkField - Link a FieldBundle to a Field in 
!            an attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSet()
      subroutine ESMF_FieldBundleAttSetLinkField(fieldbundle, field, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle
      type(ESMF_Field), intent(inout)  :: field
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a FieldBundle to a Field in an attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!      An {\tt ESMF\_FieldBundle} object.
!     \item [field]
!      An {\tt ESMF\_Field} derived object.
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

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

      end subroutine ESMF_FieldBundleAttSetLinkField

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAttWrite"
!BOP
! !IROUTINE: ESMF_FieldBundleAttWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAttWrite()
      subroutine ESMF_FieldBundleAttWrite(fieldbundle, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Print the attribute package for the {\tt fieldbundle}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!      An {\tt ESMF\_FieldBundle} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

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

      call c_ESMC_AttPackWrite(fieldbundle%btypep%base, fconvention, &
        fpurpose, fobject, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleAttWrite

!-------------------------------------------------------------------------
!  GRID
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttAddPack"
!BOPI
! !IROUTINE: ESMF_GridAttAddPack - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttAdd()
      subroutine ESMF_GridAttAddPack(grid, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt grid}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!      An {\tt ESMF\_Grid} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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

      name1 = 'longname'
      name2 = 'shortname'
      name3 = 'units'
      name4 = 'coordinates'

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
! !IROUTINE: ESMF_GridAttAddPackCustom - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttAdd()
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
!     Sets up a custom attribute package for the {\tt grid}, or adds to an existing 
!     attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!      An {\tt ESMF\_Grid} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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
!BOP
! !IROUTINE: ESMF_GridAttGet  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_GridAttGet(grid, name, <value argument>, &
!                            <defaultvalue argument>, convention, purpose, rc)
!
! !ARGUMENTS:
!     type(ESMF_Grid), intent(inout) :: grid  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an attribute from the {\tt grid}.
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
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [<defaultvalue argument>]
!           The default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttGetInt4"

!BOPI
! !IROUTINE: ESMF_GridAttGet  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!     Returns an integer attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns a 4-byte integer list attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!     Returns an 8-byte integer attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns an 8-byte integer list attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns a 4-byte real attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns a 4-byte real attribute from an {\tt ESMF\_Grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns an 8-byte real attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns an 8-byte real attribute from an {\tt ESMF\_Grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns a logical attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns a logical list attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!      Returns a character attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [defaultvalue]
!           The character default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

!BOP
! !IROUTINE: ESMF_GridAttGet - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_GridAttGetCount(grid, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of attributes associated with the given {\tt grid} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

!BOP
! !IROUTINE: ESMF_GridAttGet - Query Grid attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
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
!     Returns information associated with the named attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

!BOP
! !IROUTINE: ESMF_GridAttGet - Query Grid attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttGet()
      subroutine ESMF_GridAttGetInfoByNum(grid, attributeIndex, name, &
        typekind, itemcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: itemcount   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[itemcount]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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
      if (present(itemcount)) itemcount = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInfoByNum
      
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridAttSet - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_GridAttSet(grid, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Grid), intent(inout) :: grid  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt grid}, or set an attribute on an 
!     attribute package.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList} and a {\tt convention} and {\tt purpose}.
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
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttSetInt4"

!BOPI
! !IROUTINE: ESMF_GridAttSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!      Attaches a 4-byte integer attribute to the {\tt grid}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!     Attaches a 4-byte integer list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!      Attaches an 8-byte integer attribute to the {\tt grid}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!     Attaches an 8-byte integer list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!      Attaches a 4-byte real attribute to the {\tt grid}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!     Attaches a 4-byte real list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!      Attaches an 8-byte real attribute to the {\tt grid}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!     Attaches an 8-byte real list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!     Attaches a logical attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!     Attaches a logical list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_GridAttSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttSet()
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
!      Attaches a character attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
!BOP
! !IROUTINE: ESMF_GridAttWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttWrite()
      subroutine ESMF_GridAttWrite(grid, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Print the attribute package for the {\tt grid}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!      An {\tt ESMF\_Grid} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      call ESMF_GridValidate(grid, rc=localrc)
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
      
      fobject = 'grid'

      call c_ESMC_AttPackWrite(grid, fconvention, &
        fpurpose, fobject, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttWrite

!-------------------------------------------------------------------------
!  STATE
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttAddPack"
!BOPI
! !IROUTINE: ESMF_StateAttAddPack - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttAdd()
      subroutine ESMF_StateAttAddPack(state, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: convention
      character (len = *), intent(in) :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt state}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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

      name1 = 'longname'
      name2 = 'shortname'
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
#define ESMF_METHOD "ESMF_StateAttAddPackCustom"
!BOPI
! !IROUTINE: ESMF_StateAttAddPackCustom - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttAdd()
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
!     Sets up a custom attribute package for the {\tt state}, or adds to an existing 
!     attribute package.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
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
!BOP
! !IROUTINE: ESMF_StateAttGet  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_StateAttGet(state, name, <value argument>, &
!                            <defaultvalue argument>, convention, purpose, rc)
!
! !ARGUMENTS:
!     type(ESMF_State), intent(inout) :: state  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an attribute from the {\tt state}.
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
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [<defaultvalue argument>]
!           The default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttGetInt4"

!BOPI
! !IROUTINE: ESMF_StateAttGet  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!     Returns an integer attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns a 4-byte integer list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!     Returns an 8-byte integer attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns an 8-byte integer list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The default integer value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns a 4-byte real attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns a 4-byte real attribute from an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns an 8-byte real attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns an 8-byte real attribute from an {\tt ESMF\_State}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The real default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns a logical attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns a logical list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!           The logical default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!      Returns a character attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [defaultvalue]
!           The character default value of the named attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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

!BOP
! !IROUTINE: ESMF_StateAttGet - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_StateAttGetCount(state, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of attributes associated with the given {\tt state} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

!BOP
! !IROUTINE: ESMF_StateAttGet - Query State attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
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
!     Returns information associated with the named attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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

!BOP
! !IROUTINE: ESMF_StateAttGet - Query State attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttGet()
      subroutine ESMF_StateAttGetInfoByNum(state, attributeIndex, name, &
        typekind, itemcount, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: itemcount   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[itemcount]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

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
      if (present(itemcount)) itemcount = localItemcount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttGetInfoByNum
      
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAttSet - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_StateAttSet(state, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_State), intent(inout) :: state  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     character(ESMF_MAXSTR), intent(in), optional :: convention
!     character(ESMF_MAXSTR), intent(in), optional :: purpose
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt state}, or set an attribute on an 
!     attribute package.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList} and a {\tt convention} and {\tt purpose}.
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
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttSetInt4"

!BOPI
! !IROUTINE: ESMF_StateAttSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!      Attaches a 4-byte integer attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!     Attaches a 4-byte integer list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!      Attaches an 8-byte integer attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!     Attaches an 8-byte integer list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!      Attaches a 4-byte real attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!     Attaches a 4-byte real list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!      Attaches an 8-byte real attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!     Attaches an 8-byte real list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!     Attaches a logical attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!     Attaches a logical list attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
! !IROUTINE: ESMF_StateAttSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttSet()
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
!      Attaches a character attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!           An {\tt ESMF\_State} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [convention]
!           The convention of the attribute package.
!     \item [purpose]
!           The purpose of the attribute package.
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttWrite"
!BOP
! !IROUTINE: ESMF_StateAttWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttWrite()
      subroutine ESMF_StateAttWrite(state, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Print the attribute package for the {\tt state}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

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
      
      fobject = 'state'

      call c_ESMC_AttPackWrite(state%statep%base, fconvention, &
        fpurpose, fobject, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAttributeSetLink - Line a state attribute hierarchy with the
!                                         hierarchy of bundle, field, or state
!
! !INTERFACE:
!      subroutine ESMF_StateAttributeSet(state, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF\_State), intent(inout) :: state  
!      <value argument>, see below for supported values
!      integer, intent(out), optional :: rc   
!
!
! !DESCRIPTION:
!     Attaches a {\tt state} attribute hierarchy to the hierarchy of
!     a {\tt bundle}, {\tt field}, or another {\tt state},. 
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item type(ESMF\_FieldBundle), intent(inout) :: bundle
!     \item type(ESMF\_Field), intent(inout) :: field
!     \item type(ESMF\_State), intent(inout) :: state
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [<value argument>]
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
! !IROUTINE: ESMF_StateAttSetLinkFB - Link a State to a FieldBundle in an attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSetLink()
      subroutine ESMF_StateAttSetLinkFB(state, bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_FieldBundle), intent(inout)  :: bundle
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a state to a FieldBundle in an attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [bundle]
!      An {\tt ESMF\_FieldBundle} derived object.
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
! !IROUTINE: ESMF_StateAttSetLinkField - Link a State to a Field in an attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_eAttributeSetLink()
      subroutine ESMF_StateAttSetLinkField(state, field, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Field), intent(inout)  :: field
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a State to a Field in an attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [field]
!      An {\tt ESMF\_Field} derived object.
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
! !IROUTINE: ESMF_StateAttSetLinkState - Link a State to a State in an attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeSetLink()
      subroutine ESMF_StateAttSetLinkState(state1, state2, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state1
      type(ESMF_State), intent(inout)  :: state2
      integer, intent(out), optional  :: rc   

!
! !DESCRIPTION:
!     Attaches a State to a State in an attribute hierarchy
!
!     The arguments are:
!     \begin{description}
!     \item [state1]
!      An {\tt ESMF\_State} object.
!     \item [state2]
!      An {\tt ESMF\_State} derived object.
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
#define ESMF_METHOD "ESMF_StateAttributeCopyAll"
!BOP
! !IROUTINE: ESMF_StateAttributeCopyAll - Copy an attribute hierarchy between states
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttPackWrite()
      subroutine ESMF_StateAttributeCopyAll(state1, state2, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state1  
      type(ESMF_State), intent(inout) :: state2 
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Copy all attributes in one {\tt state} hierarchy to another.
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
!EOP

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

      call c_ESMC_AttributeCopyAll(state1%statep%base, state2%statep%base, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttributeCopyAll



end module ESMF_AttributeMod

