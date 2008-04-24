! $Id: ESMF_Attribute.F90,v 1.1 2008/04/24 21:30:00 rokuingh Exp $
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
!
! The code in this file implements the Attribute defined type
!  and functions.  This is an
!  interface to the actual C++ Attribute class implementation 
!  in the ../src dir.
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
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
! !PUBLIC MEMBER FUNCTIONS:
!
!   Classes
      public ESMF_Attribute
      
!  Attribute methods
      public ESMF_AttributeAdd             ! Add attributes
      public ESMF_AttributeGet
      public ESMF_AttributeSet
      public ESMF_AttributeWrite
      
      public ESMF_AttributeGetInfo   ! get type, length by name or number
      
      public ESMF_ArrayAttributeGetCount  ! number of attribs
      public ESMF_ArrayAttPackCreate      ! Attribute packages
      public ESMF_ArrayAttPackSet         ! Attribute packages
      public ESMF_ArrayAttPackWrite
      
      public ESMF_GridAttPackCreate      ! Attribute packages
      public ESMF_GridAttPackSet         ! Attribute packages
      public ESMF_GridAttributeGetCount  ! number of attribs
      public ESMF_GridAttPackWrite
      
      public ESMF_StateAttPackCreate      ! Attribute packages
      public ESMF_StateAttPackSet         ! Attribute packages
      public ESMF_StateAttPackWrite       ! Attribute packages
      public ESMF_StateAttributeSetLink   ! link an attribute hierarchy
      public ESMF_StateAttributeCopyAll   ! copy an attribute hierarchy
      public ESMF_StateAttributeGetCount  ! number of Attributes
  
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Attribute.F90,v 1.1 2008/04/24 21:30:00 rokuingh Exp $'
!------------------------------------------------------------------------------
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldAttAdd  - Create an attribute package
!
! !INTERFACE:
      interface ESMF_AttributeAdd
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldAttAddPack
        module procedure ESMF_FieldAttAddPackCustom

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     create an attribute package on an {\tt ESMF\_Field}.
 
!EOPI
      end interface

!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGet  - Get attributes, count, and type
!
! !INTERFACE:
      interface ESMF_AttributeGet
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttrGetInt4
        module procedure ESMF_ArrayAttrGetInt4List
        module procedure ESMF_ArrayAttrGetInt8
        module procedure ESMF_ArrayAttrGetInt8List
        module procedure ESMF_ArrayAttrGetReal4
        module procedure ESMF_ArrayAttrGetReal4List
        module procedure ESMF_ArrayAttrGetReal8
        module procedure ESMF_ArrayAttrGetReal8List
        module procedure ESMF_ArrayAttrGetLogical
        module procedure ESMF_ArrayAttrGetLogicalList
        module procedure ESMF_ArrayAttrGetChar
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
        module procedure ESMF_GridAttrGetInt4
        module procedure ESMF_GridAttrGetInt4List
        module procedure ESMF_GridAttrGetInt8
        module procedure ESMF_GridAttrGetInt8List
        module procedure ESMF_GridAttrGetReal4
        module procedure ESMF_GridAttrGetReal4List
        module procedure ESMF_GridAttrGetReal8
        module procedure ESMF_GridAttrGetReal8List
        module procedure ESMF_GridAttrGetLogical
        module procedure ESMF_GridAttrGetLogicalList
        module procedure ESMF_GridAttrGetChar
        module procedure ESMF_StateAttrGetInt4
        module procedure ESMF_StateAttrGetInt4List
        module procedure ESMF_StateAttrGetInt8
        module procedure ESMF_StateAttrGetInt8List
        module procedure ESMF_StateAttrGetReal4
        module procedure ESMF_StateAttrGetReal4List
        module procedure ESMF_StateAttrGetReal8
        module procedure ESMF_StateAttrGetReal8List
        module procedure ESMF_StateAttrGetLogical
        module procedure ESMF_StateAttrGetLogicalList
        module procedure ESMF_StateAttrGetChar

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_Field}.
 
!EOPI
      end interface

!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeSet  - Set attributes
!
! !INTERFACE:
      interface ESMF_AttributeSet 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttrSetInt4
        module procedure ESMF_ArrayAttrSetInt4List
        module procedure ESMF_ArrayAttrSetInt8
        module procedure ESMF_ArrayAttrSetInt8List
        module procedure ESMF_ArrayAttrSetReal4
        module procedure ESMF_ArrayAttrSetReal4List
        module procedure ESMF_ArrayAttrSetReal8
        module procedure ESMF_ArrayAttrSetReal8List
        module procedure ESMF_ArrayAttrSetLogical
        module procedure ESMF_ArrayAttrSetLogicalList
        module procedure ESMF_ArrayAttrSetChar
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
        module procedure ESMF_GridAttrSetInt4
        module procedure ESMF_GridAttrSetInt4List
        module procedure ESMF_GridAttrSetInt8
        module procedure ESMF_GridAttrSetInt8List
        module procedure ESMF_GridAttrSetReal4
        module procedure ESMF_GridAttrSetReal4List
        module procedure ESMF_GridAttrSetReal8
        module procedure ESMF_GridAttrSetReal8List
        module procedure ESMF_GridAttrSetLogical
        module procedure ESMF_GridAttrSetLogicalList
        module procedure ESMF_GridAttrSetChar
        module procedure ESMF_StateAttrSetInt4
        module procedure ESMF_StateAttrSetInt4List
        module procedure ESMF_StateAttrSetInt8
        module procedure ESMF_StateAttrSetInt8List
        module procedure ESMF_StateAttrSetReal4
        module procedure ESMF_StateAttrSetReal4List
        module procedure ESMF_StateAttrSetReal8
        module procedure ESMF_StateAttrSetReal8List
        module procedure ESMF_StateAttrSetLogical
        module procedure ESMF_StateAttrSetLogicalList
        module procedure ESMF_StateAttrSetChar

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Field}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayAttributeGetInfo - Get type, count from a Array attribute
!
! !INTERFACE:
      interface ESMF_AttributeGetInfo
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttrGetInfoByName
        module procedure ESMF_ArrayAttrGetInfoByNum
        module procedure ESMF_GridAttrGetInfoByName
        module procedure ESMF_GridAttrGetInfoByNum
        module procedure ESMF_StateAttrGetInfoByName
        module procedure ESMF_StateAttrGetInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_Array}.
 
!EOPI
      end interface

!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldAttAdd  - Create an attribute package
!
! !INTERFACE:
      interface ESMF_AttributeWrite
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldAttWrite
!        module procedure ESMF_GridAttPackWrite
!        module procedure ESMF_ArrayAttPackWrite

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     create an attribute package on an {\tt ESMF\_Field}.
 
!EOPI
      end interface

!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set a State attribute
!
! !INTERFACE:
      interface ESMF_StateAttributeSetLink
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_StateAttrSetLinkFB
        module procedure ESMF_StateAttrSetLinkField
        module procedure ESMF_StateAttrSetLinkState

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_State}.
 
!EOPI
      end interface
!------------------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!-------------------------------------------------------------------------
!  ARRAY
!-------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttPackCreate"
!BOP
! !IROUTINE: ESMF_ArrayAttPackCreate - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttPackCreate()
      subroutine ESMF_ArrayAttPackCreate(array, convention, purpose, attrList, &
      count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(in), optional :: count   
      character (len = *), dimension(:), intent(in), optional :: attrList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt array}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.  A user
!     can create their own attribute package by using the count and attrList parameters.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      localrc = ESMF_RC_NOT_IMPL
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

      if (present(count)) then
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(array, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      else

      name1 = 'longname'
      name2 = 'shortname'
      name3 = 'units'
      name4 = 'coordinates'

      call c_ESMC_ArrayAttPackCreate(array, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_ArrayAttPackCreate(array, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_ArrayAttPackCreate(array, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_ArrayAttPackCreate(array, name4, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttPackCreate
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttPackSet"
!BOP
! !IROUTINE: ESMF_ArrayAttPackSet - Set an attribute in the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttPackSet()
      subroutine ESMF_ArrayAttPackSet(array, name, value, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character(*), intent(in) :: name
      character(*), intent(in) :: value
      character(*), intent(in), optional :: convention
      character(*), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets an attribute the attribute package for the {\tt array}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [name]
!      The name of the attribute to be set.
!     \item [value]
!      The value of the attribute to be set.
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
      localrc = ESMF_RC_NOT_IMPL
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

!      call c_ESMC_ArrayAttPackSet(array, name, value, fconvention, &
!        fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttPackSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttPackWrite"
!BOP
! !IROUTINE: ESMF_ArrayAttPackWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttPackWrite()
      subroutine ESMF_ArrayAttPackWrite(array, convention, purpose, rc)
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
      localrc = ESMF_RC_NOT_IMPL
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

      call c_ESMC_ArrayAttPackWrite(array, fconvention, &
        fpurpose, fobject, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttPackWrite
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayAttributeGet  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_ArrayAttributeGet(array, name, <value argument>, &
!                                       defaultvalue, rc)
!
! !ARGUMENTS:
!     type(ESMF_Array), intent(inout) :: array  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInt4"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetInt4(array, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInt4List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetInt4List(array, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit

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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInt8"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetInt8(array, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInt8List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetInt8List(array, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit

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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetReal4"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetReal4(array, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetReal4List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetReal4List(array, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit

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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetReal8"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetReal8(array, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetReal8List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetReal8List(array, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit

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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetLogical"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetLogical(array, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetLogicalList"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetLogicalList(array, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit

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

      call c_ESMC_ArrayAttributeGetValue(array, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetChar"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetChar(array, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
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

      call c_ESMC_ArrayAttributeGetChar(array, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttributeGetCount"

!BOP
! !IROUTINE: ESMF_ArrayAttributeGetCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_ArrayAttributeGetCount(array, count, rc)
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

      call c_ESMC_ArrayAttributeGetCount(array, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttributeGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInfoByName"

!BOP
! !IROUTINE: ESMF_ArrayAttributeGetInfo - Query Array attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGetInfo()
      subroutine ESMF_ArrayAttrGetInfoByName(array, name, typekind, count, rc)
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

      call c_ESMC_ArrayAttrGetInfoName(array, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInfoByNum"

!BOP
! !IROUTINE: ESMF_ArrayAttributeGetInfo - Query Array attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGetInfo()
      subroutine ESMF_ArrayAttrGetInfoByNum(array, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
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
!     \item [{[count]}]
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
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttrGetInfoNum(array, attributeIndex, &
        localName, localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInfoByNum
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayAttributeSet - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_ArrayAttributeSet(array, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Array), intent(inout) :: array  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt array}.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList}.
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetInt4"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetInt4(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetInt4List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetInt4List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetInt8"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetInt8(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetInt8List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetInt8List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetReal4"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetReal4(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetReal4List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetReal4List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetReal8"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetReal8(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetReal8List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetReal8List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetLogical"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetLogical(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetLogicalList"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetLogicalList(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetChar"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetChar(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
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

      call c_ESMC_ArrayAttributeSetChar(array, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetChar

!-------------------------------------------------------------------------


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
!  GRID
!-------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttPackCreate"
!BOP
! !IROUTINE: ESMF_GridAttPackCreate - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttPackCreate()
      subroutine ESMF_GridAttPackCreate(grid, convention, purpose, attrList, &
      count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(in), optional :: count   
      character (len = *), dimension(:), intent(in), optional :: attrList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt grid}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.  A user
!     can create their own attribute package by using the count and attrList parameters.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!      An {\tt ESMF\_Grid} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      localrc = ESMF_RC_NOT_IMPL
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

      if (present(count)) then
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(grid, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      else

      name1 = 'longname'
      name2 = 'shortname'
      name3 = 'units'
      name4 = 'coordinates'

      call c_ESMC_GridAttPackCreate(grid, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_GridAttPackCreate(grid, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_GridAttPackCreate(grid, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_GridAttPackCreate(grid, name4, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttPackCreate
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttPackSet"
!BOP
! !IROUTINE: ESMF_GridAttPackSet - Set an attribute in the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttPackSet()
      subroutine ESMF_GridAttPackSet(grid, name, value, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character(ESMF_MAXSTR), intent(in) :: name
      character(ESMF_MAXSTR), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets an attribute the attribute package for the {\tt grid}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!      An {\tt ESMF\_Grid} object.
!     \item [name]
!      The name of the attribute to be set.
!     \item [value]
!      The value of the attribute to be set.
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
      localrc = ESMF_RC_NOT_IMPL
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

!      call c_ESMC_GridAttPackSet(grid, name, value, fconvention, &
!        fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttPackSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttPackWrite"
!BOP
! !IROUTINE: ESMF_GridAttPackWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttPackWrite()
      subroutine ESMF_GridAttPackWrite(grid, convention, purpose, rc)
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
      localrc = ESMF_RC_NOT_IMPL
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

      call c_ESMC_GridAttPackWrite(grid, fconvention, &
        fpurpose, fobject, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttPackWrite
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridAttributeGet  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_GridAttributeGet(grid, name, <value argument>, &
!                                       defaultvalue, rc)
!
! !ARGUMENTS:
!     type(ESMF_Grid), intent(inout) :: grid  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     <defaultvalue>, see below for supported values   
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetInt4"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetInt4(grid, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetInt4List"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetInt4List(grid, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit

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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetInt8"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetInt8(grid, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetInt8List"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetInt8List(grid, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit

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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetReal4"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetReal4(grid, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetReal4List"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetReal4List(grid, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit

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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetReal8"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetReal8(grid, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetReal8List"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetReal8List(grid, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit

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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetLogical"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetLogical(grid, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetLogicalList"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetLogicalList(grid, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit

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

      call c_ESMC_GridAttributeGetValue(grid, name, &
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetChar"

!BOPI
! !IROUTINE: ESMF_GridAttributeGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGet()
      subroutine ESMF_GridAttrGetChar(grid, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
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
!           The default integer value of the named attribute.
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

      call c_ESMC_GridAttributeGetChar(grid, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
        if(present(defaultvalue)) then
          value = defaultvalue
        else 
          return
        end if
      end if

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttributeGetCount"

!BOP
! !IROUTINE: ESMF_GridAttributeGetCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_GridAttributeGetCount(grid, count, rc)
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

      call c_ESMC_GridAttributeGetCount(grid, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttributeGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetInfoByName"

!BOP
! !IROUTINE: ESMF_GridAttributeGetInfo - Query Grid attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGetInfo()
      subroutine ESMF_GridAttrGetInfoByName(grid, name, typekind, count, rc)
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

      call c_ESMC_GridAttrGetInfoName(grid, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrGetInfoByNum"

!BOP
! !IROUTINE: ESMF_GridAttributeGetInfo - Query Grid attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeGetInfo()
      subroutine ESMF_GridAttrGetInfoByNum(grid, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
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
!     \item [{[count]}]
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
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      call c_ESMC_GridAttrGetInfoNum(grid, attributeIndex, &
        localName, localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrGetInfoByNum
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridAttributeSet - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_GridAttributeSet(grid, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Grid), intent(inout) :: grid  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt grid}.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList}.
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetInt4"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetInt4(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetInt4List"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetInt4List(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetInt8"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetInt8(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetInt8List"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetInt8List(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetReal4"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetReal4(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetReal4List"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetReal4List(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetReal8"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetReal8(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetReal8List"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetReal8List(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetLogical"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetLogical(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetLogicalList"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetLogicalList(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

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

      call c_ESMC_GridAttributeSetValue(grid, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAttrSetChar"

!BOPI
! !IROUTINE: ESMF_GridAttributeSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridAttributeSet()
      subroutine ESMF_GridAttrSetChar(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
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

      call c_ESMC_GridAttributeSetChar(grid, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttrSetChar
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!  STATE
!-------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttPackCreate"
!BOP
! !IROUTINE: ESMF_StateAttPackCreate - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttPackCreate()
      subroutine ESMF_StateAttPackCreate(state, convention, purpose, attrList, &
      count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(in), optional :: count   
      character (len = *), dimension(:), intent(in), optional :: attrList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt state}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.  A user
!     can create their own attribute package by using the count and attrList parameters.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [count]
!      The count of the number of attributes in a user specified attribute package
!     \item [attrList]
!      An array of character strings specifying the names of the user defined attributes
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc, i                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3
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

      if (present(count)) then
      
      do i = 1, count
      
        call c_ESMC_AttPackCreate(state%statep%base, attrList(i), fconvention, &
          fpurpose, fobject, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      end do
      
      else

      name1 = 'name'
      name2 = 'organization'
      name3 = 'discipline'

      call c_ESMC_AttPackCreate(state%statep%base, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(state%statep%base, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_AttPackCreate(state%statep%base, name3, fconvention, &
        fpurpose, fobject, localrc)
      
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttPackCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttPackSet"
!BOP
! !IROUTINE: ESMF_StateAttPackSet - Set an attribute in the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttPackSet()
      subroutine ESMF_StateAttPackSet(state, name, value, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character(ESMF_MAXSTR), intent(in) :: name
      character(ESMF_MAXSTR), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets an attribute the attribute package for the {\tt state}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to be set.
!     \item [value]
!      The value of the attribute to be set.
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

!      call c_ESMC_AttPackSet(state%statep%base, name, value, fconvention, &
!        fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttPackSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttPackWrite"
!BOP
! !IROUTINE: ESMF_StateAttPackWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttPackWrite()
      subroutine ESMF_StateAttPackWrite(state, convention, purpose, rc)
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

      end subroutine ESMF_StateAttPackWrite

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

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAttributeGet - Retrieve an attribute 
!
! !INTERFACE:
!      subroutine ESMF_StateAttributeGet(state, name, <value argument>, &
!                                       defaultvalue, rc)
!
! !ARGUMENTS:
!      type(ESMF_State), intent(inout) :: state  
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values
!      <defaultvalue>, see below for supported values   
!      integer, intent(out), optional :: rc   
!
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
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [<value argument>]
!      The value of the named attribute.
!     \item [<defaultvalue argument>]
!      The default value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetInt4"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve a 4-byte integer attribute 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetInt4(state, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The 4-byte integer value of the named attribute.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetInt4List"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetInt4List(state, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I4), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The 4-byte integer values of the named attribute.
!      The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetInt8"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve an 8-byte integer attribute 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetInt8(state, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The 8-byte integer value of the named attribute.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetInt8List"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetInt8List(state, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer(ESMF_KIND_I8), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The 8-byte integer values of the named attribute.
!      The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetReal4"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetReal4(state, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The 4-byte real value of the named attribute.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetReal4List"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetReal4List(state, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R4), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a list of 4-byte real attributes from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The 4-byte real values of the named attribute.  
!      The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetReal8"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetReal8(state, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from the {\tt state}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The 8-byte real value of the named attribute.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetReal8List"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetReal8List(state, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      real(ESMF_KIND_R8), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a list of 8-byte real attributes from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The 8-byte real values of the named attribute.  
!      The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetLogical"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetLogical(state, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The logical value of the named attribute.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
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
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetLogicalList"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGet()
      subroutine ESMF_StateAttrGetLogicalList(state, name, count, valueList, &
                                           defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      type(ESMF_Logical), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [count]
!      The number of values in the attribute.
!     \item [valueList]
!      The logical values of the named attribute.
!      The list must be at least {\tt count} items long.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
!     \item [{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetChar"
!BOPI
! !IROUTINE: ESMF_StateAttributeGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldAttributeGet()
      subroutine ESMF_StateAttrGetChar(state, name, value, defaultvalue, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      character (len = *), intent(inout), optional :: defaultvalue
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character attribute from the {\tt state}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to retrieve.
!     \item [value]
!      The character value of the named attribute.
!     \item [defaultvalue]
!      The default integer value of the named attribute.
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
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

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

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttributeGetCount"
!BOP
! !IROUTINE: ESMF_StateAttributeGetCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_StateAttributeGetCount(state, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of attributes associated with the given
!      {\tt state} in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [count]
!      The number of attributes associated with this object.
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
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetCount(state%statep%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttributeGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetInfoByName"
!BOP
! !IROUTINE: ESMF_StateAttributeGetInfo - Query State attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGetInfo()
      subroutine ESMF_StateAttrGetInfoByName(state, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the named attribute,
!      including {\tt typekind} and item {\tt count}.
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

      integer :: localrc                           ! Error status
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetInfoName(state%statep%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrGetInfoByNum"
!BOP
! !IROUTINE: ESMF_StateAttributeGetInfo - Query State attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeGetInfo()
      subroutine ESMF_StateAttrGetInfoByNum(state, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute,
!      including {\tt typekind} and item {\tt count}.
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
!     \item [{[count]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetInfoNum(state%statep%base, attributeIndex, &
        localName, localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrGetInfoByNum
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_StateAttributeSet - Set an attribute
!
! !INTERFACE:
!      subroutine ESMF_StateAttributeSet(state, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_State), intent(inout) :: state  
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values
!      integer, intent(out), optional :: rc   
!
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt state}.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList}.
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
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [<value argument>]
!       The value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetInt4"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetInt4(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [value]
!       The 4-byte integer value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetInt4List"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetInt4List(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
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
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of integers in the {\tt valueList}.
!     \item [valueList]
!       The 4-byte integer values of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!
!EOPI

      integer :: localrc                          ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

  
      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetInt8"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetInt8(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [value]
!       The 8-byte integer value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetInt8List"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetInt8List(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
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
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of integers in the {\tt valueList}.
!     \item [valueList]
!       The 8-byte integer values of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!
!EOPI

      integer :: localrc                          ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
  
        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetReal4"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetReal4(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
!      
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [value]
!       The 4-byte real value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetReal4List"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetReal4List(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
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
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of reals in the {\tt valueList}.
!     \item [value]
!       The 4-byte real values of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetReal8"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetReal8(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real attribute to the {\tt state}.
!      The attribute has a {\tt name} and a {\tt value}.
!      
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [value]
!       The 8-byte real value of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetReal8List"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetReal8List(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
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
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of reals in the {\tt valueList}.
!     \item [value]
!       The 8-byte real values of the attribute to set.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetLogical"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set a logical attribute 
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetLogical(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to set.
!     \item [value]
!       The logical true/false value of the attribute to set.
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
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetLogicalList"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetLogicalList(state, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
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
!       An {\tt ESMF\_State} object.
!     \item [name]
!       The name of the attribute to set.
!     \item [count]
!       The number of logicals in the {\tt valueList}.
!     \item [valueList]
!       The logical true/false values of the attribute.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                           ! Error status
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                      "count longer than valueList", &
                                      ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(state%statep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetChar"
!BOPI
! !IROUTINE: ESMF_StateAttributeSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSet()
      subroutine ESMF_StateAttrSetChar(state, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a character attribute to the {\tt state}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [state]
!      An {\tt ESMF\_State} object.
!     \item [name]
!      The name of the attribute to set.
!     \item [value]
!      The character value of the attribute to set.
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
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetChar(state%statep%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateAttrSetChar
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
#define ESMF_METHOD "ESMF_StateAttrSetLinkFB"
!BOPI
! !IROUTINE: ESMF_StateAttrSetLinkFB - Link a State to a FieldBundle in an attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSetLink()
      subroutine ESMF_StateAttrSetLinkFB(state, bundle, rc)
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

      end subroutine ESMF_StateAttrSetLinkFB
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetLinkField"
!BOPI
! !IROUTINE: ESMF_StateAttrSetLinkField - Link a State to a Field in an attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSetLink()
      subroutine ESMF_StateAttrSetLinkField(state, field, rc)
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

      end subroutine ESMF_StateAttrSetLinkField
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateAttrSetLinkState"
!BOPI
! !IROUTINE: ESMF_StateAttrSetLinkState - Link a State to a State in an attribute hierarchy
!
! !INTERFACE:
      ! Private name; call using ESMF_StateAttributeSetLink()
      subroutine ESMF_StateAttrSetLinkState(state1, state2, rc)
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

      end subroutine ESMF_StateAttrSetLinkState



end module ESMF_AttributeMod

