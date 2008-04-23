! $Id: ESMF_Attribute.F90,v 1.5 2008/04/23 21:02:25 rokuingh Exp $
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
  use ESMF_GridMod

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
      public ESMF_AttributeSet
      public ESMF_AttributeGet
      
      public ESMF_AttributeGetInfo   ! get type, length by name or number
      
      public ESMF_ArrayAttributeGetCount  ! number of attribs
      public ESMF_ArrayAttPackCreate      ! Attribute packages
      public ESMF_ArrayAttPackSet         ! Attribute packages
      public ESMF_ArrayAttPackWrite       ! Attribute packages

      public ESMF_GridAttPackCreate      ! Attribute packages
      public ESMF_GridAttPackSet         ! Attribute packages
      public ESMF_GridAttPackWrite       ! Attribute packages
      public ESMF_GridAttributeGetCount  ! number of attribs

  
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Attribute.F90,v 1.5 2008/04/23 21:02:25 rokuingh Exp $'
!------------------------------------------------------------------------------
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

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
!        module procedure ESMF_FieldAttSetInt4
!        module procedure ESMF_FieldAttSetInt4List
!        module procedure ESMF_FieldAttSetInt8
!        module procedure ESMF_FieldAttSetInt8List
!        module procedure ESMF_FieldAttSetReal4
!        module procedure ESMF_FieldAttSetReal4List
!        module procedure ESMF_FieldAttSetReal8
!        module procedure ESMF_FieldAttSetReal8List
!        module procedure ESMF_FieldAttSetLogical
!        module procedure ESMF_FieldAttSetLogicalList
!        module procedure ESMF_FieldAttSetChar
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

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Field}.
 
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
!        module procedure ESMF_FieldAttGetInt4
!        module procedure ESMF_FieldAttGetInt4List
!        module procedure ESMF_FieldAttGetInt8
!        module procedure ESMF_FieldAttGetInt8List
!        module procedure ESMF_FieldAttGetReal4
!        module procedure ESMF_FieldAttGetReal4List
!        module procedure ESMF_FieldAttGetReal8
!        module procedure ESMF_FieldAttGetReal8List
!        module procedure ESMF_FieldAttGetLogical
!        module procedure ESMF_FieldAttGetLogicalList
!        module procedure ESMF_FieldAttGetChar
!        module procedure ESMF_FieldAttGetInfoByName
!        module procedure ESMF_FieldAttGetInfoByNum
!        module procedure ESMF_FieldAttGetCount
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


! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_Field}.
 
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
!      interface ESMF_FieldAttAdd
   
! !PRIVATE MEMBER FUNCTIONS:
!        module procedure ESMF_FieldAttAddPack
!        module procedure ESMF_FieldAttAddPackCustom

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     create an attribute package on an {\tt ESMF\_Field}.
 
!EOPI
!      end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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


end module ESMF_AttributeMod

