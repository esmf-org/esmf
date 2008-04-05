! $Id: ESMF_Attribute.F90,v 1.3 2008/04/05 03:38:07 cdeluca Exp $
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
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!

  ! Contains pointer to real Base object which is defined in C++
  type ESMF_Base
  sequence
  !private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type


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
      public ESMF_Attribute, ESMF_Base

!  Attribute methods
      public ESMF_AttributeSet
      public ESMF_AttributeGet

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_Attribute.F90,v 1.3 2008/04/05 03:38:07 cdeluca Exp $'
!------------------------------------------------------------------------------

      contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeSet"
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
!     \item[base]
!       Any ESMF type.
!     \item[name]
!       The name of the attribute to set.
!     \item[value]
!       The value of the attribute.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

    integer :: status 

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    status = ESMF_RC_NOT_IMPL
      
    ! Check init status of arguments
    !ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, base, rc)
      
    !call c_ESMC_AttributeSet(base , name, value, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_AttributeSet


!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttributeGet"
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

    integer :: status 

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    status = ESMF_RC_NOT_IMPL
      
    ! Check init status of arguments
    !ESMF_INIT_CHECK_DEEP(ESMF_BaseGetInit, base, rc)
      
    !call c_ESMC_AttributeGet(base , name, value, status) 
    if (present(rc)) rc = status

  end subroutine ESMF_AttributeGet

!-------------------------------------------------------------------------

end module ESMF_AttributeMod

