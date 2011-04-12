! $Id: ESMF_Container.F90,v 1.1 2011/04/12 00:15:40 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Container.F90"
!==============================================================================
!
! ESMF Container Module
module ESMF_ContainerMod
!
!==============================================================================
!
! This file contains the Fortran wrapper code for the C++ implementation of
!  the Container class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_ContainerMod
!

!   Fortran API wrapper of C++ implemenation of Container
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_FieldMod         ! ESMF Fortran-C++ interface helper
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!     ! ESMF_Container
!
!------------------------------------------------------------------------------

  ! Fortran class type to hold pointer to C++ object
  type ESMF_Container
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Container
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ContainerCreate
  public ESMF_ContainerDestroy

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Container.F90,v 1.1 2011/04/12 00:15:40 theurich Exp $'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerCreate()"
!BOPI
! !IROUTINE: ESMF_ContainerCreate

! !INTERFACE:
  function ESMF_ContainerCreate(rc)
!
! !ARGUMENTS:
    integer,                    intent(out),  optional  :: rc  
!     
! !RETURN VALUE:
    type(ESMF_Container) :: ESMF_ContainerCreate
!         
!
! !DESCRIPTION:
!     Create empty ESMF Container.
!
!     The arguments are:
!     \begin{description}
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Container)    :: container    ! new Container

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Mark this Container object as invalid
    container%this = ESMF_NULL_POINTER
    
    call c_ESMC_ContainerCreate(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_ContainerCreate = container 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ContainerCreate)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_ContainerCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerDestroy()"
!BOPI
! !IROUTINE: ESMF_ContainerDestroy - Destroy Container object

! !INTERFACE:
  subroutine ESMF_ContainerDestroy(container, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
    integer,              intent(out),  optional  :: rc  
!         
! !DESCRIPTION:
!     Destroy an {\tt ESMF\_Container} object.
!
!     The arguments are:
!     \begin{description}
!     \item[container] 
!          {\tt ESMF\_Container} object to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ContainerDestroy(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Mark this Container object as invalid
    container%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(container)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGetInit"
!BOPI
! !IROUTINE: ESMF_ContainerGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_ContainerGetInit(container) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_ContainerGetInit   
!
! !ARGUMENTS:
      type(ESMF_Container), intent(in), optional :: container
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [{[container]}]
!           Container object.
!     \end{description}
!
!EOPI

    if (present(container)) then
      ESMF_ContainerGetInit = ESMF_INIT_GET(container)
    else
      ESMF_ContainerGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_ContainerGetInit
!------------------------------------------------------------------------------

end module ESMF_ContainerMod
