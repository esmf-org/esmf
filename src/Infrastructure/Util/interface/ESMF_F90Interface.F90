! $Id: ESMF_F90Interface.F90,v 1.6.2.2 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_F90Interface.F90"
!==============================================================================
!
! ESMF DistGrid Module
module ESMF_F90InterfaceMod
!
!==============================================================================
!
! This file contains helper methods for the F90-to-C++ and C++-to-F90 interface
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_LogErrMod        ! ESMF error handling

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
  public ESMF_InterfaceInt
  public ESMF_InterfaceIntCreate, ESMF_InterfaceIntDestroy
  
  
!------------------------------------------------------------------------------
!     ! ESMF_InterfaceInt (helps handling [optional] integer arrays on the
!                          F90-to-C++ interface)
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_InterfaceInt
  sequence
  private
#ifndef ESMF_NO_INITIALIZERS
    type(ESMF_Pointer) :: this = ESMF_NULL_POINTER
#else
    type(ESMF_Pointer) :: this
#endif
  end type



contains


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterfaceIntCreate()"
!BOPI
! !IROUTINE: ESMF_InterfaceIntCreate - Create InterfaceInt

! !INTERFACE:
  function ESMF_InterfaceIntCreate(farray1D, farray2D, farray3D, rc)
!
! !ARGUMENTS:
    integer,                      intent(in), optional  :: farray1D(:)
    integer,                      intent(in), optional  :: farray2D(:,:)
    integer,                      intent(in), optional  :: farray3D(:,:,:)
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_InterfaceInt) :: ESMF_InterfaceIntCreate
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_InterfaceInt} from optional F90 array.
!
!     The arguments are:
!     \begin{description}
!     \item[{[farray1D]}]
!          1D F90 array.
!     \item[{[farray2D]}]
!          2D F90 array.
!     \item[{[farray3D]}]
!          3D F90 array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status     ! local error status
    type(ESMF_InterfaceInt) :: array      ! opaque pointer to new C++ object
    integer, allocatable    :: len(:)
    
    ! initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! mark this InterfaceInt as invalid
    array%this = ESMF_NULL_POINTER

    ! call into the C++ interface, depending on whether or not farray is present
    if (present(farray1D)) then
      allocate(len(1))
      len = shape(farray1D)
      if (all(len .ne. 0)) then
         call c_ESMC_InterfaceIntCreate1D(array, farray1D(1), len, status)
      else
         call c_ESMC_InterfaceIntCreate1D(array, 0, len, status)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      deallocate(len)
    endif
    if (present(farray2D)) then
      allocate(len(2))
      len = shape(farray2D)
      if (all(len .ne. 0)) then
         call c_ESMC_InterfaceIntCreate2D(array, farray2D(1,1), len, status)
      else
         call c_ESMC_InterfaceIntCreate2D(array, 0, len, status)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      deallocate(len)
    endif
    if (present(farray3D)) then
      allocate(len(3))
      len = shape(farray3D)
      if (all(len .ne. 0)) then
         call c_ESMC_InterfaceIntCreate3D(array, farray3D(1,1,1), len, status)
      else
         call c_ESMC_InterfaceIntCreate3D(array, 0, len, status)
      endif
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      deallocate(len)
    endif
    
    ! set return value
    ESMF_InterfaceIntCreate = array
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_InterfaceIntCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterfaceIntDestroy()"
!BOPI
! !IROUTINE: ESMF_InterfaceIntDestroy - Destroy InterfaceInt

! !INTERFACE:
  subroutine ESMF_InterfaceIntDestroy(array, rc)
!
! !ARGUMENTS:
    type(ESMF_InterfaceInt)           :: array
    integer,  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Destroy an {\tt ESMF\_InterfaceInt} object.
!
!     The arguments are:
!     \begin{description}
!     \item[array]
!          {\tt ESMF\_InterfaceInt} object.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status       ! local error status
    
    ! initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! call into the C++ interface
    call c_ESMC_InterfaceIntDestroy(array, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_InterfaceIntDestroy
!------------------------------------------------------------------------------


end module ESMF_F90InterfaceMod
