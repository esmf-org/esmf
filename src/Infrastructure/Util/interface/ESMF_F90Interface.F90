! $Id: ESMF_F90Interface.F90,v 1.1 2006/04/13 23:20:01 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
      
  public ESMF_InterfaceIntArray
  public ESMF_InterfaceIntArrayCreate, ESMF_InterfaceIntArrayDestroy
  
  
!------------------------------------------------------------------------------
!     ! ESMF_OptionalIntArray
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_InterfaceIntArray
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
#define ESMF_METHOD "ESMF_InterfaceIntArrayCreate()"
!BOP
! !IROUTINE: ESMF_InterfaceIntArrayCreate - Create InterfaceIntArray

! !INTERFACE:
  function ESMF_InterfaceIntArrayCreate(farray1D, farray2D, farray3D, rc)
!
! !ARGUMENTS:
    integer,                      intent(in), optional  :: farray1D(:)
    integer,                      intent(in), optional  :: farray2D(:,:)
    integer,                      intent(in), optional  :: farray3D(:,:,:)
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_InterfaceIntArray) :: ESMF_InterfaceIntArrayCreate
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_InterfaceIntArray} from optional F90 array.
!
!     The arguments are:
!     \begin{description}
!     \item[{[farray]}]
!          F90 array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status       ! local error status
    type(ESMF_InterfaceIntArray)     :: array     ! opaque pointer to new C++ DistGrid  
    integer, allocatable    :: len(:)
    
    ! initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! mark this InterfaceIntArray as invalid
    array%this = ESMF_NULL_POINTER

    ! call into the C++ interface, depending on whether or not farray is present
    if (present(farray1D)) then
      allocate(len(1))
      len = shape(farray1D)
      call c_ESMC_InterfaceIntArrayCreate1D(array, farray1D(1), len, status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      deallocate(len)
    endif
    if (present(farray2D)) then
      allocate(len(2))
      len = shape(farray2D)
      call c_ESMC_InterfaceIntArrayCreate2D(array, farray2D(1,1), len, status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      deallocate(len)
    endif
    if (present(farray3D)) then
      allocate(len(3))
      len = shape(farray3D)
      call c_ESMC_InterfaceIntArrayCreate3D(array, farray3D(1,1,1), len, status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      deallocate(len)
    endif
    
    ! set return value
    ESMF_InterfaceIntArrayCreate = array
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_InterfaceIntArrayCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterfaceIntArrayCreate2D()"
!BOP
! !IROUTINE: ESMF_InterfaceIntArrayCreate2D - Create InterfaceIntArray

! !INTERFACE:
  ! Private name; call using ESMF_InterfaceIntArrayCreate()
  
  function ESMF_InterfaceIntArrayCreate2D(farray, rc)
!
! !ARGUMENTS:
    integer,                      intent(in), optional  :: farray(:,:)
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_InterfaceIntArray) :: ESMF_InterfaceIntArrayCreate2D
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_InterfaceIntArray} from optional F90 array.
!
!     The arguments are:
!     \begin{description}
!     \item[{[farray]}]
!          F90 array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status       ! local error status
    type(ESMF_InterfaceIntArray)     :: array     ! opaque pointer to new C++ DistGrid  
    integer                 :: len(2)
    
    ! initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! mark this InterfaceIntArray as invalid
    array%this = ESMF_NULL_POINTER

    ! call into the C++ interface, depending on whether or not farray is present
    if (present(farray)) then
      len = shape(farray)
      call c_ESMC_InterfaceIntArrayCreate2D(array, farray, len, status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! set return value
    ESMF_InterfaceIntArrayCreate2D = array
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_InterfaceIntArrayCreate2D
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterfaceIntArrayDestroy()"
!BOP
! !IROUTINE: ESMF_InterfaceIntArrayDestroy - Destroy InterfaceIntArray

! !INTERFACE:
  subroutine ESMF_InterfaceIntArrayDestroy(array, rc)
!
! !ARGUMENTS:
    type(ESMF_InterfaceIntArray)     :: array
    integer,                      intent(out),optional  :: rc
!         
!
! !DESCRIPTION:
!     Destroy an {\tt ESMF\_InterfaceIntArray} object.
!
!     The arguments are:
!     \begin{description}
!     \item[array]
!          {\tt ESMF\_InterfaceIntArray} object.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status       ! local error status
    
    ! initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! call into the C++ interface
    call c_ESMC_InterfaceIntArrayDestroy(array, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_InterfaceIntArrayDestroy
!------------------------------------------------------------------------------


end module ESMF_F90InterfaceMod
