! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
  public ESMF_InterfaceIntCreate
  public ESMF_InterfaceIntGet
  public ESMF_InterfaceIntDestroy
  
!------------------------------------------------------------------------------
! ESMF_InterfaceInt:
!   Handling of [optional] integer arrays on the Fortran-to-C++ interface.
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_InterfaceInt
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
#ifndef ESMF_NO_INITIALIZERS
    integer(ESMF_KIND_I8), dimension(10) :: shallowMemory = 0
#else
    integer(ESMF_KIND_I8), dimension(10) :: shallowMemory
#endif
    integer, pointer   :: farray1D(:)       ! holding Fortran reference
    integer, pointer   :: farray2D(:,:)     ! holding Fortran reference
    integer, pointer   :: farray3D(:,:,:)   ! holding Fortran reference
  end type


!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_InterfaceIntCreate -- Generic interface

! !INTERFACE:
  interface ESMF_InterfaceIntCreate

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_InterfaceIntCreateTrg
    module procedure ESMF_InterfaceIntCreatePtr
      
! !DESCRIPTION: 
!EOPI 
  end interface
!==============================================================================


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterfaceIntCreateTrg()"
!BOPI
! !IROUTINE: ESMF_InterfaceIntCreateTrg - Create InterfaceInt

! !INTERFACE:
  function ESMF_InterfaceIntCreateTrg(farray1D, farray2D, farray3D, rc)
!
! !ARGUMENTS:
    integer, target, intent(in),  optional :: farray1D(:)
    integer, target, intent(in),  optional :: farray2D(:,:)
    integer, target, intent(in),  optional :: farray3D(:,:,:)
    integer,         intent(out), optional :: rc
!         
! !RETURN VALUE:
    type(ESMF_InterfaceInt) :: ESMF_InterfaceIntCreateTrg
!
! !DESCRIPTION:
!   Create an {\tt ESMF\_InterfaceInt} from Fortran array. The 
!   {\tt transferOwnership} allows ownershipt of the Fortran array to be
!   transferred to the InterfaceInt object. InterfaceIntDestroy() will call 
!   deallocate() for Fortran arrays whose ownership was transferred.
!
!   The arguments are:
!   \begin{description}
!   \item[{[farray1D]}]
!     1D Fortran array.
!   \item[{[farray2D]}]
!     2D Fortran array.
!   \item[{[farray3D]}]
!     3D Fortran array.
!   \item[{[transferOwnership]}]
!     For a value of {\tt .true.} transfers ownership of Fortran array to the
!     newly created InterfaceInt object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: array        ! opaque pointer to new C++ object
    integer                 :: checkCount
    integer, pointer        :: farray1DPtr(:)
    integer, pointer        :: farray2DPtr(:,:)
    integer, pointer        :: farray3DPtr(:,:,:)
    
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! initialize
    nullify(farray1DPtr)
    nullify(farray2DPtr)
    nullify(farray3DPtr)
    
    ! set references
    if (present(farray1D)) farray1DPtr => farray1D
    if (present(farray2D)) farray2DPtr => farray2D
    if (present(farray3D)) farray3DPtr => farray3D
    
    ! create InterfaceInt object
    array = ESMF_InterfaceIntCreate(farray1DPtr, farray2DPtr, farray3DPtr, &
      transferOwnership=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! set return value
    ESMF_InterfaceIntCreateTrg = array
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_InterfaceIntCreateTrg
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterfaceIntCreatePtr()"
!BOPI
! !IROUTINE: ESMF_InterfaceIntCreatePtr - Create InterfaceInt

! !INTERFACE:
  function ESMF_InterfaceIntCreatePtr(farray1D, farray2D, farray3D, &
    transferOwnership, rc)
!
! !ARGUMENTS:
    integer, pointer,              optional :: farray1D(:)
    integer, pointer,              optional :: farray2D(:,:)
    integer, pointer,              optional :: farray3D(:,:,:)
    logical,          intent(in)            :: transferOwnership
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
    type(ESMF_InterfaceInt) :: ESMF_InterfaceIntCreatePtr
!
! !DESCRIPTION:
!   Create an {\tt ESMF\_InterfaceInt} from Fortran array. The 
!   {\tt transferOwnership} allows ownership of the Fortran array to be
!   transferred to the InterfaceInt object. InterfaceIntDestroy() will call 
!   deallocate() for Fortran arrays whose ownership was transferred.
!
!   The arguments are:
!   \begin{description}
!   \item[{[farray1D]}]
!     1D Fortran array.
!   \item[{[farray2D]}]
!     2D Fortran array.
!   \item[{[farray3D]}]
!     3D Fortran array.
!   \item[transferOwnership]
!     For a value of {\tt .true.} transfers ownership of Fortran array to the
!     newly created InterfaceInt object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: array        ! opaque pointer to new C++ object
    integer, allocatable    :: len(:)
    integer                 :: checkCount
    
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! mark this InterfaceInt as invalid
    call c_ESMC_InterfaceIntSetInvalid(array, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! initialize Fortran array references
    nullify(array%farray1D)
    nullify(array%farray2D)
    nullify(array%farray3D)
    
    ! check that only one of the array arguments is present
    checkCount = 0  ! reset
    if (present(farray1D)) then
      if (associated(farray1D)) checkCount = checkCount + 1
    endif
    if (present(farray2D)) then
      if (associated(farray2D)) checkCount = checkCount + 1
    endif
    if (present(farray3D)) then
      if (associated(farray3D)) checkCount = checkCount + 1
    endif
    if (checkCount>1) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
        msg="too many farrayXD arguments were specified.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call into the C++ interface, depending on whether or not farray is present
    if (present(farray1D)) then
      if (associated(farray1D)) then
        if (transferOwnership) &
          array%farray1D => farray1D
        allocate(len(1))
        len = shape(farray1D)
        if (all(len .ne. 0)) then
          call c_ESMC_InterfaceIntCreate1D(array, farray1D(1), len, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call c_ESMC_InterfaceIntCreate1D(array, 0, len, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        deallocate(len)
      endif
    endif
    if (present(farray2D)) then
      if (associated(farray2D)) then
        if (transferOwnership) &
          array%farray2D => farray2D
        allocate(len(2))
        len = shape(farray2D)
        if (all(len .ne. 0)) then
          call c_ESMC_InterfaceIntCreate2D(array, farray2D(1,1), len, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call c_ESMC_InterfaceIntCreate2D(array, 0, len, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        deallocate(len)
      endif
    endif
    if (present(farray3D)) then
      if (associated(farray3D)) then
        if (transferOwnership) &
          array%farray3D => farray3D
        allocate(len(3))
        len = shape(farray3D)
        if (all(len .ne. 0)) then
          call c_ESMC_InterfaceIntCreate3D(array, farray3D(1,1,1), len, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call c_ESMC_InterfaceIntCreate3D(array, 0, len, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        deallocate(len)
      endif
    endif
    
    ! set return value
    ESMF_InterfaceIntCreatePtr = array
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_InterfaceIntCreatePtr
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InterfaceIntGet()"
!BOPI
! !IROUTINE: ESMF_InterfaceIntGet - Get array pointer out of InterfaceInt

! !INTERFACE:
  subroutine ESMF_InterfaceIntGet(array, farray1D, farray2D, farray3D, rc)
!
! !ARGUMENTS:
    type(ESMF_InterfaceInt), intent(inout)         :: array
    integer,                 pointer,     optional :: farray1D(:)
    integer,                 pointer,     optional :: farray2D(:,:)
    integer,                 pointer,     optional :: farray3D(:,:,:)
    integer,                 intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!   Get pointer out of an {\tt ESMF\_InterfaceInt} object.
!
!   The arguments are:
!   \begin{description}
!   \item[array]
!     {\tt ESMF\_InterfaceInt} object.
!   \item[{[farray1D]}]
!     1D Fortran array.
!   \item[{[farray2D]}]
!     2D Fortran array.
!   \item[{[farray3D]}]
!     3D Fortran array.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: stat         ! Fortran return code
    
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    if (present(farray1D)) farray1D => array%farray1D
    if (present(farray2D)) farray2D => array%farray2D
    if (present(farray3D)) farray3D => array%farray3D

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_InterfaceIntGet
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
    type(ESMF_InterfaceInt), intent(inout)         :: array
    integer,                 intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!   Destroy an {\tt ESMF\_InterfaceInt} object. Deallocate Fortran arrays
!   whose ownership was transferred to the InterfaceInt object.
!
!   The arguments are:
!   \begin{description}
!   \item[array]
!     {\tt ESMF\_InterfaceInt} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: stat         ! Fortran return code
    
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! deallocate Fortran arrays whose ownership was transferred
    if (associated(array%farray1D)) then
      deallocate(array%farray1D, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, msg="deallocating array%farray1D", &
        ESMF_CONTEXT)) &
        return  ! bail out
    endif
    if (associated(array%farray2D)) then
      deallocate(array%farray2D, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, msg="deallocating array%farray1D", &
        ESMF_CONTEXT)) &
        return  ! bail out
    endif
    if (associated(array%farray3D)) then
      deallocate(array%farray3D, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, msg="deallocating array%farray1D", &
        ESMF_CONTEXT)) &
        return  ! bail out
    endif
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_InterfaceIntDestroy
!------------------------------------------------------------------------------


end module ESMF_F90InterfaceMod
