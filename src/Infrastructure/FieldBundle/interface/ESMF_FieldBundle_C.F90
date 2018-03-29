!  $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FieldBundle_C.F90"
!==============================================================================
!
! F77 interfaces for C++ layer calling into F90 implementation layer.
!
!==============================================================================
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id$'
!==============================================================================
   subroutine f_esmf_bundlecreate(bundlep, rc)
     use ESMF_UtilTypesMod    ! ESMF generic types class
     use ESMF_BaseMod         ! ESMF base class
     use ESMF_FieldBundleMod

     implicit none

     type(ESMF_FieldBundle), pointer :: bundlep
     type(ESMF_FieldBundle), target :: thebundle
     integer, intent(out) :: rc              
     integer :: localrc

     ! Initialize return codes; assume routines not initialized
     rc = ESMF_RC_NOT_IMPL
     localrc = ESMF_RC_NOT_IMPL

     thebundle = ESMF_FieldBundleCreate(rc=localrc)
    
     bundlep => thebundle
     rc = localrc
   end subroutine f_esmf_bundlecreate

   subroutine f_esmf_bundledestroy(bundlep, rc)
     use ESMF_UtilTypesMod    ! ESMF generic types class
     use ESMF_BaseMod         ! ESMF base class
     use ESMF_FieldBundleMod

     implicit none

     type(ESMF_FieldBundle), pointer :: bundlep      
     integer, intent(out), optional :: rc     

     ! Initialize return codes; assume routines not initialized
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

     call ESMF_FieldBundleDestroy(bundlep, rc=rc)


   end subroutine f_esmf_bundledestroy

  subroutine f_esmf_fbundlecollectgarbage(fb, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fbundlecollectgarbage"
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldBundleMod

    implicit none

    type(ESMF_FieldBundle):: fb
    integer, intent(out)  :: rc
  
    integer :: localrc
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    !print *, "collecting FieldBundle garbage"
  
    ! destruct internal data allocations
    call ESMF_FieldBundleDestruct(fb%this, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! deallocate actual FieldBundleType allocation      
    if (associated(fb%this)) then
      deallocate(fb%this, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Deallocating FieldBundle", &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return
    endif
    nullify(fb%this)

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_fbundlecollectgarbage
