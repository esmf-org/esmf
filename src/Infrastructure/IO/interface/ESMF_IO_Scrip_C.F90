!  $Id$
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
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable
!   arrays, or ...
!
!==============================================================================
#define ESMF_FILENAME "ESMF_IO_Scrip_C.F90"
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

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_scrip_inq"
  subroutine f_esmf_scrip_inq(filename, grid_dims, grid_rank, rc)
    use ESMF_UtilTypesMod
    use ESMF_LogErrMod
    use ESMF_IOScripMod

    implicit none

    !------------------------------------------------------------------------------

    !arguments
    character(len=*), intent(in)  :: filename
    integer, intent(out)          :: grid_dims(2)
    integer, intent(out)          :: grid_rank
    integer, intent(out)          :: rc

    ! Initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    ! Call into public interface
    call ESMF_ScripInq(filename, grid_dims=grid_dims, grid_rank=grid_rank, rc=rc)

    ! Error handling
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    rc = ESMF_SUCCESS

  end subroutine f_esmf_scrip_inq
