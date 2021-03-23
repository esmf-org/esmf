! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_MapperUtil.F90"
!==============================================================================
!
!     ESMF Mapper module
module ESMF_MapperUtilMod
!
!==============================================================================
!
! This file contains the Mapper class utility functions
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_MapperUtilMod - Mapper class utility functions
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Mapper} class utility funcs
!
! This type is implemented in Fortran 90.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_StateTypesMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  use ESMF_MapperMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_MapperUtil class

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_MapperSetCompInfo          ! Collect details from a component

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! -------------------------- ESMF-public method -------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperSetCompInfo()"
!BOP
! !IROUTINE: ESMF_MapperSetCompInfo - Collect all info from the component

! !INTERFACE:
  subroutine ESMF_MapperSetCompInfo(mapper, comp_name_len, comp_name, phase_name_len, phase_name, pet_range_start, pet_range_end, time_intvl_start, time_intvl_end, keywordEnforcer, rc)
!
!
! !ARGUMENTS:
    type(ESMF_Mapper) :: mapper
    integer, intent(in) :: comp_name_len
    character(len=*), intent(in) :: comp_name
    integer, intent(in) :: phase_name_len
    character(len=*), intent(in) :: phase_name
    integer, intent(in) :: pet_range_start
    integer, intent(in) :: pet_range_end
    real(ESMF_KIND_R8), intent(in) :: time_intvl_start
    real(ESMF_KIND_R8), intent(in) :: time_intvl_end
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Collects all info required by the ESMF\_Mapper from a component
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     The mapper class;
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    

    integer :: localrc = ESMF_RC_NOT_IMPL

    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call the C entry point
    call c_ESMC_MapperSetCompInfo(mapper,&
          comp_name_len, comp_name,&
          phase_name_len, phase_name,&
          pet_range_start, pet_range_end,&
          time_intvl_start, time_intvl_end,&
          localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT,&
          rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_MapperUtilMod
