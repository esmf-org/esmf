! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research, 
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
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
!  use ESMF_ArraySpecMod
!  use ESMF_LocalArrayMod
!  use ESMF_DELayoutMod
!  use ESMF_StaggerLocMod
!  use ESMF_DistGridMod
  use ESMF_GridMod
!  use ESMF_GeomBaseMod
!  use ESMF_ArrayMod
!  use ESMF_ArrayCreateMod
!  use ESMF_ArrayGetMod
  use ESMF_TimeMod
  use ESMF_InitMacrosMod
  use ESMF_MapperMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_Mapper class

  type ESMF_MapperExecutionBlock
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_MapperExecutionBlock

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_MapperCollect          ! Collect details from a component

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
#define ESMF_METHOD "ESMF_MapperCollect()"
!BOP
! !IROUTINE: ESMF_MapperCreate - Create a mapper

! !INTERFACE:
  subroutine ESMF_MapperCollect(keywordEnforcer, rc)
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Collects all info required by the ESMF\_Mapper from a component
!
! The arguments are:
!   \begin{description}
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_MapperUtilMod
