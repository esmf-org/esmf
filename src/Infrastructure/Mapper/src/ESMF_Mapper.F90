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
#define ESMF_FILENAME "ESMF_Mapper.F90"
!==============================================================================
!
!     ESMF Mapper module
module ESMF_MapperMod
!
!==============================================================================
!
! This file contains the Mapper class definition and Mapper class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_MapperMod - Mapper class
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Mapper} class
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

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_Mapper class

  type ESMF_Mapper
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Mapper

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_MapperCreate          ! Create a mapper
   public ESMF_MapperDestroy          ! Destroy a mapper

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
#define ESMF_METHOD "ESMF_MapperCreate()"
!BOP
! !IROUTINE: ESMF_MapperCreate - Create a mapper

! !INTERFACE:
  function ESMF_MapperCreate(keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_Mapper) :: ESMF_MapperCreate
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Returns the ESMF\_Mapper that  has been created.
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
  end function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MapperDestroy()"
!BOP
! !IROUTINE: ESMF_MapperDestroy - Destroy a mapper, release resources

! !INTERFACE:
  subroutine ESMF_MapperDestroy(mapper, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Mapper), intent(inout) :: mapper
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Destroys the ESMF\_Mapper
!
! The arguments are:
!   \begin{description}
!   \item[{[mapper]}]
!     Mapper class; 
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_MapperMod
