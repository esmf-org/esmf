! $Id$
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
#define ESMF_FILENAME "ESMF_FieldSet.F90"
!==============================================================================
!
!     ESMF FieldSet module
module ESMF_FieldSetMod
!
!==============================================================================
!
! This file contains the FieldSet methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldSetMod - Set object-wide Field properties
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldSet} methods, which sets
! properoties of a {\tt ESMF\_Field} object.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_DELayoutMod
  use ESMF_StaggerLocMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_GeomBaseMod
  use ESMF_ArrayMod
  use ESMF_ArrayCreateMod
  use ESMF_ArrayGetMod
  use ESMF_TimeMod
  use ESMF_InitMacrosMod
  use ESMF_FieldMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_FieldSet


!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSet()"
!BOP
! !IROUTINE: ESMF_FieldSet - Set object-wide Field information
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSet()
  subroutine ESMF_FieldSet(field, keywordEnforcer, name, rc)

!
! !ARGUMENTS:
    type(ESMF_Field),   intent(inout)         :: field
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!     Sets adjustable settings in an {\tt ESMF\_Field} object. 
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!       {\tt ESMF\_Field} object for which to set properties.
!     \item [{[name]}]
!       The Field name.
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc)
    
    ! Set the name in Base object
    if (present(name)) then
      !call ESMF_ArraySet(field%ftypep%array, name=name, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !  ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_SetName(field%ftypep%base, name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldSet

end module ESMF_FieldSetMod
