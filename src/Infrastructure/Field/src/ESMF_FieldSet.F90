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
  use ESMF_FieldGetMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_FieldSet, ESMF_FieldSetTimestamp, ESMF_FieldSync


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
      call ESMF_SetName(field%ftypep%base, name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldSet
!------------------------------------------------------------------------------

! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSetTimestamp()"
!BOPI
! !IROUTINE: ESMF_FieldSetTimestamp - Set timestamp on Field
!
! !INTERFACE:
  subroutine ESMF_FieldSetTimestamp(field, timestamp, rc)

!
! !ARGUMENTS:
    type(ESMF_Field),   intent(inout)         :: field
    integer,            intent(in)            :: timestamp(10)
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!     Set timestamp on an {\tt ESMF\_Field} object. 
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!       {\tt ESMF\_Field} object for which to set properties.
!     \item [timestamp]
!       Timestamp, an array of 10 integer values.
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc)

    ! Set timestamp
    field%ftypep%timestamp(:) = timestamp(:)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldSetTimestamp
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSync()"
!BOP
! !IROUTINE: ESMF_FieldSync - Synchronize DEs across the Field in case of sharing

! !INTERFACE:
  subroutine ESMF_FieldSync(field, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,          intent(out), optional :: rc  
!
! !DESCRIPTION:
!     Synchronizes access to DEs across {\tt field} to make sure PETs correctly
!     access the data for read and write when DEs are shared. 
!
!     The arguments are:
!     \begin{description}
!     \item[field] 
!          Specified {\tt ESMF\_Field} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Array)        :: array

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc)
    
    ! Retrieve the Array object and call into the Array API
    call ESMF_FieldGet(field, array=array, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_ArraySync(array, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_FieldSync
!------------------------------------------------------------------------------

end module ESMF_FieldSetMod
