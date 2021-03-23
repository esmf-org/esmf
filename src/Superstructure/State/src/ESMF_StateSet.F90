! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateSet.F90"
!
!     ESMF StateSet module
module ESMF_StateSetMod
!
!==============================================================================
!
! This file contains the State Set methods
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateSetMod - State Set Module
!
! !DESCRIPTION:
!
! The code in this file implements the State Set methods
!
!
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_StateTypesMod
      use ESMF_StateVaMod
      use ESMF_InitMacrosMod
      
      implicit none
      
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateSet

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateSet"
!BOP
! !IROUTINE: ESMF_StateSet - Set State aspects
!
! !INTERFACE:
  subroutine ESMF_StateSet(state, keywordEnforcer, stateIntent, rc)
!
! !ARGUMENTS:
    type(ESMF_State),            intent(inout)         :: state
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_StateIntent_Flag), intent(in),  optional :: stateIntent
    integer,                     intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Set the info in the {\tt state} object.
!
!     The arguments are:
!      \begin{description}     
!      \item[state]
!        The {\tt ESMF\_State} to set.
!      \item[stateIntent]
!        Intent, e.g. Import or Export, of this {\tt ESMF\_State}.
!        Possible values are listed in Section~\ref{const:stateintent}.
!      \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!      \end{description}
!
!EOP
!------------------------------------------------------------------------------
    type(ESMF_StateClass), pointer :: stypep
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    stypep => state%statep
    if (present(stateIntent)) stypep%st = stateintent
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_StateSet
!------------------------------------------------------------------------------

end module ESMF_StateSetMod
