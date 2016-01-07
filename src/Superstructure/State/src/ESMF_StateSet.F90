! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
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

#define ESMF_ENABLESTATENEEDED
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
      use ESMF_StateMod
      use ESMF_InitMacrosMod
      
      implicit none
      
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

#if defined (ESMF_ENABLESTATENEEDED)
      public ESMF_StateSetNeeded
#endif

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
#define ESMF_METHOD "ESMF_StateSetNeeded"
!BOPI
! !IROUTINE: ESMF_StateSetNeeded - Set if a data item is needed
!
! !INTERFACE:
      subroutine ESMF_StateSetNeeded(state, itemName, neededflag, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_State),  intent(inout)   :: state
      character (len=*), intent(in)      :: itemName
      type(ESMF_NeededFlag), intent(in)  :: neededflag
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,    intent(out),  optional :: rc             

!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      Sets the status of the {\tt needed} flag for the data item
!      named by {\tt itemName} in the {\tt ESMF\_State}.
!
!     The arguments are:
!      \begin{description}     
!      \item[state]
!        The {\tt ESMF\_State} to set.
!       \item[itemName]
!        Name of the data item to set.
!       \item[neededflag]
!        Set status of data item to this.  See Section~\ref{opt:neededflag}
!        for possible values.
!       \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!       \end{description}
!
!EOPI

      type(ESMF_StateItem), pointer :: dataitem
      logical :: exists
      integer :: localrc

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

!      exists = ESMF_StateClassFindData(state%statep, itemName, .true., &
!                                      dataitem=dataitem, rc=localrc)
!      if (.not. exists) then
!          if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, msg=itemName, &
!                                      ESMF_CONTEXT, rcToReturn=rc)) return
!      endif
!
!      dataitem%needed = neededflag
!
!      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetNeeded
!------------------------------------------------------------------------------
      end module ESMF_StateSetMod
