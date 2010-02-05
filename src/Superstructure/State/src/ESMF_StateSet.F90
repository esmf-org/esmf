! $Id: ESMF_StateSet.F90,v 1.2.4.1 2010/02/05 20:05:07 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

      public ESMF_StateSetNeeded

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateSet.F90,v 1.2.4.1 2010/02/05 20:05:07 svasquez Exp $'

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
!BOP
! !IROUTINE: ESMF_StateSetNeeded - Set if a data item is needed
!
! !INTERFACE:
      subroutine ESMF_StateSetNeeded(state, itemName, neededflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      character (len=*), intent(in) :: itemName
      type(ESMF_NeededFlag), intent(in) :: neededflag
      integer, intent(out), optional :: rc             

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
!EOP

      type(ESMF_StateItem), pointer :: dataitem
      logical :: exists
      integer :: localrc

      ! Assume failure until we know we will succeed
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check input variables
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)


      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      exists = ESMF_StateClassFindData(state%statep, itemName, .true., &
                                      dataitem, rc=localrc)
      if (.not. exists) then
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, itemName, &
                                      ESMF_CONTEXT, rc)) return
      endif

      dataitem%needed = neededflag

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateSetNeeded
!------------------------------------------------------------------------------
      end module ESMF_StateSetMod
