! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateVa.F90"
!
!     ESMF StateVa module
      module ESMF_StateVaMod
!
!==============================================================================
!
! This file contains the State Validate method
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"

#define ESMF_ENABLEBIGNAMEMAP
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateVaMod - State Va Module
!
! !DESCRIPTION:
!
! The code in this file implements the State Validate method.
!
!
! !USES:
      use ESMF_ArrayMod,       only: ESMF_ArrayValidate
      use ESMF_ArrayBundleMod, only: ESMF_ArrayBundleValidate
      use ESMF_FieldMod,       only: ESMF_FieldValidate
      use ESMF_FieldBundleMod, only: ESMF_FieldBundleValidate
      use ESMF_LogErrMod
      use ESMF_RHandleMod,     only: ESMF_RouteHandleValidate
      use ESMF_StateTypesMod
      use ESMF_StateContainerMod
      use ESMF_InitMacrosMod
      use ESMF_UtilTypesMod
      
      implicit none
      
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateValidate

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
#define ESMF_METHOD "ESMF_StateValidate"
!BOP
! !IROUTINE: ESMF_StateValidate - Check validity of a State
!
! !INTERFACE:
      subroutine ESMF_StateValidate(state, keywordEnforcer, nestedFlag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,          intent(in),  optional :: nestedFlag
      integer,          intent(out), optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Validates that the {\tt state} is internally consistent.
!      Currently this method determines if the {\tt State} is uninitialized 
!      or already destroyed.  The method returns an error code if problems 
!      are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to validate.
!     \item[{[nestedFlag]}]
!       {\tt .false.} - validates at the current State level only (default)
!       {\tt .true.} - recursively validates any nested States
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!
! TODO: code goes here
!
      integer :: localrc
      logical :: localnestedflag
      type(ESMF_StateClass), pointer :: stypep

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_SUCCESS

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      localnestedflag = .false.
      if (present (nestedFlag)) then
        localnestedflag = nestedFlag
      end if

      ! Validate the State

      if (.not. associated (state%statep)) then
          if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                 msg="State uninitialized or already destroyed", &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
      endif


      if (localnestedflag) then
        stypep => state%statep
        call validateWorker (stypep, rc=localrc)
      end if

      if (present(rc)) rc = localrc

      contains

        recursive subroutine validateWorker (sp, rc)
          type(ESMF_StateClass), pointer :: sp
          integer, intent (out) :: rc

          integer :: i1
          integer :: local1rc
          type(ESMF_StateItemWrap),  pointer :: ptrs(:)
          type(ESMF_StateItem), pointer :: sip
          integer :: memstat1

          rc = ESMF_SUCCESS

          if (sp%st == ESMF_STATEINTENT_INVALID) then
            if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                msg="State uninitialized or already destroyed", &
                ESMF_CONTEXT, rcToReturn=rc)) return
          end if

          ptrs => null ()
          call ESMF_ContainerGet (sp%StateContainer, itemList=ptrs, rc=local1rc)
          if (ESMF_LogFoundError(local1rc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          do, i1 = 1, size (ptrs)
            local1rc = ESMF_SUCCESS
            sip => ptrs(i1)%si

            select case (sip%otype%ot)
            case (ESMF_STATEITEM_ARRAY%ot)
              call ESMF_ArrayValidate (sip%datap%ap, rc=local1rc)

            case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
              call ESMF_ArrayBundleValidate (sip%datap%abp, rc=local1rc)

            case (ESMF_STATEITEM_FIELD%ot)
              call ESMF_FieldValidate (sip%datap%fp, rc=local1rc)

            case (ESMF_STATEITEM_FIELDBUNDLE%ot)
              call ESMF_FieldBundleValidate (sip%datap%fbp, rc=local1rc)

            case (ESMF_STATEITEM_ROUTEHANDLE%ot)
              call ESMF_RouteHandleValidate (sip%datap%rp, rc=local1rc)

            case (ESMF_STATEITEM_STATE%ot)
              if (localnestedflag)  &
                call validateWorker (sip%datap%spp, rc=local1rc)

#if 0
            case (ESMF_STATEITEM_NAME%ot, ESMF_STATEITEM_INDIRECT%ot)
              continue
#endif

            case (ESMF_STATEITEM_UNKNOWN%ot, ESMF_STATEITEM_NOTFOUND%ot)
              local1rc = ESMF_RC_OBJ_BAD

            case default
              local1rc = ESMF_RC_OBJ_BAD

            end select

            if (local1rc /= ESMF_SUCCESS) then
              rc = ESMF_RC_OBJ_BAD
              if (ESMF_LogFoundError(local1rc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) exit
            end if

          end do

          if (associated (ptrs)) then
            deallocate (ptrs, stat=memstat1)
            if (ESMF_LogFoundDeallocError(memstat1, msg= "deallocating pointers", &
        ESMF_CONTEXT, rcToReturn=rc)) return
          end if

        end subroutine validateWorker

      end subroutine ESMF_StateValidate

!------------------------------------------------------------------------------



      end module ESMF_StateVaMod





