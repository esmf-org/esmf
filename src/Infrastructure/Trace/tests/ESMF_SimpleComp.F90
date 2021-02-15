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
!
! Supporting code for Trace unit tests.
!

module SimpleComp

  use ESMF
  implicit none

  private
  public SetServices

contains

  subroutine SetServices(gcomp, rc)
      type(ESMF_GridComp)   :: gcomp
      integer, intent(out)  :: rc  

      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
           userRoutine=Init, rc=rc)
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
           userRoutine=Run, rc=rc)
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
           userRoutine=Finalize, rc=rc)
      
      rc = ESMF_SUCCESS
      
    end subroutine SetServices

    subroutine Init(gcomp, istate, estate, clock, rc)
      type(ESMF_GridComp):: gcomp
      type(ESMF_State):: istate, estate
      type(ESMF_Clock):: clock
      integer, intent(out):: rc

      rc = ESMF_SUCCESS
      
      call ESMF_TraceRegionEnter("user1", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      
      call ESMF_TraceRegionEnter("user1a", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      
      print *, "Inside Init"

      call ESMF_TraceRegionExit("user1a", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      call ESMF_TraceRegionExit("user1", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      
      
    end subroutine Init

    subroutine Run(gcomp, istate, estate, clock, rc)
      type(ESMF_GridComp):: gcomp
      type(ESMF_State):: istate, estate
      type(ESMF_Clock):: clock
      integer, intent(out):: rc

      rc = ESMF_SUCCESS

      call ESMF_TraceRegionEnter("user1a", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      
      print *, "Inside Run"

      call ESMF_TraceRegionExit("user1a", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      
      
    end subroutine Run

    subroutine Finalize(gcomp, istate, estate, clock, rc)
      type(ESMF_GridComp):: gcomp
      type(ESMF_State):: istate, estate
      type(ESMF_Clock):: clock
      integer, intent(out):: rc
      
      print *, "Inside Finalize"

      rc = ESMF_SUCCESS
      
  end subroutine Finalize 

end module SimpleComp
