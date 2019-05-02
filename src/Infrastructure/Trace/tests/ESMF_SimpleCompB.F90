! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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

module SimpleCompB

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
      
      print *, "Inside Init"

      rc = ESMF_SUCCESS

      !call ESMF_TraceRegionEnter("NEVER_EXIT", rc=rc)
      !if (rc /= ESMF_SUCCESS) return
           
    end subroutine Init

    subroutine Run(gcomp, istate, estate, clock, rc)
      type(ESMF_GridComp):: gcomp
      type(ESMF_State):: istate, estate
      type(ESMF_Clock):: clock
      integer, intent(out):: rc

      integer :: i, j, k
      character(len=64) :: name, name2
      
      
      call ESMF_TraceRegionEnter("my_run", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      
      !print *, "Inside Run"

      ! do i=1,25
      !   write(name, "(A25,I2)") "region_with_longer_name_", i
      !   call ESMF_TraceRegionEnter(trim(name), rc=rc)
      !   if (rc /= ESMF_SUCCESS) return

      !   ! waste some time
      !   k=1
      !   do j=1,i*10000
      !     k=k+1
      !     if (mod(k, 10000) == 0) then
      !       write(name2, "(A13,I6)") "inner_region_", k
      !       call ESMF_TraceRegionEnter(trim(name2), rc=rc)
      !       if (rc /= ESMF_SUCCESS) return
      !       call ESMF_TraceRegionExit(trim(name2), rc=rc)
      !       if (rc /= ESMF_SUCCESS) return
      !     endif
      !   enddo

      !   call ESMF_TraceRegionExit(trim(name), rc=rc)
      !   if (rc /= ESMF_SUCCESS) return        
      ! enddo      

      call ESMF_TraceRegionExit("my_run", rc=rc)
      if (rc /= ESMF_SUCCESS) return

      rc = ESMF_SUCCESS
      
    end subroutine Run

    subroutine Finalize(gcomp, istate, estate, clock, rc)
      type(ESMF_GridComp):: gcomp
      type(ESMF_State):: istate, estate
      type(ESMF_Clock):: clock
      integer, intent(out):: rc
      
      print *, "Inside Finalize"

      rc = ESMF_SUCCESS
      
  end subroutine Finalize 

end module SimpleCompB
