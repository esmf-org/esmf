! $Id: ESMF_VMComponentEx.F90,v 1.17.2.1 2010/02/05 20:01:28 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{VM and Components}
!
! The following example shows the role that the VM plays in connection with ESMF 
! Components. A single Component is created in the main program. Through the
! optional {\tt petList} argument the driver code specifies that only resources
! associated with PET 0 are given to the {\tt gcomp} object. 
!
! When the Component code is invoked through the standard ESMF Component methods
! Initialize, Run, or Finalize the Component's VM is automatically entered.
! Inside of the user-written Component code the Component VM can be obtained
! by querying the Component object. The VM object will indicate that only a
! single PET is executing the Component code.
!
!EOE
!------------------------------------------------------------------------------

!BOC
module ESMF_VMComponentEx_gcomp_mod
!EOC

  ! modules
  use ESMF_Mod
  
  implicit none
  
  ! module variables
  private
  
  ! module procedures
  
  public mygcomp_register
    
  contains !--------------------------------------------------------------------

  subroutine mygcomp_register(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! register INIT method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, mygcomp_init, rc=rc)
    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, mygcomp_run, rc=rc)
    ! register FINAL method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, mygcomp_final, rc=rc)
  end subroutine !--------------------------------------------------------------
  
!BOC
  recursive subroutine mygcomp_init(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: istate, estate
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    type(ESMF_VM):: vm
    
    ! get this Component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)
    
    ! the VM object contains information about the execution environment of
    ! the Component

    call ESMF_VMPrint(vm, rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------

  
  recursive subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: istate, estate
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    type(ESMF_VM):: vm
    
    ! get this Component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)
    
    ! the VM object contains information about the execution environment of
    ! the Component

    call ESMF_VMPrint(vm, rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------

  recursive subroutine mygcomp_final(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: istate, estate
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    type(ESMF_VM):: vm
    
    ! get this Component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)
    
    ! the VM object contains information about the execution environment of
    ! the Component

    call ESMF_VMPrint(vm, rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------

end module
!EOC


!BOC
program ESMF_VMComponentEx
  use ESMF_Mod
  use ESMF_VMComponentEx_gcomp_mod
  implicit none
  
  ! local variables
!EOC  
  integer:: rc
  type(ESMF_GridComp):: gcomp
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC  
  gcomp = ESMF_GridCompCreate(petList=(/0/), rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC  
  call ESMF_GridCompSetServices(gcomp, mygcomp_register, rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC  
  call ESMF_GridCompInitialize(gcomp, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_GridCompRun(gcomp, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_GridCompFinalize(gcomp, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC  
  call ESMF_GridCompDestroy(gcomp, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
!BOC  
  call ESMF_Finalize(rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMComponentEx.F90"
  else
    print *, "FAIL: ESMF_VMComponentEx.F90"
  endif
  
!BOC  
end program
!EOC
