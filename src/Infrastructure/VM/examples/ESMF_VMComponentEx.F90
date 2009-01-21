! $Id: ESMF_VMComponentEx.F90,v 1.11.2.4 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
! \subsubsection{VM Component Example}
!
! The following example shows the role that VMs play in connetion with ESMF 
! components. Here a single component is created in the main program and the
! default VM gives all its resources to the child component. When the child
! component code is entered through the registered methods (Initialize, Run or 
! Finalize) the user code will be executed in the child's VM.
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
!BOC
  public mygcomp_register
    
  contains !--------------------------------------------------------------------

  subroutine mygcomp_register(gcomp, rc)
!EOC
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    print *, '*** hi from mygcomp_register ***'
    
    ! Currently only the MPI-only VM is accessible!!!
    ! Optionally set properties for this component's VM via one of three methods
    !    call ESMF_GridCompSetVMMaxThreads(gcomp, ...)
    !    call ESMF_GridCompSetVMMinThreads(gcomp, ...)
    !    call ESMF_GridCompSetVMMaxPEs(gcomp, ...)
    
!BOC
    ! register INIT method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, mygcomp_init, &
      ESMF_SINGLEPHASE, rc)
    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, mygcomp_run, &
      ESMF_SINGLEPHASE, rc)
    ! register FINAL method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, mygcomp_final, &
      ESMF_SINGLEPHASE, rc)
  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_init(gcomp, istate, estate, clock, rc)
!EOC
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! local variables
    type(ESMF_VM):: vm
    
    print *, '*** hi from mygcomp_init ***'
    
!BOC
    ! get this component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)
!EOC
    
    rc = 0

!BOC
  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
!EOC
    ! like mygcomp_init...
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! local variables
    type(ESMF_VM):: vm

    print *, '*** hi from mygcomp_run ***'
    
!BOC
    ! get this component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)
!EOC
    
    rc = 0

!BOC
  end subroutine !--------------------------------------------------------------

  recursive subroutine mygcomp_final(gcomp, istate, estate, clock, rc)
!EOC
    ! like mygcomp_init...
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! local variables
    type(ESMF_VM):: vm

    print *, '*** hi from mygcomp_final ***'

!BOC
    ! get this component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)
!EOC
    
    rc = 0

!BOC
  end subroutine !--------------------------------------------------------------

end module
!EOC


!BOC
program ESMF_VMComponentEx
!EOC
  use ESMF_Mod
!BOC  
  use ESMF_VMComponentEx_gcomp_mod
!EOC  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_GridComp):: gcomp
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC  
  gcomp = ESMF_GridCompCreate(name='My gridded component', rc=rc)
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
