! Simple ESMF application demonstrating VM features
!
!BOE
!
! \subsubsection{VM Component Example}
!
! The following example shows the role that VMs play in connetion with ESMF 
! components. Here a single component is created in the main program and the
! default VM gives all its resources to the child component. When the child
! component code is entered through the registered methods (Initialize, Run or 
! Finalize) the user code is executed in the childs VM.
!
!EOE
!

!BOC
module ESMF_VMTest6Ex_gcomp_mod

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
    type(ESMF_GridComp), intent(inout):: gcomp
    integer, intent(out):: rc
    
    print *, '*** hi from mygcomp_register ***'
    
    ! Currently only the MPI-only VM is accessible!!!
    ! Optionally set properties for this component's VM via one of three methods
    !    call ESMF_GridCompSetVMMaxThreads(gcomp, ...)
    !    call ESMF_GridCompSetVMMinThreads(gcomp, ...)
    !    call ESMF_GridCompSetVMMaxPEs(gcomp, ...)
    
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
    ! arguments
    type(ESMF_GridComp), intent(inout):: gcomp
    type(ESMF_State), intent(in):: istate, estate
    type(ESMF_Clock), intent(in):: clock
    integer, intent(out):: rc

    ! local variables
    type(ESMF_VM):: vm
    
    print *, '*** hi from mygcomp_init ***'
    
    ! get the vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)
    
    rc = 0

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
    ! like mygcomp_init...
!EOC
    ! arguments
    type(ESMF_GridComp), intent(inout):: gcomp
    type(ESMF_State), intent(in):: istate, estate
    type(ESMF_Clock), intent(in):: clock
    integer, intent(out):: rc
    
    ! local variables
    type(ESMF_VM):: vm

    print *, '*** hi from mygcomp_run ***'
    
    ! get the vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)

    rc = 0
!BOC
  end subroutine !--------------------------------------------------------------

  recursive subroutine mygcomp_final(gcomp, istate, estate, clock, rc)
    ! like mygcomp_init...
!EOC
    ! arguments
    type(ESMF_GridComp), intent(inout):: gcomp
    type(ESMF_State), intent(in):: istate, estate
    type(ESMF_Clock), intent(in):: clock
    integer, intent(out):: rc

    ! local variables
    type(ESMF_VM):: vm

    print *, '*** hi from mygcomp_final ***'

    ! get the vm    
    call ESMF_GridCompGet(gcomp, vm=vm)

    call ESMF_VMPrint(vm, rc)

    rc = 0
!BOC
  end subroutine !--------------------------------------------------------------

end module
!EOC


!BOC
program ESMF_VMTest6Ex

  use ESMF_Mod
  use ESMF_VMTest6Ex_gcomp_mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  type(ESMF_GridComp):: gcomp
  type(ESMF_Clock):: clock
  type(ESMF_State):: dummystate

  call ESMF_Initialize(vm=vm, rc=rc)

  gcomp = ESMF_GridCompCreate(vm, 'My gridded component', rc=rc)

  call ESMF_GridCompSetServices(gcomp, mygcomp_register, rc)

  call ESMF_GridCompInitialize(gcomp, dummystate, dummystate, clock, rc=rc)
  call ESMF_GridCompRun(gcomp, dummystate, dummystate, clock, rc=rc)
  call ESMF_GridCompFinalize(gcomp, dummystate, dummystate, clock, rc=rc)

  call ESMF_GridCompDestroy(gcomp)
  
  call ESMF_Finalize(rc)
  
end program
!EOC
