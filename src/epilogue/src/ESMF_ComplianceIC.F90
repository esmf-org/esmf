! $Id: ESMF_ComplianceIC.F90,v 1.2 2010/10/01 00:09:03 theurich Exp $
!
! Compliance Interface Component
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
! !DESCRIPTION:
!  Interface Component
!-------------------------------------------------------------------------

module ESMF_ComplianceICMod

  ! ESMF module
  use ESMF_Mod

  implicit none
  
  private
  
  public setvmIC, registerIC
        
  contains

!-------------------------------------------------------------------------
!   !  The setvm routine is used by the child component to set VM properties
!   !TODO:  currently the setvmIC() is _not_ hooked into the ESMF callback 

  subroutine setvmIC(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
    
    ! Initialize user return code
    rc = ESMF_SUCCESS
    
    print *, "entering setvmIC"

    ! This code is being executed _after_ the actual Component SetVM call
    
    !TODO: currently the setvmIC() is _not_ hooked into the ESMF callback 

    print *, "leaving setvmIC"

  end subroutine

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine registerIC(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc

    ! Initialize user return code
    rc = ESMF_SUCCESS
    
    ! IMPORTANT: As an InterfaceComponent the code must ensure:
    ! 1) That the return code from the actual child method is returned to the 
    !    parent (note that this is not currently possible for the register)

    ! This code is being executed _after_ the actual Component Register call

    print *, "entering registerIC"
    
    !---------------------------------------------------------------------------
    ! Start Compliance Checking
    
    ! Stop Compliance Checking
    !---------------------------------------------------------------------------

    ! Register the IC callback routines.
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINITIC, userRoutine=ic_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUNIC, userRoutine=ic_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINALIC, userRoutine=ic_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "leaving registerIC"
    
  end subroutine

!-------------------------------------------------------------------------
!   !   Initialization routine.
    
  subroutine ic_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    print *, "entering ic_init"

    call ESMF_GridCompInitializeAct(comp, importState, exportState, clock, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    !---------------------------------------------------------------------------
    ! Start Compliance Checking
    
    ! the Component attributes should be set up, ready to output
    
!    call ESMF_AttributeWrite(comp, convention='CIM 1.0', &
!      purpose='Model Component Simulation Description', &
!      attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
!    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Stop Compliance Checking
    !---------------------------------------------------------------------------

    print *, "leaving ic_init"

  end subroutine ic_init


!-------------------------------------------------------------------------
!   !  Run routine
 
  subroutine ic_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    print *, "entering ic_run"

    call ESMF_GridCompRunAct(comp, importState, exportState, clock, rc=rc)

    print *, "leaving ic_run"

  end subroutine ic_run


!-------------------------------------------------------------------------
!   !  Finalize routine
 
  subroutine ic_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    
    ! Initialize user return code
    rc = ESMF_SUCCESS

    print *, "entering ic_final"

    call ESMF_GridCompFinalizeAct(comp, importState, exportState, clock, rc=rc)

    print *, "leaving ic_final"

  end subroutine ic_final


end module ESMF_ComplianceICMod
    

!-------------------------------------------------------------------------
! The register routine of internal ICs must be available as an external routine

subroutine ESMF_ComplianceICRegister(comp, rc)
  use ESMF_Mod
  use ESMF_ComplianceICMod
  implicit none
  type(ESMF_GridComp)   :: comp
  integer               :: rc
  
  call registerIC(comp, rc)   ! simply call the internal IC module's register
  
end subroutine
