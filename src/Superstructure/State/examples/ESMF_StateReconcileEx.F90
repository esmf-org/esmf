! $Id: ESMF_StateReconcileEx.F90,v 1.6 2005/02/28 16:23:02 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
    program ESMF_StateReconcileEx

!------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_StateReconcileEx - State reconciliation
!
! !DESCRIPTION:
!
! This program shows examples of using the State Reconcile function
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    implicit none

    ! Interface blocks for subroutines at end of program
    interface
      subroutine comp_dummy(gcomp, rc)
        use ESMF_MOd
        type(ESMF_GridComp), intent(inout) :: gcomp
        integer, intent(out) :: rc
      end subroutine comp_dummy

      subroutine comp1_init(gcomp, istate, ostate, clock, rc)
        use ESMF_Mod
        type(ESMF_State), intent(inout) :: istate, ostate
        type(ESMF_Clock), intent(in) :: clock
    
      end subroutine comp1_init
    
      subroutine comp2_init(gcomp, istate, ostate, clock, rc)
        use ESMF_Mod
        type(ESMF_GridComp), intent(inout) :: gcomp
        type(ESMF_State), intent(inout) :: istate, ostate
        type(ESMF_Clock), intent(in) :: clock
        integer, intent(out) :: rc
    
      end subroutine comp2_init

    end interface

    ! Local variables
    integer :: rc
    type(ESMF_State) :: state1
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_VM) :: vm, vmsub1, vmsub2
    character(len=ESMF_MAXSTR) :: comp1name, comp2name, statename

!EOC
    integer :: finalrc
    finalrc = ESMF_SUCCESS


    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Creating Components on subsets of the current PET list}
!      
!  A Component can be created which will run only on a subset of the
!  current PET list.
!EOE
 
    print *, "State Reconcile Example 1: Component Creation"

!BOC
    ! Get the global VM for this job.
    call ESMF_VMGetGlobal(vm=vm, rc=rc)

    comp1name = "Atmosphere"
    comp1 = ESMF_GridCompCreate(vm, comp1name, petList=(/ 0, 1 /), rc=rc)
    call ESMF_GridCompGet(comp1, vm=vmsub1, rc=rc)
    print *, "GridComp Create returned, name = ", trim(comp1name)

    comp2name = "Ocean"
    comp2 = ESMF_GridCompCreate(vm, comp2name, petList=(/ 2, 3 /), rc=rc)
    call ESMF_GridCompGet(comp2, vm=vmsub2, rc=rc)
    print *, "GridComp Create returned, name = ", trim(comp2name)

    statename = "Ocn2Atm"
    state1 = ESMF_StateCreate(statename, rc=rc)  
!EOC
    print *, "State Create returned, name = ", trim(statename)

    print *, "State Example 1 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Invoking Components on a subset of the Parent PETs}
!   
!  Here we register the subroutines which should be called for initialization.
!  Then we call ESMF\_GridCompInitialize() on all PETs, but the code runs
!  only on the PETs given in the petList when the Component was created.
!
!  Because this example is so short, we call the entry point code
!  directly instead of the normal procedure of nesting it in a separate
!  SetServices() subroutine.  
!
!EOE

!BOC
    ! This is where the VM for each component is initialized.
    ! Normally you would call SetEntryPoint inside set services,
    ! but to make this example very short, they are called inline below.
    call ESMF_GridCompSetServices(comp1, comp_dummy, rc)
    call ESMF_GridCompSetServices(comp2, comp_dummy, rc)

    print *, "ready to set entry point 1"
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_SETINIT, &
                                        comp1_init, ESMF_SINGLEPHASE, rc)

    print *, "ready to set entry point 2"
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_SETINIT, &
                                        comp2_init, ESMF_SINGLEPHASE, rc)


    print *, "ready to call init for comp 1"
    call ESMF_GridCompInitialize(comp1, state1, rc=rc)
    print *, "ready to call init for comp 2"
    call ESMF_GridCompInitialize(comp2, state1, rc=rc)
!EOC

    print *, "State Example 2 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Using State Reconcile}
!   
! Now we have {\tt state1} containing {\tt field1} on PETs 0 and 1, and
! {\tt state1} containing {\tt field2} on PETs 2 and 3.  For the code
! to have a rational view of the data, we call {\tt ESMF\_StateReconcile}
! which determines which objects are missing from any PET, and communicates
! information about the object.  After the call to reconcile, all
! {\tt ESMF\_State} objects now have a consistent view of the data.
!EOE

!BOC
    print *, "State before calling StateReconcile()"
    call ESMF_StatePrint(state1, rc=rc)

    call ESMF_StateReconcile(state1, vm, rc=rc)

    print *, "State after calling StateReconcile()"
    call ESMF_StatePrint(state1, rc=rc)
!EOC

    print *, "State Example 3 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
    call ESMF_Finalize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_StateReconcileEx.F90"
    else
        print *, "FAIL: ESMF_StateReconcileEx.F90"
    end if

!BOC
end program ESMF_StateReconcileEx

!BOE
!\subsubsection{Initialization and SetServices Routines}
!  
! These are the separate subroutines called by the code above.
!EOE

!BOC
! Initialize routine which creates "field1" on PETs 0 and 1
subroutine comp1_init(gcomp, istate, ostate, clock, rc)
    use ESMF_Mod
    type(ESMF_GridComp), intent(inout) :: gcomp
    type(ESMF_State), intent(inout) :: istate, ostate
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field1
    integer :: localrc

    print *, "i am comp1_init"

    field1 = ESMF_FieldCreateNoData(name="Comp1 Field", rc=localrc)
  
    call ESMF_StateAddField(istate, field1, rc=localrc)
    
    rc = localrc

end subroutine comp1_init

! Initialize routine which creates "field2" on PETs 2 and 3
subroutine comp2_init(gcomp, istate, ostate, clock, rc)
    use ESMF_Mod
    type(ESMF_GridComp), intent(inout) :: gcomp
    type(ESMF_State), intent(inout) :: istate, ostate
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field2
    integer :: localrc

    print *, "i am comp2_init"

    field2 = ESMF_FieldCreateNoData(name="Comp2 Field", rc=localrc)
    
    call ESMF_StateAddField(istate, field2, rc=localrc)

    rc = localrc

end subroutine comp2_init

subroutine comp_dummy(gcomp, rc)
   use ESMF_MOd
   type(ESMF_GridComp), intent(inout) :: gcomp
   integer, intent(out) :: rc

   rc = ESMF_SUCCESS
end subroutine comp_dummy
!EOC
    
