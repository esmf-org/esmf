! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!

module ESMF_StateReconcileEx_Mod

use ESMF

contains

!BOE
!\subsubsection{{\tt ESMF\_StateReconcile()} usage}
!  
! The set services routines are used to tell ESMF which routine
! hold the user code for the initialize, run, and finalize
! blocks of user level Components.
! These are the separate subroutines called by the code below.
!EOE

!BOC
! Initialize routine which creates "field1" on PETs 0 and 1
subroutine comp1_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field1
    integer :: localrc

    print *, "i am comp1_init"

    field1 = ESMF_FieldEmptyCreate(name="Comp1 Field", rc=localrc)
  
    call ESMF_StateAdd(istate, (/field1/), rc=localrc)
    
    rc = localrc

end subroutine comp1_init

! Initialize routine which creates "field2" on PETs 2 and 3
subroutine comp2_init(gcomp, istate, ostate, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: istate, ostate
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field2
    integer :: localrc

    print *, "i am comp2_init"

    field2 = ESMF_FieldEmptyCreate(name="Comp2 Field", rc=localrc)
    
    call ESMF_StateAdd(istate, (/field2/), rc=localrc)

    rc = localrc

end subroutine comp2_init

subroutine comp_dummy(gcomp, rc)
   type(ESMF_GridComp)  :: gcomp
   integer, intent(out) :: rc

   rc = ESMF_SUCCESS
end subroutine comp_dummy
!EOC
    
end module ESMF_StateReconcileEx_Mod






    program ESMF_StateReconcileEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_StateReconcileEx - State reconciliation
!
! !DESCRIPTION:
!
! This program shows examples of using the State Reconcile function
!-----------------------------------------------------------------------------
#include "ESMF.h"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod
    use ESMF_StateReconcileEx_Mod
    implicit none

    ! Local variables
    integer :: rc, petCount
    type(ESMF_State) :: state1
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_VM) :: vm
    character(len=ESMF_MAXSTR) :: comp1name, comp2name, statename

!EOC
    integer :: finalrc, result
    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_StateReconcileEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

    finalrc = ESMF_SUCCESS


    call ESMF_Initialize(vm=vm, defaultlogfilename="StateReconcileEx.Log", &
                     logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    
    ! verify that this example can run on the given petCount
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    if (petCount<4) then
      print *, "This test must run on at least 4 PETs."
      goto 20
    endif
    

!-------------------------------------------------------------------------
!BOE
!      
!  A Component can be created which will run only on a subset of the
!  current PET list.
!EOE
 
    print *, "State Reconcile Example 1: Component Creation"

!BOC
    ! Get the global VM for this job.
    call ESMF_VMGetGlobal(vm=vm, rc=rc)

    comp1name = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=comp1name, petList=(/ 0, 1 /), rc=rc)
    print *, "GridComp Create returned, name = ", trim(comp1name)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC

    comp2name = "Ocean"
    comp2 = ESMF_GridCompCreate(name=comp2name, petList=(/ 2, 3 /), rc=rc)
    print *, "GridComp Create returned, name = ", trim(comp2name)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC

    statename = "Ocn2Atm"
    state1 = ESMF_StateCreate(name=statename, rc=rc)  
!EOC
    print *, "State Create returned, name = ", trim(statename)

    print *, "State Example 1 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
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
    ! This is o.k. because the SetServices routine must execute from within
    ! the parent component VM.
    call ESMF_GridCompSetVM(comp1, comp_dummy, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC

    call ESMF_GridCompSetVM(comp2, comp_dummy, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC

    call ESMF_GridCompSetServices(comp1, userRoutine=comp_dummy, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC

    call ESMF_GridCompSetServices(comp2, userRoutine=comp_dummy, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC


    print *, "ready to set entry point 1"
    call ESMF_GridCompSetEntryPoint(comp1, ESMF_METHOD_INITIALIZE, &
         comp1_init, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC


    print *, "ready to set entry point 2"
    call ESMF_GridCompSetEntryPoint(comp2, ESMF_METHOD_INITIALIZE, &
         comp2_init, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC



    print *, "ready to call init for comp 1"
    call ESMF_GridCompInitialize(comp1, exportState=state1, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC

    print *, "ready to call init for comp 2"
    call ESMF_GridCompInitialize(comp2, exportState=state1, rc=rc)
!EOC

    print *, "State Example 2 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!   
! Now we have {\tt state1} containing {\tt field1} on PETs 0 and 1, and
! {\tt state1} containing {\tt field2} on PETs 2 and 3.  For the code
! to have a rational view of the data, we call {\tt ESMF\_StateReconcile}
! which determines which objects are missing from any PET, and communicates
! information about the object.  There is the option of turning metadata
! reconciliation on or off with the optional parameter shown in the call 
! below.  The default behavior is for metadata reconciliation to be off.
! After the call to reconcile, all
! {\tt ESMF\_State} objects now have a consistent view of the data.
!EOE

!BOC
    print *, "State before calling StateReconcile()"
    call ESMF_StatePrint(state1, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC


    call ESMF_StateReconcile(state1, vm=vm,  &
                             attreconflag=ESMF_ATTRECONCILE_OFF, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC


    print *, "State after calling StateReconcile()"
    call ESMF_StatePrint(state1, rc=rc)
!EOC

    print *, "State Example 3 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------

    call ESMF_StateDestroy (state1, rc=rc)
    if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridCompDestroy (comp2, rc=rc)
    if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridCompDestroy (comp1, rc=rc)
    if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE

20  continue
    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


    call ESMF_Finalize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_StateReconcileEx.F90"
    else
        print *, "FAIL: ESMF_StateReconcileEx.F90"
    end if

!BOC
end program ESMF_StateReconcileEx
!EOC
