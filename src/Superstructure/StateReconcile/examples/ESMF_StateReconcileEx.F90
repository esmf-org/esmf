! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
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
!\subsubsection{Reconcile a State}
!
! An {\tt ESMF\_State} object must be reconciled if it contains objects that
! were created and added to the State on a subset of the PETs from which the
! objects are now accessed and operated on. A typical case of this is when
! a State object is passed to a component that runs on a subset of PETs, and
! that component creates objects that are added to the State. After the
! component passes control back to the larger calling context (a parent
! component or the main program), the State object is not consistent across
! PETs. The {\tt ESMF\_StateReconcile()} method is used to reconcilce the State
! across all PETs.
!
! In order to demonstrate State reconciliation we need to set up at least
! one component that can be run on a subset of PETs. To this end an external
! routine is created that adds a few emtpy Fields into its {\tt exportState}.
!
!EOE
!BOC
  subroutine init(gcomp, importState, exportState, clock, rc)
    ! Abide to ESMF-prescribed Fortran interface
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Field) :: field1, field2, field3

    rc = ESMF_SUCCESS   ! indicate success... unless error is found

    field1 = ESMF_FieldEmptyCreate(name="Field1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    field2 = ESMF_FieldEmptyCreate(name="Field2", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    field3 = ESMF_FieldEmptyCreate(name="Field3", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    call ESMF_StateAdd(exportState, [field1, field2, field3], rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

  end subroutine init
!EOC

!BOE
! The standard way to register ESMF component routines is in the
! {\tt SetServices} routine.
!EOE
!BOC
  subroutine SetServices(gcomp, rc)
   ! Abide to ESMF-prescribed Fortran interface
   type(ESMF_GridComp)  :: gcomp
   integer, intent(out) :: rc

    rc = ESMF_SUCCESS   ! indicate success... unless error is found

    ! register 'init' as component initialization method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

  end subroutine SetServices
!EOC

end module ESMF_StateReconcileEx_Mod

!BOE
! A component can now be created in the main program that uses these routines.
!EOE
!BOC
  program ESMF_StateReconcileEx
!EOC

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
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
    integer             :: rc, petCount
    type(ESMF_State)    :: state
!BOC
!   ... other local variables ...
    type(ESMF_GridComp) :: comp
!EOC
    type(ESMF_VM)       :: vm

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
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! verify that this example can run on the given petCount
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (petCount<4) then
      print *, "This test must run on at least 4 PETs."
      finalrc = ESMF_FAILURE
      goto 20
    endif

!BOC
    comp = ESMF_GridCompCreate(name="MyComp", petList=[0,1], rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Here {\tt comp} is created to execute on two PETs: 0 and 1.
!
! Next the Component {\tt SetServices} method is called to register the
! custom component method(s).
!EOE
!BOC
    call ESMF_GridCompSetServices(comp, userRoutine=SetServices, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now a State is created that can be passed in when the registered Component
! method is called.
!EOE

!BOC
    state = ESMF_StateCreate(rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The {\tt state} object is used as the Component's {\tt exportState}.
!EOE
!BOC
    call ESMF_GridCompInitialize(comp, exportState=state, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Once control of execution has returned from the pevious
! {\tt ESMF\_GridCompInitialize()} call, the {\tt state} object is in an
! inconsistent state across the PETs of the current (main program) context.
! This is because Fields were added on PETs 0 and 1, but not on the remaining
! PETs (2 and 3). This situation can easliy be observed by writing the current
! {\tt state} to the ESMF log.
!EOE
!BOC
    call ESMF_StateLog(state, prefix="Before Reconcile:", rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! To reconcile {\tt state} across all of the PETs, use the
! {\tt ESMF\_StateReconcile()} method.
!EOE
!BOC
    call ESMF_StateReconcile(state, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The output of {\tt state} to the ESMF log shows that the object is now
! consistent across all PETs. I.e. {\tt state} contains identical items on
! all of the PETs.
!EOE
!BOC
    call ESMF_StateLog(state, prefix="After Reconcile:", rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------

    call ESMF_StateDestroy (state, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompDestroy (comp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
