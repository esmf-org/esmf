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


module myFortranIRFMod

  use ESMF

contains
  subroutine myInitInFortran(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! do something here
    print *, "I am in myInitInFortran()"
    call ESMF_StatePrint(importState);
    call ESMF_ClockPrint(clock);
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine

  subroutine myRunInFortran(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! do something here
    print *, "I am in myRunInFortran()"
    call ESMF_StatePrint(importState);
    call ESMF_ClockPrint(clock);
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine

  subroutine myFinalInFortran(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! do something here
    print *, "I am in myFinalInFortran()"
    call ESMF_StatePrint(importState);
    call ESMF_ClockPrint(clock);
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine


  subroutine myCplInitInFortran(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: cplcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! do something here
    print *, "I am in myInitInFortran()"
    call ESMF_StatePrint(importState);
    call ESMF_ClockPrint(clock);
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine

  subroutine myCplRunInFortran(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: cplcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! do something here
    print *, "I am in myRunInFortran()"
    call ESMF_StatePrint(importState);
    call ESMF_ClockPrint(clock);
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine

  subroutine myCplFinalInFortran(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: cplcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! do something here
    print *, "I am in myFinalInFortran()"
    call ESMF_StatePrint(importState);
    call ESMF_ClockPrint(clock);
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine

end module


subroutine my_RegistrationInFortran(gcomp, rc)
  use ESMF
  use myFortranIRFMod
  implicit none
  type(ESMF_GridComp) :: gcomp
  integer, intent(out) :: rc
  ! do something here
  print *, "I am in myRegistrationInFortran()"
  call ESMF_GridCompPrint(gcomp)
  
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, userRoutine=myInitInFortran, &
    rc=rc)
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, userRoutine=myRunInFortran, &
    rc=rc)
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, userRoutine=myFinalInFortran, &
    rc=rc)
  
  ! return successfully
  rc = ESMF_SUCCESS
end subroutine


subroutine my_CplRegistrationInFortran(cplcomp, rc)
  use ESMF
  use myFortranIRFMod
  implicit none
  type(ESMF_CplComp) :: cplcomp
  integer, intent(out) :: rc
  ! do something here
  print *, "I am in myRegistrationInFortran()"
  call ESMF_CplCompPrint(cplcomp)
  
  call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, userRoutine=myCplInitInFortran, &
    rc=rc)
  call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN, userRoutine=myCplRunInFortran, &
    rc=rc)
  call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, userRoutine=myCplFinalInFortran, &
    rc=rc)
  
  ! return successfully
  rc = ESMF_SUCCESS
end subroutine
