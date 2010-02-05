! $Id: ESMF_MyRegistrationInFortran.F90,v 1.8.2.1 2010/02/05 20:04:30 svasquez Exp $
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


module myFortranIRFMod

  use ESMF_Mod

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
  use ESMF_Mod
  use myFortranIRFMod
  implicit none
  type(ESMF_GridComp) :: gcomp
  integer, intent(out) :: rc
  ! do something here
  print *, "I am in myRegistrationInFortran()"
  call ESMF_GridCompPrint(gcomp)
  
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, userRoutine=myInitInFortran, &
    rc=rc)
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, userRoutine=myRunInFortran, &
    rc=rc)
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, userRoutine=myFinalInFortran, &
    rc=rc)
  
  ! return successfully
  rc = ESMF_SUCCESS
end subroutine


subroutine my_CplRegistrationInFortran(cplcomp, rc)
  use ESMF_Mod
  use myFortranIRFMod
  implicit none
  type(ESMF_CplComp) :: cplcomp
  integer, intent(out) :: rc
  ! do something here
  print *, "I am in myRegistrationInFortran()"
  call ESMF_CplCompPrint(cplcomp)
  
  call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETINIT, userRoutine=myCplInitInFortran, &
    rc=rc)
  call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETRUN, userRoutine=myCplRunInFortran, &
    rc=rc)
  call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETFINAL, userRoutine=myCplFinalInFortran, &
    rc=rc)
  
  ! return successfully
  rc = ESMF_SUCCESS
end subroutine
