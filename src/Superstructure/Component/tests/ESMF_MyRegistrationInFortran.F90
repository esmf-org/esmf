! $Id: ESMF_MyRegistrationInFortran.F90,v 1.2 2008/08/26 23:47:52 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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
    integer               :: rc
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
    integer               :: rc
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
    integer               :: rc
    ! do something here
    print *, "I am in myFinalInFortran()"
    call ESMF_StatePrint(importState);
    call ESMF_ClockPrint(clock);
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine

end module


subroutine myRegistrationInFortran(gcomp, rc)
  use ESMF_Mod
  use myFortranIRFMod
  implicit none
  type(ESMF_GridComp) :: gcomp
  integer, intent(out) :: rc
  ! do something here
  print *, "I am in myRegistrationInFortran()"
  call ESMF_GridCompPrint(gcomp)
  
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, myInitInFortran, &
    ESMF_SINGLEPHASE, rc)
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, myRunInFortran, &
    ESMF_SINGLEPHASE, rc)
  call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, myFinalInFortran, &
    ESMF_SINGLEPHASE, rc)
  
  ! return successfully
  rc = ESMF_SUCCESS
end subroutine
