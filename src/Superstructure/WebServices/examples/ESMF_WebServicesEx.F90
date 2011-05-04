! $Id: ESMF_WebServicesEx.F90,v 1.3 2011/05/04 14:38:34 ksaint Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
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

module ESMF_WebServUserModel

  ! ESMF Framework module
  use ESMF_Mod

  implicit none

  public ESMF_WebServUserModelRegister

  contains

  !-------------------------------------------------------------------------
  !  The Registration routine
  !  
  subroutine ESMF_WebServUserModelRegister(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                    userRoutine=user_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                    userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                    userRoutine=user_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Comp1 Register returning"

  end subroutine

  !-------------------------------------------------------------------------
  !  The Initialization routine
  !  
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init"

  end subroutine user_init

  !-------------------------------------------------------------------------
  !  The Run routine
  !  
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run"

  end subroutine user_run

  !-------------------------------------------------------------------------
  !  The Finalization routine
  !  
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Final"

  end subroutine user_final

end module ESMF_WebServUserModel



!-------------------------------------------------------------------------
!BOE
!\subsubsection{Making a Component available through WebServices}
!      
!  In this example a standard ESMF Component is made available through
!  the WebServices interface.
!EOE
!BOC
program WebServicesEx
  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_WebServMod
  use ESMF_WebServUserModel
!EOC

  implicit none

  ! Local variables
  type(ESMF_GridComp) :: comp1     !! Grid Component
  integer             :: rc        !! Return Code
  integer             :: finalrc   !! Final return code
  integer             :: portNum   !! The port number for the listening socket

  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(defaultlogfilename="WebServicesEx.Log", &
                    defaultlogtype=ESMF_LOG_MULTI, rc=rc)
  if (rc/=ESMF_SUCCESS) then
    finalrc = ESMF_FAILURE
    goto 10
  endif

!BOC
  ! create the grid component 
  comp1 = ESMF_GridCompCreate(name="My Component", rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) then
    finalrc = ESMF_FAILURE
    goto 10
  endif

!BOC
  ! Set up the register routine 
  call ESMF_GridCompSetServices(comp1, &
          userRoutine=ESMF_WebServUserModelRegister, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) then
    finalrc = ESMF_FAILURE
    goto 10
  endif

!BOC
  portNum = 27060

  ! Call the Web Services Loop and wait for requests to come in
  !call ESMF_WebServicesLoop(comp1, portNum, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) then
    finalrc = ESMF_FAILURE
    goto 10
  endif

10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_WebServicesEx.F90"
  else
    print *, "FAIL: ESMF_WebServicesEx.F90"
  endif

!BOC
end program WebServicesEx
!EOC
