! $Id: ESMF_WebServicesEx.F90,v 1.8 2011/06/24 15:04:31 rokuingh Exp $
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
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Making a Component available through WebServices}
!      
!  In this example a standard ESMF Component is made available through
!  the WebServices interface.
!EOE
!BOE
!  The first step is to make sure your callback routines for initialize, run
!  and finalize are setup.  This is done by creating a register routine that
!  sets the entry points for each of these callbacks.  In this example, we've
!  packaged it all up into a separate module.  
!EOE
!BOC
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
!EOC



!-------------------------------------------------------------------------
!BOE
!  The actual driver code then becomes very simple; ESMF is initialized,
!  the component is created, the callback functions for the component are
!  registered, and the Web Service loop is started.
!EOE
!BOC
program WebServicesEx
  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_WebServMod
  use ESMF_WebServUserModel

  implicit none

  ! Local variables
  type(ESMF_GridComp) :: comp1     !! Grid Component
  integer             :: rc        !! Return Code
  integer             :: finalrc   !! Final return code
  integer             :: portNum   !! The port number for the listening socket
!EOC
!BOE
!  The port number specifies the id of the port on the local machine on which
!  a listening socket will be created.  This socket is used by the service to
!  wait for and receive requests from the client.  Check with your system
!  administrator to determine an appropriate port to use for your service.
!EOE

!BOC
  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(defaultlogfilename="WebServicesEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC
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
  !!!!!!!
  !! KDS: I've commented out the call to ESMF_WebServicesLoop so that it won't
  !! enter the infinite loop and hold up the examples run.  I'll keep it
  !! commented out until I create an example client that will send an EXIT
  !! to the loop.
  !!!!!!!
!BOC
  portNum = 27060

  ! Call the Web Services Loop and wait for requests to come in
  !call ESMF_WebServicesLoop(comp1, portNum, rc=rc)
!EOC
  if (rc/=ESMF_SUCCESS) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
!BOE
!  The call to ESMF\_WebServicesLoop will setup the listening socket for your
!  service and will wait for requests from a client.  As requests are received,
!  the Web Services software will process the requests and then return to the
!  loop to continue to wait.
!EOE
!BOE
!  The 3 main requests processed are INIT, RUN, and FINAL.  These requests 
!  will then call the appropriate callback routine as specified in your 
!  register routine (as specified in the ESMF\_GridCompSetServices call).
!  In this example, when the INIT request is received, the user\_init routine
!  found in the ESMF\_WebServUserModel module is called.
!EOE
!BOE
!  One other request is also processed by the Component Service, and that is
!  the EXIT request.  When this request is received, the Web Services loop
!  is terminated and the remainder of the code after the ESMF\_WebServicesLoop
!  call is executed.
!EOE


10 continue
!BOC
  call ESMF_Finalize(rc=rc)
!EOC
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_WebServicesEx.F90"
  else
    print *, "FAIL: ESMF_WebServicesEx.F90"
  endif

!BOC
end program WebServicesEx
!EOC
