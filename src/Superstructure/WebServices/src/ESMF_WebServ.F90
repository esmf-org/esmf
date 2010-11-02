! $Id: ESMF_WebServ.F90,v 1.1 2010/11/02 18:36:04 ksaint Exp $
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
#define ESMC_FILENAME "ESMCI_WebServ.F90"
!==============================================================================
!
! ESMF Component module
module ESMF_WebServMod
!
!==============================================================================
! A blank line to keep protex happy because there are no public entry
! points in this file, only internal ones.
!BOP

!EOP
!
! This file contains the Component class definition and all Component
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES


contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_WebServProcessRequest()"
!BOPI
! !IROUTINE: ESMF_WebServProcessRequest 
!
! !INTERFACE:
  subroutine ESMF_WebServProcessRequest(comp, importState, exportState, &
                                        clock, phase, procType, rc)
    use ESMF_CompMod
    use ESMF_GridCompMod
    use ESMF_StateMod
    use ESMF_ClockMod
    use ESMF_UtilTypesMod
    use ESMF_VMMod

    implicit none

!
! !ARGUMENTS:
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: clock
    integer              :: phase
    character            :: procType
    integer, intent(out) :: rc

!
!
! !DESCRIPTION:
!   If this is the root process, send messages to all of the other processes
!   to run the specified routine.
!
! The arguments are:
! \begin{description}
! \item[{[comp]}]
!   {\tt ESMF\_GridComp} object that represents the Grid Component for which
!   the service is created.
! \item[{[impstate]}]
!   {\tt ESMF\_State} containing import data for coupling. 
! \item[{[expstate]}]
!   {\tt ESMF\_State} containing export data for coupling. 
! \item[{[clock]}]
!   External {\tt ESMF\_Clock} for passing in time information.
! \item[{[phase]}]
!   Indicates whether routines are {\em single-phase} or {\em multi-phase}.
! \item[{[procType]}]
!   Specifies which routine to run: 'I' for initialization, 'R' for run, and
!   'F' for finalization.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! Local variables
    integer        :: localrc
    type(ESMF_VM)  :: vm
    integer        :: localPet, petCount
    integer        :: thread_cntr
    character      :: outmsg(2)

    ! Initialize return code
    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS

    print *, "Processing request"

    call ESMF_GridCompGet(comp, vm=vm)
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount)

    if (localPet == 0) then

       outmsg(1) = procType

       ! Loop through the other, non-root processes, sending each of them 
       ! the message to process the request
       do thread_cntr = 1, petCount - 1, 1

          print *, "In do loop: ", thread_cntr
          print *, "Before MPI Send: ", procType
          call ESMF_VMSend(vm, sendData=outmsg, count=1, dst=thread_cntr, &
                           rc=localrc)

          ! Check return code to make sure send went out ok
          if (localrc /= ESMF_SUCCESS) then
              ! do something
          endif

       enddo

    endif

    rc = localrc

  end subroutine
!-------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_WebServWaitForRequest()"
!BOPI
! !IROUTINE: ESMF_WebServWaitForRequest 
!
! !INTERFACE:
  subroutine ESMF_WebServWaitForRequest(comp, exportState, rc)
    use ESMF_CompMod
    use ESMF_StateMod
    use ESMF_GridCompMod
    use ESMF_ClockMod
    use ESMF_UtilTypesMod
    use ESMF_VMMod
    
    implicit none

!
! !ARGUMENTS:
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: exportState
    integer, intent(out) :: rc
!
!
! !DESCRIPTION:
!   If this is not the root process, waits for a message from the root 
!   process and executes the appropriate routine when the message is received.
!
! The arguments are:
! \begin{description}
! \item[{[comp]}]
!   {\tt ESMF\_GridComp} object that represents the Grid Component for which
!   routine is run.
! \item[{[expstate]}]
!   {\tt ESMF\_State} containing export data for coupling. 
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    integer       :: localrc
    type(ESMF_VM) :: vm
    integer       :: localPet, petCount
    character     :: inmsg(2)
    integer       :: count

    ! Initialize return code
    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS

    ! Get the current PET info
    call ESMF_GridCompGet(comp, vm=vm)
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount)

    ! Loop forever... need to provide clean exit 
    do

       ! Wait for the request to be sent, and once it's received, process
       ! the request based on the incoming message
       print *, "Waiting for request: ", localPet
       inmsg(1) = 'A'

       call ESMF_VMRecv(vm, recvData=inmsg, count=count, src=0, &
                        blockingflag=ESMF_BLOCKING, rc=localrc)

       print *, "    Buffer value: ", inmsg(1), " - ", localPet
       print *, "Leaving MPI_Recv: ", localPet

       ! 'I' = init
       if (inmsg(1) == 'I') then

          print *, "Execute GridCompInitialize: ", localPet
          call ESMF_GridCompInitialize(comp, exportState=exportState, &
                                       rc=localrc)
          print *, "Done Execute GridCompInitialize: ", localPet

       ! 'R' = run
       else if (inmsg(1) == 'R') then

          print *, "Execute GridCompRun: ", localPet
          call ESMF_GridCompRun(comp, exportState=exportState, rc=localrc)
          print *, "Done Execute GridCompRun: ", localPet

       ! 'F' = final
       else if (inmsg(1) == 'F') then

          print *, "Execute GridCompFinalize: ", localPet
          call ESMF_GridCompFinalize(comp, exportState=exportState, rc=localrc)
          print *, "Done Execute GridCompFinalize: ", localPet

       endif

    end do

    rc = localrc

  end subroutine
!------------------------------------------------------------------------------


end module ESMF_WebServMod
