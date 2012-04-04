! $Id: ESMF_CompTunnelEx.F90,v 1.1 2012/04/04 15:55:50 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, GEOEhysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

#define FILENAME "src/Superstructure/Component/examples/ESMF_CompTunnelEx.F90"
#include "ESMF_Macros.inc"
#include "ESMF.h"

module ESMF_CompTunnelEx_mod

  ! modules
  use ESMF
  
  implicit none
  
  private
  
  public setservices

  contains !--------------------------------------------------------------------

  recursive subroutine setservices(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=initialize, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=finalize, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------
  
  recursive subroutine initialize(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS
        
    call ESMF_LogWrite("Actual Component Initialize", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine !--------------------------------------------------------------

  recursive subroutine run(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("Actual Component Run", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine !--------------------------------------------------------------

  recursive subroutine finalize(gcomp, istate, estate, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("Actual Component Finalize", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
          
  end subroutine !--------------------------------------------------------------

end module


!-------------------------------------------------------------------------
! Note - the program below is here only to make this an executable.
!-------------------------------------------------------------------------


program ESMF_CompTunnelEx
  
  ! The ESMF Framework module
  use ESMF
  use ESMF_TestMod
  
  ! User supplied modules
  use ESMF_CompTunnelEx_mod, only: setservices
  
  implicit none
  
  ! Local variables
  integer                 :: rc, userRc
  type(ESMF_VM)           :: vm
  integer                 :: localPet, petCount
  integer                 :: petList(3)
  type(ESMF_GridComp)     :: actualComp, dualComp
  logical                 :: timeoutFlag
  
  character(ESMF_MAXSTR)  :: testname
  character(ESMF_MAXSTR)  :: failMsg

  integer                 :: finalrc, result

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
  write(failMsg, *)   "Example failure"
  write(testname, *)  "Example ESMF_CompTunnelEx"
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
!-------------------------------------------------------------------------
  ! Initialize the Framework and get the default VM
  call ESMF_Initialize(vm=vm, defaultlogfilename="CompTunnelEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 6) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
!-------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Creating an {\em actual} Component}
! 
! \label{sec:CompTunnelActualCreate}
!
! The creation process of an {\em actual} Gridded Component, which will become
! one of the two end points of a Component Tunnel, uses the exact same
! method as is used for the creation of a regular Gridded Components. In fact,
! in the application context of the actual side, an actual Component is really 
! no different from a regular Component. Here the actual Component is created
! with a custom petList.
!EOE
!BOC
  petList = (/0,1,2/)
  actualComp = ESMF_GridCompCreate(petList=petList, name="actual", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Creating a {\em dual} Component}
! 
! \label{sec:CompTunnelDualCreate}
!
! The same way that an actual Component is really just a regular Component in
! the context of the actual side application, so is a {\em dual} Component
! simply a regular Component in the application context on the dual side. 
! A dual Gridded Component with custom {\tt petList} is created using the
! regular create call.
!EOE
!BOC
  petList = (/4,3,5/)
  dualComp = ESMF_GridCompCreate(petList=petList, name="dual", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Setting up the {\em actual} side of a Component Tunnel}
! 
! \label{sec:CompTunnelActualSide}
!
! After creation, the regular procedure for registering the standard Component
! methods is followed for the actual Gridded Component.
!EOE
!BOC
  call ESMF_GridCompSetServices(actualComp, userRoutine=setservices, &
    userRc=userRc, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (userRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! So far the {\tt actualComp} object is no different from a regular Gridded
! Component. In order to turn it into the {\em actual} end point of a Component
! Tunnel the {\tt ServiceLoop()} method is called.
!EOE
!BOC
  call ESMF_GridCompServiceLoop(actualComp, port=60000, timeout=20, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This call opens the actual side of the Component Tunnel, in form of a
! socket-based server, listening on {\tt port} 60000. The {\tt timeout} argument
! specifies how long the actual side will wait for the dual side
! to communicate, before it returns with a time out condition. Here the time out
! is set to 20 seconds.
!
! At this point, before a dual Component connects to the Component Tunnel, it is
! possible to manually connect to the waiting actual Component. This can be
! useful when debugging connection issues. A convenient tool for this is the 
! standard {\tt telnet} application. Below is a transcript of such a connection.
! The manually typed commands are separate from the previous responses by a 
! blank line.
!
! \begin{verbatim}
! $ telnet localhost 60000
! Trying 127.0.0.1...
! Connected to localhost.
! Escape character is '^]'.
! Hello from ESMF Actual Component server!
!
! date
! Tue Apr  3 21:53:03 2012
! 
! version
! ESMF_VERSION_STRING: 5.3.0
! \end{verbatim}
!
! If at any point the {\tt telnet} session is manually shut down, the 
! {\tt ServiceLoop()} will return with an error condition. The clean way to
! disconnect the {\tt telnet} session, and to have the {\tt ServiceLoop()}
! wait for a new connection, e.g. from a dual Component, is to give the
! {\tt reconnect} command. This will automatically shut down the {\tt telnet}
! connection.
!
! \begin{verbatim}
! reconnect
! Actual Component server will reconnect now!
! Connection closed by foreign host.
! $
! \end{verbatim}
!
! At this point the actual Component is back in listening mode, with a time out
! of 20 seconds.
!EOE

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Setting up the {\em dual} side of a Component Tunnel}
! 
! \label{sec:CompTunnelDualSide}
!
! On the dual side, the {\tt dualComp} object needs to be connected to the
! actual Component in order to complete the Component Tunnel. Instead of
! registering standard Component methods locally, a special variant of the
! {\tt SetServices()} call is used to connect to the actual Component.
!EOE
!BOC
  call ESMF_GridCompSetServices(dualComp, port=60000, server="localhost", &
    timeout=10, timeoutFlag=timeoutFlag, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt port} and {\tt server} arguments are used to connect to the desired
! actual Component. The time out of 10 seconds ensures that if the actual
! Component is not available, a time out condition is returned instead of
! resulting in a hang. The {\tt timeoutFlag} argument further absorbs the time
! out condition into a {\tt logical}. In this mode the standard return code
! will indicate success even when a time out was reached.
!EOE

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Invoking standard Component methods through a Component Tunnel}
! 
! \label{sec:CompTunnelInvoking}
!
! While the actual Component is inside the {\tt ServiceLoop()}, the connected
! dual side can invoke any of the standard Component methods that the actual
! side had registered. The connected {\tt dualComp} object serves as a portal
! through which the connected {\tt actualComp} is accessible. Typically the
! first method called is the standard {\tt CompInitialize()}.
!EOE
!BOC
  call ESMF_GridCompInitialize(dualComp, timeout=10, timeoutFlag=timeoutFlag, &
    userRc=userRc, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (userRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Again, the {\tt timeout} argument serves to prevent
! the dual side from hanging if the actual Component application has experienced
! a catastrophic condition and is no longer available. The presence of the
! {\tt timeoutFlag} allows time out conditions to be caught gracefully so the
! dual side can deal with it in an orderly fashion.
!
! The {\tt CompRun()} and {\tt CompFinalize()} methods follow the same format.
!
!BOC
  call ESMF_GridCompRun(dualComp, timeout=10, timeoutFlag=timeoutFlag, &
    userRc=userRc, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (userRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_GridCompFinalize(dualComp, timeout=10, timeoutFlag=timeoutFlag, &
    userRc=userRc, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (userRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Destroying a connected {\em dual} Component}
! 
! \label{sec:CompTunnelDualDestroy}
!
! A dual Component that is connected to an actual Component through a Component
! Tunnel is destroyed the same way a regular Component is. The only
! difference is that a connected dual Component may specify a {\tt timeout}
! argument to the {\tt CompDestroy()} call.
!EOE
!BOC
  call ESMF_GridCompDestroy(dualComp, timeout=10, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt timeout} argument again ensures that the dual side does not hang
! indefinitely in case the actual Component has become unavailable. If the
! actual Component is available, the destroy call will indicate to the actual
! Component that it should break out of the {\tt ServiceLoop()}. Either way,
! the local dual Component is destroyed.
!EOE

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Destroying a connected {\em actual} Component}
! 
! \label{sec:CompTunnelActualDestroy}
!
! An actual Component that is in a {\tt ServiceLoop()} must first return from 
! that call before it can be destroyed. This can either happen when a connected
! dual Component calls its {\tt CompDestroy()} method, or if the
! {\tt ServiceLoop()} reaches the specified time out condition. Either way,
! once control has been returned to the user code, the actual Component is 
! destroyed in the same way a regular Component is.
!EOE
!BOC
  call ESMF_GridCompDestroy(actualComp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10 continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_CompTunnelEx.F90"
  else
    print *, "FAIL: ESMF_CompTunnelEx.F90"
  endif

  call ESMF_Finalize(rc=rc)

end program ESMF_CompTunnelEx
