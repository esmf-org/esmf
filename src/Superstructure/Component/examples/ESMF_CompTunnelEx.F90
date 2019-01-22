! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, GEOEhysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

#define FILENAME "src/Superstructure/Component/examples/ESMF_CompTunnelEx.F90"
#include "ESMF_Macros.inc"
#include "ESMF.h"

module ESMF_CompTunnelEx_mod

  ! modules
  use ESMF
  
  implicit none
  
  private
  
  public setservices, setservicesCPL

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

  recursive subroutine setservicesCPL(cplcomp, rc)
    ! arguments
    type(ESMF_CplComp):: cplcomp
    integer, intent(out):: rc
    
    ! Initialize
    rc = ESMF_SUCCESS

  end subroutine !--------------------------------------------------------------
  
end module

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

program ESMF_CompTunnelEx
  
  ! The ESMF Framework module
  use ESMF
  use ESMF_TestMod
  
  ! User supplied modules
  use ESMF_CompTunnelEx_mod, only: setservices, setservicesCPL
  
  implicit none
  
  ! Local variables
  integer                 :: rc, userRc
  type(ESMF_VM)           :: vm
  integer                 :: localPet, petCount
  integer                 :: petList(3)
  type(ESMF_GridComp)     :: actualComp, dualComp
  type(ESMF_CplComp)      :: actualCplComp
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

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
#ifdef ESMF_TESTCOMPTUNNEL
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Creating an {\em actual} Component}
! 
! \label{sec:CompTunnelActualCreate}
!
! The creation process of an {\em actual} Gridded Component, which will become
! one of the two end points of a Component Tunnel, is identical to the creation
! of a regular Gridded Component. On the actual side, an actual Component is 
! very similar to a regular Component. Here the actual Component is created
! with a custom {\tt petList}.
!EOE
!BOC
  petList = (/0,1,2/)
  actualComp = ESMF_GridCompCreate(petList=petList, name="actual", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  petList = (/3,4,5/)
  actualCplComp = ESMF_CplCompCreate(petList=petList, name="actCplComp", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Creating a {\em dual} Component}
! 
! \label{sec:CompTunnelDualCreate}
!
! The same way an actual Component appears as a regular Component in
! the context of the actual side application, a {\em dual} Component
! is created as a regular Component on the dual side.
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
! Tunnel the {\tt ServiceLoop()} method is called. Here the socket-based
! implementation is chosen.
!EOE
!BOC
  call ESMF_GridCompServiceLoop(actualComp, port=61010, timeout=20, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This call opens the actual side of the Component Tunnel in form of a
! socket-based server, listening on {\tt port} 61010. The {\tt timeout} argument
! specifies how long the actual side will wait for the dual side
! to connect, before the actual side returns with a time out condition. The
! time out is set to 20 seconds.
!
! At this point, before a dual Component connects to the other side of the 
! Component Tunnel, it is
! possible to manually connect to the waiting actual Component. This can be
! useful when debugging connection issues. A convenient tool for this is the 
! standard {\tt telnet} application. Below is a transcript of such a connection.
! The manually typed commands are separate from the previous responses by a 
! blank line.
!
! \begin{verbatim}
! $ telnet localhost 61010
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
! {\tt ServiceLoop()} on the actual side will return with an error condition. 
! The clean way to
! disconnect the {\tt telnet} session, and to have the {\tt ServiceLoop()}
! wait for a new connection, e.g. from a dual Component, is to send the
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
! of 20 seconds, as specified during the ServiceLoop() call.
!
! \begin{sloppypar}
! Before moving on to the dual side of the GridComp based Component Tunnel 
! example, it should be pointed out that the exact same procedure is used to
! set up the actual side of a {\em CplComp} based Component Tunnel. Assuming
! that {\tt actualCplComp} is a CplComp object for which SetServices has already
! been called, the actual side uses {\tt ESMF\_CplCompServiceLoop()} to start
! listening for connections from the dual side.
! \end{sloppypar}
!EOE

  call ESMF_CplCompSetServices(actualCplComp, userRoutine=setservicesCPL, &
    userRc=userRc, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (userRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_CplCompServiceLoop(actualCplComp, port=61011, timeout=2, &
    timeoutFlag=timeoutFlag, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Here the {\tt timeoutFlag} is specified in order to prevent the expected
! time-out condition to be indicated through the return code. Instead, when
! {\tt timeoutFlag} is present, the return code is still {\tt ESMF\_SUCCESS}, 
! but {\tt timeoutFlag} is set to {\tt .true.} when a time-out occurs.
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
  call ESMF_GridCompSetServices(dualComp, port=61010, server="localhost", &
    timeout=10, timeoutFlag=timeoutFlag, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The {\tt port} and {\tt server} arguments are used to connect to the desired
! actual Component. The time out of 10 seconds ensures that if the actual
! Component is not available, a time out condition is returned instead of
! resulting in a hang. The {\tt timeoutFlag} argument further absorbs the time
! out condition, either returning as {\tt .true.} or {\tt .false.}. In this mode
! the standard {\tt rc} will indicate success even when a time out condition
! was reached.
!EOE

!-----------------------------------------------------------------------------
!BOE
!\subsubsection{Invoking standard Component methods through a Component Tunnel}
! 
! \label{sec:CompTunnelInvoking}
!
! Once a Component Tunnel is established, the actual Component is fully under
! the control of the dual Component. A standard Component method invoked on the
! dual Component is not executed by the dual Component itself, but by the 
! actual Component instead. In fact, it is the entry points registered with
! the actual Component that are executed when standard methods are invoked on
! the dual Component. The connected {\tt dualComp} object serves as a portal
! through which the connected {\tt actualComp} becomes accessible on the dual
! side.
!
! Typically the first standard method called is the {\tt CompInitialize()}
! routine.
!EOE
!BOC
  call ESMF_GridCompInitialize(dualComp, timeout=10, timeoutFlag=timeoutFlag, &
    userRc=userRc, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (userRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Again, the {\tt timeout} argument serves to prevent the dual side from 
! hanging if the actual Component application has experienced a catastrophic
! condition and is no longer available, or takes longer than expected. The
! presence of the {\tt timeoutFlag} allows time out conditions to be caught
! gracefully, so the dual side can deal with it in an orderly fashion, instead
! of triggering an application abort due to an error condition.
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
!\subsubsection{The non-blocking option to invoke standard Component methods through a Component Tunnel}
! 
! \label{sec:CompTunnelInvokingNonblocking}
!
! Standard Component methods called on a connected dual Component are executed
! on the actual side, across the PETs of the actual Component. By default the
! dual Component PETs are blocked until the actual Component has finished
! executing the invoked Component method, or until a time out condition has been
! reached. In many practical applications a more loose synchronization between
! dual and actual Components is useful. Having the PETs of a dual
! Component return immediately from a standard Component method allows multiple
! dual Component, on the same PETs, to control multiple actual Components. 
! If the actual Components are executing in separate executables, or the same 
! executable but on exclusive sets of PETs, they can execute concurrently, even
! with the controlling dual Components all running on the same PETs.
! The non-blocking dual side regains control over the actual Component by 
! synchronizing through the CompWait() call.
!
! Any of the standard Component methods can be called in non-blocking mode
! by setting the optional {\tt syncflag} argument to 
! {\tt ESMF\_SYNC\_NONBLOCKING}. 
!EOE
!BOC
  call ESMF_GridCompInitialize(dualComp, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!
! {\em If} communication between the dual and the actual Component was successful, 
! this call will return immediately on all of the dual Component PETs, while the
! actual Component continues to execute the invoked Component method.
! However, if the dual Component has difficulties reaching the actual Component,
! the call will block on all dual PETs until successful contact was made, or the
! default time out (3600 seconds, i.e. 1 hour) has been reached. In most cases a 
! shorter time out condition is desired with the non-blocking option, as shown
! below.
!
! First the dual Component must wait for the outstanding method.
!EOE
!BOC
  call ESMF_GridCompWait(dualComp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!
! Now the same non-blocking CompInitialize() call is issued again, but this time
! with an explicit 10 second time out.
!EOE
!BOC
  call ESMF_GridCompInitialize(dualComp, syncflag=ESMF_SYNC_NONBLOCKING, &
    timeout=10, timeoutFlag=timeoutFlag, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!
! This call is guaranteed to return within 10 seconds, or less, on the dual Component
! PETs, either without time out condition, indicating that the actual Component
! has been contacted successfully, or with time out condition, indicating that
! the actual Component was unreachable at the time. Either way, the dual 
! Component PETs are back under user control quickly.
!
! Calling the CompWait() method on the dual Component causes the dual Component
! PETs to block until the actual Component method has returned, or a time out
! condition has been reached.
!EOE
!BOC
  call ESMF_GridCompWait(dualComp, userRc=userRc, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (userRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!
! The default time out for CompWait() is 3600 seconds, i.e. 1 hour, just like
! for the other Component methods. However, the semantics of a time out 
! condition under CompWait() is different from the other Component methods. Typically the {\tt timeout} is simply the
! maximum time that any communication between dual and actual Component is allowed 
! to take before a time out condition is raised. For CompWait(), the {\tt timeout}
! is the maximum time that an actual Component is allowed to execute before
! reporting back to the dual Component. Here, even with the default time out, 
! the dual Component would return from CompWait() immediately with a time out
! condition if the actual Component has already been executing for over 1 hour, 
! and is not already waiting to report back when the dual Component calls 
! CompWait(). On the other hand, if it has only been 30 minutes since 
! CompInitialize() was called on the dual Component, then the actual Component
! still has 30 minutes before CompWait() returns with a time out condition.
! During this time (or until the actual Component returns) the dual Component
! PETs are blocked.
!
! A standard Component method is invoked in non-blocking mode.
!EOE
!BOC
  call ESMF_GridCompRun(dualComp, syncflag=ESMF_SYNC_NONBLOCKING, &
    timeout=10, timeoutFlag=timeoutFlag, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!
! Once the user code on the dual side is ready to regain control over the
! actual Component it calls CompWait() on the dual Component. Here a
! {\tt timeout} of 60s is specified, meaning that the total execution time the
! actual Component spends in the registered Run() routine may not exceed 60s
! before CompWait() returns with a time out condition.
!EOE
!BOC
  call ESMF_GridCompWait(dualComp, timeout=60, userRc=userRc, rc=rc)
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
! destroyed in the same way a regular Component is, by calling the destroy
! method.
!EOE
!BOC
  call ESMF_GridCompDestroy(actualComp, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
#endif
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
