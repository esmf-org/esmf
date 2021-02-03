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

!BOE
! \subsubsection{Tracing a simple ESMF application} \label{ex:TraceEx}
!
! This example illustrates how to trace a simple ESMF
! application and print the event stream using Babeltrace.
! The first part of the code is a module representing
! a trivial ESMF Gridded Component.  The second part is a
! main program that creates and executes the component.
!EOE

!BOC
module SimpleComp

  use ESMF
  implicit none

  private
  public SetServices

contains

  subroutine SetServices(gcomp, rc)
      type(ESMF_GridComp)   :: gcomp
      integer, intent(out)  :: rc  

      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
           userRoutine=Init, rc=rc)
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
           userRoutine=Run, rc=rc)
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
           userRoutine=Finalize, rc=rc)
      
      rc = ESMF_SUCCESS
      
    end subroutine SetServices

    subroutine Init(gcomp, istate, estate, clock, rc)
      type(ESMF_GridComp):: gcomp
      type(ESMF_State):: istate, estate
      type(ESMF_Clock):: clock
      integer, intent(out):: rc
      
      print *, "Inside Init"
      
    end subroutine Init

    subroutine Run(gcomp, istate, estate, clock, rc)
      type(ESMF_GridComp):: gcomp
      type(ESMF_State):: istate, estate
      type(ESMF_Clock):: clock
      integer, intent(out):: rc
      
      print *, "Inside Run"
      
    end subroutine Run

    subroutine Finalize(gcomp, istate, estate, clock, rc)
      type(ESMF_GridComp):: gcomp
      type(ESMF_State):: istate, estate
      type(ESMF_Clock):: clock
      integer, intent(out):: rc
      
    print *, "Inside Finalize"
    
  end subroutine Finalize 

end module SimpleComp
!EOC

!BOC
program ESMF_TraceEx
!EOC
  
!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"


!  !PROGRAM: ESMF\_Trace - Examples of Trace usage.
!
!  !DESCRIPTION: 
!
! This program shows examples of Trace usage


!BOC
      ! Use ESMF framework module
      use ESMF
      use SimpleComp, only: SetServices
!EOC
      use ESMF_TestMod
!BOC
      implicit none

      ! Local variables  
      integer :: rc, finalrc, i
      type(ESMF_GridComp)     :: gridcomp
!EOC
      integer                 :: petCount, localPet, &
                                 itemCount, count, result
      type(ESMF_VM)           :: vm

      character(ESMF_MAXSTR)  :: name
      character(ESMF_MAXSTR)  :: testname
      character(ESMF_MAXSTR)  :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_TraceEx"

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!BOC     
      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, defaultlogfilename="TraceEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)        
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_Trace Example"
        print *, "--------------------------------------- "
      endif

!BOC
      ! create the component and then execute
      ! initialize, run, and finalize routines
      gridcomp = ESMF_GridCompCreate(name="test", rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
      call ESMF_GridCompSetServices(gridcomp, userRoutine=SetServices, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
      call ESMF_GridCompInitialize(gridcomp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
      do i=1, 5
         call ESMF_GridCompRun(gridcomp, rc=rc)
      enddo
!EOC
      if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
      call ESMF_GridCompFinalize(gridcomp, rc=rc)
!EOC     
      if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
      call ESMF_GridCompDestroy(gridcomp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) finalrc = ESMF_FAILURE
      
      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
      ! file that the scripts grep for.
      call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)    

!BOC      
      call ESMF_Finalize(rc=rc)
!EOC
      
      if (localPet==0) then
         print *, "--------------------------------------- "
         print *, "End of ESMF_Trace Example"
         print *, "--------------------------------------- "
      endif

      if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
      if (finalrc==ESMF_SUCCESS) then
         print *, "PASS: ESMF_TraceEx.F90"
      else
         print *, "FAIL: ESMF_TraceEx.F90"
      endif
!BOC
end program ESMF_TraceEx
!EOC

!BOE
! Assuming the code above is executed on four PETs with
! the environment variable {\tt ESMF\_RUNTIME\_TRACE} set to
! {\tt ON}, then a folder will be created in the run directory
! called {\em traceout} containing a {\em metadata} file and
! four event stream files named {\em esmf\_stream\_XXXX}
! where {\em XXXX} is the PET number.  If Babeltrace is
! available on the system, the list of events can be printed
! by executing the following from the run directory:
! \begin{verbatim}
! $ babeltrace ./traceout
! \end{verbatim}
! For details about iterating over trace events and performing
! analyses on CTF traces, see the corresponding documentation
! in the tools listed in Section \ref{sec:Tracing}.
!EOE
