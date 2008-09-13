! $Id: ESMF_CompFortranAndCSTest.F90,v 1.7 2008/09/13 05:02:00 theurich Exp $
!
! System test CompFortranAndC
!  Description on Sourceforge under System Test #63029

!-------------------------------------------------------------------------
!ESMF_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!-------------------------------------------------------------------------
! !DESCRIPTION:
! System test CompFortranAndC.
! This system test checks that states are transfered accurately between
! components that are implemented in different languages (Fortran and C).
! Two components are created by the driver code and their SetServices()
! are invoked.
!  The rest of the code works on an array within a specific state that is 
!  on turns modified by one component followed by the other component 
!  verifying those changes. Specifically on,
!
!  "Init section":
!  --The Fortran Component adds an array to the export state and initializes 
!    its data.
!  --The C Component re-initializes the data values of the same state array.
!
!  "Run section":
!  --The Fortran Component first verifies the array values just initialized by the C
!    component, and then modifies it again before returning.
!  --The C component verifies the array values just modifed by the Fortran 
!    component and returns.
!
!  "Finalize section":
!  --The Fortran component cleans up the state contents (i.e. it Destroys the
!    Array object and deallocates the Fortran array it points to).
!
!\begin{verbatim}

    program CompFortranAndC
#define ESMF_METHOD "program CompFortranAndC"

#include "ESMF.h"
#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    use ESMF_CompMod
    
    use user_FortranComponent

    implicit none

    interface 
      subroutine my_RegistrationInC(gcomp, rc)
         use ESMF_Mod
         type(ESMF_GridComp) :: gcomp
         integer, intent(out) :: rc
      end subroutine my_RegistrationInC
    end interface
    
!   Local variables
    integer :: my_pet, rc, localrc
    type(ESMF_VM):: vm
    type(ESMF_GridComp) :: compInFortran
    type(ESMF_GridComp) :: compInC
    type(ESMF_State) :: imp, exp
    character(len=ESMF_MAXSTR) :: cname
        
   ! Variables related to the Clock
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    type(ESMF_Array) :: array
    integer :: localDeCount
    integer, allocatable :: localDeList(:)
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test CompFortranAndC:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get the default global VM
    call ESMF_VMGetGlobal(vm, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get our pet number for output print statements
    call ESMF_VMGet(vm, localPet=my_pet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cname = "Fortran Component"
    compInFortran = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    print *, "Comp Create (Fortran) finished, name = ", trim(cname)


    cname = "C Component"
    compInC = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    print *, "Comp Create (C) finished, name = ", trim(cname)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetServices(compInFortran, myRegistrationInFortran, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "CompInFortran Register finished, rc= ", rc

      call ESMF_GridCompSetServices(compInC, my_RegistrationInC, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "CompInC Register finished, rc= ", rc

!------------------------------------------------------------------------------
!  Create and initialize a Clock.
!------------------------------------------------------------------------------

      call ESMF_TimeIntervalSet(timeStep, s=2, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
      print *, "Time Interval set"

      call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=25, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
      print *, "Start Time set"

      call ESMF_TimeSet(stopTime, yy=2004, mm=9, dd=26, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
      print *, "Stop Time set"

      clock = ESMF_ClockCreate("Application Clock", timeStep, startTime, &
                                stopTime, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, terminationflag=ESMF_ABORT)
      print *, "Clock created"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      imp = ESMF_StateCreate("igrid import state", ESMF_STATE_IMPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      exp = ESMF_StateCreate("igrid export state", ESMF_STATE_EXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_GridCompInitialize(compInFortran, imp, exp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Initialize Fortran component finished"
 
      call ESMF_GridCompInitialize(compInC, imp, exp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp Initialize C component finished"

 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      call ESMF_GridCompRun(compInFortran, imp, exp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "CompInFortran Run returned"

      call ESMF_GridCompRun(compInC, imp, exp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "CompInC Run returned"
 

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(compInFortran, imp, exp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10


      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"
      print *, "Test finished, my_pet = ", my_pet
      print *, "-----------------------------------------------------------------"
      print *, "-----------------------------------------------------------------"

      print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

      call ESMF_GridCompDestroy(compInFortran, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompDestroy(compInC, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(imp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(exp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
 10   print *, "System Test CompFortranAndC complete"

      ! Standard ESMF Test output to log file
      write(failMsg, *) "System Test failure"
      write(testname, *) "System Test CompFortranAndC: Component Create Test"
  
      call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), &
        testname, failMsg, testresult, ESMF_SRCLINE)

      if ((my_pet .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS: Component Create complete."
        else
          write(finalMsg, *) "System Test did not succeed.  Error code ", rc
        endif
        write(0, *) ""
        write(0, *) trim(testname)
        write(0, *) trim(finalMsg)
        write(0, *) ""

      endif

      call ESMF_Finalize(rc=rc)

      end program CompFortranAndC
    
!\end{verbatim}
    
