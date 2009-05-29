! $Id: user_coupler.F90,v 1.11 2009/05/29 19:24:42 theurich Exp $
!
! System test of Exclusive components, user-written Coupler component.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Coupler
!
!
!\begin{verbatim}
#include "ESMF.h"

    module user_coupler

    ! ESMF Framework module
    use ESMF_TestMod
    use ESMF_Mod
    
    implicit none
    
    public usercpl_setvm, usercpl_register
        
    ! global data
    type(ESMF_RouteHandle), save :: redistRH12

    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
#undef ESMF_METHOD
#define ESMF_METHOD "usercpl_setvm"
  subroutine usercpl_setvm(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for
    ! your own code development you probably don't want to include the
    ! following call unless you are interested in exploring ESMF's
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

#undef ESMF_METHOD
#define ESMF_METHOD "usercpl_register"
    subroutine usercpl_register(comp, rc)
        type(ESMF_CplComp) :: comp
        integer, intent(out) :: rc
  
        integer :: status = ESMF_SUCCESS
  
        print *, "in user coupler register routine"
  
        ! Register the callback routines.
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
          rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
          rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
          rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        ! set return code
        rc = status


    end subroutine

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
 
    
#undef ESMF_METHOD
#define ESMF_METHOD "usercpl_init"
    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer,intent(out) :: rc
  
        ! Local variables
        integer :: status = ESMF_SUCCESS
        integer :: itemcount
        type(ESMF_Array) :: sorted_data1, sorted_data2
        type(ESMF_VM) :: vm
        integer :: pet_id
  
        print *, "User Coupler Init starting"
        status = ESMF_FAILURE
  
        ! Get VM from coupler component
        call ESMF_CplCompGet(comp, vm=vm, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPET=pet_id, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
  
        ! Since the components we are coupling between are running concurrently,
        ! they have each separately created ESMF objects.   We are planning to
        ! use a communications call (Redist) here, so first we must make a new
        ! call to reconcile the object lists in all the import and export states.
  
        ! New routine:
        ! Must be called on each state which is going to be accessed from
        ! this coupler.  When the call returns all objects which were not
        ! in existence on all PETs now have an object which represents them.
        call ESMF_StateReconcile(importState, vm, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        !call ESMF_StatePrint(importState, rc=status)
        !if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        !    ESMF_CONTEXT, rc)) return
  
        call ESMF_StateReconcile(exportState, vm, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        !call ESMF_StatePrint(exportState, rc=status)
        !if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        !    ESMF_CONTEXT, rc)) return
  
        call ESMF_StateGet(importState, itemcount=itemcount, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        !print *, "Import State contains ", itemcount, " items."
        call ESMF_StateGet(exportState, itemcount=itemcount, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        !print *, "Export State contains ", itemcount, " items."

        ! Get the src and dst arrays
        call ESMF_StateGet(importState, "sorted_data1", sorted_data1, rc=status)       
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_StateGet(exportState, "sorted_data2", sorted_data2, rc=status)       
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! precompute redist handle
        call ESMF_ArrayRedistStore(sorted_data1, sorted_data2, redistRH12, &
            rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        rc = status
  
    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
#undef ESMF_METHOD
#define ESMF_METHOD "usercpl_run"
    subroutine user_run(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer,intent(out) :: rc

        ! Local variables
        integer :: status = ESMF_SUCCESS
        type(ESMF_Array) :: sorted_data1, sorted_data2

        print *, "User Coupler Run starting"
        
        ! query data from States
        call ESMF_StateGet(importState, "sorted_data1", sorted_data1, rc=status)    
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_StateGet(exportState, "sorted_data2", sorted_data2, rc=status)    
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! preform data redistribution
        ! deliver sorted result from component 1 to component 2
        ! component 2 will verify component 1 result
        call ESMF_ArrayRedist(srcArray=sorted_data1, dstArray=sorted_data2, &
          routehandle=redistRH12, checkflag=.true., rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
#undef ESMF_METHOD
#define ESMF_METHOD "usercpl_final"
    subroutine user_final(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer,intent(out) :: rc
  
        ! Local variables
        integer :: status = ESMF_SUCCESS
  
        print *, "User Coupler Final starting"
          
        ! release route handle
        call ESMF_ArrayRedistRelease(redistRH12, rc=status)     
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        rc = status

    end subroutine user_final

    end module user_coupler
    
!\end{verbatim}
