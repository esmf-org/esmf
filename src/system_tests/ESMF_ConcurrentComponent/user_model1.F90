! $Id: user_model1.F90,v 1.1.2.6 2008/11/07 22:58:18 theurich Exp $
!
! System test for Concurrent Components.  User-code, component 1.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-written code, supplies functions for component 1.
!  Creates a Field which is passed back thru the Export State.
!
!\begin{verbatim}
#include "ESMF.h"

    module user_model1

    ! ESMF Framework module
    use ESMF_TestMod
    use ESMF_Mod

    implicit none
    
    public userm1_register
        
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
#define ESMF_METHOD "userm1_register"

    subroutine userm1_register(comp, rc)
        type(ESMF_GridComp)  :: comp
        integer, intent(out) :: rc

        integer                             :: status = ESMF_SUCCESS

        print *, "In user 1 register routine"
        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                        user_init, ESMF_SINGLEPHASE, status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                        user_run, ESMF_SINGLEPHASE, status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                       user_final, ESMF_SINGLEPHASE, status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        !print *, "Registered Initialize, Run, and Finalize routines"

        ! return code
        rc = status

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
#define ESMF_METHOD "userm1_init"
    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: comp
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

       ! Local variables
        integer                             :: status = ESMF_SUCCESS
        type(ESMF_VM)                       :: vm
        type(ESMF_Array)                    :: rawdata, sorted_data
        type(ESMF_DistGrid)                 :: distgrid
        type(ESMF_ArraySpec)                :: arrayspec
        integer                             :: petCount
  
        print *, "In user 1 init routine"
        ! Determine petCount
        call ESMF_GridCompGet(comp, vm=vm, rc=rc)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, petCount=petCount, rc=rc)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/9/), &
            regDecomp=(/petCount/), rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        rawdata = ESMF_ArrayCreate(arrayspec, indexflag=ESMF_INDEX_GLOBAL, &
            distgrid=distgrid, name="rawdata", rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_StateAdd(exportState, rawdata, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
  
        sorted_data = ESMF_ArrayCreate(arrayspec, indexflag=ESMF_INDEX_GLOBAL, &
            distgrid=distgrid, name="sorted_data1", rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_StateAdd(exportState, sorted_data, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        rc = status

    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
#define ESMF_METHOD "userm1_run"
    subroutine user_run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: comp
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        ! local variables
        integer                             :: status = ESMF_SUCCESS

        type(ESMF_Array)                    :: rawdata, sorted_data
        integer, dimension(9), target       :: d = (/3,7,8,5,2,1,9,5,4/)
        integer, dimension(:), pointer      :: pd        ! raw data ptr
        integer, dimension(:), pointer      :: rdptr     ! raw data ptr
        integer, dimension(:), pointer      :: sdptr     ! sorted data ptr
  
        print *, "In user 1 run routine"
        call ESMF_StateGet(exportState, "rawdata", rawdata, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_ArrayGet(rawdata, localDe=0, farrayPtr=rdptr, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
  
        call ESMF_StateGet(exportState, "sorted_data1", sorted_data, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_ArrayGet(sorted_data, localDe=0, farrayPtr=sdptr, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! sort the input data locally
        pd => d
        call quicksortI4(pd, 1, 9)

        ! assign sorting result to output that will be delivered to component 2
        ! through coupler component
        sdptr(:) = d(lbound(sdptr, 1):ubound(sdptr, 1))

        rc = status

    end subroutine user_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
#define ESMF_METHOD "userm1_final"
    subroutine user_final(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: comp
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        ! local variables
        integer                             :: status = ESMF_SUCCESS
        type(ESMF_Array)                    :: rawdata, sorted_data
        type(ESMF_DistGrid)                 :: distgrid

        print *, "In user 1 final routine"

        call ESMF_StateGet(exportState, "rawdata", rawdata, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArrayGet(rawdata, distgrid=distgrid, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
  
        call ESMF_StateGet(exportState, "sorted_data1", sorted_data, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArrayDestroy(rawdata, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_ArrayDestroy(sorted_data, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_DistGridDestroy(distgrid, rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        rc = status

    end subroutine user_final

    function partition(array, left, right, pindex)
        integer, pointer :: array(:)
        integer, intent(in)    :: left, right, pindex

        integer :: partition

        integer :: pvalue, tmp, sindex, i

        pvalue = array(pindex)
        tmp = array(right)
        array(right) = pvalue
        array(pindex) = tmp

        sindex = left

        do i = left, right-1
            if(array(i) .le. pvalue) then
                tmp = array(i)
                array(i) = array(sindex)
                array(sindex) = tmp
                sindex = sindex + 1
            endif
        end do

        tmp = array(sindex)
        array(sindex) = array(right)
        array(right) = tmp
        partition = sindex
    end function partition

    recursive subroutine quicksortI4(array, left, right)

        integer, pointer :: array(:)
        integer, intent(in)    :: left, right

        integer :: pindex, npindex
        if(right .gt. left) then
            pindex = left + (right - left)/2
            npindex = partition(array, left, right, pindex)
            call quicksortI4(array, left, npindex-1)
            call quicksortI4(array, npindex+1, right)
        endif
    end subroutine quicksortI4

    end module user_model1
    
!\end{verbatim}
