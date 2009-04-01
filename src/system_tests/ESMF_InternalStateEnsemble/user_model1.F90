! $Id: user_model1.F90,v 1.6 2009/04/01 20:20:39 svasquez Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_model1

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userm1_setvm, userm1_register

  !module variables
  private 

  ! Internal State Variables
  type testData
  sequence
      type(ESMF_ArraySpec)  :: arrayspec
      type(ESMF_DistGrid)   :: distgrid
      type(ESMF_Array)      :: array
  end type

   type dataWrapper
   sequence
      type(testData), pointer :: p 
   end type
      
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
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
    call ESMF_VMGet(vm, supportPthreadsFlag=supportPthreads, rc=rc)
    if (supportPthreads) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Register starting"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "Registered Initialize, Run, and Finalize routines"
    print *, "User Comp1 Register returning"
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    integer               :: petCount
    integer               :: i, j
    real(ESMF_KIND_R8), pointer :: farrayPtr1(:,:)   ! matching F90 array pointer
    real(ESMF_KIND_R8), pointer :: farrayPtr2(:,:)   ! matching F90 array pointer
    real(ESMF_KIND_R8), pointer :: farrayPtr3(:,:)   ! matching F90 array pointer
    type (testData), pointer :: data1, data2, data3
    type(dataWrapper) :: wrap1, wrap2, wrap3


    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Init starting"

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Initialize internal State 1
    ! Allocate private data block
    allocate(data1)
    
    ! Create the source Array and add it to the export State
    call ESMF_ArraySpecSet(arrayspec=data1%arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data1%distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data1%array = ESMF_ArrayCreate(arrayspec=data1%arrayspec, distgrid=data1%distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array=data1%array, name="array data1", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=data1%array, localDe=0, farrayPtr=farrayPtr1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Fill source Array with data of all one's.
    do j = lbound(farrayPtr1, 2), ubound(farrayPtr1, 2)
       do i = lbound(farrayPtr1, 1), ubound(farrayPtr1, 1)
                  farrayPtr1(i,j) = 1
       enddo
    enddo
print *, " array1 = ", farrayPtr1(1,1)

    call ESMF_StateAdd(exportState, data1%array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    wrap1%p => data1

    ! Set internal State 1
    call ESMF_GridCompSetInternalState(comp, wrap1, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
    ! Initialize internal State 2
    ! Allocate private data block
    allocate(data2)
    
    ! Create the source Array and add it to the export State
    call ESMF_ArraySpecSet(arrayspec=data2%arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data2%distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data2%array = ESMF_ArrayCreate(arrayspec=data2%arrayspec, distgrid=data2%distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array=data2%array, name="array data2", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=data2%array, localDe=0, farrayPtr=farrayPtr2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Fill source Array with data of all two's.
    do j = lbound(farrayPtr2, 2), ubound(farrayPtr2, 2)
       do i = lbound(farrayPtr2, 1), ubound(farrayPtr2, 1)
                  farrayPtr2(i,j) = 2
       enddo
    enddo
print *, " array2 = ", farrayPtr2(1,1)

    call ESMF_StateAdd(exportState, data2%array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    wrap2%p => data2
    ! Set internal State 2
    call ESMF_GridCompSetInternalState(comp, wrap2, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
   
    ! Initialize internal State 3
    ! Allocate private data block
    allocate(data3)
    
    ! Create the source Array and add it to the export State
    call ESMF_ArraySpecSet(arrayspec=data3%arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data3%distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data3%array = ESMF_ArrayCreate(arrayspec=data3%arrayspec, distgrid=data3%distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array=data3%array, name="array data3", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=data2%array, localDe=0, farrayPtr=farrayPtr3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Fill source Array with data of all three's.
    do j = lbound(farrayPtr3, 2), ubound(farrayPtr3, 2)
       do i = lbound(farrayPtr3, 1), ubound(farrayPtr3, 1)
                  farrayPtr3(i,j) = 3
       enddo
    enddo
print *, " array3 = ", farrayPtr3(1,1)

    call ESMF_StateAdd(exportState, data3%array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    wrap3%p => data3
    ! Set internal State 3
    call ESMF_GridCompSetInternalState(comp, wrap3, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    print *, "User Comp1 Init returning"

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    real(ESMF_KIND_R8), pointer :: farrayPtr1(:,:)   ! matching F90 array pointer
    real(ESMF_KIND_R8), pointer :: farrayPtr2(:,:)   ! matching F90 array pointer
    real(ESMF_KIND_R8), pointer :: farrayPtr3(:,:)   ! matching F90 array pointer
    integer               :: i, j
    type(ESMF_Time) :: startTime, currTime
    type(ESMF_Calendar) :: gregorianCalendar
    type (testData), pointer :: data1, data2, data3
    type(dataWrapper) :: wrap1, wrap2, wrap3

    
    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Run starting"


    ! Get internal State 1
    call ESMF_GridCompGetInternalState(comp, wrap1, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data1 => wrap1%p

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=data1%array, localDe=0, farrayPtr=farrayPtr1, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! multiple each element by 10
    do j = lbound(farrayPtr1, 2), ubound(farrayPtr1, 2)
           do i = lbound(farrayPtr1, 1), ubound(farrayPtr1, 1)
                 farrayPtr1(i,j) = farrayPtr1(i,j) * 10
           enddo
    enddo
print *, " array1 = ", farrayPtr1(1,1)

    ! Add array to export state
    call ESMF_StateAdd(exportState, data1%array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    ! Get internal State 2
    call ESMF_GridCompGetInternalState(comp, wrap2, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data2 => wrap2%p

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=data2%array, localDe=0, farrayPtr=farrayPtr2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

   
    ! multiple each element by 10
    do j = lbound(farrayPtr2, 2), ubound(farrayPtr2, 2)
           do i = lbound(farrayPtr2, 1), ubound(farrayPtr2, 1)
                 farrayPtr2(i,j) = farrayPtr2(i,j) * 10
           enddo
    enddo
print *, " array2 = ", farrayPtr2(1,1)

    ! Add array to export state
    call ESMF_StateAdd(exportState, data2%array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out



    ! Get internal State 3
    call ESMF_GridCompGetInternalState(comp, wrap3, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data3 => wrap3%p

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array=data3%array, localDe=0, farrayPtr=farrayPtr3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! multiple each element by 10
    do j = lbound(farrayPtr3, 2), ubound(farrayPtr3, 2)
           do i = lbound(farrayPtr3, 1), ubound(farrayPtr3, 1)
                 farrayPtr3(i,j) = farrayPtr3(i,j) * 10
           enddo
    enddo
print *, " array3 = ", farrayPtr3(1,1)

    ! Add array to export state
    call ESMF_StateAdd(exportState, data3%array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    print *, "User Comp1 Run returning"

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type (dataWrapper) :: wrap1, wrap2, wrap3
    type (testData), pointer :: data1, data2, data3

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "User Comp1 Final starting"

    ! Get internal State 1
    call ESMF_GridCompGetInternalState(comp, wrap1, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data1 = wrap1%p
    deallocate(data1)

    ! Get internal State 2
    call ESMF_GridCompGetInternalState(comp, wrap2, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data2 = wrap2%p
    deallocate(data2)

    ! Get internal State 3
    call ESMF_GridCompGetInternalState(comp, wrap3, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    data3 = wrap3%p
    deallocate(data3)

    print *, "User Comp1 Final returning"

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
