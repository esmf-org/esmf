! $Id$
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

module user_modelA

#include "ESMF.h"

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod

  implicit none
    
  public usermA_register

  private

  type dblock
  sequence
     real(ESMF_KIND_R8)::offset
  end type

  type dataWrapper
  sequence
    type(dblock), pointer :: p
  end type
        
  contains

  subroutine usermA_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User CompA Register returning"
    
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
    type(ESMF_ArraySpec)  :: arrayspec
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Array)      :: array
    type(ESMF_VM)         :: vm
    integer               :: petCount, i, j
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching F90 array pointer
    real(ESMF_KIND_R8) :: perturbation
    character(len=ESMF_MAXSTR) :: compName
    type(dataWrapper) :: intStatePtr

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Get component information/attribute
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_AttributeGet(comp, "perturbation", perturbation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Create the source Array and add it to the export State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArraySet(array, name="array data", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Set the array values using the perturbation.
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
       do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
                  farrayPtr(i,j) = 10+perturbation
       enddo
    enddo

    ! Set the export state 
    call ESMF_StateAdd(exportState, (/array/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    allocate(intStatePtr%p)

    ! Set the internal state
    call ESMF_GridCompGet(comp, name=compName, rc=rc)
    if (trim(compName) .eq. "user model A-1") then
	intStatePtr%p%offset = 5.0
    else
	intStatePtr%p%offset = 10.0
    endif
    call ESMF_GridCompSetInternalState(comp, intStatePtr, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User CompA Init returning ", compName

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
    type(ESMF_Array)      :: array
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)   ! matching F90 array pointer
    integer               :: i, j, l1, l2
    character(len=ESMF_MAXSTR) :: compName
    type(dataWrapper)   :: intStatePtr
    real(ESMF_KIND_R8) :: offset

    call ESMF_GridCompGet(comp, name=compName, rc=rc)
    call ESMF_GridCompGetInternalState(comp, intStatePtr, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    offset = intStatePtr%p%offset
    print *, 'internal state', offset
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Get the source Array from the export State
    call ESMF_StateGet(exportState, "array data", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Gain access to actual data via F90 array pointer
    call ESMF_ArrayGet(array, localDe=0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! increment each element by the offset set in the init routine and passed over using
    ! internal state
    do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
   	do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
       		farrayPtr(i,j) = farrayPtr(i,j) + offset
      enddo
   enddo
  
#if 0
   l1=lbound(farrayPtr,1)
   l2=lbound(farrayPtr,2)
   print *, "User CompA Run returning ", trim(compName), farrayPtr(l1,l2)
#endif

   print *, "User CompA Run returning ", trim(compName)

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    type(dataWrapper)   :: intStatePtr

    ! Local variables
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Array) :: array
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_GridCompGetInternalState(comp, intStatePtr, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    deallocate(intStatePtr%p)

    call ESMF_StateGet(exportState, "array data", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayGet(array, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    print *, "User CompA Final returning"

  end subroutine user_final


end module user_modelA
    
!\end{verbatim}
