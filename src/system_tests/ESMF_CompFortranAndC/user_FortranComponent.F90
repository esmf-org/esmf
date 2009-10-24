! $Id: user_FortranComponent.F90,v 1.15 2009/10/24 05:35:18 theurich Exp $
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

module user_FortranComponent

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public mySetVMInFortran, mySetServicesInFortran
  
  ! module variable      
  real(ESMF_KIND_R8), save, allocatable    :: farray(:,:)
  
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine mySetVMInFortran(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "In mySetVMInFortran routine"

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for
    ! your own code development you probably don't want to include the
    ! following call unless you are interested in exploring ESMF's
    ! threading features.

    ! First test whether ESMF-threading is supported.
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif
  end subroutine

  subroutine mySetServicesInFortran(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "In mySetServicesInFortran routine"

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
      userRoutine=myInitInFortran, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
      userRoutine=myRunInFortran, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
      userRoutine=myFinalInFortran, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
  end subroutine


!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !  Initialization routine.

  subroutine myInitInFortran(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! Local variables
    type(ESMF_Array)        :: array
    type(ESMF_ArraySpec)    :: arrayspec
    type (ESMF_DistGrid)    :: distgrid
    type (ESMF_VM)          :: vm
    integer                 :: petCount, i
    character(ESMF_MAXSTR)  :: name

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "In myInitInFortran routine"

    ! Allocate the Fortran array and initialize data
    allocate (farray(5,2))
    
    do i=1,5
      farray(i,:) = float(i)
    end do

    ! This is where the model specific setup code goes.  

    call ESMF_GridCompPrint(comp, "", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StatePrint(exportState, "", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Create and Array
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5*petCount,2/),&
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    array = ESMF_ArrayCreate(farray=farray, distgrid=distgrid, &
      indexflag=ESMF_INDEX_DELOCAL, name="array1", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Add Array to the export State
    call ESMF_StateAdd(exportState, array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_StatePrint(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine myInitInFortran


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine myRunInFortran(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Array)                            :: array
    real(ESMF_KIND_R8), pointer, dimension(:,:) :: farrayPtr
    integer                                     :: i,j
    type(ESMF_Field)                            :: field

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "In myRunInFortran routine"

    ! print data that was modified on the C side in "myInitInC"
    print *, "In Fortran Component Run, farray= ", farray

    ! get Array object from export State    
    call ESMF_StateGet(exportState, "array1", array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! access Array data through farrayPtr
    call ESMF_ArrayGet(array, 0, farrayPtr=farrayPtr, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! values must be as set in "myInitInC"
    do j=1,2
      do i=1,5
        if ( abs(farrayPtr(i,j)-float(j-1)) > 1.e-8 ) then
          print *, "ERROR! farrayPtr has wrong value at i,j=",i,j
          rc = ESMF_FAILURE ! indicate failure in return code
          return ! bail out
        end if
      end do
    end do 

    ! modify the Array data again 
    do j=1,2
      do i=1,5
        farrayPtr(i,j) = float(j*10+i)
      end do
    end do 
    
    ! get Field object from import State    
    call ESMF_StateGet(importState, "Field from C", field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! print Field object to test its health
    call ESMF_FieldPrint(field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine myRunInFortran


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine myFinalInFortran(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! Local variables
    type(ESMF_Array)    :: array
    type(ESMF_DistGrid) :: distgrid

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "In myFinalInFortran routine"
    
    ! get Array object from export State    
    call ESMF_StateGet(exportState,"array1", array=array, rc=rc)

    ! Destroy Array and DistGrid
    call ESMF_ArrayGet(array, distgrid=distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_ArrayDestroy(array, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! deallocate Fortran memory allocation
    deallocate (farray)
 
  end subroutine myFinalInFortran


end module user_FortranComponent
    
!\end{verbatim}
    
