! $Id$
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component
!
!
!\begin{verbatim}

module user_model2

  ! ESMF Framework module
  use ESMF

  implicit none
    
  public userm2_setvm, userm2_register
        
  contains

!--------------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm2_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc
    type(ESMF_VM) :: vm
    logical       :: pthreadsEnabled
    logical       :: ssiSharedMemoryEnabled
    integer       :: ssiMaxPetCount

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_LogWrite("Executing 'userm2_setvm'", ESMF_LOGMSG_INFO, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! The following call will give each PET up to half the number of PEs that
    ! are held by the largest SSI. This will reduce the number of PETs that are
    ! executing the component, but each PET will have multipe PEs available,
    ! e.g. to do user-level OpenMP threading.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, &
      ssiSharedMemoryEnabledFlag=ssiSharedMemoryEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled.and.ssiSharedMemoryEnabled) then
      call ESMF_VMGet(vm, ssiMaxPetCount=ssiMaxPetCount, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      call ESMF_GridCompSetVMMaxPEs(comp, &
        maxPeCountPerPet=ssiMaxPetCount, &
        rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif

  end subroutine

  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_LogWrite("Executing 'userm2_register'", ESMF_LOGMSG_INFO, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Register the callback routines.
    call ESMF_GridCompSetEntryPoint(comp, methodflag=ESMF_METHOD_RUN, &
      userRoutine=user2_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!--------------------------------------------------------------------------------
!   !  The Run routine where data is validated.
!   !

  subroutine user2_run(comp, importState, exportState, clock, rc)
!$  use omp_lib
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    real(ESMF_KIND_R8)    :: pi
    type(ESMF_VM)         :: vm
    logical               :: ssiSharedMemoryEnabled
    type(ESMF_Field)      :: field
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:)   ! matching F90 array pointer
    integer               :: i, j, k, loop, tid, currentSsiPe
    integer               :: localDeCount, lde, deCount
    integer, allocatable  :: localDeToDeMap(:)
    type(ESMF_LocalArray), allocatable :: localArrayList(:)
    character(len=320)    :: msg
    logical               :: dataOkay

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_LogWrite("Entering 'user2_run'", ESMF_LOGMSG_INFO, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    pi = 3.14159d0

    ! Query the VM and do some logging
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, ssiSharedMemoryEnabledFlag=ssiSharedMemoryEnabled, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMLog(vm, prefix="model2: ", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get the source Field from the export State
    call ESMF_StateGet(importState, "MyField", field, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Determine all accessible DEs from local PET
    call ESMF_FieldGet(field, ssiLocalDeCount=localDeCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    write(msg,*) "user2_run: ssiLocalDeCount=", localDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

    ! Allocate map and list variables
    allocate(localDeToDeMap(0:localDeCount-1))
    allocate(localArrayList(0:localDeCount-1))
    
    ! Request map and list variables from the Field
    call ESMF_FieldGet(field, localDeToDeMap=localDeToDeMap, &
      localarrayList=localArrayList, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Test sharing for correctness
    dataOkay = .true.

do loop=1, 5 ! repeatedly go through the work loops to monitor PE affinity.

!$omp parallel do reduction (.and.:dataOkay) &
!$omp& default (none)  &
!$omp& shared  (vm, pi, localArrayList, localDeToDeMap, localDeCount)  &
!$omp& private (lde, i, j, tid, currentSsiPe, farrayPtr, msg, rc)
    ! Loop over all the locally accessible DEs and check for data correctness

    do lde=0, localDeCount-1
!$    tid = omp_get_thread_num()
      ! Access the data pointer for this DE
      call ESMF_LocalArrayGet(localArrayList(lde), farrayPtr=farrayPtr, rc=rc)
      ! No RC checking inside OpenMP region
      
      call ESMF_VMGet(vm, currentSsiPe=currentSsiPe, rc=rc)
      ! No RC checking inside OpenMP region
      
      !! Doing logging inside the OpenMP loop is just done to produce output
      !! to show that the loop is parallelized for the test. Not a good idea
      !! for real applications!
!$omp critical
      write(msg,*) "user2_run: OpenMP thread:", tid, &
        " on SSIPE: ", currentSsiPe, " Testing data for localDe =", lde, &
        " DE=", localDeToDeMap(lde), &
        " lbound:", lbound(farrayPtr), " ubound:", ubound(farrayPtr)
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      ! No RC checking inside OpenMP region
      !call ESMF_LogFlush(rc=rc)
      ! No RC checking inside OpenMP region
!$omp end critical

      ! Test Field data against exact solution
      do k = lbound(farrayPtr, 3), ubound(farrayPtr, 3)
      do j = lbound(farrayPtr, 2), ubound(farrayPtr, 2)
      do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
          if (abs(farrayPtr(i,j,k) - (10.0d0 &
            + 5.0d0 * sin(real(i,ESMF_KIND_R8)/100.d0*pi) &
            + 2.0d0 * sin(real(j,ESMF_KIND_R8)/150.d0*pi))) > 1.d-8) then
            dataOkay = dataOkay .and. .false.
          endif
      enddo
      enddo
      enddo

    enddo
!$omp end parallel do

enddo
  
    if (dataOkay) then
      write(msg,*) "user2_run: All data correct."
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    else
      write(msg,*) "user2_run: Incorrect data detected."
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      rc=ESMF_FAILURE ! pass error back to the parent level
    endif

    ! Deallocate map and list variables
    deallocate(localDeToDeMap)
    deallocate(localArrayList)

    call ESMF_LogWrite("Exiting 'user2_run'", ESMF_LOGMSG_INFO, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine user2_run

end module user_model2
!\end{verbatim}
