! $Id: user_FortranComponent.F90,v 1.11 2009/05/29 19:24:42 theurich Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
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
    
    public mySetVMInFortran, myRegistrationInFortran
        
    real(ESMF_KIND_R8), save, allocatable    :: farray(:,:)
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine mySetVMInFortran(comp, rc)
        type(ESMF_GridComp) :: comp
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
          call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
        endif
#endif

    end subroutine

    subroutine myRegistrationInFortran(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

        ! Initialize return code
        rc = ESMF_SUCCESS

        print *, "In myRegistrationInFortran routine"

        ! Register the callback routines.


        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
          userRoutine=myInitInFortran, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
          userRoutine=myRunInFortran, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
          userRoutine=myFinalInFortran, rc=rc)

        print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine


!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !  Initialization routine.
 
    

    subroutine myInitInFortran(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        type(ESMF_Array) :: array
        type(ESMF_ArraySpec) :: arrayspec
        type (ESMF_DistGrid) :: distgrid
        type (ESMF_VM) :: vm
        integer:: petCount
        character(ESMF_MAXSTR) :: name
        integer, intent(out) :: rc

        !local data
        integer :: i

        ! Initialize return code
        rc = ESMF_SUCCESS

        print *, "Fortran User Comp Init starting"

        ! Allocate the Fortran array
        allocate (farray(5,2))

        ! This is where the model specific setup code goes.  

        call ESMF_GridCompPrint(comp, "", rc=rc)
        call ESMF_StatePrint(exportState, "", rc=rc)
        
        call ESMF_GridCompGet(comp, vm=vm, rc=rc)
        call ESMF_VMGet(vm, petCount=petCount, rc=rc)


        ! Add an array state to the export state.
        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, &
          rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), &
          maxIndex=(/5*petCount,2/), rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        do i=1,5
          farray(i,:) = float(i)
        end do

        array = ESMF_ArrayCreate(farray=farray, distgrid=distgrid, &
          indexflag=ESMF_INDEX_DELOCAL, name="array1", rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        write(name, *) "Adding an Array to a State Test"
        call ESMF_StateAdd(exportState, array, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        call ESMF_StatePrint(exportState, rc=rc)

        print *, "User routine myInitInFortran returning"
   
    end subroutine myInitInFortran


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine myRunInFortran(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        integer, save :: onetime=1              ! static variable

        type(ESMF_Array) :: array
        real(ESMF_KIND_R8), pointer, dimension(:,:) :: farrayPtr
        integer :: status
        integer :: i,j

        ! Initialize return code
        rc = ESMF_SUCCESS

        print *, "User Fortran Comp Run starting"

        ! static data array farray modified in my_InitInC
        print *, "In Fortran Component Run, farray= ",farray

        call ESMF_StateGet(exportState,"array1", array, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        call ESMF_ArrayGet(array, 0, farrayPtr=farrayPtr, rc=rc)

          ! values must be as set in "myInitInC"
          do j=1,2
            do i=1,5
              if ( abs(farrayPtr(i,j)-float(j-1)) > 1.e-8 ) then
                print *, "ERROR! farrayPtr has wrong value at i,j=",i,j
                if (rc/=ESMF_SUCCESS) return ! bail out
              end if
            end do
          end do 

       print *,"data in exp state successfully transmitted to myRunInFortran in", &
               " Fortran Component"

       ! modify the data again 
       do j=1,2
         do i=1,5
           farrayPtr(i,j) = float(j*10+i)
         end do
       end do 

       print *," data in exp state data modified in myRunInFortran in Fortran", &
               " component"

       print *, "User routine myRunInFortran returning"

    end subroutine myRunInFortran


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine myFinalInFortran(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        ! Internal data
        type(ESMF_Array) :: array

        ! Initialize return code
        rc = ESMF_SUCCESS

        print *, "User Comp Final starting"
    
        ! Extract the exportState array so it can be destroyed
        call ESMF_StateGet(exportState,"array1", array=array, rc=rc)

        ! Free up memory
        call ESMF_ArrayDestroy(array, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        deallocate (farray)    
 
        print *, "User routine myFinalInFortran returning"
   
    end subroutine myFinalInFortran


    end module user_FortranComponent
    
!\end{verbatim}
    
