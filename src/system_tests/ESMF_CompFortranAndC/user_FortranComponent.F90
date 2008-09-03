! $Id: user_FortranComponent.F90,v 1.2 2008/09/03 23:39:23 rosalind Exp $
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
    
    public user_register
        
    real(ESMF_KIND_R8), save, allocatable    :: farray(:,:)
    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine user_register(comp, rc)
        type(ESMF_GridComp) :: comp
        integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
        type(ESMF_VM) :: vm
        logical :: supportPthreads
#endif

        print *, "In user register routine"

        ! Register the callback routines.


        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
                                                          ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
                                                          ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
                                                          ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"


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

        rc = ESMF_SUCCESS

    end subroutine


!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   second and main Initialization routine.
 
    

    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        type(ESMF_Array) :: array
        type(ESMF_Array) :: array0
        type(ESMF_ArraySpec) :: arrayspec
        type (ESMF_DistGrid) :: distgrid
        character(ESMF_MAXSTR) :: name
        integer, intent(out) :: rc

        !local data
        integer :: i
        print *, "Fortran User Comp Init starting"

        ! Allocate the Fortran array
        allocate (farray(5,2))

        ! This is where the model specific setup code goes.  

        call ESMF_GridCompPrint(comp, "", rc)
        call ESMF_StatePrint(exportState, "", rc)


        ! Add an array state to the export state.
        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, &
          rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,2/), &
           rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

        do i=1,5
          farray(i,:) = float(i)
        end do

        array = ESMF_ArrayCreate(farray=farray, distgrid=distgrid, &
          name="array1", rc=rc)

       !call ESMF_ArrayPrint(array)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

        call ESMF_StateAdd(exportState, array, rc)
        write(name, *) "Adding an Array to a State Test"

        call ESMF_StateGet(exportState,"array1", array0, rc=rc)
        call ESMF_ArrayPrint(array0)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

        call ESMF_StatePrint(exportState, rc=rc)

        print *, "User Comp Init returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc

        integer, save :: onetime=1              ! static variable

        type(ESMF_Array) :: array0
        real(ESMF_KIND_R8), pointer, dimension(:,:) :: farrayPtr
        integer :: status
        integer :: i,j

        print *, "User Fortran Comp Run starting"

        ! static data array farray modified in my_InitInC
        print *, "In Fortran Component Run, farray= ",farray

        call ESMF_StateGet(exportState,"array1", array0, rc=rc)
        call ESMF_ArrayPrint(array0)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

        call ESMF_ArrayGet(array0, 0, farrayPtr=farrayPtr, rc=rc)

          ! values must be as set in "myInitInC"
          do j=1,2
            do i=1,5
              if ( abs(farrayPtr(i,j)-float(j-1)) > 1.e-8 ) then
                print *, "ERROR! farrayPtr has wrong value at i,j=",i,j
                call ESMF_Finalize(terminationflag=ESMF_ABORT)
              end if
            end do
          end do 

       print *,"data in exp state successfully transmitted to user_run in", &
               " Fortran Component"

       ! modify the data again 
       do j=1,2
         do i=1,5
           farrayPtr(i,j) = float(j*10+i)
         end do
       end do 

       print *," data in exp state data modified in user_run in Fortran", &
               " component"

       call ESMF_ArrayPrint(array0)

       !call ESMF_StatePrint(exportState, rc=status)
 
        print *, "User Comp Run returning"

        rc = ESMF_SUCCESS

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer, intent(out) :: rc


        print *, "User Comp Final starting"
    
        deallocate (farray)    
 
        print *, "User Comp Final returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_FortranComponent
    
!\end{verbatim}
    
