! $Id: user_coupler.F90,v 1.2 2004/03/16 23:28:17 cdeluca Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Coupler, Version A (minimalist)
!
!
!\begin{verbatim}

    module user_coupler

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
    public usercpl_register
        
    type(ESMF_RouteHandle), save :: routehandle
    type(ESMF_Field), save :: humidityM

    contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
    subroutine usercpl_register(comp, rc)
        type(ESMF_CplComp) :: comp
        integer :: rc

        print *, "in user setservices routine"

        ! Register the callback routines.

        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
                                                  ESMF_SINGLEPHASE, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
                                                  ESMF_SINGLEPHASE, rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
                                                  ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

        rc = ESMF_SUCCESS

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, importstate, exportstate, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importstate, exportstate
        type(ESMF_Clock) :: clock
        integer :: rc

        ! Local variables
        type(ESMF_Field) :: humidity1, humidity3
        type(ESMF_DELayout) :: cpllayout
        type(ESMF_Grid) :: grid1
        type(ESMF_Array) :: array1
        type(ESMF_DataMap) :: datamap1
        type(ESMF_ArraySpec) :: arrayspec1
        integer :: rank1, counts(2)
        type(ESMF_DataKind) :: dk1
        type(ESMF_DataType) :: dt1
     

        print *, "User Coupler Init starting"
        call ESMF_StateGetField(importstate, "humidity", humidity1, &
                                "comp1 export", rc)
        call ESMF_FieldPrint(humidity1, rc=rc)
  
        ! assumes humidity2 field is identical to humidity1

        call ESMF_StateGetField(exportstate, "humidity", humidity3, rc=rc)
        call ESMF_FieldPrint(humidity3, rc=rc)

        ! Get layout from coupler component
        call ESMF_CplCompGet(comp, layout=cpllayout, rc=rc)

        ! Precompute communication patterns
        call ESMF_FieldRedistStore(humidity1, humidity3, cpllayout, &
                                   routehandle, rc=rc)

        ! TODO: make this work: field create from field
        !humidityM = ESMF_FieldCreateFromField(humidity1, rc=rc)

        call ESMF_FieldGet(humidity1, grid=grid1, datamap=datamap1, &
                           array=array1, rc=rc)
        call ESMF_ArrayGet(array1, rank=rank1, type=dt1, kind=dk1, rc=rc)
        call ESMF_ArraySpecSet(arrayspec1, rank1, dt1, dk1, rc)
 
        humidityM = ESMF_FieldCreate(grid=grid1, arrayspec=arrayspec1, &
                                     datamap=datamap1, rc=rc)

        print *, "User Coupler Init returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
    subroutine user_run(comp, importstate, exportstate, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importstate, exportstate
        type(ESMF_Clock) :: clock
        integer :: rc

      ! Local variables
        type(ESMF_Field) :: humidity1, humidity2, humidity3
        real(ESMF_KIND_R4), dimension(:,:), pointer :: h1, h2, hM
        integer :: status

        print *, "User Coupler Run starting"

        ! Get input data
        call ESMF_StateGetField(importstate, "humidity", humidity1, &
                                "comp1 export", rc)
        call ESMF_FieldPrint(humidity1, "", rc=rc)

        call ESMF_StateGetField(importstate, "humidity", humidity2, &
                                "comp2 export", rc)
        call ESMF_FieldPrint(humidity2, "", rc=rc)


        ! Get location of output data
        call ESMF_StateGetField(exportstate, "humidity", humidity3, rc=rc)
        call ESMF_FieldPrint(humidity3, "", rc=rc)

        ! Get data pointers, merge data
        call ESMF_FieldGetDataPointer(humidity1, h1, rc=rc)
        call ESMF_FieldGetDataPointer(humidity2, h2, rc=rc)
        call ESMF_FieldGetDataPointer(humidityM, hM, rc=rc)

        hM(:,:) = h1(:,:) + h2(:,:)

        ! These are fields on different layouts - call Redist to rearrange
        !  the data using the Comm routines.
        call ESMF_FieldRedist(humidityM, humidity2, routehandle, rc=status)


        ! Output data operated on in place, export state now has new values.
        call ESMF_StatePrint(exportstate, rc=status)

 
        print *, "User Coupler Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importstate, exportstate, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importstate, exportstate
        type(ESMF_Clock) :: clock
        integer :: rc


        print *, "User Coupler Final starting"

        call ESMF_StatePrint(importstate, rc=rc)
        call ESMF_StatePrint(exportstate, rc=rc)
    
        call ESMF_FieldRedistRelease(routehandle)
        call ESMF_FieldDestroy(humidityM, rc=rc)

        print *, "User Coupler Final returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
