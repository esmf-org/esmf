! $Id: user_coupler.F90,v 1.18 2009/09/22 14:56:34 feiliu Exp $
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
        
    type(ESMF_RouteHandle), save :: routehandle1, routehandle2
    type(ESMF_Field), save :: h1a, h2a, humidityM

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

        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, user_init, rc=rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, user_run, rc=rc)
        call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, rc=rc)

        print *, "Registered Initialize, Run, and Finalize routines"

        rc = ESMF_SUCCESS

    end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
    subroutine user_init(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

        ! Local variables
        type(ESMF_Field) :: humidity1, humidity2, humidity3
        type(ESMF_VM) :: vm
        type(ESMF_IGrid) :: igrid1
        type(ESMF_Array) :: array1
        type(ESMF_FieldDataMap) :: datamap1
        type(ESMF_ArraySpec) :: arrayspec1
        integer :: rank1
        type(ESMF_TypeKind) :: dk1
        integer :: status
     
        print *, "User Coupler Init starting"

        call ESMF_StateGetField(importState, "humidity", humidity1, &
                                "comp1 export", rc)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_FieldPrint(humidity1, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10
  
        call ESMF_StateGetField(importstate, "humidity", humidity2, &
                                "comp1 export", rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! one route from h1, one from h2

        call ESMF_StateGetField(exportState, "humidity", humidity3, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_FieldPrint(humidity3, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Get VM from coupler component to use in computing the redist
        call ESMF_CplCompGet(comp, vm=vm, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! Precompute communication patterns
        call ESMF_FieldRedistStore(humidity1, humidity3, vm, &
                                   routehandle=routehandle1, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        call ESMF_FieldRedistStore(humidity2, humidity3, vm, &
                                   routehandle=routehandle2, rc=status)
        if (status .ne. ESMF_SUCCESS) goto 10

        ! TODO: make this work: field create from field
        !humidityM = ESMF_FieldCreateFromField(humidity1, rc=rc)

        call ESMF_FieldGet(humidity1, igrid=igrid1, datamap=datamap1, &
                           array=array1, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_ArrayGet(array1, rank=rank1, typekind=dk1, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10
        call ESMF_ArraySpecSet(arrayspec1, rank1, dk1, rc)
        if (status .ne. ESMF_SUCCESS) goto 10
 
        h1a = ESMF_FieldCreate(igrid=igrid1, arrayspec=arrayspec1, &
                               datamap=datamap1, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        h2a = ESMF_FieldCreate(igrid=igrid1, arrayspec=arrayspec1, &
                               datamap=datamap1, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        humidityM = ESMF_FieldCreate(igrid=igrid1, arrayspec=arrayspec1, &
                                     datamap=datamap1, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 10

        print *, "User Coupler Init returning"

10 continue
        rc = status

    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
    subroutine user_run(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc

      ! Local variables
        type(ESMF_Field) :: humidity1, humidity2, humidity3
        real(ESMF_KIND_R4), dimension(:,:), pointer :: h1, h2, hM
        integer :: status

        print *, "User Coupler Run starting"

        ! Get input data
        call ESMF_StateGetField(importState, "humidity", humidity1, &
                                "comp1 export", rc)
        call ESMF_FieldPrint(humidity1, rc=rc)

        call ESMF_StateGetField(importState, "humidity", humidity2, &
                                "comp2 export", rc)
        call ESMF_FieldPrint(humidity2, rc=rc)


        ! Get location of output data
        call ESMF_StateGetField(exportState, "humidity", humidity3, rc=rc)
        call ESMF_FieldPrint(humidity3, rc=rc)

        ! These are fields on different layouts - call Redist to rearrange
        !  the data using the Comm routines.
        call ESMF_FieldRedist(humidity1, h1a, routehandle1, rc=status)
        call ESMF_FieldRedist(humidity2, h2a, routehandle2, rc=status)

        ! Get data pointers, merge data
        call ESMF_FieldGetDataPointer(h1a, h1, rc=rc)
        call ESMF_FieldGetDataPointer(h2a, h2, rc=rc)
        call ESMF_FieldGetDataPointer(humidityM, hM, rc=rc)

        hM(:,:) = h1(:,:) + h2(:,:)


        ! Output data operated on in place, export state now has new values.
        call ESMF_StatePrint(exportState, rc=status)

 
        print *, "User Coupler Run returning"

        rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, importState, exportState, clock, rc)
        type(ESMF_CplComp) :: comp
        type(ESMF_State) :: importState, exportState
        type(ESMF_Clock) :: clock
        integer :: rc


        print *, "User Coupler Final starting"

        call ESMF_StatePrint(importState, rc=rc)
        call ESMF_StatePrint(exportState, rc=rc)
    
        call ESMF_FieldRedistRelease(routehandle1)
        call ESMF_FieldRedistRelease(routehandle1)
        call ESMF_FieldDestroy(humidityM, rc=rc)

        print *, "User Coupler Final returning"
   
        rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
