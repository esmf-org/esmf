! $Id: user_coupler.F90,v 1.5 2003/08/21 19:58:43 nscollins Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Coupler
!
!
!\begin{verbatim}

    module user_coupler

    ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
    public usercpl_register
        
    ! global data
    type(ESMF_RouteHandle), save :: routehandle

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
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
 
    
    subroutine user_init(comp, statelist, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: statelist
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      integer :: itemcount
      type(ESMF_State) :: state1, state2
      type(ESMF_Field) :: humidity1, humidity2
      type(ESMF_DELayout) :: cpllayout


      print *, "User Coupler Init starting"

      call ESMF_StateGetInfo(statelist, itemcount=itemcount, rc=rc)
      print *, "Statelist contains ", itemcount, " items."
       
      call ESMF_StateGetData(statelist, "comp1 export", state1, rc)
      call ESMF_StatePrint(state1, rc=rc)

      call ESMF_StateGetData(statelist, "comp2 import", state2, rc)
      call ESMF_StatePrint(state2, rc=rc)

      ! Get input data
      call ESMF_StateGetData(state1, "humidity", humidity1, rc=rc)
      call ESMF_FieldPrint(humidity1, rc=rc)

      ! Get location of output data
      call ESMF_StateGetData(state2, "humidity", humidity2, rc=rc)
      call ESMF_FieldPrint(humidity1, rc=rc)

      ! Get layout from coupler component
      call ESMF_CplCompGet(comp, layout=cpllayout, rc=rc)


      ! These are fields on different Grids - call RegridStore to set
      ! up the Regrid structure

      call ESMF_FieldRegridStore(humidity1, humidity2, cpllayout, &
                                 routehandle, &
                                 regridtype=ESMF_RegridMethod_Bilinear, &
                                 rc=rc)


      print *, "User Coupler Init returning"
   
      rc = ESMF_SUCCESS

    end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
    subroutine user_run(comp, statelist, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: statelist
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      type(ESMF_State) :: mysource, mydest
      type(ESMF_Field) :: humidity1, humidity2
      type(ESMF_DELayout) :: cpllayout
      integer :: status

      print *, "User Coupler Run starting"

      ! Get input data
      call ESMF_StateGetData(statelist, "comp1 export", mysource, rc)
      call ESMF_StatePrint(mysource, rc=status)
      call ESMF_StateGetData(mysource, "humidity", humidity1, rc=status)
      call ESMF_FieldPrint(humidity1, rc=status)

      ! Get location of output data
      call ESMF_StateGetData(statelist, "comp2 import", mydest, rc)
      call ESMF_StatePrint(mydest, rc=status)
      call ESMF_StateGetData(mydest, "humidity", humidity2, rc=status)
      call ESMF_FieldPrint(humidity1, rc=status)

      ! Get layout from coupler component
      call ESMF_CplCompGet(comp, layout=cpllayout, rc=status)

      ! These are fields on different Grids - call Regrid to rearrange
      !  and move the data.  The communication and weights have been
      !  precomputed during the init phase.
      call ESMF_FieldRegrid(humidity1, humidity2, routehandle, rc=status)

      ! Set output data
      call ESMF_StateAddData(mydest, humidity2, rc=status)
      call ESMF_StatePrint(mydest, rc=status)
 
      print *, "User Coupler Run returning"

      rc = status

    end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
    subroutine user_final(comp, statelist, clock, rc)
      type(ESMF_CplComp) :: comp
      type(ESMF_State) :: statelist
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      type(ESMF_State) :: state1, state2

      print *, "User Coupler Final starting"
      call ESMF_StateGetData(statelist, "comp1 export", state1, rc)
      call ESMF_StatePrint(state1, rc=rc)

      call ESMF_StateGetData(statelist, "comp2 import", state2, rc)
      call ESMF_StatePrint(state2, rc=rc)
   
      ! Release resources stored for the Regridding.
      call ESMF_FieldRegridRelease(routehandle, rc)

      print *, "User Coupler Final returning"
   
      rc = ESMF_SUCCESS

    end subroutine user_final


    end module user_coupler
    
!\end{verbatim}
    
