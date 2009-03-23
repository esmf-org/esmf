! $Id: CouplerMod.F90,v 1.5 2009/03/23 20:40:48 theurich Exp $
!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: CouplerMod.F90 - Source for 2-way Coupler Component
!
! !DESCRIPTION:
!  The Coupler Component provides two-way coupling between the Injector
!  and FlowSolver Models.  During initialization this Component is
!  responsible for setting that data "is needed" from the export state
!  of each model.  In its run routine it calls route to transfer the
!  needed data directly from one Component's export state to the other
!  Component's import state.
!
!
!EOP

    module global_data
      use ESMF_Mod
      type(ESMF_RouteHandle), save :: fromFlow_rh, fromInject_rh
    end module

    module CouplerMod

    ! ESMF Framework module - defines ESMF data types and procedures
    use global_data

    implicit none
    
    ! Public entry point 
    public Coupler_register
        
    contains


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: Coupler_register - public SetServices entry point

! !INTERFACE:
     subroutine Coupler_register(comp, rc)
!
! !ARGUMENTS:
     type(ESMF_CplComp), intent(inout) :: comp
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied setservices routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

      ! because none of the arguments to this subroutine will ever be optional,
      ! go ahead and set rc to an initial return code before using it below.
      ! (this makes some eager error-checking compilers happy.)
      rc = ESMF_FAILURE

      ! Register the callback routines.

      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, coupler_init, rc=rc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, coupler_run, rc=rc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, coupler_final, rc=rc)

      print *, "CouplerMod: Registered Initialize, Run, and Finalize routines"

    end subroutine


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: coupler_init - coupler init routine

! !INTERFACE:
      subroutine coupler_init(comp, importState, exportState, clock, rc)

!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied init routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Nested state object containing import data.
!     \item[exportState]
!          Nested state object containing export data.
!     \item[clock] 
!          External clock.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

!   ! Local variables
    type(ESMF_Field) :: src_field, dst_field
    type(ESMF_VM) :: vm
    character(ESMF_MAXSTR) :: statename

    print *, "Coupler Init starting"

    ! because none of the arguments to this subroutine will ever be optional,
    ! go ahead and set rc to an initial return code before using it below.
    ! (this makes some eager error-checking compilers happy.)
    rc = ESMF_FAILURE

    ! Get VM from coupler component to use in computing redistribution
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)

    call ESMF_StateGet(importState, name=statename, rc=rc)
    call ESMF_StateGet(importState, "SIE", src_field, rc=rc)
    call ESMF_StateGet(exportState, "SIE", dst_field, rc=rc)

    if (trim(statename) .eq. "FlowSolver Feedback") then
      call ESMF_StateSetNeeded(importState, "SIE", ESMF_NEEDED, rc)
      call ESMF_StateSetNeeded(importState, "V", ESMF_NEEDED, rc)
      call ESMF_StateSetNeeded(importState, "RHO", ESMF_NEEDED, rc)
      call ESMF_StateSetNeeded(importState, "FLAG", ESMF_NEEDED, rc)

      call ESMF_FieldRedistStore(src_field, dst_field, vm, &
                                 routehandle=fromFlow_rh, rc=rc)
      
    endif

    if (trim(statename) .eq. "Injection Feedback") then
      call ESMF_StateSetNeeded(importState, "SIE", ESMF_NEEDED, rc)
      call ESMF_StateSetNeeded(importState, "V", ESMF_NEEDED, rc)
      call ESMF_StateSetNeeded(importState, "RHO", ESMF_NEEDED, rc)
      call ESMF_StateSetNeeded(importState, "FLAG", ESMF_NEEDED, rc)

      call ESMF_FieldRedistStore(src_field, dst_field, vm, &
                                 routehandle=fromInject_rh, rc=rc)

    endif

    print *, "Coupler Init returning"
   
    end subroutine coupler_init


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: coupler_run - coupler run routine

! !INTERFACE:
      subroutine coupler_run(comp, importState, exportState, clock, rc)

!
! !ARGUMENTS:
     type(ESMF_CplComp), intent(inout) :: comp
     type(ESMF_State), intent(inout) :: importState, exportState
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied run routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Nested state object containing import data.
!     \item[exportState]
!          Nested state object containing export data.
!     \item[clock] 
!          External clock.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

      ! Local variables
        type(ESMF_Field) :: srcfield, dstfield
        type(ESMF_RouteHandle) :: routehandle
      
        character(len=ESMF_MAXSTR) :: statename
       
        integer :: i, datacount
        character(len=ESMF_MAXSTR), dimension(7) :: datanames

        ! none of the arguments to this subroutine will ever be optional, so
        ! go ahead and set rc to an initial return code before using it below.
        ! (this makes some eager error-checking compilers happy.)
        rc = ESMF_FAILURE

        datacount = 7
        datanames(1) = "SIE"
        datanames(2) = "U"
        datanames(3) = "V"
        datanames(4) = "RHO"
        datanames(5) = "P"
        datanames(6) = "Q"
        datanames(7) = "FLAG"

        ! In this case, the coupling is symmetric - you call redist going
        ! both ways - so we only care about the coupling direction in order 
        ! to get the right routehandle selected.
        call ESMF_StateGet(importState, name=statename, rc=rc)
        if (trim(statename) .eq. "FlowSolver Feedback") then
            routehandle = fromFlow_rh 
        else
            routehandle = fromInject_rh 
        endif

        do i=1, datacount

           ! check isneeded flag here
           if (.not. ESMF_StateIsNeeded(importState, datanames(i), rc)) then 
               !print *, "skipping field ", trim(datanames(i)), " not needed"
               cycle
           endif

           !print *, "processing field ", trim(datanames(i)), " as needed"
!BOP
! !DESCRIPTION:
! \subsubsection{Example of Redist Usage:}
!
!   The following piece of code provides an example of calling the data
!   redistribution routine  between two Fields in the Coupler Component.  
!   Unlike regrid, which translates between
!   different IGrids, redist translates between different DELayouts on
!   the same IGrid.   The first two lines get the Fields from the 
!   States, each corresponding to a different subcomponent.  One is
!   an Export State and the other is an Import State.
!
!\begin{verbatim}
           call ESMF_StateGet(importState, datanames(i), srcfield, rc=rc)
           call ESMF_StateGet(exportState, datanames(i), dstfield, rc=rc)
!\end{verbatim}
!
!   The redist routine uses information contained in the Fields and the
!   Coupler VM object to call the communication routines to move the data.
!   Because many Fields may share the same IGrid association, the same
!   routing information may be needed repeatedly.  Route information is 
!   saved so the precomputed information can be retained.  The following 
!   is an example of a Field redist call:
!
!\begin{verbatim}
           call ESMF_FieldRedist(srcfield, dstfield, routehandle, rc=rc)
!\end{verbatim}
!EOP

        enddo
 
        ! rc has the last error code already

    end subroutine coupler_run


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  coupler_final - finalization routine

! !INTERFACE:
      subroutine coupler_final(comp, importState, exportState, clock, rc)

!
! !ARGUMENTS:
      type(ESMF_CplComp) :: comp
      type(ESMF_State), intent(inout) :: importState, exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied finalize routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Nested state object containing import data.
!     \item[exportState]
!          Nested state object containing export data.
!     \item[clock] 
!          External clock.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

        print *, "Coupler Final starting"
   
        ! none of the arguments to this subroutine will ever be optional, so
        ! go ahead and set rc to an initial return code before using it below.
        ! (this makes some eager error-checking compilers happy.)
        rc = ESMF_FAILURE

        ! Only thing to do here is release redist and route handles
        call ESMF_FieldRedistRelease(fromFlow_rh, rc)

        call ESMF_FieldRedistRelease(fromInject_rh, rc)

        rc = ESMF_SUCCESS
    
        print *, "Coupler Final returning"
   
    end subroutine coupler_final


    end module CouplerMod
    
    
