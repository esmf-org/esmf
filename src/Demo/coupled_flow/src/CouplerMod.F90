! $Id: CouplerMod.F90,v 1.5 2003/09/04 18:57:54 cdeluca Exp $
!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: CouplerMod.F90 - Source for 2-way Coupler Component
!
! !DESCRIPTION:
!  The Coupler Component provides two-way coupling between the Injector
!  and FlowSolver Models.  During initialization this component is
!  responsible for setting that data "is needed" from the export state
!  of each model.  In its Run routine it calls Route to transfer the
!  needed data directly from one Component's export state to the other
!  Component's import state.
!
!
!EOP

    module CouplerMod

    ! ESMF Framework module - defines ESMF data types and procedures
    use ESMF_Mod

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

      ! Register the callback routines.

      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, coupler_init, &
                                                         ESMF_SINGLEPHASE, rc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, coupler_run, &
                                                         ESMF_SINGLEPHASE, rc)
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, coupler_final, &
                                                         ESMF_SINGLEPHASE, rc)

      print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: coupler_init - coupler init routine

! !INTERFACE:
      subroutine coupler_init(comp, statelist, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: comp
      type(ESMF_State), intent(inout) :: statelist
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
!     \item[statelist]
!          Nested state object.
!     \item[clock] 
!          External clock.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

!     ! Local variables
        type(ESMF_State) :: flowstates, injectstates
        type(ESMF_State) :: toflow, fromflow
        type(ESMF_State) :: toinject, frominject

        print *, "Coupler Init starting"

        call ESMF_StateGetData(statelist, &
                     "Coupler States FlowSolver to Injector", flowstates, rc)

        call ESMF_StateGetData(flowstates, "FlowSolver Feedback", fromflow, rc)
        call ESMF_StateSetNeeded(fromflow, "SIE", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(fromflow, "V", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(fromflow, "RHO", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(fromflow, "FLAG", ESMF_STATEDATAISNEEDED, rc)

        call ESMF_StateGetData(statelist, &
                       "Coupler States Injector to FlowSolver", injectstates, rc)

        call ESMF_StateGetData(injectstates, "Injection Feedback", frominject, rc)
        call ESMF_StateSetNeeded(frominject, "SIE", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(frominject, "V", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(frominject, "RHO", ESMF_STATEDATAISNEEDED, rc)
        call ESMF_StateSetNeeded(frominject, "FLAG", ESMF_STATEDATAISNEEDED, rc)

        print *, "Coupler Init returning"
   
    end subroutine coupler_init


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: coupler_run - coupler run routine

! !INTERFACE:
      subroutine coupler_run(comp, statelist, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_CplComp), intent(inout) :: comp
     type(ESMF_State), intent(inout) :: statelist
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
!     \item[statelist]
!          Nested state object.
!     \item[clock] 
!          External clock.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

      ! Local variables
        type(ESMF_State) :: toflow, toinjector
        type(ESMF_State) :: mysource, mydest
        type(ESMF_Field) :: srcfield, dstfield
        type(ESMF_Array) :: srcarray, dstarray
        real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: srcptr, dstptr
        type(ESMF_DELayout) :: cpllayout
      
        character(len=ESMF_MAXSTR) :: statename
       
        integer :: status
        integer :: i, datacount
        character(len=ESMF_MAXSTR), dimension(7) :: datanames

        datacount = 7
        datanames(1) = "SIE"
        datanames(2) = "U"
        datanames(3) = "V"
        datanames(4) = "RHO"
        datanames(5) = "P"
        datanames(6) = "Q"
        datanames(7) = "FLAG"

        ! Find which direction we are coupling based on the name of the state we have.
        call ESMF_StateGetName(statelist, statename, rc)
        if (trim(statename) .eq. "Coupler States Injector to FlowSolver") then

            ! Injector to FlowSolver
            call ESMF_StateGetData(statelist, "Injection Feedback", mysource, rc)
            call ESMF_StateGetData(statelist, "FlowSolver Input", mydest, rc)

        else if (trim(statename) .eq. "Coupler States FlowSolver to Injector") then

            ! FlowSolver to Injector
            call ESMF_StateGetData(statelist, "FlowSolver Feedback", mysource, rc)
            call ESMF_StateGetData(statelist, "Injection Input", mydest, rc)

        else

           print *, "Unexpected Statelist in Coupler Run routine, named ", trim(statename)
           rc = ESMF_FAILURE
           return
        endif

        ! Get layout from coupler component
        call ESMF_CplCompGet(comp, layout=cpllayout, rc=status)

        do i=1, datacount

           ! check isneeded flag here
           if (.not. ESMF_StateIsNeeded(mysource, datanames(i), rc)) then 
               !print *, "skipping field ", trim(datanames(i)), " not needed"
               cycle
           endif

           !print *, "processing field ", trim(datanames(i)), " as needed"
!BOP
! !DESCRIPTION:
! \subsubsection{Example of Route Usage:}
!
!   The following piece of code provides an example of calling Route
!   between two Fields in the Coupler Component.  
!   Unlike Regrid, which translates between
!   different Grids, Route translates between different Layouts on
!   the same Grid.   The first two lines get the Fields from the 
!   States, each corresponding to a different subcomponent.  One is
!   an Export State and the other is an Import State.
!
!\begin{verbatim}
           call ESMF_StateGetData(mysource, datanames(i), srcfield, rc=status)
           call ESMF_StateGetData(mydest, datanames(i), dstfield, rc=status)
!\end{verbatim}
!
!   The Route routine uses information contained in the Fields and the
!   Coupler Layout object to call the Communication routines to move the data.
!   Because many Fields may share the same Grid association, the same
!   routing information may be needed repeatedly.  Route information is cached 
!   so the precomputed information can be retained.  The following is an
!   example of a Field Route call:
!
!\begin{verbatim}
           call ESMF_FieldRedist(srcfield, dstfield, cpllayout, rc=status)
!\end{verbatim}
!EOP

        enddo
 
        rc = status

    end subroutine coupler_run


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  coupler_final - finalization routine

! !INTERFACE:
      subroutine coupler_final(comp, statelist, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: comp
      type(ESMF_State), intent(inout) :: statelist
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
!     \item[statelist]
!          Nested state object.
!     \item[clock] 
!          External clock.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI
        ! Local variables
        type(ESMF_State) :: state1, state2

        print *, "Coupler Final starting"
  
        ! Nothing to do here.
        rc = ESMF_SUCCESS
    
        print *, "Coupler Final returning"
   
    end subroutine coupler_final


    end module CouplerMod
    
    
