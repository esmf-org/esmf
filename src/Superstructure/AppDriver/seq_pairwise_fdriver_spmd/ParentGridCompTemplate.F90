! $Id: ParentGridCompTemplate.F90,v 1.5.4.3 2008/05/06 04:31:39 cdeluca Exp $
!
! Template code for a Gridded Component which creates 3 child Components:
!  two Gridded Components which perform a computation and a Coupler component
!  which mediates the data exchange between them.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  A template for a user-written Gridded Component which creates 3 child
!  Components:
!  two Gridded Components which perform a computation and a Coupler component
!  which mediates the data exchange between them.
!
!
!\begin{verbatim}

    module UserParentGridCompMod
    
    ! ESMF Framework module
    use ESMF_Mod
    
    ! User Component registration routines
    use UserGridComp1Mod, only : UserGrid1_SetServices => UserGrid_SetServices
    use UserGridComp2Mod, only : UserGrid2_SetServices => UserGrid_SetServices
    use UserCplCompMod, only : UserCpl_SetServices


    implicit none
    private
    
    public UserPComp_SetServices

    type(ESMF_GridComp), save :: comp1Grid, comp2Grid
    type(ESMF_CplComp), save :: compCoupler
    character(len=ESMF_MAXSTR), save :: gname1, gname2, cname
    type(ESMF_State), save :: G1imp, G1exp, G2imp, G2exp
    type(ESMF_State), save :: Cplimp, Cplexp

    contains

    subroutine UserPComp_SetServices(gcomp, rc)
       type(ESMF_GridComp) :: gcomp
       integer :: rc

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)

    end subroutine UserPComp_SetServices


    subroutine my_init(gcomp, importState, exportState, parentclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: parentclock
      integer :: rc
     
      type(ESMF_Grid) :: parentgrid

      call ESMF_LogWrite("Parent Gridded Component Initialize routine called",&
                          ESMF_LOG_INFO)

      ! Get the layout and grid associated with this component
      call ESMF_GridCompGet(gcomp, grid=parentgrid, rc=rc)

      ! Create the first child Gridded component
      gname1 = "ESMF Gridded Child Component 1"
      comp1Grid = ESMF_GridCompCreate(name=gname1, & 
                                         grid=parentgrid, rc=rc)

      ! Create the second child Gridded component
      gname2 = "ESMF Gridded Child Component 2"
      comp2Grid = ESMF_GridCompCreate(name=gname2, &
                                         grid=parentgrid, rc=rc)

      ! Create the Coupler component
      cname = "ESMF Coupler Component"
      compCoupler = ESMF_CplCompCreate(name=cname, rc=rc)

      call ESMF_LogWrite("Component Creates finished", ESMF_LOG_INFO)

      ! Now call the SetServices routine for each so they can register their
      ! subroutines for Init, Run, and Finalize
      call ESMF_GridCompSetServices(comp1Grid, UserGrid1_SetServices, rc)
      call ESMF_GridCompSetServices(comp2Grid, UserGrid2_SetServices, rc)
      call ESMF_CplCompSetServices(compCoupler, UserCpl_SetServices, rc)


      ! Now create Import and Export State objects in order to pass data
      ! between the Coupler and the Gridded Components
      G1imp = ESMF_StateCreate("GComp1 Import", ESMF_STATE_IMPORT)
      G1exp = ESMF_StateCreate("GComp1 Export", ESMF_STATE_EXPORT)

      G2imp = ESMF_StateCreate("GComp2 Import", ESMF_STATE_IMPORT)
      G2exp = ESMF_StateCreate("GComp2 Export", ESMF_STATE_EXPORT)

      Cplimp = ESMF_StateCreate("Coupler Import", ESMF_STATE_IMPORT)
      Cplexp = ESMF_StateCreate("Coupler Export", ESMF_STATE_EXPORT)
      call ESMF_StateAdd(Cplimp, G1imp, rc=rc)
      call ESMF_StateAdd(Cplimp, G2imp, rc=rc)
      call ESMF_StateAdd(Cplexp, G1exp, rc=rc)
      call ESMF_StateAdd(Cplexp, G2exp, rc=rc)

      ! Now give each of the subcomponents a chance to initialize themselves.
      call ESMF_GridCompInitialize(comp1Grid, G1imp, G1exp, parentclock, rc=rc)
      call ESMF_GridCompInitialize(comp2Grid, G2imp, G2exp, parentclock, rc=rc)

      call ESMF_CplCompInitialize(compCoupler, Cplimp, Cplexp, parentclock, rc=rc)

      call ESMF_LogWrite("Parent Component Initialize finished", ESMF_LOG_INFO)
      rc=ESMF_SUCCESS

    end subroutine my_init


    subroutine my_run(gcomp, importState, exportState, parentclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: parentclock
      integer :: rc

     
      call ESMF_LogWrite("Parent Gridded Component Run routine called", ESMF_LOG_INFO)

      ! Now run the subcomponents
      call ESMF_GridCompRun(comp1Grid, G1imp, G1exp, parentclock, rc=rc)
      call ESMF_CplCompRun(compCoupler, G1exp, G2imp, parentclock, rc=rc)
      call ESMF_GridCompRun(comp2Grid, G2imp, G2exp, parentclock, rc=rc)


      call ESMF_LogWrite("Parent Component Run finished", ESMF_LOG_INFO)
      rc=ESMF_SUCCESS

    end subroutine my_run


    subroutine my_final(gcomp, importState, exportState, parentclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: parentclock
      integer :: rc
     
      call ESMF_LogWrite("Parent Gridded Component Finalize routine called", ESMF_LOG_INFO)

      ! Give each of the subcomponents a chance to finalize themselves.
      call ESMF_GridCompFinalize(comp1Grid, G1imp, G1exp, parentclock, rc=rc)
      call ESMF_GridCompFinalize(comp2Grid, G2imp, G2exp, parentclock, rc=rc)

      call ESMF_CplCompFinalize(compCoupler, G1exp, G2imp, parentclock, rc=rc)

      ! Now remove the Components to free up their resources
      call ESMF_GridCompDestroy(comp1Grid, rc)
      call ESMF_GridCompDestroy(comp2Grid, rc)
      call ESMF_CplCompDestroy(compCoupler, rc)

      call ESMF_LogWrite( "Parent Gridded Component Finalize routine finished", ESMF_LOG_INFO)
      rc=ESMF_SUCCESS

    end subroutine my_final

    end module UserParentGridCompMod

!\end{verbatim}
    
