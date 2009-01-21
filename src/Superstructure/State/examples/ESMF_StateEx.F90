! $Id: ESMF_StateEx.F90,v 1.23.2.5 2009/01/21 21:25:25 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_StateEx

!------------------------------------------------------------------------------
!ESMF_EXremoveAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_StateEx - State creation and operation
!
! !DESCRIPTION:
!
! This program shows examples of State creation and manipulation
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    implicit none

    ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: statename, bundlename, dataname
    !type(ESMF_Field) :: field1
    type(ESMF_FieldBundle) :: bundle1, bundle2
    type(ESMF_State) :: state1, state2, state3
!EOC
    integer :: finalrc
    finalrc = ESMF_SUCCESS


    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Empty State Create}
!      
!  Creation of an empty {\tt ESMF\_State}, which will be added to later.
!EOE
 
    print *, "State Example 1: Import State"

    ! This will probably be called from inside the Component Init code
!BOC
    statename = "Atmosphere"
    state1 = ESMF_StateCreate(statename, statetype=ESMF_STATE_IMPORT, rc=rc)  
!EOC
    print *, "State Create returned, name = ", trim(statename)

    ! Data would be added here and the State reused inside the run
    !  routine of a sequential application.

    print *, "State Example 1 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Adding Items to a State}
!   
!  Creation of an empty {\tt ESMF\_State}, and adding an {\tt ESMF\_FieldBundle}
!  to it.  Note that the {\tt ESMF\_FieldBundle} does not get destroyed when
!  the {\tt ESMF\_State} is destroyed; the {\tt ESMF\_State} only contains
!  a reference to the objects it contains.  It also does not make a copy;
!  the original objects can be updated and code accessing them by using
!  the {\tt ESMF\_State} will see the updated version.
!EOE


    ! Example 2:
    !
    !  Create, Add Data, Query, then Destroy a State.
 
    print *, "State Example 2: Export State"

!BOC
    statename = "Ocean"
    state2 = ESMF_StateCreate(statename, statetype=ESMF_STATE_EXPORT, rc=rc)  
!EOC

    print *, "State Create returned, name = ", trim(statename)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    bundlename = "Temperature"
    bundle1 = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
    print *, "FieldBundle Create returned", rc
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_StateAdd(state2, bundle1, rc)
    print *, "StateAdd returned", rc
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Loop here, updating FieldBundle contents each time step

!BOC
    call ESMF_StateDestroy(state2, rc)
!EOC
    print *, "State Destroy returned", rc
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_FieldBundleDestroy(bundle1, rc)
!EOC
    print *, "FieldBundle Destroy returned", rc
    print *, "State Example 2 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Adding Placeholders to a State}
!   
! If a component could potentially produce a large number of optional
! items, one strategy is to add the names only of those objects to the
! {\tt ESMF\_State}.  Other components can call framework routines to
! set the {\tt ESMF\_NEEDED} flag to indicate they require that data.
! The original component can query this flag and then produce only the
! data what is required by another component.
!EOE

    print *, "State Example 3: Export State with Placeholder"

    ! The producing Component creates the menu of data items available.
!BOC
    statename = "Ocean"
    state3 = ESMF_StateCreate(statename, statetype=ESMF_STATE_EXPORT, rc=rc)  
!EOC
    print *, "State Create returned", rc, " name = ", trim(statename)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    dataname = "Downward wind"
    call ESMF_StateAdd(state3, dataname, rc)
!EOC
    print *, "StateAdd returned", rc, " name = ", trim(dataname)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    dataname = "Humidity"
    call ESMF_StateAdd(state3, dataname, rc)
!EOC
    print *, "StateAdd returned", rc, " name = ", trim(dataname)

    ! See next example for how this is used.

    print *, "State Example 3 finished"
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Marking an Item Needed}
!   
! How to set the {\tt NEEDED} state of an item.
!EOE

    print *, "State Example 4: Get/Set Needed flags in Export State"

    ! Given state3 from the previous example, the Coupler or Application
    ! is given an opportunity to mark which data items are needed.

!BOC
    dataname = "Downward wind"
    call ESMF_StateSetNeeded(state3, dataname, ESMF_NEEDED, rc)
!EOC
    print *, "StateSetNeeded returned", rc
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Creating a Needed Item}
!   
! Query an item for the {\tt NEEDED} status, and creating an item on demand.
! Similar flags exist for "Ready", "Valid", and "Required for Restart",
! to mark each data item as ready, having been validated, or needed if the
! application is to be checkpointed and restarted.  The flags are supported
! to help coordinate the data exchange between components.
!EOE

    !  Query Needed flags, and add FieldBundle data
 
    print *, "State Example 5: Get/Set Needed flags in Export State, continued"

    ! Given state3 from the previous examples, the producing Component
    ! can check the state to see what data items are required.

!BOC
    dataname = "Downward wind"
    if (ESMF_StateIsNeeded(state3, dataname, rc)) then
!EOC
      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
        bundlename = dataname
        bundle2 = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
!EOC
        print *, "FieldBundle Create returned", rc, "name = ", trim(bundlename)
        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
        call ESMF_StateAdd(state3, bundle2, rc)
!EOC
        print *, "StateAdd returned", rc
        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    else
        print *, "Data not marked as needed", trim(dataname)
    endif
!EOC
    call ESMF_StateDestroy(state3, rc)
    print *, "State Destroy returned", rc
    print *, "State Example 5 finished"

!-------------------------------------------------------------------------
!   ! Similar flags exist for "Ready" and for "Valid" to mark each data
!   !  item as ready or having been validated, to help synchronize data
!   !  exchange between Components and Couplers.  Also "Required for 
!   !  Restart".
!-------------------------------------------------------------------------
    call ESMF_Finalize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_StateExample.F90"
    else
        print *, "FAIL: ESMF_StateExample.F90"
    end if

    end program ESMF_StateEx
    
