! $Id: ESMF_StateEx.F90,v 1.14 2004/06/12 04:18:05 cdeluca Exp $


!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create {\tt States}.
! Also see the Programming Model section of this document.
!
!EOP
!BOC
!
! Example code showing various ways of creating States.

    program ESMF_StateExample

!   ! ESMF Framework module
    use ESMF_Mod
    implicit none
!   ! Local variables
    integer :: x, y, rc
    character(ESMF_MAXSTR) :: compname, statename, bundlename, dataname
    type(ESMF_Field) :: field1
    type(ESMF_Bundle) :: bundle1, bundle2
    type(ESMF_State) :: state1, state2, state3, state4
!EOC
    integer :: finalrc
    finalrc = ESMF_SUCCESS
!BOC
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   ! Creation of a state, might also be from a query of a component.
!   !
    call ESMF_Initialize(rc=rc)
 
    print *, "State Example 1: Import State"

!   ! This will probably be called from inside the Component Init code
    compname = "Atmosphere"
    state1 = ESMF_StateCreate(compname, statetype=ESMF_STATE_IMPORT, rc=rc)  
    print *, "State Create returned, name = ", trim(compname)

    ! Data would be added here and the State reused inside the run
    !  routine of a sequential application.

    print *, "State Example 1 finished"
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  Create, Add Data, Query, then Destroy a State.
 
    print *, "State Example 2: Export State"

    compname = "Ocean"
    state2 = ESMF_StateCreate(compname, statetype=ESMF_STATE_EXPORT, rc=rc)  

    print *, "State Create returned, name = ", trim(compname)
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    bundlename = "Temperature"
    bundle1 = ESMF_BundleCreate(name=bundlename, rc=rc)
    print *, "Bundle Create returned", rc
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    call ESMF_StateAddData(state2, bundle1, rc)
    print *, "StateAddData returned", rc
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    ! Loop here, updating Bundle contents each time step
    call ESMF_StateDestroy(state2, rc)
    print *, "State Destroy returned", rc
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    call ESMF_BundleDestroy(bundle1, rc)
    print *, "Bundle Destroy returned", rc
    print *, "State Example 2 finished"
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  Create, Add Placeholder, Query, then Destroy a State.
!   !  This example applies to a Gridded Component which potentially
!   !  could create a large number of export items but it is unlikely
!   !  that any other component would require all of them.  This allows
!   !  the consuming component to mark those needed, and the producer
!   !  only has to fill the data items requested.
 
    print *, "State Example 3: Export State with Placeholder"

    ! The producing Component creates the menu of data items available.
    compname = "Ocean"
    state3 = ESMF_StateCreate(compname, statetype=ESMF_STATE_EXPORT, rc=rc)  
    print *, "State Create returned", rc, " name = ", trim(compname)
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    dataname = "Downward wind"
    call ESMF_StateAddData(state3, dataname, rc)
    print *, "StateAddData returned", rc, " name = ", trim(dataname)
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    dataname = "Humidity"
    call ESMF_StateAddData(state3, dataname, rc)
    print *, "StateAddData returned", rc, " name = ", trim(dataname)

    ! See next example for how this is used.

    print *, "State Example 3 finished"
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  Mark Needed Flag
 
    print *, "State Example 4: Get/Set Needed flags in Export State"

    ! Given state3 from the previous example, the Coupler or Application
    ! is given an opportunity to mark which data items are needed.

    dataname = "Downward wind"
    call ESMF_StateSetNeeded(state3, dataname, ESMF_STATEITEM_NEEDED, rc)
    print *, "StateSetNeeded returned", rc
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
!-------------------------------------------------------------------------
!   ! Example 5:
!   !
!   !  Query Needed flags, and add Bundle data
 
    print *, "State Example 5: Get/Set Needed flags in Export State, continued"

    ! Given state3 from the previous examples, the producing Component
    ! can check the state to see what data items are required.

    dataname = "Downward wind"
    if (ESMF_StateIsNeeded(state3, dataname, rc)) then
!EOC
      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if
!BOC
      bundlename = dataname
      bundle2 = ESMF_BundleCreate(name=bundlename, rc=rc)
      print *, "Bundle Create returned", rc, "name = ", trim(bundlename)
!EOC
      if (rc.NE.ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
      end if
!BOC
      call ESMF_StateAddData(state3, bundle2, rc)
      print *, "StateAddData returned", rc
!EOC
      if (rc.NE.ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
      end if
!BOC
    else
      print *, "Data marked as not needed", trim(dataname)
    endif
    call ESMF_StateDestroy(state3, rc)
    print *, "State Destroy returned", rc
    print *, "State Example 5 finished"

    call ESMF_Finalize(rc)
!-------------------------------------------------------------------------
!   ! Similar flags exist for "Ready" and for "Valid" to mark each data
!   !  item as ready or having been validated, to help synchronize data
!   !  exchange between Components and Couplers.  Also "Required for 
!   !  Restart".
!-------------------------------------------------------------------------
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_StateExample.F90"
    else
        print *, "FAIL: ESMF_StateExample.F90"
    end if
!BOC
    end program ESMF_StateExample
!EOC
    
