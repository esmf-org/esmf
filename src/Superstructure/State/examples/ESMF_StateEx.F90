! $Id: ESMF_StateEx.F90,v 1.1 2003/10/22 20:09:41 cdeluca Exp $
!
! Example code for creating States.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Examples of creating {\tt States}.  
!
!
!\begin{verbatim}

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
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   ! Creation of a state, might also be from a query of a component.
!   !
 
    print *, "State Example 1: Import State"

!   ! This will probably be called from inside the Component Init code
    compname = "Atmosphere"
    state1 = ESMF_StateCreate(compname, ESMF_STATEIMPORT, rc)  
    print *, "State Create returned, name = ", trim(compname)

    ! Data would be added here and the State reused inside the run
    !  routine of a sequential application.

    print *, "State Example 1 finished"


!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  Create, Add Data, Query, then Destroy a State.
 
    print *, "State Example 2: Export State"

    compname = "Ocean"
    state2 = ESMF_StateCreate(compname, ESMF_STATEEXPORT, rc)  
    print *, "State Create returned, name = ", trim(compname)

    bundlename = "Temperature"
    bundle1 = ESMF_BundleCreate(bundlename, rc=rc)
    print *, "Bundle Create returned", rc

    call ESMF_StateAddData(state2, bundle1, rc)
    print *, "StateAddData returned", rc
    
    ! Loop here, updating Bundle contents each time step

    call ESMF_StateDestroy(state2, rc)
    print *, "State Destroy returned", rc

    call ESMF_BundleDestroy(bundle1, rc)
    print *, "Bundle Destroy returned", rc

    print *, "State Example 2 finished"


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
    state3 = ESMF_StateCreate(compname, ESMF_STATEEXPORT, rc)  
    print *, "State Create returned", rc, " name = ", trim(compname)

    dataname = "Downward wind"
    call ESMF_StateAddData(state3, dataname, rc)
    print *, "StateAddData returned", rc, " name = ", trim(dataname)
    
    dataname = "Humidity"
    call ESMF_StateAddData(state3, dataname, rc)
    print *, "StateAddData returned", rc, " name = ", trim(dataname)
    
    ! See next example for how this is used.

    print *, "State Example 3 finished"


!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  Mark Needed Flag
 
    print *, "State Example 4: Get/Set Needed flags in Export State"

    ! Given state3 from the previous example, the Coupler or Application
    ! is given an opportunity to mark which data items are needed.

    dataname = "Downward wind"
    call ESMF_StateSetNeeded(state3, dataname, ESMF_STATEDATAISNEEDED, rc)
    print *, "StateSetNeeded returned", rc
    
!-------------------------------------------------------------------------
!   ! Example 5:
!   !
!   !  Query Needed flags, and add Bundle data
 
    print *, "State Example 5: Get/Set Needed flags in Export State, continued"

    ! Given state3 from the previous examples, the producing Component
    ! can check the state to see what data items are required.

    dataname = "Downward wind"
    if (ESMF_StateIsNeeded(state3, dataname, rc)) then

      print *, "Data marked as needed", trim(dataname)

      bundlename = dataname
      bundle2 = ESMF_BundleCreate(bundlename, rc=rc)
      print *, "Bundle Create returned", rc, "name = ", trim(bundlename)
      
      call ESMF_StateAddData(state3, bundle2, rc)
      print *, "StateAddData returned", rc
    else
      print *, "Data marked as not needed", trim(statename)
    endif
    
    call ESMF_StateDestroy(state3, rc)
    print *, "State Destroy returned", rc

    print *, "State Example 5 finished"

!-------------------------------------------------------------------------
!   ! Similar flags exist for "Ready" and for "Valid" to mark each data
!   !  item as ready or having been validated, to help synchronize data
!   !  exchange between Components and Couplers.  Also "Required for 
!   !  Restart".
!-------------------------------------------------------------------------


    end program ESMF_StateExample
    
!\end{verbatim}
    
