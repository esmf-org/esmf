! $Id: ESMF_StateEx.F90,v 1.1 2003/02/03 17:09:51 nscollins Exp $
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

    program ESMF_StateCreateExample
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by States
    use ESMF_IOMod
    use ESMF_ArrayMod
    use ESMF_FieldMod
    use ESMF_BundleMod
    use ESMF_StateMod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    integer :: timestep
    integer, dimension(2) :: delist
    type(ESMF_StateDataNeeded) :: isneeded
    character(ESMF_MAXSTR) :: compname, statename, bundlename, fieldname
    type(ESMF_Array) :: array1
    type(ESMF_Field) :: field1
    type(ESMF_Bundle) :: bundle1, bundle2
    type(ESMF_State) :: state1, state2, state3, state4
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   ! Creation of a state, might also be from a query of a component.
!   !
 
    print *, "State Example 1: Import State"

!   ! This will probably happen inside the component code.
    compname = "Atmosphere"
    state1 = ESMF_StateCreate(compname, ESMF_STATEIMPORT, rc)  
    print *, "State Create returned, name = ", trim(compname)

    ! Data would be added here and the State reused inside the inner
    !  time loop of a sequential application.

    ! The user code will probably get a handle to the state by querying
    ! the component:
    ! call ESMF_CompQueryState(comp, ESMF_COMP_IMPORTSTATE, state1, rc)
   

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

    statename = "Downward wind"
    call ESMF_StateAddNameOnly(state3, statename, rc)
    print *, "StateAddNameOnly returned", rc, " name = ", trim(statename)
    
    ! See next example for how this is used.

    print *, "State Example 3 finished"


!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  Mark and Query Needed flags, and add Bundle data
 
    print *, "State Example 4: Get/Set Needed flags in Export State"

    ! Given state3 from the previous example, the consuming Component
    ! is given an opportunity to mark which data items are needed.

    statename = "Downward wind"
    isneeded = ESMF_STATEDATAISNEEDED
    call ESMF_StateSetNeeded(state3, statename, isneeded, rc)
    print *, "StateSetNeeded returned", rc
    
    ! Back in the producing Component, it can check the state to see
    ! which data items are needed.

    call ESMF_StateGetNeeded(state3, statename, isneeded, rc)
    print *, "StateGetNeeded returned", rc

    ! this should work - the operator is overloaded by the State module.
    !if (isneeded .eq. ESMF_STATEDATAISNEEDED) then
    if (ESMF_needeq(isneeded, ESMF_STATEDATAISNEEDED)) then
      print *, "Data marked as needed", trim(statename)

      bundlename = statename
      bundle2 = ESMF_BundleCreate(bundlename, rc=rc)
      print *, "Bundle Create returned", rc, "name = ", trim(bundlename)
      
      fieldname = "Downward wind field"
      field1 = ESMF_FieldCreateNoData(fieldname, rc=rc)
      print *, "Field Create returned", rc, "name = ", trim(fieldname)

      call ESMF_BundleAddFields(bundle2, field1, rc) 
      print *, "Bundle AddField returned", rc

      call ESMF_StateAddData(state3, bundle1, rc)
      print *, "StateAddData returned", rc
    else
      print *, "Data marked as not needed", trim(statename)
    endif
    
    call ESMF_StateDestroy(state3, rc)
    print *, "State Destroy returned", rc

    print *, "State Example 4 finished"



    end program ESMF_StateCreateExample
    
!\end{verbatim}
    
