! $Id: ESMF_StateCreateEx.F90,v 1.2 2003/01/29 23:50:31 nscollins Exp $
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
    character(ESMF_MAXSTR) :: cname, sname, bname, fname
    type(ESMF_Array) :: array1
    type(ESMF_Field) :: field1
    type(ESMF_Bundle) :: bundle1, bundle2
    type(ESMF_State) :: state1, state2, state3, state4
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Create/Destroy an Empty State.
 
    print *, "State Example 1: Empty State"

    cname = "Atmosphere"
    state1 = ESMF_StateCreate(cname, ESMF_STATEIMPORT, rc)  
    print *, "State Create returned, name = ", trim(cname)

    call ESMF_StatePrint(state1, rc=rc)
    print *, "State Print returned", rc

    call ESMF_StateDestroy(state1, rc)
    print *, "State Destroy returned", rc

    print *, "State Example 1 finished"


!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  Create, Add Data, Query, then Destroy a State.
 
    print *, "State Example 2: Export State"

    cname = "Ocean"
    state2 = ESMF_StateCreate(cname, ESMF_STATEEXPORT, rc)  
    print *, "State Create returned, name = ", trim(cname)

    bundle1 = ESMF_BundleCreate(rc=rc)
    print *, "Bundle Create returned", rc

    call ESMF_StateAddData(state2, bundle1, rc)
    print *, "StateAddData returned", rc
    
    call ESMF_StatePrint(state2, rc=rc)
    print *, "State Print returned", rc

    call ESMF_StateDestroy(state2, rc)
    print *, "State Destroy returned", rc

    call ESMF_BundleDestroy(bundle1, rc)
    print *, "Bundle Destroy returned", rc

    print *, "State Example 2 finished"


!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  Create, Add Placeholder, Query, then Destroy a State.
 
    print *, "State Example 3: Export State with Placeholder"

    cname = "Ocean"
    state3 = ESMF_StateCreate(cname, ESMF_STATEEXPORT, rc)  
    print *, "State Create returned", rc, " name = ", trim(cname)

    sname = "Downward wind"
    call ESMF_StateAddNameOnly(state3, sname, rc)
    print *, "StateAddNameOnly returned", rc, " name = ", trim(sname)
    
    call ESMF_StatePrint(state3, rc=rc)
    print *, "State Print returned", rc

    ! save for next test, do not destroy yet

    print *, "State Example 3 finished"


!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  Mark and Query Needed flags, and add Bundle data
 
    print *, "State Example 4: Get/Set Needed flags in Export State"

    ! inherit state3 from test above

    sname = "Downward wind"
    isneeded = ESMF_STATEDATAISNEEDED
    call ESMF_StateSetNeeded(state3, sname, isneeded, rc)
    print *, "StateSetNeeded returned", rc
    
    call ESMF_StatePrint(state3, rc=rc)
    print *, "State Print returned", rc

    call ESMF_StateGetNeeded(state3, sname, isneeded, rc)
    print *, "StateGetNeeded returned", rc

    ! this should work - the operator is overloaded by the State module.
    !if (isneeded .eq. ESMF_STATEDATAISNEEDED) then
    if (ESMF_needeq(isneeded, ESMF_STATEDATAISNEEDED)) then
      print *, "Data marked as needed", trim(sname)

      bname = sname
      bundle2 = ESMF_BundleCreate(bname, rc=rc)
      print *, "Bundle Create returned", rc, "name = ", trim(bname)
      
      fname = "Downward wind field"
      field1 = ESMF_FieldCreateNoData(fname, rc=rc)
      print *, "Field Create returned", rc, "name = ", trim(fname)

      call ESMF_BundleAddFields(bundle2, field1, rc) 
      print *, "Bundle AddField returned", rc

      call ESMF_StateAddData(state3, bundle1, rc)
      print *, "StateAddData returned", rc
    else
      print *, "Data marked as not needed", trim(sname)
    endif
    
    call ESMF_StateDestroy(state3, rc)
    print *, "State Destroy returned", rc

    print *, "State Example 4 finished"



    end program ESMF_StateCreateExample
    
!\end{verbatim}
    
