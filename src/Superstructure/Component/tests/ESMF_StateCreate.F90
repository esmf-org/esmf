! $Id: ESMF_StateCreate.F90,v 1.5 2003/02/11 23:41:42 nscollins Exp $
!
! Test code which creates a new State.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for State Create code.
!
!
!\begin{verbatim}

    program ESMF_StateCreateTest
    
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
    character(ESMF_MAXSTR) :: cname, sname, bname, fname
    type(ESMF_Array) :: array1
    type(ESMF_Field) :: field1
    type(ESMF_Bundle) :: bundle1, bundle2, bundle3
    type(ESMF_State) :: state1, state2, state3, state4
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Quick Test - Create/Destroy an Empty State.
 
    print *, "State Test 1: Empty State"

    cname = "Atmosphere"
    state1 = ESMF_StateCreate(cname, ESMF_STATEIMPORT, rc=rc)  
    print *, "State Create returned, name = ", trim(cname)

    call ESMF_StatePrint(state1, rc=rc)
    print *, "State Print returned", rc

    call ESMF_StateDestroy(state1, rc=rc)
    print *, "State Destroy returned", rc

    print *, "State Test 1 finished"
    print *, " "


!-------------------------------------------------------------------------
!   ! Test 2:
!   !
!   !  Quick Test - Create, Add Data, Print, Query, then Destroy a State.
 
    print *, "State Test 2: Export State"

    cname = "Ocean"
    state2 = ESMF_StateCreate(cname, ESMF_STATEEXPORT, rc=rc)  
    print *, "State Create returned, name = ", trim(cname)

    bundle1 = ESMF_BundleCreate(name="Surface pressure", rc=rc)
    print *, "Bundle Create returned", rc

    call ESMF_StateAddData(state2, bundle1, rc=rc)
    print *, "StateAddData returned", rc
    
    call ESMF_StatePrint(state2, rc=rc)
    print *, "State Print returned", rc

    call ESMF_StateGetData(state2, "Surface pressure", bundle3, rc=rc)
    print *, "State GetData returned", rc

    call ESMF_BundlePrint(bundle3, "", rc=rc)
    print *, "Bundle Print returned", rc

    call ESMF_StateDestroy(state2, rc=rc)
    print *, "State Destroy returned", rc

    call ESMF_BundleDestroy(bundle1, rc=rc)
    print *, "Bundle Destroy returned", rc

    print *, "State Test 2 finished"
    print *, " "


!-------------------------------------------------------------------------
!   ! Test 3:
!   !
!   !  Quick Test - Create, Add Placeholder, Query, then Destroy a State.
 
    print *, "State Test 3: Export State with Placeholder"

    cname = "Ocean"
    state3 = ESMF_StateCreate(cname, ESMF_STATEEXPORT, rc=rc)
    print *, "State Create returned, name = ", trim(cname)

    sname = "Downward wind"
    call ESMF_StateAddData(state3, sname, rc=rc)
    print *, "StateAddData (name only) returned", rc
    
    call ESMF_StatePrint(state3, rc=rc)
    print *, "State Print returned", rc

    ! save for next test, do not destroy yet

    print *, "State Test 3 finished"
    print *, " "


!-------------------------------------------------------------------------
!   ! Test 4:
!   !
!   !  Quick Test - Mark and query Needed flags
 
    print *, "State Test 4: Get/Set Needed flags in Export State"

    ! inherit state3 from test above

    sname = "Downward wind"
    call ESMF_StateSetNeeded(state3, sname, ESMF_STATEDATAISNEEDED, rc=rc)
    print *, "StateSetNeeded returned", rc
    
    call ESMF_StatePrint(state3, rc=rc)
    print *, "State Print returned", rc

    if (ESMF_StateIsNeeded(state3, sname, rc=rc)) then
      print *, "Data marked as needed", trim(sname)

      bname = sname
      bundle2 = ESMF_BundleCreate(bname, rc=rc)
      print *, "Bundle Create returned", rc, "name =", trim(bname)
      
      fname = "Downward wind field"
      field1 = ESMF_FieldCreateNoData(fname, rc=rc)
      print *, "Field Create returned", rc, "name =", trim(fname)

      call ESMF_BundleAddFields(bundle2, field1, rc=rc) 
      print *, "Bundle AddField returned", rc

      call ESMF_StateAddData(state3, bundle1, rc=rc)
      print *, "StateAddData returned", rc
    else
      print *, "Data marked as not needed", trim(sname)
    endif
    
    call ESMF_StateDestroy(state3, rc=rc)
    print *, "State Destroy returned", rc

    print *, "State Test 4 finished"
    print *, " "



    end program ESMF_StateCreateTest
    
!\end{verbatim}
    
