! $Id: ESMF_StateCreate.F90,v 1.9 2004/06/12 04:18:05 cdeluca Exp $
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
    
!   ! Local macros to make this simpler until we've got a template.
#define P_START(string) print *, "------> Start: ", string
#define P_END(string)   print *, "------>   End: ", string
#define P_BLANK()       print *, " "
#define P_IN(string)    print *, "Calling ", string 
#define P_OUT(string)   print *, "Return  ", string, " (rc=", rc, ")"
#define P_OUT2(string, val)   print *, "Return  ", string, " (rc=", rc, "), value = ", val

!   ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    integer :: timestep
    integer, dimension(2) :: delist
    character(ESMF_MAXSTR) :: sname, bname, fname
    type(ESMF_Array) :: array1, array2
    type(ESMF_Field) :: field1, field2
    type(ESMF_Bundle) :: bundle1, bundle2, bundle3, qbundle
    type(ESMF_State) :: state1, state2, state3, state4, state5
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Quick Test - Create/Destroy an Empty State.
 
    call ESMF_Initialize(rc=rc)
 
    P_START("State Test 1: Empty State")

    sname = "Atmosphere Import"
    P_IN("ESMF_StateCreate")
    state1 = ESMF_StateCreate(sname, ESMF_STATE_IMPORT, rc=rc)  
    P_OUT2("ESMF_StateCreate", trim(sname))

    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state1, rc=rc)
    P_OUT("ESMF_StatePrint")

    P_IN("ESMF_StateDestroy")
    call ESMF_StateDestroy(state1, rc=rc)
    P_OUT("ESMF_StateDestroy")

    P_END("State Test 1: Empty State")
    P_BLANK()


!-------------------------------------------------------------------------
!   ! Test 2:
!   !
!   !  Quick Test - Create, Add Data, Print, Query, then Destroy a State.
 
    P_START("State Test 2: Export State")

    sname = "Ocean Export"
    P_IN("ESMF_StateCreate")
    state2 = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)  
    P_OUT2("ESMF_StateCreate", trim(sname))

    bname="Surface pressure"
    P_IN("ESMF_BundleCreate")
    bundle1 = ESMF_BundleCreate(name=bname, rc=rc)
    P_OUT2("ESMF_BundleCreate", trim(bname))

    P_IN("ESMF_StateAddData (Bundle)")
    call ESMF_StateAddData(state2, bundle1, rc=rc)
    P_OUT("ESMF_StateAddData (Bundle)")
    
    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state2, rc=rc)
    P_OUT("ESMF_StatePrint")

    bname="Temperature"
    P_IN("ESMF_BundleCreate")
    bundle2 = ESMF_BundleCreate(name=bname, rc=rc)
    P_OUT2("ESMF_BundleCreate", trim(bname))

    P_IN("ESMF_StateAddData (Bundle)")
    call ESMF_StateAddData(state2, bundle2, rc=rc)
    P_OUT("ESMF_StateAddData (Bundle)")
    
    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state2, rc=rc)
    P_OUT("ESMF_StatePrint")

    P_IN("ESMF_StateGetData")
    call ESMF_StateGetData(state2, "Surface pressure", qbundle, rc=rc)
    P_OUT("ESMF_StateGetData")

    P_IN("ESMF_BundlePrint")
    call ESMF_BundlePrint(qbundle, "", rc=rc)
    P_OUT("ESMF_BundlePrint")

    P_IN("ESMF_StateDestroy")
    call ESMF_StateDestroy(state2, rc=rc)
    P_OUT("ESMF_StateDestroy")

    P_IN("ESMF_BundleDestroy")
    call ESMF_BundleDestroy(bundle1, rc=rc)
    P_OUT("ESMF_BundleDestroy")

    P_IN("ESMF_BundleDestroy")
    call ESMF_BundleDestroy(bundle2, rc=rc)
    P_OUT("ESMF_BundleDestroy")

    P_END("State Test 2: Export State")
    P_BLANK()


!-------------------------------------------------------------------------
!   ! Test 3:
!   !
!   !  Quick Test - Create, Add Placeholder, Query, then Destroy a State.
 
    P_START("State Test 3: Export State with Placeholder")

    sname = "Ocean Export"
    P_IN("ESMF_StateCreate")
    state3 = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)
    P_OUT2("ESMF_StateCreate", trim(sname))

    sname = "Downward wind"
    P_IN("ESMF_StateAddData (Name only)")
    call ESMF_StateAddData(state3, sname, rc=rc)
    P_OUT2("ESMF_StateAddData (Name only)", trim(sname))
    
    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state3, rc=rc)
    P_OUT("ESMF_StatePrint")

    ! save for next test, do not destroy yet

    P_END("State Test 3 finished")
    P_BLANK()


!-------------------------------------------------------------------------
!   ! Test 4:
!   !
!   !  Quick Test - Mark and query Needed flags
 
    P_START("State Test 4: Get/Set Needed flags in Export State")

    ! inherit state3 from test above

    sname = "Downward wind"
    P_IN("ESMF_StateSetNeeded")
    call ESMF_StateSetNeeded(state3, sname, ESMF_STATEITEM_NEEDED, rc=rc)
    P_OUT2("ESMF_StateSetNeeded", trim(sname))
    
    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state3, rc=rc)
    P_OUT("ESMF_StatePrint")

    if (ESMF_StateIsNeeded(state3, sname, rc=rc)) then
      P_OUT2("Data marked as needed", trim(sname))

      bname = sname
      P_IN("ESMF_BundleCreate")
      bundle2 = ESMF_BundleCreate(name=bname, rc=rc)
      P_OUT2("ESMF_BundleCreate", trim(bname))
      
      fname = "Downward wind field"
      P_IN("ESMF_FieldCreate")
      field1 = ESMF_FieldCreateNoData(fname, rc=rc)
      P_OUT2("ESMF_FieldCreate", trim(fname))

      P_IN("ESMF_BundleAddFields")
      call ESMF_BundleAddField(bundle2, field1, rc=rc) 
      P_OUT("ESMF_BundleAddFields")

      P_IN("ESMF_StateAddData (Bundle)")
      call ESMF_StateAddData(state3, bundle2, rc=rc)
      P_OUT("ESMF_StateAddData (Bundle)")
    else
      P_OUT2("Data marked as *NOT* needed", trim(sname))
    endif

    P_IN("ESMF_BundlePrint")
    call ESMF_StatePrint(state3, rc=rc)
    P_OUT("ESMF_BundlePrint")
    
    P_IN("ESMF_StateDestroy")
    call ESMF_StateDestroy(state3, rc=rc)
    P_OUT("ESMF_StateDestroy")

    P_IN("ESMF_BundleDestroy")
    call ESMF_BundleDestroy(bundle2, rc=rc)
    P_OUT("ESMF_BundleDestroy")

    P_IN("ESMF_FieldDestroy")
    call ESMF_FieldDestroy(field1, rc=rc)
    P_OUT("ESMF_FieldDestroy")

    P_END("State Test 4 finished")
    P_BLANK()



!-------------------------------------------------------------------------
!   ! Test 5:
!   !
!   !  Longer Test - Overwriting existing placeholders, especially
!   !    handling fields inside bundles.
 
    P_START("State Test 5: State with Multiple Placeholders")

    sname = "Sea Ice Export"
    P_IN("ESMF_StateCreate")
    state4 = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)
    P_OUT2("ESMF_StateCreate", trim(sname))

    sname = "Surface pressure"
    P_IN("StateAddData (Name only)")
    call ESMF_StateAddData(state4, sname, rc=rc)
    P_OUT2("StateAddData (Name only)", trim(sname))
    
    P_IN("ESMF_StateSetNeeded")
    call ESMF_StateSetNeeded(state4, sname, ESMF_STATEITEM_NEEDED, rc=rc)
    P_OUT("ESMF_StateSetNeeded")
    
    sname = "Energy Flux"
    P_IN("StateAddData (Name only)")
    call ESMF_StateAddData(state4, sname, rc=rc)
    P_OUT2("StateAddData (Name only)", trim(sname))
    
    P_IN("ESMF_StateSetNeeded")
    call ESMF_StateSetNeeded(state4, sname, ESMF_STATEITEM_NEEDED, rc=rc)
    P_OUT("ESMF_StateSetNeeded")
    
    sname = "Humidity"
    P_IN("StateAddData (Name only)")
    call ESMF_StateAddData(state4, sname, rc=rc)
    P_OUT2("StateAddData (Name only)", trim(sname))
    
    P_IN("ESMF_StateSetNeeded")
    call ESMF_StateSetNeeded(state4, sname, ESMF_STATEITEM_NEEDED, rc=rc)
    P_OUT("ESMF_StateSetNeeded")
    
    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state4, rc=rc)
    P_OUT("ESMF_StatePrint")

    bname = "Collected quantities"
    P_IN("ESMF_BundleCreate")
    bundle2 = ESMF_BundleCreate(name=bname, rc=rc)
    P_OUT2("ESMF_BundleCreate", trim(bname))
      
    fname = "Surface pressure"
    P_IN("ESMF_FieldCreate")
    field1 = ESMF_FieldCreateNoData(fname, rc=rc)
    P_OUT2("ESMF_FieldCreate", trim(fname))

    P_IN("ESMF_BundleAddFields")
    call ESMF_BundleAddField(bundle2, field1, rc=rc) 
    P_OUT("ESMF_BundleAddFields")

    fname = "Energy Flux"
    P_IN("ESMF_FieldCreate")
    field2 = ESMF_FieldCreateNoData(fname, rc=rc)
    P_OUT2("ESMF_FieldCreate", trim(fname))

    P_IN("ESMF_BundleAddFields")
    call ESMF_BundleAddField(bundle2, field2, rc=rc) 
    P_OUT("ESMF_BundleAddFields")

    P_IN("ESMF_BundlePrint")
    call ESMF_BundlePrint(bundle2, "", rc=rc)
    P_OUT("ESMF_BundlePrint")


    P_IN("ESMF_StateAddData (Bundle)")
    call ESMF_StateAddData(state4, bundle2, rc=rc)
    P_OUT("ESMF_StateAddData (Bundle)")

    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state4, rc=rc)
    P_OUT("ESMF_StatePrint")
    
    P_IN("ESMF_StateDestroy")
    call ESMF_StateDestroy(state4, rc=rc)
    P_OUT("ESMF_StateDestroy")

    P_IN("ESMF_BundleDestroy")
    call ESMF_BundleDestroy(bundle2, rc=rc)
    P_OUT("ESMF_BundleDestroy")

    P_IN("ESMF_FieldDestroy")
    call ESMF_FieldDestroy(field1, rc=rc)
    P_OUT("ESMF_FieldDestroy")

    P_IN("ESMF_FieldDestroy")
    call ESMF_FieldDestroy(field2, rc=rc)
    P_OUT("ESMF_FieldDestroy")

    P_END("State Test 5 finished")
    P_BLANK()

!-------------------------------------------------------------------------
!   ! Test 6:
!   !
!   !  Quick Test - Create Nested States
 
    P_START("State Test 6: Nested States")

    sname = "Coupler Statelist"
    P_IN("ESMF_StateCreate")
    state5 = ESMF_StateCreate(sname, ESMF_STATE_LIST, rc=rc)  
    P_OUT2("ESMF_StateCreate", trim(sname))

    sname = "Atmosphere Import"
    P_IN("ESMF_StateCreate")
    state1 = ESMF_StateCreate(sname, ESMF_STATE_IMPORT, rc=rc)  
    P_OUT2("ESMF_StateCreate", trim(sname))

    P_IN("ESMF_StateAddData (State)")
    call ESMF_StateAddData(state5, state1, rc=rc)
    P_OUT("ESMF_StateAddData (State)")

    sname = "Ocean Export"
    P_IN("ESMF_StateCreate")
    state2 = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)  
    P_OUT2("ESMF_StateCreate", trim(sname))

    P_IN("ESMF_StateAddData (State)")
    call ESMF_StateAddData(state5, state2, rc=rc)
    P_OUT("ESMF_StateAddData (State)")

    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state5, rc=rc)
    P_OUT("ESMF_StatePrint")

    P_IN("ESMF_StateDestroy")
    call ESMF_StateDestroy(state5, rc=rc)
    P_OUT("ESMF_StateDestroy")

    P_IN("ESMF_StateDestroy")
    call ESMF_StateDestroy(state1, rc=rc)
    P_OUT("ESMF_StateDestroy")

    P_IN("ESMF_StateDestroy")
    call ESMF_StateDestroy(state2, rc=rc)
    P_OUT("ESMF_StateDestroy")

    P_END("State Test 6: Nested States")
    P_BLANK()


!-------------------------------------------------------------------------
!   ! Test 7:
!   !
!   !  Quick Test - Add multiple arrays to a state
 
    P_START("State Test 7: Multiple Arrays in a State")

    P_IN("ESMF_ArrayCreate")
    array1 = ESMF_ArrayCreate(rank=3, kind=ESMF_I4, type=ESMF_DATA_INTEGER, &
                              counts=(/ 3,4,5 /), rc=rc)
    P_OUT("ESMF_ArrayCreate")

    P_IN("ESMF_ArrayCreate")
    array2 = ESMF_ArrayCreate(rank=2, kind=ESMF_R8, type=ESMF_DATA_REAL, &
                              counts=(/ 5,6 /), rc=rc)
    P_OUT("ESMF_ArrayCreate")

    sname = "Atmosphere Import"
    P_IN("ESMF_StateCreate")
    state1 = ESMF_StateCreate(sname, ESMF_STATE_IMPORT, rc=rc)  
    P_OUT2("ESMF_StateCreate", trim(sname))

    P_IN("ESMF_StateAddData (Array)")
    call ESMF_StateAddData(state1, array1, rc=rc)
    P_OUT("ESMF_StateAddData (Array)")
    
    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state1, rc=rc)
    P_OUT("ESMF_StatePrint")

    P_IN("ESMF_StateAddData (Array)")
    call ESMF_StateAddData(state1, array2, rc=rc)
    P_OUT("ESMF_StateAddData (Array)")
    
    P_IN("ESMF_StatePrint")
    call ESMF_StatePrint(state1, rc=rc)
    P_OUT("ESMF_StatePrint")

    P_IN("ESMF_StateDestroy")
    call ESMF_StateDestroy(state1, rc=rc)
    P_OUT("ESMF_StateDestroy")

    P_END("State Test 7 finished")
    P_BLANK()


    call ESMF_Finalize(rc)

    end program ESMF_StateCreateTest
    
!\end{verbatim}
    
