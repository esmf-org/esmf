! $Id: ESMF_BundleCreateEx.F90,v 1.5 2004/01/07 22:09:42 svasquez Exp $
!
! Example/test code which creates a new bundle.

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Bundles.
!
!\begin{verbatim}

!   ! Example program showing various ways to create a Bundle object.

    program ESMF_BundleCreateEx

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
!   ! Local variables
    integer :: i, x, y, rc, mycell, fieldcount
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: arraya, arrayb
    type(ESMF_DataMap) :: datamap
    type(ESMF_RelLoc) :: relativelocation
    character (len = ESMF_MAXSTR) :: bname1, bname2, fname1, fname2
    type(ESMF_IOSpec) :: iospec
    type(ESMF_Field) :: field(10), returnedfield1, returnedfield2, simplefield
    type(ESMF_Bundle) :: bundle1, bundle2, bundle3, bundle4
    real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr1, f90ptr2
    integer :: i, x, y, rc, mycell, fieldcount, finalrc
!\end{verbatim}
!EOP

    integer :: finalrc
    finalrc = ESMF_SUCCESS
        
!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------
!   !  Create several empty Fields and add them to a new Bundle.
 
    field(1) = ESMF_FieldCreateNoData(name="pressure", rc=rc)
!\end{verbatim}
!EOP
    
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    field(2) = ESMF_FieldCreateNoData(name="temperature", rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    field(3) = ESMF_FieldCreateNoData(name="heat flux", rc=rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    bundle1 = ESMF_BundleCreate(3, field, name="atmosphere data", rc=rc)

    print *, "Bundle example 1 returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------
!   !  Create an empty Bundle and then add a single field to it.


    simplefield = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                                    name="rh", rc=rc)
!\end{verbatim}
!EOP


    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    bundle2 = ESMF_BundleCreate(name="time step 1", rc=rc);
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
    
!BOP
!\begin{verbatim}
    call ESMF_BundleAddFields(bundle2, simplefield, rc);
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_BundleGetFieldCount(bundle2, fieldcount, rc);

    print *, "Bundle example 2 returned, fieldcount =", fieldcount
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------
!   !  Create an empty Bundle and then add multiple fields to it.


    bundle3 = ESMF_BundleCreate(name="southern hemisphere", rc=rc);
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_BundleAddFields(bundle3, 3, field, rc);
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_BundleGetFieldCount(bundle3, fieldcount, rc);

    print *, "Bundle example 3 returned, fieldcount =", fieldcount
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------
!   !  Get a Field back from a Bundle, first by name and then by index.
!   !  Also get the Bundle name.

    call ESMF_BundleGetFields(bundle1, "pressure", returnedfield1, rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_FieldGetName(returnedfield1, fname1, rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_BundleGetFields(bundle1, 2, returnedfield2, rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_FieldGetName(returnedfield2, fname2, rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_BundleGetName(bundle1, bname1, rc)

    print *, "Bundle example 4 returned, field names = ", &
                   trim(fname1), ", ", trim(fname2)
    print *, "Bundle name = ", trim(bname1)
!\end{verbatim}
!EOP
 
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
!-------------------------------------------------------------------------

     call ESMF_BundleDestroy(bundle1, rc=rc)
!\end{verbatim}
!EOP

     if (rc.NE.ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
     end if

!BOP
!\begin{verbatim}
     call ESMF_BundleDestroy(bundle2, rc=rc)
!\end{verbatim}
!EOP

     if (rc.NE.ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
     end if

!BOP
!\begin{verbatim}
     call ESMF_BundleDestroy(bundle3, rc=rc)
!\end{verbatim}
!EOP

     if (rc.NE.ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
     end if

!BOP
!\begin{verbatim}
     call ESMF_BundleDestroy(bundle4, rc=rc)
!\end{verbatim}
!EOP

     if (rc.NE.ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
     end if

!BOP
!\begin{verbatim}
     do i=1, 3
         call ESMF_FieldDestroy(field(i),rc=rc)
!\end{verbatim}
!EOP

         if (rc.NE.ESMF_SUCCESS) then
             finalrc = ESMF_FAILURE
    	 end if

!BOP
!\begin{verbatim}
     enddo

     call ESMF_FieldDestroy(simplefield, rc=rc)
!\end{verbatim}
!EOP

     if (rc.NE.ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
     end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_BundleCreateEx.F90"
     else
        print *, "FAIL: ESMF_BundleCreateEx.F90"
     end if

!BOP
!\begin{verbatim}
     end program ESMF_BundleCreateEx
!\end{verbatim}
!EOP
    
