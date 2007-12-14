! $Id: ESMF_BundleCreateEx.F90,v 1.30.2.1 2007/12/14 20:25:30 svasquez Exp $
!
! Example/test code which creates a new bundle.

!-------------------------------------------------------------------------
!ESMF_EXremoveAMPLE        String used by test script to count examples.
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
    integer :: i, rc, fieldcount
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    !type(ESMF_FieldDataMap) :: datamap
    type(ESMF_DELayout) :: delayout
    type(ESMF_VM) :: vm
    character (len = ESMF_MAXSTR) :: bname1, fname1, fname2
    type(ESMF_Field) :: field(10), returnedfield1, returnedfield2, simplefield
    type(ESMF_Bundle) :: bundle1, bundle2, bundle3
  !real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr1, f90ptr2
    integer :: counts(2)
    real(ESMF_KIND_R8) :: min_coord(2), max_coord(2)
!\end{verbatim}
!EOP

    integer :: finalrc
    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
    ! Initialize framework
    call ESMF_Initialize(rc=rc)
    call ESMF_VMGetGlobal(vm, rc)
!EOC
    
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC

!   !  Create several Fields and add them to a new Bundle.
 
!    counts = (/ 100, 200 /)
!    min_coord = (/  0.0,  0.0 /)
!    max_coord = (/ 50.0, 60.0 /)
    delayout = ESMF_DELayoutCreate(vm, rc=rc)
    grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/100,200/), &
                                  regDecomp=(/2,2/), name="atmgrid", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/100,200/), &
!                                  regDecomp=(/2,2/), rc=rc)
!    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!
!    coordX = ESMF_ArrayCreate(farray=fa_x, distgrid=distgrid, rc=rc)
!    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!    call ESMF_ArrayPrint(coordX, rc=rc)
!    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    field(1) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="pressure", rc=rc)
!EOC
    
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    field(2) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="temperature", rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    field(3) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="heat flux", rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    bundle1 = ESMF_BundleCreate(3, field, name="atmosphere data", rc=rc)

    print *, "Bundle example 1 returned"
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
!-------------------------------------------------------------------------
!   !  Create an empty Bundle and then add a single field to it.


    simplefield = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, name="rh", rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    bundle2 = ESMF_BundleCreate(name="time step 1", rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
    
!BOC
    call ESMF_BundleAddField(bundle2, simplefield, rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_BundleGet(bundle2, fieldCount=fieldcount, rc=rc)

    print *, "Bundle example 2 returned, fieldcount =", fieldcount
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
!-------------------------------------------------------------------------
!   !  Create an empty Bundle and then add multiple fields to it.


    bundle3 = ESMF_BundleCreate(name="southern hemisphere", rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_BundleAddField(bundle3, 3, field, rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_BundleGet(bundle3, fieldCount=fieldcount, rc=rc)

    print *, "Bundle example 3 returned, fieldcount =", fieldcount
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
!-------------------------------------------------------------------------
!   !  Get a Field back from a Bundle, first by name and then by index.
!   !  Also get the Bundle name.

    call ESMF_BundleGetField(bundle1, "pressure", returnedfield1, rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldGet(returnedfield1, name=fname1, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_BundleGetField(bundle1, 2, returnedfield2, rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_FieldGet(returnedfield2, name=fname2, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_BundleGet(bundle1, name=bname1, rc=rc)

    print *, "Bundle example 4 returned, field names = ", &
                   trim(fname1), ", ", trim(fname2)
    print *, "Bundle name = ", trim(bname1)
!EOC
 
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
!-------------------------------------------------------------------------

     call ESMF_BundleDestroy(bundle1, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     call ESMF_BundleDestroy(bundle2, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     call ESMF_BundleDestroy(bundle3, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     do i=1, 3
         call ESMF_FieldDestroy(field(i),rc=rc)
!EOC

         if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
     enddo

     call ESMF_FieldDestroy(simplefield, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_BundleCreateEx.F90"
    else
       print *, "FAIL: ESMF_BundleCreateEx.F90"
    end if

      call ESMF_Finalize(rc=rc)

!BOC
     end program ESMF_BundleCreateEx
!EOC
    
