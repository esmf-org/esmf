! $Id: ESMF_RouteEx.F90,v 1.8 2004/02/18 22:17:57 nscollins Exp $
!
! Example/test code which creates a new field.

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to call Redist,
!  Halo, and Regrid on a Field.
! Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing various ways to call the communications
!   ! routines on a Field object
    program ESMF_RouteUseEx

    ! ESMF Framework module
    use ESMF_Mod

    implicit none
    
!   ! Local variables
    integer :: x, y, rc, mycell, finalrc, numdes
    integer :: i, j
    type(ESMF_Grid) :: srcgrid, dstgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: arraya, arrayb
    type(ESMF_DataMap) :: datamap
    type(ESMF_DELayout) :: layout1, layout2
    type(ESMF_RouteHandle) :: halo_rh, redist_rh, regrid_rh
    character (len = ESMF_MAXSTR) :: fname
    type(ESMF_IOSpec) :: iospec
    type(ESMF_Field) :: field1, field2, field3, field4
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1, f90ptr2
    real (ESMF_KIND_R8), dimension(2) :: mincoords

    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination grid with data on it, to use
!   !  in the Halo, Redist, and Regrid calls below.
 
    call ESMF_Initialize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    layout1 = ESMF_DELayoutCreate(rc=rc)
    call ESMF_DELayoutGetNumDEs(layout1, numdes, rc)
    layout2 = ESMF_DELayoutCreate((/ (i,i=0,numdes-1) /), 2, (/ numdes, 1 /), &
                                  (/ ESMF_COMMTYPE_MP, ESMF_COMMTYPE_MP /), rc)


    mincoords = (/ 0.0, 0.0 /)
    srcgrid = ESMF_GridCreateLogRectUniform(2, (/ 90, 180 /), mincoords, &
                                         layout=layout1, name="srcgrid", rc=rc)

    ! same grid coordinates, but different layout
    dstgrid = ESMF_GridCreateLogRectUniform(2, (/ 90, 180 /), mincoords, &
                                         layout=layout2, name="dstgrid", rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! allow for a halo width of 3
    allocate(f90ptr1(96,186))
    do j=1, 180
      do i=1, 90
        f90ptr1(i+3, j+3) = i*1000 + j
      enddo
    enddo

    arraya = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)  

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    field1 = ESMF_FieldCreate(srcgrid, arraya, horizRelloc=ESMF_CELL_CENTER, &
                                haloWidth=3, name="src pressure", rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArraySpecInit(arrayspec, 2, ESMF_DATA_REAL, ESMF_R8, rc)

    field2 = ESMF_FieldCreate(dstgrid, arrayspec, horizRelloc=ESMF_CELL_CENTER, &
                                                    name="dst pressure", rc=rc)

 
    ! fields all ready to go

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Calling Field Halo


    call ESMF_FieldHalo(field1, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    print *, "Halo example 1 returned"

    ! should transition over to:

    !call ESMF_FieldHaloStore(field1, routehandle=halo_rh, rc=rc)

    !if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! this should take the route handle and not the layout
    !call ESMF_FieldHalo(field1, halo_rh, rc=rc)

    !if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    !call ESMF_FieldHaloRelease(halo_rh, rc=rc)

    !if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    !print *, "Halo example 1a returned"

!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  Calling Field Redist


    call ESMF_FieldRedistStore(field1, field2, layout1, &
                                                routehandle=redist_rh, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! this should take the route handle and not the layout
    call ESMF_FieldRedist(field1, field2, layout1, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldRedistRelease(redist_rh, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    print *, "Redist example 2 returned"

!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  Calling Field Regrid


    !call ESMF_FieldRegridStore(field1, field2, layout1, &
    !                                            routehandle=regrid_rh, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    !call ESMF_FieldRegrid(field1, field2, regrid_rh, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    !call ESMF_FieldRegridRelease(regrid_rh, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    print *, "Regrid example 3 returned"

!-------------------------------------------------------------------------
!    ! Cleanup

    call ESMF_FieldDestroy(field1, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_FieldDestroy(field2, rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridDestroy(srcgrid)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_GridDestroy(dstgrid)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


    if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_RouteEx.F90"
    else
       print *, "FAIL: ESMF_RouteEx.F90"
    end if

    call ESMF_Finalize(rc=rc)

    end program ESMF_RouteUseEx
    
!\end{verbatim}
    
