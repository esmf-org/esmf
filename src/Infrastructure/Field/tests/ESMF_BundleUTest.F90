! $Id: ESMF_BundleUTest.F90,v 1.1 2003/03/10 21:54:22 cdeluca Exp $
!
! Example/test code which creates a new bundle.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Bundles.
! Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing various ways to create a Bundle object.
    program ESMF_BundleCreateEx
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! Other ESMF modules which are needed by Bundles
    use ESMF_IOMod
    use ESMF_ArrayMod
    use ESMF_DataMapMod
    use ESMF_GridMod
    use ESMF_FieldMod
    use ESMF_BundleMod
    
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
        
!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  Create several empty Fields and add them to a new Bundle.
 
    field(1) = ESMF_FieldCreateNoData(name="pressure", rc=rc)
    field(2) = ESMF_FieldCreateNoData(name="temperature", rc=rc)
    field(3) = ESMF_FieldCreateNoData(name="heat flux", rc=rc)

    bundle1 = ESMF_BundleCreate(3, field, name="atmosphere data", rc=rc)
   

    print *, "Bundle example 1 returned"

!-------------------------------------------------------------------------
!   ! Example 2:
!   !
!   !  Create an empty Bundle and then add a single field to it.


    simplefield = ESMF_FieldCreate(grid, arrayspec, ESMF_CELL_CENTER, &
                                    name="rh", rc=rc)

    bundle2 = ESMF_BundleCreate(name="time step 1", rc=rc);
    
    call ESMF_BundleAddFields(bundle2, simplefield, rc);

    call ESMF_BundleGetFieldCount(bundle2, fieldcount, rc);

    print *, "Bundle example 2 returned, fieldcount =", fieldcount

!-------------------------------------------------------------------------
!   ! Example 3:
!   !
!   !  Create an empty Bundle and then add multiple fields to it.


    bundle3 = ESMF_BundleCreate(name="southern hemisphere", rc=rc);
    
    call ESMF_BundleAddFields(bundle3, 3, field, rc);

    call ESMF_BundleGetFieldCount(bundle3, fieldcount, rc);

    print *, "Bundle example 3 returned, fieldcount =", fieldcount

!-------------------------------------------------------------------------
!   ! Example 4:
!   !
!   !  Get a Field back from a Bundle, first by name and then by index.
!   !  Also get the Bundle name.

    call ESMF_BundleGetFields(bundle1, "pressure", returnedfield1, rc)
    call ESMF_FieldGetName(returnedfield1, fname1, rc)

    call ESMF_BundleGetFields(bundle1, 2, returnedfield2, rc)
    call ESMF_FieldGetName(returnedfield2, fname2, rc)

    call ESMF_BundleGetName(bundle1, bname1, rc)
    print *, "Bundle example 4 returned, field names = ", &
                   trim(fname1), ", ", trim(fname2)
    print *, "Bundle name = ", trim(bname1)

!-------------------------------------------------------------------------

     call ESMF_BundleDestroy(bundle1)
     call ESMF_BundleDestroy(bundle2)
     call ESMF_BundleDestroy(bundle3)
     call ESMF_BundleDestroy(bundle4)

     do i=1, 3
         call ESMF_FieldDestroy(field(i))
     enddo
     call ESMF_FieldDestroy(simplefield)

     end program ESMF_BundleCreateEx
    
!\end{verbatim}
    
