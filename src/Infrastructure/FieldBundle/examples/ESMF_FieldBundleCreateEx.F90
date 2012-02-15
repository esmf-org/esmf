! $Id: ESMF_FieldBundleCreateEx.F90,v 1.20 2012/02/15 23:13:36 svasquez Exp $
!
! Example/test code which creates a new bundle.

!-------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new FieldBundles.
!
!\begin{verbatim}

!   ! Example program showing various ways to create a FieldBundle object.

program ESMF_FieldBundleCreateEx
#include "ESMF.h"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod

    implicit none
    
!   ! Local variables
    integer :: i, rc, fieldcount
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    character (len = ESMF_MAXSTR) :: bname1, fname1, fname2
    type(ESMF_Field) :: field(10), returnedfield1, returnedfield2
    type(ESMF_Field) :: simplefield
    type(ESMF_FieldBundle) :: bundle1, bundle2, bundle3
!\end{verbatim}
!EOP

    integer :: finalrc, result

    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_FieldBundleCreateEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
    ! Initialize framework
    call ESMF_Initialize(defaultlogfilename="FieldBundleCreateEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC
    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
!-------------------------------------------------------------------------
!   !  Create several Fields and add them to a new FieldBundle.
 
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/100,200/), &
                                  regDecomp=(/2,2/), name="atmgrid", rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC

    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    field(1) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="pressure", rc=rc)
!EOC
    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    field(2) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="temperature", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    field(3) = ESMF_FieldCreate(grid, arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="heat flux", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    bundle1 = ESMF_FieldBundleCreate(fieldList=field(1:3), &
				name="atmosphere data", rc=rc)

    print *, "FieldBundle example 1 returned"
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
!-------------------------------------------------------------------------
!   !  Create an empty FieldBundle and then add a single field to it.


    simplefield = ESMF_FieldCreate(grid, arrayspec, &
                  staggerloc=ESMF_STAGGERLOC_CENTER, name="rh", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    bundle2 = ESMF_FieldBundleCreate(name="time step 1", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOC
    call ESMF_FieldBundleAdd(bundle2, (/simplefield/), rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleGet(bundle2, fieldCount=fieldcount, rc=rc)

    print *, "FieldBundle example 2 returned, fieldcount =", fieldcount
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
!-------------------------------------------------------------------------
!   !  Create an empty FieldBundle and then add multiple fields to it.


    bundle3 = ESMF_FieldBundleCreate(name="southern hemisphere", rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleAdd(bundle3, field(1:3), rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleGet(bundle3, fieldCount=fieldcount, rc=rc)

    print *, "FieldBundle example 3 returned, fieldcount =", fieldcount
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
!-------------------------------------------------------------------------
!   !  Get a Field back from a FieldBundle, first by name and then by index.
!   !  Also get the FieldBundle name.

    call ESMF_FieldBundleGet(bundle1, "pressure", field=returnedfield1, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldGet(returnedfield1, name=fname1, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleGet(bundle1, 2, returnedfield2, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldGet(returnedfield2, name=fname2, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleGet(bundle1, name=bname1, rc=rc)

    print *, "FieldBundle example 4 returned, field names = ", &
                   trim(fname1), ", ", trim(fname2)
    print *, "FieldBundle name = ", trim(bname1)
!EOC
 
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
!-------------------------------------------------------------------------

     call ESMF_FieldBundleDestroy(bundle1, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
     call ESMF_FieldBundleDestroy(bundle2, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
     call ESMF_FieldBundleDestroy(bundle3, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
     do i=1, 3
         call ESMF_FieldDestroy(field(i),rc=rc)
!EOC

         if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
     enddo

     call ESMF_FieldDestroy(simplefield, rc=rc)
!EOC

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridDestroy(grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


    if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldBundleCreateEx.F90"
    else
       print *, "FAIL: ESMF_FieldBundleCreateEx.F90"
    end if

      call ESMF_Finalize(rc=rc)

!BOC
     end program ESMF_FieldBundleCreateEx
!EOC
    
