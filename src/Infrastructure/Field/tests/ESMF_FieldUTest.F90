! $Id: ESMF_FieldUTest.F90,v 1.1 2003/03/10 21:54:22 cdeluca Exp $
!
!-------------------------------------------------------------------------
! Unit test code for Fields.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  The following routines are the Unit Test Suite for the {\tt Field}
!  class.   The following arguments are accepted:
!
!  \begin{description}
!  \item[-a]  (all) run all possible tests
!  \item[-b]  (brief) run minimal tests
!  \end{description}
!
!
! 
!EOP
!
! to include the unit test source in the docs, add a !BOP here
!
!\begin{verbatim}

! !INTERFACE:
    program FieldUnitTest
    
! !USES:
#include "ESMF.h"

    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_ArrayMod
    use ESMF_DataMapMod
    use ESMF_GridMod
    use ESMF_FieldMod
    
    implicit none
    
! !Local Variables:
    integer :: x, y, rc 
    type(ESMF_Grid) :: grid
    type(ESMF_Array) :: arr
    real, dimension(:,:), pointer :: f90ptr1
    type(ESMF_DataMap) :: dm
    type(ESMF_RelLoc) :: rl
    character (len = 20) :: fname
    type(ESMF_IOSpec) :: ios
    type(ESMF_Field) :: f1, f2, f3, f4, f5
    
    print *, "running with library version ", ESMF_VERSION_STRING
    
    f1 = ESMF_FieldCreateNoData()
    print *, "field create 1 returned"
    call ESMF_FieldPrint(f1)
    call ESMF_FieldDestroy(f1)
    print *, "field 1 destroyed"
    call ESMF_FieldPrint(f1)

    f2 = ESMF_FieldCreateNoData("pressure")
    print *, "field create 2 returned"
    call ESMF_FieldDestroy(f2)

    grid = ESMF_GridCreate(name="atmgrid", rc=rc)
    allocate(f90ptr1(10,20))
    arr = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)  
    
    f3 = ESMF_FieldCreate(grid, arr, ESMF_NO_COPY, ESMF_CELL_CENTER, &
                                   dm, "Field 0", ios, rc)
    print *, "field create 3 returned"
    call ESMF_FieldDestroy(f3)
    
    f4 = ESMF_FieldCreate(grid, arr, ESMF_DO_COPY, &
                                     name="Field 1", iospec=ios, rc=rc)
    print *, "field create 4 returned"
    call ESMF_FieldDestroy(f4)
    
    f5 = ESMF_FieldCreateNoData("Field 2", rc=rc)
    print *, "field create 5 returned"
    call ESMF_FieldDestroy(f5)
    
    end program FieldUnitTest
    
!\end{verbatim}
!EOP
    
