! $Id: ESMF_InternGridWrapUTest.F90,v 1.1 2007/06/22 23:21:38 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_InternGridWrapUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOC
! !PROGRAM: ESMF_InternGridWrapUTest - Tests related to periodic interngrids
!
! !DESCRIPTION:
!
! The code in this file tests things specific to periodic interngrids.
!
!-----------------------------------------------------------------------------

    ! ESMF Framework module, and test methods
    use ESMF_Mod
    use ESMF_TestMod

    implicit none
    
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternGridWrapUTest.F90,v 1.1 2007/06/22 23:21:38 cdeluca Exp $'
!------------------------------------------------------------------------------

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0
      
    ! individual test result code
    integer :: rc
      
    ! individual test name 
    character(ESMF_MAXSTR) :: name, gName, Rgname
      
    ! individual test failure messages
    character(ESMF_MAXSTR*2) :: failMsg

    ! Local variables
    integer :: haloWidth
    integer :: interngridCount(2), interngridStart(2)
    integer :: dataIndexList(3), lbounds(3), localCount(3), ubounds(3)
    real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr1
    real (ESMF_KIND_R8), dimension(2) :: origin
    type(ESMF_DELayout)     :: layout
    type(ESMF_InternGrid)         :: interngrid
    type(ESMF_VM)           :: vm


!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_VMGetGlobal(vm, rc)
    write(name, *) "ESMF_VMGetGlobal Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    layout = ESMF_DELayoutCreate(vm, (/ 3, 2 /), rc=rc)
    write(name, *) "ESMF_DELayoutCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    origin = (/ 0.0, 0.0 /)

    !--------------------------------------------------------------------------
    !NEX_UTest
    interngrid = ESMF_InternGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
	                            periodic=(/ ESMF_FALSE, ESMF_FALSE /), &
                                    name="atminterngrid", rc=rc)
    write(name, *) "ESMF_InternGridCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=rc)
    write(name, *) "ESMF_InternGridDistribute Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    ! the interngridStart lines should all print the same values (and do not).
    ! this is support request 1095990, bug 1156870.
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !NEX_UTest
    ! get interngrid information used to calculate global indices
    call ESMF_InternGridGetDELocalInfo(interngrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=interngridCount, &
                                 globalStartPerDim=interngridStart, rc=rc)
    print *, "F/F interngridStart=", interngridStart
    write(name, *) "ESMF_InternGridGetDELocalInfo Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    interngrid = ESMF_InternGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
	                            periodic=(/ ESMF_FALSE, ESMF_TRUE /), &
                                    name="atminterngrid", rc=rc)
    write(name, *) "ESMF_InternGridCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=rc)
    write(name, *) "ESMF_InternGridDistribute Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    ! get interngrid information used to calculate global indices
    call ESMF_InternGridGetDELocalInfo(interngrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=interngridCount, &
                                 globalStartPerDim=interngridStart, rc=rc)
    print *, "F/T interngridStart=", interngridStart
    write(name, *) "ESMF_InternGridGetDELocalInfo Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    interngrid = ESMF_InternGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
	                            periodic=(/ ESMF_TRUE, ESMF_FALSE /), &
                                    name="atminterngrid", rc=rc)
    write(name, *) "ESMF_InternGridCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=rc)
    write(name, *) "ESMF_InternGridDistribute Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    ! get interngrid information used to calculate global indices
    call ESMF_InternGridGetDELocalInfo(interngrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=interngridCount, &
                                 globalStartPerDim=interngridStart, rc=rc)
    print *, "T/F interngridStart=", interngridStart
    write(name, *) "ESMF_InternGridGetDELocalInfo Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    interngrid = ESMF_InternGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
	                            periodic=(/ ESMF_TRUE, ESMF_TRUE /), &
                                    name="atminterngrid", rc=rc)
    write(name, *) "ESMF_InternGridCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=rc)
    write(name, *) "ESMF_InternGridDistribute Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    ! get interngrid information used to calculate global indices
    call ESMF_InternGridGetDELocalInfo(interngrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=interngridCount, &
                                 globalStartPerDim=interngridStart, rc=rc)
    print *, "T/T interngridStart=", interngridStart
    write(name, *) "ESMF_InternGridGetDELocalInfo Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    
#if 0
    ! these tests could be extended to compute the lbounds and ubounds
    ! for a global index, and tested.  this is the code, copied from the
    ! field global example, which would be used.

! globalStartPerDim really should be called something more descriptive like
! globalOffsetPerDim because it refers to the amount that must be added to a
! local index to translate it to a global index.  So the globalStart referring
! to an index instead of an offset should be this value plus one
    interngridStart(1) = interngridStart(1) + 1
    interngridStart(2) = interngridStart(2) + 1

! set local counts for the Field.  For this example, the second dimension of the
! data will correspond to the first InternGrid dimension and the third data dimension
! will correspond to the second InternGrid dimension
    dataIndexList(1) = 0
    dataIndexList(2) = 1
    dataIndexList(3) = 2
    lbounds(2)    = interngridStart(1)
    localCount(2) = interngridCount(1)
    lbounds(3)    = interngridStart(2)
    localCount(3) = interngridCount(2)

! the first data dimension is unrelated to the interngrid, so it has a user-specified
! count and its local index is assumed to start at one, although that is not
! necessarily true
    lbounds(1)    = 1
    localCount(1) = 32

! calculate upper bounds from lower bounds and counts
    do i = 1,3
      ubounds(i) = lbounds(i) + localCount(i) - 1
    enddo

! set the haloWidth of the Array
    haloWidth = 3

! modify the lower and upper bounds by the haloWidth
! Currently ESMF requires that all dimensions include the halo width, even if
! they are not related to the interngrid.  Hopefully that will no longer be
! required soon.
    do i = 1,3
      lbounds(i) = lbounds(i) - haloWidth
      ubounds(i) = ubounds(i) + haloWidth
    enddo

! allocate the F90 pointer with the calculated bounds
    allocate(f90ptr1(lbounds(1):ubounds(1), &
                     lbounds(2):ubounds(2), &
                     lbounds(3):ubounds(3)))

#endif

    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------


    call ESMF_TestEnd(result, ESMF_SRCLINE)


    end program ESMF_InternGridWrapUTest

    
