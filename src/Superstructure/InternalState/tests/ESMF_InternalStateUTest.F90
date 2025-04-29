! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_InternalStateUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF_Conf.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_InternalStateUTest - Unit test for InternalState.
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for InternalState code.
!
!-------------------------------------------------------------------------
!
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    implicit none

    ! Local variables
    integer                :: rc, i
    character(ESMF_MAXSTR) :: cname
    type(ESMF_GridComp)    :: gcomp

    ! individual test failure message
    character(ESMF_MAXSTR) :: name
    character(ESMF_MAXSTR) :: failMsg
    integer                :: result = 0

    ! Internal State Variables
    type testData
    sequence
      logical :: testBool
      integer :: testInteger
      real    :: testReal
    end type

    type dataWrapper
    sequence
      type(testData), pointer :: p
    end type

    type(dataWrapper)             :: wrapAdd, wrapGet
    character(len=:), allocatable :: labelList(:)
    logical                       :: isValid

!-------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifndef ESMF_NO_F2018ASSUMEDTYPE 
! The InternalState API is only available with compilers that support the
! Fortran 2018 assumed-type dummy argument feature.

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    gcomp = ESMF_GridCompCreate(rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add InternalState (no label) to a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    allocate(wrapAdd%p)
    wrapAdd%p%testBool    = .true.
    wrapAdd%p%testInteger = 1234
    wrapAdd%p%testReal    = 1234.5678
    call ESMF_InternalStateAdd(gcomp, internalState=wrapAdd, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get list of InternalState labels from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, labelList=labelList, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! write the labelList to log
    do i=1, size(labelList)
      call ESMF_LogWrite("InternalState label: "//labelList(i), &
        ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add InternalState (with label #1) to a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    allocate(wrapAdd%p)
    wrapAdd%p%testBool    = .true.
    wrapAdd%p%testInteger = 2345
    wrapAdd%p%testReal    = 2345.6789
    call ESMF_InternalStateAdd(gcomp, internalState=wrapAdd, label="L1", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get list of InternalState labels from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, labelList=labelList, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! write the labelList to log
    do i=1, size(labelList)
      call ESMF_LogWrite("InternalState label: "//labelList(i), &
        ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add InternalState (with label #2) to a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    allocate(wrapAdd%p)
    wrapAdd%p%testBool    = .true.
    wrapAdd%p%testInteger = 3456
    wrapAdd%p%testReal    = 3456.7890
    call ESMF_InternalStateAdd(gcomp, internalState=wrapAdd, label="L2", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get list of InternalState labels from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, labelList=labelList, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! write the labelList to log
    do i=1, size(labelList)
      call ESMF_LogWrite("InternalState label: "//labelList(i), &
        ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get the InternalState without label from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, internalState=wrapGet, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Verify InternalState without label contents Test"
    write(failMsg, *) "InternalState members do not validate"
    isValid = (wrapGet%p%testBool) .and. (wrapGet%p%testInteger==1234) &
      .and. ((wrapGet%p%testReal-1234.5678)<tiny(wrapGet%p%testReal))
    call ESMF_Test((isValid), name, failMsg, result, ESMF_SRCLINE)

    ! reset InternalState values
    wrapGet%p%testBool    = .false.
    wrapGet%p%testInteger = 4321
    wrapGet%p%testReal    = 4321.8765

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get the InternalState with label #1 from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, internalState=wrapGet, label="L1", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Verify InternalState with label #1 contents Test"
    write(failMsg, *) "InternalState members do not validate"
    isValid = (wrapGet%p%testBool) .and. (wrapGet%p%testInteger==2345) &
      .and. ((wrapGet%p%testReal-2345.6789)<tiny(wrapGet%p%testReal))
    call ESMF_Test((isValid), name, failMsg, result, ESMF_SRCLINE)

    ! reset InternalState values
    wrapGet%p%testBool    = .false.
    wrapGet%p%testInteger = 5432
    wrapGet%p%testReal    = 5432.9876

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get the InternalState with label #2 from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, internalState=wrapGet, label="L2", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Verify InternalState with label #2 contents Test"
    write(failMsg, *) "InternalState members do not validate"
    isValid = (wrapGet%p%testBool) .and. (wrapGet%p%testInteger==3456) &
      .and. ((wrapGet%p%testReal-3456.7890)<tiny(wrapGet%p%testReal))
    call ESMF_Test((isValid), name, failMsg, result, ESMF_SRCLINE)

    ! reset InternalState values
    wrapGet%p%testBool    = .false.
    wrapGet%p%testInteger = 6543
    wrapGet%p%testReal    = 6543.0987

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get the InternalState without label from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, internalState=wrapGet, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Verify InternalState without label modified contents Test"
    write(failMsg, *) "InternalState members do not validate"
    isValid = (.not.wrapGet%p%testBool) .and. (wrapGet%p%testInteger==4321) &
      .and. ((wrapGet%p%testReal-4321.8765)<tiny(wrapGet%p%testReal))
    call ESMF_Test((isValid), name, failMsg, result, ESMF_SRCLINE)

    ! deallocate InternalState data
    deallocate(wrapGet%p)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get the InternalState with label #1 from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, internalState=wrapGet, label="L1", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Verify InternalState with label #1 modified contents Test"
    write(failMsg, *) "InternalState members do not validate"
    isValid = (.not.wrapGet%p%testBool) .and. (wrapGet%p%testInteger==5432) &
      .and. ((wrapGet%p%testReal-5432.9876)<tiny(wrapGet%p%testReal))
    call ESMF_Test((isValid), name, failMsg, result, ESMF_SRCLINE)

    ! deallocate InternalState data
    deallocate(wrapGet%p)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Get the InternalState with label #2 from a Component Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_InternalStateGet(gcomp, internalState=wrapGet, label="L2", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Verify InternalState with label #2 modified contents Test"
    write(failMsg, *) "InternalState members do not validate"
    isValid = (.not.wrapGet%p%testBool) .and. (wrapGet%p%testInteger==6543) &
      .and. ((wrapGet%p%testReal-6543.0987)<tiny(wrapGet%p%testReal))
    call ESMF_Test((isValid), name, failMsg, result, ESMF_SRCLINE)

    ! deallocate InternalState data
    deallocate(wrapGet%p)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "GridCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(gcomp, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#else
    write(name, *) "Dummy Test"
    write(failMsg, *) "Dummy Test Failure"
    do i=1, 20
      ! correct number of dummy ESMF_Test() calls to satisfy test scripts
      call ESMF_Test(.true., name, failMsg, result, ESMF_SRCLINE)
    enddo
#endif

    call ESMF_TestEnd(ESMF_SRCLINE)

    end program ESMF_InternalStateUTest

