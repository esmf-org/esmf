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

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Add and Get the Internal State}
!
!   Adding and getting of InternalState information are supported from anywhere
!   in the Component's SetServices, Initialize, Run, or Finalize code.
!
!   The code below demonstrates the basic InternalState API. Notice that an
!   extra level of indirection to the user data is necessary!
!
!EOE
!-------------------------------------------------------------------------

program ESMF_InternalStateEx

!-------------------------------------------------------------------------
#include "ESMF.h"
! !USES:
!BOC
  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod
  implicit none

  type(ESMF_GridComp) :: comp
  integer :: rc, finalrc, i

  ! Internal State Variables
  type testData
  sequence
    integer :: testValue
    real    :: testScaling
  end type

  type dataWrapper
  sequence
    type(testData), pointer :: p
  end type

  type(dataWrapper)             :: wrap
  character(len=:), allocatable :: labelList(:)
!EOC
  integer :: result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_InternalStateEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  finalrc = ESMF_SUCCESS
!BOC
!-------------------------------------------------------------------------

  call ESMF_Initialize(defaultlogfilename="InternalStateEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------

  !  Creation of a Component
  comp = ESMF_GridCompCreate(name="test", rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

!EOC

#ifndef ESMF_NO_F2018ASSUMEDTYPE 
! The InternalState API is only available with compilers that support the
! Fortran 2018 assumed-type dummy argument feature.

!BOE
! This could be called, for example, during a Component's initialize phase.
!EOE
!BOC

  ! Allocate private data
  allocate(wrap%p)

  ! Initialize private data block
  wrap%p%testValue = 4567
  wrap%p%testScaling = 0.5

  ! Add Internal State to Component
  call ESMF_InternalStateAdd(comp, internalState=wrap, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! "Forget" the local reference to the private data block, do demonstrate that
  ! it can be correctly retrieved below.
  wrap%p => null()

!EOC
!BOE
! This could be called, for example, during a Component's run phase.
!EOE
!BOC

  ! Get Internal State
  call ESMF_InternalStateGet(comp, internalState=wrap, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! Access private data block and verify data
  if ((wrap%p%testValue .ne. 4567) .or. &
    (wrap%p%testScaling - 0.5 > tiny(wrap%p%testScaling))) then
    print *, "private data validation NOT successful"
    finalrc = ESMF_FAILURE
  else
    print *, "private data validation successful"
  endif

  ! Deallocate the private data block
  deallocate(wrap%p)

!EOC
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Add and Get Internal State with label}
!
!   InternalState information added to a component can be associated with a
!   string label. Multiple such named internal states can be added to the same
!   component object.
!
!EOE
!BOC
  ! Allocate another private data block
  allocate(wrap%p)

  ! Initialize private data block
  wrap%p%testValue = 1234
  wrap%p%testScaling = 0.8

  ! Add Internal State to the Component with label
  call ESMF_InternalStateAdd(comp, internalState=wrap, &
    label="first named data block", rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! And allocate another private data block
  allocate(wrap%p)

  ! Initialize private data block
  wrap%p%testValue = 4321
  wrap%p%testScaling = 0.1

  ! Add Internal State to the Component with label
  call ESMF_InternalStateAdd(comp, internalState=wrap, &
    label="second named data block", rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! A list of InternalState labels can be queried from the component object.
  call ESMF_InternalStateGet(comp, labelList=labelList, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! write the labelList to log
  do i=1, size(labelList)
    call ESMF_LogWrite("InternalState label: "//labelList(i), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo

!EOC
!BOE
!   The last internal state added under a specific label can be retrieved using
!   the {\tt ESMF\_InternalStateGet()} method with the {\tt lable} argument.
!EOE
!BOC

  ! Get Internal State
  call ESMF_InternalStateGet(comp, internalState=wrap, &
    label="first named data block", rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! Deallocate the private data block
  deallocate(wrap%p)

  ! Get Internal State
  call ESMF_InternalStateGet(comp, internalState=wrap, &
    label="second named data block", rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! Deallocate the private data block
  deallocate(wrap%p)

!EOC

#endif

  call ESMF_GridCompDestroy(comp, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


  call ESMF_Finalize(rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

  if (finalrc .eq. ESMF_SUCCESS) then
    print *, "PASS: ESMF_InternalStateEx.F90"
  else
    print *, "FAIL: ESMF_InternalStateEx.F90"
  end if

end program ESMF_InternalStateEx

