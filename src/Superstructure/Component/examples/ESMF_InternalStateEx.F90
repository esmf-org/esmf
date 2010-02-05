! $Id: ESMF_InternalStateEx.F90,v 1.18.2.1 2010/02/05 20:03:51 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
!BOP
!\subsubsection{Setting and Getting the Internal State}  
!
!   ESMF provides the concept of an Internal State that is associated with
!   a Component. Through the Internal State API a user can attach a private
!   data block to a Component, and later retrieve a pointer to this memory
!   allocation. Setting and getting of Internal State information are
!   supported from anywhere in the Component's SetServices, Initialize, Run,
!   or Finalize code.
!
!   The code below demonstrates the basic Internal State API
!   of {\tt ESMF\_<Grid|Cpl>SetInternalState()} and
!   {\tt ESMF\_<Grid|Cpl>GetInternalState()}. Notice that an extra level of
!   indirection to the user data is necessary!
!
!EOP
!-------------------------------------------------------------------------

program ESMF_InternalStateEx

!-------------------------------------------------------------------------
! !USES:
!BOC
  ! ESMF Framework module
  use ESMF_Mod
  implicit none
  
  type(ESMF_GridComp) :: comp
  integer :: rc, finalrc

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

  type(dataWrapper) :: wrap1, wrap2
  type(testData), target :: data
  type(testData), pointer :: datap  ! extra level of indirection

  finalrc = ESMF_SUCCESS
!-------------------------------------------------------------------------
        
  call ESMF_Initialize(rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

!-------------------------------------------------------------------------

  !  Creation of a Component
  comp = ESMF_GridCompCreate(name="test", rc=rc)  
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

!-------------------------------------------------------------------------
! This could be called, for example, during a Component's initialize phase.

    ! Initialize private data block
  data%testValue = 4567
  data%testScaling = 0.5

  ! Set Internal State
  wrap1%p => data
  call ESMF_GridCompSetInternalState(comp, wrap1, rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

!-------------------------------------------------------------------------
! This could be called, for example, during a Component's run phase.

  ! Get Internal State
  call ESMF_GridCompGetInternalState(comp, wrap2, rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  ! Access private data block and verify data
  datap => wrap2%p 
  if ((datap%testValue .ne. 4567) .or. (datap%testScaling .ne. 0.5)) then
    print *, "did not get same values back"
    finalrc = ESMF_FAILURE
  else
    print *, "got same values back from GetInternalState as original"
  endif

!EOC
!-------------------------------------------------------------------------

  call ESMF_GridCompDestroy(comp, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  call ESMF_Finalize(rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  if (finalrc .eq. ESMF_SUCCESS) then
    print *, "PASS: ESMF_InternalStateEx.F90"
  else 
    print *, "FAIL: ESMF_InternalStateEx.F90"
  end if

end program ESMF_InternalStateEx
    
