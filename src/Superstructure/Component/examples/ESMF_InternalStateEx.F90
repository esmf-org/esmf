! $Id: ESMF_InternalStateEx.F90,v 1.16 2009/03/27 05:33:32 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
!   These routines save the address of an internal, private data block
!   during the execution of a Component's initialize, run, or finalize
!   code, and retrieve the address back during a different invocation 
!   of these routines.  One situation where this is useful is in the
!   creation of ensembles of the same component.  In this case it can 
!   be tricky to distinguish which data belongs to which ensemble
!   member - especially if the ensemble members are executing on the
!   same PETs.  Internal states enable the user to create an array of
!   internal data spaces, one for each ensemble member.  The correct
!   data space can then be referenced for each ensemble member's calculations.
!   
!   See the code below for a simple example of using this capability.
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
  type(testData), pointer :: datap

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
    
