! $Id: ESMF_InternalStateEx.F90,v 1.10.2.3 2009/01/21 21:25:24 cdeluca Exp $
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
!
      program ESMF_InternalStateEx

!==============================================================================
!
! !PROGRAM: ESMF_InternalStateEx - Example of using Set/Get Internal State
!
! !DESCRIPTION:
!  Example of using the Component level Internal State routines.

!  These include:
!   ESMF_GridCompGetInternalState
!   ESMF_GridCompSetInternalState
!   ESMF_CplCompGetInternalState
!   ESMF_CplCompSetInternalState
!
!-------------------------------------------------------------------------
!BOC
!\subsubsection{Example of Getting and Setting an Internal State}  
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
!EOC
!
! !USES:
!BOC
    ! ESMF Framework module
    use ESMF_Mod
    implicit none
    
    type(ESMF_GridComp) :: comp1
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

    type (dataWrapper) :: wrap1, wrap2
    type(testData), target :: data1, data2


    finalrc = ESMF_SUCCESS
!-------------------------------------------------------------------------
        
    call ESMF_Initialize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 


!-------------------------------------------------------------------------

!   !  Creation of a Component
    comp1 = ESMF_GridCompCreate(name="test", rc=rc)  
    if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

!-------------------------------------------------------------------------
!   This could be called, for example, during a routine's initialize phase.

!   !  Set Internal State
    data1%testValue = 4567
    data1%testScaling = 0.5
    wrap1%p => data1

    call ESMF_GridCompSetInternalState(comp1, wrap1, rc)
    if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

!-------------------------------------------------------------------------
!   And this could be called, for example, during a routine's run phase.

!   ! Get Internal State
!   !   note that we do not assign the pointer inside wrap2 - this call
!   !   does that.
    call ESMF_GridCompGetInternalState(comp1, wrap2, rc)
    if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

    data2 = wrap2%p 
    if ((data2%testValue .ne. 4567) .or. (data2%testScaling .ne. 0.5)) then
       print *, "did not get same values back"
       finalrc = ESMF_FAILURE
    else
       print *, "got same values back from GetInternalState as original"
    endif


!EOC
!-------------------------------------------------------------------------
!   !  Destroying a component

    call ESMF_GridCompDestroy(comp1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

    call ESMF_Finalize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

    if (finalrc .eq. ESMF_SUCCESS) then
        print *, "PASS: ESMF_InternalStateEx.F90"
    else 
        print *, "FAIL: ESMF_InternalStateEx.F90"
    end if


    end program ESMF_InternalStateEx
    
