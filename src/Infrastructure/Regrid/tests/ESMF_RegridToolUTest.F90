! $Id: ESMF_RegridToolUTest.F90,v 1.10.2.4 2009/01/21 21:25:23 cdeluca Exp $
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
    program ESMF_RegridToolUTest

!------------------------------------------------------------------------------

!================================================================================


#include "ESMF_Macros.inc"

    ! USES:Framework module
    use ESMF_TestMod  ! test methods
    use ESMF_Mod      ! Framework module

    use ESMF_RegridSubroutines


    integer :: result=0

    call ESMF_TestStart(ESMF_SRCLINE, rc=lrc)
    call ESMF_VMGetGlobal(vm, rc=lrc)
    !--- Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPET=localPet,  rc=lrc)

#ifdef ESMF_TESTEXHAUSTIVE
    ! Added the following string for the nightly build scripts
    !UTest_EX
    ! Get the test list file only if ESMF_TESTEXHAUSTIVE = ON
    call readTestList(testListFile ="ESMF_RegridTestList.rc", npets=npets, rc=lrc)

#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_RegridToolUTest

!===============================================================================
