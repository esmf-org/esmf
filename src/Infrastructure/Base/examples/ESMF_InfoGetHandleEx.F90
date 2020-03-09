! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2020, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Get Handle}
! \label{get_info_handle_from_esmf_object}
! This example demonstrates how to retrieve an \texttt{ESMF\_Info} object handle
! from an ESMF object. \texttt{ESMF\_Info} handles are a view into the object's
! \texttt{ESMF\_Info} storage and should not be created/destroyed as the \texttt{ESMF\_Info}'s
! lifetime is determined by its host object's lifetime. Destroying the host object
! will leave a handle in an undefined state.
!EOE

    program ESMF_InfoGetHandleEx

!==============================================================================
!==============================================================================

#include "ESMF.h"

    use ESMF
    use ESMF_TestMod

    implicit none

!------------------------------------------------------------------------------

!BOE
! Variable declarations:
!EOE
!BOC
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Array) :: array
    type(ESMF_Info) :: infoh
    real(ESMF_KIND_R8), dimension(10,10), target :: farray
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
    integer :: rc
!EOC
    type(ESMF_VM) :: vm
    integer :: finalrc, result, localPet
    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!------------------------------------------------------------------------------

    write(failMsg, *) "Example ESMF_InfoGetHandleEx failure"
    write(testname, *) "Example ESMF_InfoGetHandleEx"

    finalrc = ESMF_SUCCESS

!------------------------------------------------------------------------------

!   ! Initialize the Framework, and get the default VM
    call ESMF_Initialize(vm=vm, defaultlogfilename="InfoGetHandleEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------

!BOE
! Create an ESMF Array.
!EOE
!BOC
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    nullify(farrayPtr)
    farrayPtr => farray
    array = ESMF_ArrayCreate(distgrid, farrayPtr, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Get the \texttt{ESMF\_Info} handle from the object. See example \ref{info_tutorial} for
! additional usage examples.
!EOE
!BOC
    infoh = ESMF_InfoGetHandle(array, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
!BOE
! Destroy everything except the \texttt{ESMF\_Info} object. Attempting to destroy
! the \texttt{ESMF\_Info} handle will result in an error.
!EOE
!BOC
    call ESMF_ArrayDestroy(array, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!------------------------------------------------------------------------------

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

    call ESMF_Finalize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    if (finalrc .eq. ESMF_SUCCESS) then
        print *, "PASS: ESMF_InfoGetHandleEx.F90"
    else
        print *, "FAIL: ESMF_InfoGetHandleEx.F90"
    end if

!==============================================================================
!==============================================================================

    end program ESMF_InfoGetHandleEx
