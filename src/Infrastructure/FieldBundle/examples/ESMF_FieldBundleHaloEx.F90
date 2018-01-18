! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

     program FieldBundleHaloEx

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
! !PROGRAM: ESMF_FieldBundleHaloEx - FieldBundle Sparse Matrix Multiplication
!     
! !DESCRIPTION:
!     
! This program shows examples of FieldBundle interfaces for halo update of data.
!-----------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHaloEx"
     ! ESMF Framework module
     use ESMF
     use ESMF_TestMod
     implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

    ! Local variables
    type(ESMF_FieldBundle)                      :: fieldBundle
    type(ESMF_Field)                            :: field(4)
    type(ESMF_Grid)                             :: grid
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_VM)                               :: vm
    type(ESMF_RouteHandle)                      :: routehandle
    type(ESMF_ArraySpec)                        :: arrayspec
    integer                                     :: rc, finalrc, lpe, i, halo(2,2,4,4)
    real(ESMF_KIND_R4), pointer                 :: fptr(:,:)
    integer                                     :: excllb(2), exclub(2), sizes(2)
    integer                                     :: j, k, iter, result
    type(ESMF_STAGGERLOC)                       :: staggers(4)
    character(len=16)                           :: names(4) 
    real                                        :: PI=3.14159265358
    character(ESMF_MAXSTR)                      :: testname
    character(ESMF_MAXSTR)                      :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_FieldBundleHaloEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    finalrc = ESMF_SUCCESS
!------------------------------------------------------------------------------
    call ESMF_Initialize(defaultlogfilename="FieldBundleHaloEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{Perform FieldBundle halo update}
! \label{sec:fieldbundle:usage:halo}
!
!\begin{sloppypar}
! {\tt ESMF\_FieldBundleHalo} interface can be used to perform halo updates
! for all the Fields contained in the {\tt ESMF\_FieldBundle}.
!\end{sloppypar}
! 
!
! In this example, we will set up a FieldBundle for a 2D inviscid and compressible
! flow problem. We will illustrate the FieldBundle halo update operation but we will
! not solve the non-linear PDEs. The emphasis here is to demonstrate
! how to set up halo regions, how a numerical scheme updates
! the exclusive regions, and how a halo update communicates data in the halo regions. Here
! are the governing equations:
!
!
! $u_t + u u_x + v u_y + \frac{1}{\rho} p_x = 0$ (conservation of momentum in x-direction)
!
!
! $v_t + u v_x + v v_y + \frac{1}{\rho} p_y = 0$ (conservation of momentum in y-direction)
!
!
! ${\rho}_t + {\rho u}_x + {\rho v}_y = 0$ (conservation of mass)
!
!
! $\frac{\rho}{\rho^\gamma} + u {(\frac{p}{\rho^\gamma})}_x + v {(\frac{p}{\rho^\gamma})}_y = 0$ (conservation of energy)
!
!
! The four unknowns are pressure $p$, density $\rho$, velocity ($u$, $v$). The grids
! are set up using Arakawa D stagger ($p$ on corner, $\rho$ at center, $u$ and $v$ on edges).
! $p$, $\rho$, $u$, and $v$ are bounded by necessary boundary conditions and initial conditions.
!
!
! Section \ref{Array:Halo} provides a detailed discussion of the 
! halo operation implemented in ESMF.
!EOE
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, localPet=lpe, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
    ! create distgrid and grid according to the following decomposition 
    ! and stagger pattern, r is density.
    !
    ! p--------u-------+p+-------u--------p
    ! !                 |                 |
    ! !                 |                 |
    ! !                 |                 |
    ! v        r        v        r        v
    ! !      PET 0      |      PET 1      |
    ! !                 |                 |
    ! !                 |                 |
    ! p--------u-------+p+-------u--------p
    ! !                 |                 |
    ! !                 |                 |
    ! !                 |                 |
    ! v        r        v        r        v
    ! !      PET 2      |      PET 3      |
    ! !                 |                 |
    ! !                 |                 |
    ! p--------u-------+p+-------u--------p
    !
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/256,256/), &
        regDecomp=(/2,2/), &
        rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    ! create field bundles and fields
    fieldBundle = ESMF_FieldBundleCreate(rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    ! set up exclusive/total region for the fields
    !
    ! halo: L/U, nDim, nField, nPet
    ! halo configuration for pressure, and similarly for density, u, and v
    halo(1,1,1,1) = 0
    halo(2,1,1,1) = 0
    halo(1,2,1,1) = 0
    halo(2,2,1,1) = 0
    halo(1,1,1,2) = 1   ! halo in x direction on left hand side of pet 1
    halo(2,1,1,2) = 0
    halo(1,2,1,2) = 0
    halo(2,2,1,2) = 0
    halo(1,1,1,3) = 0
    halo(2,1,1,3) = 1   ! halo in y direction on upper side of pet 2
    halo(1,2,1,3) = 0
    halo(2,2,1,3) = 0
    halo(1,1,1,4) = 1   ! halo in x direction on left hand side of pet 3
    halo(2,1,1,4) = 1   ! halo in y direction on upper side of pet 3
    halo(1,2,1,4) = 0
    halo(2,2,1,4) = 0
!EOC
    ! density
    halo(1,1,2,1) = 0
    halo(2,1,2,1) = 1   
    halo(1,2,2,1) = 0
    halo(2,2,2,1) = 1
    halo(1,1,2,2) = 1   
    halo(2,1,2,2) = 0
    halo(1,2,2,2) = 0
    halo(2,2,2,2) = 1
    halo(1,1,2,3) = 0
    halo(2,1,2,3) = 1
    halo(1,2,2,3) = 1
    halo(2,2,2,3) = 0
    halo(1,1,2,4) = 1
    halo(2,1,2,4) = 0
    halo(1,2,2,4) = 1
    halo(2,2,2,4) = 0
    ! u
    halo(1,1,3,1) = 0
    halo(2,1,3,1) = 1   
    halo(1,2,3,1) = 0
    halo(2,2,3,1) = 1
    halo(1,1,3,2) = 1   
    halo(2,1,3,2) = 0
    halo(1,2,3,2) = 0
    halo(2,2,3,2) = 1
    halo(1,1,3,3) = 0
    halo(2,1,3,3) = 1
    halo(1,2,3,3) = 1
    halo(2,2,3,3) = 0
    halo(1,1,3,4) = 1
    halo(2,1,3,4) = 0
    halo(1,2,3,4) = 1
    halo(2,2,3,4) = 0
    ! v
    halo(1,1,4,1) = 0
    halo(2,1,4,1) = 1   
    halo(1,2,4,1) = 0
    halo(2,2,4,1) = 1
    halo(1,1,4,2) = 1   
    halo(2,1,4,2) = 0
    halo(1,2,4,2) = 0
    halo(2,2,4,2) = 1
    halo(1,1,4,3) = 0
    halo(2,1,4,3) = 1
    halo(1,2,4,3) = 1
    halo(2,2,4,3) = 0
    halo(1,1,4,4) = 1
    halo(2,1,4,4) = 0
    halo(1,2,4,4) = 1
    halo(2,2,4,4) = 0

!BOC
    ! names and staggers of the 4 unknown fields
    names(1) = "pressure"
    names(2) = "density"
    names(3) = "u"
    names(4) = "v"
    staggers(1) = ESMF_STAGGERLOC_CORNER
    staggers(2) = ESMF_STAGGERLOC_CENTER
    staggers(3) = ESMF_STAGGERLOC_EDGE2
    staggers(4) = ESMF_STAGGERLOC_EDGE1
    
    ! create a FieldBundle
    lpe = lpe + 1
    do i = 1, 4
        field(i) = ESMF_FieldCreate(grid, arrayspec, &
                totalLWidth=(/halo(1,1,i,lpe), halo(1,2,i,lpe)/), &
                totalUWidth=(/halo(2,1,i,lpe), halo(2,2,i,lpe)/), &
                staggerloc=staggers(i), name=names(i), &
                rc=rc)
!EOC
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
        call ESMF_FieldBundleAdd(fieldBundle, (/field(i)/), rc=rc)
!EOC
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    enddo

    ! compute the routehandle
    call ESMF_FieldBundleHaloStore(fieldBundle, routehandle=routehandle, &
                                   rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    do iter = 1, 10
        do i = 1, 4
            call ESMF_FieldGet(field(i), farrayPtr=fptr, &
                exclusiveLBound=excllb, exclusiveUBound=exclub, rc=rc)
!EOC
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
            sizes = exclub - excllb
            ! fill the total region with 0.
            fptr = 0.
            ! only update the exclusive region on local PET
            do j = excllb(1), exclub(1)
              do k = excllb(2), exclub(2)
                fptr(j,k) = iter * cos(2.*PI*j/sizes(1))*sin(2.*PI*k/sizes(2))
              enddo 
            enddo 
        enddo
        ! call halo execution to update the data in the halo region,
        ! it can be verified that the halo regions change from 0. 
        ! to non zero values.
        call ESMF_FieldBundleHalo(fieldbundle, routehandle=routehandle, rc=rc)
!EOC
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    enddo
    ! release halo route handle
    call ESMF_FieldBundleHaloRelease(routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


    ! release all acquired resources
    call ESMF_FieldBundleDestroy(fieldBundle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    do i = 1, 4
        call ESMF_FieldDestroy(field(i), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)



    call ESMF_Finalize(rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

     if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldBundleHaloEx.F90"
     else
       print *, "FAIL: ESMF_FieldBundleHaloEx.F90"
     end if

    end program FieldBundleHaloEx
