! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

     program FieldHaloEx

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
! !PROGRAM: ESMF_FieldHaloEx - Field Halo demonstration
!
! !DESCRIPTION:
!
! This program shows examples of Field interfaces for
! Halo operations
!-----------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldHaloEx"
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
    integer :: npx
    parameter(npx=64)
    integer :: rc, finalrc, startx, endx, iter
    real    :: dt, dx, alpha

    type(ESMF_Field)                            :: field
    type(ESMF_Grid)                             :: grid
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_VM)                               :: vm
    type(ESMF_RouteHandle)                      :: routehandle
    integer                                     :: lpe, i, result

    real(ESMF_KIND_R8), allocatable         :: tmp_farray(:)
    real(ESMF_KIND_R8), pointer             :: fptr(:)

    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_FieldHaloEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    finalrc = ESMF_SUCCESS
!------------------------------------------------------------------------------
    call ESMF_Initialize(defaultlogfilename="FieldHaloEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{Field Halo solving a domain decomposed heat transfer problem}
! \label{sec:field:usage:halo}
!
! The {\tt ESMF\_FieldHalo()} interface can be used to perform halo updates for a Field. This
! eases communication programming from a user perspective. By definition, the user
! program only needs to update locally owned exclusive region in each domain, then call
! FieldHalo to communicate the values in the halo region from/to neighboring domain elements.
! In this example, we solve a 1D heat transfer problem: $u_t = \alpha^2 u_{xx}$ with the
! initial condition $u(0, x) = 20$ and boundary conditions $u(t, 0) = 10, u(t, 1) = 40$.
! The temperature field $u$
! is represented by a {\tt ESMF\_Field}. A finite difference explicit time stepping scheme is employed.
! During each time step, FieldHalo update is called to communicate values in the halo region
! to neighboring domain elements. The steady state (as $t \rightarrow \infty$) solution
! is a linear temperature profile along $x$. The numerical solution is an approximation of
! the steady state solution. It can be verified to represent a linear temperature profile.
!
! Section \ref{Array:Halo} provides a discussion of the
! halo operation implemented in {\tt ESMF\_Array}.
!
!EOE

    ! Get current VM and pet number
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, localPet=lpe, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
! create 1D distgrid and grid decomposed according to the following diagram:
! +------------+   +----------------+   +---------------+   +--------------+
! |   DE 0  |  |   |  |   DE 1   |  |   |  |   DE 2  |  |   |  |   DE 3    |
! |  1 x 16 |  |   |  |  1 x 16  |  |   |  |  1 x 16 |  |   |  |  1 x 16   |
! |         | 1|<->|1 |          | 1|<->|1 |         | 1|<->|1 |           |
! |         |  |   |  |          |  |   |  |         |  |   |  |           |
! +------------+   +----------------+   +---------------+   +--------------+
    distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/npx/), &
        regDecomp=(/4/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! set up initial condition and boundary conditions of the
    ! temperature Field
    if(lpe == 0) then
        allocate(fptr(17), tmp_farray(17))
        fptr = 20.
        fptr(1) = 10.
        tmp_farray(1) = 10.
        startx = 2
        endx = 16

        field = ESMF_FieldCreate(grid, fptr, totalUWidth=(/1/), &
                name="temperature", rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else if(lpe == 3) then
        allocate(fptr(17), tmp_farray(17))
        fptr = 20.
        fptr(17) = 40.
        tmp_farray(17) = 40.
        startx = 2
        endx = 16

        field = ESMF_FieldCreate(grid, fptr, totalLWidth=(/1/), &
                name="temperature", rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else
        allocate(fptr(18), tmp_farray(18))
        fptr = 20.
        startx = 2
        endx = 17

        field = ESMF_FieldCreate(grid, fptr, &
            totalLWidth=(/1/), totalUWidth=(/1/), name="temperature", rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    ! compute the halo update routehandle of the decomposed temperature Field
    call ESMF_FieldHaloStore(field, routehandle=routehandle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    dt = 0.01
    dx = 1./npx
    alpha = 0.1

    ! Employ explicit time stepping
    ! Solution converges after about 9000 steps based on apriori knowledge.
    ! The result is a linear temperature profile stored in field.
    do iter = 1, 9000
     ! only elements in the exclusive region are updated locally
     ! in each domain
     do i = startx, endx
       tmp_farray(i) = &
       fptr(i)+alpha*alpha*dt/dx/dx*(fptr(i+1)-2.*fptr(i)+fptr(i-1))
      enddo
      fptr = tmp_farray
     ! call halo update to communicate the values in the halo region to
     ! neighboring domains
     call ESMF_FieldHalo(field, routehandle=routehandle, rc=rc)
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

    ! release the halo routehandle
    call ESMF_FieldHaloRelease(routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! destroy all objects created in this example to prevent memory leak
    call ESMF_FieldDestroy(field, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    deallocate(fptr, tmp_farray)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


     call ESMF_Finalize(rc=rc)

     if (rc.NE.ESMF_SUCCESS) then
       finalrc = ESMF_FAILURE
     end if

     if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldHaloEx.F90"
     else
       print *, "FAIL: ESMF_FieldHaloEx.F90"
     end if

    end program FieldHaloEx
