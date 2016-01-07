! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridCsrvUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridCsrvUTest - Unit tests for conservative Field Regrid methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 conservative Field Regrid unit tests.
!
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    use ESMF_GridUtilMod

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1
    logical :: itrp = .false.
    logical :: csrv = .false.
  
    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
#ifdef ESMF_TESTEXHAUSTIVE
      ! initialize 
      rc=ESMF_SUCCESS

! This #if surrounds all the tests to enable turning on just one test
#if 1
      ! do test
      call test_csrvregrid(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a sphere"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a sphere interpolation error"

      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a sphere conservation error"

      
      ! return result
        call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_csrvregridWMasks(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a sphere with masks"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a sphere with masks interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a sphere with masks conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_cartcsrvregridWMasks(itrp, csrv, rc)

       !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a cartesian grid with masks"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a cartesian grid with masks interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a cartesian grid with Masks conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      ! do test
      rc=ESMF_SUCCESS

      ! do test
      call test_3DcartcsrvregridWMasks(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a 3D cartesian grid with masks"

      ! return result
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a 3D cartesian grid with masks interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a 3D cartesian grid with Masks conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      ! do test
      rc=ESMF_SUCCESS

      call test_RegridCsrv3DCartMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a 3D cartesian Mesh"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a 3D cartesian Mesh interpolation error"
 
      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a 3D cartesian Mesh conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_csrvregridWMasksUserArea(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a sphere with user areas"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a sphere with user areas: interpolation error"

      
      ! return result
      call ESMF_Test((itrp .eqv. .true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
       !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a sphere with user areas: conservation error"

      
      ! return result
      call ESMF_Test((csrv .eqv. .true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_RegridCsrvCartPHMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a Mesh with a pentagon and hexagon"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a Mesh with a pentagon and hexagon", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a Mesh with a pentagon and hexagon", &
                     " conservation error"

       
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


     !!! Test frac norm option
      rc=ESMF_SUCCESS ! initialize 
      call test_sph_csrv_w_frac_norm(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a sphere with frac norm option"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a sphere with frac norm option: interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a sphere with frac norm option: conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_RegridCsrvCartPHFracNorm(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a pentagon and hexagon with frac norm"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a pentagon and hexagon with frac norm:", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a pentagon and hexagon with frac norm:", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !==========================================================================

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_RegridCart4ConcaveMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a Cart. Mesh with a concave quad."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a Cart. Mesh with a concave quad.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a Cart. Mesh with a concave quad.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      !============== Concave Cart Mesh =======================================

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_RegridCart4ConcaveMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a Cart. Mesh with a concave quad."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a Cart. Mesh with a concave quad.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a Cart. Mesh with a concave quad.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      !============== Concave Sph Mesh =======================================

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_RegridSph4ConcaveMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a Sph. Mesh with a concave quad."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a Sph. Mesh with a concave quad.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a sph. Mesh with a concave quad.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !============== MOAB Mesh to Mesh =======================================
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_MOABMeshToMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding MOAB Mesh to Mesh."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding MOAB Mesh to Mesh.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding MOAB Mesh to Mesh.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_RegridCsrvCartMultiPoly(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Conservative regridding on a Mesh with a multipolygon"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Conservative regridding on a Mesh with a multipolygon", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Conservative regridding on a Mesh with a multipolygon", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test ignoreDegenerate"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_ignoreDegenerate(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
#endif
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

contains 
#define ESMF_METHOD "test_csrvregrid"

      subroutine test_csrvregrid(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 180
  Src_ny = 100

!  Src_nx = 360
!  Src_ny = 180

  Src_dx = 360.0/Src_nx
  Src_dy = 180.0/Src_ny

  Dst_nx = 100
  Dst_ny = 80

!  Dst_nx = 144
!  Dst_ny = 72

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
!                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

!    write(*,*) "A Corner: x=[",clbnd(1),cubnd(1),"] y=[",clbnd(2),cubnd(2),"]"

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Src_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Src_dy - 90.0
        yp1= REAL(i2-1+1)*Src_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx + 0.5*Src_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! set src data 
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
        !farrayPtr(i1,i2) = 1.
        farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

     enddo
     enddo

  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Dst_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Dst_dy - 90.0
        yp1= REAL(i2-1+1)*Dst_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

     enddo
     enddo


  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
       isSphere=.true., isLatLonDeg=.true., filename="dstGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get src Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     minerror(1) = 100000.
     maxerror(1) = 0.
     error = 0.
     dstmass = 0.
 
     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif
        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo

  enddo    ! lDE
  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo

  enddo    ! lDE

  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10) csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
    write(*,*) "=== Spherical grids ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "
  endif

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_csrvregrid


      subroutine test_csrvregridwmasks(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 180
  Src_ny = 100

  Src_dx = 360.0/Src_nx
  Src_dy = 180.0/Src_ny

  Dst_nx = 100
  Dst_ny = 80

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Src_dy - 90.0
     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Src_dy - 90.0
        yp1= REAL(i2-1+1)*Src_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx + 0.5*Src_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! set src data 
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
       ! farrayPtr(i1,i2) = 1.
       farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        if ((lat>-45) .and. (lat<45)) then
           srcMask(i1,i2)=1
        else
           srcMask(i1,i2)=0
        endif
        
     enddo
     enddo
  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Dst_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Dst_dy - 90.0
        yp1= REAL(i2-1+1)*Dst_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
       ! xfarrayPtr(i1,i2) = 1.0

        if ((lon>-45) .and. (lon<45)) then
           dstMask(i1,i2)=1
        else 
           dstMask(i1,i2)=0
        endif

     enddo
     enddo


  enddo    ! lDE


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField, dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     minerror(1) = 100000.
     maxerror(1) = 0.
     error = 0.
     dstmass = 0.
 
     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! skip if masked
        if (dstMask(i1,i2) .eq. 1) cycle

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
        ! (Note that this is what SCRIP does)
!        if (dstFracptr(i1,i2) .lt. 0.999) cycle
         if (dstFracptr(i1,i2) .lt. 0.001) cycle

        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2)/dstFracPtr(i1,i2)) &
                                       - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2)/dstFracPtr(i1,i2)) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif

     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo

  enddo    ! lDE


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Spherical grids with masks ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_csrvregridwmasks

 
      subroutine test_cartcsrvregridwmasks(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),xfptr(:,:),errorfptr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: x,y
  real(ESMF_KIND_R8) :: cnr_x,cnr_xp1,cnr_y,cnr_yp1
  real(ESMF_KIND_R8) :: Src_dx, Src_dy
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
  real(ESMF_KIND_R8) :: Src_minx, Src_miny
  real(ESMF_KIND_R8) :: Src_maxx, Src_maxy
  real(ESMF_KIND_R8) :: Dst_minx, Dst_miny
  real(ESMF_KIND_R8) :: Dst_maxx, Dst_maxy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc

  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 30
  Src_ny = 30

  Src_minx=0.0
  Src_miny=0.0

  Src_maxx=10.0
  Src_maxy=10.0

  Dst_nx = 20
  Dst_ny = 20

  Dst_minx=0.0
  Dst_miny=0.0

  Dst_maxx=10.0
  Dst_maxy=10.0


  ! setup source grid
  srcGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_CART, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              coordSys=ESMF_COORDSYS_CART, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! SET CORNER STAGGER COORDS
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates
        fptrXC(i1,i2) = ((Src_maxx-Src_minx)*REAL(i1-1)/REAL(Src_nx-1))+Src_minx
        fptrYC(i1,i2) = ((Src_maxy-Src_miny)*REAL(i2-1)/REAL(Src_ny-1))+Src_miny
     enddo
     enddo


     !! SET CENTER STAGGER COORDS, FUNC, ETC. 
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        !!! compute corner coordinates surrounding center
        cnr_x = ((Src_maxx-Src_minx)*REAL(i1-1)/REAL(Src_nx-1))+Src_minx
        cnr_xp1 = ((Src_maxx-Src_minx)*REAL(i1-1+1)/REAL(Src_nx-1))+Src_minx

        cnr_y = ((Src_maxy-Src_miny)*REAL(i2-1)/REAL(Src_ny-1))+Src_miny
        cnr_yp1 = ((Src_maxy-Src_miny)*REAL(i2-1+1)/REAL(Src_ny-1))+Src_miny

        ! Calc Center coordinates as average of corner coords
        x = (cnr_x+cnr_xp1)/2.0
        y = (cnr_y+cnr_yp1)/2.0

        ! Set source value
        fptr(i1,i2)=x+y+20.0

        ! Set Center coordinates
        fptrXC(i1,i2) = x
        fptrYC(i1,i2) = y


        ! Set Mask
        if ((x > 4.0) .and. (x < 6.0)) then
           srcMask(i1,i2)=1
        else
           srcMask(i1,i2)=0
        endif
        
     enddo
     enddo
  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        fptrXC(i1,i2) = ((Dst_maxx-Dst_minx)*REAL(i1-1)/REAL(Dst_nx-1))+Dst_minx
        fptrYC(i1,i2) = ((Dst_maxy-Dst_miny)*REAL(i2-1)/REAL(Dst_ny-1))+Dst_miny

     enddo
     enddo


     !! DO CENTER STAGGER STUFF
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfptr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        !!! compute corner coordinates surrounding center
        cnr_x = ((Dst_maxx-Dst_minx)*REAL(i1-1)/REAL(Dst_nx-1))+Dst_minx
        cnr_xp1 = ((Dst_maxx-Dst_minx)*REAL(i1-1+1)/REAL(Dst_nx-1))+Dst_minx

        cnr_y = ((Dst_maxy-Dst_miny)*REAL(i2-1)/REAL(Dst_ny-1))+Dst_miny
        cnr_yp1 = ((Dst_maxy-Dst_miny)*REAL(i2-1+1)/REAL(Dst_ny-1))+Dst_miny

        ! Calc Center coordinates as average of corner coords
        x = (cnr_x+cnr_xp1)/2.0
        y = (cnr_y+cnr_yp1)/2.0

        ! Set Center coordinates
        fptrXC(i1,i2) = x
        fptrYC(i1,i2) = y
     
        ! Init dest
        fptr(i1,i2)=0.0

        ! Init exact destination value
        xfptr(i1,i2)=x+y+20.0

        ! Set mask
        if ((y > 4.0) .and. (y < 6.0)) then
           dstMask(i1,i2)=1
        else
           dstMask(i1,i2)=0
        endif
   
     enddo
     enddo

  enddo    ! lDE




  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField, dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Init
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  dstmass = 0.
 
  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! skip if masked
        if (dstMask(i1,i2) .eq. 1) cycle

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*fptr(i1,i2)

        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1,i2)*fptr(i1,i2)

        ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
        ! (Note that this is what SCRIP does)
        if (dstFracptr(i1,i2) .lt. 0.999) cycle

        if (xfptr(i1,i2) .ne. 0.0) then
           errorfptr(i1,i2)=ABS(fptr(i1,i2) - xfptr(i1,i2))/ABS(xfptr(i1,i2))
           error = error + errorfptr(i1,i2)
           if (errorfptr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfptr(i1,i2)
           endif
           if (errorfptr(i1,i2) < minerror(1)) then
             minerror(1) = errorfptr(i1,i2)
           endif
        else
           errorfptr(i1,i2)=ABS(fptr(i1,i2) - xfptr(i1,i2))
           error = error + errorfptr(i1,i2)
           if (errorfptr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfptr(i1,i2)
           endif
           if (errorfptr(i1,i2) < minerror(1)) then
             minerror(1) = errorfptr(i1,i2)
           endif
        endif

     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*fptr(i1,i2)
     enddo
     enddo

  enddo    ! lDE


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Cartesian grid with masks ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_cartcsrvregridwmasks



 
      subroutine test_3Dcartcsrvregridwmasks(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray, dstFracArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:,:), dstMask(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:,:),xfptr(:,:,:),errorfptr(:,:,:),iwtsptr(:,:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:,:), dstAreaptr(:,:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:,:), dstFracptr(:,:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3
  integer :: lDE, srcLocalDECount, dstLocalDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny, Src_nz
  integer :: Dst_nx, Dst_ny, Dst_nz
  real(ESMF_KIND_R8) :: x,y,z
  real(ESMF_KIND_R8) :: cnr_x,cnr_xp1
  real(ESMF_KIND_R8) :: cnr_y,cnr_yp1
  real(ESMF_KIND_R8) :: cnr_z,cnr_zp1
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, Src_dz
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy, Dst_dz
  real(ESMF_KIND_R8) :: Src_minx, Src_miny, Src_minz
  real(ESMF_KIND_R8) :: Src_maxx, Src_maxy, Src_maxz
  real(ESMF_KIND_R8) :: Dst_minx, Dst_miny, Dst_minz
  real(ESMF_KIND_R8) :: Dst_maxx, Dst_maxy, Dst_maxz
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid,dim
  real(ESMF_KIND_R8) :: mincoord(3), maxcoord(3)


  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc

  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 30
  Src_ny = 30
  Src_nz = 30

  Src_minx=0.0
  Src_miny=0.0
  Src_minz=0.0

  Src_maxx=10.0
  Src_maxy=10.0
  Src_maxz=10.0

  Dst_nx = 20
  Dst_ny = 20
  Dst_nz = 20

  Dst_minx=0.0
  Dst_miny=0.0
  Dst_minz=0.0

  Dst_maxx=10.0
  Dst_maxy=10.0
  Dst_maxz=10.0


  ! setup source grid
  srcGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/src_nx,src_ny,src_nz/),regDecomp=(/petCount,1,1/), &
                              coordSys=ESMF_COORDSYS_CART, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/dst_nx,dst_ny,dst_nz/),regDecomp=(/1,petCount,1/), &
                              coordSys=ESMF_COORDSYS_CART, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs in source grid
  call ESMF_GridGet(srcGrid, localDECount=srcLocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs in dest. grid
  call ESMF_GridGet(dstGrid, localDECount=dstLocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Frac array
  call ESMF_FieldGet(dstFracField, array=dstFracArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     !! SET CORNER STAGGER COORDS
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER_VFACE, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER_VFACE, coordDim=2, &
                            farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER_VFACE, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        ! Set source coordinates
        fptrXC(i1,i2,i3) = ((Src_maxx-Src_minx)*REAL(i1-1)/REAL(Src_nx))+Src_minx
        fptrYC(i1,i2,i3) = ((Src_maxy-Src_miny)*REAL(i2-1)/REAL(Src_ny))+Src_miny
        fptrZC(i1,i2,i3) = ((Src_maxz-Src_minz)*REAL(i3-1)/REAL(Src_nz))+Src_minz

     enddo
     enddo
     enddo


     !! SET CENTER STAGGER COORDS, FUNC, ETC. 
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        !!! compute corner coordinates surrounding center
        cnr_x = ((Src_maxx-Src_minx)*REAL(i1-1)/REAL(Src_nx))+Src_minx
        cnr_xp1 = ((Src_maxx-Src_minx)*REAL(i1-1+1)/REAL(Src_nx))+Src_minx

        cnr_y = ((Src_maxy-Src_miny)*REAL(i2-1)/REAL(Src_ny))+Src_miny
        cnr_yp1 = ((Src_maxy-Src_miny)*REAL(i2-1+1)/REAL(Src_ny))+Src_miny

        cnr_z = ((Src_maxz-Src_minz)*REAL(i3-1)/REAL(Src_nz))+Src_minz
        cnr_zp1 = ((Src_maxz-Src_minz)*REAL(i3-1+1)/REAL(Src_nz))+Src_minz

        ! Calc Center coordinates as average of corner coords
        x = (cnr_x+cnr_xp1)/2.0
        y = (cnr_y+cnr_yp1)/2.0
        z = (cnr_z+cnr_zp1)/2.0

        ! Set source value
        fptr(i1,i2,i3)=x+y+z+20.0
!        fptr(i1,i2,i3)=1.0

        ! Set Center coordinates
        fptrXC(i1,i2,i3) = x
        fptrYC(i1,i2,i3) = y
        fptrZC(i1,i2,i3) = z


        ! Set Mask
        if ((x > 4.0) .and. (x < 6.0)) then
           srcMask(i1,i2,i3)=1
        else
           srcMask(i1,i2,i3)=0
        endif

   
     enddo
     enddo
     enddo
  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER_VFACE, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER_VFACE, coordDim=2, &
                            farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER_VFACE, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     mincoord=10000
     maxcoord=-10000

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        fptrXC(i1,i2,i3) = ((Dst_maxx-Dst_minx)*REAL(i1-1)/REAL(Dst_nx))+Dst_minx
        fptrYC(i1,i2,i3) = ((Dst_maxy-Dst_miny)*REAL(i2-1)/REAL(Dst_ny))+Dst_miny
        fptrZC(i1,i2,i3) = ((Dst_maxz-Dst_minz)*REAL(i3-1)/REAL(Dst_nz))+Dst_minz

     enddo
     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, fptr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfptr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        !!! compute corner coordinates surrounding center
        cnr_x = ((Dst_maxx-Dst_minx)*REAL(i1-1)/REAL(Dst_nx))+Dst_minx
        cnr_xp1 = ((Dst_maxx-Dst_minx)*REAL(i1-1+1)/REAL(Dst_nx))+Dst_minx

        cnr_y = ((Dst_maxy-Dst_miny)*REAL(i2-1)/REAL(Dst_ny))+Dst_miny
        cnr_yp1 = ((Dst_maxy-Dst_miny)*REAL(i2-1+1)/REAL(Dst_ny))+Dst_miny

        cnr_z = ((Dst_maxz-Dst_minz)*REAL(i3-1)/REAL(Dst_nz))+Dst_minz
        cnr_zp1 = ((Dst_maxz-Dst_minz)*REAL(i3-1+1)/REAL(Dst_nz))+Dst_minz

        ! Calc Center coordinates as average of corner coords
        x = (cnr_x+cnr_xp1)/2.0
        y = (cnr_y+cnr_yp1)/2.0
        z = (cnr_z+cnr_zp1)/2.0

        ! Set Center coordinates
        fptrXC(i1,i2,i3) = x
        fptrYC(i1,i2,i3) = y
        fptrZC(i1,i2,i3) = z
     
        ! Init dest
        fptr(i1,i2,i3)=0.0

        ! Init exact destination value
        xfptr(i1,i2,i3)=x+y+z+20.0
!        xfptr(i1,i2,i3)=1.0

        ! Set mask
        if ((y > 4.0) .and. (y < 6.0)) then
           dstMask(i1,i2,i3)=1
        else
           dstMask(i1,i2,i3)=0
        endif

   
     enddo
     enddo
     enddo
  enddo    ! lDE


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField,  dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  dstmass = 0.
  do lDE=0,dstLocalDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

 
     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! skip if masked
        if (dstMask(i1,i2,i3) .eq. 1) cycle

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*fptr(i1,i2)

        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1,i2,i3)*fptr(i1,i2,i3)

        ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
        ! (Note that this is what SCRIP does)
        if (dstFracptr(i1,i2,i3) .lt. 0.999) cycle


        if (xfptr(i1,i2,i3) .ne. 0.0) then
           errorfptr(i1,i2,i3)=ABS(fptr(i1,i2,i3) - xfptr(i1,i2,i3))/ABS(xfptr(i1,i2,i3))
           error = error + errorfptr(i1,i2,i3)
           if (errorfptr(i1,i2,i3) > maxerror(1)) then
             maxerror(1) = errorfptr(i1,i2,i3)
           endif
           if (errorfptr(i1,i2,i3) < minerror(1)) then
             minerror(1) = errorfptr(i1,i2,i3)
           endif
        else
           errorfptr(i1,i2,i3)=ABS(fptr(i1,i2,i3) - xfptr(i1,i2,i3))
           error = error + errorfptr(i1,i2,i3)
           if (errorfptr(i1,i2,i3) > maxerror(1)) then
             maxerror(1) = errorfptr(i1,i2,i3)
           endif
           if (errorfptr(i1,i2,i3) < minerror(1)) then
             minerror(1) = errorfptr(i1,i2,i3)
           endif
        endif

     enddo
     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,srcLocalDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2,i3)*srcAreaptr(i1,i2,i3)*fptr(i1,i2,i3)
     enddo
     enddo
     enddo

  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
       filename="srcGridCnr", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
       filename="dstGridCnr", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid",&
       array1=dstArray, array2=xdstArray, array3=errorArray, array4=dstFracArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif



  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== 3D Cartesian grid with masks ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_3Dcartcsrvregridwmasks



 subroutine test_RegridCsrv3DCartMesh(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
  integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes, numElems
  integer :: iconn,inode


  ! result code
  integer :: finalrc
  
  ! Init to success
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

  ! Setup source mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=18

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9, &
               10,11,12,13,14,15,16,17,18/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 3D Mesh the size is 3x the
     ! number of nodes.
     allocate(nodeCoords(3*numNodes))
     nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                  1.0,0.0,0.0, & ! node id 2
                  2.0,0.0,0.0, & ! node id 3
                  0.0,1.0,0.0, & ! node id 4
                  1.0,1.0,0.0, & ! node id 5
                  2.0,1.0,0.0, & ! node id 6
                  0.0,2.0,0.0, & ! node id 7
                  1.0,2.0,0.0, & ! node id 8
                  2.0,2.0,0.0, & ! node id 9
                  0.0,0.0,1.0, & ! node id 10
                  1.0,0.0,1.0, & ! node id 11
                  2.0,0.0,1.0, & ! node id 12
                  0.0,1.0,1.0, & ! node id 13
                  1.0,1.0,1.0, & ! node id 14
                  2.0,1.0,1.0, & ! node id 15
                  0.0,2.0,1.0, & ! node id 16
                  1.0,2.0,1.0, & ! node id 17
                  2.0,2.0,1.0 /) ! node id 18

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numElems=4

     ! Allocate and fill the element id array.
     allocate(elemIds(numElems))
     elemIds=(/1,2,3,4/) 

     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numElems))
     elemTypes=ESMF_MESHELEMTYPE_HEX

     ! Allocate and fill the element mask array
     allocate(elemMask(numElems))
     elemMask=(/0,0,0,1/)
                 
     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(8*numElems))
     elemConn=(/1,2,5,4,10,11,14,13, &  ! elem id 1
                2,3,6,5,11,12,15,14, &  ! elem id 2
                4,5,8,7,13,14,17,16,   &  ! elem id 3
                5,6,9,8,14,15,18,17/)  ! elem id 4

  else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=8

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5,10,11,13,14/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                    1.0,0.0,0.0, & ! node id 2
                    0.0,1.0,0.0, & ! node id 4
                    1.0,1.0,0.0, & ! node id 5
                    0.0,0.0,1.0, & ! node id 10
                    1.0,0.0,1.0, & ! node id 11
                    0.0,1.0,1.0, & ! node id 13
                    1.0,1.0,1.0 /) ! node id 14


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0, & ! node id 5
                    0, & ! node id 10
                    0, & ! node id 11
                    0, & ! node id 13
                    0/)  ! node id 14

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element mask array
       allocate(elemMask(numElems))
       elemMask=(/0/)

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=8

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6,11,12,14,15/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 3D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                    2.0,0.0,0.0, & ! node id 3
                    1.0,1.0,0.0, & ! node id 5
                    2.0,1.0,0.0, & ! node id 6
                    1.0,0.0,1.0, & ! node id 11
                    2.0,0.0,1.0, & ! node id 12
                    1.0,1.0,1.0, & ! node id 14
                    2.0,1.0,1.0/) ! node id 15

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1, & ! node id 6
                    0, & ! node id 11
                    1, & ! node id 12
                    0, & ! node id 14
                    1/)  ! node id 15

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element mask array
       allocate(elemMask(numElems))
       elemMask=(/0/)

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=8

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,13,14,16,17/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/0.0,1.0,0.0, & ! node id 4
                     1.0,1.0,0.0, & ! node id 5
                     0.0,2.0,0.0, & ! node id 7
                     1.0,2.0,0.0, & ! node id 8
                     0.0,1.0,1.0, & ! node id 13
                     1.0,1.0,1.0, & ! node id 14
                     0.0,2.0,1.0, & ! node id 16
                     1.0,2.0,1.0/) ! node id 17


        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     0, & ! node id 13
                     0, & ! node id 14
                     2, & ! node id 16
                     2/)  ! node id 17
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element mask array
       allocate(elemMask(numElems))
       elemMask=(/0/)

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1


     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=8

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9,14,15,17,18/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/1.0,1.0,0.0, &  ! node id 5
                     2.0,1.0,0.0, &  ! node id 6
                     1.0,2.0,0.0, &  ! node id 8
                     2.0,2.0,0.0, &  ! node id 9
                     1.0,1.0,1.0, &  ! node id 14
                     2.0,1.0,1.0, &  ! node id 15
                     1.0,2.0,1.0, &  ! node id 17
                     2.0,2.0,1.0/)  ! node id 18

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3, & ! node id 9
                     0, & ! node id 14
                     1, & ! node id 15
                     2, & ! node id 17
                     3/)  ! node id 18
 
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element mask array
       allocate(elemMask(numElems))
       elemMask=(/1/)

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

       endif
    endif
  

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=3,spatialDim=3, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         elementMask=elemMask, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
   srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source frac field
   srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  iconn=1
  do i1=1,numElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     z=0.0
     do i3=1,8  ! 8 is the number of nodes in each element
        inode=3*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        z=z+nodeCoords(inode+3)
        
        iconn=iconn+1
     enddo
     x=0.125*x
     y=0.125*y
     z=0.125*z

     ! Set source function
     ! (Set huge value for masked values, so it can be detected)
     if (elemMask(i1) .eq. 0) then
        srcFarrayPtr(i1) = 20.0+x+y+z
     else 
        srcFarrayPtr(i1) = 100000000.0
     endif
  enddo
 
   ! For now, Easy set interpolated function
   !srcFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)
   deallocate(elemMask)


  
  !!!!!!!!!!!!!!! setup destination Mesh !!!!!!!!!!!!!!!!!
  ! setup source Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=10

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9, &
               10/)

     ! Allocate and fill node coordinate array.
     ! Since this is a 3D Mesh the size is 3x the
     ! number of nodes.
     allocate(nodeCoords(3*numNodes))
     nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                  1.0,0.0,0.0, & ! node id 2
                  2.0,0.0,0.0, & ! node id 3
                  0.5,1.0,0.0, & ! node id 4
                  1.5,1.0,0.0, & ! node id 5
                  1.0,2.0,0.0, & ! node id 6
                  0.5,0.5,1.0, & ! node id 7
                  1.0,0.5,1.0, & ! node id 8
                  1.5,0.5,1.0, & ! node id 9
                  1.0,1.5,1.0/)  ! node id 10

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numElems=4

     ! Allocate and fill the element id array.
     allocate(elemIds(numElems))
     elemIds=(/1,2,3,4/) 

     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numElems))
     elemTypes=ESMF_MESHELEMTYPE_TETRA
                 

     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numElems))
     elemConn=(/1,2,7,4, &  ! elem id 1
                2,3,9,5, &  ! elem id 2
                2,5,8,4, &  ! elem id 3
                4,5,10,6/)  ! elem id 4

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,7/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes)) 
       nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                    1.0,0.0,0.0, & ! node id 2
                    0.5,1.0,0.0, & ! node id 4
                    0.5,0.5,1.0/) ! node id 7
 


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 7

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,9/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 3D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                    2.0,0.0,0.0, & ! node id 3
                    1.5,1.0,0.0, & ! node id 5
                    1.5,0.5,1.0/) ! node id 9


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    2, & ! node id 5
                    1/)  ! node id 9

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 1

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/2,4,5,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                  0.5,1.0,0.0, & ! node id 4
                  1.5,1.0,0.0, & ! node id 5
                  1.0,0.5,1.0/) ! node id 8



        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 2
                     0, & ! node id 4
                     2, & ! node id 5
                     2/)  ! node id 8

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 3

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,3,4,2/) ! elem id 3

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,6,10/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/0.5,1.0,0.0, & ! node id 4
                     1.5,1.0,0.0, & ! node id 5
                     1.0,2.0,0.0, & ! node id 6
                     1.0,1.5,1.0/)  ! node id 10



        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     2, & ! node id 5
                     3, & ! node id 6
                     3/)  ! node id 10
 
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 4

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 4

       endif
    endif

   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=3,spatialDim=3, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Array spec
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)


  ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="dest_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="dest_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create exact dest. field
  xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="xdest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Init destination field to 0.0
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif
  
   ! Init destination field to 0.0
   dstFarrayPtr=0.0


  ! Init exact destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  iconn=1
  do i1=1,numElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     z=0.0
     do i3=1,4  ! 4 is the number of nodes in each element
        inode=3*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        z=z+nodeCoords(inode+3)
        
        iconn=iconn+1
     enddo
     x=0.25*x
     y=0.25*y
     z=0.25*z

     ! Set source function
     xdstFarrayPtr(i1) = 20.0+x+y+z
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)




  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif



  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     if (dstFracptr(i1) .lt. 0.999) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== 3D Cartesian Mesh ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return success if we've gotten this far
    rc=ESMF_SUCCESS

 end subroutine test_RegridCsrv3DCartMesh



      subroutine test_csrvregridwmasksUserArea(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount
  integer :: src_num, dst_num
  integer :: tmp(1), gtmp(1)

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 180
  Src_ny = 100

  Src_dx = 360.0/Src_nx
  Src_dy = 180.0/Src_ny

  Dst_nx = 100
  Dst_ny = 80

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Add Area
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_AREA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_AREA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! (Get memory and set coords for src)
  src_num=0
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Src_dy - 90.0
     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Src_dy - 90.0
        yp1= REAL(i2-1+1)*Src_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx + 0.5*Src_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! set src data 
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
       ! farrayPtr(i1,i2) = 1.
       farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

#if 1
        if ((lat>-45) .and. (lat<45)) then
           srcMask(i1,i2)=1
        else
           srcMask(i1,i2)=0
        endif
#else
           srcMask(i1,i2)=0
#endif      
  
        ! Set user area
        src_num=src_num+1
     enddo
     enddo
  enddo    ! lDE


  ! Sum number of src elements
  tmp(1)=src_num
  call ESMF_VMAllReduce(vm, tmp, gtmp, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
  src_num=gtmp(1)


  ! Set src areas 
  do lDE=0,localDECount-1

     !! Get area pointer
     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_AREA, farrayPtr=srcAreaPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! Set area, so total area is 1.0
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
           
        ! Set user area
        srcAreaPtr(i1,i2)=1.0/REAL(src_num)
           
     enddo
     enddo
  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Get memory and set coords for dst
  dst_num=0
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Dst_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Dst_dy - 90.0
        yp1= REAL(i2-1+1)*Dst_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
       ! xfarrayPtr(i1,i2) = 1.0


#if 1
        if ((lon>-45) .and. (lon<45)) then
           dstMask(i1,i2)=1
        else 
           dstMask(i1,i2)=0
        endif
#else
           dstMask(i1,i2)=0
#endif

           dst_num=dst_num+1
     enddo
     enddo
  enddo    ! lDE


  ! Sum number of dst elements
  tmp(1)=dst_num
  call ESMF_VMAllReduce(vm, tmp, gtmp, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
  dst_num=gtmp(1)


  ! Set dst areas 
  do lDE=0,localDECount-1

     !! Get dst area
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_AREA, farrayPtr=dstAreaPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

    !! Set dst area so total is 1.0
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        dstAreaPtr(i1,i2)=1.0/REAL(dst_num)
     enddo
     enddo
  enddo    ! lDE




  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField, dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  dstmass = 0.
  do lDE=0,localDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
         
        ! skip if masked
        if (dstMask(i1,i2) .eq. 1) cycle

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)


        ! If this destination cell isn't covered by a sig. amount of source, then don't compute error on it.
        ! (Note that this is what SCRIP does)
        if (dstFracptr(i1,i2) .lt. 0.999) cycle

        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2)/dstFracPtr(i1,i2)) &
                                       - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2)/dstFracPtr(i1,i2)) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif

     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo

  enddo    ! lDE


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Spherical grids with masks and user area ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_csrvregridwmasksUserArea


 subroutine test_ignoreDegenerate(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif


  ! Setup Src Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/1.0,1.0, & ! node id 1
                   1.0,0.0, & ! node id 2
                   2.0,0.0, & ! node id 3
                   0.0, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.0, 1.0, & ! node id 6
                   0.0, 2.0, & ! node id 7
                   1.0, 1.0, & ! node id 8
                   2.0, 1.0 /) ! node id 9

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numQuadElems=3
     numTriElems=2
     numTotElems=numQuadElems+numTriElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numQuadElems+3*numTriElems))
     elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,9,8/)   ! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0, 1.0, & ! node id 1
                     1.0, 0.0, & ! node id 2
                    0.0,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
       numTriElems=0
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.0, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                      0.0,2.0, & ! node id 7
                      1.0,1.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,1.0, &  ! node id 8
                     2.0,1.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
               meshLoc=ESMF_MESHLOC_ELEMENT, &
               name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


  ! Create Dest Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
                  2.0,2.0 /) ! node id 9

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numQuadElems=3
     numTriElems=2
     numTotElems=numQuadElems+numTriElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numQuadElems+3*numTriElems))
     elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,9,8/)   ! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
       numTriElems=0
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
   endif


  ! Create Mesh structure in 1 step
  dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
       coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
     endif

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                     meshLoc=ESMF_MESHLOC_ELEMENT, &
                     name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          ignoreDegenerate=.true., &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)



  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, dstMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_ignoreDegenerate



 subroutine test_RegridCsrvCartPHMesh(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
  integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
   real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn

  ! result code
  integer :: finalrc
  
  ! Init to success
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

  ! Setup source mesh

!             Src Mesh
!
!  2.5        8        10 --------11   
!          /     \   /            |
!  2.1   7         9              12
!        |         |      5       /
!        |    4    |            /
 !        |         |          /
!  1.0   4 ------- 5 ------- 6
!        |         |  \   3  |
!        |    1    |    \    |
!        |         |  2   \  |
! -0.1   1 ------- 2 ------- 3
!
!      -0.1       1.0       2.1   2.5 
! 
!        Node Id labels at corners
!       Element Id labels in centers


  ! Setup Src Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=12

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/-0.1,-0.1, & ! node id 1
                   1.0,-0.1, & ! node id 2
                   2.1,-0.1, & ! node id 3
                  -0.1, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.1, 1.0, & ! node id 6
                  -0.1, 2.1, & ! node id 7
                   0.5, 2.5, & ! node id 8
                   1.0, 2.1, & ! node id 9
                   1.5, 2.5, & ! node id 10
                   2.5, 2.5, & ! node id 11
                   2.5, 2.1/)  ! node id 12

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus tot and num conn.
     numQuadElems=1
     numTriElems=2
      numPentElems=1
     numHexElems=1
     numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
     numElemConn=3*numTriElems+4*numQuadElems+ &
                 5*numPentElems+6*numHexElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 

     ! Allocate and fill the element Mask array.
     allocate(elemMask(numTotElems))
     elemMask=(/0,1,0,0,0/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 5, &                      ! elem id 4
                 6/)                       ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(numElemConn))
     elemConn=(/1,2,5,4, &       ! elem id 1
                2,3,5,   &       ! elem id 2
                3,6,5,   &       ! elem id 3
                4,5,9,8,7, &     ! elem id 4
                5,6,12,11,10,9/) ! elem id 5

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 
 
       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/-0.1, -0.1, & ! node id 1
                     1.0, -0.1, & ! node id 2
                    -0.1,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=1
       numTriElems=0
       numPentElems=0
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems
       
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element Mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0/) 


       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4
 
       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,-0.1, & ! node id 2
                    2.1,-0.1, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.1, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=2
       numPentElems=0
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/)

       ! Allocate and fill the element Mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/1,0/)  

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3
 
    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=5

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      0.5,2.5, & ! node id 8
                      1.0,2.1 /) ! node id 9

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     2/)  ! node id 9

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=0
       numPentElems=1
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

       ! Allocate and fill the element Mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
         elemTypes=(/5/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,5,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=6

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,9,10,11,12/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 9
                     1.5,2.5, &  ! node id 10
                     2.5,2.5, &  ! node id 11
                     2.5,2.1 /)  ! node id 12

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 9
                     3, & ! node id 10
                     3, & ! node id 11
                     3/)  ! node id 12
 

        ! Set the number of each type of element, plus tot and num conn.
        numQuadElems=0
        numTriElems=0
        numPentElems=0
        numHexElems=1
        numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+6*numHexElems


        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
         elemIds=(/5/)  

       ! Allocate and fill the element Mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/6/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,6,5,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         elementMask=elemMask, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
   srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

 
  ! Create source frac field
   srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     ! (Set huge value for masked values, so it can be detected)
     if (elemMask(i1) .eq. 0) then
        srcFarrayPtr(i1) = 20.0+x+y
     else 
        srcFarrayPtr(i1) = 100000000.0
     endif
  enddo
 
    ! For now, Easy set interpolated function
   !srcFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)
   deallocate(elemMask)


  
  !!!!!!!!!!!!!!! Setup Destination Mesh !!!!!!!!!!!!!!!!!

   !              Dest Mesh
   !
   !  2.0   7 ------- 8 ------- 9
   !        |         |           \
   !  1.5   |    4    |    5        10
   !        |         |           /
   !  1.0   4 ------- 5 ------- 6
   !        |         |  \   3  |
   !        |    1    |    \    |
   !        |         |  2   \  |
   !  0.0   1 ------- 2 ------- 3  
   !
   !       0.0       1.0        2.0  2.25
   ! 
   !        Node Id labels at corners
   !       Element Id labels in centers
   
   ! Create Dest Mesh
   if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=10

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
                  2.0,2.0, & ! node id 9
                  2.25,1.5 /) ! node id 10

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numTriElems=2
     numQuadElems=2
     numPentElems=1
     numTotElems=numTriElems+numQuadElems+numPentElems
     numElemConn=3*numTriElems+4*numQuadElems+ &
                 5*numPentElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 5/)                       ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
      allocate(elemConn(numElemConn))
     elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,10,9,8/)! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numTriElems=0
       numQuadElems=1
       numPentElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
                   5*numPentElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numTriElems=2
       numQuadElems=0
       numPentElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
                   5*numPentElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                    ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numTriElems=0
        numQuadElems=1
        numPentElems=0
        numTotElems=numTriElems+numQuadElems+numPentElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
                    5*numPentElems


        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
         elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=5

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9,10/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0, &  ! node id 9
                     2.25,1.5 /) ! node id 10

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3, & ! node id 9
                     3/)  ! node id 10
 
        ! Set the number of each type of element, plus the total number.
        numTriElems=0
        numQuadElems=0
        numPentElems=1
        numTotElems=numTriElems+numQuadElems+numPentElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
                    5*numPentElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numElemConn))
        elemTypes=(/5/) ! elem id 5
 
        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,5,4,3/) ! elem id 5
       endif
   endif


   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   



   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
      x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     xdstFarrayPtr(i1) = 20.0+x+y
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

 
  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif



  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
 
  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     if (dstFracptr(i1) .lt. 0.999) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
            endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Mesh with Pentagons and Hexagons ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

 
  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
   call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return success if we've gotten this far
    rc=ESMF_SUCCESS

 end subroutine test_RegridCsrvCartPHMesh


subroutine test_sph_csrv_w_frac_norm(itrp, csrv, rc)
  logical, intent(out)  :: itrp
  logical, intent(out)  :: csrv
  integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 180
  Src_ny = 100

  Src_dx = 360.0/Src_nx
  Src_dy = 180.0/Src_ny

  Dst_nx = 100
  Dst_ny = 80

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Src_dy - 90.0
     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Src_dy - 90.0
        yp1= REAL(i2-1+1)*Src_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx + 0.5*Src_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! set src data 
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
       ! farrayPtr(i1,i2) = 1.
       farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        if ((lat>-45) .and. (lat<45)) then
           srcMask(i1,i2)=1
        else
           srcMask(i1,i2)=0
        endif
        
     enddo
     enddo
  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Dst_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Dst_dy - 90.0
        yp1= REAL(i2-1+1)*Dst_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
       ! xfarrayPtr(i1,i2) = 1.0

        if ((lon>-45) .and. (lon<45)) then
           dstMask(i1,i2)=1
        else 
           dstMask(i1,i2)=0
        endif

     enddo
     enddo


  enddo    ! lDE


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField, dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          normType=ESMF_NORMTYPE_FRACAREA, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     minerror(1) = 100000.
     maxerror(1) = 0.
     error = 0.
     dstmass = 0.
 
     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! skip if masked
        if (dstMask(i1,i2) .eq. 1) cycle

        ! Calculate total mass
        ! NOTE: DO need to include dstFrac here, because the frac has been included in the weights 
        dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*farrayPtr(i1,i2)


        ! If this destination cell isn't covered by a reasonable amount of source, then compute error on it.
        ! (Note that this is what SCRIP does)
         if (dstFracptr(i1,i2) .lt. 0.001) cycle

         if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) &
                                     - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2)/dstFracPtr(i1,i2)) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif

     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo

  enddo    ! lDE


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Spherical grids with frac normalization ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_sph_csrv_w_frac_norm


 subroutine test_RegridCsrvCartPHFracNorm(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
  integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn

  ! result code
  integer :: finalrc
  
  ! Init to success
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

  ! Setup source mesh

!             Src Mesh
!
!  2.5        8        10 --------11   
!          /     \   /            |
!  2.1   7         9              12
!        |         |      5       /
!        |    4    |            /
!        |         |          /
!  1.0   4 ------- 5 ------- 6
!        |         |  \   3  |
!        |    1    |    \    |
!        |         |  2   \  |
! -0.1   1 ------- 2 ------- 3
!
!      -0.1       1.0       2.1   2.5 
! 
!        Node Id labels at corners
!       Element Id labels in centers


  ! Setup Src Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=12

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/-0.1,-0.1, & ! node id 1
                   1.0,-0.1, & ! node id 2
                   2.1,-0.1, & ! node id 3
                  -0.1, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.1, 1.0, & ! node id 6
                  -0.1, 2.1, & ! node id 7
                   0.5, 2.5, & ! node id 8
                   1.0, 2.1, & ! node id 9
                   1.5, 2.5, & ! node id 10
                   2.5, 2.5, & ! node id 11
                   2.5, 2.1/)  ! node id 12

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus tot and num conn.
     numQuadElems=1
     numTriElems=2
     numPentElems=1
     numHexElems=1
     numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
     numElemConn=3*numTriElems+4*numQuadElems+ &
                 5*numPentElems+6*numHexElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 

     ! Allocate and fill the element Mask array.
     allocate(elemMask(numTotElems))
     elemMask=(/0,1,0,0,0/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 5, &                      ! elem id 4
                 6/)                       ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(numElemConn))
     elemConn=(/1,2,5,4, &       ! elem id 1
                2,3,5,   &       ! elem id 2
                3,6,5,   &       ! elem id 3
                4,5,9,8,7, &     ! elem id 4
                5,6,12,11,10,9/) ! elem id 5

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/-0.1, -0.1, & ! node id 1
                     1.0, -0.1, & ! node id 2
                    -0.1,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=1
       numTriElems=0
       numPentElems=0
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems
       
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element Mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0/) 


       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,-0.1, & ! node id 2
                    2.1,-0.1, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.1, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=2
       numPentElems=0
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/)

       ! Allocate and fill the element Mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/1,0/)  

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=5

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      0.5,2.5, & ! node id 8
                      1.0,2.1 /) ! node id 9

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     2/)  ! node id 9

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=0
       numPentElems=1
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

       ! Allocate and fill the element Mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/5/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,5,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=6

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,9,10,11,12/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 9
                     1.5,2.5, &  ! node id 10
                     2.5,2.5, &  ! node id 11
                     2.5,2.1 /)  ! node id 12

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 9
                     3, & ! node id 10
                     3, & ! node id 11
                     3/)  ! node id 12
 

        ! Set the number of each type of element, plus tot and num conn.
        numQuadElems=0
        numTriElems=0
        numPentElems=0
        numHexElems=1
        numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+6*numHexElems


        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

       ! Allocate and fill the element Mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/6/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,6,5,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         elementMask=elemMask, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
   srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source frac field
   srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     ! (Set huge value for masked values, so it can be detected)
     if (elemMask(i1) .eq. 0) then
        srcFarrayPtr(i1) = 20.0+x+y
     else 
        srcFarrayPtr(i1) = 100000000.0
     endif
  enddo
 
   ! For now, Easy set interpolated function
   !srcFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)
   deallocate(elemMask)


  
  !!!!!!!!!!!!!!! Setup Destination Mesh !!!!!!!!!!!!!!!!!

   !              Dest Mesh
   !
   !  2.0   7 ------- 8 ------- 9
   !        |         |           \
   !  1.5   |    4    |    5        10
   !        |         |           /
   !  1.0   4 ------- 5 ------- 6
   !        |         |  \   3  |
   !        |    1    |    \    |
   !        |         |  2   \  |
   !  0.0   1 ------- 2 ------- 3  
   !
   !       0.0       1.0        2.0  3.0
   ! 
   !        Node Id labels at corners
   !       Element Id labels in centers
   
   ! Create Dest Mesh
   if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=10

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
                  2.0,2.0, & ! node id 9
                  3.0,1.5 /) ! node id 10

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numTriElems=2
     numQuadElems=2
     numPentElems=1
     numTotElems=numTriElems+numQuadElems+numPentElems
     numElemConn=3*numTriElems+4*numQuadElems+ &
                 5*numPentElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 5/)                       ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(numElemConn))
     elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,10,9,8/)! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numTriElems=0
       numQuadElems=1
       numPentElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
                   5*numPentElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numTriElems=2
       numQuadElems=0
       numPentElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
                   5*numPentElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numTriElems=0
        numQuadElems=1
        numPentElems=0
        numTotElems=numTriElems+numQuadElems+numPentElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
                    5*numPentElems


        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=5

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9,10/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0, &  ! node id 9
                     3.0,1.5 /) ! node id 10

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3, & ! node id 9
                     3/)  ! node id 10
 
        ! Set the number of each type of element, plus the total number.
        numTriElems=0
        numQuadElems=0
        numPentElems=1
        numTotElems=numTriElems+numQuadElems+numPentElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
                    5*numPentElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numElemConn))
        elemTypes=(/5/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,5,4,3/) ! elem id 5
       endif
   endif


   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   



   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     xdstFarrayPtr(i1) = 20.0+x+y
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          normType=ESMF_NORMTYPE_FRACAREA, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif



  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! Here do include frac, because we're doing frac norm option
     dstmass = dstmass + dstFracptr(i1)*dstAreaptr(i1)*dstFarrayPtr(i1)
     

     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     if (dstFracptr(i1) .lt. 0.1) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Mesh with Pentagons and Hexagons using frac. normalization ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return success if we've gotten this far
    rc=ESMF_SUCCESS

 end subroutine test_RegridCsrvCartPHFracNorm


 subroutine test_RegridCart4ConcaveMesh(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
  integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn

  ! result code
  integer :: finalrc

  ! Init to success
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

 ! XMRKX

!!!! Setup source mesh !!!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   2.0   7 ------- 8 
  !         |         |\
  !   1.3   |    3    |  9 
  !         |         |4    \
  !   1.0   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       1.0 1.3   2.0 
   !
  !      Node Ids at corners
  !      Element Ids in centers
  ! 
  !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !             Mesh Owners
  !
  !   2.0  2 ------- 2 
  !        |         |\
  !   1.3  |    2    |  3 
  !        |         |3   \
  !   1.0  0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0       1.0 1.3    2.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

  ! Setup mesh info depending on the 
  ! number of PETs
  if (petCount .eq. 1) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, &
                 1.0,0.0, &
                 2.0,0.0, &
                 0.0,1.0, &
                 1.0,1.0, &
                 2.0,1.0, &
                 0.0,2.0, &
                  1.0,2.0, &
                 1.3,1.3 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

        ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
      else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    1.3,1.3/)
 
       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif


   ! Create Mesh structure in 1 step
  srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
       coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        rc=rc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
    srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
   srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1)
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))

     ! Set source function
     srcFarrayPtr(i1) = 20.0+x+y
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

  !!!!!!!!!!!!!!! Setup Destination Mesh !!!!!!!!!!!!!!!!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   2.0   7 ------- 8 
  !         |         |\
  !   1.4   |    3    |  9 
  !         |         |4    \
  !   1.0   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       1.0 1.4   2.0 
  !
  !      Node Ids at corners
  !      Element Ids in centers
  ! 
  !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !             Mesh Owners
  !
  !   2.0  2 ------- 2 
  !        |         |\
  !   1.4  |    2    |  3 
  !        |         |3   \
  !   1.0  0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0       1.0 1.4    2.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

  ! Setup mesh info depending on the 
  ! number of PETs
  if (petCount .eq. 1) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, &
                 1.0,0.0, &
                 2.0,0.0, &
                 0.0,1.0, &
                 1.0,1.0, &
                 2.0,1.0, &
                 0.0,2.0, &
                 1.0,2.0, &
                 1.4,1.4 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    1.4,1.4/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif


   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     xdstFarrayPtr(i1) = 20.0+x+y
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif



  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     if (dstFracptr(i1) .lt. 0.999) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Mesh with concave quads ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return success if we've gotten this far
    rc=ESMF_SUCCESS

 end subroutine test_RegridCart4ConcaveMesh

 subroutine test_RegridSph4ConcaveMesh(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
  integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn

  ! result code
  integer :: finalrc

  ! Init to success
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

 ! XMRKX

!!!! Setup source mesh !!!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   20   7 ------- 8 
  !         |         |\
  !   13    |    3    |  9 
  !         |         |4    \
  !   10   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       10  13     20 
  !
  !      Node Ids at corners
  !      Element Ids in centers
  ! 
  !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !             Mesh Owners
  !
  !   20  2 ------- 2 
  !        |         |\
  !   12   |    2    |  3 
  !        |         |3   \
  !   10   0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0       10  12      20 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

  ! Setup mesh info depending on the 
  ! number of PETs
  if (petCount .eq. 1) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, &
                 10.0,0.0, &
                 20.0,0.0, &
                 0.0,10.0, &
                 10.0,10.0, &
                 20.0,10.0, &
                 0.0,20.0, &
                 10.0,20.0, &
                 12.0,12.0 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    10.0,0.0, &
                    0.0,10.0, &
                    10.0,10.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/10.0,0.0, &
                    20.0,0.0, &
                    10.0,10.0, &
                    20.0,10.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,10.0, &
                    10.0,10.0, &
                    0.0,20.0, &
                    10.0,20.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/10.0,10.0, &
                    20.0,10.0, &
                    10.0,20.0, &
                    12.0,12.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif


   ! Create Mesh structure in 1 step
  srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        rc=rc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
   srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
   srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1)
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))

     ! Set source function
     srcFarrayPtr(i1) = 20.0+x+y
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

  !!!!!!!!!!!!!!! Setup Destination Mesh !!!!!!!!!!!!!!!!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   20.0   7 ------- 8 
  !         |         |\
  !   14.0  |    3    |  9 
  !         |         |4    \
  !   10.0  4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0      10.0 14.0   20.0 
  !
  !      Node Ids at corners
  !      Element Ids in centers
  ! 
  !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !             Mesh Owners
  !
  !   20.0 2 ------- 2 
  !        |         |\
  !   14.0 |    2    |  3 
  !        |         |3   \
  !   10.0 0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0      10.0 14.0    20.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

  ! Setup mesh info depending on the 
  ! number of PETs
  if (petCount .eq. 1) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, &
                 10.0,0.0, &
                 20.0,0.0, &
                 0.0,10.0, &
                 10.0,10.0, &
                 20.0,10.0, &
                 0.0,20.0, &
                 10.0,20.0, &
                 14.0,14.0 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    10.0,0.0, &
                    0.0,10.0, &
                    10.0,10.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/10.0,0.0, &
                    20.0,0.0, &
                    10.0,10.0, &
                    20.0,10.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,10.0, &
                    10.0,10.0, &
                    0.0,20.0, &
                    10.0,20.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/10.0,10.0, &
                    20.0,10.0, &
                    10.0,20.0, &
                    14.0,14.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif


   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     xdstFarrayPtr(i1) = 20.0+x+y
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif



  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     if (dstFracptr(i1) .lt. 0.999) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Mesh with concave quads ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return success if we've gotten this far
    rc=ESMF_SUCCESS

 end subroutine test_RegridSph4ConcaveMesh



 subroutine test_MOABMeshToMesh(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
   integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
 
  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn

  ! result code
  integer :: finalrc

  ! Init to success
  rc=ESMF_SUCCESS
  itrp=.true.
  csrv=.true.

  ! Don't do the test is MOAB isn't available
#ifdef ESMF_MOAB

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 PET then exit successfully
  if (petCount .ne. 1) then
    itrp = .true.
    csrv = .true.
    rc=ESMF_SUCCESS
    return
  endif

#if 0
  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif
#endif

  call ESMF_MeshSetMOAB(.true., rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

 ! XMRKX

!!!! Setup source mesh !!!!

  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/0.0,0.0, & ! node id 1
                   1.0,0.0, & ! node id 2
                   2.0,0.0, & ! node id 3
                    0.0, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.0, 1.0, & ! node id 6
                   0.0, 2.0, & ! node id 7
                   1.0, 2.0, & ! node id 8
                   2.0, 2.0 /) ! node id 9

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numQuadElems=3
     numTriElems=2
     numTotElems=numQuadElems+numTriElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
      ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,9,8/)   ! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/0.0, 0.0, & ! node id 1
                     1.0, 0.0, & ! node id 2
                    0.0,  1.0, & ! node id 4
                      1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
       numTriElems=0
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.0, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
        numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

         ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                      0.0,2.0, & ! node id 7
                      1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                      2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
         allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        rc=rc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
   srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
    srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
   ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1)
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))

     ! Set source function
     srcFarrayPtr(i1) = 20.0+x+y
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
    deallocate(elemConn)

  !!!!!!!!!!!!!!! Setup Destination Mesh !!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
   !              Mesh Ids
  !
  !   2.0   7 ------- 8 -------- 9
  !         |         |          |
  !         |    3    |    4     |
  !         |         |          |
  !   1.0   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       1.0        2.0 
  !
  !      Node Ids at corners
  !      Element Ids in centers
  ! 
  !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !             Mesh Owners
  !
  !   2.0  2 ------- 2--------- 3 
  !        |         |          | 
  !        |    2    |    3     |
  !        |         |          |
  !   1.0  0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0       1.0        2.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

  ! Setup mesh info depending on the 
  ! number of PETs
  if (petCount .eq. 1) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 
     
      !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, &
                 1.0,0.0, &
                 2.0,0.0, &
                 0.0,1.0, &
                 1.0,1.0, &
                 2.0,1.0, &
                 0.0,2.0, &
                 1.0,2.0, &
                 2.0,2.0 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

       !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)
 
       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

        !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
        elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numTotElems=1
 
       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    2.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif
 
 ! XMRKX

   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_MeshSetMOAB(.false., rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif   

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     xdstFarrayPtr(i1) = 20.0+x+y
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


#if 1
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
! COMMENT THESE OUT UNTIL THAT PART IS WORKING
!          dstFracField=dstFracField, &
!          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

#if 1
  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif
#endif

  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     !if (dstFracptr(i1) .lt. 0.999) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
!     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
     srcmass(1) = srcmass(1) + srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Mesh with concave quads ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

#endif
   ! rc, itrp, csrv init to success above

 end subroutine test_MOABMeshToMesh

                
 subroutine test_RegridCsrvCartMultiPoly(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
  integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
    real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode, num_conn
  integer :: numQuadElems,numTriElems,numMultiElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn

  ! result code
  integer :: finalrc
  
  ! Init to success
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

  ! Setup source mesh
!
!  2.5        8        10 --------11   
!          /     \         \      |
!  2.1   7         9           \  12
!        |         | \    5       
!        |    4    |   \         
!        |         |      \    
!  1.0   4 ------- 5 ------- 6
!        |         |  \   3  |
!        |    1    |    \    |
!        |         |  2   \  |
! -0.1   1 ------- 2 ------- 3
!
!      -0.1       1.0       2.1   2.5 
! 
!        Node Id labels at corners
!       Element Id labels in centers
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=12

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/-0.1,-0.1, & ! node id 1
                   1.0,-0.1, & ! node id 2
                   2.1,-0.1, & ! node id 3
                  -0.1, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.1, 1.0, & ! node id 6
                  -0.1, 2.1, & ! node id 7
                   0.5, 2.5, & ! node id 8
                   1.0, 2.1, & ! node id 9
                   1.5, 2.5, & ! node id 10
                   2.5, 2.5, & ! node id 11
                   2.5, 2.1/)  ! node id 12

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0
 
     ! Set the number of each type of element, plus tot and num conn.
     numQuadElems=1
     numTriElems=2
     numPentElems=1
     numMultiElems=1
     numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
     numElemConn=3*numTriElems+4*numQuadElems+ &
                 5*numPentElems+7*numMultiElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 5, &                      ! elem id 4
                 7/)                       ! elem id 5


     ! Allocate and fill the element connection type array.
      ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(numElemConn))
     elemConn=(/1,2,5,4, &       ! elem id 1
                2,3,5,   &       ! elem id 2
                3,6,5,   &       ! elem id 3
                4,5,9,8,7, &     ! elem id 4
                5,6,9,ESMF_MESH_POLYBREAK,12,11,10/) ! elem id 5

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
        ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 
 
       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/-0.1, -0.1, & ! node id 1
                     1.0, -0.1, & ! node id 2
                    -0.1,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                     0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=1
       numTriElems=0
       numPentElems=0
       numMultiElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+7*numMultiElems
       
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

        ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
        allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,-0.1, & ! node id 2
                    2.1,-0.1, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.1, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                     1/)  ! node id 6

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=2
       numPentElems=0
       numMultiElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+7*numMultiElems
 
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=5

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,9/) 

         ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      0.5,2.5, & ! node id 8
                      1.0,2.1 /) ! node id 9

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
         allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                      0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     2/)  ! node id 9

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=0
       numPentElems=1
       numMultiElems=0
        numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+7*numMultiElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/5/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,5,4,3/) ! elem id 4

      else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=6

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,9,10,11,12/) 
 
        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 9
                     1.5,2.5, &  ! node id 10
                     2.5,2.5, &  ! node id 11
                     2.5,2.1 /)  ! node id 12

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
         nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 9
                     3, & ! node id 10
                     3, & ! node id 11
                     3/)  ! node id 12
 
 
        ! Set the number of each type of element, plus tot and num conn.
        numQuadElems=0
         numTriElems=0
        numPentElems=0
        numMultiElems=1
        numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+7*numMultiElems


        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/7/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,3,ESMF_MESH_POLYBREAK,6,5,4/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
    srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

 
  ! Create source frac field
    srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     num_conn=0
     do i2=1,elemTypes(i1)
         if (elemConn(iconn) .ne. ESMF_MESH_POLYBREAK) then
           inode=2*(elemConn(iconn)-1)
           x=x+nodeCoords(inode+1)
           y=y+nodeCoords(inode+2)
           num_conn=num_conn+1
        endif
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(num_conn,ESMF_KIND_R8))
     y=y*(1.0/REAL(num_conn,ESMF_KIND_R8))

     ! Set source function
     srcFarrayPtr(i1) = 20.0+x+y
  enddo
 

 ! XMRKX

   ! For now, Easy set interpolated function
   !srcFarrayPtr=1.0
 

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

  
  !!!!!!!!!!!!!!! Setup Destination Mesh !!!!!!!!!!!!!!!!!
!
!  2.5        8        10 --------11   
!          /     \         \      |
!  2.1   7         9           \  12
!        |         | \    5       
!        |    4    |   \         
!        |         |      \    
!  1.0   4 ------- 5 ------- 6
!        |         |  \   3  |
!        |    1    |    \    |
!        |         |  2   \  |
! -0.1   1 ------- 2 ------- 3
!
!      -0.1       1.0       2.1   2.5 
! 
!        Node Id labels at corners
!       Element Id labels in centers
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=12

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/-0.1,-0.1, & ! node id 1
                   1.0,-0.1, & ! node id 2
                   2.1,-0.1, & ! node id 3
                  -0.1, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.1, 1.0, & ! node id 6
                  -0.1, 2.1, & ! node id 7
                   0.5, 2.5, & ! node id 8
                   1.0, 2.1, & ! node id 9
                   1.5, 2.5, & ! node id 10
                   2.5, 2.5, & ! node id 11
                   2.5, 2.1/)  ! node id 12

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0
 
     ! Set the number of each type of element, plus tot and num conn.
     numQuadElems=1
     numTriElems=2
     numPentElems=1
     numMultiElems=1
     numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
     numElemConn=3*numTriElems+4*numQuadElems+ &
                 5*numPentElems+7*numMultiElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 5, &                      ! elem id 4
                 7/)                       ! elem id 5


     ! Allocate and fill the element connection type array.
      ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(numElemConn))
     elemConn=(/1,2,5,4, &       ! elem id 1
                2,3,5,   &       ! elem id 2
                3,6,5,   &       ! elem id 3
                4,5,9,8,7, &     ! elem id 4
                5,6,9,ESMF_MESH_POLYBREAK,12,11,10/) ! elem id 5

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
        ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 
 
       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/-0.1, -0.1, & ! node id 1
                     1.0, -0.1, & ! node id 2
                    -0.1,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=1
       numTriElems=0
       numPentElems=0
       numMultiElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+7*numMultiElems
       
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

        ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
        allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,-0.1, & ! node id 2
                    2.1,-0.1, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.1, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=2
       numPentElems=0
       numMultiElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+7*numMultiElems
 
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=5

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,9/) 

         ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      0.5,2.5, & ! node id 8
                      1.0,2.1 /) ! node id 9

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
         allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     2/)  ! node id 9

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=0
       numPentElems=1
       numMultiElems=0
        numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+7*numMultiElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/5/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,5,4,3/) ! elem id 4

      else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=6

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,9,10,11,12/) 
 
        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 9
                     1.5,2.5, &  ! node id 10
                     2.5,2.5, &  ! node id 11
                     2.5,2.1 /)  ! node id 12

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 9
                     3, & ! node id 10
                     3, & ! node id 11
                     3/)  ! node id 12
 
 
        ! Set the number of each type of element, plus tot and num conn.
        numQuadElems=0
         numTriElems=0
        numPentElems=0
        numMultiElems=1
        numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+7*numMultiElems


        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/7/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,3,ESMF_MESH_POLYBREAK,6,5,4/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Set exact dest value
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     num_conn=0
     do i2=1,elemTypes(i1)
         if (elemConn(iconn) .ne. ESMF_MESH_POLYBREAK) then
           inode=2*(elemConn(iconn)-1)
           x=x+nodeCoords(inode+1)
           y=y+nodeCoords(inode+2)
           num_conn=num_conn+1
        endif
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(num_conn,ESMF_KIND_R8))
     y=y*(1.0/REAL(num_conn,ESMF_KIND_R8))

     ! Set exact function
     xdstFarrayPtr(i1) = 20.0+x+y
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

 
  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif



  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
 
  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     if (dstFracptr(i1) .lt. 0.999) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
            endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Mesh containing a Multipolygon Element ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

 
  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
   call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return success if we've gotten this far
    rc=ESMF_SUCCESS

 end subroutine test_RegridCsrvCartMultiPoly



end program ESMF_FieldRegridCsrvUTest
