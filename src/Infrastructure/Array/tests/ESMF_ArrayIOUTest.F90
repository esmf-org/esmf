! $Id: ESMF_ArrayIOUTest.F90,v 1.22 2010/09/24 04:26:24 theurich Exp $
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
!
program ESMF_ArrayIOUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_PIOArrayUTest -  Tests ArrayWrite() and ArrayRead()
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
 
  ! local variables
  type(ESMF_VM):: vm
  type(ESMF_ArraySpec):: arrayspec
  integer(ESMF_KIND_I4), pointer, dimension(:,:,:) ::  Farray3D_withhalo, &
               Farray3D_wouthalo, Farray3D_withhalo2, &
               Farray3D_wouthalo2, Farray3D_withhalo3
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray2D_withhalo, Farray2D_wouthalo
  real(ESMF_KIND_R8), dimension(5,5) :: FarrayGr_1 , FarrayGr_2
  type(ESMF_DistGrid)                     :: distgrid, distgrid_diff
  type(ESMF_Array)                        :: array_withhalo, array_wouthalo
  type(ESMF_Array)                        :: array_withhalo2, array_wouthalo2
  type(ESMF_Array)                        :: array_withhalo3
  type(ESMF_Array)                        :: array_diff
  integer                                 :: rc, de
  integer, allocatable :: totalLWidth(:), totalUWidth(:), &
                       computationalLWidth(:),computationalUWidth(:)
  integer, allocatable :: exclusiveLBound(:,:), exclusiveUBound(:,:)
  integer      :: localDeCount, localPet, petCount
  integer :: i,j,k
  real :: Maxvalue(1), diff

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  !-----------------------------------------------------------------------------

  ! Set up
  ! *******
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  if (petCount /= 6) then
    print *, "This system test needs to run on exactly 6 PETs, petCount = ", &
      petCount
    goto 10
  endif

!-------------------------------------------------------------------------------
!
! Tests: 3D case (Integer)
! Write Fortran data (Farray3D_*halo) from an ESMF_Array with/without Halos
! Then read them back in to a new array (Farray3D_*halo2), and
! compare the read in data and the original data
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), &
              maxIndex=(/5,5,5/), regDecomp=(/2,3,1/),  rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Spec Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4,   &
                         rank=3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array with Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(computationalLWidth(3)) ! dimCount=3
  allocate(computationalUWidth(3)) ! dimCount=3
  allocate(totalLWidth(3))         ! dimCount=3
  allocate(totalUWidth(3))         ! dimCount=3
  array_withhalo = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,3,1/), computationalUWidth=(/1,1,2/), &
          totalLWidth=(/1,7,1/), totalUWidth=(/4,2,3/), &
          indexflag=ESMF_INDEX_GLOBAL, name="temperature", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_wouthalo = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='velocity', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!-------------------------------------------------------------------------------
! !  Get Fortran pointer to Array data
! !  Data is type ESMF_KIND_I4
  localDeCount = 1
  call ESMF_ArrayGet(array_withhalo, localDe=0, farrayPtr=Farray3D_withhalo, rc=rc)
  call ESMF_ArrayGet(array_wouthalo, localDe=0, farrayPtr=Farray3D_wouthalo, rc=rc)

  localDeCount = 1
  allocate(exclusiveLBound(3,localDeCount))         ! dimCount=3
  allocate(exclusiveUBound(3,localDeCount))         ! dimCount=3

  call ESMF_ArrayGet(array_wouthalo, exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)

  do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
    Farray3D_wouthalo(i,j,k) = i+j+k  ! init to something i, j, k dependent
  enddo
  enddo
  enddo

  call ESMF_ArrayGet(array_withhalo, exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)
  Farray3D_withhalo = 1     ! All entries are 1 including the halos.
  do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
    Farray3D_withhalo(i,j,k) = i+j+k  ! init to something i, j, k dependent
  enddo
  enddo
  enddo


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Write ESMF_Array with Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_withhalo, file='file3D_withhalo.nc', rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the binary file.
  write(name, *) "Write ESMF_Array with Halo to binary Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_withhalo, file='file3D_withhalo.bin', &
       iofmt=ESMF_IOFMT_BIN, rc=rc)
#if (defined ESMF_PIO && defined ESMF_MPIIO)
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)

#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Write ESMF_Array without Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_wouthalo, file='file3D_wouthalo.nc', rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create Array without Halo for PIO Read"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_wouthalo2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='velocity', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create Array with Halo for PIO Read"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_withhalo2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,3,1/), computationalUWidth=(/1,1,2/), &
          totalLWidth=(/1,7,1/), totalUWidth=(/4,2,3/), &
          indexflag=ESMF_INDEX_GLOBAL, name='temperature', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create Array with Halo for PIO Read"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_withhalo3 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,3,1/), computationalUWidth=(/1,1,2/), &
          totalLWidth=(/1,7,1/), totalUWidth=(/4,2,3/), &
          indexflag=ESMF_INDEX_GLOBAL, name='temperature', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
! !  Get Fortran pointer to Array data
! !  Data is type ESMF_KIND_I4
  do de=0,localDeCount-1
    call ESMF_ArrayGet(array_wouthalo2, localDe=de, farrayPtr=Farray3D_wouthalo2, rc=rc) 
  enddo
  call ESMF_ArrayGet(array_wouthalo2, exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a netCDF file to an ESMF array.
  write(name, *) "Read ESMF_Array without Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_wouthalo2, file='file3D_wouthalo.nc', rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare readin and the existing file
  write(name, *) "Compare readin data to the existing data without halo"
  write(failMsg, *) "Comparison failed"
  Maxvalue(1) = 0.0
  do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   diff = abs( Farray3D_wouthalo2(i,j,k)-Farray3D_wouthalo(i,j,k) )
   if (Maxvalue(1).le.diff) Maxvalue(1)=diff
  enddo
  enddo
  enddo
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error (Without Halo case) = ", Maxvalue(1)
  call ESMF_Test((Maxvalue(1) .lt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not failed as was expected"
  call ESMF_Test((Maxvalue(1) .gt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#endif

!-------------------------------------------------------------------------------
! !  Get Fortran pointer to Array data
! !  Data is type ESMF_KIND_I4
  do de=0,localDeCount-1
    call ESMF_ArrayGet(array_withhalo2, localDe=de, &
         farrayPtr=Farray3D_withhalo2, rc=rc)
    call ESMF_ArrayGet(array_withhalo3, localDe=de, &
         farrayPtr=Farray3D_withhalo3, rc=rc)
  enddo
  Farray3D_withhalo2 = 0    ! Initialize it for some fortran compilers
  Farray3D_withhalo3 = 0    ! Initialize it for some fortran compilers
  call ESMF_ArrayGet(array_withhalo2, exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a netCDF file to an ESMF array.
  write(name, *) "Read ESMF_Array with Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_withhalo2, file='file3D_withhalo.nc', rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a binary file to an ESMF array.
  write(name, *) "Read ESMF_Array with Halo binary Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_withhalo3, file='file3D_withhalo.bin', &
       iofmt=ESMF_IOFMT_BIN, rc=rc)
#if (defined ESMF_PIO && defined ESMF_MPIIO)
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare readin and the existing file
  write(name, *) "Compare readin data to the existing data with halo"
  write(failMsg, *) "Comparison failed"
  Maxvalue(1) = 0.0
  do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   diff = abs( Farray3D_withhalo2(i,j,k)-Farray3D_withhalo(i,j,k) )
   if (Maxvalue(1).le.diff) Maxvalue(1)=diff
  enddo
  enddo
  enddo
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error (With Halo case) = ", Maxvalue(1)
  call ESMF_Test((Maxvalue(1) .lt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not failed as was expected"
  call ESMF_Test((Maxvalue(1) .gt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare readin and the existing file
  write(name, *) "Compare readin data to the existing binary data with halo"
  write(failMsg, *) "Comparison failed"
  Maxvalue(1) = 0.0
  do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   diff = abs( Farray3D_withhalo3(i,j,k)-Farray3D_withhalo(i,j,k) )
   if (Maxvalue(1).le.diff) Maxvalue(1)=diff
  enddo
  enddo
  enddo
#if (defined ESMF_PIO && defined ESMF_MPIIO)
  write(*,*)"Maximum Error (With Halo case) = ", Maxvalue(1)
  call ESMF_Test((Maxvalue(1) .lt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not failed as was expected"
  call ESMF_Test((Maxvalue(1) .gt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#endif

  deallocate (computationalLWidth, computationalUWidth)
  deallocate (totalLWidth, totalUWidth)
  deallocate (exclusiveLBound, exclusiveUBound)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array with Halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_withhalo, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array without Halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_wouthalo, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! Tests: 2D case (real8)
! Write Fortran data (Farray2D_withhalo) from an ESMF_Array with Halos
! Then read data back in to a new array without Halos (Farray2D_wouthalo),
! and compare the read in data and the original data
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), &
              maxIndex=(/5,5/), regDecomp=(/2,3/),  rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Spec Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8,   &
                         rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array with Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(computationalLWidth(2)) ! dimCount=2
  allocate(computationalUWidth(2)) ! dimCount=2
  allocate(totalLWidth(2))         ! dimCount=2
  allocate(totalUWidth(2))         ! dimCount=2
  array_withhalo = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,3/), computationalUWidth=(/1,1/), &
          totalLWidth=(/1,7/), totalUWidth=(/4,2/), &
          indexflag=ESMF_INDEX_GLOBAL, name='u-velocity', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_wouthalo = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,0/), computationalUWidth=(/0,0/), &
          totalLWidth=(/0,0/), totalUWidth=(/0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='u-velocity', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
! !  Get Fortran pointer to Array data
! !  Data is type ESMF_KIND_R8
  localDeCount = 1
  call ESMF_ArrayGet(array_withhalo, localDe=0, farrayPtr=Farray2D_withhalo, rc=rc)
  call ESMF_ArrayGet(array_wouthalo, localDe=0, farrayPtr=Farray2D_wouthalo, rc=rc)

  localDeCount = 1
  allocate(exclusiveLBound(2,localDeCount))         ! dimCount=2
  allocate(exclusiveUBound(2,localDeCount))         ! dimCount=2

  call ESMF_ArrayGet(array_withhalo, exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)
  Farray2D_withhalo = 0.02  ! halo points will have value 0.02
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
    Farray2D_withhalo(i,j) = sin (dble (i)/5.0d0) * tan (dble (j)/5.0d0)
  enddo
  enddo


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Write 2D ESMF_Array with Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_withhalo, file='file2D_withhalo.nc', rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!-------------------------------------------------------------------------------
! !  Get Fortran pointer to Array data
! !  Data is type ESMF_KIND_R8
  call ESMF_ArrayGet(array_wouthalo, localDe=0, &
          farrayPtr=Farray2D_wouthalo, rc=rc)

  call ESMF_ArrayGet(array_wouthalo, exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a netCDF file to an ESMF array.
  write(name, *) "Read 2D ESMF_Array written for Array with halo to ESMF_Array without halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_wouthalo, file="file2D_withhalo.nc", rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare readin and the existing file
  write(name, *) "Compare readin data to the existing data without halo"
  write(failMsg, *) "Comparison failed"
  Maxvalue(1) = 0.0
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   diff = abs( Farray2D_wouthalo(i,j) - Farray2D_withhalo(i,j) )
   if (Maxvalue(1).le.diff) Maxvalue(1)=diff
  enddo
  enddo
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error (With/Without Halo case) = ", Maxvalue(1)
  call ESMF_Test((Maxvalue(1) .lt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not failed as was expected"
  call ESMF_Test((Maxvalue(1) .gt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
!------------------------------------------------------------------------
! Further test that read a file2D_wouthalo.nc file into an Array
! that was created on a different DistGrid.
!
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid_diff = ESMF_DistGridCreate(minIndex=(/1,1/), &
              maxIndex=(/5,5/), regDecomp=(/3,2/),  rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_diff = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid_diff, &
          computationalLWidth=(/0,0/), computationalUWidth=(/0,0/), &
          totalLWidth=(/0,0/), totalUWidth=(/0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='u-velocity', rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a netCDF file to an ESMF array.
  write(name, *) "Read 2D ESMF_Array with different distgrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_diff, file="file2D_withhalo.nc", rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!-------------------------------------------------------------------------------
! !  Compare global Fortran array (Data is type ESMF_KIND_R8)
! !  Both data are read in, if ESMF_PIO is not defined, we should
! !  skip this comparison.
  !NEX_UTest_Multi_Proc_Only
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  Maxvalue(1) = 0.0  ! initialize
  call ESMF_ArrayGather(array_diff, FarrayGr_1, patch=1, rootPet=0, rc=rc)
  call ESMF_ArrayGather(array_wouthalo, FarrayGr_2, patch=1, rootPet=0, rc=rc)
  write(name, *) "Compare readin data from a different distgrid"
  write(failMsg, *) "Comparison failed"
  if (localPet .eq.0) then
   do j=1,5
   do i=1,5
     diff = abs(FarrayGr_1(i,j) - FarrayGr_2(i,j) )
     if (diff .gt. Maxvalue(1)) Maxvalue(1) = diff
   enddo
   enddo
  endif
  call ESMF_VMBroadcast(vm, Maxvalue, count=1, root=0, rc=rc)
  write(*,*)"Maximum Error (different distgrid) = ", Maxvalue(1)
  call ESMF_Test((Maxvalue(1) .lt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#else
   Maxvalue(1) = 1.0 ! initialize to ensure proper logic below
   write(failMsg, *) "Comparison did not failed as was expected"
   call ESMF_Test((Maxvalue(1) .gt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#endif

  deallocate (computationalLWidth, computationalUWidth)
  deallocate (totalLWidth, totalUWidth)
  deallocate (exclusiveLBound, exclusiveUBound)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array with Halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_withhalo, rc=rc)
  call ESMF_ArrayDestroy(array_withhalo2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array without Halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_wouthalo, rc=rc)
  call ESMF_ArrayDestroy(array_wouthalo2, rc=rc)
  call ESMF_ArrayDestroy(array_diff, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_DistGridDestroy(distgrid_diff, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_ArrayIOUTest
