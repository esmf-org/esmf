! $Id: ESMF_ArrayIOUTest.F90,v 1.4 2010/06/25 15:21:17 samsoncheung Exp $
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
  integer(ESMF_KIND_I4), pointer, dimension(:,:,:) ::  Farray3D_halo, Farray3D_nohalo, Farray3D_nohalo2
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray2D_halo, Farray2D_nohalo
  type(ESMF_DistGrid)                     :: distgrid
  type(ESMF_Array)                        :: array_halo, array_nohalo
  type(ESMF_Array)                        :: array_nohalo2
  integer                                 :: rc, de
  integer, allocatable :: totalLWidth(:), totalUWidth(:), &
                       computationalLWidth(:),computationalUWidth(:)
  integer, allocatable :: totalLBound(:,:), totalUBound(:,:)
  integer      :: localDeCount, localPet, petCount
  integer :: i,j,k
  real :: Maxvalue, diff
  logical :: PIONotPresent

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
  ! assume PIO library present until proven otherwise
  PIONotPresent = .false.

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "dstDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), &
              maxIndex=(/5,5,5/), regDecomp=(/2,3,1/),  rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Array Spec Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4,   &
                         rank=3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Array with Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(computationalLWidth(3)) ! dimCount=3
  allocate(computationalUWidth(3)) ! dimCount=3
  allocate(totalLWidth(3))         ! dimCount=3
  allocate(totalUWidth(3))         ! dimCount=3
  array_halo = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,3,1/), computationalUWidth=(/1,1,2/), &
          totalLWidth=(/1,7,1/), totalUWidth=(/4,2,3/), &
          indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_nohalo = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!-------------------------------------------------------------------------------
! !  Assign value to an existing, allocated F90 pointer.
! !  Data is type ESMF_KIND_I4
  localDeCount = 1
  do de=0,localDeCount-1
    call ESMF_ArrayGet(array_halo, localDe=de, farrayPtr=Farray3D_halo, rc=rc)
    call ESMF_ArrayGet(array_nohalo, localDe=de, farrayPtr=Farray3D_nohalo, rc=rc)
    Farray3D_halo   = 12  ! e.g. fill the entire local DE allocation
    Farray3D_nohalo = 12  ! e.g. fill the entire local DE allocation
  enddo

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
! ! Given an ESMF array, print the netCDF file.
  write(name, *) "Write ESMF_Array with Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_halo, Farray3D_halo, 3, (/5,5,5/), fname='file3D_halo.nc', rc=rc)
  if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        PIONotPresent = .true.
  endif
  call ESMF_Test((rc==ESMF_SUCCESS .or. PIONotPresent), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
! ! Given an ESMF array, print the netCDF file.
  write(name, *) "Write ESMF_Array without Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_nohalo, Farray3D_nohalo, 3, (/5,5,5/), rc=rc)
  call ESMF_Test((rc==ESMF_SUCCESS .or. PIONotPresent), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_nohalo2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
! !  Assign value to an existing, allocated F90 pointer.
! !  Data is type ESMF_KIND_I4
  localDeCount = 1
  allocate(totalLBound(3,localDeCount))         ! dimCount=3
  allocate(totalUBound(3,localDeCount))         ! dimCount=3
  do de=0,localDeCount-1
    call ESMF_ArrayGet(array_nohalo2, localDe=de, farrayPtr=Farray3D_nohalo2, rc=rc) 
  enddo
  call ESMF_ArrayGet(array_nohalo2, totalLBound=totalLBound, &
                     totalUBound=totalUBound, rc=rc)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
! ! Read in a netCDF file to an ESMF array.
  write(name, *) "Read ESMF_Array without Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_nohalo2, Farray3D_nohalo2,3, (/5,5,5/), &
                      fname='ESMF_Array_int.nc', rc=rc)
  if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        PIONotPresent = .true.
  endif
  call ESMF_Test((rc==ESMF_SUCCESS .or. PIONotPresent), name, &
                 failMsg, result,ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
! ! Compare readin and the existing file
  write(name, *) "Compare readin data to the existing data without halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  Maxvalue = 0.0
  do k=totalLBound(3,1),totalUBound(3,1)
  do j=totalLBound(2,1),totalUBound(2,1)
  do i=totalLBound(1,1),totalUBound(1,1)
   diff = abs( Farray3D_nohalo2(i,j,k)-Farray3D_nohalo(i,j,k) )
   if (Maxvalue.le.diff) Maxvalue=diff
  enddo
  enddo
  enddo
  rc = 1
  if (Maxvalue .lt. 1.e-6) rc=0
  write(*,*)"Maximum Error (Without Halo case) = ", Maxvalue
  call ESMF_Test((rc==ESMF_SUCCESS .or. PIONotPresent), name, &
                 failMsg, result,ESMF_SRCLINE)

  deallocate (computationalLWidth, computationalUWidth)
  deallocate (totalLWidth, totalUWidth)
  deallocate (totalLBound, totalUBound)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array with Halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_halo, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array without Halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_nohalo, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Destroy DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "dstDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), &
              maxIndex=(/5,5/), regDecomp=(/2,3/),  rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Array Spec Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8,   &
                         rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Array with Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(computationalLWidth(2)) ! dimCount=2
  allocate(computationalUWidth(2)) ! dimCount=2
  allocate(totalLWidth(2))         ! dimCount=2
  allocate(totalUWidth(2))         ! dimCount=2
  array_halo = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,3/), computationalUWidth=(/1,1/), &
          totalLWidth=(/1,7/), totalUWidth=(/4,2/), &
          indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_nohalo = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
          computationalLWidth=(/0,0/), computationalUWidth=(/0,0/), &
          totalLWidth=(/0,0/), totalUWidth=(/0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
! !  Assign value to an existing, allocated F90 pointer.
! !  Data is type ESMF_KIND_R8
  localDeCount = 1
  do de=0,localDeCount-1
    call ESMF_ArrayGet(array_halo, localDe=de, farrayPtr=Farray2D_halo, rc=rc)
    call ESMF_ArrayGet(array_nohalo, localDe=de, farrayPtr=Farray2D_nohalo, rc=rc)
    Farray2D_halo   = 0.12  ! e.g. fill the entire local DE allocation
    Farray2D_nohalo = 0.12  ! e.g. fill the entire local DE allocation
  enddo

!------------------------------------------------------------------------
! ! Given an ESMF array, print the netCDF file.
  write(name, *) "Write 2D ESMF_Array with Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_halo, Farray2D_halo, 2, (/5,5/),  &
                       fname='file2D_halo.nc', rc=rc)
  call ESMF_Test((rc==ESMF_SUCCESS .or. PIONotPresent), name, &
                 failMsg, result,ESMF_SRCLINE)

!------------------------------------------------------------------------
! ! Given an ESMF array, print the netCDF file.
  write(name, *) "Write 2D ESMF_Array without Halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_nohalo, Farray2D_nohalo, 2, (/5,5/),  &
                       fname="file2D_nohalo.nc", rc=rc)
  call ESMF_Test((rc==ESMF_SUCCESS .or. PIONotPresent), name, &
                 failMsg, result,ESMF_SRCLINE)

  deallocate (computationalLWidth, computationalUWidth)
  deallocate (totalLWidth, totalUWidth)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array with Halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_halo, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array without Halo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_nohalo, rc=rc)
  call ESMF_ArrayDestroy(array_nohalo2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------



  !NEX_disable_UTest_Multi_Proc_Only
  write(name, *) "Destroy DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_ArrayIOUTest
