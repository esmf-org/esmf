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
program ESMF_ArrayIOUTest

!------------------------------------------------------------------------------

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
  use ESMF
  
  implicit none

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
 
  ! local variables
  type(ESMF_VM):: vm
  type(ESMF_ArraySpec):: arrayspec, arrayspec_20DE
  integer(ESMF_KIND_I4), pointer, dimension(:,:,:) :: Farray3D_withhalo, &
               Farray3D_wouthalo, Farray3D_withhalo2, &
               Farray3D_wouthalo2, Farray3D_withhalo3
  integer(ESMF_KIND_I4), pointer, dimension(:,:,:) :: Farray3D_DE0, Farray3D_DE1
  integer(ESMF_KIND_I4), pointer, dimension(:,:,:) :: Farray3D_DE0_r, Farray3D_DE1_r
  real(ESMF_KIND_R8),    pointer, dimension(:,:)   ::  Farray2D_withhalo, Farray2D_wouthalo
  real(ESMF_KIND_R8), dimension(5,5) :: FarrayGr_1 , FarrayGr_2
  type(ESMF_DistGrid)                     :: distgrid, distgrid_diff
  type(ESMF_DistGrid)                     :: distgrid_2DE, distgrid_20DE
  type(ESMF_Array)                        :: array_withhalo, array_wouthalo
  type(ESMF_Array)                        :: array_withhalo2, array_wouthalo2
  type(ESMF_Array)                        :: array_withhalo3
  type(ESMF_Array)                        :: array_diff
  type(ESMF_Array)                        :: array_2DE, array_2DE_r
  type(ESMF_Array)                        :: array_20DE, array_20DE_r
  type(ESMF_Array)                        :: array_undist, array_undist_r
  type(ESMF_DistGrid)                     :: distgrid_tmp
  type(ESMF_Array)                        :: array_tmp
  integer                                 :: rank, tileCount, dimCount, jj
  integer, allocatable                    :: arrayToDistGridMap(:), regDecomp(:)
  integer, allocatable                    :: minIndexPTile(:,:), maxIndexPTile(:,:)
  integer, allocatable                    :: minIndexNew(:), maxIndexNew(:)
  integer, allocatable                    :: undistLBound(:), undistUBound(:)
  real(ESMF_KIND_R8),    pointer          :: arrayPtrR8D4(:,:,:,:)
  real(ESMF_KIND_R8),    pointer          :: arrayPtrR8D4_r(:,:,:,:)
  type(ESMF_RouteHandle)                  :: rh
  integer                                 :: rc, de
  integer, allocatable :: totalLWidth(:), totalUWidth(:), &
                       computationalLWidth(:),computationalUWidth(:)
  integer, allocatable :: exclusiveLBound(:,:), exclusiveUBound(:,:)
  integer      :: localDeCount, localPet, petCount
  integer :: i,j,k
  integer :: Maxvalue(1), diff
  real(ESMF_KIND_R8) :: r8Max(1), r8diff
  character(ESMF_MAXSTR) :: string
  integer :: msglen
  logical :: passfail
  logical :: valid_de1

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! Set up
  ! *******
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (petCount /= 6) then
    print *, "This system test needs to run on exactly 6 PETs, petCount = ", &
      petCount
    goto 10
  endif

  call ESMF_LogSet (flush = .true.)

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
  !NEX_UTest_Multi_Proc_Only
! !  Get Fortran pointer to Array data
! !  Data is type ESMF_KIND_I4
  write(name, *) "Accessing Fortran pointers for Arrays Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  localDeCount = 1
  rc = ESMF_SUCCESS
  ESMF_BLOCK(aget_i4)
    call ESMF_ArrayGet(array_withhalo, localDe=0, farrayPtr=Farray3D_withhalo, rc=rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit aget_i4

    call ESMF_ArrayGet(array_wouthalo, localDe=0, farrayPtr=Farray3D_wouthalo, rc=rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit aget_i4

    localDeCount = 1
    allocate(exclusiveLBound(3,localDeCount))         ! dimCount=3
    allocate(exclusiveUBound(3,localDeCount))         ! dimCount=3

    call ESMF_ArrayGet(array_wouthalo, exclusiveLBound=exclusiveLBound, &
                       exclusiveUBound=exclusiveUBound, rc=rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit aget_i4

    Farray3D_wouthalo = 1
    do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
    do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
    do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
      Farray3D_wouthalo(i,j,k) = i+j+k  ! init to something i, j, k dependent
    enddo
    enddo
    enddo

    call ESMF_ArrayGet(array_withhalo, exclusiveLBound=exclusiveLBound, &
                       exclusiveUBound=exclusiveUBound, rc=rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit aget_i4

    Farray3D_withhalo = 1     ! All entries are 1 including the halos.
    do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
    do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
    do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
      Farray3D_withhalo(i,j,k) = i+j+k  ! init to something i, j, k dependent
    enddo
    enddo
    enddo
  ESMF_ENDBLOCK(aget_i4)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Write ESMF_Array with Halo to NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_withhalo, fileName='file3D_withhalo.nc',    &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, attempt to overwrite an existing file (should fail)
  write(name, *) "Write ESMF_Array with Halo Test overwrite test"
  write(failMsg, *) "Did not return ESMF_RC_FILE_WRITE"
  call ESMF_ArrayWrite(array_withhalo, fileName='file3D_withhalo.nc',    &
      status=ESMF_FILESTATUS_NEW, rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_RC_FILE_WRITE), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the binary file.
  write(name, *) "Write ESMF_Array with Halo to binary Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_withhalo, fileName='file3D_withhalo.bin', &
       status=ESMF_FILESTATUS_REPLACE, iofmt=ESMF_IOFMT_BIN, rc=rc)
#if (defined ESMF_PIO && defined ESMF_MPIIO)
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)

#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Write ESMF_Array without Halo to NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_wouthalo, fileName='file3D_wouthalo.nc',         &
      status=ESMF_FILESTATUS_REPLACE, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
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
  Farray3D_wouthalo2 = 0    ! Initialize it for some fortran compilers
  call ESMF_ArrayGet(array_wouthalo2, exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a netCDF file to an ESMF array.
  write(name, *) "Read ESMF_Array without Halo from NetCDF file Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_wouthalo2, fileName='file3D_wouthalo.nc',  &
      iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare read in and the existing file
  write(name, *) "Compare read in data to the existing data - 3D without halo"
  write(failMsg, *) "Comparison failed"
  Maxvalue(1) = 0
  do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   diff = abs( Farray3D_wouthalo2(i,j,k)-Farray3D_wouthalo(i,j,k) )
   Maxvalue(1) = max (Maxvalue(1), diff)
  enddo
  enddo
  enddo
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(failMsg, *) "Comparison failed, max error =", Maxvalue(1)
  write(*,*)"Maximum Error (3D without Halo case) = ", Maxvalue(1)
  call ESMF_Test((Maxvalue(1) == 0), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as was expected"
  call ESMF_Test((Maxvalue(1) > 0), name, failMsg, result,ESMF_SRCLINE)
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
  write(name, *) "Read ESMF_Array with Halo from NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_withhalo2, fileName='file3D_withhalo.nc', rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a binary file to an ESMF array.
  write(name, *) "Read ESMF_Array with Halo from binary Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_withhalo3, fileName='file3D_withhalo.bin', &
       iofmt=ESMF_IOFMT_BIN, rc=rc)
#if (defined ESMF_PIO && defined ESMF_MPIIO)
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare read in and the existing file
  write(name, *) "Compare read in data to the existing NetCDF data - 3D with halo"
  Maxvalue(1) = 0
  do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   diff = abs( Farray3D_withhalo2(i,j,k)-Farray3D_withhalo(i,j,k) )
   Maxvalue(1) = max (Maxvalue(1), diff)
  enddo
  enddo
  enddo
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(failMsg, *) "Comparison failed, max error =", Maxvalue(1)
  write(*,*)"Maximum Error (3D with Halo NetCDF case) = ", Maxvalue(1)
  call ESMF_Test((Maxvalue(1) == 0), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as was expected"
  call ESMF_Test((Maxvalue(1) > 0), name, failMsg, result,ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare read in and the existing file
  write(name, *) "Compare read in data to the existing binary data - 3D with halo"
  Maxvalue(1) = 0
  do k=exclusiveLBound(3,1),exclusiveUBound(3,1)
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   diff = abs( Farray3D_withhalo3(i,j,k)-Farray3D_withhalo(i,j,k) )
   Maxvalue(1) = max (Maxvalue(1), diff)
  enddo
  enddo
  enddo
#if (defined ESMF_PIO && defined ESMF_MPIIO)
  write(failMsg, *) "Comparison failed.  Max error =", Maxvalue(1)
  write(*,*)"Maximum Error (3D with Halo binary data case) = ", Maxvalue(1)
  call ESMF_Test((Maxvalue(1) == 0), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as was expected"
  call ESMF_Test((Maxvalue(1) > 0), name, failMsg, result,ESMF_SRCLINE)
#endif

  deallocate (computationalLWidth, computationalUWidth)
  deallocate (totalLWidth, totalUWidth)
  deallocate (exclusiveLBound, exclusiveUBound)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a binary file to an ESMF array specifying a variable.
  write(name, *) "Read ESMF_Array variable with Halo from binary Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_withhalo3, fileName='file3D_withhalo.bin', &
       iofmt=ESMF_IOFMT_BIN, variableName='dummyname', rc=rc)
! ! Should fail since varname not supported in binary mode
  call ESMF_Test(rc /= ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

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
  write(name, *) "Write 2D ESMF_Array with Halo to NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_withhalo, fileName='file2D_withhalo.nc',        &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!-------------------------------------------------------------------------------
! !  Get Fortran pointer to Array data
! !  Data is type ESMF_KIND_R8
  call ESMF_ArrayGet(array_wouthalo, localDe=0, farrayPtr=Farray2D_wouthalo, rc=rc)
  Farray2D_wouthalo = 0.0

  call ESMF_ArrayGet(array_wouthalo, exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Read in a netCDF file to an ESMF array.
  write(name, *) "Read 2D ESMF_Array written for Array with halo to ESMF_Array without halo Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_wouthalo, fileName="file2D_withhalo.nc", rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare read in and the existing file
  write(name, *) "Compare read in data to the existing data - 2D with/without halo"
  write(failMsg, *) "Comparison failed"
  r8Max(1) = 0.0
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   r8diff = abs( Farray2D_wouthalo(i,j) - Farray2D_withhalo(i,j) )
   r8Max(1) = max (r8Max(1), r8diff)
  enddo
  enddo
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error (With/Without Halo case) = ", r8Max(1)
  call ESMF_Test((r8Max(1) .lt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as was expected, max error =", r8Max(1)
  call ESMF_Test((r8Max(1) .gt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
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
  write(name, *) "Read 2D ESMF_Array with different distgrid from NetCDF test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array_diff, fileName="file2D_withhalo.nc", rc=rc)
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
  r8Max(1) = 0.0  ! initialize
  call ESMF_ArrayGather(array_diff, FarrayGr_1, tile=1, rootPet=0, rc=rc)
  call ESMF_ArrayGather(array_wouthalo, FarrayGr_2, tile=1, rootPet=0, rc=rc)
  write(name, *) "Compare read in data from a different distgrid"
  write(failMsg, *) "Comparison failed"
  if (localPet .eq.0) then
   do j=1,5
   do i=1,5
     r8diff = abs(FarrayGr_1(i,j) - FarrayGr_2(i,j) )
     r8Max(1) = max (r8Max(1), r8diff)
   enddo
   enddo
  endif
  call ESMF_VMBroadcast(vm, r8Max, count=1, rootPet=0, rc=rc)
  write(*,*)"Maximum Error (different distgrid) = ", r8Max(1)
  call ESMF_Test((r8Max(1) .lt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#else
   r8Max(1) = 1.0 ! initialize to ensure proper logic below
   write(failMsg, *) "Comparison did not fail as was expected"
   call ESMF_Test((r8Max(1) .gt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
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

!------------------------------------------------------------------------
! Multiple DEs per PET tests
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create 2 DE/Pet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid_2DE = ESMF_DistGridCreate(minIndex=(/1,1,1/), &
              maxIndex=(/10,5,5/), regDecomp=(/4,3,1/),  rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Spec Set 2 DE/Pet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4,   &
                         rank=3, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DE Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_2DE = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid_2DE, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='velocity', rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DE Array access and fill DE0 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE, localDe=0, farrayPtr=Farray3D_DE0, rc=rc)
  Farray3D_DE0 = localPet*100
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DE Array access and fill DE1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE, localDe=1, farrayPtr=Farray3D_DE1, rc=rc)
  Farray3D_DE1 = localPet*100 + 1
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Write 2DE ESMF_Array to NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_2DE, fileName='Array_2DE.nc',         &
      status=ESMF_FILESTATUS_REPLACE, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DE read Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_2DE_r = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid_2DE, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='velocity', rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DE read Array access and zero fill DE0 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_r, localDe=0, farrayPtr=Farray3D_DE0_r, rc=rc)
  Farray3D_DE0 = 0
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DE read Array access and zero fill DE1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_r, localDe=1, farrayPtr=Farray3D_DE1_r, rc=rc)
  Farray3D_DE1 = 0
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, read the netCDF file.
  write(name, *) "Read 2DE ESMF_Array from NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead (array_2DE_r, fileName='Array_2DE.nc',         &
      iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DE read Array - DE 0 comparison Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray3D_DE0_r == localPet*100))
#else
  rc = ESMF_SUCCESS
#endif
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2 DE read Array - DE 1 comparison Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray3D_DE1_r == localPet*100 + 1))
#else
  rc = ESMF_SUCCESS
#endif
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy 2 DE Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_2DE, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy 2 DE read Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_2DE_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy 2 DE DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid_2DE, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Multiple/zero DEs per PET tests
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create 2/0 DE/Pet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid_20DE = ESMF_DistGridCreate(minIndex=(/1,1,1/), &
              maxIndex=(/10,5,5/), regDecomp=(/4,2,1/),  rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Spec Set 2/0 DE/Pet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec_20DE, typekind=ESMF_TYPEKIND_I4,   &
                         rank=3, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2/0 DE Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_20DE = ESMF_ArrayCreate(arrayspec=arrayspec_20DE, distgrid=distgrid_20DE, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='velocity', rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2/0 DE Array access and fill DE0 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farray3D_DE0 => null ()
  call ESMF_ArrayGet(array_20DE, localDe=0, farrayPtr=Farray3D_DE0, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (size (Farray3D_DE0) > 0) then
    Farray3D_DE0 = localPet*100
  end if

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2/0 DE Array access and fill DE1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farray3D_DE1 => null ()
  call ESMF_ArrayGet(array_20DE, localDe=1, farrayPtr=Farray3D_DE1, rc=rc)
  ! Pets 0 and 1 should succeed.  Pets 2-5 should fail.
  if (localPet == 0 .or. localPet == 1) then
    passfail = rc == ESMF_SUCCESS .and. associated (Farray3D_DE1)
  else
    passfail = rc /= ESMF_SUCCESS .and. .not. associated (Farray3D_DE1)
  end if
  call ESMF_Test(passfail, name, failMsg, result, ESMF_SRCLINE)  
  if (associated (Farray3D_DE1)) then
    Farray3D_DE1 = localPet*100 + 1
  end if

! call ESMF_ArrayPrint (array_2DE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Write 2/0DE ESMF_Array to NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_20DE, fileName='Array_20DE.nc',         &
      status=ESMF_FILESTATUS_REPLACE, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2/0 DE create read Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_20DE_r = ESMF_ArrayCreate(arrayspec=arrayspec_20DE, distgrid=distgrid_20DE, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='velocity', rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2/0 DE read Array access and zero fill DE0 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farray3D_DE0_r => null ()
  call ESMF_ArrayGet(array_20DE_r, localDe=0, farrayPtr=Farray3D_DE0_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  
  Farray3D_DE0_r = 0

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2/0 DE read Array access and zero fill DE1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farray3D_DE0_r => null ()
  call ESMF_ArrayGet(array_20DE_r, localDe=1, farrayPtr=Farray3D_DE1_r, rc=rc)
  ! Pets 0 and 1 should succeed.  Pets 2-5 should fail.
  if (localPet == 0 .or. localPet == 1) then
    passfail = rc == ESMF_SUCCESS
  else
    passfail = rc /= ESMF_SUCCESS
  end if
  call ESMF_Test(passfail, name, failMsg, result, ESMF_SRCLINE)  
  if (localPet == 0 .or. localPet == 1) then
    Farray3D_DE1_r = 0
  end if

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, read the netCDF file.
  write(name, *) "Read 2/0DE ESMF_Array from NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead (array_20DE_r, fileName='Array_20DE.nc',         &
      iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! TODO: This ArrayGet should not be needed!
  write(name, *) "2/0 DE 0 post read Array access Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farray3D_DE0_r => null ()
  call ESMF_ArrayGet(array_20DE_r, localDe=0, farrayPtr=Farray3D_DE0_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2/0 DE read Array - DE 0 comparison Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray3D_DE0_r == localPet*100))
#else
  rc = ESMF_SUCCESS
#endif
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! TODO: This ArrayGet should not be needed!
  write(name, *) "2/0 DE 1 post read Array access Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farray3D_DE1_r => null ()
  call ESMF_ArrayGet(array_20DE_r, localDe=1, farrayPtr=Farray3D_DE1_r, rc=rc)
  ! Pets 0 and 1 should succeed.  Pets 2-5 should fail.
  if (localPet == 0 .or. localPet == 1) then
    passfail = rc == ESMF_SUCCESS
  else
    passfail = rc /= ESMF_SUCCESS
  end if
  call ESMF_Test(passfail, name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2/0 DE read Array - DE 1 comparison Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  if (associated (Farray3D_DE1_r)) then
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray3D_DE1_r == localPet*100 + 1))
  else
    rc = ESMF_SUCCESS
  end if
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  rc = ESMF_SUCCESS
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy 2/0 DE Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_20DE, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy 2/0 DE read Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_20DE_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy 2/0 DE DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid_20DE, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Array with undistributed dimension(s)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create 1 DE/Pet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), &
              maxIndex=(/5,10/), regDecomp=(/petCount,1/),  rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array with undistributed dimensions Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_undist = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
          indexflag=ESMF_INDEX_GLOBAL, distgridToArrayMap=(/1,3/), &
          undistLBound=(/2,3/), undistUBound=(/8,5/), &
          name="myData", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array with undistributed dimensions get and fill Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"  
  ! access the pointer to the allocated memory on each DE
  call ESMF_ArrayGet(array_undist, farrayPtr=arrayPtrR8D4, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! initialize data
  arrayPtrR8D4 = 12345._ESMF_KIND_R8*localPet

#define TEST_WORKAROUND
#if defined(TEST_WORKAROUND)
!!!! Calling ArrayWrite on array directly does not work because it has 
!!!! undistributed dimensions!!  

!!!! The way to solve this is to create a new Array on a new DistGrid, that
!!!! has all distributed dims, and still results in exactly the same memory
!!!! allocation. This new Array can then use the original arrayPtr to access
!!!! the original memory.

  ! get some basic information out of the Array
  call ESMF_ArrayGet(array_undist, rank=rank, tileCount=tileCount, dimCount=dimCount, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (tileCount /= 1) then
    ! code below, and I/O only supports single tile case for now.
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  
  ! get more info out of the Array
  allocate(minIndexPTile(dimCount,1), maxIndexPTile(dimCount,1))
  allocate(arrayToDistGridMap(rank))
  allocate(undistLBound(rank), undistUBound(rank))
  call ESMF_ArrayGet(array_undist, arrayToDistGridMap=arrayToDistGridMap, &
    undistLBound=undistLBound, undistUBound=undistUBound, &
    minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! construct the new minIndex and maxIndex
  allocate(minIndexNew(rank), maxIndexNew(rank))
  jj=0  ! reset
  do i=1, rank
    j = arrayToDistGridMap(i)
    if (j>0) then
      ! valid DistGrid dimension
      minIndexNew(i) = minIndexPTile(j,1)
      maxIndexNew(i) = maxIndexPTile(j,1)
    else
      ! undistributed dimension
      jj=jj+1
      minIndexNew(i) = undistLBound(jj)
      maxIndexNew(i) = undistUBound(jj)
    endif
  enddo
  
  ! general dimensionality of regDecomp
  allocate(regDecomp(rank))
  regDecomp = 1 ! default all dims to 1
  regDecomp(1) = petCount ! first element petCount for default distribution
  
  ! now create the fixed up DistGrid
  distgrid_tmp = ESMF_DistGridCreate(minIndex=minIndexNew, &
    maxIndex=maxIndexNew, regDecomp=regDecomp, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! finally create the fixed up Array, passing in same memory allocation ptr
  array_tmp = ESMF_ArrayCreate(distgrid=distgrid_tmp, &
    farrayPtr=arrayPtrR8D4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!!!! Now array_tmp is an Array that references the exact same data allocation 
!!!! as the oritional array object did, however, array_tmp only has distributed
!!!! dimensions, and therefore will work in the ArrayWrite() call below...

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Write ESMF_Array with undistributed dimensions (prototype) to NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_tmp, fileName="Array_myData.nc",         &
      status=ESMF_FILESTATUS_REPLACE, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, write the netCDF file.
  write(name, *) "Array with undistributed dimensions write to NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array_undist, fileName="Array_undist.nc",         &
      status=ESMF_FILESTATUS_REPLACE, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array with undistributed dimensions Create for read Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_undist_r = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
          indexflag=ESMF_INDEX_GLOBAL, distgridToArrayMap=(/1,3/), &
          undistLBound=(/2,3/), undistUBound=(/8,5/), &
          name="myData", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Given an ESMF array, read the netCDF file.
  write(name, *) "Array with undistributed dimensions read from NetCDF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead (array_undist_r, fileName='Array_undist.nc',         &
      iofmt=ESMF_IOFMT_NETCDF, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Access read-in Array
  write(name, *) "Array with undistributed dimensions access read-in data Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet (array_undist, farrayPtr=arrayPtrR8D4_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare read-in Array to expected
  write(name, *) "Array with undistributed dimensions comparison Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  passfail = all (arrayPtrR8D4_r == 12345.0_ESMF_KIND_R8*localPet)
  call ESMF_Test(passfail, name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array_undist, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_ArrayIOUTest
