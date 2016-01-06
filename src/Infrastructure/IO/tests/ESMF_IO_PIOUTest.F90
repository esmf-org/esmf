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
program ESMF_IO_PIOUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_PIOUTest -  Tests some basic ESMF PIO configuration and usage
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
#if defined (ESMF_PIO)
  use esmfpio
  use esmfpiolib_mod
  use esmfpio_types
!  use esmfpio_types, only : io_desc_t, var_desc_t, file_desc_t, iosystem_desc_t
#endif

  implicit none

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
 
  ! local variables
  type(ESMF_VM):: vm
  integer :: localPet, petCount
  integer :: mpi_comm
  integer :: rc

#if defined (ESMF_PIO)
  type(iosystem_desc_t) :: iosysdesc(2)
  type(file_desc_t) :: filedesc(2)
  type(io_desc_t) :: iodesc(2)
  type(var_desc_t) :: vardesc(2)

  type(iosystem_desc_t) :: pio_ios
  type(file_desc_t)     :: pio_file1
  type(io_desc_t)       :: pio_iodesc1
  type(var_desc_t)      :: pio_vardesc1
#endif
  integer :: iotype
  integer :: pioerr

  character(32) :: fname
  integer :: pio_dims(1)
  integer, parameter :: dim_x=10
  integer :: dimid_x
  integer :: addr_diff, desc_len
  real(ESMF_KIND_R8), dimension(dim_x) :: test_data, read_data

#if defined (ESMF_PIO)
  integer (kind=pio_offset) :: compdof(dim_x) ! global degrees of freedom for computational decomposition
#endif

  integer :: i

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0
  integer :: countfail = 0

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! Set up
  ! *******
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,  &
      mpiCommunicator=mpi_comm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!------------------------------------------------------------------------
! These test INTERNAL procedures and parameters.  These are NOT part of
! the ESMF API.
!------------------------------------------------------------------------

  addr_diff = -1
  desc_len  = -1
!------------------------------------------------------------------------
  !NEX_UTest
  ! Ensure at least enough C++ space to hold Fortran derived type

#if defined (ESMF_PIO)
  call ESMCI_pio_size_check ('iosystem_desc_t', iosysdesc(1), iosysdesc(2),  &
      addr_diff, desc_len,  &
      rc)
  if (rc /= ESMF_SUCCESS)  &
      print *, 'iosystem_desc_t:  Fortran addr_diff =', addr_diff, ', C++ desc_len =', desc_len
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO_SIZE_IOSYSTEM_DESC size =", desc_len, ", reserved space =", addr_diff
  write(name, *) "Checking sizeof(iosystem_desc_t) vs PIO_SIZE_IOSYSTEM_DESC"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Ensure at least enough C++ space to hold Fortran derived type

#if defined (ESMF_PIO)
  call ESMCI_pio_size_check ('file_desc_t', filedesc(1), filedesc(2),  &
      addr_diff, desc_len,  &
      rc)
  if (rc /= ESMF_SUCCESS)  &
      print *, 'file_desc_t:  Fortran addr_diff =', addr_diff, ', C++ desc_len =', desc_len
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO_SIZE_FILE_DESC size =", desc_len, ", reserved space =", addr_diff
  write(name, *) "Checking sizeof(file_desc_t) vs PIO_SIZE_FILE_DESC"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Ensure at least enough C++ space to hold Fortran derived type

#if defined (ESMF_PIO)
  call ESMCI_pio_size_check ('io_desc_t', iodesc(1), iodesc(2),  &
      addr_diff, desc_len,  &
      rc)
  if (rc /= ESMF_SUCCESS)  &
      print *, 'io_desc_t:  Fortran addr_diff =', addr_diff, ', C++ desc_len =', desc_len
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO_SIZE_IO_DESC size =", desc_len, ", reserved space =", addr_diff
  write(name, *) "Checking sizeof(io_desc_t) vs PIO_SIZE_IO_DESC"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Ensure at least enough C++ space to hold Fortran derived type

#if defined (ESMF_PIO)
  call ESMCI_pio_size_check ('var_desc_t', vardesc(1), vardesc(2),  &
      addr_diff, desc_len,  &
      rc)
  if (rc /= ESMF_SUCCESS)  &
      print *, 'var_desc_t:  Fortran addr_diff =', addr_diff, ', C++ desc_len =', desc_len
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO_SIZE_VAR_DESC size =", desc_len, ", reserved space =", addr_diff
  write(name, *) "Checking sizeof(var_desc_t) vs PIO_SIZE_VAR_DESC"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------


!------------------------------------------------------------------------
  !NEX_UTest
  ! Open a PIO iosystem instance

#if defined (ESMF_PIO)
  call pio_init (  &
      comp_rank = localPet,  &
      comp_comm     = mpi_comm,  &
      num_iotasks   = petCount,  &
      num_aggregator= 1,  &
      stride        = 1,  &
      rearr         = 1,  &
      iosystem      = pio_ios)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pio_ios%num_tasks /= 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO initialization failure"
  write(name, *) "Initialize a PIO instance"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
! Test NETCDF
!------------------------------------------------------------------------
#if defined (ESMF_PIO)
  iotype = PIO_iotype_netcdf
#endif
  fname = 'pio_file1f_netcdf.dat'

!------------------------------------------------------------------------
  !NEX_UTest
  ! Create a PIO file for I/O

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = pio_createfile (  &
      iosystem = pio_ios,  &
      file     = pio_file1,  &
      iotype   = iotype,  &
      fname    = fname)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO file create failure"
  write(name, *) "Create PIO NETCDF file: ", fname//'_netcdf'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Create a PIO mapping

  pio_dims = (/ dim_x * petCount /)
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  compdof = (/ (i, i=(localPet*dim_x)+1, (localPet*dim_x)+dim_x) /)
  call pio_initdecomp (  &
      iosystem    = pio_ios,  &
      basepiotype = PIO_double,  &
      dims        = pio_dims,  &
      compdof     = compdof,  &
      iodesc      = pio_iodesc1)
  rc = ESMF_SUCCESS
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO mapping failure"
  write(name, *) "Create a PIO NETCDF mapping"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Start defining the dimensions of the variable to be written

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = pio_def_dim (file=pio_file1,  &
      name='x',  &
      len=size (test_data),  &
      dimid=dimid_x)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO define dimesnion failure"
  write(name, *) "PIO NETCDF define dimension"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Start defining the variable to be written

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = pio_def_var (file=pio_file1,  &
      name='arraydata',  &
      type=PIO_double,  &
      dimids=(/dimid_x/),  &
      vardesc=pio_vardesc1)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO define variable failure"
  write(name, *) "PIO NETCDF define variable"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! End defining variables

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = pio_enddef (file=pio_file1)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO failure ending variable definition mode"     
  write(name, *) "PIO NETCDF ending define mode"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Set variable descriptor frame number

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  call pio_setframe (vardesc=pio_vardesc1, frame=1_PIO_offset)
  rc = ESMF_SUCCESS
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO setframe failure"
  write(name, *) "PIO NETCDF setframe test"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------
 
!------------------------------------------------------------------------
  !NEX_UTest
  ! Write some data

  do, i=1, size (test_data)
    test_data(i) = i + localPet*100
  end do

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  call pio_write_darray (  &
      file    = pio_file1,  &
      vardesc = pio_vardesc1,  &
      iodesc  = pio_iodesc1,  &
      array   = test_data,  &
      iostat  = pioerr)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO write failure"
  write(name, *) "Write data to PIO NETCDF file"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Close file after write

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  call pio_closefile (file=pio_file1)
  rc = ESMF_SUCCESS
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO closefile failure"
  write(name, *) "PIO NETCDF closefile after write test"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Open a PIO file for read

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  pioerr = pio_openfile (  &
      iosystem = pio_ios,  &
      file     = pio_file1,  &
      iotype   = iotype,  &
      fname    = fname,  &
      mode     = 0)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO file open failure"
  write(name, *) "Open PIO NETCDF file for read: ", fname//'_netcdf'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Set variable descriptor frame number

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  call pio_setframe (vardesc=pio_vardesc1, frame=1_PIO_offset)
  rc = ESMF_SUCCESS
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO setframe failure"
  write(name, *) "PIO NETCDF setframe for read test"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------
 
!------------------------------------------------------------------------
  !NEX_UTest
  ! Read back data

  read_data = -42.42
#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  call pio_read_darray (  &
      file    = pio_file1,  &
      vardesc = pio_vardesc1,  &
      iodesc  = pio_iodesc1,  &
      array   = read_data,  &
      iostat  = pioerr)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO read failure"
  write(name, *) "Read data from PIO NETCDF file"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Compare results

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  do, i=1, size (test_data)
    if (test_data(i) /= read_data(i)) exit
  end do

  if (i > size (test_data)) then
    rc = ESMF_SUCCESS
  else
    rc = ESMF_FAILURE
    print *, 'Comparison failed at element', i, ', ', test_data(i), ' /= ', read_data(i)
  end if
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO comparison failure"
  write(name, *) "Compare written data with data read back"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Free the decomposition

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  call pio_freedecomp (  &
      file=pio_file1,  &
      iodesc = pio_iodesc1)
  pioerr = 0
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO file decomposition failure"
  write(name, *) "Free PIO NETCDF decomposition"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Close the PIO file

#if defined (ESMF_PIO) && defined (ESMF_NETCDF)
  call pio_closefile (  &
      file     = pio_file1)
  pioerr = 0
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO file close failure"
  write(name, *) "Close PIO NETCDF file after read"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
! Test PNETCDF
!------------------------------------------------------------------------

#if defined (ESMF_PIO)
  iotype = PIO_iotype_pnetcdf
#endif
  fname = 'pio_file1f_pnetcdf.dat'

!------------------------------------------------------------------------
  !NEX_UTest
  ! Create a PIO file for I/O

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = pio_createfile (  &
      iosystem = pio_ios,  &
      file     = pio_file1,  &
      iotype   = iotype,  &
      fname    = fname)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO file create failure"
  write(name, *) "Create PNETCDF PIO file: ", fname//'_pnetcdf'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Create a PIO mapping

  pio_dims = (/ dim_x * petCount /)
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  compdof = (/ (i, i=(localPet*dim_x)+1, (localPet*dim_x)+dim_x) /)
  call pio_initdecomp (  &
      iosystem    = pio_ios,  &
      basepiotype = PIO_double,  &
      dims        = pio_dims,  &
      compdof     = compdof,  &
      iodesc      = pio_iodesc1)
  rc = ESMF_SUCCESS
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO mapping failure"
  write(name, *) "Create a PIO PNETCDF mapping"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Start defining the dimensions of the variable to be written

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = pio_def_dim (file=pio_file1,  &
      name='x',  &
      len=size (test_data),  &
      dimid=dimid_x)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO define dimesnion failure"
  write(name, *) "PIO PNETCDF define dimension"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Start defining the variable to be written

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = pio_def_var (file=pio_file1,  &
      name='arraydata',  &
      type=PIO_double,  &
      dimids=(/dimid_x/),  &
      vardesc=pio_vardesc1)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO define variable failure"
  write(name, *) "PIO PNETCDF define variable"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! End defining variables

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = pio_enddef (file=pio_file1)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO failure ending variable definition mode"     
  write(name, *) "PIO PNETCDF ending define mode"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Set variable descriptor frame number

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  call pio_setframe (vardesc=pio_vardesc1, frame=1_PIO_offset)
  rc = ESMF_SUCCESS
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO setframe failure"
  write(name, *) "PIO PNETCDF setframe test"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------
 
!------------------------------------------------------------------------
  !NEX_UTest
  ! Write some data

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  do, i=1, size (test_data)
    test_data(i) = i + localPet*100
  end do

  call pio_write_darray (  &
      file    = pio_file1,  &
      vardesc = pio_vardesc1,  &
      iodesc  = pio_iodesc1,  &
      array   = test_data,  &
      iostat  = pioerr)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO write failure"
  write(name, *) "Write data to PIO file"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Close file after write

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  call pio_closefile (file=pio_file1)
  rc = ESMF_SUCCESS
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO closefile failure"
  write(name, *) "PIO PNETCDF closefile after write test"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Open a PIO file for read

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  pioerr = pio_openfile (  &
      iosystem = pio_ios,  &
      file     = pio_file1,  &
      iotype   = iotype,  &
      fname    = fname,  &
      mode     = 0)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO file open failure"
  write(name, *) "Open PNETCDF PIO file for read: ", fname//'_pnetcdf'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Set variable descriptor frame number

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  call pio_setframe (vardesc=pio_vardesc1, frame=1_PIO_offset)
  rc = ESMF_SUCCESS
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO setframe failure"
  write(name, *) "PIO PNETCDF setframe for read test"
  call ESMF_Test ((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------
 
!------------------------------------------------------------------------
  !NEX_UTest
  ! Read back data

  read_data = -42.42
#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  call pio_read_darray (  &
      file    = pio_file1,  &
      vardesc = pio_vardesc1,  &
      iodesc  = pio_iodesc1,  &
      array   = read_data,  &
      iostat  = pioerr)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO read failure"
  write(name, *) "Read data from PIO PNETCDF file"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Compare results

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  do, i=1, size (test_data)
    if (test_data(i) /= read_data(i)) exit
  end do

  if (i > size (test_data)) then
    rc = ESMF_SUCCESS
  else
    rc = ESMF_FAILURE
    print *, 'Comparison failed at element', i, ', ', test_data(i), ' /= ', read_data(i)
  end if
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO comparison failure"
  write(name, *) "Compare written data with data read back"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Free the decomposition

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  call pio_freedecomp (  &
      file=pio_file1,  &
      iodesc = pio_iodesc1)
  pioerr = 0
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO free decomposition failure"
  write(name, *) "Free PIO PNETCDF decomposition"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest
  ! Close the PIO file

#if defined (ESMF_PIO) && defined (ESMF_PNETCDF)
  call pio_closefile (  &
      file     = pio_file1)
  pioerr = 0
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO file close failure"
  write(name, *) "Close PIO PNETCDF file after read"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

! TODO:
! iotypes we could also be testing for:
!  PIO_iotype_netcdf4c (ESMF_IOFMT_NETCDF4C)
!  PIO_iotype_netcdf4p (ESMF_IOFMT_NETCDF4P)

!------------------------------------------------------------------------
  !NEX_UTest
  ! Finalize PIO

#if defined (ESMF_PIO)
  call pio_finalize (iosystem=pio_ios, ierr=pioerr)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, pioerr == 0)
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "PIO finalization failure"
  write(name, *) "Finalize PIO instance"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_IO_PIOUTest
