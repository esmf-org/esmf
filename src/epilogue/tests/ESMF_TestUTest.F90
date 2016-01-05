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
program ESMF_TestUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_TestUTest - Check ESMF_TestMod functionality
!
! !DESCRIPTION:
!
!EOP
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

  character(ESMF_MAXSTR) :: name
  character(ESMF_MAXSTR) :: failMsg

  character(ESMF_MAXSTR) :: string

  type(ESMF_VM) :: vm
  integer :: mypet
  character(2) :: mypet_str

  integer :: i, i_block1
  integer :: iounit, ioerr
  character(16), parameter :: filenames(3) =  &
      (/ 'testfile_base', 'testfile_same', 'testfile_diff' /)
  logical :: same

  integer :: result = 0
  integer :: rc

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Use of ESMF_Test(), true"
  write(failMsg, *) "Dummy fail message"
  call ESMF_Test(.true., name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

! Test file comparison

  ! file_base and file_same are identical, file_diff differs

  call ESMF_VMGetCurrent(vm=vm)
  call ESMF_VMGet (vm, localpet=mypet)
  write (mypet_str, '(a,i1)') '.', mypet

block1:  &
  do, i_block1=1, 1
    do, i=1, size (filenames)
      call ESMF_UtilIOUnitGet (iounit, rc=rc)
      if (rc /= ESMF_SUCCESS) exit block1
      open (iounit, file=trim (filenames(i)) // mypet_str,  &
          form='formatted', action='write', iostat=ioerr)
      if (ioerr /= 0) then
        rc = ESMF_FAILURE
        exit block1
      end if

      string = 'line 1'
      write (iounit, '(a)') string

      if (i /= 3) then
        string = 'line 2'
      else
        string = 'different line 2 Version'
      end if
      write (iounit, '(a)') string

      string = 'line 3'
      write (iounit, '(a)') string

      close (iounit, status='keep')
    end do
  end do block1
  if (rc /= ESMF_SUCCESS) then
    print *, '*** Could not create data files ***'
  end if

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare identical files"
  write(failMsg, *) "Files did not compare"
  same = ESMF_TestFileCompare ('testfile_base' // mypet_str, 'testfile_same' // mypet_str)
  call ESMF_Test(same, name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare non-identical files"
  write(failMsg, *) "Files compared erroneously"
  same = ESMF_TestFileCompare ('testfile_base' // mypet_str, 'testfile_diff' // mypet_str)
  call ESMF_Test(.not. same, name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare non-identical files with exception list (non-match)"
  write(failMsg, *) "Files compared erroneously"
  same = ESMF_TestFileCompare ('testfile_base' // mypet_str, 'testfile_diff' // mypet_str,  &
      (/ 'exception1', 'exception2' /) )
  call ESMF_Test(.not. same, name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare non-identical files with exception list (matching)"
  write(failMsg, *) "Files did not compare with exception"
  same = ESMF_TestFileCompare ('testfile_base' // mypet_str, 'testfile_diff' // mypet_str,  &
      (/ 'exception1', 'exception2', 'Version   ' /) )
  call ESMF_Test(.not. same, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_UtilIOUnitGet (iounit, rc=rc)
  do, i=1, size (filenames)
    open (iounit, file=trim (filenames(i)) // mypet_str,  &
        form='formatted', status='old')
    close (iounit, status='delete')
  end do

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_TestUTest
