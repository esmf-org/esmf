! $Id: ESMF_Test.F90,v 1.3 2003/06/20 16:45:02 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF Test Module
      module ESMF_TestMod
!
!==============================================================================
!
! This file contains the Test class definition and all Test class
! methods.
!
!------------------------------------------------------------------------------

!===============================================================================
!BOP
!
! !MODULE: ESMF_TestMod
!
! !DESCRIPTION:
!   contains methods to support testing
!
!------------------------------------------------------------------------------

      implicit none

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_Test
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Test.F90,v 1.3 2003/06/20 16:45:02 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_Test - Print PASS/FAIL messages for tests
!
! !INTERFACE:
      subroutine ESMF_Test(condition, name, failMsg, result, file, line, unit)

! !ARGUMENTS:
      logical, intent(in) :: condition      ! pass/fail condition
      character(*), intent(in) :: name      ! test name
      character(*), intent(in) :: failMsg   ! fail message
      integer, intent(inout) :: result      ! accumulated result
      character(*), intent(in) :: file      ! test file name
      integer, intent(in) :: line           ! test file line number
      integer, intent(in), optional :: unit ! additional output unit number

! !DESCRIPTION:
!     Prints a {\tt PASS} message to stdout if {\tt condition} is true,
!     and a {\tt FAIL} message if {\tt condition} is false.  If {\tt unit}
!     is specified, will in addition write the same message to that 
!     Fortran unit number.
!
!EOP
!-------------------------------------------------------------------------------

      if(condition) then
        print *, "PASS ", trim(name), ", ", trim(file), ", line", line
        if (present(unit)) then
          write(unit, *) "PASS ", trim(name), ", ", trim(file), ", line", line
        endif
      else
        print *, "FAIL ", trim(name), ", ", trim(file), ", line", &
                  line, trim(failMsg)
        if (present(unit)) then
          write(unit, *) "FAIL ", trim(name), ", ", trim(file), ", line", &
                          line, trim(failMsg)
        endif
        result = result + 1  ! count total failures; 0 = all pass
      end if

      end subroutine ESMF_Test

!------------------------------------------------------------------------------

      end module ESMF_TestMod
