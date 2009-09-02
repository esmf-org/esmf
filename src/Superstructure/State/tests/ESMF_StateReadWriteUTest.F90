! $Id: ESMF_StateReadWriteUTest.F90,v 1.1 2009/09/02 05:54:12 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!

!-------------------------------------------------------------------------------

      program ESMF_StateReadWriteUTest

!==============================================================================
!
#include "ESMF.h"
!
!BOP
! !PROGRAM: ESMF_StateReadWriteUTest - Test code which Reads/Writes States
!
! !DESCRIPTION:
!
! The code in this file drives F90 State Read/Write unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod 
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_StateReadWriteUTest.F90,v 1.1 2009/09/02 05:54:12 eschwab Exp $'
!------------------------------------------------------------------------------

!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: sname
    character(1000) :: testName
    type(ESMF_State) :: state

    ! individual test failure messages
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

!-------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      
#ifdef ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Creation of an empty export State 
      sname = "Ocean Export"
      state = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test reading a netCDF file into Arrays in a State
      call ESMF_StateRead(state, "io_netcdf_testdata.nc", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Reading a netCDF file into Arrays in a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_LIB_NOT_PRESENT), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test writing Arrays in a State to a netCDF file
      call ESMF_StateWrite(state, "io_netcdf_testdata_out.nc", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Writing a netCDF file from Arrays in a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_LIB_NOT_PRESENT), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a State
      call ESMF_StateDestroy(state, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      ! End of Exhaustive tests
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      call ESMF_TestEnd(result, ESMF_SRCLINE)
 
  
      end program ESMF_StateReadWriteUTest

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
