! $Id: ESMF_StateReadWriteUTest.F90,v 1.2.2.1 2010/02/05 20:05:08 svasquez Exp $
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
      '$Id: ESMF_StateReadWriteUTest.F90,v 1.2.2.1 2010/02/05 20:05:08 svasquez Exp $'
!------------------------------------------------------------------------------

!   ! Local variables
    type(ESMF_State) :: state
    type(ESMF_Array) :: latArray, lonArray, timeArray, humidArray, &
                        tempArray, pArray, rhArray
    type(ESMF_VM) :: vm
    integer :: rc

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
      call ESMF_VMGetGlobal(vm=vm, rc=rc)
      
#ifdef ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Creation of an empty export State 
      state = ESMF_StateCreate("Ocean Export", ESMF_STATE_EXPORT, rc=rc)  
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test reading a netCDF file into Arrays in a State
      call ESMF_StateRead(state, "io_netcdf_testdata.nc", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Reading a netCDF file into Arrays in a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_LIB_NOT_PRESENT), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test reconciling Arrays across all PETs in a VM
      call ESMF_StateReconcile(state, vm, ESMF_ATTRECONCILE_ON, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Reconciling Arrays across all PETs in a VM"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test writing Arrays in a State to a netCDF file
      call ESMF_StateWrite(state, "io_netcdf_testdata_out.nc", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing a netCDF file from Arrays in a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_LIB_NOT_PRESENT), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      ! Get each Array by name from the State
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "lat", latArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'lat' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_ARG_INCOMP), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "lon", lonArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'lon' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_ARG_INCOMP), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "time", timeArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'time' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_ARG_INCOMP), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "Q", humidArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'Q' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_ARG_INCOMP), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "TEMP", tempArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'TEMP' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_ARG_INCOMP), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "p", pArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'p' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_ARG_INCOMP), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "rh", rhArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'rh' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_ARG_INCOMP), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      ! Destroy the State
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test destroying a State
      call ESMF_StateDestroy(state, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      ! Destroy the constituent Arrays
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test destroying an Array
      call ESMF_ArrayDestroy(latArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying Array named 'lat' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test destroying an Array
      call ESMF_ArrayDestroy(lonArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying Array named 'lon' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test destroying an Array
      call ESMF_ArrayDestroy(timeArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying Array named 'time' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test destroying an Array
      call ESMF_ArrayDestroy(humidArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying Array named 'Q' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test destroying an Array
      call ESMF_ArrayDestroy(tempArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying Array named 'TEMP' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test destroying an Array
      call ESMF_ArrayDestroy(pArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying Array named 'p' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test destroying an Array
      call ESMF_ArrayDestroy(rhArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying Array named 'rh' from a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS.or.rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      ! End of Exhaustive tests
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      call ESMF_TestEnd(result, ESMF_SRCLINE)
 
      end program ESMF_StateReadWriteUTest

!-------------------------------------------------------------------------
