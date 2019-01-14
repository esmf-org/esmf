! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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
      use ESMF 
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

!   ! Local variables
    type(ESMF_State) :: state
    type(ESMF_Array) :: latArray, lonArray, timeArray, humidArray, &
                        tempArray, pArray, rhArray
    type(ESMF_VM) :: vm
    integer :: localPet
    integer :: i
    integer :: rc, localrc
    logical :: have_netcdf

    character(*), parameter :: netcdf_file    = 'io_netcdf_testdata.nc'
    character(*), parameter :: netcdf_fileout = 'io_netcdf_testdata_out.nc'

    integer, parameter :: narrays = 7
    character(8), parameter :: arraynames(narrays) = (/  &
        "lat ", "lon ", "time",  &
        "Q   ", "TEMP", "p   ",  &
        "rh  "  &
    /)
    type(ESMF_StateItem_Flag) :: itemtype
    integer :: itemcount
    logical :: passfail

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
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_VMGetGlobal(vm=vm, rc=rc)
      call ESMF_VMGet (vm, localPet=localPet)

#ifdef ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Creation of an empty export State 
      state = ESMF_StateCreate(name="Ocean Export", stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)  
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test reading a with no file name specified
      call ESMF_StateRead(state, filename=' ', rc=rc)
      write(failMsg, *) "Did not return an error"
      write(name, *) "Reading a file with no file name specified"
      call ESMF_Test(rc /= ESMF_SUCCESS, &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test reading a netCDF file into Arrays in a State
      call ESMF_StateRead(state, filename=netcdf_file, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS when reading: ", netcdf_file
      write(name, *) "Reading netCDF file ", netcdf_file, " into Arrays in a State"
      call ESMF_Test((rc == ESMF_SUCCESS .or. rc == ESMF_RC_LIB_NOT_PRESENT), &
                      name, failMsg, result, ESMF_SRCLINE)

      have_netcdf = rc /= ESMF_RC_LIB_NOT_PRESENT
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test for number of State items read from the file.
      call ESMF_StateGet (state, itemCount=itemcount, rc=localrc)
      write(failMsg, *) "Incorrect number of items read from file"
      write(name, *) "Checking read-in Array item count in a State test"
      ! Current implementation reads data on PET 0
      if (localPet == 0) then
        passfail = itemcount == narrays
      else
        passfail = itemcount == 0
      end if
      call ESMF_Test((rc == ESMF_SUCCESS .and. passfail) .or. .not. have_netcdf, &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test for presense of State items read from the file.
      if (localPet == 0) then
        do, i=1, narrays
          call ESMF_StateGet (state, itemname=arraynames(i), itemType=itemtype, rc=localrc)
          if (localrc /= ESMF_SUCCESS) exit
          if (itemtype /= ESMF_STATEITEM_ARRAY) then
            localrc = ESMF_RC_NOT_FOUND
            exit
          end if
        end do
        rc = merge (ESMF_SUCCESS, localrc, i>narrays)
      else
        i = 1
        rc = ESMF_SUCCESS
      end if
      write(failMsg, *) "Could not find read-in Array: ", trim (arraynames(min (i, narrays)))
      write(name, *) "Checking read-in Array names in a State test"
      call ESMF_Test((rc == ESMF_SUCCESS .or. .not. have_netcdf), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test reconciling Arrays across all PETs in a VM
      call ESMF_StateReconcile(state, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Reconciling Arrays across all PETs in a VM"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test writing Arrays with no file name specified
      call ESMF_StateWrite(state, filename=' ', rc=rc)
      write(failMsg, *) "Did not return an error"
      write(name, *) "Writing a file with no file name"
      call ESMF_Test(rc /= ESMF_SUCCESS, &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test writing Arrays in a State to a netCDF file
      call ESMF_StateWrite(state, filename=netcdf_fileout, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing netCDF file ", netcdf_fileout, " from Arrays in a State"
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
      if (have_netcdf) then
        call ESMF_Test(rc == ESMF_SUCCESS, &
            name, failMsg, result, ESMF_SRCLINE)
      else
        call ESMF_Test(rc == ESMF_RC_NOT_FOUND, &
            name, failMsg, result, ESMF_SRCLINE)
      end if
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "lon", lonArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'lon' from a State"
      if (have_netcdf) then
        call ESMF_Test(rc == ESMF_SUCCESS, &
            name, failMsg, result, ESMF_SRCLINE)
      else
        call ESMF_Test(rc == ESMF_RC_NOT_FOUND, &
            name, failMsg, result, ESMF_SRCLINE)
      end if
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "time", timeArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'time' from a State"
      if (have_netcdf) then
        call ESMF_Test(rc == ESMF_SUCCESS, &
            name, failMsg, result, ESMF_SRCLINE)
      else
        call ESMF_Test(rc == ESMF_RC_NOT_FOUND, &
            name, failMsg, result, ESMF_SRCLINE)
      end if
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "Q", humidArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'Q' from a State"
      if (have_netcdf) then
        call ESMF_Test(rc == ESMF_SUCCESS, &
            name, failMsg, result, ESMF_SRCLINE)
      else
        call ESMF_Test(rc == ESMF_RC_NOT_FOUND, &
            name, failMsg, result, ESMF_SRCLINE)
      end if
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "TEMP", tempArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'TEMP' from a State"
      if (have_netcdf) then
        call ESMF_Test(rc == ESMF_SUCCESS, &
            name, failMsg, result, ESMF_SRCLINE)
      else
        call ESMF_Test(rc == ESMF_RC_NOT_FOUND, &
            name, failMsg, result, ESMF_SRCLINE)
      end if
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "p", pArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'p' from a State"
      if (have_netcdf) then
        call ESMF_Test(rc == ESMF_SUCCESS, &
            name, failMsg, result, ESMF_SRCLINE)
      else
        call ESMF_Test(rc == ESMF_RC_NOT_FOUND, &
            name, failMsg, result, ESMF_SRCLINE)
      end if
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test getting an Array from a State
      call ESMF_StateGet(state, "rh", rhArray, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array named 'rh' from a State"
      if (have_netcdf) then
        call ESMF_Test(rc == ESMF_SUCCESS, &
            name, failMsg, result, ESMF_SRCLINE)
      else
        call ESMF_Test(rc == ESMF_RC_NOT_FOUND, &
            name, failMsg, result, ESMF_SRCLINE)
      end if
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

      call ESMF_TestEnd(ESMF_SRCLINE)
 
      end program ESMF_StateReadWriteUTest

!-------------------------------------------------------------------------
