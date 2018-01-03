! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_StateReadWriteEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------

#include "ESMF.h"

!BOE
!\subsubsection{Read Arrays from a NetCDF file and add to a State}
! \label{example:StateRdWr}
! This program shows an example of reading and writing Arrays from a State
! from/to a NetCDF file.
!EOE

!-----------------------------------------------------------------------------

!BOC
    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod
    implicit none

    ! Local variables
    type(ESMF_State) :: state
    type(ESMF_Array) :: latArray, lonArray, timeArray, humidArray, &
                        tempArray, pArray, rhArray
    type(ESMF_VM) :: vm
    integer :: localPet, rc
!EOC
    integer :: finalrc, result
    character(ESMF_MAXSTR) :: testname
    character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_StateReadWriteEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(vm=vm, defaultlogfilename="StateReadWriteEx.Log", &
                     logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    state = ESMF_StateCreate(name="Ocean Import",  &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)  
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!  The following line of code will read all Array data contained in a NetCDF
!  file, place them in {\tt ESMF\_Arrays} and add them to an {\tt ESMF\_State}.
!  Only PET 0 reads the file; the States in the other PETs remain empty.
!  Currently, the data is not decomposed or distributed; each PET
!  has only 1 DE and only PET 0 contains data after reading the file.
!  Future versions of ESMF will support data decomposition and distribution
!  upon reading a file.
!
!  Note that the third party NetCDF library must be installed.  For more
!  details, see the "ESMF Users Guide", 
!  "Building and Installing the ESMF, Third Party Libraries, NetCDF" and
!  the website http://www.unidata.ucar.edu/software/netcdf.
!EOE

!BOC
    ! Read the NetCDF data file into Array objects in the State on PET 0
    call ESMF_StateRead(state, "io_netcdf_testdata.nc", rc=rc)

    ! If the NetCDF library is not present (on PET 0), cleanup and exit 
    if (rc == ESMF_RC_LIB_NOT_PRESENT) then
      call ESMF_StateDestroy(state, rc=rc)
      goto 10
    endif
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!  Only reading data into {\tt ESMF\_Arrays} is supported at this time;
!  {\tt ESMF\_ArrayBundles}, {\tt ESMF\_Fields}, and {\tt ESMF\_FieldBundles}
!  will be supported in future releases of ESMF.
!EOE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Print Array data from a State}
!
!  To see that the State now contains the same data as in the file, the
!  following shows how to print out what Arrays are contained within the
!  State and to print the data contained within each Array.  The NetCDF utility
!  "ncdump" can be used to view the contents of the NetCDF file.
!  In this example, only PET 0 will contain data.
!EOE

!BOC
    if (localPet == 0) then
      ! Print the names and attributes of Array objects contained in the State
      call ESMF_StatePrint(state, rc=rc)

      ! Get each Array by name from the State
      call ESMF_StateGet(state, "lat",  latArray,   rc=rc)
      call ESMF_StateGet(state, "lon",  lonArray,   rc=rc)
      call ESMF_StateGet(state, "time", timeArray,  rc=rc)
      call ESMF_StateGet(state, "Q",    humidArray, rc=rc)
      call ESMF_StateGet(state, "TEMP", tempArray,  rc=rc)
      call ESMF_StateGet(state, "p",    pArray,     rc=rc)
      call ESMF_StateGet(state, "rh",   rhArray,    rc=rc)

      ! Print out the Array data
      call ESMF_ArrayPrint(latArray,   rc=rc)
      call ESMF_ArrayPrint(lonArray,   rc=rc)
      call ESMF_ArrayPrint(timeArray,  rc=rc)
      call ESMF_ArrayPrint(humidArray, rc=rc)
      call ESMF_ArrayPrint(tempArray,  rc=rc)
      call ESMF_ArrayPrint(pArray,     rc=rc)
      call ESMF_ArrayPrint(rhArray,    rc=rc)
    endif
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!  Note that the Arrays "lat", "lon", and "time" hold spatial and temporal
!  coordinate data for the dimensions latitude, longitude and time,
!  respectively.  These will be used in future releases of ESMF to create
!  {\tt ESMF\_Grids}.
!EOE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Write Array data within a State to a NetCDF file}
!
!  All the Array data within the State on PET 0 can be written out to a NetCDF
!  file as follows:
!EOE

!BOC
    ! Write Arrays within the State on PET 0 to a NetCDF file
    call ESMF_StateWrite(state, "io_netcdf_testdata_out.nc", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!  Currently writing is limited to PET 0; future versions of ESMF will allow
!  parallel writing, as well as parallel reading.
!EOE

    ! Destroy the State container
    call ESMF_StateDestroy(state, rc=rc)

    if (localPet == 0) then
      ! Destroy the constituent Arrays
      call ESMF_ArrayDestroy(latArray,   rc=rc)
      call ESMF_ArrayDestroy(lonArray,   rc=rc)
      call ESMF_ArrayDestroy(timeArray,  rc=rc)
      call ESMF_ArrayDestroy(humidArray, rc=rc)
      call ESMF_ArrayDestroy(tempArray,  rc=rc)
      call ESMF_ArrayDestroy(pArray,     rc=rc)
      call ESMF_ArrayDestroy(rhArray,    rc=rc)
    endif
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

 10 continue  ! Exit point if NetCDF not present (PET 0)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


    call ESMF_Finalize(rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_StateReadWriteEx.F90"
    else
        print *, "FAIL: ESMF_StateReadWriteEx.F90"
    end if

    end program ESMF_StateReadWriteEx

