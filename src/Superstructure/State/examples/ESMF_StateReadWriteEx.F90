! $Id: ESMF_StateReadWriteEx.F90,v 1.1 2009/10/16 05:58:29 eschwab Exp $
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
    program ESMF_StateReadWriteEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------
!BOE
!\subsubsection{State Read/Write from/to a netCDF file}
!
!  Initialize ESMF and Create an empty {\tt ESMF\_State}, which will be
!  subsequently filled with {\tt ESMF\_Array}s from a file.
!  
!EOE
!==============================================================================
!BOC
! !PROGRAM: ESMF_StateReadWriteEx - State Read/Write from/to a netCDF file
!
! !DESCRIPTION:
!
! This program shows an example of reading and writing Arrays from a State
! to/from a netCDF file.
!-----------------------------------------------------------------------------

    ! ESMF Framework module
    use ESMF_Mod
    implicit none

    ! Local variables
    type(ESMF_State) :: state
    type(ESMF_Array) :: latArray, lonArray, timeArray, humidArray, &
                        tempArray, pArray, rhArray
    type(ESMF_VM) :: vm
    integer :: rc
    character(ESMF_MAXSTR) :: state_name
!EOC
    integer :: finalrc
    finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!BOE
!\subsubsection{ESMF Initialization and Empty State Create}
!
!  Initialize ESMF and Create an empty {\tt ESMF\_State}, which will be
!  subsequently filled with {\tt ESMF\_Array}s from a file.
!  
!EOE

!BOC
    call ESMF_Initialize(rc=rc)
    call ESMF_VMGetGlobal(vm=vm, rc=rc)

    state_name = "Ocean Import"
    state = ESMF_StateCreate(state_name, ESMF_STATE_IMPORT, rc=rc)  
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Reading Arrays from a netCDF file and Adding to a State}
!
!  The following line of code will read all Array data contained in a netCDF
!  file, place them in {\tt ESMF\_Array}s and add them to an {\tt ESMF\_State}.
!  Only PET 0 reads the file; the remaining PETs get a copy via the subsequent
!  call to {\tt ESMF\_StateReconcile()}.  Note that currently, the data
!  is not distributed; each PET has only one DE which contains a full copy
!  of all the data.  Future versions of ESMF will support data decomposition
!  upon reading a file.
!EOE

!BOC
    ! Read netCDF data file into Array objects in the State on PET 0
    call ESMF_StateRead(state, "io_netcdf_testdata.nc", rc=rc)

    ! Reconcile the Arrays, including their attributes, across all PETS in
    ! the VM
    call ESMF_StateReconcile(state, vm, ESMF_ATTRECONCILE_ON, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!  Only reading data into {\tt ESMF\_Array}s is supported at this time;
!  {\tt ESMF\_ArrayBundle}s, {\tt ESMF\_Field}s, and {\tt ESMF\_FieldBundle}s
!  will be supported in future releases of ESMF.
!EOE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Printing Array data from a State}
!
!  To see that the State now contains the same data as in the file, the
!  following shows how to print out what Arrays are contained within the
!  State and the data contained within each Array.  The netCDF utility
!  "ncdump" can be used to view the contents of the netCDF file.
!EOE

!BOC
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
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!  Note that the Arrays "lat", "lon", and "time" hold spatial and temporal
!  coordinate data for latitude, longitude and time, respectively.  These
!  will be used in future releases of ESMF to create {\tt ESMF\_Grid}s.
!EOE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Writing Array data within a State to a netCDF file}
!
!  All the data within the State on PET 0 can be written out to a netCDF
!  file as follows:
!EOE

!BOC
    ! Write Arrays within the State on PET 0 to a netCDF file
    call ESMF_StateWrite(state, "io_netcdf_testdata_out.nc", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!  Currently writing is limited to PET 0, future versions of ESMF will allow
!  parallel writing, as well as parallel reading.
!EOE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Destroying a State and its constituent Arrays}
!
!  Destroying a State only deallocates the container, not the contents, as
!  the contents may be used in other States and elsewhere.  The contents of a
!  State, such as the Arrays in this example, must be destroyed separately.
!EOE

!BOC
    ! Destroy the State container
    call ESMF_StateDestroy(state, rc=rc)

    ! Destroy the constituent Arrays
    call ESMF_ArrayDestroy(latArray,   rc=rc)
    call ESMF_ArrayDestroy(lonArray,   rc=rc)
    call ESMF_ArrayDestroy(timeArray,  rc=rc)
    call ESMF_ArrayDestroy(humidArray, rc=rc)
    call ESMF_ArrayDestroy(tempArray,  rc=rc)
    call ESMF_ArrayDestroy(pArray,     rc=rc)
    call ESMF_ArrayDestroy(rhArray,    rc=rc)

    call ESMF_Finalize(rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_StateReadWriteEx.F90"
    else
        print *, "FAIL: ESMF_StateReadWriteEx.F90"
    end if

    end program ESMF_StateReadWriteEx
