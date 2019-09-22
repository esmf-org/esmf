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
#define ESMF_FILENAME "ESMF_IOFileTypeCheck.F90"
!
!     ESMF IOFileTypeCheck Module
      module ESMF_IOFileTypeCheckMod
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_IOFileTypeCheckMod - Grid I/O utility class
!
! !DESCRIPTION:
!
! The code in this file check the grid file type based on the contents in the file
! files
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_UtilMod
      use ESMF_InitMacrosMod    ! ESMF initializer macros
      use ESMF_LogErrMod        ! ESMF error handling
      use ESMF_VMMod
      use ESMF_IOUGridMod
      use ESMF_IOGridspecMod
      use ESMF_IOScripMod
#ifdef ESMF_NETCDF
      use netcdf
#endif

!     NEED TO ADD MORE HERE
      implicit none
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_FileTypeCheck

!==============================================================================

      contains

!==============================================================================

! -------------------------- ESMF-public method -------------------------------
! Return the file type of a given grid file:
! It could be one of the following:
!   ESMF_FILEFORMAT_SCRIP,  ESMF_FILEFORMAT_UGRID,
!   ESMF_FILEFORMAT_ESMFMESH,
!   ESMF_FILEFORMAT_CFGRID (same as ESMF_FILEFORMAT_GRIDSPEC), and
!   ESMF_FILEFORMAT_MOSAIC (GRIDSPEC Mosaic file) and ESMF_FILEFORMAT_TILE (GRIDSPEC Tile file)
!
!   The rule to check the file format is as follows:
!   ESMF_FILEFORMAT_SCRIP:  variables grid_corner_lon and grid_corner_lat exists
!   ESMF_FILEFORMAT_UGRID: a variable with attribute "cf_role" or "standard_name" set to "mesh_topology"
!   ESMF_FILEFORMAT_ESMFMESH: variables nodeCoords and elementConn exists
!   ESMF_FILEFORMAT_CFGRID: variables with attributes "degree_north" and "degree_east" exists
!   ESMF_FILEFORMAT_MOSAIC: a variable with attribute "standard_name" set to "grid_mosaic_spec"
!   ESMF_FILEFORMAT_TILE:  a varilable with attribute "standard_name" set to "grid_tile_spec"
! -----------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FileTypeCheck"
  subroutine ESMF_FileTypeCheck(filename, filetype, varname, rc)

! ARGUMENTS:
    character(len=*),  intent(in)           :: filename
    type(ESMF_Fileformat_Flag), intent(out) :: filetype
    character(len=*), intent(out), optional :: varname  ! for UGRID/GRIDSPEC files
    integer, intent(out), optional          :: rc

    logical :: foundlon, foundlat
    logical :: foundscriplon, foundscriplat
    logical :: foundesmfcoord, foundesmfconn
    logical :: useStandname, foundtype
    integer :: ncStatus
    integer ::  gridid, varid, dimid, len
    character(len=128) :: attvalue
    integer :: i, nvars
    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0

    if (present(rc)) rc = ESMF_FAILURE
    filetype = ESMF_FILEFORMAT_UNKNOWN

    foundlon = .false.
    foundlat = .false.
    foundscriplon = .false.
    foundscriplat = .false.
    foundesmfcoord = .false.
    foundesmfconn = .false.
    foundtype = .false.

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

     !find out how many variables are in the file
     ncStatus = nf90_inquire(gridid, nVariables=nvars)
     errmsg = 'nf90_inquire failed '//trim(filename)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
     do i=1,nvars
        ! Check UGRID or GRIDSPEC MOSAIC/TILE
        ! Seach for a variable with cf_role or standard_name = 'mesh_topology'
        ncStatus=nf90_get_att(gridid, i, 'cf_role', attvalue)
        if (ncStatus == nf90_noerror) then
          ncStatus = nf90_inquire_attribute(gridid, i, 'cf_role', len=len)
        else
          ncStatus = nf90_get_att (gridid, i, "standard_name", attvalue)
          useStandName = .true.
          if (ncStatus == nf90_noerror) then
             ncStatus = nf90_inquire_attribute(gridid, i, 'standard_name', len=len)
          endif
        endif
        if (ncStatus == nf90_noerror) then
          if (attvalue(len:len) .eq. achar(0)) len = len-1
          if (attvalue(1:len) .eq. 'mesh_topology') then
             fileType=ESMF_FILEFORMAT_UGRID
             foundtype = .true.
          elseif (useStandName .and. attvalue(1:len) .eq. 'grid_mosaic_spec') then
             ! It is a GRIDSPEC MOSAIC file
             filetype = ESMF_FILEFORMAT_MOSAIC
             foundtype = .true.
          elseif (useStandName .and. attvalue(1:len) .eq. 'grid_tile_spec') then
             ! It is a GRIDSPEC TILE file
             filetype = ESMF_FILEFORMAT_TILE
             foundtype = .true.
          endif
          if (foundtype) then
            if (present(varname)) then
              ncStatus = nf90_inquire_variable(gridid, i, name=attvalue)
              errmsg = 'nf90_inquire_variable failed '//trim(filename)
              if (CDFCheckError (ncStatus, &
                   ESMF_METHOD, &
                   ESMF_SRCLINE,&
                   errmsg, &
                   rc)) return
              varname = trim(attvalue)
            endif
            rc=ESMF_SUCCESS
            goto 1000
          endif
        endif
        ! check if it is CFGRID
        ! check if the coordinate variables exist or not
                ncStatus = nf90_inquire_attribute(gridid, i, "units", len=len)
        if (ncStatus == nf90_noerror) then
          ncStatus=nf90_get_att(gridid, i, 'units', attvalue)
          if (ncStatus /= nf90_noerror) then
            print '("NetCDF error: ", A)', trim(nf90_strerror(ncStatus))
            goto 1000
          endif
          if (attvalue(len:len) .eq. achar(0)) len = len-1
          if (len >= 6 .and. (attvalue(1:6) .eq. "degree")) then
            if (attvalue(1:len) .eq. "degrees_east" .or. &
                attvalue(1:len) .eq. "degree_east" .or. &
                attvalue(1:len) .eq. "degree_E" .or. &
                attvalue(1:len) .eq. "degrees_E" .or. &
                attvalue(1:len) .eq. "degreeE" .or. &
                attvalue(1:len) .eq. "degreesE")  then
                foundlon = .true.
             else if (attvalue(1:len) .eq. "degrees_north" .or. &
                attvalue(1:len) .eq. "degree_north" .or. &
                attvalue(1:len) .eq. "degree_N" .or. &
                attvalue(1:len) .eq. "degrees_N" .or. &
                attvalue(1:len) .eq. "degreeN" .or. &
                attvalue(1:len) .eq. "degreesN")  then
                foundlat = .true.
             endif
          endif
        endif

        ! Check the variable names to determin if it is SCRIP or ESMFMESH
        ncStatus = nf90_inquire_variable(gridid, i, name=attvalue)
        errmsg = 'nf90_inquire_variable failed '//trim(filename)
        if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,&
                errmsg, &
                rc)) return
        if (trim(attvalue) .eq. 'grid_corner_lon') then
           foundscriplon = .true.
        elseif (trim(attvalue) .eq. 'grid_corner_lat') then
           foundscriplat = .true.
        elseif (trim(attvalue) .eq. 'nodeCoords') then
           foundesmfcoord = .true.
        elseif (trim(attvalue) .eq. 'elementConn') then
           foundesmfconn = .true.
        endif
     enddo

     if (foundlon .and. foundlat) then
         filetype = ESMF_FILEFORMAT_GRIDSPEC
         rc=ESMF_SUCCESS
         goto 1000
     endif

     if (foundscriplon .and. foundscriplat) then
         filetype = ESMF_FILEFORMAT_SCRIP
         rc=ESMF_SUCCESS
         goto 1000
     endif

     if (foundesmfcoord .and. foundesmfconn) then
         filetype = ESMF_FILEFORMAT_ESMFMESH
         rc=ESMF_SUCCESS
         goto 1000
     endif

1000 continue
     ncStatus = nf90_close(gridid)
     if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return

     return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

  end subroutine ESMF_FileTypeCheck

#undef  ESMF_METHOD
#define ESMF_METHOD "CDFCheckError"
function CDFCheckError (ncStatus, module, fileName, lineNo, errmsg, rc)

    logical                       :: CDFCheckError

    integer,          intent(in)  :: ncStatus
    character(len=*), intent(in)  :: module
    character(len=*), intent(in)  :: fileName
    integer,          intent(in)  :: lineNo
    character(len=*), intent(in)  :: errmsg
    integer, intent(out),optional :: rc

    integer, parameter :: nf90_noerror = 0

    CDFCheckError = .FALSE.

#ifdef ESMF_NETCDF
    if ( ncStatus .ne. nf90_noerror) then
        call ESMF_LogWrite (  &
            msg="netCDF Error: " // trim (errmsg) // ": " // trim (nf90_strerror(ncStatus)),  &
            logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=lineNo, file=fileName, method=module)
        print '("NetCDF Error: ", A, " : ", A)', &
            trim(errmsg),trim(nf90_strerror(ncStatus))
        call ESMF_LogFlush()
        if (present(rc)) rc = ESMF_FAILURE
        CDFCheckError = .TRUE.
    else
       if (present(rc)) rc = ESMF_SUCCESS
       return
    end if
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

end function CDFCheckError


end module ESMF_IOFileTypeCheckMod


