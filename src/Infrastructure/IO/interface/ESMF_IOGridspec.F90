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
#define ESMF_FILENAME "ESMF_IOGridspec.F90"
!
!     ESMF IOGridspec Module
      module ESMF_IOGridspecMod
!
!==============================================================================
!
! This file contains the Grid class definition and all Grid class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_IOGridspecMod - Grid I/O utility class
!
! !DESCRIPTION:
!
! The code in this file reads the Gridspec Grid files
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_UtilMod
      use ESMF_InitMacrosMod    ! ESMF initializer macros
      use ESMF_LogErrMod        ! ESMF error handling
      use ESMF_VMMod
#ifdef ESMF_NETCDF
      use netcdf
#endif

!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE:
      private
      integer, SAVE :: PetNo, PetCnt
      type(ESMF_VM), SAVE:: vm
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_GridspecInq
  public ESMF_GridspecGetVarByName
  public ESMF_GridspecGetVar1D
  public ESMF_GridspecGetVar2D

!==============================================================================

      contains

!==============================================================================
!
! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecInq"

!BOPI
! !ROUTINE: ESMF_GridspecInq: Find out the variable IDs for the latitude and longitude
!           variables (coordids), the dimensions of the coordinates (ndims) (1 for regular
!           gird, 2 for curvilinear), the dimension IDs (dimids) for the lat, lon, and
!           the grid dimension (grid_dims).
!       and whether grid masking is present, from a GridSpec file and its mosaic.
!
! !INTERFACE:
  subroutine ESMF_GridspecInq(grid_filename, ndims, grid_dims, coord_names, &
        is3D, dimids, coordids, hasbound, units, rc)

! !ARGUMENTS:

    character(len=*), intent(in)  :: grid_filename
    integer, intent(out)          :: ndims
    integer, intent(out)          :: grid_dims(:)
    logical, intent(in), optional :: is3D
    character(len=*), intent(in), optional :: coord_names(:)
    integer, intent(out), optional:: dimids(:)
    integer, intent(out), optional:: coordids(:)
    logical, intent(out), optional:: hasbound
    character(len=*), intent(out), optional ::units
    integer, intent(out), optional :: rc

    integer :: localrc, ncStatus
    integer :: varid, dimid
    integer :: varids(2)
    integer :: nvars, len
    integer :: gridid, i
    integer :: dimidslocal(2), localdimids(2)
    character (len=256) :: errmsg, boundvar
    character (len=80)  :: attstr, axisstr
    logical :: foundlon, foundlat
    logical :: useCoordName
    logical :: is3Dlocal;
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    foundlat = .false.
    foundlon = .false.
    if (present(is3D)) then
      is3Dlocal = is3D
    else
      is3Dlocal = .false.
    endif
    ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        trim(grid_filename), &
        rc)) return

    if (present(coord_names)) then
        if (size(coord_names) /= 2) then
            call ESMF_LogSetError(ESMF_FAILURE, &
                 msg="- coord_names has to be of size 2", &
                 ESMF_CONTEXT, rcToReturn=rc)
            return
        endif
      do i=1,2
        ncStatus = nf90_inq_varid(gridid, coord_names(i), varid)
        errmsg ="variable "//trim(coord_names(1))//" in "//trim(grid_filename)
        if (CDFCheckError (ncStatus, &
                ESMF_METHOD,  &
                ESMF_SRCLINE, &
                errmsg, &
                rc)) return
        ! The coordinate variables are not in particular order.  It could be lon, lat
        ! or lat, lon.  We need to use its units value to decide whether it is longitude or
        ! latitude.
        ! First check units attribute for longitude, it has to be one of degrees_east,
        ! degree_east, degree_E, degreeE or degreesE
        !
        ! Added support for Cartesian coordinates in v8.0.0.  The Catesian coordinates
        ! should have attribute "axis" set to "X" or "Y" and units are "m" or
        ! "meters" for meters and "km" for kilometers.

        ncStatus = nf90_inquire_attribute(gridid, varid, "units", len=len)
        errmsg ="attribute units for "//trim(coord_names(1))//" in "//trim(grid_filename)
        if (CDFCheckError (ncStatus, &
                ESMF_METHOD,  &
                ESMF_SRCLINE, &
                errmsg, &
                rc)) return
        ncStatus = nf90_get_att(gridid, varid, 'units',attstr)
        if (CDFCheckError (ncStatus, &
                ESMF_METHOD,  &
                ESMF_SRCLINE, &
                errmsg, &
                rc)) return
        if (attstr(len:len) .eq. achar(0)) len = len-1
        if (attstr(1:len) .eq. 'degrees_east' .or. &
                attstr(1:len) .eq. 'degree_east' .or. &
                attstr(1:len) .eq. 'degree_E' .or. &
                attstr(1:len) .eq. 'degrees_E' .or. &
                attstr(1:len) .eq. 'degreeE' .or. &
                attstr(1:len) .eq. 'degreesE')  then
                foundlon = .true.
                varids(1)=varid
                if (present(units)) units = "degrees"
        endif
        ! It is not longitude, check for Latitude units
        if (attstr(1:len) .eq. 'degrees_north' .or. &
                attstr(1:len) .eq. 'degree_north' .or. &
                attstr(1:len) .eq. 'degree_N' .or. &
                attstr(1:len) .eq. 'degrees_N' .or. &
                attstr(1:len) .eq. 'degreeN' .or. &
                attstr(1:len) .eq. 'degreesN')  then
                varids(2)=varid
                foundlat = .true.
                if (present(units)) units = "degrees"
        endif
        if (attstr(1:len) .eq. 'm' .or. attstr(1:len) .eq. 'meters' .or. &
            attstr(1:len) .eq. 'km' .or. attstr(1:len) .eq. 'kilometers') then
            if (present(units)) units = attstr(1:len)
            ! check if the axis attribute exists
            ncStatus = nf90_get_att(gridid, varid, 'axis',axisstr)
            if (trim(axisstr) .eq. 'X') then
                foundlon = .true.
                varids(1)=varid
            elseif (trim(axisstr) .eq. 'Y') then
                varids(2)=varids(1)
                foundlat = .true.
                varids(2)=varid
            else
                call ESMF_LogSetError(ESMF_FAILURE, &
                   msg="- Not a valid coordinate variable.", &
                   ESMF_CONTEXT, rcToReturn=rc)
                return
            endif
        endif
      enddo
      if (.not. (foundlon .and. foundlat)) then
        call ESMF_LogSetError(ESMF_FAILURE, &
           msg="- Not a valid coordinate variable.", &
           ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    else
        ! get a list of variables in the file, inquire its standard_name and/or long_name to
        ! find out which one is longitude and which one is latitude variable
        ncStatus = nf90_inquire(gridid, nVariables = nvars)
        if (CDFCheckError (ncStatus, &
            ESMF_METHOD,  &
            ESMF_SRCLINE, &
            trim(grid_filename), &
            rc)) return

        foundlon = .false.
        foundlat  = .false.
        do i=1,nvars
           ! check if units attribute exists. If not, skip.  If yes, check if
           ! the value is degrees_east or degrees_north
           !
           ncStatus = nf90_inquire_attribute(gridid, i, "units", len=len)
           if (ncStatus /= nf90_noerror) CYCLE
           ncStatus = nf90_get_att(gridid, i, 'units',attstr)
           if (CDFCheckError (ncStatus, &
                ESMF_METHOD,  &
                ESMF_SRCLINE, &
                errmsg, &
                rc)) return

           if (attstr(len:len) .eq. achar(0)) len = len-1
           if (len >= 6 .and. (attstr(1:6) .eq. "degree")) then
              if (attstr(1:len) .eq. "degrees_east" .or. &
                  attstr(1:len) .eq. "degree_east" .or. &
                  attstr(1:len) .eq. "degree_E" .or. &
                  attstr(1:len) .eq. "degrees_E" .or. &
                  attstr(1:len) .eq. "degreeE" .or. &
                  attstr(1:len) .eq. "degreesE")  then
                  if (foundlon) then
                   call ESMF_LogSetError(ESMF_FAILURE, &
                      msg="- Duplicate longitude variables defined", &
                      ESMF_CONTEXT, rcToReturn=rc)
                  return
                else
                   varids(1)=i
                   foundlon = .true.
                   if (present(units)) units = "degrees"
                endif
              else if (attstr(1:len) .eq. "degrees_north" .or. &
                   attstr(1:len) .eq. "degree_north" .or. &
                   attstr(1:len) .eq. "degree_N" .or. &
                   attstr(1:len) .eq. "degrees_N" .or. &
                   attstr(1:len) .eq. "degreeN" .or. &
                   attstr(1:len) .eq. "degreesN")  then
                if (foundlat) then
                  call ESMF_LogSetError(ESMF_FAILURE, &
                     msg="- Duplicate latitude variables defined", &
                     ESMF_CONTEXT, rcToReturn=rc)
                  return
                else
                  varids(2)=i
                  foundlat = .true.
                  if (present(units)) units = "degrees"
                endif
              !else
          !        print *, "not the right units :", attstr(1:len), len
              endif
           endif
         enddo
    endif

    ! Check if "bounds" attribute is defined for the coordinate variable
    if (present(hasbound)) then
      ncStatus = nf90_get_att(gridid, varids(1), "bounds", boundvar)
      if (ncStatus /= nf90_noerror) then
         hasbound = .false.
      else
         hasbound = .true.
      endif
    endif

    ! find the dimension of the coordinate variables
    ncStatus = nf90_inquire_variable(gridid, varids(1), ndims=ndims, dimids=dimidslocal)
    errmsg ="longitude variable in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return

    ! if the longitude is a 1D array, need to get the latitude dimension from the
    ! latitude variable
    if (ndims == 1) then
         if (present(dimids)) dimids(1)=dimidslocal(1)
         localdimids(1)=dimidslocal(1)
         ncStatus = nf90_inquire_variable(gridid, varids(2), ndims=ndims, dimids=dimidslocal)
         errmsg ="latitude variable in "//trim(grid_filename)
         if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
         if (present(dimids)) dimids(2)=dimidslocal(1)
         localdimids(2)=dimidslocal(1)
    else        
         if (present(dimids)) dimids = dimidslocal
         localdimids = dimidslocal
    endif
    ! find the dimension values
    ncStatus = nf90_inquire_dimension (gridid, localdimids(1), len=grid_dims(1))
    errmsg ="grid 1st dimension in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
    ncStatus = nf90_inquire_dimension (gridid, localdimids(2), len=grid_dims(2))
    errmsg ="grid 2nd dimension in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return

    ncStatus = nf90_close(gridid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, &
      trim(grid_filename), &
      rc)) return

    if (present(coordids)) coordids = varids
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    ndims = 0
    grid_dims = 0
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

end subroutine ESMF_GridspecInq

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecGetVar1D"
!BOPI
! !ROUTINE: ESMF_GridspecGetVar1D
!

! !INTERFACE:
  subroutine ESMF_GridspecGetVar1D(grid_filename, varids, loncoord, latcoord, &
                        cornerlon, cornerlat, rc)
!
! !ARGUMENTS:
    character(len=*), intent(in)  :: grid_filename
    integer, intent(in)            :: varids(:)
    real(ESMF_KIND_R8), intent(out) :: loncoord(:)
    real(ESMF_KIND_R8), intent(out) :: latcoord(:)
    real(ESMF_KIND_R8), intent(out), optional  :: cornerlon(:,:)
    real(ESMF_KIND_R8), intent(out), optional  :: cornerlat(:,:)
    integer, intent(out), optional:: rc

    integer:: ncStatus
    integer:: gridid, boundId, VarId, totaldims
    integer:: len, i
    character(len=256) :: errmsg, boundvar
    character (len=80)  :: attstr

#ifdef ESMF_NETCDF

    ! Open the grid files
    ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(grid_filename), &
      rc)) return

    ncStatus = nf90_get_var(gridid, varids(1), loncoord)
    errmsg = "longitude variable in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      errmsg, &
      rc)) return
    ncStatus = nf90_get_var(gridid, varids(2), latcoord)
    errmsg = "latitude variable in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      errmsg, &
      rc)) return
    
    if (present(cornerlon)) then
      ! find the bound variable for lon
      ncStatus = nf90_inquire_attribute(gridid, varids(1), "bounds", len=len)
      errmsg = "attribute bounds in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      ncStatus = nf90_get_att(gridid, varids(1), "bounds", boundvar)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return

      ncStatus = nf90_inq_varid(gridid, boundvar(1:len), boundId)
      errmsg = "longitude bound variable in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      ncStatus = nf90_get_var(gridid, boundId, cornerlon)
      errmsg = "longitude bound variable in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
    endif

    if (present(cornerlat)) then
      ! find the bound variable for lat
      ncStatus = nf90_inquire_attribute(gridid, varids(2), "bounds", len=len)
      errmsg = "attribute bounds in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      ncStatus = nf90_get_att(gridid, varids(2), "bounds", boundvar)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return

      ncStatus = nf90_inq_varid(gridid, boundvar(1:len), boundId)
      errmsg = "latitude bound variable in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      ncStatus = nf90_get_var(gridid, boundId, cornerlat)
      errmsg = "longitude bound variable in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
    endif

    ncStatus = nf90_close(gridid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(grid_filename),&
      rc)) return

    if(present(rc)) rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

end subroutine ESMF_GridspecGetVar1D

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecGetVar2D"
!BOPI
! !ROUTINE: ESMF_GridspecGetVar2D
!
! !INTERFACE:
  subroutine ESMF_GridspecGetVar2D(grid_filename, varids, loncoord, latcoord, &
                                  cornerlon, cornerlat, start, count, rc)
!
! !ARGUMENTS:
    character(len=*), intent(in)    :: grid_filename
    integer, intent(in)             :: varids(:)
    real(ESMF_KIND_R8), intent(out), optional  :: loncoord(:,:)
    real(ESMF_KIND_R8), intent(out), optional  :: latcoord(:,:)
    real(ESMF_KIND_R8), intent(out), optional  :: cornerlon(:,:,:)
    real(ESMF_KIND_R8), intent(out), optional  :: cornerlat(:,:,:)
    integer, intent(in), optional   :: start(:), count(:)
    integer, intent(out), optional  :: rc

    integer:: ncStatus
    integer:: gridid, boundId
    integer:: len, i
    integer:: start2(2), count2(2), start3(3), count3(3)
    integer:: dimids(2), grid_dims(2)
    character(len=256) :: errmsg
    character(len=80)  :: boundvar, attstr

#ifdef ESMF_NETCDF
    ! Open the grid and mosaic files
    ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(grid_filename), &
      rc)) return

    ! inquire the variable dimensions
    ncStatus = nf90_inquire_variable(gridid, varids(1), dimids = dimids)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(grid_filename), &
      rc)) return

    ! find the dimension values
    ncStatus = nf90_inquire_dimension (gridid, dimids(1), len=grid_dims(1))
    errmsg ="grid 1st dimension in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
    ncStatus = nf90_inquire_dimension (gridid, dimids(2), len=grid_dims(2))
    errmsg ="grid 2nd dimension in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return

    if (present(start)) then
        start2(:) = start(1:2)
        start3(2)=start(1)
        start3(3)=start(2)
        start3(1)=1
    else
        start2(:)=1
        start3(:)=1
    endif
    if (present(count)) then
        count2(:) = count(1:2)
        count3(2)=count(1)
        count3(3)=count(2)
        count3(1)=4
    else
        count2(:) = grid_dims(1:2)
        count3(2)=grid_dims(1)
        count3(3)=grid_dims(2)
        count3(1)=4
    endif

    if (present(loncoord)) then
       ncStatus = nf90_get_var(gridid, varids(1), loncoord, &
                start=start2, count=count2)
       errmsg = "longitude variable in "//trim(grid_filename)
       if (CDFCheckError (ncStatus, &
         ESMF_METHOD, &
         ESMF_SRCLINE,&
         errmsg, &
         rc)) return
    endif
    if (present(latcoord)) then
       ncStatus = nf90_get_var(gridid, varids(2), latcoord, &
                start=start2, count=count2)
       errmsg = "latitude variable in "//trim(grid_filename)
       if (CDFCheckError (ncStatus, &
         ESMF_METHOD, &
         ESMF_SRCLINE,&
         errmsg, &
         rc)) return
    endif
    if (present(cornerlon)) then
      ! find the bound variable for lon
      ncStatus = nf90_inquire_attribute(gridid, varids(1), "bounds", len=len)
      errmsg = "attribute bounds in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      ncStatus = nf90_get_att(gridid, varids(1), "bounds", boundvar)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return

      ncStatus = nf90_inq_varid(gridid, boundvar(1:len), boundId)
      errmsg = "longitude bound variable in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      ncStatus = nf90_get_var(gridid, boundId, cornerlon, &
                 start=start3, count=count3)
      errmsg = "longitude bound variable in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
    endif

    if (present(cornerlat)) then
      ! find the bound variable for lat
      ncStatus = nf90_inquire_attribute(gridid, varids(2), "bounds", len=len)
      errmsg = "attribute bounds in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      ncStatus = nf90_get_att(gridid, varids(2), "bounds", boundvar)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return

      ncStatus = nf90_inq_varid(gridid, boundvar(1:len), boundId)
      errmsg = "latitude bound variable in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      ncStatus = nf90_get_var(gridid, boundId, cornerlat, &
                 start=start3, count=count3)
      errmsg = "longitude bound variable in "//trim(grid_filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
    endif

    ncStatus = nf90_close(gridid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(grid_filename),&
      rc)) return

    if(present(rc)) rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

end subroutine ESMF_GridspecGetVar2D

!------------------------------------------------------------------------------
!
!  Get one 2D slice of the data and the missing value attribute
!  It will be used to generate mask.  The variable field can be 2D, 3D, or 4D,
!  assuming the lon, lat are the first two dimensions (fast changing two dimensions).
!  The missing value attribute name is either "missing_value" or "_FillValue"
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecGetVarByName"
!BOPI
! !ROUTINE: ESMF_GridspecGetVarByName
!
! !INTERFACE:
  subroutine ESMF_GridspecGetVarByName(grid_filename, var_name, dimids,  &
                        var_buffer, missing_value, start, count, rc)
    character(len=*),  intent(in) :: grid_filename
    character(len=*),  intent(in) :: var_name
    integer, intent(in)           :: dimids(:)
    real(ESMF_KIND_R8), intent(out) :: var_buffer(:,:)
    real(ESMF_KIND_R8), intent(out), optional:: missing_value
    integer, intent(in), optional :: start(:), count(:)
    integer, intent(out), optional:: rc

    integer:: ncStatus
    integer:: gridid, varid, ndims
    integer:: vardimids(4)
    integer:: len, i
    integer, pointer:: lstart(:), lcount(:)
    character(len=256) :: errmsg
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    ! Open the grid and mosaic files
    ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(grid_filename), &
      rc)) return

    ncStatus = nf90_inq_varid( gridid, var_name, varid)
    errmsg = "variable "//trim(var_name)// " in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      errmsg, &
      rc)) return

    ! get missisng value
    if (present(missing_value)) then
      ncStatus = nf90_get_att(gridid, varid, "_FillValue", missing_value)
      if (ncStatus /= nf90_noerror) then
          ncStatus = nf90_get_att(gridid, varid, "missing_value", missing_value)
          errmsg = "missing value attribute does not exist for "//trim(var_name)
          if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
      end if
    end if

    ! inquire the dimension of the variable and make sure it matches with the dimids
    ! passed in
    ncStatus = nf90_inquire_variable(gridid, varid, ndims=ndims, dimids=vardimids)
    errmsg = "Variable "//trim(var_name)//" in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      errmsg, &
      rc)) return

    if (ndims < 2) then
        call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
               msg="- variable dimension is less than 2", &
               ESMF_CONTEXT, rcToReturn=rc)
        return
    else if (vardimids(1) /= dimids(1) .or. vardimids(2) /= dimids(2)) then
        call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
               msg="- the first two dimensions does not match with the lat/lon dimension ids", &
               ESMF_CONTEXT, rcToReturn=rc)
        return
    end if

    ! find the dimension length and check if it matches with the var array
    ! get variable

    allocate(lstart(ndims))
    allocate(lcount(ndims))
    lstart(:)=1
    lcount(:)=1
    if (present(start) .and. .not. present(count)) then
        call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
               msg="- Both start and count arguments have to be present at the same time", &
               ESMF_CONTEXT, rcToReturn=rc)
        return
    endif
    if (present(start)) then
        lstart(1:2)=start
    endif
    if (present(count)) then
        lcount(1:2)=count
    else
        ncStatus = nf90_inquire_dimension (gridid, dimids(1), len=lcount(1))
        if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
        ncStatus = nf90_inquire_dimension (gridid, dimids(2), len=lcount(2))
        if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
    endif
    if (lcount(1) /= size(var_buffer, 1) .or. &
        lcount(2) /= size(var_buffer, 2)) then
        call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
               msg="- the variable array dimension does not match with dimension length", &
               ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    ncStatus = nf90_get_var(gridid, varid, var_buffer, start=lstart, &
               count=lcount)
    errmsg = "Variable "//trim(var_name)//" in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      errmsg, &
      rc)) return

    ncStatus = nf90_close(gridid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(grid_filename),&
      rc)) return

    deallocate(lstart, lcount)
    if(present(rc)) rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

end subroutine ESMF_GridspecGetVarByName

!------------------------------------------------------------------------------
!
!  check CDF file error code
!
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
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

end function CDFCheckError


end module ESMF_IOGridspecMod
