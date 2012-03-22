! $Id: ESMF_IOGridspec.F90,v 1.1 2012/03/22 23:32:44 peggyli Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
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
! !MODULE: ESMF_IOGridspecMod - Grid IO utility class
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
!	and whether grid masking is present, from a GridSpec file and its mosaic.
!
! !INTERFACE:
  subroutine ESMF_GridspecInq(grid_filename, ndims, grid_dims, dimids, coordids, rc)

! !ARGUMENTS:
 
    character(len=*), intent(in)  :: grid_filename
    integer, intent(out)          :: ndims
    integer, intent(out)          :: grid_dims(:)
    integer, intent(out), optional:: dimids(:)
    integer, intent(out), optional:: coordids(:)
    integer, intent(out), optional :: rc

    integer :: localrc, ncStatus
    integer :: varid, dimid
    integer :: varids(2)
    integer :: nvars
    integer :: gridid, i
    integer :: dimidslocal(2), localdimids(2)
    character (len=256) :: errmsg
    character (len=80)  :: attstr
    logical :: foundlon, foundlat

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        trim(grid_filename), &
        rc)) return

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
       ncStatus = nf90_get_att(gridid, i, 'standard_name',attstr)
       if (ncStatus /= nf90_NOERR) then
          ncStatus = nf90_get_att(gridid, i, 'long_name',attstr)
	  if (ncStatus /= nf90_NOERR) CYCLE
       end if
       if (attstr(1:10) .eq. 'longitude') then
	  if (foundlon) then
            call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg="- Duplicate longitude variables defined", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
	  else
	     varids(1)=i
	     foundlon = .true.
	  endif
       else if (attstr(1:9) .eq. 'latitude') then
	  if (foundlon) then
            call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg="- Duplicate longitude variables defined", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
	  else
	     varids(2)=i
	     foundlat = .true.
	  endif
	end if
        if (foundlon .and. foundlat) EXIT
    end do

    ! find the dimension of the coordinate variables
    ncStatus = nf90_inquire_variable(gridid, varids(1), ndims=ndims, dimids=dimidslocal)
    errmsg ="longitude variable in "//trim(grid_filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            trim(grid_filename),&
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
            trim(grid_filename),&
            rc)) return
         if (present(dimids)) dimids(2)=dimidslocal(1)
         localdimids(2)=dimidslocal(1)
    else	
	 if (present(dimids)) dimids = dimidslocal
	 localdimids = dimidslocal
    endif
    ! find the dimension values
    ncStatus = nf90_inquire_dimension (gridid, localdimids(1), len=grid_dims(1))
        if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
    ncStatus = nf90_inquire_dimension (gridid, localdimids(2), len=grid_dims(2))
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
    integer:: gridid, mosaicid, VarId, totaldims
    integer:: len, i
    character(len=256) :: errmsg

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
		                  cornerlon, cornerlat, rc)
!
! !ARGUMENTS:
    character(len=*), intent(in)  :: grid_filename
    integer, intent(in)             :: varids(:)
    real(ESMF_KIND_R8), intent(out)  :: loncoord(:,:)
    real(ESMF_KIND_R8), intent(out)  :: latcoord(:,:)
    real(ESMF_KIND_R8), intent(out), optional  :: cornerlon(:,:)
    real(ESMF_KIND_R8), intent(out), optional  :: cornerlat(:,:)
    integer, intent(out), optional:: rc

    integer:: ncStatus
    integer:: gridid, totaldims
    integer:: len, i
    character(len=256) :: errmsg

#ifdef ESMF_NETCDF
    ! Open the grid and mosaic files
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
                        var_buffer, missing_value, rc)
    character(len=*),  intent(in) :: grid_filename
    character(len=*),  intent(in) :: var_name
    integer, intent(in)           :: dimids(:)
    real(ESMF_KIND_R8), intent(out) :: var_buffer(:,:)
    real(ESMF_KIND_R8), intent(out), optional:: missing_value
    integer, intent(out), optional:: rc

    integer:: ncStatus
    integer:: gridid, varid, ndims
    integer:: vardimids(4)
    integer:: len, i
    integer, pointer:: start(:), count(:)
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
    allocate(start(ndims))
    allocate(count(ndims))
    start(:)=1
    count(:)=1
    ncStatus = nf90_inquire_dimension (gridid, dimids(1), len=count(1))
        if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
    ncStatus = nf90_inquire_dimension (gridid, dimids(2), len=count(2))
        if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
    if (count(1) /= size(var_buffer, 1) .or. & 
	count(2) /= size(var_buffer, 2)) then
	call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
	       msg="- the variable array dimension does not match with dimension length", &
	       ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, &
	       count=count)
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

    deallocate(start, count)
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
        call ESMF_LogWrite (msg="netCDF Status Return Error", logmsgList=ESMF_LOGMSG_ERROR, &
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
