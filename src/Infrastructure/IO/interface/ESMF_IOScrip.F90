! $Id: ESMF_IOScrip.F90,v 1.23 2011/03/10 22:05:53 peggyli Exp $
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
#define ESMF_FILENAME "ESMF_IOScrip.F90"
!
!     ESMF IOScrip Module
      module ESMF_IOScripMod
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
! !MODULE: ESMF_IOScripMod - Grid IO utility class
!
! !DESCRIPTION:
!
! The code in this file reads the SCRIP Grid files and write out Scrip Weight
! files
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
  public ESMF_ScripInq
  public ESMF_ScripGetVar
  public ESMF_OutputScripWeightFile
  public ESMF_GetMeshFromFile

!==============================================================================

      contains

!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ScripInq"
!BOPI
! !ROUTINE: ESMF_ScripInq: Return the dimension, grid_rank, and other related 
!  information from a SCRIP file
!
! !INTERFACE:
  subroutine ESMF_ScripInq(filename, grid_size, grid_corners, &
			  grid_dims, grid_rank,rc)

! !ARGUMENTS:
 
    character(len=*), intent(in)  :: filename
    integer, intent(out), optional :: grid_size
    integer, intent(out), optional :: grid_corners
    integer, pointer, optional     :: grid_dims(:)
    integer, intent(out), optional :: grid_rank
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: DimId, VarId
    integer :: ncid, local_rank
    character(len=256) :: errmsg

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, &
      trim(filename), &
      rc)) return

    ! get number of vertices
    errmsg = "dimension grid_size in "//trim(filename)
    if (present(grid_size)) then
      ncStatus = nf90_inq_dimid (ncid, "grid_size", DimId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=grid_size)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
    end if

    ! Get vertex dimension
    if (present(grid_corners)) then
      ncStatus = nf90_inq_dimid (ncid, "grid_corners", DimId)
      errmsg = "dimension grid_corners in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=grid_corners)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
    end if
      
    ! Get vertex dimension
    if (present(grid_rank)) then
      ncStatus = nf90_inq_dimid (ncid, "grid_rank", DimId)
      errmsg = "dimension grid_rank in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=local_rank)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      grid_rank = local_rank
    end if
      
    ! get grid dimension
    ! If we didn't get the rank earlier, get it now
    if (present(grid_dims)) then
      if (.not. present(grid_rank)) then
        ncStatus = nf90_inq_dimid (ncid, "grid_rank", DimId)
        errmsg = "dimension grid_rank in "//trim(filename)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
          rc)) return

        ncStatus = nf90_inquire_dimension (ncid, DimId, len=local_rank)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
        rc)) return
      end if   
      allocate(grid_dims(local_rank))
      ncStatus = nf90_inq_varid (ncid, "grid_dims", VarId)
      errmsg = "dimension grid_dims in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

      ncStatus = nf90_get_var (ncid, VarId, grid_dims)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
    end if
    ncStatus = nf90_close(ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, &
      trim(filename), &
      rc)) return
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_ScripInq

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ScripInqUnits"
!BOPI
! !ROUTINE: ESMF_ScripInqUnits: Return the units attribute for
! coordinate variables, assuming all the coordinate variables
! have the same units
!
! !INTERFACE:
subroutine ESMF_ScripInqUnits(filename, units, rc)

! !ARGUMENTS:

    character(len=*), intent(in)   :: filename
    character(len=*), intent(out)   :: units
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: ncid, VarId, len
    character(len=80) :: buffer, buffer1
    character(len=256) :: errmsg

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, &
      trim(filename), &
      rc)) return

    ncStatus = nf90_inq_varid (ncid, "grid_center_lat", VarId)
    errmsg = "variable grid_center_lat in "//trim(filename)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
 
    ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
    errmsg = "attribute units for grid_center_lat in "//trim(filename)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
    ncStatus = nf90_get_att(ncid, VarId, "units", buffer)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
    
    units = buffer(1:len)
    ! if len != 7, something is wrong, check the value.  If it starts 
    ! with Degrees/degrees/Radians/radians, ignore the garbage after the
    ! word.  Otherwise, return the whole thing
    call ESMF_StringLowerCase(buffer(1:len))
    if ((len > 7) .and. (buffer(1:7) .eq. 'degrees' .or.  &
      buffer(1:7) .eq. 'radians')) then
      units = buffer(1:7)
    else
      units = buffer(1:len)
    endif
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
#endif

    return
end subroutine ESMF_ScripInqUnits

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ScripGetVar"
!BOPI
! !ROUTINE: ESMF_ScripGetVar
!

! !INTERFACE:
  subroutine ESMF_ScripGetVar(filename, grid_center_lon, grid_center_lat, &
	      		grid_imask, grid_corner_lon, grid_corner_lat, &
			grid_area, convertToDeg, rc)
!
! !ARGUMENTS:
    character(len=*), intent(in)  :: filename
    real(ESMF_KIND_R8), intent(inout),optional:: grid_center_lon(:)
    real(ESMF_KIND_R8), intent(inout),optional:: grid_center_lat(:)
    integer, intent(inout), optional:: grid_imask(:)
    real(ESMF_KIND_R8), intent(inout),optional:: grid_corner_lon(:,:)
    real(ESMF_KIND_R8), intent(inout),optional:: grid_corner_lat(:,:)
    real(ESMF_KIND_R8), intent(inout),optional:: grid_area(:)
    logical, intent(in), optional:: convertToDeg
    integer, intent(out), optional:: rc

    integer:: ncStatus
    integer:: ncid, VarId
    integer:: len
    character(len=128) :: units
    character(len=256) :: errmsg
    real(ESMF_KIND_R8) :: rad2deg
    logical :: convertToDegLocal

#ifdef ESMF_NETCDF
    convertToDegLocal = .false.
    if (present(convertToDeg)) convertToDegLocal = convertToDeg

    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(filename), &
      rc)) return

    ! Read in grid_center_lon and grid_center_lat
    if (present(grid_center_lon)) then
      ncStatus = nf90_inq_varid (ncid, "grid_center_lon", VarId)
      errmsg = "variable grid_center_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_center_lon)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for grid_center_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
	  errmsg,&
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! if units is not "degrees" or "radians" return errors
      call ESMF_StringLowerCase(units(1:len))
      if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg="- units attribute is not degrees or radians", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return
      endif
      ! if units is "radians", convert it to degrees
      if (convertToDegLocal) then
         if (units(1:7) .eq. "radians") then
            rad2deg = 180.0/3.141592653589793238
  !         print *, 'Convert radians to degree ', rad2deg
            grid_center_lon(:) = grid_center_lon(:)*rad2deg
         endif
      endif	   
    endif
    if (present(grid_center_lat)) then
      ncStatus = nf90_inq_varid (ncid, "grid_center_lat", VarId)
      errmsg = "variable grid_center_lat in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_center_lat)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for grid_center_lat in "//trim(filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
	  errmsg,&
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ! if units is not "degrees" or "radians" return errors
      call ESMF_StringLowerCase(units(1:len))
      if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg="- units attribute is not degrees or radians", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return
      endif
      ! if units is "radians", convert it to degree
      if (convertToDegLocal) then
         if (units(1:7) .eq. "radians") then
            rad2deg = 180.0/3.141592653589793238
            grid_center_lat(:) = grid_center_lat(:)*rad2deg
         endif
      endif
    endif

    if (present(grid_imask)) then
      ncStatus = nf90_inq_varid (ncid, "grid_imask", VarId)
      errmsg = "variable grid_imask in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_imask)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
    endif

    ! Read in grid_corner_lon and grid_corner_lat
    if (present(grid_corner_lon)) then
      ncStatus = nf90_inq_varid (ncid, "grid_corner_lon", VarId)
      errmsg = "variable grid_corner_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_corner_lon)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "attribute units for grid_center_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
	  errmsg,&
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ! if units is not "degrees" or "radians" return errors
      call ESMF_StringLowerCase(units(1:len))
      if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg="- units attribute is not degrees or radians", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return
      endif
      ! if units is "radians", convert it to degree
      if (convertToDegLocal) then
         if (units(1:7) .eq. "radians") then
            rad2deg = 180.0/3.141592653589793238
!            print *, 'Convert radians to degree ', rad2deg
            grid_corner_lon(:,:) = grid_corner_lon(:,:)*rad2deg
         endif
      endif	   
    endif
    if (present(grid_corner_lat)) then
      ncStatus = nf90_inq_varid (ncid, "grid_corner_lat", VarId)
      errmsg = "variable grid_corner_lat in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_corner_lat)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for grid_center_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
	  errmsg,&
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! if units is not "degrees" or "radians" return errors
      call ESMF_StringLowerCase(units(1:len))
      if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg="- units attribute is not degrees or radians", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return
      endif
      ! if units is "radians", convert it to degree
      if (convertToDegLocal) then
         if (units(1:7) .eq. "radians") then
            rad2deg = 180.0/3.141592653589793238
            grid_corner_lat(:,:) = grid_corner_lat(:,:)*rad2deg
         endif
      endif
    endif

    ! Read in grid_area and grid_corner_lat
    if (present(grid_area)) then
      ncStatus = nf90_inq_varid (ncid, "grid_area", VarId)
      errmsg = "variable grid_area in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_area)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
	errmsg,&
        rc)) return
      ! get the attribute 'units'
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
    endif

    ncStatus = nf90_close(ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(filename),&
      rc)) return

    if(present(rc)) rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif
    
end subroutine ESMF_ScripGetVar

#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_OutputScripWeightFile"
!BOPI
! !ROUTINE: ESMF_OUtputScripWeightFile
! output the weight and indices tables together with the source/dest vertex 
! coordinates/masks to the SCRIP format NetCDF file
!

! !INTERFACE:
subroutine ESMF_OutputScripWeightFile (wgtFile, factorList, factorIndexList, &
				      srcFile, dstFile, srcIsScrip, dstIsScrip,&
				      title, method, srcArea, dstArea, &
				      srcFrac, dstFrac, rc)
!
! !ARGUMENTS:
      character(len=*), intent(in) :: wgtFile
      real(ESMF_KIND_R8) , intent(in) :: factorList(:)   
      integer(ESMF_KIND_I4) , intent(in), target :: factorIndexList(:,:) 
      character(len=*), optional, intent(in) :: srcFile
      character(len=*), optional, intent(in) :: dstFile
      logical, optional, intent(in) :: srcIsScrip
      logical, optional, intent(in) :: dstIsScrip
      character(len=*), optional, intent(in) :: title
      character(len=*), optional, intent(in) :: method
      real(ESMF_KIND_R8),optional, intent(in) :: srcArea(:),dstArea(:)
      real(ESMF_KIND_R8),optional, intent(in) :: srcFrac(:), dstFrac(:)
      integer, optional :: rc

      integer :: total, localCount(1)
      integer :: ncid, ncid1
      integer :: ncStatus
      integer :: status
      integer :: i,j, start
      integer :: srcDim, dstDim
      integer:: naDimId, nbDimId, nsDimId, srankDimId, drankDimId, varId
      integer :: nvaDimId, nvbDimId
      integer :: src_grid_rank, dst_grid_rank, src_grid_corner, dst_grid_corner
      integer, pointer :: src_grid_dims(:), dst_grid_dims(:)
      real(ESMF_KIND_R8), pointer   :: coords(:),area(:),frac(:)
      real(ESMF_KIND_R8), pointer   :: latBuffer(:),lonBuffer(:)
      real(ESMF_KIND_R8), pointer   :: latBuffer2(:,:),lonBuffer2(:,:)
      real(ESMF_KIND_R8), pointer   :: weightbuf(:)
      integer(ESMF_KIND_I4), pointer:: indexbuf(:), next(:)
      integer(ESMF_KIND_I4), pointer:: mask(:) 
      integer(ESMF_KIND_I4), pointer:: allCounts(:) 
      character(len=256) :: titlelocal, norm, map_method, conventions
      character(len=80) :: srcunits, dstunits
      integer :: maxcount
      logical :: srcIsScriplocal, dstIsScriplocal
      integer :: srcNodeDim, dstNodeDim, srcCoordDim, dstCoordDim
      integer, parameter :: nf90_noerror = 0
      character(len=256) :: errmsg

#ifdef ESMF_NETCDF
      ! write out the indices and weights table sequentially to the output file
      ! first find out the starting index of my portion of table
      ! Global reduce

      if (present(srcIsScrip)) then
	srcIsScriplocal = srcIsScrip
      else
        srcIsScriplocal = .true.
      endif

      if (present(dstIsScrip)) then
	dstIsScriplocal = dstIsScrip
      else
        dstIsScriplocal = .true.
      endif

      call ESMF_VMGetGlobal(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      ! set up local pet info
      call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      localCount(1)=size(factorList,1)
      allocate(allCounts(PetCnt))
      call ESMF_VMAllGather(vm,localCount,allCounts,1,rc=status)
      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

      ! calculate the size of the global weight table
      total = 0
      do i=1,PetCnt
	 total=allCounts(i)+total
      end do
      !print *, PetNo, 'local count ', localCount(1), AllCounts(PetNo+1), total

     !Read the variables from the input grid files at PET0
      if (PetNo == 0) then
        ! Check if srcFile and dstFile exists
        if (.not. present(srcFile)) then
             call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
		  msg="- The srcFile argument does not exist on PET0 ", &
                  ESMF_CONTEXT, rcToReturn=rc)
	     return
        endif
        if (.not. present(dstFile)) then
             call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
		  msg="- The dstFile argument does not exist on PET0 ",  &
                  ESMF_CONTEXT, rcToReturn=rc)
	     return
        endif
        ! Create output file and create dimensions and variables
         ncStatus = nf90_create(trim(wgtFile), NF90_CLOBBER, ncid)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   trim(wgtFile),&
           rc)) return
         
         ! global variables
         if (present(title)) then
	   titlelocal = trim(title)
         else
           titlelocal = "ESMF Offline Regridding Weight Generator"
         endif
         norm = "destarea"
         if (present(method)) then
	   map_method = trim(method)
         else
           map_method = "Bilinear remapping without Conservative Correction"
         endif
         conventions = "NCAR-CSM"

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "title", trim(titlelocal))
         errmsg = "Attribute title in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "normalization", trim(norm))
         errmsg = "Attribute normalization in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "map_method", trim(map_method))
         errmsg = "Attribute map_method in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "conventions", trim(conventions))
         errmsg = "Attribute conventions in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

	 ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "domain_a", trim(srcFile))
         errmsg = "Attribute domain_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "domain_b", trim(dstFile))
         errmsg = "Attribute domain_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "grid_file_src", trim(srcFile))
         errmsg = "Attribute grid_file_src in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "grid_file_dst", trim(dstFile))
         errmsg = "Attribute grid_file_dst in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "CVS_revision", ESMF_VERSION_STRING)
         errmsg = "Attribute CVS_revision in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
 
        ! Get Source Grid dimension and variables
	if (srcIsScriplocal) then 
          allocate(src_grid_dims(2))
          call ESMF_ScripInq(srcFile, grid_rank=src_grid_rank, grid_size=srcDim, &
	      grid_dims=src_grid_dims, grid_corners=src_grid_corner, rc=status)
          call ESMF_ScripInqUnits(srcFile,units = srcunits, rc=status)
        else
          call ESMF_EsmfInq(srcFile, elementCount=srcDim, maxNodePElement=src_grid_corner, &
	      coordDim = srcCoordDim, nodeCount=srcNodeDim, rc=status)
          call ESMF_EsmfInqUnits(srcFile,units = srcunits, rc=status)
          allocate(src_grid_dims(1))
          src_grid_dims(1)=1
          src_grid_rank = 1    
        endif 
        if (dstIsScriplocal) then
          allocate(dst_grid_dims(2))
          call ESMF_ScripInq(dstFile, grid_rank=dst_grid_rank, grid_size=dstDim, &
	     grid_dims=dst_grid_dims, grid_corners=dst_grid_corner, rc=status)
          call ESMF_ScripInqUnits(dstFile,units = dstunits, rc=status)
        else
          call ESMF_EsmfInq(dstFile, elementCount=dstDim, maxNodePElement=dst_grid_corner, &
	      coordDim = dstCoordDim, nodeCount=dstNodeDim, rc=status)    
          call ESMF_EsmfInqUnits(dstFile,units = dstunits, rc=status)
          allocate(dst_grid_dims(1))
          dst_grid_dims(1)=1   
          dst_grid_rank = 1
        endif
        ! define dimensions
         ncStatus = nf90_def_dim(ncid,"n_a",srcDim, naDimId)
         errmsg = "Dimension n_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_def_dim(ncid,"n_b",dstDim, nbDimId)
         errmsg = "Dimension n_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_def_dim(ncid,"n_s",total, nsDimId)
         errmsg = "Dimension n_s in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! define max number of vertices
         ncStatus = nf90_def_dim(ncid,"nv_a",src_grid_corner, nvaDimId)
         errmsg = "Dimension nv_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_def_dim(ncid,"nv_b",dst_grid_corner, nvbDimId)
         errmsg = "Dimension nv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! define max number of vertices
         ncStatus = nf90_def_dim(ncid,"num_wgts",1, VarId)
         errmsg = "Dimension num_wgts in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ! define grid ranks
         ncStatus = nf90_def_dim(ncid,"src_grid_rank",src_grid_rank, srankDimId)
         errmsg = "Dimension src_grid_rank in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_def_dim(ncid,"dst_grid_rank",dst_grid_rank, drankDimId)
         errmsg = "Dimension dst_grid_rank in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! define variables
         ! Grid Dims
         ncStatus = nf90_def_var(ncid,"src_grid_dims",NF90_INT, (/srankDimId/),  VarId)
         errmsg = "Variable src_grid_dims in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus = nf90_def_var(ncid,"dst_grid_dims",NF90_INT, (/drankDimId/),  VarId)
         errmsg = "Variable dst_grid_dims in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! yc_a: source vertex coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yc_a",NF90_DOUBLE, (/naDimId/),  VarId)
         errmsg = "Variable yc_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(srcunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! yc_b: destination vertex coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yc_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         errmsg = "Variable yc_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(dstunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! xc_a: source vertex coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xc_a",NF90_DOUBLE, (/naDimId/),  VarId)
         errmsg = "Variable xc_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(srcunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! xc_b: dest. vertex coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xc_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         errmsg = "Variable xc_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(dstunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! yv_a: source corner coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yv_a",NF90_DOUBLE, (/nvaDimId,naDimId/),  VarId)
         errmsg = "Variable yv_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(srcunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! xv_a: source corner coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xv_a",NF90_DOUBLE, (/nvaDimId,naDimId/),  VarId)
         errmsg = "Variable xv_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(srcunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! yv_b: source corner coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yv_b",NF90_DOUBLE, (/nvbDimId,nbDimId/),  VarId)
         errmsg = "Variable yv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(dstunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! xv_b: source corner coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xv_b",NF90_DOUBLE, (/nvbDimId,nbDimId/),  VarId)
         errmsg = "Variable xv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(dstunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! mask_a
         ncStatus = nf90_def_var(ncid,"mask_a",NF90_INT, (/naDimId/),  VarId)
         errmsg = "Variable mask_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! mask_b
         ncStatus = nf90_def_var(ncid,"mask_b",NF90_INT, (/nbDimId/),  VarId)
         errmsg = "Variable mask_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! area_a
         ncStatus = nf90_def_var(ncid,"area_a",NF90_DOUBLE, (/naDimId/),  VarId)
         errmsg = "Variable area_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "square radians")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! area_b
         ncStatus = nf90_def_var(ncid,"area_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         errmsg = "Variable area_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "square radians")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! frac_a
         ncStatus = nf90_def_var(ncid,"frac_a",NF90_DOUBLE, (/naDimId/),  VarId)
         errmsg = "Variable frac_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! frac_b
         ncStatus = nf90_def_var(ncid,"frac_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         errmsg = "Variable frac_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return


        ! col: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"col",NF90_INT, (/nsDimId/),  VarId)
         errmsg = "Variable col in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! row: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"row",NF90_INT, (/nsDimId/),  VarId)
         errmsg = "Variable row in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

        ! S: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"S",NF90_DOUBLE, (/nsDimId/),  VarId)
         errmsg = "Variable S in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   errmsg,&
           rc)) return

         ncStatus=nf90_enddef(ncid) 
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
	   trim(wgtFile),&
           rc)) return
       
        ! write src_grid_dims and dst_grid_dims
	ncStatus=nf90_inq_varid(ncid,"src_grid_dims",VarId)
        ncStatus=nf90_put_var(ncid,VarId, src_grid_dims )          
        errmsg = "Variable src_grid_dims in "//trim(wgtfile)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
	  errmsg,&
          rc)) return

	ncStatus=nf90_inq_varid(ncid,"dst_grid_dims",VarId)
        ncStatus=nf90_put_var(ncid,VarId, dst_grid_dims)          
        errmsg = "Variable dst_grid_dims in "//trim(wgtfile)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
	  errmsg,&
          rc)) return

        if (SrcIsScriplocal) then
           ! Read the srcGrid variables and write them out
           allocate(latBuffer(srcDim), lonBuffer(srcDim))
           call ESMF_ScripGetVar(srcFile, grid_center_lon=lonBuffer, &
	 	grid_center_lat=latBuffer, rc=status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return

	   ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer)          
           errmsg = "Variable xc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
  	     errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer)          
           errmsg = "Variable yc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
  	     errmsg,&
             rc)) return
           deallocate(latBuffer, lonBuffer)
          ! Write xv_a, yv_a	
           allocate(latBuffer2(src_grid_corner,srcDim),lonBuffer2(src_grid_corner,srcDim))         
           call ESMF_ScripGetVar(srcFile, grid_corner_lon=lonBuffer2, &
	 	grid_corner_lat=latBuffer2, rc=status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return

	   ncStatus=nf90_inq_varid(ncid,"xv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)          
           errmsg = "Variable xv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
  	     errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)          
           errmsg = "Variable yv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
  	     errmsg,&
             rc)) return
           deallocate(latBuffer2, lonBuffer2) 

           allocate(mask(srcDim))         
           call ESMF_ScripGetVar(srcFile, grid_imask=mask, rc=status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return
           ncStatus=nf90_inq_varid(ncid,"mask_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)          
           errmsg = "Variable mask_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
  	     errmsg,&
             rc)) return
           deallocate(mask)
        else 
           ncStatus=nf90_open(srcFile,NF90_NOWRITE,ncid1)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
	     trim(srcFile),&
             rc)) return
	   ! check if centerCoords exit
           ncStatus=nf90_inq_varid(ncid1,"centerCoords",VarId)
	   if (ncStatus /= nf90_noerror) then
	     print *, "centerCoords does not exit"
           else 
	     allocate(latBuffer2(srcCoordDim, srcDim))
             allocate(latBuffer(srcDim), lonBuffer(srcDim))
             ncStatus=nf90_get_var(ncid1,VarId, latBuffer2)
             errmsg = "Variable centerCoors in "//trim(srcFile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
	       errmsg,&
               rc)) return
	     do i=1,srcDim
		lonBuffer(i)=latBuffer2(1,i)
                latBuffer(i)=latBuffer2(2,i)
             enddo
   	     ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
             ncStatus=nf90_put_var(ncid,VarId, lonBuffer)          
             errmsg = "Variable xc_a in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
	       errmsg,&
               rc)) return

             ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
             ncStatus=nf90_put_var(ncid,VarId, latBuffer)          
             errmsg = "Variable yc_a in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
    	       errmsg,&
               rc)) return
             deallocate(latBuffer, lonBuffer, latBuffer2)
           endif
           
           ! output xv_a and yv_a is harder, we have to read in the nodeCoords and
           ! elementConn and construct the the latitudes nd longitudes for
           ! all the corner vertices
           allocate(latBuffer2(src_grid_corner,srcDim),lonBuffer2(src_grid_corner,srcDim))         
           call ESMF_EsmfGetVerts(ncid1, srcFile, srcDim, src_grid_corner, srcNodeDim, &
		latBuffer2, lonBuffer2,status) 
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return

	   ncStatus=nf90_inq_varid(ncid,"xv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)          
           errmsg = "Variable xv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE, errmsg, &
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)          
           errmsg = "Variable yv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return
           deallocate(latBuffer2, lonBuffer2) 
           allocate(mask(srcDim))         
           ncStatus=nf90_inq_varid(ncid1,"elementMask",VarId)
	   if (ncStatus /= nf90_noerror) then
	     print *, "elementMask does not exit"
             mask = 1
           else 
             ncStatus=nf90_get_var(ncid1,VarId, mask)
             errmsg = "Variable elementMask in "//trim(srcFile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
           end if
           ncStatus=nf90_inq_varid(ncid,"mask_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)          
           errmsg = "Variable mask_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return
           deallocate(mask)
           ncStatus=nf90_close(ncid1)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,trim(srcFile),&
             rc)) return
        endif 

        ! Read the dstGrid variables and write them out
        if (DstIsScriplocal) then
         allocate(latBuffer(dstDim), lonBuffer(dstDim))
         call ESMF_ScripGetVar(dstFile, grid_center_lon=lonBuffer, &
	 	grid_center_lat=latBuffer, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return
	 ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, lonBuffer)          
         errmsg = "Variable xc_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return

         ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, latBuffer)          
         errmsg = "Variable yc_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
         deallocate(latBuffer, lonBuffer)

         allocate(latBuffer2(dst_grid_corner,dstDim),lonBuffer2(dst_grid_corner,dstDim))         
         call ESMF_ScripGetVar(dstFile, grid_corner_lon=lonBuffer2, &
	 	grid_corner_lat=latBuffer2, rc=status)
	 if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return

	 ncStatus=nf90_inq_varid(ncid,"xv_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)          
         errmsg = "Variable xv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return

         ncStatus=nf90_inq_varid(ncid,"yv_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, latBuffer2)          
         errmsg = "Variable yv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
         deallocate(latBuffer2, lonBuffer2)
         allocate(mask(dstDim))         
         call ESMF_ScripGetVar(dstFile, grid_imask=mask, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return
         ncStatus=nf90_inq_varid(ncid,"mask_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, mask)          
         errmsg = "Variable mask_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
         deallocate(mask)
        else
           ncStatus=nf90_open(dstFile,NF90_NOWRITE,ncid1)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
         rc)) return
	   ! check if centerCoords exit
           ncStatus=nf90_inq_varid(ncid1,"centerCoords",VarId)
	   if (ncStatus /= nf90_noerror) then
	     print *, "centerCoords does not exit"
           else 
	     allocate(latBuffer2(dstCoordDim, dstDim))
             allocate(latBuffer(dstDim), lonBuffer(dstDim))
             ncStatus=nf90_get_var(ncid1,VarId, latBuffer2)
             errmsg = "Variable centerCoords in "//trim(dstFile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
	     do i=1,dstDim
		lonBuffer(i)=latBuffer2(1,i)
                latBuffer(i)=latBuffer2(2,i)
             enddo
   	     ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
             ncStatus=nf90_put_var(ncid,VarId, lonBuffer)          
             errmsg = "Variable xc_b in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return

             ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
             ncStatus=nf90_put_var(ncid,VarId, latBuffer)          
             errmsg = "Variable yc_b in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
             deallocate(latBuffer, lonBuffer, latBuffer2)
           endif
           
           ! output xv_b and yv_b is harder, we have to read in the nodeCoords and
           ! elementConn and construct the the latitudes nd longitudes for
           ! all the corner vertices
           allocate(latBuffer2(dst_grid_corner,dstDim),lonBuffer2(dst_grid_corner,dstDim))         
           call ESMF_EsmfGetVerts(ncid1, dstFile, dstDim, dst_grid_corner, dstNodeDim, &
		latBuffer2, lonBuffer2, status) 
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return

	   ncStatus=nf90_inq_varid(ncid,"xv_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)          
           errmsg = "Variable xv_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)          
           errmsg = "Variable yv_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return
           deallocate(latBuffer2, lonBuffer2) 
           ! Write mask_b
           allocate(mask(dstDim))         
           ncStatus=nf90_inq_varid(ncid1,"elementMask",VarId)
	   if (ncStatus /= nf90_noerror) then
	     print *, "elementMask does not exit"
             mask = 1
           else 
             ncStatus=nf90_get_var(ncid1,VarId, mask)
             errmsg = "Variable elementMask in "//trim(dstFile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
           end if
           ncStatus=nf90_inq_varid(ncid,"mask_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)          
           errmsg = "Variable mask_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return
           deallocate(mask)
           ncStatus=nf90_close(ncid1)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,trim(dstFile),&
             rc)) return
        endif


         ! Write area_a
         ncStatus=nf90_inq_varid(ncid,"area_a",VarId)
         if (present(srcArea)) then
           ncStatus=nf90_put_var(ncid,VarId, srcArea)          
           errmsg = "Variable area_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return 
         else
          ! Just set these to 0.0, because not provided
           allocate(area(srcDim))         
           area=0.0
           ncStatus=nf90_put_var(ncid,VarId, area)          
           errmsg = "Variable area_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
           deallocate(area)
         endif

         ! Write area_b
         ncStatus=nf90_inq_varid(ncid,"area_b",VarId)
         if (present(dstArea)) then
           ncStatus=nf90_put_var(ncid,VarId, dstArea)          
           errmsg = "Variable area_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return 
         else
          ! Just set these to 0.0, because not provided
           allocate(area(dstDim))         
           area=0.0
           ncStatus=nf90_put_var(ncid,VarId, area)          
           errmsg = "Variable area_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
           deallocate(area)
         endif

         ! Write frac_a
         ncStatus=nf90_inq_varid(ncid,"frac_a",VarId)
	 if (present(srcFrac)) then
            ncStatus=nf90_put_var(ncid,VarId, srcFrac)          
	 else
            allocate(frac(srcDim))         
            frac=0.0
            ncStatus=nf90_put_var(ncid,VarId, frac)          
            deallocate(frac)
	 endif
         errmsg = "Variable frac_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return

         ! Write frac_b
         ncStatus=nf90_inq_varid(ncid,"frac_b",VarId)
	 if (present(dstFrac)) then
            ncStatus=nf90_put_var(ncid,VarId, dstFrac)          
	 else
            allocate(frac(dstDim))         
            frac=1.0
            ncStatus=nf90_put_var(ncid,VarId, frac)          
            deallocate(frac)
         endif
         errmsg = "Variable frac_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return

    end if

    ! Block all other PETs until the NetCDF file has been created
    call ESMF_VMBarrier(vm)

    ! find the max of allCounts(i) and allocate colrow
    maxcount=0
    do i=1,PetCnt
	if (allCounts(i) > maxcount) maxcount = allCounts(i)
    enddo

    if (PetNo == 0) then 
	! First write out its own weight and indices, then receive the data from 
	! other PETs and write them out
	start = 1
        ! allocate indexbuf and weightbuf to receive data from other PETs
        allocate(indexbuf(maxcount*2), weightbuf(maxcount))
        do i=1, PetCnt
          ! write the local factorList and factorIndexList first
          localCount(1)=allCounts(i)
 	  if (i==1) then 
  	    !do j=1,localCount(1)
            !    indexbuf(j) = factorIndexList(j,1)
            !enddo
            next => factorIndexList(:,1)
            ncStatus=nf90_inq_varid(ncid,"col",VarId)
   	    ncStatus=nf90_put_var(ncid,VarId, next,(/start/),localCount)          
            errmsg = "Variable col in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return
            !do j=1,localCount(1)
            !  indexbuf(j) = factorIndexList(j,2)
            !enddo
            next => factorIndexList(:,2)
            ncStatus=nf90_inq_varid(ncid,"row",VarId)
   	    ncStatus=nf90_put_var(ncid,VarId, next,(/start/),localCount)          
            errmsg = "Variable row in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return

            ncStatus=nf90_inq_varid(ncid,"S",VarId)
            ncStatus=nf90_put_var(ncid,VarId, factorList, (/start/),localCount)
            errmsg = "Variable S in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return
	  else 
            if (localCount(1) > 0) then
              ! receive the factorList and factorIndexList 
              call ESMF_VMRecv(vm, indexbuf, localCount(1)*2, i-1, rc=status)
	      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	  ESMF_CONTEXT, rcToReturn=rc)) return
              call ESMF_VMRecv(vm, weightbuf, localCount(1), i-1, rc=status)
	      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	  ESMF_CONTEXT, rcToReturn=rc)) return

              ncStatus=nf90_inq_varid(ncid,"col",VarId)
       	      ncStatus=nf90_put_var(ncid,VarId, indexbuf,(/start/),localCount)          
              errmsg = "Variable col in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
              next => indexbuf(localCount(1)+1:localCount(1)*2)
              ncStatus=nf90_inq_varid(ncid,"row",VarId)
     	      ncStatus=nf90_put_var(ncid,VarId, next ,(/start/),localCount)          
              errmsg = "Variable row in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return

              ncStatus=nf90_inq_varid(ncid,"S",VarId)
              ncStatus=nf90_put_var(ncid,VarId, weightbuf, (/start/),localCount)
              errmsg = "Variable S in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
            end if
          end if
          start = start + localCount(1)
       end do
    else
       allocate(indexbuf(localcount(1)*2))
       if (localcount(1) > 0) then
         do j=1,localCount(1)
             indexbuf(j) = factorIndexList(j,1)
             indexbuf(j+localCount(1)) = factorIndexList(j,2)
         enddo
         ! a non-root PET, send the results to PET 0
         call ESMF_VMSend(vm, indexbuf, localCount(1)*2, 0, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return
         call ESMF_VMSend(vm, factorList, localCount(1), 0, rc=status)
	 if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT, rcToReturn=rc)) return
       end if
    end if
       
    call ESMF_VMBarrier(vm)
    
    if (PetNo == 0) then
       ncStatus = nf90_close(ncid)                        
       if (CDFCheckError (ncStatus, &
         ESMF_METHOD, &
         ESMF_SRCLINE, trim(wgtfile),&
         rc)) return
       deallocate(weightbuf)
    end if
    deallocate(indexbuf)
    if (present(rc)) rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
#endif

    return
end subroutine ESMF_OutputScripWeightFile

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_EsmfInq"
!BOPI
! !ROUTINE: ESMF_EsmfInq: Return the dimension information
!  information from a ESMF Unstructured grid file
!
! !INTERFACE:
subroutine ESMF_EsmfInq(filename, nodeCount, elementCount, &
	      		maxNodePElement, coordDim, rc)    

! !ARGUMENTS:

    character(len=*), intent(in)   :: filename
    integer, intent(out), optional :: nodeCount
    integer, intent(out), optional :: elementCount
    integer, intent(out), optional :: maxNodePElement
    integer, intent(out), optional :: coordDim
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: DimId, VarId
    integer :: ncid, local_rank
    character(len=256):: errmsg

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, trim(filename), rc)) return

    ! get number of elements
    if (present(nodeCount)) then
      ncStatus = nf90_inq_dimid (ncid, "nodeCount", DimId)
      errmsg = "Dimension nodeCount in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=nodeCount)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return
    end if

    ! get number of elements
    if (present(elementCount)) then
      ncStatus = nf90_inq_dimid (ncid, "elementCount", DimId) 
      errmsg = "Dimension elementCount in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=elementCount)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return
    end if

    ! get number of elements
    if (present(maxNodePElement)) then
      ncStatus = nf90_inq_dimid (ncid, "maxNodePElement", DimId)
      errmsg = "Dimension maxNodePElement in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=maxNodePElement)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return
    end if

    ! get number of elements
    if (present(coordDim)) then
      ncStatus = nf90_inq_dimid (ncid, "coordDim", DimId)
      errmsg = "Dimension coordDim in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=coordDim)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return
    end if
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
#endif

    return
end subroutine ESMF_EsmfInq

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_EsmfInqUnits"
!BOPI
! !ROUTINE: ESMF_EsmfInqUnits: Return the units attribute for
! coordinate variables, assuming all the coordinate variables
! have the same units
!
! !INTERFACE:
subroutine ESMF_EsmfInqUnits(filename, units, rc)

! !ARGUMENTS:

    character(len=*), intent(in)   :: filename
    character(len=*), intent(out)   :: units
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: ncid, VarId, len
    character(len=80) :: buffer
    character(len=256) :: errmsg

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, trim(filename),rc)) return

    ncStatus = nf90_inq_varid (ncid, "nodeCoords", VarId)
    errmsg = "Variable nodeCoords in "//trim(filename)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

    ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

    ncStatus = nf90_get_att(ncid, VarId, "units", buffer)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return
    ! if len != 7, something is wrong, check the value.  If it starts 
    ! with Degrees/degrees/Radians/radians, ignore the garbage after the
    ! word.  Otherwise, return the whole thing
    call ESMF_StringLowerCase(buffer(1:len))
    if ((len > 7) .and. (buffer(1:7) .eq. 'degrees' .or.  &
      buffer(1:7) .eq. 'radians')) then
      units = buffer(1:7)
    else
      units = buffer(1:len)
    endif
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
#endif

    return
end subroutine ESMF_EsmfInqUnits

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetMeshFromFile"
subroutine ESMF_GetMeshFromFile (filename, nodeCoords, elementConn, &
				    elmtNums, startElmt, rc)

    character(len=*), intent(in)   :: filename
    real(ESMF_KIND_R8), pointer    :: nodeCoords (:,:)
    integer(ESMF_KIND_I4), pointer :: elementConn (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
    integer,           intent(out) :: startElmt
    integer,           intent(out) :: rc

    integer :: ncid
    integer :: ncStatus
    integer :: RecCnt (2)

    integer :: DimId
    integer :: nodeCnt, ElmtCount, MaxNodePerElmt, NodeDim
    integer :: localCount, remain

    integer :: VarNo
    character(len=256)::errmsg

#ifdef ESMF_NETCDF

    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    ! get number of vertices
    ncStatus = nf90_inq_dimid (ncid, "nodeCount", DimId)
    errmsg = "Dimension nodeCount in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=nodeCnt)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get vertex dimension
    ncStatus = nf90_inq_dimid (ncid, "coordDim", DimId)
    errmsg = "Dimension coordDim in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=NodeDim)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! allocate memory for verticies
    allocate (nodeCoords (NodeDim, nodeCnt))

    RecCnt(:) = ubound(nodeCoords)
    !print *, "nodeCoords:",nodeCnt, NodeDim

    ! read vertex data
    ncStatus = nf90_inq_varid (ncid, "nodeCoords", VarNo)
    errmsg = "Variable nodeCoords in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_get_var (ncid, VarNo, nodeCoords, start=(/1,1/), count=(/NodeDim, nodeCnt/))
    errmsg = "Variable nodeCoords in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! get number of elmts
    ncStatus = nf90_inq_dimid (ncid, "elementCount", DimId)
    errmsg = "Dimension elementCount in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=ElmtCount)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get max_verts_per_elmt
    ncStatus = nf90_inq_dimid (ncid, "maxNodePElement", DimId)
    errmsg = "Dimension maxNodePElement in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=MaxNodePerElmt)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Decompose the elmt array evenly on all the PEs
    localcount = elmtCount/PetCnt;
    remain = mod(elmtCount,PetCnt);
    startElmt = localcount * PetNo+1
    if (PetNo == (PetCnt-1)) localcount = localcount+remain

    ! allocate memory for elmts
    allocate (elementConn (MaxNodePerElmt, localcount))
    allocate (elmtNums (localcount))

    ! read elmt_verts data
    ncStatus = nf90_inq_varid (ncid, "elementConn", VarNo)
    errmsg = "Variable elementConn in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_get_var (ncid, VarNo, elementConn, start=(/1,startElmt/), count=(/MaxNodePerElmt, localcount/))
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    
    ! read num_elmt_verts
    ncStatus = nf90_inq_varid (ncid, "numElementConn", VarNo)
    errmsg = "Variable numElementConn in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_get_var (ncid, VarNo, elmtNums, start=(/startElmt/), count=(/localcount/))
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_close (ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GetMeshFromFile


!
!  Get the NodeCoords and ElementConn from the ESMF unstructured file and construct
!   corner vertices table similar to what stored in the SCRIP file
!
#undef  ESMF_METHOD
#define ESMF_METHOD "EsmfGetVerts"
subroutine ESMF_EsmfGetVerts(ncid, filename, numElements, numNodePElement, numNodes, &
		latBuffer, lonBuffer, rc) 

    integer, intent(in)  :: ncid
    character(len=*), intent(in) :: filename
    integer, intent(in)  :: numElements
    integer, intent(in)  :: numNodePElement
    integer, intent(in)  :: numNodes
    real(ESMF_KIND_R8), intent(inout)   :: latBuffer(:,:)
    real(ESMF_KIND_R8), intent(inout)   :: lonBuffer(:,:)
    integer, intent(out), optional      :: rc

    integer :: ncStatus
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:,:)
    integer(ESMF_KIND_I4), allocatable :: elementConn(:,:)
    integer :: varId, fillValue
    integer, parameter :: fillValue1 = -9999
    integer :: i,j, index
    character(len=256)::errmsg

#ifdef ESMF_NETCDF
    allocate(nodeCoords(2, numNodes))
    allocate(elementConn(numNodePElement, numElements))

    ! Get NodeCoords    
    ncStatus = nf90_inq_varid(ncid, "nodeCoords", varId)
    errmsg = "Variable nodeCoords in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ncStatus = nf90_get_var(ncid, varId, nodeCoords)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get elementConn table
    ncStatus = nf90_inq_varid(ncid, "elementConn", varId)
    errmsg = "Variable elementConn in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ncStatus = nf90_get_var(ncid, varId, elementConn)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Get fill value
    ncStatus = nf90_get_att(ncid, varId, "_FillValue", fillValue)
    errmsg = "Attribute _FillValue for elementConn in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Fill latBuffer and lonBuffer
    do i=1, numElements
      do j=1, numNodePElement
         if (elementConn(j,i) /= fillValue) then
            index = elementConn(j,i)
            latBuffer(j,i) = nodeCoords(2,index)
            lonBuffer(j,i) = nodeCoords(1,index)
         else
            latBuffer(j,i) = fillValue1
            lonBuffer(j,i) = fillValue1
         endif
      enddo
    enddo
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif
end subroutine ESMF_EsmfGetVerts
!-----------------------------------------------------------------------

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
        call ESMF_LogWrite (msg="netCDF Status Return Error", msgtype=ESMF_LOG_ERROR, &
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


end module ESMF_IOScripMod
