! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_IOGridmosaic.F90"
!
!     ESMF IOGridMosaic Module
      module ESMF_IOGridmosaicMod
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
! !MODULE: ESMF_IOGridMosaicMod - Module to support GridSpec Mosiaic and Tile files
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
      use ESMF_UtilStringMod
      use ESMF_VMMod

#ifdef ESMF_NETCDF
      use netcdf
#endif

!     NEED TO ADD MORE HERE
      implicit none

    private
  integer, SAVE :: PetNo, PetCnt
  type(ESMF_VM), SAVE:: vm

 !------------------------------------------------------------------------------
! !PUBLIC TYPES:

  type ESMF_Mosaic
    character(len=ESMF_MAXSTR)  :: name
    integer                                  :: ntiles      ! number of tiles
    integer                                  :: tilesize    ! the size of the tile
    integer                                  :: ncontacts   ! number of contacts
    character(len=ESMF_MAXPATHLEN)           :: tileDirectory  ! the path of the tile files
    character(len=ESMF_MAXPATHLEN),  pointer :: filenames(:)  ! the tile filename array
    integer, pointer                         :: contact(:,:)   ! pair of tiles in each contact
    integer, pointer                         :: connindex(:,:,:)  ! the end points of the contact edges
  end type
    
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_Mosaic
  public ESMF_GridspecReadTile
  public ESMF_GridspecReadMosaic

!==============================================================================

      contains

!==============================================================================
!
! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecReadMosaic"

!BOP
! !INTERFACE:
subroutine ESMF_GridspecReadMosaic(filename, mosaic, rc)

! !ARGUMENTS:
 
    character(len=*), intent(in)     :: filename
    type(ESMF_Mosaic), intent(inout) :: mosaic
    integer, optional, intent(out)   :: rc

! !DESCRIPTIONS:
!   find a variable with the standard_name attribute set to "grid_mosaic_spec"
!   use the "children" attribute to find the variable that defines tile names
!   use the "contact_regions" attribute to find the variable that defines the contacts
!   The value of the variable is the mosaic name used in the contact list
!   
!   find a variable with standard_name set to "grid_file_location".  It is the 
!   path where the tile files are stored.  If not exist, assuming the tile files are
!   in the working directory
!  
!   there should be a variable that defines the filenames of the tile files.  Let's give
!   it a standard_name called "grid_file_names", together with grid_file_location, we can
!   locate the tile files.  It is a 1D array with the same size as the "children" variable.
!
!   the "children" varible is a 1D array containing a list of tile names.  The dimension
!   of the variable defines the number of tiles
!
!   the "contact" variable defines the contacts of the tiles, it should have the following 
!   attributes:
!    standard_name = "grid_contact_spec"
!    contact_type = "boundary"
!    contact_index = variable name that defines how each contact is connected
!   This variable is a 1D array and its size defines the number of contacts in this mosaic
!   Each entry is a character string with the format:
!     mosaic_name:tilex::mosaic_name::tiley
!   Assuming mosaic_name should match with the value defined in the grid_mosaic variable, 
!    and the tilename is one of the tiles defined in the children variable.
!
!   the "contact_index" variable defines the two edges of the neighboring tiles at the boundary
!   It is a 1D array with the same size as the "contact" variable
!   Each entry has the following format:
!    i1,i2:j1,j2::i3,i4,j3,j4
!   where (i1,j1) and (i2, j2) are the two end points of edge in the first tile and 
!   (i3,j3) and (i4,j4) are the two end points of the edge in the second tile.  The index are
!   both local indices within the tile with 1-based index.
!   (i1,j1) is the same point as (i3,j3) and (i2,j2) is the same point as (i4,j4)
!
#ifdef ESMF_NETCDF

    integer   :: ncid, varid
    integer   :: i, j, nvars, attlen
    integer   :: ntiles, ncontacts
    character(len=ESMF_MAXSTR) :: attstr
    character(len=ESMF_MAXSTR) :: mosaicname
    character(len=ESMF_MAXPATHLEN) :: tempname
    integer            :: strlen
    character(len=1), allocatable :: temptilenames(:,:)
    character(len=ESMF_MAXSTR), allocatable :: tilenames(:)
    character(len=1),  allocatable :: tilefilenames(:,:)
    integer, pointer :: contact(:,:)
    integer, pointer :: connindex(:,:,:)
    integer   :: tilesize
    integer   :: dimids(2), ndims, dims(2)
    integer   :: ncStatus, localrc
    integer   :: totallen
    integer   :: k

    !initialize mosaic values
    mosaic%ntiles = 0

    if (present(rc)) rc=ESMF_FAILURE

    ncStatus = nf90_open(path=filename, mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        trim(filename), &
        rc)) return

    ncStatus = nf90_inquire(ncid, nVariables=nvars)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        trim(filename), &
        rc)) return
     do i=1,nvars
       ! Check its standard_name attribute
       ncStatus = nf90_inquire_attribute(ncid, i, 'standard_name', len=attlen)
       if (ncStatus == nf90_noerr) then
          ncStatus = nf90_get_att(ncid, i, 'standard_name', values=attstr)
          if (attstr(1:attlen) .eq. 'grid_mosaic_spec') then
            ! get mosaic name
            ncStatus = nf90_get_var(ncid, i, mosaicname)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               trim(filename), &
               rc)) return
            mosaic%name = trim(mosaicname)
            ! Get attributes children and contact_regions
            ncStatus = nf90_get_att(ncid, i, 'children', values=attstr)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "children attribute does not exit", &
               rc)) return
            ! Get the tilenames from the variable defined in "children" variable
            ncStatus = nf90_inq_varid(ncid, trim(attstr), varid)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "children attribute does not exit", &
               rc)) return
            ! Find the dimension of the tile variable
            ncStatus = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "children attribute does not exit", &
               rc)) return
            if (ndims /= 2) then
                call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- tile variable dimension greater than 1", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
               return
            endif
            ! query it's dimension to find out number of tiles
            ncStatus = nf90_inquire_dimension(ncid, dimids(2), len=ntiles)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "tile dimension inquire", &
               rc)) return
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=strlen)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "tile dimension inquire", &
               rc)) return
             mosaic%ntiles = ntiles
             ! get the tile names
             allocate(tilenames(ntiles), temptilenames(strlen, ntiles))
             ncStatus = nf90_get_var(ncid, varid, temptilenames, start=(/1,1/), count=(/strlen, ntiles/))
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "get tile names", &
               rc)) return
              ! replace null character by blank
             do j=1,ntiles
              tilenames(j) = ESMF_UtilArray2String(temptilenames(:,j))
              call trim_null(tilenames(j))
             enddo
             
            ! Find contact regions
            ncStatus = nf90_get_att(ncid, i, 'contact_regions', values=attstr)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact_regions attribute does not exit", &
               rc)) return
            ! Get the contact info from the variable defined in "contact_regions" variable
            ncStatus = nf90_inq_varid(ncid, trim(attstr), varid)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "children attribute does not exit", &
               rc)) return
            ! Find the dimension of this variable
            ncStatus = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "children attribute does not exit", &
               rc)) return
            if (ndims /= 2) then
                call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- contact region variable dimension greater than 1", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
               return
            endif
            ! query it's dimension to find out the string length
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=dims(1))
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            ! query it's dimension to find out number of contacts
            ncStatus = nf90_inquire_dimension(ncid, dimids(2), len=dims(2))
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            allocate(mosaic%contact(2,dims(2)))
            allocate(mosaic%connindex(2,4,dims(2)))
            call readContacts(ncid, varid, dims, mosaicname, tilenames, &
                 mosaic%contact, mosaic%connindex, localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
	    mosaic%ncontacts=dims(2)
          elseif (attstr(1:attlen) .eq. 'grid_file_location') then
            ! Get the directory name where the tile files are stored
            ncStatus = nf90_inquire_variable(ncid, i, ndims=ndims, dimids=dimids)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "inquire variable grid_file_location", &
               rc)) return
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=dims(1))
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            !allocate(pathname(dims(1)))
            ! initialize the string to null characters
            do k=1,ESMF_MAXPATHLEN
                mosaic%tileDirectory(k:k)=char(0)
            enddo
            ncStatus = nf90_get_var(ncid, i, mosaic%tileDirectory, start=(/1/), count=(/dims(1)/))
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "fail to get grid_file_location", &
               rc)) return
            call trim_null(mosaic%tileDirectory)
          elseif (attstr(1:attlen) .eq. 'grid_file_names') then
            ! Find the dimension of this variable
            ncStatus = nf90_inquire_variable(ncid, i, ndims=ndims, dimids=dimids)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "children attribute does not exit", &
               rc)) return
            if (ndims /= 2) then
                call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- contact region variable dimension greater than 1", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
               return
            endif    
            ! query it's dimension to find out the string length
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=dims(1))
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            allocate(tilefilenames(dims(1), ntiles))
            allocate(mosaic%filenames(ntiles))
            ! initialize the string to null characters
	    do j=1,ntiles
              do k=1,ESMF_MAXPATHLEN
                mosaic%filenames(ntiles)(k:k)=char(0)
              enddo
            enddo
            ncStatus = nf90_get_var(ncid, i, tilefilenames, start=(/1,1/), count=(/dims(1), ntiles/))
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "fail to get grid_file_names", &
               rc)) return
            ! replace null character by blank
            do j=1,ntiles
              mosaic%filenames(j)=ESMF_UtilArray2String(tilefilenames(:,j))
              call trim_null(mosaic%filenames(j))
            enddo
          endif
       endif
     enddo

     ncStatus = nf90_close(ncid)
     if (CDFCheckError (ncStatus, &
           ESMF_METHOD,  &
           ESMF_SRCLINE, &
           "close mosaic file", &
           rc)) return

     ! check if we found the mosaic variables
     if (mosaic%ntiles == 0) then
        call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg="- Failed to parse GridSpec Mosaic file", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
        return    
      else
        ! find the tileSize by reading one of the tilefiles
        tempname = trim(mosaic%TileDirectory)//trim(mosaic%filenames(1))
        call ESMF_GridSpecQueryTileSize(tempname, tilesize, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        mosaic%tilesize = tilesize
        if (present(rc)) rc=ESMF_SUCCESS
        return
      endif

#else       

    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GridspecReadMosaic

! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecQueryTileSize"
subroutine ESMF_GridspecQueryTileSize(filename, tileSize, rc)

    character(len=*), intent(in)     :: filename
    integer, intent(out)             :: tileSize
    integer, intent(out), optional   :: rc

#ifdef ESMF_NETCDF
    integer :: ncid, nvars, attlen, i
    integer :: nx, ny
    integer :: ncStatus
    integer :: ndims, dimids(2)
    character(len=128) :: attstr

    if (present(rc)) rc=ESMF_SUCCESS

    ncStatus = nf90_open(path=filename, mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        trim(filename), &
        rc)) return
    ncStatus = nf90_inquire(ncid, nVariables=nvars)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        trim(filename), &
        rc)) return
    do i=1,nvars
       ! Check its standard_name attribute
       ncStatus = nf90_inquire_attribute(ncid, i, 'standard_name', len=attlen)
       if (ncStatus == nf90_noerr) then
          ncStatus = nf90_get_att(ncid, i, 'standard_name', values=attstr)
          if (attstr(1:attlen) .eq. 'geographic_longitude' .or. &
                 attstr(1:attlen) .eq. 'geographic_latitude') then
            ! read the longitude or latitude variable
            ! First find the dimension of this variable
            ncStatus = nf90_inquire_variable(ncid, i, ndims=ndims, dimids=dimids)
            if (ndims /= 2) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The longitude variable should have dimension 2", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            ! find out the dimenison size
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=nx)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            ncStatus = nf90_inquire_dimension(ncid, dimids(2), len=ny)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            if (nx /= ny) then   
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The tile has to be square.", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            tileSize=(nx-1)/2
            goto 20
         endif
       endif
     enddo
     
20   continue
     ncStatus = nf90_close(ncid)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "close tile file", &
               rc)) return
     return
#else       

    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GridspecQueryTileSize

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecReadTile"

!BOP
! !INTERFACE:
! Read in a tile file that defines the supergrid of a given tile
! A tile file should have a dummy variable that has the standard_name attribute set to "grid_tile_spec".  The "projection"
! attribute should be set to "cube_gnomonic".  
! The latitude and longitude variables should have standard name called "geographic_longitude" and "geographic_latitude" 
! and their dimensions should be (2*tielsize+1, 2*tilesize+1).  It defines the corner coordinates, edge coordinates and 
! the center coordinates in one variable called "super grid".
! Output the tile size, the corner and the center coordinates.
subroutine ESMF_GridspecReadTile(filename, tileSize, cornerLon, cornerLat, centerLon, centerLat, start, count, rc)

! !ARGUMENTS:
 
    character(len=*), intent(in)     :: filename
    integer, intent(in)              :: tileSize
    real(ESMF_KIND_R8), pointer      :: cornerLon(:,:)
    real(ESMF_KIND_R8), pointer      :: cornerLat(:,:)
    real(ESMF_KIND_R8), pointer      :: centerLon(:,:)
    real(ESMF_KIND_R8), pointer      :: centerLat(:,:)
    integer, optional, intent(in)  :: start(2)
    integer, optional, intent(in)  :: count(2)
    integer, optional, intent(out)          :: rc

    integer :: ncid, nvars, attlen, i
    integer :: nx, ny
    integer :: ncStatus
    integer :: ndims, dimids(2)
    character(len=128) :: attstr
    integer :: start1(2), count1(2)
    real(ESMF_KIND_R8), allocatable :: supercoord(:,:)
    integer :: localrc

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                           ESMF_ERR_PASSTHRU, &
                           ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                           ESMF_ERR_PASSTHRU, &
                           ESMF_CONTEXT, rcToReturn=rc)) return

#ifdef ESMF_NETCDF

    ncStatus = nf90_open(path=filename, mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        filename, &
        rc)) return
    ncStatus = nf90_inquire(ncid, nVariables=nvars)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        filename, &
        rc)) return
    do i=1,nvars
       ! Check its standard_name attribute
       ncStatus = nf90_inquire_attribute(ncid, i, 'standard_name', len=attlen)
       if (ncStatus == nf90_noerr) then
          ncStatus = nf90_get_att(ncid, i, 'standard_name', values=attstr)
          if (attstr(1:attlen) .eq. 'grid_tile_spec') then
            ! check the projection attribute
            ncStatus = nf90_inquire_attribute(ncid, i, 'projection', len=attlen)  
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "projection attribute does not exist", &
               rc)) return
            ncStatus = nf90_get_att(ncid, i, 'projection', values=attstr)
            if (attstr(1:attlen) .ne. 'cube_gnomonic') then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- Only Cube Gnomonic projection is currently supported", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif   
          elseif (attstr(1:attlen) .eq. 'geographic_longitude' .or. &
                 attstr(1:attlen) .eq. 'geographic_latitude') then
            ! read the longitude or latitude variable
            ! First find the dimension of this variable
            ncStatus = nf90_inquire_variable(ncid, i, ndims=ndims, dimids=dimids)
            if (ndims /= 2) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The longitude variable should have dimension 2", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            ! find out the dimenison size
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=nx)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            ncStatus = nf90_inquire_dimension(ncid, dimids(2), len=ny)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            if (nx /= ny) then   
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The tile has to be square.", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            if (nx /= (tilesize*2+1)) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The tilesize does not match with the supergrid dimension", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            
            if (present(start) .and. present(count)) then
               ! read a block instead of the entire array
               count1=count*2+1
               start1=start*2-1
               if (.not. allocated(supercoord)) then
                  allocate(supercoord(count1(1), count1(2)))
               endif
               ncStatus = nf90_get_var(ncid, i, supercoord, start=start1, count=count1)
               if (CDFCheckError (ncStatus, &
                  ESMF_METHOD,  &
                  ESMF_SRCLINE, &
                  "error reading geographic longitude coordinates", &
                  rc)) return
               ! copy to the corner and center lat/lon arrays
               if (attstr(1:attlen) .eq. 'geographic_latitude') then
                 cornerLat=supercoord(1:count1(1):2, 1:count1(2):2)
                 centerLat=supercoord(2:count1(1):2, 2:count1(2):2)
               else
                 cornerLon=supercoord(1:count1(1):2, 1:count1(2):2)
                 centerLon=supercoord(2:count1(1):2, 2:count1(2):2)
               endif
            else
               if (.not. allocated(supercoord)) then
                   allocate(supercoord(nx, ny))
               endif
               ncStatus = nf90_get_var(ncid, i, supercoord)
               if (CDFCheckError (ncStatus, &
                  ESMF_METHOD,  &
                  ESMF_SRCLINE, &
                  "error reading geographic longitude coordinates", &
                  rc)) return
               ! copy to the corner and center lat/lon arrays
               if (attstr(1:attlen) .eq. 'geographic_latitude') then
                  cornerLat=supercoord(1:nx:2, 1:ny:2)
                  centerLat=supercoord(2:nx:2, 2:ny:2)
               else
                  cornerLon=supercoord(1:nx:2, 1:ny:2)
                  centerLon=supercoord(2:nx:2, 2:ny:2)
               endif
             endif
          endif
       endif
     enddo
     ncStatus = nf90_close(ncid)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "close tile file", &
               rc)) return
#else       

    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GridspecReadTile

! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "readContacts"

! Read contacts and contact_index variables and store the information in contact and conindex arrays
subroutine readContacts(ncid, varid, dims, mosaicname, tilenames, &
                 contact, connindex, rc)

  integer, intent(in)                       :: ncid, varid
  integer, intent(in)                       :: dims(2)
  character(len=*), intent(in)              :: mosaicname
  character(len=*), intent(in), target      :: tilenames(:)
  integer, pointer                          :: contact(:,:)
  integer, pointer                          :: connindex(:,:,:)
  integer, intent(out)                      :: rc

  ! varid is the id for the "contact_region" variable
  ! its standard_name should be "grid_contact_spec" and contact_type should be "boundary"
  ! The "contact_index" attribute defines the variable that defines the edge connection
  ! The contact_region array contacts the neighboring pair of tiles for each connection:
  ! contacts =
  !  "C48_mosaic:tile1::C48_mosaic:tile2",
  !  "C48_mosaic:tile1::C48_mosaic:tile3",
  !  "C48_mosaic:tile1::C48_mosaic:tile5",
  !  "C48_mosaic:tile1::C48_mosaic:tile6",
  !  "C48_mosaic:tile2::C48_mosaic:tile3",
  !  "C48_mosaic:tile2::C48_mosaic:tile4",
  !  "C48_mosaic:tile2::C48_mosaic:tile6",
  !  "C48_mosaic:tile3::C48_mosaic:tile4",
  !  "C48_mosaic:tile3::C48_mosaic:tile5",
  !  "C48_mosaic:tile4::C48_mosaic:tile5",
  !  "C48_mosaic:tile4::C48_mosaic:tile6",
  !  "C48_mosaic:tile5::C48_mosaic:tile6" ;
  ! 
  ! The contact_index contains a pair of four points that defines the two edges that contact to 
  ! each other from the two neighboring tiles.  Assuming the four points are A, B, C, and D.  
  ! A and B defines the edge of tile 1 and C and D defines the edge of tile2.  A is the same point
  ! as C and B is the same as D.  (Ai, Aj) is the index for point A.
  !  Ai:Bi,Aj:Bj::Ci:Di,Cj:Dj
  !
  ! Here is an example of the 12 contacts for a cube-sphere grid
  ! contact_index =
  !  "96:96,1:96::1:1,1:96",
  !  "1:96,96:96::1:1,96:1",
  !  "1:1,1:96::96:1,96:96",
  !  "1:96,1:1::1:96,96:96",
  !  "1:96,96:96::1:96,1:1",
  !  "96:96,1:96::96:1,1:1",
  !  "1:96,1:1::96:96,96:1",
  !  "96:96,1:96::1:1,1:96",
  !  "1:96,96:96::1:1,96:1",
  !  "1:96,96:96::1:96,1:1",
  !  "96:96,1:96::96:1,1:1",
  !  "96:96,1:96::1:1,1:96" ;

#ifdef ESMF_NETCDF
  character(len=128) attstr
  integer   :: i, j, nvars, attlen
  integer   :: tile1, tile2
  integer   :: tiletuple(2,4)
  integer   :: varid1
  integer   :: ncStatus
  character(len=1), allocatable :: contactstring(:,:)
  character(len=1), allocatable :: indexstring(:,:)
  ! Check the attributes first
  ncStatus = nf90_inquire_attribute(ncid, varid, 'standard_name', len=attlen)
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         "standard_name attribute does not exit", &
         rc)) return
  ncStatus = nf90_get_att(ncid, varid, 'standard_name', values=attstr)
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         "standard_name attribute does not exit", &
         rc)) return
  if (attstr(1:attlen) .ne. 'grid_contact_spec') then
     call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
          msg="- The standard_name attribute is not 'grid_contact_spec'", & 
          ESMF_CONTEXT, rcToReturn=rc) 
     return
  endif
  ! Check the attributes first
  ncStatus = nf90_inquire_attribute(ncid, varid, 'contact_type', len=attlen)
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         "contact_type attribute does not exit", &
         rc)) return
  ncStatus = nf90_get_att(ncid, varid, 'contact_type', values=attstr)
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         "error reading contact_type attribute", &
         rc)) return
  if (attstr(1:attlen) .ne. 'boundary') then
     call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
          msg="- Only 'boundary' contact_type is supported", & 
          ESMF_CONTEXT, rcToReturn=rc) 
     return
  endif
  allocate(contactstring(dims(1),dims(2)))
  ncStatus = nf90_get_var(ncid, varid, contactstring, start=(/1,1/), count=(/dims(1), dims(2)/))
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         "Error reading contacts variable", &
         rc)) return
  do j=1, dims(2)
    call parse_contact(ESMF_UtilArray2String(contactstring(:,j)), tilenames, tile1, tile2)
    contact(1,j)=tile1
    contact(2,j)=tile2
  enddo

  ! read the contact_index
  ncStatus = nf90_inquire_attribute(ncid, varid, 'contact_index', len=attlen)
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         "contact_index attribute does not exit", &
         rc)) return
  ncStatus = nf90_get_att(ncid, varid, 'contact_index', values=attstr)
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         "error reading contact_index attribute", &
         rc)) return

  ncStatus = nf90_inq_varid(ncid, attstr(1:attlen), varid1)
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         attstr(1:attlen), &
         rc)) return
  allocate(indexstring(dims(1),dims(2)))
  ncStatus = nf90_get_var(ncid, varid1, indexstring, start=(/1,1/), count=(/dims(1), dims(2)/))
  if (CDFCheckError (ncStatus, &
         ESMF_METHOD,  &
         ESMF_SRCLINE, &
         "contact_type attribute does not exit", &
         rc)) return
  do j=1, dims(2)
    call parse_contactindex(ESMF_UtilArray2String(indexstring(:,j)), tiletuple)
    connindex(:,:,j)=tiletuple
  enddo

#else       
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif
    
end subroutine readContacts

subroutine trim_null(string)

  character(len=*), intent(inout) :: string
  
  integer :: i

  do i=1,len(string)
   if (iachar(string(i:i))==0) then
      string(i:i)=' '
   endif
  enddo
  return
end subroutine trim_null

! Parse a contact string "mosaic:tile1::mosaic::tile2" and a tilename array.  Return the index of tile1 and tile2
subroutine parse_contact(string, tiles, tile1, tile2)

  character(len=*)                      :: string
  character(len=*), intent(in), target :: tiles(:)
  integer, intent(out) :: tile1, tile2

  integer :: pos, pos1, pos2
  integer :: length
  character(len=ESMF_MAXSTR) :: tempstr, tilestr
  integer :: i

  call trim_null(string)
  length = len_trim(string)
  pos=index(string, "::")
  pos1 = index(string(1:pos-1), ":")
  tilestr = string(pos1+1:pos-1)

  do i=1,size(tiles)
    if (trim(tilestr) .eq. trim(tiles(i))) then
       tile1 = i
       exit
    endif
  enddo
  tempstr = string(pos+2:length)
  pos1 = index(tempstr,":");
  tilestr = tempstr(pos1+1:len(trim(tempstr)))
  do i=1,size(tiles)
    if (tilestr .eq. trim(tiles(i))) then
       tile2 = i
       exit
    endif
  enddo
  return
end subroutine parse_contact

! Parse a contact string "mosaic:tile1::mosaic::tile2" and a tilename array.  Return the index of tile1 and tile2
subroutine parse_contactindex(string, tiletuple)

  character(len=*)     :: string
  integer, intent(out) :: tiletuple(2,4)

  ! The contact_index contains a pair of four points that defines the two edges that contact to 
  ! each other from the two neighboring tiles.  Assuming the four points are A, B, C, and D.  
  ! A and B defines the edge of tile 1 and C and D defines the edge of tile2.  A is the same point
  ! as C and B is the same as D.  (Ai, Aj) is the index for point A.
  !  Ai:Bi,Aj:Bj::Ci:Di,Cj:Dj

  integer :: pos, pos1, pos2
  integer :: length, stat
  integer :: Ai, Bi, Aj, Bj, Ci, Di, Cj, Dj
  character(len=40) :: string1
  integer :: i,j

  call trim_null(string)
  length = len_trim(string)
  pos=index(string, "::")
  pos1 = index(string(1:pos-1), ",")
  pos2 = index(string(1:pos1-1), ":")
  read(string(1:pos2-1),*,iostat=stat) Ai
  read(string(pos2+1:pos1-1),*,iostat=stat) Bi
  pos2 = index(string(pos1+1:pos-1), ":")
  read(string(pos1+1:pos1+pos2-1),*,iostat=stat) Aj
  read(string(pos1+pos2+1:pos-1),*,iostat=stat) Bj
  string1 = string(pos+2:length)
  length = length-pos-1
  pos1 = index(string1, ",")
  pos2 = index(string1(1:pos1-1), ":")
  !print *, "Ci,Di:",string1(1:pos2-1), " ",string1(pos2+1:pos1-1)
  read(string1(1:pos2-1),*,iostat=stat) Ci
  read(string1(pos2+1:pos1-1),*,iostat=stat) Di
  pos2 = index(string1(pos1+1:length), ":")
  !print *, pos, pos1, pos2, string1(pos1+1:length)
  !print *, "Cj,Dj:",string1(pos1+1:pos1+pos2-1), " ", string1(pos1+pos2+1:length)
  read(string1(pos1+1:pos1+pos2-1),*,iostat=stat) Cj
  read(string1(pos1+pos2+1:length),*,iostat=stat) Dj
  !print *, "parse_contactindex:", Ai, Aj, Bi, Bj, Ci, Cj, Di, Dj
  tiletuple(1,1)=Ai
  tiletuple(2,1)=Aj
  tiletuple(1,2)=Bi
  tiletuple(2,2)=Bj
  tiletuple(1,3)=Ci
  tiletuple(2,3)=Cj
  tiletuple(1,4)=Di
  tiletuple(2,4)=Dj
  ! devide by 2 if the index /= 1
  do j=1,4
   do i=1,2
     if (tiletuple(i,j)/=1) tiletuple(i,j)=tiletuple(i,j)/2
   enddo
  enddo
  return
end subroutine parse_contactindex


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
        call ESMF_LogWrite (msg="netCDF Status Return Error", logmsgFlag=ESMF_LOGMSG_ERROR, &
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

end module ESMF_IOGridmosaicMod

 