! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
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
      use ESMF_StaggerLocTypeMod

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
    character(len=ESMF_MAXSTR)               :: name
    integer                                  :: ntiles      ! number of tiles
    integer                                  :: nx, ny      ! the size of the tile, maybe a rectangular grid
    integer                                  :: ncontacts   ! number of contacts
    character(len=ESMF_MAXPATHLEN)           :: tileDirectory  ! the path of the tile files
    character(len=ESMF_MAXPATHLEN), pointer  :: filenames(:)  ! the tile filename array
    character(len=ESMF_MAXSTR), pointer      :: tilenames(:)  ! the tile names  
    integer, allocatable                     :: contact(:,:)   ! pair of tiles in each contact
    integer, allocatable                     :: connindex(:,:,:)  ! the end points of the contact edges
  end type
    
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_Mosaic
  public ESMF_GridspecReadTile
  public ESMF_GridspecReadStagger
  public ESMF_GridspecReadMosaic
  public ESMF_GridspecQueryTileFile
  public ESMF_GridspecQueryTileSize
  public ESMF_GridspecQueryTileGlobal
  public ESMF_MosaicDestroy

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_GridspecReadStagger -- Generic interface

! !INTERFACE:
interface ESMF_GridspecReadStagger

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_GridspecReadStaggerR8
      module procedure ESMF_GridspecReadStaggerR4

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_GridspecReadStagger} functions.
!EOPI
end interface

!==============================================================================

      contains

!==============================================================================
!
! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecReadMosaic"

!BOPI
! !INTERFACE:
subroutine ESMF_GridspecReadMosaic(filename, mosaic, tileFilePath, rc)

! !ARGUMENTS:
 
    character(len=*), intent(in)           :: filename
    type(ESMF_Mosaic), intent(inout)       :: mosaic
    character(len=*), intent(in), optional :: tileFilePath
    integer, optional, intent(out)         :: rc

! !DESCRIPTIONS:
!   find a variable with the standard_name attribute set to "grid_mosaic_spec"
!   use the "children" attribute to find the variable that defines tile names
!   use the "contact_regions" attribute to find the variable that defines the contacts
!   The value of the variable is the mosaic name used in the contact list
!   
!   Variable "gridfiles" defines the path where the tile files are stored. If the 
!   optional argument tileFilePath is given, use it instead of the path defined in the
!   mosaic file.  Variable "gridfiles" defines the name of the tile files.  It is a 1D 
!   array with the same size as the "children" variable.
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
!EOPI
#ifdef ESMF_NETCDF

    integer   :: ncid, varid
    integer   :: i, j, nvars, attlen
    integer   :: ntiles, ncontacts
    character(len=128) :: attstr
    character(len=128) :: mosaicname
    character(len=ESMF_MAXPATHLEN) :: tempname
    integer            :: strlen
    character(len=1), allocatable :: temptilenames(:,:)
    character(len=1),  allocatable :: tilefilenames(:,:)
    integer, pointer :: contact(:,:)
    integer, pointer :: connindex(:,:,:)
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
               "children variable does not exit", &
               rc)) return
            ! Find the dimension of the tile variable
            ncStatus = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "fail to inquire the variable defined by children attribute", &
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
             allocate(mosaic%tilenames(ntiles), temptilenames(strlen, ntiles))
             ncStatus = nf90_get_var(ncid, varid, temptilenames, start=(/1,1/), count=(/strlen, ntiles/))
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "get tile names", &
               rc)) return
              ! replace null character by blank
             do j=1,ntiles
              mosaic%tilenames(j) = ESMF_UtilArray2String(temptilenames(:,j))
              call trim_null(mosaic%tilenames(j))
             enddo
             
            if (ntiles > 1) then
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
                 "contact_regions variable does not exit", &
                 rc)) return
              ! Find the dimension of this variable
              ncStatus = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
              if (CDFCheckError (ncStatus, &
                 ESMF_METHOD,  &
                 ESMF_SRCLINE, &
                 "fail to inquire the variable defined by contact_regions", &
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
              call readContacts(ncid, varid, dims, mosaicname, mosaic%tilenames, &
                   mosaic%contact, mosaic%connindex, localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rcToReturn=rc)) return
              mosaic%ncontacts=dims(2)
            endif
         endif
       endif
     enddo

     ! find the tile file path
     if (present(tileFilePath)) then
       mosaic%tileDirectory = tileFilePath
     else
       ncStatus = nf90_inq_varid(ncid, 'gridlocation', varid)
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "inquire variable gridlocation", &
               rc)) return
       ! Get the directory name where the tile files are stored
       ncStatus = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "inquire variable gridlocation", &
               rc)) return
       ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=dims(1))
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
       ! initialize the string to null characters
       do k=1,ESMF_MAXPATHLEN
           mosaic%tileDirectory(k:k)=char(0)
       enddo
       ncStatus = nf90_get_var(ncid, varid, mosaic%tileDirectory, start=(/1/), count=(/dims(1)/))
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "fail to get gridlocation", &
               rc)) return
       call trim_null(mosaic%tileDirectory)
     endif

     ! get the tile file names
     ncStatus = nf90_inq_varid(ncid, 'gridfiles', varid)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "inquire variable gridfiles", &
               rc)) return
       
     ! Find the dimension of this variable
     ncStatus = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "inquire variable gridfiles error", &
               rc)) return
     if (ndims /= 2) then
        call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- gridfiles variable dimension /= 2", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
        return
     endif    
     ! query it's dimension to find out the string length
     ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=dims(1))
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "gridfiles dimension inquire", &
               rc)) return
     allocate(tilefilenames(dims(1), ntiles))
     allocate(mosaic%filenames(ntiles))
     ! initialize the string to null characters
     do j=1,ntiles
        do k=1,ESMF_MAXPATHLEN
            mosaic%filenames(ntiles)(k:k)=char(0)
        enddo
     enddo
     ncStatus = nf90_get_var(ncid, varid, tilefilenames, start=(/1,1/), count=(/dims(1), ntiles/))
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "fail to get gridfiles", &
               rc)) return
     ! replace null character by blank
     do j=1,ntiles
          mosaic%filenames(j)=ESMF_UtilArray2String(tilefilenames(:,j))
          call trim_null(mosaic%filenames(j))
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
         ! find the dimension of the tile by reading one of the tilefiles
         ! assuming all the tilefiles have the same size
         tempname = trim(mosaic%TileDirectory)//trim(mosaic%filenames(1))
         call ESMF_GridSpecQueryTileSize(tempname, mosaic%nx, mosaic%ny, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
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
#define ESMF_METHOD "ESMF_GridspecQueryTileFile"
subroutine ESMF_GridspecQueryTileFile(filename, isSupergrid, rc)

    character(len=*), intent(in)     :: filename
    logical, intent(out)             :: isSupergrid
    integer, intent(out), optional   :: rc

#ifdef ESMF_NETCDF
    integer :: ncid, nvars, attlen, i
    integer :: ncStatus
    character(len=128) :: attstr

    if (present(rc)) rc=ESMF_SUCCESS

    ! Check if the file contain a dummy variable with the standard_name
    ! attribute set to grid_tile_spec

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
    isSupergrid = .false.
    do i=1,nvars
       ! Check its standard_name attribute
       ncStatus = nf90_inquire_attribute(ncid, i, 'standard_name', len=attlen)
       if (ncStatus == nf90_noerr) then
          ncStatus = nf90_get_att(ncid, i, 'standard_name', values=attstr)
          if (attstr(1:attlen) .eq. 'grid_tile_spec') then
            isSupergrid = .true.
          endif
       endif
       if (isSupergrid) exit
     enddo
     ncStatus = nf90_close(ncid)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, &
        filename, &
        rc)) return
#else       

    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GridSpecQueryTileFile


! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecQueryTileGlobal"
subroutine ESMF_GridspecQueryTileGlobal(filename, isGlobal, rc)

    character(len=*), intent(in)     :: filename
    logical, intent(out)             :: isGlobal
    integer, intent(out), optional   :: rc

#ifdef ESMF_NETCDF
    integer :: ncid, nvars, attlen, i
    integer :: ncStatus
    integer :: nx, ny
    integer :: ndims, dimids(2)
    real(ESMF_KIND_R8), allocatable :: supergrid(:,:)
    real(ESMF_KIND_R8) :: minlon, maxlon
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
          if (attstr(1:attlen) .eq. 'geographic_longitude') then
            ! read the longitude variable
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
            allocate(supergrid(nx,ny))
            ncStatus = nf90_get_var(ncid, i, supergrid)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            minlon = minval(supergrid)
            maxlon = maxval(supergrid)
            if (maxlon-minlon == 360.0) then
              isGlobal = .true.
            else
              isGlobal = .false.
            endif
            deallocate(supergrid)
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

end subroutine ESMF_GridSpecQueryTileGlobal

! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecQueryTileSize"
subroutine ESMF_GridspecQueryTileSize(filename, nx, ny, units, rc)

    character(len=*), intent(in)     :: filename
    integer, intent(out)             :: nx, ny
    character(len=*), intent(out), optional    :: units
    integer, intent(out), optional   :: rc

#ifdef ESMF_NETCDF
    integer :: ncid, nvars, attlen, i
    integer :: ncStatus
    integer :: ndims, dimids(2)
    character(len=128) :: attstr
    character(len=1024) :: errmsg

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
            ! return the dimension of the center grid
            nx = (nx-1)/2
            ny = (ny-1)/2

            ! find out units attribute
            if (present(units)) then
              ncStatus = nf90_inquire_attribute(ncid, i, "units", len=attlen)
              errmsg ="attribute units for coordinate variable" //" in "//trim(filename)
              if (CDFCheckError (ncStatus, &
                  ESMF_METHOD,  &
                  ESMF_SRCLINE, &
                  errmsg, &
                  rc)) return
              ncStatus = nf90_get_att(ncid, i, 'units',attstr)
              if (CDFCheckError (ncStatus, &
                  ESMF_METHOD,  &
                  ESMF_SRCLINE, &
                  errmsg, &
                  rc)) return
              if (attstr(1:6) .eq. 'degree') then 
                 units = 'degrees'
              elseif (attstr(1:6) .eq. 'radian') then
                 units = 'radians'
              endif
            endif

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

!BOPI
! !INTERFACE:
! Read in a tile file that defines the supergrid of a given tile
! A tile file should have a dummy variable that has the standard_name attribute set to "grid_tile_spec".  
! The latitude and longitude variables should have standard name called "geographic_longitude" and "geographic_latitude" 
! and their dimensions should be (2*nx+1, 2*ny+1).  It defines the corner coordinates, edge coordinates and 
! the center coordinates in one variable called "super grid".
! Output the tile size, the corner and the center coordinates.
subroutine ESMF_GridspecReadTile(filename, nx, ny, centerLon, centerLat, cornerLon, cornerLat, start, count, rc)

! !ARGUMENTS:
 
    character(len=*), intent(in)               :: filename
    integer, intent(in)                        :: nx, ny
    real(ESMF_KIND_R8),  pointer               :: centerLon(:,:)
    real(ESMF_KIND_R8),  pointer               :: centerLat(:,:)
    real(ESMF_KIND_R8), optional, pointer      :: cornerLon(:,:)
    real(ESMF_KIND_R8), optional, pointer      :: cornerLat(:,:)
    integer, optional, intent(in)              :: start(2)
    integer, optional, intent(in)              :: count(2)
    integer, optional, intent(out)             :: rc
!EOPI

    integer :: ncid, nvars, attlen, i
    integer :: nx1, ny1
    integer :: ncStatus
    integer :: ndims, dimids(2)
    character(len=128) :: attstr
    integer :: start1(2), count1(2)
    real(ESMF_KIND_R8), allocatable :: supercoord(:,:)
    integer :: localrc
    logical :: foundit

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
    foundit = .false.
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
            ! skip checking the attributes -- not sure which one should be set to what
            ! but makesure this dummy variable exists
            foundit = .true.
#if 0
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
#endif
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
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=nx1)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            ncStatus = nf90_inquire_dimension(ncid, dimids(2), len=ny1)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            if (nx1 /= (nx*2+1)) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The x dimension of the tile does not match with the supergrid dimension", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            if (ny1 /= (ny*2+1)) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The y dimension of the tile does not match with the supergrid dimension", & 
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
                 if (present(cornerLat)) then
                    cornerLat=supercoord(1:count1(1):2, 1:count1(2):2)
                 endif
                 centerLat=supercoord(2:count1(1):2, 2:count1(2):2)
               else
                 if (present(cornerLon)) then
                    cornerLon=supercoord(1:count1(1):2, 1:count1(2):2)
                 endif
                 centerLon=supercoord(2:count1(1):2, 2:count1(2):2)
               endif
            else
               if (.not. allocated(supercoord)) then
                   allocate(supercoord(nx1, ny1))
               endif
               ncStatus = nf90_get_var(ncid, i, supercoord)
               if (CDFCheckError (ncStatus, &
                  ESMF_METHOD,  &
                  ESMF_SRCLINE, &
                  "error reading geographic longitude coordinates", &
                  rc)) return
               ! copy to the corner and center lat/lon arrays
               if (attstr(1:attlen) .eq. 'geographic_latitude') then
                  if (present(cornerLat)) then
                     cornerLat=supercoord(1:nx1:2, 1:ny1:2)
                  endif
                  centerLat=supercoord(2:nx1:2, 2:ny1:2)
               else
                  if (present(cornerLon)) then
                     cornerLon=supercoord(1:nx1:2, 1:ny1:2)
                  endif
                  centerLon=supercoord(2:nx1:2, 2:ny1:2)
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
     if (.not. foundit .and. present(rc)) rc=ESMF_FAILURE
     return
#else       

    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GridspecReadTile

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecReadStaggerR8"

!BOPI
! !IROUTINE: ESMF_GridspecReadStagger - Read in a tile file that defines the supergrid of a given tile from a GRIDSPEC Tile file..

! !INTERFACE:
! Read in a tile file that defines the supergrid of a given tile
! A tile file should have a dummy variable that has the standard_name attribute set to "grid_tile_spec".  
! The latitude and longitude variables should have standard name called "geographic_longitude" and "geographic_latitude" 
! and their dimensions should be (2*nx+1, 2*ny+1).  It defines the corner coordinates, edge coordinates and 
! the center coordinates in one variable called "super grid".
! This subroutine reads one stagger location at a time
subroutine ESMF_GridspecReadStaggerR8(filename, nx, ny, lon, lat, staggerLoc, start, count, rc)

! !ARGUMENTS:
 
    character(len=*), intent(in)               :: filename
    integer, intent(in)                        :: nx, ny
    real(ESMF_KIND_R8), TARGET                 :: lon(:,:)
    real(ESMF_KIND_R8), TARGET                 :: lat(:,:)
    type(ESMF_StaggerLoc)                      :: staggerLoc
    integer, optional, intent(in)              :: start(2)
    integer, optional, intent(in)              :: count(2)
    integer, optional, intent(out)             :: rc
!EOPI

    integer :: ncid, nvars, attlen, i
    integer :: nx1, ny1
    integer :: ncStatus
    integer :: ndims, dimids(2)
    character(len=128) :: attstr
    integer :: start1(2), count1(2)
    real(ESMF_KIND_R8), allocatable :: supercoord(:,:)
    integer :: localrc
    logical :: foundit

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
    foundit = .false.
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
            ! skip checking the attributes -- not sure which one should be set to what
            ! but makesure this dummy variable exists
            foundit = .true.
#if 0
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
#endif
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
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=nx1)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            ncStatus = nf90_inquire_dimension(ncid, dimids(2), len=ny1)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            if (nx1 /= (nx*2+1)) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The x dimension of the tile does not match with the supergrid dimension", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            if (ny1 /= (ny*2+1)) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The y dimension of the tile does not match with the supergrid dimension", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            
            if (present(start) .and. present(count)) then
               ! read a block instead of the entire array
               count1 = count
               if (staggerLoc == ESMF_STAGGERLOC_CENTER) then
                  start1=start*2
               elseif (staggerLoc == ESMF_STAGGERLOC_CORNER) then
                  start1=start*2-1
               elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1) then
                  start1(1)=start(1)*2-1
                  start1(2)=start(2)*2
               elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2) then
                  start1(1)=start(1)*2
                  start1(2)=start(2)*2-1
               endif
            else
               count1(1) = nx
               count1(2) = ny
               if (staggerLoc == ESMF_STAGGERLOC_CENTER) then
                  start1=2
               elseif (staggerLoc == ESMF_STAGGERLOC_CORNER) then
                  start1=1
               elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1) then
                  start1(1)=1
                  start1(2)=2
               elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2) then
                  start1(1)=2
                  start1(2)=1
               endif
            endif
            if (attstr(1:attlen) .eq. 'geographic_latitude') then
               ncStatus = nf90_get_var(ncid, i, lat, start=start1, count=count1, stride=(/2,2/))
            else
               ncStatus = nf90_get_var(ncid, i, lon, start=start1, count=count1, stride=(/2,2/))
            endif
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "error reading stagger coordinates", &
               rc)) return
         endif
       endif
     enddo
     ncStatus = nf90_close(ncid)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "close tile file", &
               rc)) return
     if (.not. foundit .and. present(rc)) rc=ESMF_FAILURE
     return
#else       

    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GridspecReadStaggerR8

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridspecReadStaggerR4"

!BOPI
! !IROUTINE: ESMF_GridspecReadStagger - Read in a tile file that defines the supergrid of a given tile from a GRIDSPEC Tile file..

! !INTERFACE:
! Read in a tile file that defines the supergrid of a given tile
! A tile file should have a dummy variable that has the standard_name attribute set to "grid_tile_spec".  
! The latitude and longitude variables should have standard name called "geographic_longitude" and "geographic_latitude" 
! and their dimensions should be (2*nx+1, 2*ny+1).  It defines the corner coordinates, edge coordinates and 
! the center coordinates in one variable called "super grid".
! This subroutine reads one stagger location at a time
subroutine ESMF_GridspecReadStaggerR4(filename, nx, ny, lon, lat, staggerLoc, start, count, rc)

! !ARGUMENTS:
 
    character(len=*), intent(in)               :: filename
    integer, intent(in)                        :: nx, ny
    real(ESMF_KIND_R4), TARGET                 :: lon(:,:)
    real(ESMF_KIND_R4), TARGET                 :: lat(:,:)
    type(ESMF_StaggerLoc)                      :: staggerLoc
    integer, optional, intent(in)              :: start(2)
    integer, optional, intent(in)              :: count(2)
    integer, optional, intent(out)             :: rc
!EOPI

    integer :: ncid, nvars, attlen, i
    integer :: nx1, ny1
    integer :: ncStatus
    integer :: ndims, dimids(2)
    character(len=128) :: attstr
    integer :: start1(2), count1(2)
    real(ESMF_KIND_R8), allocatable :: supercoord(:,:)
    integer :: localrc
    logical :: foundit

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
    foundit = .false.
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
            ! skip checking the attributes -- not sure which one should be set to what
            ! but makesure this dummy variable exists
            foundit = .true.
#if 0
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
#endif
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
            ncStatus = nf90_inquire_dimension(ncid, dimids(1), len=nx1)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            ncStatus = nf90_inquire_dimension(ncid, dimids(2), len=ny1)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "contact dimension inquire", &
               rc)) return
            if (nx1 /= (nx*2+1)) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The x dimension of the tile does not match with the supergrid dimension", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            if (ny1 /= (ny*2+1)) then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- The y dimension of the tile does not match with the supergrid dimension", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
            
            if (present(start) .and. present(count)) then
               ! read a block instead of the entire array
               count1 = count
               if (staggerLoc == ESMF_STAGGERLOC_CENTER) then
                  start1=start*2
               elseif (staggerLoc == ESMF_STAGGERLOC_CORNER) then
                  start1=start*2-1
               elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1) then
                  start1(1)=start(1)*2-1
                  start1(2)=start(2)*2
               elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2) then
                  start1(1)=start(1)*2
                  start1(2)=start(2)*2-1
               endif
            else
               count1(1) = nx
               count1(2) = ny
               if (staggerLoc == ESMF_STAGGERLOC_CENTER) then
                  start1=2
               elseif (staggerLoc == ESMF_STAGGERLOC_CORNER) then
                  start1=1
               elseif (staggerLoc == ESMF_STAGGERLOC_EDGE1) then
                  start1(1)=1
                  start1(2)=2
               elseif (staggerLoc == ESMF_STAGGERLOC_EDGE2) then
                  start1(1)=2
                  start1(2)=1
               endif
            endif
            if (attstr(1:attlen) .eq. 'geographic_latitude') then
               ncStatus = nf90_get_var(ncid, i, lat, start=start1, count=count1, stride=(/2,2/))
            else
               ncStatus = nf90_get_var(ncid, i, lon, start=start1, count=count1, stride=(/2,2/))
            endif
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "error reading stagger coordinates", &
               rc)) return
         endif
       endif
     enddo
     ncStatus = nf90_close(ncid)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD,  &
               ESMF_SRCLINE, &
               "close tile file", &
               rc)) return
     if (.not. foundit .and. present(rc)) rc=ESMF_FAILURE
     return
#else       

    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GridspecReadStaggerR4

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
  integer, intent(inout)                     :: contact(:,:)
  integer, intent(inout)                    :: connindex(:,:,:)
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

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MosaicDestroy"

!BOPI
! !INTERFACE:
subroutine ESMF_MosaicDestroy(mosaic, rc)

! !ARGUMENTS:
 
    type(ESMF_Mosaic), intent(inout)       :: mosaic
    integer, optional, intent(out)         :: rc

!EOPI

   ! Only allocated when there is netcdf
#ifdef ESMF_NETCDF

    ! Get rid of allocated members
    deallocate(mosaic%filenames)
    if (allocated(mosaic%contact)) deallocate(mosaic%contact)
    if (allocated(mosaic%connindex)) deallocate(mosaic%connindex)
#endif

    ! return success
    if (present(rc)) rc=ESMF_SUCCESS   

end subroutine ESMF_MosaicDestroy



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

end module ESMF_IOGridmosaicMod

 
