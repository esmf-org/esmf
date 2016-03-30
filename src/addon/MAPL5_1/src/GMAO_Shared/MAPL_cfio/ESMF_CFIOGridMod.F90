!==============================================================================
!BOP
! !MODULE: ESMF_CFIOGridMod.F90 - Source file for CFIO Grid

       module ESMF_CFIOGridMod

!
! !DESCRIPTION:
!
! The code in this file provides grid data type definitions and interface
! specifications
!
! !REVISION HISTORY:
! Feb 2007  Baoyu Yin   Separated from ESMF_CFIOMod

!------------------------------------------------------------------------------
! !USES:
      use ESMF_CFIOUtilMod, only : MLEN, MVARLEN

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
! !PUBLIC DATA TYPES:
!
      public :: ESMF_CFIOGrid            ! A CFIO grid object

      public :: ESMF_CFIOGridCreate      ! constructor
      public :: ESMF_CFIOGridSet         ! set a CFIO grid
      public :: ESMF_CFIOGridGet         ! get a CFIO grid
      public :: ESMF_CFIOGridDestroy     ! destructor
!
!EOP
!------------------------------------------------------------------------------
! Define a new data type "CFIO_Grid" -- contains grid information

      type ESMF_CFIOGrid
!         private
         character(len=MVARLEN) :: gName ! name for this grid
         integer :: im              ! size of longitudinal dimension
         integer :: jm              ! size of latitudinal  dimension
         integer :: km              ! size of vertical dimension
         integer :: tm              ! size of time dimension
         real*8, pointer :: lon(:) => NULL()    ! longitude of center of gridbox in
                                    ! degrees east of Greenwich (can be
                                    ! -180 -> 180 or 0 -> 360)
         real*8, pointer :: lat(:) => NULL()    ! latitude of center of gridbox in
                                    ! degrees north of equator
         real*8, pointer :: lev(:) => NULL()    ! Level (units given by levUnits) of
                                    ! center of gridbox
         character(len=MLEN) :: levUnits   ! units of level dimension, e.g.,
                                           ! "hPa", "sigma_level"
         character(len=MLEN) :: coordinate ! string to indicate vertical coord
                                           ! (pressure, sigma, pressure_sigma)
         character(len=MLEN) :: standardName ! string for CF standard name
         character(len=MLEN) :: formulaTerm  ! string for CF formula terms
         real, pointer :: ak(:) => NULL()     ! parameter for hybrid sigma prs coord.
         real, pointer :: bk(:) => NULL()     ! parameter for hybrid sigma prs coord.
         real, pointer :: sigma(:) => NULL()  ! parameter for sigma coordinate
         real :: ptop               ! parameter for sigma/eta coordinate
         character(len=MVARLEN) :: ptopUnit !  unit of ptop
         logical :: twoDimLat       ! support 2D lat/lon or not
         logical :: reduceGrid      ! support for reduced grid
         logical :: stnGrid         ! support for station data
      end type ESMF_CFIOGrid
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------!BOP
! !ROUTINE: ESMF_CFIOGridCreate -- ESMF_Grid object constructor

! !INTERFACE:
      type(ESMF_CFIOGrid) function ESMF_CFIOGridCreate (gName, rc)   
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      character(len=*), intent(in), OPTIONAL :: gName  ! grid object name
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
!
! !DESCRIPTION:
!     Create a CFIO grid object and initialize vars
!EOP
!------------------------------------------------------------------------------
      type(ESMF_CFIOGrid) :: grid               ! a CFIO grid object

      grid%im = 0
      grid%jm = 0
      grid%km = 0
      grid%tm = 0

      grid%levUnits = 'unknown'
      grid%coordinate = 'unknown'
      grid%standardName = 'unknown'
      grid%formulaTerm = 'unknown'

      grid%ptop = 0
      grid%ptopUnit = 'Pa' 
      grid%twoDimLat = .false.
      grid%reduceGrid = .false.
      grid%stnGrid = .false.

      nullify(grid%lon)
      nullify(grid%lat)
      nullify(grid%lev)
      nullify(grid%ak)
      nullify(grid%bk)
      nullify(grid%sigma)

      if ( present(gName) ) grid%gName = gName

      if ( present(rc) ) rc = 0

      ESMF_CFIOGridCreate = grid

      end function ESMF_CFIOGridCreate



!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGridSet -- set up a grid

! !INTERFACE:
      subroutine ESMF_CFIOGridSet (grid, gName, im, jm, km, tm, lat, lon, lev,&
                                   coordinate, standardName, formulaTerm,   &
                                   levUnit, ak, bk, sigma, ptop, ptopUnit,  &
                                   twoDimLat, reduceGrid, stnGrid, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      character(len=*), intent(in), OPTIONAL :: gName  ! grid name
      integer, intent(in), OPTIONAL :: im  ! size of longitudinal dimension
      integer, intent(in), OPTIONAL :: jm  ! size of latitudinal  dimension
      integer, intent(in), OPTIONAL :: km  ! size of vertical dimension
      integer, intent(in), OPTIONAL :: tm  ! size of time dimension
      real, intent(in), OPTIONAL :: lon(:) ! longitude 
      real, intent(in), OPTIONAL :: lat(:) ! latitude 
      real, intent(in), OPTIONAL :: lev(:) ! Level   
      character(len=*), intent(in), OPTIONAL :: levUnit   
                                 ! units of level dimension, e.g., "hPa".
      character(len=*), intent(in), OPTIONAL :: coordinate
                                 ! string to indicate vertical coord
                                 ! (pressure, sigma, pressure_sigma)
      character(len=*), intent(in), OPTIONAL :: standardName 
                                 ! string for standard name
      character(len=*), intent(in), OPTIONAL :: formulaTerm  
                                 ! formula terms
      real, intent(in), OPTIONAL :: ak(:)     
                                 ! parameter for hybrid sigma prs coord.
      real, intent(in), OPTIONAL :: bk(:)     
                                 ! parameter for hybrid sigma prs coord.
      real, intent(in), OPTIONAL :: sigma(:)  
                                 ! parameter for sigma coordinate
      real, intent(in), OPTIONAL :: ptop              
                                 ! parameter for sigma coordinate
      character(len=*), intent(in), OPTIONAL :: ptopUnit   
                                 ! unit of ptop
      logical, intent(in), OPTIONAL :: twoDimLat       
                                 ! support 2D lat/lon or not
      logical, intent(in), OPTIONAL :: reduceGrid      
                                 ! support for reduced grid 
      logical, intent(in), OPTIONAL :: stnGrid      
                                 ! support for statio grid 

! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc ! Error return code:
                                           ! 0   all is well
                                           ! -1  Problem in setting lon
                                           ! -2  Problem in setting lat
                                           ! -3  Problem in setting lev
! !INPUT/OUTPUT PARAMETERS:
!
      type(ESMF_CFIOGrid), intent(inout) :: grid   ! CFIO grid   
!
! !DESCRIPTION:
!     Initializing a CFIO grid
!EOP
!------------------------------------------------------------------------------
       integer :: rtcode  = 0
       integer :: i, j
       integer :: sz

       if ( present(gName) ) grid%gName = gName
       if ( present(im) ) grid%im = im
       if ( present(jm) ) grid%jm = jm
       if ( present(km) ) grid%km = km
       if ( present(tm) ) grid%tm = tm

       if (present(twoDimLat)) then
          grid%twoDimLat = twoDimLat
          if (.not. (present(im) .or. present(jm) .or. &
                     present(lon) .or. present(lat))) then
             rtcode = -1
          else
             sz = im*jm
             allocate(grid%lon(sz), grid%lat(sz), stat = rtcode)
             grid%lon = lon
             grid%lat = lat
          end if
          if (rtcode .ne. 0) then 
             print *, "problem in setting ESMF_CFIOGridSet:lat"
             rtcode = -2
             if ( present(rc) ) rc = rtcode
             return
          end if
       else
          if ( present(lon) ) then
             grid%im = size(lon)
             allocate(grid%lon(grid%im), stat = rtcode)
             grid%lon = lon
          else if ( present(im) ) then
             allocate(grid%lon(im), stat = rtcode)
             do i = 1, im
                grid%lon(i) = 360./im * (i-1)
             end do
          end if
          if (rtcode .ne. 0) then 
             print *, "problem in setting ESMF_CFIOGridSet:lon"
             rtcode = -1
             if ( present(rc) ) rc = rtcode
             return
          end if

          if ( present(lat) ) then
             grid%jm = size(lat)
             allocate(grid%lat(grid%jm), stat = rtcode)
             grid%lat = lat
          else if ( present(jm) ) then
             allocate(grid%lat(jm), stat = rtcode)
             do j = 1, jm
                grid%lat(j) = 180./(jm-1) * (j-1) - 90
             end do
          end if
          if (rtcode .ne. 0) then 
             print *, "problem in setting ESMF_CFIOGridSet:lat"
             rtcode = -2
             if ( present(rc) ) rc = rtcode
             return
          end if
       end if ! if (usableTwoDimLat)...

        if ( present(lev) ) then
           grid%km = size(lev)
           allocate(grid%lev(grid%km), stat = rtcode)
           grid%lev = lev
           if (rtcode .ne. 0) then 
           print *, "problem in setting ESMF_CFIOGridSet:lev"
              rtcode = -3
              if ( present(rc) ) rc = rtcode
              return
           end if
        end if    

        if ( present(levUnit) ) grid%levUnits = levUnit

       if ( present(coordinate) ) grid%coordinate = coordinate
       if ( present(standardName) ) grid%standardName = standardName
       if ( present(formulaTerm) ) grid%formulaTerm = formulaTerm
       if ( present(ak) ) then
          allocate(grid%ak(size(ak)), stat=rtcode)
          grid%ak = ak 
       end if
       if ( present(bk) ) then
          allocate(grid%bk(size(bk)), stat=rtcode)
          grid%bk = bk 
       end if
       if ( present(ptop) ) then
          grid%ptop = ptop
       end if
       if ( present(ptopUnit) ) grid%ptopUnit = ptopUnit

       if ( present(stnGrid) ) then
          grid%stnGrid = stnGrid
       end if

       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOGridSet


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGridGet -- get grid info 

! !INTERFACE:
      subroutine ESMF_CFIOGridGet (grid, gName, im, jm, km, tm, lat, lon, lev,&
                                   coordinate, standardName, formulaTerm,   &
                                   levUnit, ak, bk, sigma, ptop, twoDimLat, &
                                   reduceGrid, stnGrid, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIOGrid), intent(in) :: grid   ! CFIO grid

! !OUTPUT PARAMETERS:
!
      character(len=*), intent(out), OPTIONAL :: gName  ! grid name
      integer, intent(out), OPTIONAL :: im  ! size of longitudinal dimension
      integer, intent(out), OPTIONAL :: jm  ! size of latitudinal  dimension
      integer, intent(out), OPTIONAL :: km  ! size of vertical dimension
      integer, intent(out), OPTIONAL :: tm  ! size of time dimension
      real, pointer, OPTIONAL :: lon(:) ! longitude
      real, pointer, OPTIONAL :: lat(:) ! latitude
      real, pointer, OPTIONAL :: lev(:) ! Level
      character(len=*), intent(out), OPTIONAL :: levUnit
                                 ! units of level dimension, e.g., "hPa".
      character(len=*), intent(out), OPTIONAL :: coordinate
                                 ! string to indicate vertical coord
                                 ! (pressure, sigma, pressure_sigma)
      character(len=*), intent(out), OPTIONAL :: standardName 
                                 ! string for standard name
      character(len=*), intent(out), OPTIONAL :: formulaTerm  
                                 ! formula terms
      real, intent(out), OPTIONAL :: ak(:)
                                 ! parameter for hybrid sigma prs coord.
      real, intent(out), OPTIONAL :: bk(:)
                                 ! parameter for hybrid sigma prs coord.
      real, intent(out), OPTIONAL :: sigma(:)
                                 ! parameter for sigma coordinate
      real, intent(out), OPTIONAL :: ptop
                                 ! parameter for sigma coordinate
      logical, intent(out), OPTIONAL :: twoDimLat
                                 ! support 2D lat/lon or not
      logical, intent(out), OPTIONAL :: reduceGrid
                                 ! support for reduced grid 
      logical, intent(out), OPTIONAL :: stnGrid
                                 ! support for station grid 
      integer, intent(out), OPTIONAL :: rc ! Error return code:
                                           ! 0   all is well
                                           ! -1  problem in getting lon
                                           ! -2  problem in getting lat
                                           ! -3  problem in getting lev
!
! !DESCRIPTION:
!     Get grid info
!EOP
!------------------------------------------------------------------------------
       integer :: rtcode = 0
                                                                                     
       if ( present(gName) ) gName = grid%gName
       if ( present(im) ) im = grid%im
       if ( present(jm) ) jm = grid%jm
       if ( present(km) ) km = grid%km 
       if ( present(tm) ) tm = grid%tm
                                                                                     
        if ( present(lon) ) then
           allocate(lon(size(grid%lon)), stat=rtcode)
           lon = grid%lon
           if (rtcode .ne. 0) then 
              print *, "problem in getting ESMF_CFIOGridGet:lon"
              rtcode = -1
              if ( present(rc) ) rc = rtcode
              return
           end if
        end if
        if ( present(lat) ) then
           allocate(lat(size(grid%lat)), stat=rtcode)
           lat = grid%lat
           if (rtcode .ne. 0) then 
              print *, "problem in getting ESMF_CFIOGridGet:lat"
              rtcode = -2
              if ( present(rc) ) rc = rtcode
              return
           end if
        end if
        if ( present(lev) ) then
           allocate(lev(size(grid%lev)), stat=rtcode)
           if (rtcode .ne. 0) then 
              print *, "problem in getting ESMF_CFIOGridGet:lev"
              rtcode = -3
              if ( present(rc) ) rc = rtcode
              return
           end if
           lev = grid%lev
        end if
         
        if ( present(ak) ) then
           if ( associated(grid%ak) ) then
              ak = grid%ak
           else
              print *, "ak was not defined in the input file"
           end if
        end if
        if ( present(bk) ) then
           if ( associated(grid%bk) ) then
              bk = grid%bk
           else
              print *, "bk was not defined in the input file"
           end if
        end if
        if ( present(sigma) ) then
           if ( associated(grid%sigma) ) then
              sigma = grid%sigma
           else
              print *, "sigam was not defined in the input file"
           end if
        end if
        if ( present(ptop) ) then
           ptop = grid%ptop
        end if

        if ( present(twoDimLat) ) then
           twoDimLat = grid%twoDimLat
        end if
        if ( present(reduceGrid) ) then
           reduceGrid = grid%reduceGrid
        end if
         
        if ( present(standardName) ) standardName = grid%standardName
        if ( present(coordinate) ) coordinate = grid%coordinate
        if ( present(formulaTerm) ) formulaTerm = grid%formulaTerm
        if ( present(levUnit) ) levUnit = grid%levUnits 
        if ( present(stnGrid) ) stnGrid = grid%stnGrid  

        if ( present(rc) ) rc = rtcode


      end subroutine ESMF_CFIOGridGet

!------------------------------------------------------------------------------!BOP
! !ROUTINE: ESMF_CFIOGridDestroy -- destructor for a CFIO grid object

! !INTERFACE:
      subroutine ESMF_CFIOGridDestroy (grid, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(ESMF_CFIOGrid), intent(inout) :: grid ! CFIOGrid object


!
! !DESCRIPTION:
!     destructor for a CFIO grid object
!EOP
!------------------------------------------------------------------------------
      integer :: rtcode = 0

      if ( associated(grid%lon) ) deallocate(grid%lon, stat=rtcode)
      if ( associated(grid%lat) ) deallocate(grid%lat, stat=rtcode)
      if ( associated(grid%lev) ) deallocate(grid%lev, stat=rtcode)

      if ( associated(grid%ak) ) deallocate(grid%ak, stat=rtcode)
      if ( associated(grid%bk) ) deallocate(grid%bk, stat=rtcode)
      if ( associated(grid%sigma) ) deallocate(grid%sigma, stat=rtcode)

      if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOGridDestroy

      end module ESMF_CFIOGridMod
