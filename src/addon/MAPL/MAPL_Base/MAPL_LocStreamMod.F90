! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 
!  $Id: MAPL_LocStreamMod.F90,v 1.56.12.2.2.4 2014-02-15 16:34:25 atrayano Exp $

#include "MAPL_ErrLog.h"

#define DEALOC_(A) if(associated(A)) then;A=0;call MAPL_DeAllocNodeArray(A,rc=STATUS);if(STATUS==MAPL_NoShm) deallocate(A,stat=STATUS);VERIFY_(STATUS);NULLIFY(A);endif

module MAPL_LocStreamMod

!=============================================================================
  !BOP

  ! !MODULE: MAPL_LocStreamMod -- Manipulate location streams


  ! !USES:

use ESMF
use ESMFL_Mod
use MAPL_BaseMod
use MAPL_ConstantsMod
use MAPL_IOMod
use MAPL_CommsMod
use MAPL_HashMod
use MAPL_ShmemMod

implicit none
private

! !PUBLIC MEMBER FUNCTIONS:

public MAPL_LocStreamCreate
public MAPL_LocStreamAdjustNsubtiles
public MAPL_LocStreamTransform
public MAPL_LocStreamIsAssociated
public MAPL_LocStreamXformIsAssociated
public MAPL_LocStreamGet
public MAPL_LocStreamCreateXform
public MAPL_LocStreamFracArea
public MAPL_GridCoordAdjust
public MAPL_LocStreamTileWeight

#undef DO_NOT_USE_FCOLLECT
#if defined(TWO_SIDED_COMM) || defined(ONE_SIDED_COMM)
#define DO_NOT_USE_FCOLLECT
#endif

INCLUDE 'mpif.h'

! !PUBLIC TYPES:

type, public :: MAPL_LocStream
  private
  type(MAPL_LocStreamType), pointer :: Ptr=>null()
end type MAPL_LocStream

type, public :: MAPL_LocStreamXform
  private
  type(MAPL_LocStreamXformType), pointer :: Ptr=>null()
end type MAPL_LocStreamXform

!EOP

integer, parameter :: NumGlobalVars=4
integer, parameter :: NumLocalVars =4


type MAPL_GeoLocation
   integer                    :: T ! 1st Type designation
   real                       :: A ! Stream area
   real                       :: X ! Stream coordinate
   real                       :: Y ! Stream coordinate
end type MAPL_GeoLocation

type MAPL_IndexLocation
   integer                    :: I ! Global index into associated grid
   integer                    :: J ! Global index into associated grid
   real                       :: W ! Weight at I J
end type MAPL_IndexLocation

type MAPL_Tiling
   character(len=ESMF_MAXSTR)          :: NAME=""
   integer                             :: IM=0
   integer                             :: JM=0
!!   type(MAPL_IndexLocation),  pointer  :: Global_IndexLocation(:)=>null() ! Locations in local PE
end type MAPL_Tiling

type MAPL_LocStreamType
   character(len=ESMF_MAXSTR)         :: ROOTNAME=""
   character(len=ESMF_MAXSTR)         :: NAME=""
   integer                            :: NT_GLOBAL=0                     ! Total number locations
   integer                            :: NT_LOCAL=0                      ! Number locations on local PE
   integer                            :: N_GRIDS=0                       ! Number of associated grids
   integer                            :: Current_tiling=-1               ! Grid tiling currently attached 
   type(ESMF_GRID)                    :: GRID                            ! Grid currently attached
   type(ESMF_GRID)                    :: TILEGRID                        ! the next best thing to LocStream grid
   integer,                  pointer  :: GLOBAL_Id(:)           =>null() ! All Location Ids in file order
   integer,                  pointer  :: LOCAL_Id (:)           =>null() ! Location Ids on local PE
   type(MAPL_GeoLocation),   pointer  :: Global_GeoLocation  (:)=>null() ! All GeoLocations
!!   type(MAPL_IndexLocation), pointer  :: Global_IndexLocation(:)=>null() ! All IndexLocations for attach grid
   type(MAPL_GeoLocation),   pointer  :: Local_GeoLocation   (:)=>null() ! GeoLocations on local PE
   type(MAPL_IndexLocation), pointer  :: Local_IndexLocation (:)=>null() ! Local IndexLocations for attach grid
   type(MAPL_Tiling),        pointer  :: Tiling(:)              =>null() ! Grid associated tilings
   real, pointer              :: D(:,:,:)=>null() ! Bilinear weights
   logical                            :: IsTileAreaValid
end type MAPL_LocStreamType

type MAPL_LocStreamXformType
   character(len=ESMF_MAXSTR)         :: NAME=""
   type(MAPL_LocStream)               :: InputStream
   type(MAPL_LocStream)               :: OutputStream
   integer                  ,pointer  :: IndexIn (:)=>null()
   integer                  ,pointer  :: IndexOut(:)=>null()

   logical                            :: do_not_use_fcollect
   integer                  ,pointer  :: Len(:)=>null()
   integer                  ,pointer  :: Senders(:)=>null()
#if defined(TWO_SIDED_COMM)
   integer                  ,pointer  :: Receivers(:)=>null()
#elif defined(ONE_SIDED_COMM)
   real                     ,pointer  :: Buff(:)
   integer                            :: window
#endif
   integer                            :: InputLen, Comm, myId

   integer                            :: Count
   integer                            :: LastLocal
   logical                            :: Local
end type MAPL_LocStreamXformType

! Overloads
!----------

interface MAPL_LocStreamCreate
   module procedure MAPL_LocStreamCreateFromFile
   module procedure MAPL_LocStreamCreateFromStream
end interface

interface MAPL_LocStreamTransform
   module procedure MAPL_LocStreamTransformField
   module procedure MAPL_LocStreamTransformT2G
   module procedure MAPL_LocStreamTransformG2T
   module procedure MAPL_LocStreamTransformT2T
   module procedure MAPL_LocStreamTransformT2TR4R8
   module procedure MAPL_LocStreamTransformT2TR8R4
end interface

contains

!===================================================================

  logical function MAPL_LocStreamIsAssociated(LocStream, RC)
    type(MAPL_LocStream),                 intent(IN   ) :: LocStream
    integer, optional,                    intent(  OUT) :: RC  
    
    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamIsAssocited'
    integer                    :: STATUS

    MAPL_LocStreamIsAssociated = associated(LocStream%Ptr)

    RETURN_(ESMF_SUCCESS)
  end function MAPL_LocStreamIsAssociated

!===================================================================

  logical function MAPL_LocStreamXformIsAssociated(Xform, RC)
    type(MAPL_LocStreamXform),            intent(IN   ) :: Xform
    integer, optional,                    intent(  OUT) :: RC  
    
    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamXformIsAssocited'
    integer                    :: STATUS

    MAPL_LocStreamXformIsAssociated = associated(Xform%Ptr)

    RETURN_(ESMF_SUCCESS)
  end function MAPL_LocStreamXformIsAssociated

!===================================================================


  subroutine MAPL_LocStreamGet(LocStream, NT_LOCAL, TILETYPE, TILEKIND, &
                               TILELONS, TILELATS, TILEAREA, &
!                              TILEI, TILEJ, TILEGRID, &
                               TILEGRID, &
                               GRIDIM, GRIDJM, GRIDNAMES, &
                               ATTACHEDGRID, LOCAL_ID, RC)
    type(MAPL_LocStream),                 intent(IN   ) :: LocStream
    integer, optional,                    intent(  OUT) :: NT_LOCAL  
    integer, optional,                    pointer       :: TILETYPE(:)
    integer, optional,                    pointer       :: TILEKIND(:)
    real   , optional,                    pointer       :: TILELONS(:)
    real   , optional,                    pointer       :: TILELATS(:)
    real   , optional,                    pointer       :: TILEAREA(:)
!    integer, optional,                    pointer       :: TILEI(:)
!    integer, optional,                    pointer       :: TILEJ(:)
    integer, optional,                    pointer       :: GRIDIM(:)
    integer, optional,                    pointer       :: GRIDJM(:)
    integer, optional,                    pointer       :: LOCAL_ID(:)
    character(len=ESMF_MAXSTR), optional, pointer       :: GRIDNAMES(:)
    type(ESMF_Grid), optional,            intent(  OUT) :: TILEGRID
    type(ESMF_Grid), optional,            intent(  OUT) :: ATTACHEDGRID
    integer, optional,                    intent(  OUT) :: RC  
    
! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamGet'
    integer                    :: STATUS
    integer                    :: n_ele, i
    integer, pointer           :: tmp_iptr(:) => null()
    real,    pointer           :: tmp_rptr(:) => null()
    character(len=ESMF_MAXSTR), pointer       :: tmp_strptr(:) => null()

    if (present(NT_LOCAL)) then
       NT_LOCAL = locstream%Ptr%NT_LOCAL
    end if

    if (present(tiletype)) then
#ifdef GFORTRAN
       allocate(tmp_iptr(lbound(locstream%Ptr%Local_GeoLocation,1):ubound(locstream%Ptr%Local_GeoLocation,1)))
       do i = lbound(locstream%Ptr%Local_GeoLocation,1), ubound(locstream%Ptr%Local_GeoLocation,1)
         tmp_iptr(i) = locstream%Ptr%Local_GeoLocation(i)%t
       enddo
       tiletype => tmp_iptr
#else
       tiletype => locstream%Ptr%Local_GeoLocation(:)%t
#endif
    end if

    if (present(tilekind)) then
       PRINT *, 'IN LocStreamGet TILEKIND  NO LONGER VALID ARGUMENT'
       ASSERT_(.false.)
!       tilekind => locstream%Ptr%Local_GeoLocation(:)%u
    end if

    if (present(tilelons)) then
#ifdef GFORTRAN
       allocate(tmp_rptr(lbound(locstream%Ptr%Local_GeoLocation,1):ubound(locstream%Ptr%Local_GeoLocation,1)))
       do i = lbound(locstream%Ptr%Local_GeoLocation,1), ubound(locstream%Ptr%Local_GeoLocation,1)
         tmp_rptr(i) = locstream%Ptr%Local_GeoLocation(i)%x
       enddo
       tilelons => tmp_rptr
#else
       tilelons => locstream%Ptr%Local_GeoLocation(:)%x
#endif
    end if

    if (present(tilelats)) then
#ifdef GFORTRAN
       allocate(tmp_rptr(lbound(locstream%Ptr%Local_GeoLocation,1):ubound(locstream%Ptr%Local_GeoLocation,1)))
       do i = lbound(locstream%Ptr%Local_GeoLocation,1), ubound(locstream%Ptr%Local_GeoLocation,1)
         tmp_rptr(i) = locstream%Ptr%Local_GeoLocation(i)%y
       enddo
       tilelats => tmp_rptr
#else
       tilelats => locstream%Ptr%Local_GeoLocation(:)%y
#endif
    end if

    if (present(tilearea)) then
       if (locstream%Ptr%IsTileAreaValid) then
#ifdef GFORTRAN
          allocate(tmp_rptr(lbound(locstream%Ptr%Local_GeoLocation,1):ubound(locstream%Ptr%Local_GeoLocation,1)))
          do i = lbound(locstream%Ptr%Local_GeoLocation,1), ubound(locstream%Ptr%Local_GeoLocation,1)
            tmp_rptr(i) = locstream%Ptr%Local_GeoLocation(i)%a
          enddo
          tilearea => tmp_rptr
#else
          tilearea => locstream%Ptr%Local_GeoLocation(:)%a
#endif
       else
          tilearea => null()
       end if
    end if

    if (present(gridim)) then
#ifdef GFORTRAN
       allocate(tmp_iptr(lbound(locstream%Ptr%tiling,1):ubound(locstream%Ptr%tiling,1)))
       do i = lbound(locstream%Ptr%tiling,1), ubound(locstream%Ptr%tiling,1)
         tmp_iptr(i) = locstream%Ptr%tiling(i)%im
       enddo
       gridim => tmp_iptr
#else
       gridim => locstream%Ptr%tiling(:)%im
#endif
    end if

    if (present(gridjm)) then
#ifdef GFORTRAN
       allocate(tmp_iptr(lbound(locstream%Ptr%tiling,1):ubound(locstream%Ptr%tiling,1)))
       do i = lbound(locstream%Ptr%tiling,1), ubound(locstream%Ptr%tiling,1)
         tmp_iptr(i) = locstream%Ptr%tiling(i)%jm
       enddo
       gridjm => tmp_iptr
#else
       gridjm => locstream%Ptr%tiling(:)%jm
#endif
    end if

    if (present(local_id)) then
       local_id => locstream%Ptr%local_id
    end if

    if (present(gridnames)) then
#ifdef GFORTRAN
       allocate(tmp_strptr(lbound(locstream%Ptr%tiling,1):ubound(locstream%Ptr%tiling,1)))
       do i = lbound(locstream%Ptr%tiling,1), ubound(locstream%Ptr%tiling,1)
         tmp_strptr(i) = locstream%Ptr%tiling(i)%name
       enddo
       gridnames => tmp_strptr
#else
       gridnames => locstream%Ptr%tiling(:)%name
#endif
    end if

    if (present(attachedgrid)) then
       attachedgrid = locstream%Ptr%grid
    end if

!!$    if (present(tilei)) then
!!$       tilei => locstream%Ptr%TILING(locstream%ptr%CURRENT_TILING)%Global_IndexLocation(:)%i
!!$    end if
!!$
!!$    if (present(tilej)) then
!!$       tilej => locstream%Ptr%TILING(locstream%ptr%CURRENT_TILING)%Global_IndexLocation(:)%j
!!$    end if

    if (present(tilegrid)) then
       tilegrid = locstream%Ptr%TILEGRID
    end if

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_LocStreamGet

!===================================================================


!BOPI
! !IROUTINE: MAPL_LocStreamCreate
! !IIROUTINE: MAPL_LocStreamCreateFromFile --- Create from file

  ! !INTERFACE:
  subroutine MAPL_LocStreamCreateFromFile(LocStream, LAYOUT, FILENAME, NAME, MASK, GRID, NewGridNames, RC)

    !ARGUMENTS:
    type(MAPL_LocStream),                 intent(  OUT) :: LocStream
    type(ESMF_DELayout),                  intent(IN   ) :: LAYOUT
    character(len=*),                     intent(IN   ) :: FILENAME
    character(len=*),                     intent(IN   ) :: NAME
    integer,                    optional, intent(IN   ) :: MASK(:)
    type(ESMF_Grid), optional,            intent(INout) :: GRID
    logical,                    optional, intent(IN   ) :: NewGridNames
    integer,                    optional, intent(  OUT) :: RC  

! !DESCRIPTION: Creates a location stream from a file. This does
! not decompose the location stream; so the global stream is
! described in each processor.  The stream can be decomposed
! later in various ways. Currently we only decompose it by 
! "attaching" it to a decomposed grid.
!EOPI

! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamCreateFromFile'
    integer                    :: STATUS

    integer                           :: UNIT
    integer                           :: N, I, K, L, NT
    type(MAPL_LocStreamType), pointer :: STREAM
    real,    pointer                  :: AVR(:,:)
    logical, pointer                  :: MSK(:)
    real                              :: X, Y, X0, Y0, XE, DX, DY
    integer                           :: II, JJ
    logical                           :: found
    logical                           :: DoCoeffs
    character(len=ESMF_MAXSTR)        :: gname

    integer                           :: irec
    integer(kind=1)                   :: byte(4)
    integer                           :: I1, IN, J1, JN
    integer                           :: iostat
    logical                           :: isascii
    logical                           :: read_always
    logical, pointer                  :: ISMINE(:)
    type(MAPL_Tiling       ), pointer :: TILING
    type (ESMF_VM)                            :: vm
    logical                           :: NewGridNames_

! Begin
!------

    NewGridNames_ = .false.
    if (present(NewGridNames)) then
       NewGridNames_ = NewGridNames
    end if

! Allocate the Location Stream
!-----------------------------

    call ESMF_VMGetCurrent(vm, rc=status)

    LocStream%Ptr => null()
    allocate(LocStream%Ptr, STAT=STATUS)
    VERIFY_(STATUS)

    STREAM => LocStream%Ptr

! Use the filename as identifier. NAME is thus the 
! same for all streams made from this file
!-------------------------------------------------

    STREAM%NAME     = NAME
    STREAM%ROOTNAME = FILENAME

! Use some heuristics to determine filetype (choices are BINARY and ASCII)
!------------------------------------------------------------------------
    ! just get the unit
    UNIT = GETFILE(FILENAME, DO_OPEN=0, ALL_PES=.true., RC=STATUS)
    VERIFY_(STATUS)

    INQUIRE(IOLENGTH=IREC) BYTE
    open (UNIT=UNIT, FILE=FILENAME, FORM='unformatted', ACCESS='DIRECT', RECL=IREC, IOSTAT=status)
    VERIFY_(STATUS)
    read (UNIT, REC=1, ERR=100) BYTE
    call FREE_FILE(UNIT)

    isascii = .true.
    do i=1,size(byte)
       if (BYTE(I) < 7) then
          isascii = .false.
          exit
       end if
    end do

    if (isascii) then
! Open file and read header info
!-------------------------------

       UNIT = GETFILE(FILENAME, form='FORMATTED', RC=status)
       VERIFY_(STATUS)

! Total number of tiles in exchange grid
!---------------------------------------

       call READ_PARALLEL(layout, NT, UNIT=UNIT, rc=status)
       VERIFY_(STATUS)

! Number of grids that can be attached
!-------------------------------------

       call READ_PARALLEL(layout, STREAM%N_GRIDS, unit=UNIT, rc=status)
       VERIFY_(STATUS)

! The exchange grid is used to tile each attached grid
!-----------------------------------------------------

       allocate(STREAM%TILING(STREAM%N_GRIDS), STAT=STATUS)
       VERIFY_(STATUS)

! The names and sizes of the grids to be tiled
!---------------------------------------------

       do N=1,STREAM%N_GRIDS
          call READ_PARALLEL(layout, STREAM%TILING(N)%NAME, unit=UNIT, rc=status)
          VERIFY_(STATUS)
          if (NewGridNames_) then
             call GenOldGridName_(STREAM%TILING(N)%NAME)
          end if
          call READ_PARALLEL(layout, STREAM%TILING(N)%IM, unit=UNIT, rc=status)
          VERIFY_(STATUS)
          call READ_PARALLEL(layout, STREAM%TILING(N)%JM, unit=UNIT, rc=status)
          VERIFY_(STATUS)
       enddo


! Read location stream file into AVR
!---------------------------------------

       allocate(AVR(NT,NumGlobalVars+NumLocalVars*STREAM%N_GRIDS), STAT=STATUS)
       VERIFY_(STATUS)

       do I=1, NT
          call READ_PARALLEL(layout, AVR(I,:), unit=UNIT, rc=status)
          VERIFY_(STATUS)
       end do

       call FREE_FILE(UNIT)

! Allocate msk for which tiles to include in the stream being created.
!--------------------------------------------------------------------

       allocate(MSK(NT), STAT=STATUS)
       VERIFY_(STATUS)

! We include any tile whose type matches any element of the mask
!---------------------------------------------------------------

       if(present(MASK)) then
          do N=1,NT
             if (nint(AVR(N,1)) < 0) AVR(N,1) = MAPL_Ocean
             MSK(N) = any(nint(AVR(N,1))==MASK(:))
          end do
       else
          MSK = .true.
       end if

! The number of tiles in the new stream
!--------------------------------------

       STREAM%NT_GLOBAL = count(MSK)

       if (present(GRID)) then
          allocate(ISMINE(STREAM%NT_GLOBAL), STAT=STATUS)
          VERIFY_(STATUS)
          ISMINE = .false.
          call ESMF_GRID_INTERIOR  (GRID, I1,IN,J1,JN)
          call ESMF_GridGet(grid, name=gname, rc=status)
          VERIFY_(STATUS)
          read_always = .false.
       else
          gname = ""
          read_always = .true.
       end if

! Fill ISMINE (Pick off local tiles)
!------------------------------------
       if (present(GRID)) then
          do N=1,STREAM%N_GRIDS
             if (gname == STREAM%TILING(N)%NAME) then
                K = 0
                do I=1, NT
                   if(MSK(I)) then
                      K = K + 1
                      II = nint(AVR(I,NumGlobalVars+1+NumLocalVars*(N-1)))
                      JJ = nint(AVR(I,NumGlobalVars+2+NumLocalVars*(N-1)))
                      ISMINE(K) = I1<=II .and. IN>=II .and. &
                                  J1<=JJ .and. JN>=JJ
                   endif
                end do
             endif
          end do
          STREAM%NT_LOCAL = count(ISMINE)
          allocate(STREAM%LOCAL_IndexLocation(STREAM%NT_LOCAL), STAT=STATUS)
          VERIFY_(STATUS)

          allocate(STREAM%LOCAL_IndexLocation(STREAM%NT_LOCAL), STAT=STATUS)
          VERIFY_(STATUS)
          do N=1,STREAM%N_GRIDS
             if (gname == STREAM%TILING(N)%NAME) then
                K = 0
                L = 0
                do I=1, NT
                   if(MSK(I)) then
                      K = K + 1
                      if (ISMINE(K)) then
                         L = L + 1
                         II = nint(AVR(I,NumGlobalVars+1+NumLocalVars*(N-1)))
                         JJ = nint(AVR(I,NumGlobalVars+2+NumLocalVars*(N-1)))
                         STREAM%LOCAL_IndexLocation(L)%I = II
                         STREAM%LOCAL_IndexLocation(L)%J = JJ
                         STREAM%LOCAL_IndexLocation(L)%W = AVR(I,NumGlobalVars+3+NumLocalVars*(N-1))
                      end if
                   end if
                end do
             endif
          end do
       end if

! Allocate space for global versions of stream parameters
!--------------------------------------------------------

       allocate(STREAM%GLOBAL_ID         (STREAM%NT_GLOBAL), STAT=STATUS)
       VERIFY_(STATUS)
       allocate(STREAM%GLOBAL_GEOLOCATION(STREAM%NT_GLOBAL), STAT=STATUS)
       VERIFY_(STATUS)

!!       do N=1,STREAM%N_GRIDS
!!          allocate(STREAM%TILING(N)%GLOBAL_IndexLocation(STREAM%NT_GLOBAL), STAT=STATUS)
!!          VERIFY_(STATUS)
!!       end do

! Fill global stream parameters subject to mask
!----------------------------------------------

       K = 0
       do I=1, NT
          if(MSK(I)) then
             K = K + 1
             STREAM%GLOBAL_ID         (K)   = I
             STREAM%GLOBAL_GeoLocation(K)%T = nint(AVR(I,1))
             !if (STREAM%GLOBAL_GeoLocation(K)%T < 0)  STREAM%GLOBAL_GeoLocation(K)%T = MAPL_Ocean
             STREAM%GLOBAL_GeoLocation(K)%A =      AVR(I,2)
             STREAM%GLOBAL_GeoLocation(K)%X =      AVR(I,3) * (MAPL_PI/180.)
             STREAM%GLOBAL_GeoLocation(K)%Y =      AVR(I,4) * (MAPL_PI/180.)
!!             X = AVR(I,3)
!!             Y = AVR(I,4)
!!             do N=1,STREAM%N_GRIDS
!!                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%I = nint(AVR(I,NumGlobalVars+1+NumLocalVars*(N-1)))
!!                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%J = nint(AVR(I,NumGlobalVars+2+NumLocalVars*(N-1)))
!!                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%W =      AVR(I,NumGlobalVars+3+NumLocalVars*(N-1))
!!             end do
          end if
       end do

       STREAM%IsTileAreaValid = .true.

       deallocate(MSK)
       deallocate(AVR)
    else
       ! BINARY
! Open file and read header info
!-------------------------------

       UNIT = GETFILE(FILENAME, form='UNFORMATTED', RC=status)
       VERIFY_(STATUS)

! Total number of tiles in exchange grid
!---------------------------------------
       if ( MAPL_am_I_root() ) read(UNIT) NT
       call MAPL_CommsBcast(vm, DATA=NT, N=1, ROOT=0, RC=status)

! Number of grids that can be attached
!-------------------------------------

       if ( MAPL_am_I_root() ) read(UNIT) STREAM%N_GRIDS
       call MAPL_CommsBcast(vm, DATA=STREAM%N_GRIDS, N=1, ROOT=0, RC=status)

! The exchange grid is used to tile each attached grid
!-----------------------------------------------------

       allocate(STREAM%TILING(STREAM%N_GRIDS), STAT=STATUS)
       VERIFY_(STATUS)

! The names and sizes of the grids to be tiled
!---------------------------------------------

       do N=1,STREAM%N_GRIDS
          if ( MAPL_am_I_root() ) then
            read(UNIT) STREAM%TILING(N)%NAME
            read(UNIT) STREAM%TILING(N)%IM
            read(UNIT) STREAM%TILING(N)%JM
          endif
          call MAPL_CommsBcast(vm, DATA=STREAM%TILING(N)%NAME, N=ESMF_MAXSTR, ROOT=0, RC=status)
          if (NewGridNames_) then
             call GenOldGridName_(STREAM%TILING(N)%NAME)
          end if
          call MAPL_CommsBcast(vm, DATA=STREAM%TILING(N)%IM, N=1, ROOT=0, RC=status)
          call MAPL_CommsBcast(vm, DATA=STREAM%TILING(N)%JM, N=1, ROOT=0, RC=status)
       enddo

! Read location stream file into AVR
!---------------------------------------

       call MAPL_AllocateShared(AVR, (/NT,1/), TransRoot=.true., RC=STATUS)
       VERIFY_(STATUS)

       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
       if ( MAPL_am_I_root() ) then
         read(UNIT) AVR
         read(unit)
         read(unit)
       endif
       call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)

! Allocate msk for which tiles to include in the stream being created.
!--------------------------------------------------------------------

       call MAPL_AllocateShared(MSK, (/NT/), TransRoot=.true., RC=STATUS)
       VERIFY_(STATUS)

! We include any tile whose type matches any element of the mask
!---------------------------------------------------------------

       if (.not. MAPL_ShmInitialized  .or. MAPL_AmNodeRoot) then
          if(present(MASK)) then
             do N=1,NT
                if (nint(AVR(N,1)) < 0) AVR(N,1) = MAPL_Ocean
                MSK(N) = any(nint(AVR(N,1))==MASK(:))
             end do
          else
             MSK = .true.
          end if
       end if
       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)

! The number of tiles in the new stream
!--------------------------------------

       STREAM%NT_GLOBAL = count(MSK)

       if (present(GRID)) then
          allocate(ISMINE(STREAM%NT_GLOBAL), STAT=STATUS)
          VERIFY_(STATUS)
          ISMINE = .false.
          call ESMF_GRID_INTERIOR  (GRID, I1,IN,J1,JN)
          call ESMF_GridGet(grid, name=gname, rc=status)
          VERIFY_(STATUS)
          read_always = .false.
       else
          gname = ""
          read_always = .true.
       end if

! Fill ISMINE (Pick off local tiles)
!------------------------------------
    if (present(GRID)) then
       do N=1,STREAM%N_GRIDS
          if (read_always .or. gname == STREAM%TILING(N)%NAME) then
             call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
             if ( MAPL_am_I_root() ) read(UNIT) AVR
             call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
             K = 0
             do I=1, NT
                if(MSK(I)) then
                   K = K + 1
                   if (gname==STREAM%TILING(N)%NAME) then
                   ISMINE(K) = (I1<=nint(AVR(I,1)) .and. IN>=nint(AVR(I,1)))
                   endif
                end if
             end do
             call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
             if ( MAPL_am_I_root() ) read(UNIT) AVR
             call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
             K = 0
             do I=1, NT
                if(MSK(I)) then
                   K = K + 1
                   if ((gname==STREAM%TILING(N)%NAME) .and. (ISMINE(K))) then
                   ISMINE(K) = (J1<=nint(AVR(I,1)) .and. JN>=nint(AVR(I,1)))
                   endif
                end if
             end do
             if ( MAPL_am_I_root() ) read(UNIT)
          else
             if ( MAPL_am_I_root() ) read(UNIT)
             if ( MAPL_am_I_root() ) read(UNIT)
             if ( MAPL_am_I_root() ) read(UNIT)
          endif
       end do
       STREAM%NT_LOCAL = count(ISMINE)
       allocate(STREAM%LOCAL_IndexLocation(STREAM%NT_LOCAL), STAT=STATUS)
       VERIFY_(STATUS)
    end if

       if ( MAPL_am_I_root() ) then
         rewind(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
       endif

! Allocate space for local versions of stream parameters
!--------------------------------------------------------

! Fill global stream parameters subject to mask
!----------------------------------------------

       do N=1,STREAM%N_GRIDS
          if (read_always .or. gname == STREAM%TILING(N)%NAME) then
             call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
             if ( MAPL_am_I_root() ) read(UNIT) AVR
             call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
             K = 0
             L = 0
             do I=1, NT
                if(MSK(I)) then
                   K = K + 1
                   if (ISMINE(K)) then
                       L = L + 1
                       STREAM%LOCAL_IndexLocation(L)%I = nint(AVR(I,1))
                   endif
                end if
             end do
             
             call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
             if ( MAPL_am_I_root() ) read(UNIT) AVR
             call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
             K = 0
             L = 0
             do I=1, NT
                if(MSK(I)) then
                   K = K + 1
                   if (ISMINE(K)) then
                       L = L + 1
                       STREAM%LOCAL_IndexLocation(L)%J = nint(AVR(I,1))
                   endif
                end if
             end do
             
             call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
             if ( MAPL_am_I_root() ) read(UNIT) AVR
             call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
             K = 0
             L = 0
             do I=1, NT
                if(MSK(I)) then
                   K = K + 1
                   if (ISMINE(K)) then
                       L = L + 1
                       STREAM%LOCAL_IndexLocation(L)%W = AVR(I,1)
                   endif
                end if
             end do
          else
             if ( MAPL_am_I_root() ) read(UNIT)
             if ( MAPL_am_I_root() ) read(UNIT)
             if ( MAPL_am_I_root() ) read(UNIT)
          endif
       end do
    end if

! If grid is present attach that grid to the stream.
!  It must be one of the possible grids described in 
!  the tile file. This is ascertained by name.
!---------------------------------------------------
     
    if (present(GRID)) then
       call MAPL_LocStreamAttachGrid(LocStream, GRID, &
            ISMINE=ISMINE, RC=STATUS)
       VERIFY_(STATUS)

! Allocate Local arrays. Note STREAM%NT_LOCAL was defined in call to attach
!--------------------------------------------------------------------------

       allocate(STREAM%LOCAL_ID         (STREAM%NT_LOCAL), STAT=STATUS)
       VERIFY_(STATUS)
       allocate(STREAM%LOCAL_GeoLocation(STREAM%NT_LOCAL), STAT=STATUS)
       VERIFY_(STATUS)
       allocate(STREAM%D      (-1:1,-1:1,STREAM%NT_LOCAL), STAT=STATUS)
       VERIFY_(STATUS)
    end if

! For flat files we economize on space by rereading the file to get
!   the girds information.  ASCII files are very inefficient anyway
!   and are expensive to reread.
!------------------------------------------------------------------

    if(.not.isascii) then

       if ( MAPL_am_I_root() ) then
         rewind(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
         read(UNIT)
       endif

       K = 0
       L = 0
       do I=1, NT
          if(MSK(I)) then
             K = K + 1
             if (ISMINE(K)) then
                 L = L + 1
                 STREAM%LOCAL_ID(L) = I
             endif
          end if
       end do

       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
       if ( MAPL_am_I_root() ) read(UNIT) AVR
       call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
       K = 0
       L = 0
       do I=1, NT
          if(MSK(I)) then
             K = K + 1
             if (ISMINE(K)) then
                 L = L + 1
                 STREAM%LOCAL_GeoLocation(L)%T = nint(AVR(I,1))
                 !if (STREAM%LOCAL_GeoLocation(L)%T < 0)  STREAM%LOCAL_GeoLocation(L)%T = MAPL_Ocean
             endif
          end if
       end do

       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
       if ( MAPL_am_I_root() ) read(UNIT) AVR
       call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
       K = 0
       L = 0
       do I=1, NT
          if(MSK(I)) then
             K = K + 1
             if (ISMINE(K)) then
                 L = L + 1 
                 STREAM%LOCAL_GeoLocation(L)%X = AVR(I,1) * (MAPL_PI/180.)
             endif 
          end if
       end do

       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
       if ( MAPL_am_I_root() ) read(UNIT) AVR
       call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
       K = 0
       L = 0
       do I=1, NT
          if(MSK(I)) then
             K = K + 1
             if (ISMINE(K)) then
                 L = L + 1
                 STREAM%LOCAL_GeoLocation(L)%Y = AVR(I,1) * (MAPL_PI/180.)
             endif
          end if
       end do

       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
       if ( MAPL_am_I_root() ) then
          do I=1, 3*STREAM%N_GRIDS ! skip I,J,W
             read(UNIT)
          end do
          read(UNIT, iostat=iostat) AVR
       end if
       call MAPL_CommsBcast(vm, DATA=iostat, N=1, ROOT=0, RC=status)
       VERIFY_(STATUS)

       STREAM%IsTileAreaValid = iostat == 0
       if (STREAM%IsTileAreaValid) then
          call MAPL_BcastShared(vm, DATA=AVR, N=NT, ROOT=0, RootOnly=.false., RC=status)
          VERIFY_(STATUS)
          K = 0
          L = 0
          do I=1, NT
             if(MSK(I)) then
                K = K + 1
                if (ISMINE(K)) then
                   L = L + 1
                   STREAM%LOCAL_GeoLocation(L)%A = AVR(I,1)
                endif
             end if
          end do
       end if

       call FREE_FILE(UNIT)

       DEALOC_(MSK)
       DEALOC_(AVR)
    else  ! .not.isascii
       if(present(GRID)) then
          STREAM%LOCAL_GeoLocation   = pack(STREAM%GLOBAL_GeoLocation,  ISMINE)
          STREAM%LOCAL_ID            = pack(STREAM%GLOBAL_ID,           ISMINE)

          deallocate(STREAM%GLOBAL_GeoLocation)
          deallocate(Stream%Global_id)
       end if
    endif


    if(present(Grid)) then ! A grid was attached
       deallocate(ISMINE)

       DoCoeffs = .true.

       TILING => STREAM%TILING(STREAM%CURRENT_TILING)

! Compute coefficients for interpolating in G2T if the grid is lat-lon
!---------------------------------------------------------------------

       DX = 360./float(tiling%IM)

       I  = index(TILING%NAME,'-',.true.)
       ASSERT_(I>0)
       I  = I+1

       if    (TILING%NAME(I:I+1)=='DC') then
          X0 = -180.
          XE = 180.0-0.5*DX
       elseif(TILING%NAME(I:I+1)=='DE') then
          X0 = -180. + DX*0.5
          XE = 200.
       else
          DoCoeffs = .false.
       end if

       if    (TILING%NAME(1:2)=='PE') then
          DY = 180./float(tiling%JM  )
          Y0 = -90.  + DY*0.5
       elseIF(TILING%NAME(1:2)=='PC') then
          DY = 180./float(tiling%JM-1)
          Y0 = -90.
       else
          DoCoeffs = .false.
       end if

       if(DoCoeffs) then
          call ESMF_GRID_INTERIOR  (GRID, I1,IN,J1,JN)

          do I = 1, size(STREAM%LOCAL_IndexLocation)
             X  = STREAM%LOCAL_GeoLocation(I)%X*(180./MAPL_PI)
             if(X>XE) X = X - 360.0
             Y  = STREAM%LOCAL_GeoLocation(I)%Y*(180./MAPL_PI)
             II = STREAM%LOCAL_IndexLocation(I)%I + I1 - 1
             JJ = STREAM%LOCAL_IndexLocation(I)%J + J1 - 1
             call GetBilinearCoeffs(X0,Y0,DX,DY,X,Y,II,JJ, &
                  Stream%D(:,:,I),RC=STATUS)
          end do
       else
          do I = 1, size(STREAM%LOCAL_IndexLocation)
             Stream%D(:,:,I) = 0.0
             Stream%D(0,0,I) = 1.0
          end do
       endif
    endif

! Create a tile grid
!-------------------
    call MAPL_LocStreamCreateTileGrid(LocStream, GRID, RC=status)
    VERIFY_(STATUS)
       
    RETURN_(ESMF_SUCCESS)

100 RETURN_(ESMF_FAILURE)

  contains

    subroutine GetBilinearCoeffs(X0,Y0,DX,DY,X,Y,II,JJ,D,RC)
      real,              intent(IN   )  :: X, Y, X0, Y0, DX, DY
      integer,           intent(IN   )  :: II, JJ
      real,              intent(  OUT)  :: D(-1:,-1:)
      integer, optional, intent(  OUT)  :: RC

      character(len=ESMF_MAXSTR) :: IAm='GetBilinearCoeffs'
      integer                    :: STATUS
      real                       :: DX0, DY0
      real                       :: X00, Y00
      integer                    :: I

      X00     = X0 + (II-1)*DX
      Y00     = Y0 + (JJ-1)*DY

      DX0     = (X - X00)/DX
      DY0     = (Y - Y00)/DY

      D = 0.0

      if    (DX0 >= 0.0 .and. DY0 >= 0.0) then
        D( 1, 0) = DX0*(1.0-DY0) 
        D( 0, 1) = DY0*(1.0-DX0) 
        D( 0, 0) = (1.0-DX0)*(1.0-DY0) 
        D( 1, 1) = DX0*DY0
      elseif(DX0 >= 0.0 .and. DY0 <= 0.0) then
        DY0 = -DY0
        D( 1, 0) = DX0*(1.0-DY0) 
        D( 0,-1) = DY0*(1.0-DX0) 
        D( 0, 0) = (1.0-DX0)*(1.0-DY0) 
        D( 1,-1) = DX0*DY0 
      elseif(DX0 <= 0.0 .and. DY0 >= 0.0) then
        DX0 = -DX0
        D(-1, 0) = DX0*(1.0-DY0) 
        D( 0, 1) = DY0*(1.0-DX0) 
        D( 0, 0) = (1.0-DX0)*(1.0-DY0) 
        D(-1, 1) = DX0*DY0 
      else
        DX0 = -DX0
        DY0 = -DY0
        D(-1, 0) = DX0*(1.0-DY0) 
        D( 0,-1) = DY0*(1.0-DX0) 
        D( 0, 0) = (1.0-DX0)*(1.0-DY0) 
        D(-1,-1) = DX0*DY0 
      end if

      RETURN_(ESMF_SUCCESS)
   end subroutine GetBilinearCoeffs

   subroutine GenOldGridName_(name)
     character(len=*) :: name

     integer :: im, jm
     integer :: nn, xpos
     character(len=128) :: gridname
     character(len=2)   :: dateline, pole
     character(len=8)   :: imsz, jmsz
     character(len=128) :: imstr, jmstr
     

     ! Parse name for grid info 
     !-------------------------

     Gridname = AdjustL(name)
     nn   = len_trim(Gridname)
     xpos = index(Gridname,'x')
     imsz = Gridname(3:xpos-1)
     dateline = Gridname(1:2)
     pole = Gridname(xpos+1:xpos+2)


     if (pole=='6C') then ! cubed-sphere
        dateline='CF'
        pole='PE'

        read(IMSZ,*) IM
        jm = 6*im
     else
        jmsz = Gridname(xpos+3:nn)
        read(IMSZ,*) IM
        read(JMSZ,*) JM
     endif
    
     write(imstr,*) im
     write(jmstr,*) jm
     gridname =  pole // trim(adjustl(imstr))//'x'//&
                 trim(adjustl(jmstr))//'-'//dateline

     name = gridname

   end subroutine GenOldGridName_

  end subroutine MAPL_LocStreamCreateFromFile



!BOPI
! !IIROUTINE: MAPL_LocStreamCreateFromStream --- Create from stream

! !INTERFACE:
  subroutine MAPL_LocStreamCreateFromStream(LocStreamOut, LocStreamIn, NAME, MASK, RC)

! !ARGUMENTS:
    type(MAPL_LocStream),                 intent(  OUT) :: LocStreamOut
    type(MAPL_LocStream),                 intent(IN   ) :: LocStreamIn
    character(len=*),                     intent(IN   ) :: NAME
    integer,                    optional, intent(IN   ) :: MASK(:)
    integer,                    optional, intent(  OUT) :: RC  

! !DESCRIPTION: Creates a location stream as a subset of another
!   according to mask.

!EOP

! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamCreateFromStream'
    integer                    :: STATUS

    integer                           :: N, I, K, NT
    type(MAPL_LocStreamType), pointer :: STREAMOUT
    type(MAPL_LocStreamType), pointer :: STREAMIN
    integer                           :: NT_LOCAL(1)
    logical, pointer                  :: MSK(:)
    type(ESMF_VM)                     :: VM
    
! Begin
!------

    ASSERT_(     associated(LocStreamIn %Ptr))

! Allocate the Location Stream
!-----------------------------

    LocStreamOut%Ptr => null()
    allocate(LocStreamOut%Ptr, STAT=STATUS)
    VERIFY_(STATUS)

    STREAMOUT => LocStreamOut%Ptr
    STREAMIN  => LocStreamIn %Ptr

! Make sure that the input stream is attached
!--------------------------------------------
    ASSERT_(STREAMIN%CURRENT_TILING > 0)

! New stream has the same identifier as old
!------------------------------------------

    STREAMOUT%NAME       = NAME
    STREAMOUT%ROOTNAME   = STREAMIN%ROOTNAME
    STREAMOUT%N_GRIDS    = STREAMIN%N_GRIDS

    STREAMOUT%CURRENT_TILING = STREAMIN%CURRENT_TILING
    STREAMOUT%GRID = STREAMIN%GRID
    STREAMOUT%isTileAreaValid = STREAMIN%isTileAreaValid

! Allocate the allowed tilings and copy the names and sizes of the grids
!-----------------------------------------------------------------------

    allocate(STREAMOUT%TILING(STREAMOUT%N_GRIDS), STAT=STATUS)
    VERIFY_(STATUS)

    STREAMOUT%Tiling     = STREAMIN%Tiling           

! Local number of tiles in input stream
!--------------------------------------

    NT = STREAMIN%NT_LOCAL

! Allocate msk for which tiles to include in the stream being created.
!--------------------------------------------------------------------

    allocate(MSK(NT), STAT=STATUS)
    VERIFY_(STATUS)

! We include any tile whose type matches any element of the mask
!---------------------------------------------------------------

    if(present(MASK)) then
       do N=1,NT
          MSK(N) = any(STREAMIN%Local_GeoLocation(N)%T==MASK)
       end do
    else
       MSK = .true.
    end if

! The number of tiles in the new stream
!--------------------------------------

    STREAMOUT%NT_LOCAL = count(MSK)

    NT_LOCAL(1) = STREAMOUT%NT_LOCAL
    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(STATUS)
    call ESMF_VMAllFullReduce(vm, sendData=nt_local, recvData=STREAMOUT%NT_GLOBAL, &
         count=1, reduceflag=ESMF_REDUCE_SUM, rc=STATUS)
    VERIFY_(STATUS)

! Allocate space for local versions of stream parameters
!--------------------------------------------------------

    allocate(STREAMOUT%LOCAL_ID         (STREAMOUT%NT_LOCAL), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STREAMOUT%LOCAL_GEOLOCATION(STREAMOUT%NT_LOCAL), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STREAMOUT%LOCAL_INDEXLOCATION(STREAMOUT%NT_LOCAL), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STREAMOUT%D(-1:1,-1:1,STREAMOUT%NT_LOCAL), STAT=STATUS)
    VERIFY_(STATUS)

! Fill local stream parameters subject to mask
!----------------------------------------------

    K = 0
    do I=1, NT
       if(MSK(I)) then
          K = K + 1
          STREAMOUT%LOCAL_ID         (K)   = STREAMIN%LOCAL_ID         (I)  
          STREAMOUT%LOCAL_GeoLocation(K)%T = STREAMIN%LOCAL_GeoLocation(I)%T
          STREAMOUT%LOCAL_GeoLocation(K)%A = STREAMIN%LOCAL_GeoLocation(I)%A
          STREAMOUT%LOCAL_GeoLocation(K)%X = STREAMIN%LOCAL_GeoLocation(I)%X
          STREAMOUT%LOCAL_GeoLocation(K)%Y = STREAMIN%LOCAL_GeoLocation(I)%Y

          STREAMOUT%LOCAL_IndexLocation(K)%I = STREAMIN%LOCAL_IndexLocation(I)%I
          STREAMOUT%LOCAL_IndexLocation(K)%J = STREAMIN%LOCAL_IndexLocation(I)%J
          STREAMOUT%LOCAL_IndexLocation(K)%W = STREAMIN%LOCAL_IndexLocation(I)%W

          STREAMOUT%D(:,:,K) = STREAMIN%D(:,:,I)
       end if
    end do
    deallocate(MSK)

! Create a tile grid
!-------------------
    call MAPL_LocStreamCreateTileGrid(LocStreamOut, STREAMIN%GRID, RC=status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_LocStreamCreateFromStream


!======================================================

  subroutine MAPL_LocStreamAttachGrid(LocStream, GRID, ISMINE, RC)

    type(MAPL_LocStream),  intent(INOUT) :: LocStream
    type(ESMF_Grid),       intent(INout) :: Grid
    logical, optional,     pointer       :: ISMINE(:)
    integer, optional,     intent(  OUT) :: RC  
    
! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamAttachGrid'
    integer                    :: STATUS

    type(MAPL_LocStreamType), pointer :: STREAM
    type(MAPL_Tiling       ), pointer :: TILING
    integer                           :: IM_WORLD, JM_WORLD
    integer                           :: I1, IN, J1, JN
    integer                           :: gridRank
    integer                           :: DIMS(ESMF_MAXGRIDDIM)
    integer                           :: N

! Begin
!------

! Make sure Location stream has been created
!-------------------------------------------

    ASSERT_(associated(LocStream%Ptr))

! Alias to the pointer
!---------------------

    STREAM => LocStream%Ptr

! Location stream must have some allowed grids
!---------------------------------------------

    ASSERT_(STREAM%N_GRIDS>0)

! Find the given grid among the allowed grids
!--------------------------------------------

    STREAM%CURRENT_TILING = GRIDINDEX(STREAM, GRID, RC=STATUS)
    VERIFY_(STATUS)

    TILING => STREAM%TILING(STREAM%CURRENT_TILING)

!!    STREAM%GLOBAL_INDEXLOCATION => TILING%GLOBAL_INDEXLOCATION

! Put associated ESMF_LogRectGrid in stream and query grid info
!--------------------------------------------------------------

    STREAM%GRID = GRID

! Verify that the grid is the right size
!---------------------------------------

    call ESMF_GridGet(GRID, dimCount=gridRank, rc=STATUS)
    VERIFY_(STATUS)
    call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
    VERIFY_(STATUS)
    
    IM_WORLD = DIMS(1)
    JM_WORLD = DIMS(2)
    
    ASSERT_(IM_WORLD==TILING%IM)
    ASSERT_(JM_WORLD==TILING%JM)
    
! Find out which tiles are in local PE
!-------------------------------------

    call ESMF_GRID_INTERIOR  (GRID, I1,IN,J1,JN)
     
! Local location uses local indexing
!-----------------------------------

    STREAM%LOCAL_IndexLocation(:)%I = STREAM%LOCAL_IndexLocation(:)%I-I1+1
    STREAM%LOCAL_IndexLocation(:)%J = STREAM%LOCAL_IndexLocation(:)%J-J1+1

    RETURN_(ESMF_SUCCESS)
    
  end subroutine MAPL_LocStreamAttachGrid

!======================================================

  subroutine MAPL_LocStreamCreateTileGrid(LocStream, GRID, RC)

    type(MAPL_LocStream),  intent(INOUT) :: LocStream
    type(ESMF_Grid),       intent(INout) :: Grid
    integer, optional,     intent(  OUT) :: RC  
    
! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamCreateTileGrid'
    integer                    :: STATUS


    type(MAPL_LocStreamType), pointer :: STREAM
    type(MAPL_Tiling       ), pointer :: TILING
    type (ESMF_Grid)                  :: TILEGRID
    type (ESMF_DistGrid)              :: distgrid
    character(len=ESMF_MAXSTR)        :: GNAME
    integer                           :: arbIndexCount
    integer, allocatable              :: arbIndex(:,:)
    integer, parameter                :: DUMMY_NSUBTILES=1
    integer*8                         :: ADDR

! Begin
!------

! Make sure Location stream has been created
!-------------------------------------------

    ASSERT_(associated(LocStream%Ptr))

! Alias to the pointer
!---------------------
  
    STREAM => LocStream%Ptr


! Get the attached grid's info
!-----------------------------
  
    call ESMF_GridGet(GRID, NAME=GNAME, RC=STATUS)
    VERIFY_(STATUS)

! Create TILEGRID
!----------------
    distgrid = ESMF_DistGridCreate( &
         arbSeqIndexList=STREAM%LOCAL_ID, rc=status)
    VERIFY_(STATUS)
  
    TILEGRID = ESMF_GridEmptyCreate(rc=status)
    VERIFY_(STATUS)
         
    arbIndexCount = size(STREAM%LOCAL_ID)
    allocate(arbIndex(arbIndexCount,1), stat=status)
    VERIFY_(STATUS)

    arbIndex(:,1) = STREAM%LOCAL_ID
    call ESMF_GridSet(tilegrid,  &
         name="tile_grid_"//trim(Stream%NAME)//'@'//trim(GNAME),    &
         distgrid=distgrid, & 
         gridMemLBound=(/1/), &
         indexFlag=ESMF_INDEX_USER, &
         distDim = (/1/), &
         localArbIndexCount=arbIndexCount, &
         localArbIndex=arbIndex, &
         minIndex=(/1/), &
         maxIndex=(/STREAM%NT_GLOBAL/), &
         rc=status)
    VERIFY_(STATUS)

    deallocate(arbIndex)
    call ESMF_GridCommit(tilegrid, rc=status)
    VERIFY_(STATUS)

    call ESMF_AttributeSet(tilegrid, name='GRID_EXTRADIM', value=DUMMY_NSUBTILES, rc=status)
    VERIFY_(STATUS)

    STREAM%TILEGRID = TILEGRID

!ALT: here we are using a C routine to get the pointer to LocStream
!     and we are going to store it in TILEGRID as INTEGER*8 attribute
    call c_MAPL_LocStreamRetrievePtr(LocStream, ADDR)
    call ESMF_AttributeSet(tilegrid, name='TILEGRID_LOCSTREAM_ADDR', &
         value=ADDR, rc=status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_LocStreamCreateTileGrid

!======================================================

  subroutine MAPL_LocStreamAdjustNsubtiles(LocStream, NSUBTILES, RC)

    type(MAPL_LocStream),  intent(INOUT) :: LocStream
    integer,               intent(IN   ) :: NSUBTILES
    integer, optional,     intent(  OUT) :: RC  
    
! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamAdjustNsubtiles'
    integer                    :: STATUS

    type(MAPL_LocStreamType), pointer :: STREAM

! Alias to the pointer
!---------------------
  
    STREAM => LocStream%Ptr

!======================================================
! Check if the location stream is already attached
!-------------------------------------------------

    if (stream%current_tiling > 0) then
       call ESMF_AttributeSet(stream%tilegrid, name='GRID_EXTRADIM', &
            value=NSUBTILES, rc=status)
       VERIFY_(STATUS)
    end if

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_LocStreamAdjustNsubtiles
!======================================================
  

  !BOPI
  ! !IROUTINE: MAPL_LocStreamTransform
  ! !IIROUTINE: MAPL_LocStreamTransformField --- Transform field

  !INTERFACE:
  subroutine MAPL_LocStreamTransformField (LocStream, OUTPUT, INPUT, MASK, &
                                           GRID_ID, GLOBAL, ISMINE, INTERP, RC )

    !ARGUMENTS:
    type(ESMF_Field),          intent(OUT) :: OUTPUT
    type(ESMF_Field),          intent(INout) :: INPUT
    type(MAPL_LocStream),      intent(IN ) :: LocStream
    integer, optional,         intent(IN ) :: MASK(:)
    logical, optional,         intent(IN ) :: ISMINE(:), INTERP
    logical, optional,         intent(IN ) :: GLOBAL
    integer, optional,         intent(IN ) :: GRID_ID
    integer, optional,         intent(OUT) :: RC  
    !EOPI

! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamTransform'
  integer                    :: STATUS

  integer                    :: N, NT
  integer                    :: OutRank
  integer                    :: InRank
  type(ESMF_GRID)            :: INGRID
  type(ESMF_GRID)            :: OUTGRID
  type(ESMF_ARRAY)           :: INARRAY
  type(ESMF_ARRAY)           :: OUTARRAY
  real, pointer              :: TILEVAR(:)
  real, pointer              :: GRIDVAR(:,:)
  logical, pointer           :: MSK(:)

! Begin

  NT = LocStream%Ptr%NT_LOCAL

  allocate(MSK(NT),STAT=STATUS)
  VERIFY_(STATUS)

  if(present(MASK)) then
     do N = 1, NT
        MSK(N) = any(LocSTREAM%Ptr%LOCAL_GEOLOCATION(N)%T==MASK)
     end do
  else
     MSK = .true.
  end if

  call ESMF_FieldGet     (OUTPUT,   GRID=OUTGRID, RC=STATUS)
  VERIFY_(STATUS)
  call ESMF_FieldGet(OUTPUT, Array=OUTARRAY,     RC=STATUS)
  VERIFY_(STATUS)
  call ESMF_ArrayGet     (OUTARRAY, RANK=OUTRANK, RC=STATUS)
  VERIFY_(STATUS)

  call ESMF_FieldGet     (INPUT,    GRID=INGRID,  RC=STATUS)
  VERIFY_(STATUS)
  call ESMF_FieldGet(INPUT, Array=INARRAY,      RC=STATUS)
  VERIFY_(STATUS)
  call ESMF_ArrayGet     (INARRAY,  RANK=INRANK,  RC=STATUS)
  VERIFY_(STATUS)

  if    ( INRANK==1 .and. OUTRANK==2) then ! T2G
     call ESMF_ArrayGet(OUTARRAY, localDE=0, farrayptr=GRIDVAR, RC=STATUS)
     VERIFY_(STATUS)
     call ESMF_ArrayGet( INARRAY, localDE=0, farrayptr=TILEVAR, RC=STATUS)
     VERIFY_(STATUS)

     ASSERT_(size(TILEVAR)==NT)

     call MAPL_LocStreamTransformT2G (LOCSTREAM, GRIDVAR, TILEVAR, MASK=MSK, RC=STATUS)
     VERIFY_(STATUS)

   elseif( OUTRANK==1 .and. INRANK==2) then ! G2T
     call ESMF_ArrayGet(OUTARRAY, localDE=0, farrayptr=TILEVAR, RC=STATUS)
     VERIFY_(STATUS)
     call ESMF_ArrayGet( INARRAY, localDE=0, farrayptr=GRIDVAR, RC=STATUS)
     VERIFY_(STATUS)

     ASSERT_(size(TILEVAR)==NT)

     call MAPL_LocStreamTransformG2T(LOCSTREAM, TILEVAR, GRIDVAR, MSK, &
          GRID_ID, GLOBAL, ISMINE, INTERP, RC=STATUS)
     VERIFY_(STATUS)

  else
     RETURN_(ESMF_FAILURE)
  end if

  deallocate(MSK)

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformField

subroutine MAPL_LocStreamFracArea (LocStream, TYPE, AREA, RC )
  type(MAPL_LocStream),      intent(IN ) :: LocStream
  integer,                   intent(IN ) :: TYPE
  real,                      intent(OUT) :: AREA(:,:)
  integer, optional,         intent(OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamFracArea'
  integer                    :: STATUS

  integer                    :: II, JJ, N

! Make sure Location stream has been created...
!----------------------------------------------

  ASSERT_(associated(LocStream%Ptr))

! and a grid attached...
!-----------------------

  ASSERT_(LocStream%Ptr%Current_Tiling > 0)

! Compute area over masked locations
!-----------------------------------------------

  AREA   = 0.0
     
  do N = 1, size(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION)
     if(LOCSTREAM%Ptr%LOCAL_GEOLOCATION(N)%T == TYPE) then
        II = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I
        JJ = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J 
        AREA  (II,JJ) = AREA  (II,JJ) + LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W
     end if
  end do

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamFracArea


!BOPI
! !IIROUTINE: MAPL_LocStreamTransformT2G --- T2G

!INTERFACE:
subroutine MAPL_LocStreamTransformT2G (LocStream, OUTPUT, INPUT, MASK, SAMPLE, TRANSPOSE, RC )
  
  !ARGUMENTS:
  type(MAPL_LocStream),      intent(IN ) :: LocStream
  real,                      intent(INOUT) :: OUTPUT(:,:)
  real,                      intent(INOUT) :: INPUT(:)
  logical, optional,         intent(IN ) :: MASK(:) 
  logical, optional,         intent(IN ) :: SAMPLE
  logical, optional,         intent(IN ) :: TRANSPOSE
  integer, optional,         intent(OUT) :: RC  
  !EOPI
  
! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamTransformT2G'
  integer                    :: STATUS
  real,         allocatable  :: FF(:,:)
  integer                    :: II, JJ, N, I1, IN, J1, JN
  logical,      allocatable  :: usableMASK(:) 
  logical                    :: uSAMPLE 
  logical                    :: usableTRANSPOSE

! Make sure Location stream has been created...
!----------------------------------------------

  ASSERT_(associated(LocStream%Ptr))

! and a grid attached...
!-----------------------

  ASSERT_(LocStream%Ptr%Current_Tiling > 0)

! that's the size of the output array
!------------------------------------

  call ESMF_GRID_INTERIOR  (LocStream%Ptr%GRID, I1,IN,J1,JN)

  ASSERT_(IN-I1+1==size(OUTPUT,1))
  ASSERT_(JN-J1+1==size(OUTPUT,2))

! Allocate space for mask and cumulative weight at each grid point
!------------------------------------------------------------

  allocate(FF(size(OUTPUT,1),size(OUTPUT,2)), stat=STATUS)
  VERIFY_(STATUS)
  FF = 0.0
  allocate(usableMASK(size(INPUT)), STAT=STATUS)
  VERIFY_(STATUS)

! Make usable mask from optional argument
!----------------------------------------
  
  if (present(MASK)) then
     usableMASK = MASK
  else
     usableMASK = .TRUE.
  end if

  if (present(SAMPLE)) then
     uSAMPLE = SAMPLE
  else
     uSAMPLE = .false.
  end if

  if(present(TRANSPOSE)) then
     usableTRANSPOSE = TRANSPOSE
  else
     usableTRANSPOSE = .false.
  end if

! Compute weighted average over masked locations
!-----------------------------------------------

  if (usableTRANSPOSE) then
      INPUT  = 0.0
  else
      OUTPUT = 0.0
      if(uSample) then
         OUTPUT = MAPL_Undef
      end if
  endif

  do N = 1, size(INPUT)
     if(usableMASK(N) .and. INPUT(N)/=MAPL_UNDEF) then
        II = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I
        JJ = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J 
        if(uSample) then
           if( LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W > FF(II,JJ)) then
              OUTPUT(II,JJ) = INPUT(N)
              FF    (II,JJ) = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W
           end if
        else
          if(usableTRANSPOSE) then
               INPUT(N) = INPUT(N) + LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W * OUTPUT(II,JJ)
          else
               OUTPUT(II,JJ) = OUTPUT(II,JJ) + LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W * INPUT(N)
!jk
               FF    (II,JJ) = FF    (II,JJ) + LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W
          endif
     
        endif
     end if
  end do

  if (usableTRANSPOSE) then
      OUTPUT=0.
  endif

  if(.not.uSample) then
     if (usableTRANSPOSE) then
     else
         where(FF>0)
!jk
            OUTPUT = OUTPUT / FF
         end where
     endif
  endif

  if (usableTRANSPOSE) then
  else
     where(FF<=0)
!jk
        OUTPUT = MAPL_Undef
     end where
  endif

  deallocate(usableMASK)
  deallocate(FF)

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformT2G


!BOPI
! !IIROUTINE: MAPL_LocStreamTransformG2T --- G2T

!INTERFACE:
subroutine MAPL_LocStreamTransformG2T ( LocStream, OUTPUT, INPUT,      &
                                        MASK, GRID_ID, GLOBAL, ISMINE, &
                                        INTERP, TRANSPOSE, RC )

  !ARGUMENTS:
  type(MAPL_LocStream),      intent(IN ) :: LocStream
  real,                      intent(INOUT) :: OUTPUT(:)
  real,                      intent(INOUT) :: INPUT(:,:)
  logical, optional,         intent(IN ) :: MASK(:), ISMINE(:), INTERP
  logical, optional,         intent(IN ) :: GLOBAL
  integer, optional,         intent(IN ) :: GRID_ID
  logical, optional,         intent(IN ) :: TRANSPOSE
  integer, optional,         intent(OUT) :: RC  
  !EOPI

! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamTransformG2T'
  integer                    :: STATUS

  integer                    :: N, I1, IN, J1, JN, I, J, IM, JM
  logical,      allocatable  :: usableMASK(:)
 
  logical                    :: usableATTACHED
  logical                    :: usableGLOBAL
  logical                    :: usableINTERP
  logical                    :: usableTRANSPOSE
  real, pointer              :: ghostedINPUT(:,:)
  real                       :: D(-1:1,-1:1)
  real                       :: val, wt
  logical                    :: undefVals

  integer, parameter :: HALOWIDTH = 1

  IM = size(INPUT,1)
  JM = size(INPUT,2)

  if(present(INTERP)) then
     usableINTERP = INTERP
  else
     usableINTERP = .false.
  end if

  if(present(GRID_ID)) then
     usableATTACHED = .false.
  else
     usableATTACHED = .true.
  end if

  if(present(GLOBAL)) then
     usableGLOBAL = GLOBAL
  else
     usableGLOBAL = .false.
  end if

  if(present(TRANSPOSE)) then
     usableTRANSPOSE = TRANSPOSE
  else
     usableTRANSPOSE = .false.
  end if

! Make sure Location stream has been created...
!----------------------------------------------

  ASSERT_(associated(LocStream%Ptr))

! and a grid attached...
!-----------------------

  if (usableATTACHED) then
     ASSERT_(LocStream%Ptr%Current_Tiling > 0)

! that's the size of the output array
!------------------------------------

     call ESMF_GRID_INTERIOR  (LocStream%Ptr%GRID, I1,IN,J1,JN)

     ASSERT_(IN-I1+1==IM)
     ASSERT_(JN-J1+1==JM)
  else
     ASSERT_(GRID_ID <= LocStream%Ptr%N_GRIDS)
  endif

! Make usable mask from optional argument
!----------------------------------------

  allocate(usableMASK(size(OUTPUT)), STAT=STATUS)
  VERIFY_(STATUS)
  
  if (present(MASK)) then
     usableMASK = MASK
  else
     usableMASK = .TRUE.
  end if

  if(usableINTERP) then
     allocate(ghostedINPUT(1-HALOWIDTH:IM+HALOWIDTH,1-HALOWIDTH:JM+HALOWIDTH),STAT=STATUS)
     VERIFY_(STATUS)
     ghostedINPUT = MAPL_UNDEF ! ALT: this initializion should not be necessary
                               ! but ifort is not happy in SendRecv
     ghostedINPUT(1:IM,1:JM) = INPUT
     call ESMFL_HALO(LocStream%Ptr%GRID, ghostedINPUT, rc=status)
  end if

! Fill output subject to mask
!----------------------------
  if (usableTRANSPOSE) then
      INPUT =0.
  endif

  if (usableGLOBAL) then
     PRINT *, 'IN G2T GLOBAL NO LONGER VALID ARGUMENT'
     ASSERT_(.FALSE.)
  else
     do N = 1, size(OUTPUT)
        if(usableMASK(N)) then
           if(usableINTERP) then
              OUTPUT(N) = 0.0
              WT        = 0.0
              D         = LOCSTREAM%Ptr%D(:,:,N)
              do J=-1,1
                 do I=-1,1
                    val = GHOSTEDINPUT(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I+I, &
                                       LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J+J  )
                    if(VAL /= MAPL_Undef) then
                       OUTPUT(N) = OUTPUT(N) + ( VAL * D(I,J) )
                       wt = wt + D(I,J)
                    endif
                 end do
              end do

              if(WT/=0.0) then
                 OUTPUT(N) = OUTPUT(N)/WT
              else
                 OUTPUT(N) = MAPL_Undef
              end if

           else
              if (usableTRANSPOSE) then
                  INPUT(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I, &
                        LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J  ) = &
                  INPUT(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I, &
                        LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J  ) + OUTPUT(N)
              else
                  OUTPUT(N) = INPUT(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I, &
                                LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J  )
              endif
           end if
        end if
     end do
     if (usableTRANSPOSE) then
         OUTPUT=0.
     endif
  endif

  if(usableINTERP) then
     deallocate(ghostedINPUT,STAT=STATUS)
     VERIFY_(STATUS)
  end if
  deallocate(usableMASK)

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformG2T

subroutine MAPL_LocStreamTileWeight ( LocStream, OUTPUT, INPUT, RC )
  type(MAPL_LocStream),      intent(IN ) :: LocStream
  real,                      intent(OUT) :: OUTPUT(:)
  real,                      intent(IN ) :: INPUT(:,:)
  integer, optional,         intent(OUT) :: RC  

! Local variables

  integer                    :: N

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamTileWeight'
  integer                    :: STATUS


! Fill output subject to mask
!----------------------------

     do N = 1, size(OUTPUT)
        OUTPUT(N) = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W * &
                    INPUT(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I, &
                          LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J  )
     end do

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTileWeight



!BOPI
! !IIROUTINE: MAPL_LocStreamTransformT2T --- T2T

!INTERFACE:
subroutine MAPL_LocStreamTransformT2T ( OUTPUT, XFORM, INPUT, RC )

  !ARGUMENTS:
  real,                      intent(OUT) :: OUTPUT(:)
  type(MAPL_LocStreamXform), intent(IN ) :: XFORM
  real,                      intent(IN ) :: INPUT(:)
  integer, optional,         intent(OUT) :: RC  
  !EOPI

! Local variables

  character(len=ESMF_MAXSTR)  :: IAm='MAPL_LocStreamTransformT2T'
  integer                     :: STATUS

  integer                     :: N, offset
  integer                     :: me
  real,   allocatable         :: FULLINPUT(:)

#if defined(TWO_SIDED_COMM)
  integer, allocatable        :: request(:)
  integer                     :: msg_tag
  integer                     :: count
  integer                     :: NumReceivers
  integer                     :: mpstatus(MPI_STATUS_SIZE)
#elif defined(ONE_SIDED_COMM)
  logical                     :: use_lock

  use_lock = .false.
#endif

  ASSERT_(associated(Xform%PTR))
  
  do N = 1,Xform%PTR%LastLocal
     OUTPUT(Xform%PTR%IndexOut(N)) = INPUT(Xform%PTR%IndexIn(N))
  end do
  
  if(.not.Xform%PTR%Local) then
     
     if (Xform%PTR%do_not_use_fcollect) then
     me = Xform%PTR%myId

#if defined(TWO_SIDED_COMM)
     msg_tag = me
     count = size(input)
     NumReceivers = size(Xform%PTR%receivers)
     allocate(request(NumReceivers), stat=status)
     VERIFY_(status)
     do n=1, NumReceivers
        call MPI_ISend(input, count, MPI_REAL, &
                       Xform%PTR%receivers(n), msg_tag, &
                       Xform%PTR%Comm, request(n), status)
        VERIFY_(status)
     end do

#elif defined(ONE_SIDED_COMM)
     if (use_lock) then
        call MPI_WIN_LOCK(MPI_LOCK_EXCLUSIVE, me, 0, Xform%PTR%window, status)
        VERIFY_(STATUS)
     end if

     Xform%Ptr%Buff = input
     if (use_lock) then
        call MPI_WIN_UNLOCK(me, Xform%PTR%window, status)
        VERIFY_(STATUS)

        call mpi_Barrier(Xform%PTR%Comm,STATUS)
        VERIFY_(STATUS)
     end if
#endif

     allocate(FULLINPUT(Xform%Ptr%InputLen),STAT=STATUS)
     VERIFY_(STATUS)

#if defined(ONE_SIDED_COMM)
     if (.not. use_lock) then
        call mpi_win_fence(0, Xform%PTR%window, status)
        VERIFY_(STATUS)
     endif
#endif

     offset = 1
     if (associated(Xform%PTR%senders)) then
        do n=1,size(Xform%PTR%senders)
#if defined(TWO_SIDED_COMM)
! ALT: the senders' id is also used as a mpi_tag 
           msg_tag = Xform%PTR%senders(N)
           call MPI_RECV(FULLINPUT(offset), Xform%PTR%len(N), MPI_REAL, &
                         Xform%PTR%senders(N), msg_tag, &
                         Xform%Ptr%Comm, mpstatus, status)
           VERIFY_(STATUS)

#elif defined(ONE_SIDED_COMM)
           if (use_lock) then
              call MPI_WIN_LOCK(MPI_LOCK_SHARED, Xform%PTR%senders(N), &
                   0, Xform%PTR%window, status)
              VERIFY_(STATUS)
           end if
           
           call MPI_GET(FULLINPUT(offset),       Xform%PTR%len(N), MPI_REAL, &
                        Xform%PTR%senders(N), 0, Xform%PTR%len(N), MPI_REAL, &
                        Xform%PTR%window,                               STATUS)
           VERIFY_(STATUS)

           if (use_lock) then
              call MPI_WIN_UNLOCK(Xform%PTR%senders(N), Xform%PTR%window, status)
              VERIFY_(STATUS)
           end if
#endif

           offset = offset + Xform%PTR%len(N)
        enddo
     endif

#if defined(TWO_SIDED_COMM)
     do n=1, NumReceivers
        call MPI_Wait(request(n),MPI_STATUS_IGNORE,status)
        VERIFY_(STATUS)
     end do
     if (allocated(request)) deallocate(request)
#elif defined(ONE_SIDED_COMM)
     if (.not. use_lock) then
        call mpi_win_fence(0, Xform%PTR%window, status)
        VERIFY_(STATUS)
     endif
#endif

     else
     allocate(FULLINPUT(Xform%Ptr%InputStream%Ptr%NT_GLOBAL),STAT=STATUS)
     VERIFY_(STATUS)

     call ESMFL_FCOLLECT(Xform%Ptr%InputStream%Ptr%TILEGRID, FULLINPUT, INPUT, RC=STATUS)
     VERIFY_(STATUS)
     endif

     do N = Xform%PTR%LastLocal+1,Xform%PTR%Count
        OUTPUT(Xform%PTR%IndexOut(N)) = FULLINPUT(Xform%PTR%IndexIn(N))
     end do

     deallocate(FULLINPUT)

  end if

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformT2T



!BOPI
! !IIROUTINE: MAPL_LocStreamTransformT2TR4R8 --- T2TR4R8

!INTERFACE:
subroutine MAPL_LocStreamTransformT2TR4R8 ( OUTPUT, XFORM, INPUT, RC )
  
  !ARGUMENTS:
  real(kind=ESMF_KIND_R8),   intent(OUT) :: OUTPUT(:)
  type(MAPL_LocStreamXform), intent(IN ) :: XFORM
  real,                      intent(IN ) :: INPUT(:)
  integer, optional,         intent(OUT) :: RC  
  !EOPI

! Local variables

  character(len=ESMF_MAXSTR)  :: IAm='MAPL_LocStreamTransformT2TR4R8'
  integer                     :: STATUS

#ifdef OLD_RUN
  integer                     :: N
  real,   allocatable         :: FULLINPUT(:)

  ASSERT_(associated(Xform%PTR))

  do N = 1,Xform%PTR%LastLocal
     OUTPUT(Xform%PTR%IndexOut(N)) = INPUT(Xform%PTR%IndexIn(N))
  end do

  if(.not.Xform%PTR%Local) then

     allocate(FULLINPUT(Xform%Ptr%InputStream%Ptr%NT_GLOBAL),STAT=STATUS)
     VERIFY_(STATUS)

     call ESMFL_FCOLLECT(Xform%Ptr%InputStream%Ptr%TILEGRID, FULLINPUT, INPUT, RC=STATUS)
     VERIFY_(STATUS)

     do N = Xform%PTR%LastLocal+1,Xform%PTR%Count
        OUTPUT(Xform%PTR%IndexOut(N)) = FULLINPUT(Xform%PTR%IndexIn(N))
     end do

     deallocate(FULLINPUT)

  end if

#else

  real,   allocatable         :: OUTPUTR4(:)
  integer                     :: OUTSIZE

  OUTSIZE = size(OUTPUT)
  allocate(OUTPUTR4(OUTSIZE),STAT=STATUS)
  VERIFY_(STATUS)

  OUTPUTR4 = OUTPUT

  call MAPL_LocStreamTransformT2T( OUTPUTR4, XFORM, INPUT, RC ) 
  OUTPUT = OUTPUTR4
  deallocate(OUTPUTR4)
   
#endif

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformT2TR4R8



!BOPI
! !IIROUTINE: MAPL_LocStreamTransformT2TR8R4 --- T2TR8R4

!INTERFACE:
subroutine MAPL_LocStreamTransformT2TR8R4 ( OUTPUT, XFORM, INPUT, RC )

  !ARGUMENTS:
  real,                      intent(OUT) :: OUTPUT(:)
  type(MAPL_LocStreamXform), intent(IN ) :: XFORM
  real(kind=ESMF_KIND_R8),   intent(IN ) :: INPUT(:)
  integer, optional,         intent(OUT) :: RC  
  !EOPI

! Local variables

  character(len=ESMF_MAXSTR)  :: IAm='MAPL_LocStreamTransformT2TR8R4'
  integer                     :: STATUS

#ifdef OLD_RUN
  integer                     :: N
  real(kind=ESMF_KIND_R8),  allocatable  :: FULLINPUT(:)

  ASSERT_(associated(Xform%PTR))

  do N = 1,Xform%PTR%LastLocal
     OUTPUT(Xform%PTR%IndexOut(N)) = real(INPUT(Xform%PTR%IndexIn(N)))
  end do

  if(.not.Xform%PTR%Local) then

     allocate(FULLINPUT(Xform%Ptr%InputStream%Ptr%NT_GLOBAL),STAT=STATUS)
     VERIFY_(STATUS)

     call ESMFL_FCOLLECT(Xform%Ptr%InputStream%Ptr%TILEGRID, FULLINPUT, INPUT, RC=STATUS)
     VERIFY_(STATUS)

     do N = Xform%PTR%LastLocal+1,Xform%PTR%Count
        OUTPUT(Xform%PTR%IndexOut(N)) = FULLINPUT(Xform%PTR%IndexIn(N))
     end do

     deallocate(FULLINPUT)

  end if

#else

  real,   allocatable         :: INPUTR4(:)
  integer                     :: INPUTSIZE

  INPUTSIZE = size(INPUT)
  allocate(INPUTR4(INPUTSIZE),STAT=STATUS)
  VERIFY_(STATUS)

  INPUTR4 = INPUT

  call MAPL_LocStreamTransformT2T( OUTPUT, XFORM, INPUTR4, RC ) 
  deallocate(INPUTR4)

#endif

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformT2TR8R4

subroutine MAPL_LocStreamCreateXform ( Xform, LocStreamOut, LocStreamIn, NAME, MASK_OUT, &
     UseFCollect, RC )
  type(MAPL_LocStreamXform), intent(OUT) :: Xform
  type(MAPL_LocStream),      intent(IN ) :: LocStreamOut
  type(MAPL_LocStream),      intent(IN ) :: LocStreamIn
  character(len=*),          intent(IN ) :: NAME
  logical, optional,         intent(IN ) :: MASK_OUT(:)
  logical, optional,         intent(IN ) :: UseFCollect
  integer, optional,         intent(OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR)  :: IAm='MAPL_LocStreamCreateXform'
  integer                     :: STATUS

  integer                     :: N, M, MM
  logical                     :: DONE(LocStreamOut%Ptr%NT_local)
  logical, pointer            :: ISDONE(:)
  logical                     :: dn(1)
  type (ESMF_VM)              :: vm
  integer                     :: NDES, hash
  integer, pointer            :: GLOBAL_IdByPe(:) =>null() ! All Location Ids in PE   order
  integer                     :: I, First, Last, Comm
  logical, allocatable        :: IsNeeded(:)
  integer, allocatable        :: PELens(:), Begs(:), Ends(:)
  integer                     :: NumSenders
  integer                     :: NumReceivers
  integer                     :: lNumReceivers
  integer                     :: K, myId
  integer                     :: MyLen(1)
  integer                     :: SizeOfReal
  integer, allocatable        :: allSenders(:,:)
  integer, allocatable        :: iReq(:,:)

! Both streams must be subsets of same parent.
! The parent stream is usually an exchange grid.
!-----------------------------------------------

  ASSERT_(trim(LocStreamOut%PTR%ROOTNAME)==trim(LocStreamIn%PTR%ROOTNAME))

  allocate(XFORM%Ptr, STAT=STATUS)
  VERIFY_(STATUS)

  Xform%Ptr%InputStream  = LocStreamIn
  Xform%Ptr%OutputStream = LocStreamOut
  Xform%Ptr%Name         = NAME

  Xform%Ptr%do_not_use_fcollect = .false. ! defaults to FCOLLECT for now
#ifdef DO_NOT_USE_FCOLLECT
  Xform%Ptr%do_not_use_fcollect = .true.
#endif

  if (present(UseFCollect)) then
     Xform%Ptr%do_not_use_fcollect = .not. UseFCollect
  end if

! We have to fill all output locations where mask is true
!--------------------------------------------------------

  if(present(MASK_OUT)) then
     DONE = .not. MASK_OUT
  else
     DONE = .false.
  end if

  Xform%Ptr%count = count(.not.DONE)

  ALLOCATE(Xform%Ptr%IndexOut(Xform%Ptr%count), stat=STATUS)
  VERIFY_(STATUS)
  ALLOCATE(Xform%Ptr%IndexIn (Xform%Ptr%count), stat=STATUS)
  VERIFY_(STATUS)

  MM=1
    Hash  = MAPL_HashCreate(8*1024)
    do M = 1, LocStreamIn%Ptr%NT_local
       n = MAPL_HashIncrement(Hash,LocStreamIn%Ptr%Local_Id(M))
       ASSERT_(N==M)
    enddo
    do N = 1, LocStreamOut%Ptr%NT_local
       if(DONE(N)) cycle
       M = MAPL_HashIncrement(Hash,LocStreamOut%Ptr%Local_Id(N))
       if(m<=LocStreamIn%Ptr%NT_local) then  
         Xform%Ptr%IndexOut(MM) = N
         Xform%Ptr%IndexIn (MM) = M
         DONE  (N) = .TRUE.
         MM=MM+1
      endif
    end do
    call MAPL_HashDestroy(Hash)

  Xform%Ptr%LastLocal = MM-1

! Otherwise, assume nothing and do a full collect.
!-------------------------------------------------

  call ESMF_VMGetCurrent ( vm, rc=status )
  VERIFY_(STATUS)

  call ESMF_VMGet ( vm, petCount=nDEs, &
       mpiCommunicator=Xform%Ptr%Comm, rc=status )
  VERIFY_(STATUS)

  allocate(IsDone(NDES))
  dn(1) = all(done)

  call MAPL_CommsAllGather(vm, dn, 1, &
                           isdone, 1, rc=status)
  VERIFY_(STATUS)

  Xform%Ptr%Local = all(isdone)

  NEED_COMM: if(.not.Xform%Ptr%Local) then

     if (Xform%Ptr%do_not_use_fcollect) then
     allocate(PELens(NDES),Begs(NDES),Ends(NDES),IsNeeded(Ndes))
#if defined(ONE_SIDED_COMM)
     allocate(Xform%Ptr%Buff(LocStreamIn%Ptr%NT_LOCAL))
     allocate(Xform%Ptr%Len(NDES), stat=status)
     VERIFY_(STATUS)

     CALL MPI_TYPE_EXTENT(MPI_REAL, SizeOfReal, status)
     VERIFY_(STATUS)

     call mpi_Win_Create(Xform%Ptr%Buff,LocStreamIn%Ptr%NT_LOCAL*SizeOfReal, &
          SizeOfReal,MPI_INFO_NULL,Xform%Ptr%Comm,Xform%Ptr%Window,status)
     VERIFY_(STATUS)
#endif

     MyLen(1) = LocStreamIn%Ptr%NT_LOCAL

     call MAPL_CommsAllGather(vm, MyLen, 1, &
                                 PELens, 1, rc=status)
     VERIFY_(STATUS)

     Begs(1) = 1
     Ends(1) = PELens(1)
     do i=2,NDES
        Begs(i) = Ends(i-1) + 1
        Ends(i) = Ends(i-1) + PELens(i)
     end do

     ASSERT_(Ends(NDES) == LocStreamIn%Ptr%NT_GLOBAL)
     endif

     allocate(GLOBAL_IdByPe(LocStreamIn%Ptr%NT_GLOBAL), STAT=STATUS)
     VERIFY_(STATUS)

! Collect all tile ides in the input stream's pe order
!-----------------------------------------------------

     call ESMFL_FCOLLECT(LocStreamIn%Ptr%TILEGRID, GLOBAL_IdByPe, &
          LocStreamIn%Ptr%LOCAL_ID, RC=STATUS)
     VERIFY_(STATUS)

! Make a Hash of the tile locations by input order
!-------------------------------------------------

     Hash  = MAPL_HashCreate(80*1024)
     do M = 1, LocStreamIn%Ptr%NT_global
        n = MAPL_HashIncrement(Hash,Global_IdByPe(M))
        ASSERT_(N==M)
     enddo

     if(Xform%Ptr%do_not_use_fcollect) then
! Find out which processors have output tiles we need
!----------------------------------------------------
     
     IsNeeded = .false.
     do N = 1, LocStreamOut%Ptr%NT_local
        if(.not.DONE(N)) then
           M = MAPL_HashIncrement(Hash,LocStreamOut%Ptr%Local_Id(N))
           do i=1,ndes
              if(M>=Begs(i) .and. M<=Ends(i)) then
                 IsNeeded(i) = .true.
                 exit
              end if
           enddo
        end if
     end do

! Allocate my senders and their size in the Xform.
!  Note that fullinput has all tiles from all of those
!  pes that have tiles we need.
!-----------------------------------------------------

     NumSenders = count(IsNeeded)

     allocate(Xform%Ptr%senders(NumSenders), stat=status)
     VERIFY_(STATUS)
     allocate(Xform%Ptr%    len(NumSenders), stat=status)
     VERIFY_(STATUS)

     First = 1
     Last  = 0
     M = 0
     do I=1,NDES
        if(Isneeded(i)) then
           m = m + 1
           Last = Last + PELens(i)
           Global_IdByPe(First:Last) &
                 = Global_IdByPe(Begs(i):Ends(i))
           First = First + PELens(i)
           Xform%Ptr%senders(m) = i-1
           Xform%Ptr%    len(m) = PELens(i)
        end if
     end do

     deallocate(PELens,Begs,Ends,IsNeeded)

     Xform%Ptr%InputLen = Last 

     call ESMF_VmGet(VM, localPet=MYID, rc=status)
     VERIFY_(STATUS)
     Xform%Ptr%myId = myid

#if defined(TWO_SIDED_COMM)
#if 0
     allocate(allSenders(ndes,ndes), stat=status)
     VERIFY_(STATUS)
     allSenders(:,myId+1) = -1
     if (m>0) allSenders(1:M,myId+1) = Xform%Ptr%senders
     
     do I=1,NDES
        call MAPL_CommsBcast(vm, DATA=allSenders(:,I), N=ndes, ROOT=I-1, RC=status)
        VERIFY_(STATUS)
     end do
     NumReceivers = count(allSenders == myId)
     allocate(Xform%Ptr%receivers(NumReceivers), stat=status)
     VERIFY_(STATUS)

     M = 0
     do I=1,NDES
        if(myId == I-1) cycle ! skip myself
        do K=1,NDES
           if(allSenders(K,I) < 0) exit !senders are packed. we have reached end ...
           if(allSenders(K,I) == myId) then
              M = M+1
              Xform%Ptr%receivers(m) = i-1
              exit
           end if
        end do
     end do
     ASSERT_(NumReceivers==M)
     deallocate(allSenders)
#else
     allocate(allSenders(ndes,1), stat=status)
     VERIFY_(STATUS)

     do I=1,NDES
        lNumReceivers = 0
        if (m>0) lNumReceivers = count(Xform%Ptr%senders == I-1)
        call MPI_GATHER(  lNumReceivers, 1, MPI_INTEGER, &
                        allSenders(:,1), 1, MPI_INTEGER, &
                        I-1, Xform%Ptr%Comm,  status )
     enddo
     call ESMF_VMBarrier(vm, rc=status)
     VERIFY_(STATUS)

     NumReceivers = 0
     do I=1,NDES
        NumReceivers = NumReceivers + allSenders(I,1)
     end do
     allocate(Xform%Ptr%receivers(NumReceivers), stat=status)
     VERIFY_(STATUS)

     M = 0
     do I=1,NDES
        if(myId == I-1) cycle ! skip myself
        do K=1,allSenders(I,1)
           M = M+1
           Xform%Ptr%receivers(M) = I-1
        end do
     end do

     ASSERT_(NumReceivers==M)
     deallocate(allSenders)
#endif
#endif

! Put the tiles we being brought over into a hash table
!------------------------------------------------------

     call MAPL_HashDestroy(Hash)

     Hash  = MAPL_HashCreate(8*1024)
     do M = 1, Xform%Ptr%InputLen
        n = MAPL_HashIncrement(Hash,Global_IdByPe(M))
        ASSERT_(N==M)
     enddo

! Pick out the ones we need fromthose brought over
!-------------------------------------------------
     endif

     do N = 1, LocStreamOut%Ptr%NT_local
        if(.not.DONE(N)) then
           M = MAPL_HashIncrement(Hash,LocStreamOut%Ptr%Local_Id(N))
           Xform%Ptr%IndexOut(MM) = N
           Xform%Ptr%IndexIn (MM) = M
           DONE  (N) = .TRUE.
           MM=MM+1
        end if
     end do

     call MAPL_HashDestroy(Hash)

     deallocate(GLOBAL_IdByPe)

  end if NEED_COMM

! Make sure that did it
!----------------------

  ASSERT_(all(DONE))

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamCreateXform



integer function GRIDINDEX(STREAM,GRID,RC) 
  type(MAPL_LocStreamType),      intent(IN ) :: Stream
  type(ESMF_Grid),               intent(IN ) :: Grid
  integer, optional,             intent(OUT) :: RC  

  character(len=ESMF_MAXSTR) :: IAm='GridIndex'
  integer                    :: STATUS
  integer                    :: N

  character(len=ESMF_MAXSTR) :: NAME

! Find the given grid among the allowed grids
!--------------------------------------------

  call ESMF_GridGet(GRID, NAME=NAME, RC=STATUS)
  VERIFY_(STATUS)

  GridIndex = 0

  do N=1,STREAM%N_GRIDS
     if(STREAM%TILING(N)%NAME==NAME) then
        GridIndex = N
        exit
     end if
  end do

  ASSERT_(GridIndex/=0)

  RETURN_(ESMF_SUCCESS)

end function GRIDINDEX

subroutine MAPL_GridCoordAdjust(GRID, LOCSTREAM, RC)
  type(ESMF_Grid),               intent(INout ) :: Grid
  type(MAPL_LocStream),          intent(IN ) :: Locstream
  integer, optional,             intent(OUT) :: RC  

! local vars
!------------ 
  character(len=ESMF_MAXSTR) :: IAm='MAPL_GridCoordAdjust'
  integer                    :: STATUS
  
  integer :: NGRIDS
  integer :: I, J, N
  integer :: IM, JM

  logical :: found
  integer :: COUNTS(3)
  integer :: rank, NT, IG
  type(ESMF_TypeKind_Flag)   :: tk
  character(len=ESMF_MAXSTR) :: GRIDNAME
  character(len=ESMF_MAXSTR), pointer :: GNAMES(:)
  type (ESMF_Array) :: carray(2)
  real(ESMF_KIND_R8) :: X, Y, W
  real(ESMF_KIND_R8), allocatable :: sumw(:,:), sumxw(:,:), sumyw(:,:)
  real(ESMF_KIND_R8), pointer :: gridx(:,:), gridy(:,:)

! get grid name
  call ESMF_GridGet(grid, name=gridname, rc=status)
  VERIFY_(STATUS)

  call MAPL_LocstreamGet(LOCSTREAM, GRIDNAMES = GNAMES, RC=STATUS)
  VERIFY_(STATUS)
! query loc_in for ngrids
  ngrids = size(gnames)
  ASSERT_(ngrids==2)

! validate that gridname_in is there
  found = .false.
  DO I = 1, NGRIDS
     IF (GNAMES(I) == GRIDNAME) THEN
        FOUND = .TRUE.
        exit
     ENDIF
  ENDDO
  ASSERT_(FOUND)

! get id of the grid we just found
  IG = I 
  ASSERT_(IG == LocStream%Ptr%Current_Tiling)

! get IM, JM and IM_WORLD, JM_WORLD
  call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, RC=STATUS)
  VERIFY_(STATUS)

  IM = COUNTS(1)
  JM = COUNTS(2)

! Retrieve the coordinates so we can set them
  call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
       staggerloc=ESMF_STAGGERLOC_CENTER, &
       farrayPtr=gridX, rc=status)
  VERIFY_(STATUS)

  call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
       staggerloc=ESMF_STAGGERLOC_CENTER, &
       farrayPtr=gridY, rc=status)
  VERIFY_(STATUS)

  allocate(sumxw(IM, JM), sumyw(IM, JM), sumw (IM, JM), stat=status)
  VERIFY_(STATUS)

  SUMW = 0.0
  SUMXW = 0.0
  SUMYW = 0.0

  NT = LOCSTREAM%Ptr%NT_Local

! loop over tiles
  DO N = 1, NT
     I = LOCSTREAM%Ptr%Local_IndexLocation(N)%I
     J = LOCSTREAM%Ptr%Local_IndexLocation(N)%J
     W = LOCSTREAM%Ptr%Local_IndexLocation(N)%W
     X = locstream%Ptr%Local_GeoLocation(N)%X
     Y = locstream%Ptr%Local_GeoLocation(N)%Y
     SUMW(I,J) = SUMW(I,J) + W
     SUMXW(I,J) = SUMXW(I,J) + X * W
     SUMYW(I,J) = SUMYW(I,J) + Y * W
  END DO

  WHERE (SUMW == 0.0)
     SUMXW = MAPL_UNDEF
     SUMYW = MAPL_UNDEF
  ELSEWHERE
     SUMXW = SUMXW / SUMW
     SUMYW = SUMYW / SUMW

! Make sure the longitudes are between -180 and 180 degrees
     SUMXW = mod(SUMXW + 72180._8,360._8) - 180._8 ! -180<= lon0 <180
! Convert to radians
     SUMXW = SUMXW * (MAPL_PI_R8)/180._8
     SUMYW = SUMYW * (MAPL_PI_R8)/180._8
     
  END WHERE

! Modify grid coordinates
!------------------------
  GRIDX = SUMXW
  GRIDY = SUMYW

! Clean-up
!---------
  deallocate(sumw)
  deallocate(sumyw)
  deallocate(sumxw)

! All done
!---------
  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_GridCoordAdjust

end module MAPL_LocStreamMod
