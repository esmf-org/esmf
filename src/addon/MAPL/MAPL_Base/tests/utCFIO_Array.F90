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
!
! Simple unit test for CFIO Read/Write of Arrays
!

#include "MAPL_Generic.h"

   Program utCFIO

   use ESMF

   use MAPL_BaseMod
   use MAPL_CommsMod
   use MAPL_ConstantsMod

   use ESMF_CfioMod
   use MAPL_CfioMod

   implicit NONE

   type(ESMF_Grid)     :: grid2d, grid3d
   type (ESMF_VM)      :: VM
   type(ESMF_DELayout) :: layout

   integer             :: nymd, nhms
   type(ESMF_Time)     :: time2, time3, times
   type(ESMF_Time)     :: time2a, time2b, time2c
   type(ESMF_Time)     :: time3a, time3b, time3c

   type(ESMF_CFIO) :: cfio

   integer, parameter :: IM_WORLD = 288, JM_WORLD = 181, KM_WORLD = 72 ! global
   integer :: i, j, k, im, jm, km, n                                   ! local

   character(len=*), parameter :: &
          dirname = '/share/dasilva/fvInput/fvchem/b/',               &
     f2d_Filename = dirname // 'bian_biogenic_ch4_20010112.2x2.5.nc', &
     f3d_Filename = dirname // 'bian_OHCH4_2x25x72.019498.nc',        &
     src_Filename = dirname // 'gocart.du_src.sfc.1971.hdf'

   real, pointer, dimension(:,:)   :: emcoterp, emconvoc
   real, pointer, dimension(:,:,:) :: oh, ch4, xa, xb, xc, xm, xe

   real, pointer :: lons(:), lats(:)

   integer :: status, rc, dims(3)
   logical :: IamRoot

   character(len=*), parameter :: Iam = 'utCFIO'

!                             -----
    
    call test_main()

CONTAINS

    subroutine test_main()

!   Initialize framework
!   --------------------
    call ESMF_Initialize (vm=vm, rc=status)
    VERIFY_(status)

    IamRoot = MAPL_am_I_root()

!   Get the global vm
!   -----------------
    call ESMF_VMGetGlobal(vm, rc=status)
    VERIFY_(status)

!   Create a grid
!   -------------
    call MyGridCreate_ ( vm, grid2d, grid3d, rc=status )
    VERIFY_(status)

!   Set the time as the one on the hardwired file name
!   --------------------------------------------------
    call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeSet(Time2,  yy=2001, mm=1, dd=1,  h=0,  m=0, s=0, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeSet(Time3,  yy=2001, mm=1, dd=14, h=12, m=0, s=0, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeSet(Time2a, yy=2001, mm=3, dd=1,  h=12, m=0, s=0, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeSet(Times,  yy=1971, mm=6, dd=5,  h=0, m=0, s=0, rc=status )
    VERIFY_(STATUS)

!   Get Local dimensions
!   --------------------
    call MAPL_GridGet ( GRID3D, &
                        localCellCountPerDim=DIMS,RC=STATUS)
    VERIFY_(STATUS)

!   Allocate arrays
!   ---------------
    im = dims(1);   jm = dims(2);   km = dims(3)
    allocate( emcoterp(im,jm), emconvoc(im,jm), &
              oh(im,jm,km), ch4(im,jm,km), &
              stat=STATUS ) 
    VERIFY_(STATUS)

!   Read 2D arrays from file
!   ------------------------
!    if ( IamRoot ) print *, 'Reading ' // fFilename
    call ESMF_ioRead  ( 'du_src', src_Filename, Times, grid3d, emcoterp,  &
                        verbose=.true., force_regrid=.true.,                &
                        time_is_cyclic = .true., rc=STATUS                  )
    VERIFY_(STATUS)
    call ESMF_ioRead  ( 'emcoterp', f2d_Filename, Time2, grid3d, emcoterp,  &
                        verbose=.true., force_regrid=.true.,                &
                        time_is_cyclic = .true., rc=STATUS                  )
    VERIFY_(STATUS)
    call ESMF_ioRead  ( 'emconvoc', f2d_Filename, Time2, grid3d, emconvoc,  &
                        verbose=.true., force_regrid=.true.,                &
                        time_is_cyclic = .true., rc=STATUS                  )
    VERIFY_(STATUS)
    call ESMF_ioRead  ( 'OH',       f3d_Filename, Time3, grid3d, oh,        &
                        verbose=.true., force_regrid=.true., rc=STATUS      )
    VERIFY_(STATUS)
    call ESMF_ioRead  ( 'CH4',       f3d_Filename, Time3, grid3d, ch4,      &
                        verbose=.true., force_regrid=.true., rc=STATUS      )
    VERIFY_(STATUS)

    if ( IamRoot ) print *, 'Re-reading w/ time interpolation...'
    call ESMF_ioRead  ( 'emconvoc', f2d_Filename, Time2a, grid3d, emconvoc, &
                        verbose=.true., force_regrid=.true.,                &
                        time_is_cyclic = .true., time_interp=.true.,        &
                        rc=STATUS                  )
    VERIFY_(STATUS)

    deallocate(oh,ch4,emconvoc)

    allocate( xa(im,jm,km), xb(im,jm,km), xc(im,jm,km), &
              xm(im,jm,km), xe(im,jm,km),               &
              stat=STATUS ) 
    VERIFY_(STATUS)

    print *

!   Testing time interpolation
!   --------------------------
    call ESMF_TimeSet(Time3a, yy=2001, mm=3, dd=16, h=12, m=0, s=0, rc=status )
    call ESMF_TimeSet(Time3b, yy=2001, mm=3, dd=31, h=18, m=0, s=0, rc=status )
    call ESMF_TimeSet(Time3c, yy=2001, mm=4, dd=16, h= 0, m=0, s=0, rc=status )

    call ESMF_ioRead  ( 'CH4',       f3d_Filename, Time3a, grid3d, xa,      &
                        verbose=.true., force_regrid=.true.,                &
                        time_interp = .true.,                               &
                        rc=STATUS      )
    VERIFY_(STATUS)

    call ESMF_ioRead  ( 'CH4',       f3d_Filename, Time3b, grid3d, xb,      &
                        verbose=.true., force_regrid=.true., rc=STATUS,     &
                        time_interp = .true.                                )
    VERIFY_(STATUS)
    call ESMF_ioRead  ( 'CH4',       f3d_Filename, Time3c, grid3d, xc,      &
                        verbose=.true., force_regrid=.true., rc=STATUS,     &
                        time_interp = .true.                                )
    VERIFY_(STATUS)

    xm = ( xa + xc ) / 2.
    xe = abs ( xb  - xm )

    n = im * jm

    i = im/2

    if ( IamRoot ) then

         print *
         print *, 'Time interpolation stats: '
         print *
      do j = 1, jm, jm/4
         print *, 'Lat = ', lats(j)
         print *, 'CH4 at time a: ', (xa(i,j,k), k=1,km,km/4)
         print *, 'CH4 at time b: ', (xb(i,j,k), k=1,km,km/4)
         print *, 'CH4 m e a n  : ', (xm(i,j,k), k=1,km,km/4)
         print *, 'CH4 at time c: ', (xc(i,j,k), k=1,km,km/4)
         print *, 'CH4 e r r o r: ', (xe(i,j,k), k=1,km,km/4)
         print *
      end do

      print *, 'CH4 RMS error: ', sqrt(sum(xe*xe)/n)

    endif

!   All done
!   --------
    call ESMF_Finalize ( rc=status )
    VERIFY_(STATUS)
    
  end subroutine test_main

!........................................................................

    subroutine MyGridCreate_ ( vm, grid2d, grid3d, rc)

    type (ESMF_VM),    intent(IN   ) :: VM
    type(ESMF_GRID), intent(INOUT)   :: grid2d, grid3d
    integer, optional, intent(OUT)   :: rc

! Local vars
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MyGridCreate'

    integer                         :: LM
    integer                         :: L
    integer                         :: NX, NY
    integer, allocatable            :: IMXY(:), JMXY(:)
    character(len=ESMF_MAXSTR)      :: gridname
    real(ESMF_KIND_R8)              :: minCoord(3)
    real(ESMF_KIND_R8)              :: deltaX, deltaY, deltaZ
    real                            :: LON0, LAT0

    real :: pi, d2r

! grid create

    lm = KM_WORLD   ! no. vertical layers
    nx = 2
    ny = 2

     pi  = 4.0 * atan ( 1.0 ) 
    d2r  = pi / 180.
    LON0 = -180  * d2r
    LAT0 = -90.0 * d2r

! Get the IMXY vector
! -------------------
    allocate( imxy(0:nx-1) )  
    call MAPL_GET_LOCAL_DIMS ( IM_WORLD, imxy, nx )

! Get the JMXY vector
! -------------------
    allocate( jmxy(0:ny-1) )  
    call MAPL_GET_LOCAL_DIMS ( JM_WORLD, jmxy, ny )

    deltaX = 2.0*pi/IM_WORLD
    deltaY = pi/(JM_WORLD-1)
    deltaZ = 1.0

! Define South-West Corner of First Grid-Box
! ------------------------------------------
    minCoord(1) = LON0 - deltaX/2 
    minCoord(2) = LAT0 - deltaY/2
    minCoord(3) = deltaZ/2.

    layout = ESMF_DELayoutCreate(vm, deCountList=(/NX, NY/), rc=status)
    VERIFY_(STATUS)

    if ( MAPL_Am_I_Root() ) then
       print *
       print *, '            Testing CFIO Read Array'
       print *, '            -----------------------'
       print *
       print *, 'Decomposition Information:'
       print *, '   X AXIS: nx, imxy = ', nx, imxy
       print *, '   Y AXIS: ny, jmxy = ', ny, jmxy
       print *, '   Z AXIS: nz       = ', lm, ' (not decomposed)'
    end if

!   2D grid
!   -------
    grid2d = ESMF_GridCreateHorzLatLonUni(         &
         counts = (/IM_WORLD, JM_WORLD/),        &
         minGlobalCoordPerDim=minCoord(1:2),     &
         deltaPerDim=(/deltaX, deltaY /),        &
         horzStagger=ESMF_Grid_Horz_Stagger_A,   &
         periodic=(/ESMF_TRUE, ESMF_FALSE/),     &
         name='Grid2d', rc=status)
    VERIFY_(STATUS)

    call ESMF_GridDistribute(grid2d,               &
         deLayout=layout,                        &
         countsPerDEDim1=imxy,                   &
         countsPerDEDim2=jmxy,                   &
         rc=status)
    VERIFY_(STATUS)


!   3D Grid
!   -------
    grid3d = ESMF_GridCreateHorzLatLonUni(         &
         counts = (/IM_WORLD, JM_WORLD/),        &
         minGlobalCoordPerDim=minCoord(1:2),     &
         deltaPerDim=(/deltaX, deltaY /),        &
         horzStagger=ESMF_Grid_Horz_Stagger_A,   &
         periodic=(/ESMF_TRUE, ESMF_FALSE/),     &
         name='Grid3d', rc=status)
    VERIFY_(STATUS)

    call ESMF_GridAddVertHeight(grid3d,            &
         delta=(/(deltaZ, L=1,LM) /),            &
         rc=status)
    VERIFY_(STATUS)

    call ESMF_GridDistribute(grid3d,               &
         deLayout=layout,                        &
         countsPerDEDim1=imxy,                   &
         countsPerDEDim2=jmxy,                   &
         rc=status)
    VERIFY_(STATUS)

    call MAPL_GridGetLatLons ( grid3d, lons, lats )

    if ( MAPL_Am_I_Root() ) then
       print *
       print *, 'Grid points:'
       print *, '  - Longitudes: '
       write(*,100) lons(:)
       print *, '  - Latitudes:'
       write(*,100) lats(:)
       print *
100    format(( '   ',8F8.2))
    endif

    deallocate(imxy)
    deallocate(jmxy)

    RETURN_(STATUS)

  end subroutine MyGridCreate_

  subroutine MAPL_GridGetLatLons ( grid, lons, lats )

    implicit NONE
    type(ESMF_Grid) :: grid

    real, pointer   :: lons(:), lats(:)

!                     ---

    type(ESMF_Array)       :: eARRAY(2)

    real(KIND=8), pointer  :: R8D2(:,:)
    real, pointer          :: lons2d(:,:), lats2d(:,:)
    real, pointer          :: LONSLocal(:,:), LATSlocal(:,:)
    integer                :: IM_WORLD, JM_WORLD, dims(3)

!                          ----

!      Get world dimensions
!      --------------------
       call ESMF_GridGet ( grid, horzRelloc=ESMF_CELL_CENTER, &
                           vertRelLoc=ESMF_CELL_CENTER, &
                           globalCellCountPerDim=DIMS, RC=STATUS)
       VERIFY_(STATUS)

       IM_WORLD = dims(1)
       JM_WORLD = dims(2)

!      Allocate memory for output if necessary
!      ---------------------------------------
       if ( .not. associated(lons) ) then
            allocate(lons(IM_WORLD), stat=STATUS)
       else
            if(size(LONS,1) /= IM_WORLD) STATUS = 1
       end if
       VERIFY_(status)
       if ( .not. associated(lats) ) then
            allocate(lats(JM_WORLD), stat=STATUS)
       else
            if(size(LATS,1) /= JM_WORLD) STATUS = 1
       end if
       VERIFY_(status)

!      Retrieve the ESMF array with coordinates 
!      ----------------------------------------
       call ESMF_GridGetCoord ( grid, horzRelLoc =ESMF_CELL_CENTER, &
                                centerCoord=eARRAY, RC=STATUS ) 
       VERIFY_(STATUS) 

!      Local work space
!      ----------------
       allocate(LONS2d(IM_WORLD,JM_WORLD), LATS2d(IM_WORLD,JM_WORLD), &
                STAT=status)             
       VERIFY_(status)

!      Get the local longitudes and gather them into a global array
!      ------------------------------------------------------------
       call ESMF_ArrayGetData(EARRAY(1), R8D2, RC=STATUS)
       VERIFY_(STATUS)

       allocate(LONSLOCAL(size(R8D2,1),size(R8D2,2)), STAT=status)             
       VERIFY_(status)

       LONSLOCAL = R8D2*(180/MAPL_PI)

       call ArrayGather(LONSLOCAL, LONS2D, GRID, RC=STATUS)

!      Get the local longitudes and gather them into a global array
!      ------------------------------------------------------------
       call ESMF_ArrayGetData(eARRAY(2), R8D2, RC=STATUS)
       VERIFY_(STATUS)

       allocate(LATSLOCAL(size(R8D2,1),size(R8D2,2)), STAT=status)             
       VERIFY_(status)

       LATSlocal = R8D2*(180/MAPL_PI)

       call ArrayGather(LATSLOCAL, LATS2D, GRID, RC=STATUS)
       VERIFY_(STATUS)

!      Return 1D arrays
!      ----------------
       LONS = LONS2D(:,1)
       LATS = LATS2D(1,:)

       DEALLOCATE(LONSLOCAL, LATSLOCAL, LONS2d, LATS2d )
        
     end subroutine MAPL_GridGetLatLons

end Program utCFIO




