!
! Simple unit test for CFIO Read/Write Bundle
!

#include "MAPL_Generic.h"

   Program utDistIO

   use ESMF_Mod

   use GEOS_BaseMod
   use GEOS_CommsMod

   use m_zeit

   implicit NONE

   type(ESMF_Grid)     :: grid
   type (ESMF_VM)      :: VM
   type(ESMF_DELayout) :: layout


   integer :: IM_WORLD = 288*4, JM_WORLD = 181*4, KM_WORLD = 72   ! globalc72
   integer :: i, j, k, im, jm, km                             ! local
   integer :: lu 10


   character(len=*), parameter :: &
       dirname = '/lscr1/dasilva'

   real :: gArr(:,:), dArr(:,:)


   integer :: status, rc, ntimes = 500 
   logical :: IamRoot

   character(len=*), parameter :: Iam = 'utDistIO'

!                             -----
    
    call test_main()

CONTAINS

    subroutine test_main()

!   Initialize framework
!   --------------------
    call ESMF_Initialize (vm=vm, rc=status)
    VERIFY_(status)

    IamRoot = GEOS_am_I_root()

!   Get the global vm
!   -----------------
    call ESMF_VMGetGlobal(vm, rc=status)
    VERIFY_(status)

!   Create a grid
!   -------------
    grid = MyGridCreate_ ( vm, rc=status )
    VERIFY_(status)

!   Get local im, jm
!   ----------------

    if ( IamRoot ) print *, 'Hello, World'

!   Allocate and write global array
!   -------------------------------
    if ( IamRoot ) then
       allocate(gArr(IM_WORLD,JM_WORLD))
       gArr = 1.0;
       open(lu,filen=dirname//'/global.dat')
       do n = 1, ntimes
          write(lu) gArr
       end do
       close(lu)
    end if

!   Next, scatter array
!   -------------------
    call 

!   All done
!   --------
    call ESMF_Finalize ( status )
    VERIFY_(STATUS)
    
  end subroutine test_main

!........................................................................

  function MyGridCreate_ ( vm, rc) result(grid)

    type (ESMF_VM),    intent(IN   ) :: VM
    integer, optional, intent(OUT)   :: rc
    type (ESMF_Grid)                 :: grid

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
    call GEOS_GET_LOCAL_DIMS ( IM_WORLD, imxy, nx )

! Get the JMXY vector
! -------------------
    allocate( jmxy(0:ny-1) )  
    call GEOS_GET_LOCAL_DIMS ( JM_WORLD, jmxy, ny )

    deltaX = 2.0*pi/IM_WORLD
    deltaY = pi/(JM_WORLD-1)
    deltaZ = 1.0

    if ( GEOS_Am_I_Root() ) then
       print *, 'nx : imxy = ', nx, ' : ', imxy
       print *, 'ny : jmxy = ', ny, ' : ', jmxy
    endif

! Define South-West Corner of First Grid-Box
! ------------------------------------------
    minCoord(1) = LON0 - deltaX/2 
    minCoord(2) = LAT0 - deltaY/2
    minCoord(3) = deltaZ/2.

    layout = ESMF_DELayoutCreate(vm, deCountList=(/NX, NY/), rc=status)
    VERIFY_(STATUS)

    grid = ESMF_GridCreateHorzLatLonUni(         &
         counts = (/IM_WORLD, JM_WORLD/),        &
         minGlobalCoordPerDim=minCoord(1:2),     &
         deltaPerDim=(/deltaX, deltaY /),        &
         horzStagger=ESMF_Grid_Horz_Stagger_A,   &
         periodic=(/ESMF_TRUE, ESMF_FALSE/),     &
         name='Beatrice', rc=status)
    VERIFY_(STATUS)

    call ESMF_GridAddVertHeight(grid,            &
         delta=(/(deltaZ, L=1,LM) /),            &
         rc=status)
    VERIFY_(STATUS)

    call ESMF_GridDistribute(grid,               &
         deLayout=layout,                        &
         countsPerDEDim1=imxy,                   &
         countsPerDEDim2=jmxy,                   &
         rc=status)
    VERIFY_(STATUS)

    deallocate(imxy)
    deallocate(jmxy)

    RETURN_(STATUS)

  end function MyGridCreate_

end Program utDistIO



