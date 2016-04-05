!
! Simple unit test for CFIO Read/Write Bundle
!

#include "MAPL_Generic.h"

   Program utCFIO

   use ESMF_Mod
   use MAPL_Mod

   implicit NONE

   type(ESMF_Grid)     :: grid
   type (ESMF_VM)      :: VM

   integer             :: nymd, nhms
   type(ESMF_Time)     :: fTime, dTime
   type(ESMF_TimeInterval)  :: fTimeStep, dTimeStep
   type(ESMF_Clock)    :: fClock, dClock

   type(ESMF_FieldBundle)   :: fBundle, dBundle

   type(MAPL_CFIO) :: cfio

!   integer :: IM_WORLD = 72, JM_WORLD = 46, KM_WORLD = 26   
   integer :: IM_WORLD = 540, JM_WORLD = 361, KM_WORLD = 72   
   integer :: i, j, k, im, jm, km                                      ! local

   character(len=*), parameter :: &
       dirname = '.',             &
     fFilename = dirname // '/sample.prs.nc'

   integer :: status, rc
   logical :: IamRoot
   integer, pointer :: resolution(:)
   real,    pointer ::levels(:)

   character(len=*), parameter :: Iam = 'utCFIO'

!                             -----
    
    call test_main()

CONTAINS

    subroutine test_main()

      character(len=ESMF_MAXSTR) :: string
      integer                    :: I

!   Initialize framework
!   --------------------
    call ESMF_Initialize (vm=vm, rc=status)
    VERIFY_(status)

    IamRoot = MAPL_am_I_root()

!   Get the global vm
!   -----------------
    call ESMF_VMGetGlobal(vm, rc=status)
    VERIFY_(status)

    call MAPL_MemUtilsInit( rc=STATUS )
    VERIFY_(STATUS)

!   Create a grid
!   -------------
    grid = MyGridCreate_ ( vm, rc=status )
    VERIFY_(status)

!   Create empty bundles
!   --------------------
!    fBundle = ESMF_FieldBundleCreate ( name='Francesca', grid=grid, rc=status )
!    VERIFY_(status)
    fBundle = ESMF_FieldBundleCreate ( name='PRECIP', grid=grid, rc=status )
    VERIFY_(status)
    dBundle = ESMF_FieldBundleCreate ( name='Denise',    grid=grid, rc=status )
    VERIFY_(status)

!   Set the time as the one on the hardwired file name
!   --------------------------------------------------
    call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeSet( fTime, yy=2006, mm=8, dd=9, h=6, m=30, s=0, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeSet( dTime, yy=2006, mm=8, dd=9,  h=6, m=0, s=0, rc=status )
    VERIFY_(STATUS)
    call ESMF_TimeIntervalSet( fTimeStep, h=6, m=0, s=0, rc=status )
    VERIFY_(STATUS)
    fClock = ESMF_ClockCreate ( name="Clovis", timeStep=fTimeStep, &
                                startTime=fTime, rc=status )
    VERIFY_(STATUS)

!   Read Bundle from file on a clean slate
!   --------------------------------------
    if ( IamRoot ) print *, 'Reading ' // fFilename
    call ESMF_TimeGet  ( fTime, TimeString=string  ,rc=STATUS )
    VERIFY_(STATUS)
    string(11:11)=" "
    if ( IamRoot ) print *, 'time ' // trim(string)


    DO I = 1, 30
       call MAPL_CFIORead( fFilename, fTime, fBundle, NOREAD=.true., RC=STATUS)
       VERIFY_(STATUS)

       if (mod(I,10)==0) then
          call MAPL_MemUtilsWrite( vm, 'noRead', RC=status )
          VERIFY_(STATUS)
       end if

       call MAPL_CFIORead( fFilename, fTime, fBundle, RC=STATUS)
       VERIFY_(STATUS)
       if (mod(I,10)==0) then
          call MAPL_MemUtilsWrite( vm, 'Read', RC=status )
          VERIFY_(STATUS)
       end if
    end DO


#if 0
! this the equivalent of ESMF_ioRead
    call MAPL_cfioRead  ( fFilename, fTime, fBundle, rc=status, &
                        verbose=.true., force_regrid=.true.   )
    VERIFY_(status)

!   Next, create a bundle with same variables as the first one, and use
!    that to determine which variables to read from the second file
!   -------------------------------------------------------------------

    call MAPL_cfioRead  ( fFilename, fTime, dBundle, rc=status, &
                        verbose=.true., noRead = .true.,      &
                        only_vars = 'phis,qv' )

    VERIFY_(status)
    if ( IamRoot ) print *, 'Reading ' // fFilename
    call MAPL_cfioRead  ( fFilename, dTime, dBundle, rc=status, &
                        verbose=.true., force_regrid=.true. )
    VERIFY_(status)

!   Setup data types need for write
!   -------------------------------
    allocate ( resolution(2), levels(KM_WORLD), stat=status )
    VERIFY_(status)
    resolution = (/ IM_WORLD/2, JM_WORLD/2 /)
    levels     = (/ (k, k=1,KM_WORLD) /)

!   Write the same bundle to a differfent file
!   ------------------------------------------
    call MAPL_cfioCreate ( cfio, 'Cindy', fClock, fBundle, fTimeStep, &
         resolution, levels, descr='Bundle Write Test', rc=status )
    VERIFY_(status)

    call MAPL_cfioWrite ( cfio, fClock, fBundle, rc=status, &
                          verbose = .true. )
    VERIFY_(status)

    call MAPL_cfioDestroy ( cfio )
#else
    print *,'calling Finalize'
#endif

!   All done
!   --------
    call ESMF_Finalize ( rc=status )
    VERIFY_(STATUS)
    
  end subroutine test_main

!........................................................................

  function MyGridCreate_ ( vm, rc) result(grid)
    
    type (ESMF_VM),    intent(INOUT) :: VM
    integer, optional, intent(OUT)   :: rc
    type (ESMF_Grid)                 :: grid

! Local vars
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MyGridCreate'

    type(MAPL_MetaComp)          :: MAPL
    type(ESMF_Config)            :: config
!    type(ESMF_VM)                :: vm
    character(len=ESMF_MAXSTR)   :: gridname
    integer                      :: ndes, nx, ny

! fake MAPL
    config = ESMF_ConfigCreate (rc=STATUS )
    VERIFY_(STATUS)

   call ESMF_ConfigLoadFile(config, 'CAP.rc', rc=STATUS )
   VERIFY_(STATUS)
    
!>>>>>>>>>>>>
! We will set-up a somewhat realistic resource "file"

    call ESMF_VmGetCurrent(VM, rc=status)
    VERIFY_(STATUS)
    call ESMF_VmGet(VM, petCount=ndes, rc=status)
    VERIFY_(STATUS)

    nx = ndes
    ny = 1
    call ESMF_ConfigSetAttribute(config, value=nx, Label='NX:', rc=status)
!    VERIFY_(STATUS)
    call ESMF_ConfigSetAttribute(config, value=ny, Label='NY:', rc=status)
!    VERIFY_(STATUS)
!    call ESMF_ConfigSetAttribute(config, value=gridname, Label='GRIDNAME:', rc = status )
    call ESMF_ConfigGetAttribute(config, value=gridname, Label='GRIDNAME:', rc = status )
    VERIFY_(STATUS)
    call ESMF_ConfigSetAttribute(config, value=KM_WORLD, Label='LM:', rc = status )
!    VERIFY_(STATUS)

    call ESMF_ConfigGetAttribute(config, value=nx, Label='NX:', rc=status)
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(config, value=ny, Label='NY:', rc=status)
    VERIFY_(STATUS)
    print *,'GRIDNAME=',trim(gridname)
    print *,'NX=',nx
    print *,'NX=',nY

!<<<<<<<<<<<<   
!  CAP's MAPL MetaComp
!---------------------

    call MAPL_Set (MAPL, name='CAP', cf=CONFIG,    rc=STATUS )
    VERIFY_(STATUS)

! grid create
    call MAPL_GridCreate(MAPLOBJ=MAPL, ESMFGRID=grid, rc=status)
    RETURN_(STATUS)

  end function MyGridCreate_

end Program utCFIO



