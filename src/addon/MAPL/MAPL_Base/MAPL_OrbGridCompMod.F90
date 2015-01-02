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
#include "MAPL_Generic.h"

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: MAPL_OrbGridCompMod - Implements MAPL Orbital Simulator
!
! !INTERFACE:
!
   MODULE MAPL_OrbGridCompMod
!
! !USES:
!
   Use ESMF
   Use MAPL_Mod
  
   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices
!
! !DESCRIPTION: 
!
!  {\tt MAPL_OrbGridComp} is an ESMF gridded component implementing
!  the MAPL-5 Nominal Orbit Calculator.
!
!  Developed for MAPL-5 release Fortuna 2.3 and later.
!
! !REVISION HISTORY:
!
!  30Nov2010 da Silva  Initial version.
!
!EOP
!-------------------------------------------------------------------------

! Legacy state
! ------------
  TYPE Orb_State
     PRIVATE

     type (ESMF_Config)         :: CF           ! Private Config

     integer                             :: no  ! Number of masks
     character(len=ESMF_MAXSTR), pointer :: Instrument(:)
     character(len=ESMF_MAXSTR), pointer :: Satellite(:)
     real, pointer                       :: Swath(:)
     integer, pointer                    :: halo(:)

     logical                    :: verbose=.FALSE.
     real, pointer              :: x(:,:), y(:,:)
     integer                    :: face

  END TYPE Orb_State

! Hook for the ESMF
! -----------------
  TYPE Orb_Wrap
     TYPE (Orb_State), pointer :: PTR => null()
  END TYPE Orb_WRAP




CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices --- Sets IRF services for the Orb Grid Component
!
! !INTERFACE:

   SUBROUTINE SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: Sets Initialize, Run and Finalize services. 
!
! !REVISION HISTORY:
!
!  30Nov2010 da Silva  Initial version.
!
!EOP
!-------------------------------------------------------------------------

!   Local derived type aliases
!   --------------------------
    type (Orb_State), pointer  :: self   ! internal, that is
    type (Orb_wrap)            :: wrap

    character(len=ESMF_MAXSTR) :: comp_name, filename

    integer :: i, nCols
                            __Iam__('SetServices')

!                              ------------

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
    Iam = TRIM(comp_name) // '::' // TRIM(Iam)

!   Greetings
!   ---------
    IF(MAPL_AM_I_ROOT()) THEN
         PRINT *, TRIM(Iam)//': ACTIVE'
    END IF

!   Wrap internal state for storing in GC; rename legacyState
!   -------------------------------------
    allocate ( self, stat=STATUS )
    VERIFY_(STATUS)
    wrap%ptr => self
 
!   Load private Config Attributes
!   ------------------------------
    self%CF = ESMF_ConfigCreate(__RC__)
    call ESMF_ConfigLoadFile ( self%CF,'MAPL_OrbGridComp.rc',rc=status)
    if (status == ESMF_SUCCESS) then 
    
       call ESMF_ConfigGetAttribute(self%CF, self%verbose, Label='verbose:', default=.false. ,  __RC__ )

!                       ------------------------
!                         Get Mask Definitions
!                       ------------------------

       call ESMF_ConfigGetDim(self%CF, self%no, nCols, LABEL='Nominal_Orbits::',__RC__)
       ASSERT_(self%no>0)
       allocate(self%Instrument(self%no), self%Satellite(self%no), & 
          self%Swath(self%no), self%halo(self%no), __STAT__)
       if ( self%verbose .AND. MAPL_AM_I_ROOT() ) then
             write(*,*)"                                   Swath"
             write(*,*)"Instrument          Satellite       (km)        Halo Width"
             write(*,*)"---------------    -----------    ---------    -------------"
       end if
       call ESMF_ConfigFindLabel(self%CF, 'Nominal_Orbits::',__RC__)
       do i = 1, self%no
          call ESMF_ConfigNextLine(self%CF,__RC__)
          call ESMF_ConfigGetAttribute(self%CF,self%Instrument(i),__RC__)
          call ESMF_ConfigGetAttribute(self%CF,self%Satellite(i),__RC__)
          call ESMF_ConfigGetAttribute(self%CF,self%Swath(i),__RC__)
          call ESMF_ConfigGetAttribute(self%CF,self%halo(i),__RC__)
          if ( self%verbose .AND. MAPL_AM_I_ROOT() ) then
             write(*,'(1x,a15,4x,a11,4x,f9.1,4x,i3)') self%Instrument(i), self%Satellite(i), self%Swath(i), self%halo(i)
          end if
       end do
    else
       self%no = 0
    endif

!                       ------------------------
!                       ESMF Functional Services
!                       ------------------------

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize_, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_,       RC=STATUS)
    VERIFY_(STATUS)
        
!   Store internal state in GC
!   --------------------------
    call ESMF_UserCompSetInternalState ( GC, 'Orb_state', wrap, STATUS )
    VERIFY_(STATUS)
  
!                         ------------------
!                         MAPL Data Services
!
    ! in addition to bundle add each instrument as export in case we want to write out in history
    do i=1,self%no
       call MAPL_AddExportSpec(GC, &
        SHORT_NAME     = trim(self%Instrument(i)) , &
        UNITS          = 'days' , &
        DIMS           = MAPL_DimsHorzOnly , &
                RC = STATUS )
    enddo

    call MAPL_AddExportSpec(GC, &
     SHORT_NAME     = 'SATORB', &
     LONG_NAME      = 'Satellite_orbits', &
     UNITS          = 'days' , &
     DIMS           = MAPL_DimsHorzOnly , &
     DATATYPE       = MAPL_BundleItem , &
                RC = STATUS )
    VERIFY_(STATUS)

    call MAPL_TimerAdd (gc,name="Run"     ,rc=status)

!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, __RC__ )
 
!   All done
!   --------
    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE SetServices

!BOP

! !IROUTINE: INITIALIZE_

! !DESCRIPTION: The Initialize method of the orbital component, sets up bundle
!

! !INTERFACE:

  subroutine Initialize_( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Local
    type(ESMF_VM)                           :: VM
    type(ESMF_FIELD)                        :: FIELD
    type(ESMF_GRID)                         :: GRID
    type(ESMF_FIELDBUNDLE)                  :: BUNDLE
    type(Orb_state), pointer           :: self         ! Legacy state
    type(Orb_Wrap)               :: wrap

    integer :: KND, HW, DIMS, LOCATION
    integer :: i
!   New stuff for lat-lon grid needed if doing cube-sphere
    type (MAPL_MetaComp),     pointer   :: MAPL_OBJ
    character(len=ESMF_MAXSTR)    :: gridtype_default
    character(len=ESMF_MAXSTR)    :: gridtype
!   extra things for cubed sphere
    integer                       :: IM, JM, face
    real(ESMF_KIND_R8), pointer                 :: EdgeLons(:,:), EdgeLats(:,:)
! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    call ESMF_GridCompGet ( GC, name=COMP_NAME, VM=VM, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME)//'::Initialize_'

    call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_UserCompGetInternalState(gc, 'Orb_state', WRAP, STATUS)
    VERIFY_(STATUS)
    self => wrap%ptr

    if (self%no == 0) then
       RETURN_(ESMF_SUCCESS)
    endif

    call ESMF_StateGet(EXPORT,'SATORB',BUNDLE,RC=STATUS)
    VERIFY_(STATUS)
  
    call ESMF_GridCompGet ( GC, grid=GRID, RC=STATUS)
    VERIFY_(STATUS)

    ! set some info about the fields we will be adding . . .
    HW=0
    DIMS=MAPL_DimsHorzOnly
    LOCATION=MAPL_VLocationCenter
    KND=MAPL_R4
    do i = 1, self%no
     field=mapl_FieldCreateEmpty(trim(self%Instrument(i)),Grid,RC=STATUS)
     VERIFY_(STATUS)
     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, hw=hw, RC=STATUS)
     VERIFY_(STATUS)
     call MAPL_FieldBundleAdd(Bundle,Field,RC=STATUS)
     VERIFY_(STATUS)
    enddo

    ! find out what type of grid we are on, if so 
    gridtype_default='Lat-Lon'
    call ESMF_AttributeGet(Grid,'GridType',gridtype,gridtype_default)
    if (gridtype=='Cubed-Sphere') then

       call MAPL_GetObjectFromGC(GC,MAPL_OBJ,rc=status)
       VERIFY_(STATUS)
       call MAPL_Get(MAPL_OBJ, im=im, jm=jm, rc=status)
       VERIFY_(STATUS)
 
       allocate(EdgeLons(IM+1,JM+1),stat=status)
       VERIFY_(STATUS)
       allocate(EdgeLats(IM+1,JM+1),stat=status)
       VERIFY_(STATUS)
       call MAPL_GridGet(Grid,gridCornerLons=EdgeLons,gridCornerLats=EdgeLats,rc=status)
       VERIFY_(STATUS)
       call check_face(IM+1,JM+1,EdgeLons,EdgeLats,FACE)
       self%face=face
       allocate(self%x(IM+1,JM+1),self%y(IM+1,JM+1))
       call cube_xy(IM+1,JM+1,self%x,self%y,EdgeLons,EdgeLats,face)
       deallocate(EdgeLons)
       deallocate(EdgeLats)

    endif

    RETURN_(ESMF_SUCCESS)

    end subroutine Initialize_
!BOP
!
! !IROUTINE:  Run_ --- Runs Orb
!
! !INTERFACE:
!

   SUBROUTINE Run_ ( gc, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, optional ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 
  ! local
  type (ESMF_VM)                      :: VM
  type (MAPL_MetaComp),     pointer   :: MAPL_OBJ
  integer                             :: IM,JM,LM
  real, pointer, dimension(:,:)       :: LONS
  real, pointer, dimension(:,:)       :: LATS
  real, pointer, dimension(:,:)       :: PTR_TMP, PTR_TMP_EX
  integer                             :: iyr,imm,idd,ihr,imn,isc
  type(ESMF_TimeInterval)             :: timeinterval
  type(ESMF_Time)                     :: IntervalTime
  integer, dimension(2)               :: interval_nymd, interval_nhms, ihalo, jhalo ! dates and halos for masking routine
  integer                             :: deltat ! sub interval for satellite propagation
  real, dimension(3)                  :: swath ! satellite swath for masking routine
  real                                :: undef ! undefined for masking routine
  character(len=ESMF_MAXSTR)          :: sat_name     ! Satellite name
  character(len=ESMF_MAXSTR)          :: grid_name

  type(Orb_state), pointer     :: self     ! Legacy state

  type(ESMF_Grid)               :: Grid        ! Grid
  type(ESMF_Time)               :: Time     ! Current time
  type(ESMF_Config)             :: CF          ! Universal Config 

  integer                       :: i, j, it, iim, ic, k, km, nymd, nhms  ! date, time

  character(len=ESMF_MAXSTR)    :: comp_name
  character(len=ESMF_MAXSTR)    :: gridtype_default
  character(len=ESMF_MAXSTR)    :: gridtype

  type(ESMF_FieldBundle)        :: BUNDLE
  integer                       :: NORB
  integer                       :: IM_world,JM_world,counts(5),imsize
 
                                __Iam__('Run_')

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, VM=VM, __RC__ )
   Iam = trim(comp_name) // '::Run'

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, CLOCK, self, GRID, CF, time, nymd, nhms, timeinterval,  __RC__)

   if (self%no == 0) then
      RETURN_(ESMF_SUCCESS)
   endif

   call MAPL_GetObjectFromGC ( GC, MAPL_OBJ, RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_TimerOn(MAPL_OBJ,"Run")
   call MAPL_Get(MAPL_OBJ,            &
        IM                  = IM,     &
        JM                  = JM,     &
        LM                  = LM,     &
        LONS     = LONS,              &
        LATS     = LATS,              &
        RC=STATUS )
   VERIFY_(STATUS)

!  Figure out what type of grid we are on 

   gridtype_default='Lat-Lon'
   call ESMF_AttributeGet(Grid,'GridType',gridtype,gridtype_default)

!  Get the time interval, and start and end time
!   timeinterval=timeinterval/2
!   IntervalTime=time-timeinterval
   IntervalTime=time
   call ESMF_TimeGet(IntervalTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
   call MAPL_PackTime(nymd,iyr,imm,idd)
   call MAPL_PackTime(nhms,ihr,imn,isc)
   interval_nymd(1)=nymd
   interval_nhms(1)=nhms
   IntervalTime=time+timeinterval
   call ESMF_TimeGet(IntervalTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
   call MAPL_PackTime(nymd,iyr,imm,idd)
   call MAPL_PackTime(nhms,ihr,imn,isc)
   interval_nymd(2)=nymd
   interval_nhms(2)=nhms

!  set swath to zero for now
   swath=0.
   call MAPL_GridGet(GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
   VERIFY_(STATUS)
   IM_world = counts(1)
   JM_world = counts(2)
   if (JM_world == 6*IM_world) then
      imsize = 4*IM_World
   else
      imsize = IM_World
   endif
!  swatch(3) is actually the size of the length to use for interpolation
   if( imsize.le.200       ) call ESMF_ConfigGetAttribute(self%CF,swath(3),LABEL='INTERPOLATION_WIDTH:', DEFAULT= 10.0 ,RC=STATUS)
   if( imsize.gt.200 .and. &
       imsize.le.400       ) call ESMF_ConfigGetAttribute(self%CF,swath(3),LABEL='INTERPOLATION_WIDTH:', DEFAULT= 5.0 ,RC=STATUS)
   if( imsize.gt.400 .and. &
       imsize.le.800       ) call ESMF_ConfigGetAttribute(self%CF,swath(3),LABEL='INTERPOLATION_WIDTH:', DEFAULT= 2.0 ,RC=STATUS)
   if( imsize.gt.800 .and. &
       imsize.le.1600      ) call ESMF_ConfigGetAttribute(self%CF,swath(3),LABEL='INTERPOLATION_WIDTH:', DEFAULT=  1.0 ,RC=STATUS)
   if( imsize.gt.1600      ) call ESMF_ConfigGetAttribute(self%CF,swath(3),LABEL='INTERPOLATION_WIDTH:', DEFAULT=  0.5 ,RC=STATUS)

!  define undef
   undef=MAPL_UNDEF

!  set deltat in seconds
   deltat=10

!  get orb bundle
   call ESMF_StateGet(EXPORT,'SATORB',BUNDLE,RC=STATUS)
   VERIFY_(STATUS)
   call ESMF_FieldBundleGet(BUNDLE,fieldCOUNT=NORB,RC=STATUS)
   VERIFY_(STATUS)
   ASSERT_(NORB==self%no)

!  loop over each satellite and get it's mask
   do k=1,NORB
    call ESMFL_BundleGetPointerToData(BUNDLE,trim(self%instrument(k)),PTR_TMP,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,PTR_TMP_EX,trim(self%instrument(k)),RC=STATUS)
    VERIFY_(STATUS)
    sat_name=trim(self%Satellite(k))
    swath(1)=self%swath(k)
    swath(2)=self%swath(k)
    ihalo = self%halo(k)
    jhalo = self%halo(k)
    if (gridtype == 'Lat-Lon') then
     if (associated(PTR_TMP)) call DoMasking_ (PTR_TMP, im, jm, lons, lats, undef, &
                              sat_name, interval_nymd, interval_nhms, deltat, swath,  &
                              ihalo, jhalo, rc=status )
    else if (gridtype == 'Cubed-Sphere') then
     if (associated(PTR_TMP)) call DoMasking_CS (PTR_TMP, im, jm, self%x, self%y, undef, &
                              sat_name, interval_nymd, interval_nhms, deltat, swath,  &
                              ihalo, jhalo, self%face, rc=status )
    endif
    ! if HISTORY is asking for mask to write this will be allocated
    if (associated(PTR_TMP_EX)) PTR_TMP_EX=PTR_TMP 
    if (associated(PTR_TMP)) nullify(PTR_TMP)
    if (associated(PTR_TMP_EX)) nullify(PTR_TMP_EX)
 
   enddo

!  All done
!  --------
   call MAPL_TimerOff(MAPL_OBJ,"Run")
   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Run_

!.......................................................................

    subroutine extract_ ( GC, CLOCK, self, GRID, CF, time, nymd, nhms, timeinterval, rc)

    type(ESMF_GridComp), intent(INout)  :: GC           ! Grid Comp object
    type(ESMF_Clock), intent(in)        :: CLOCK        ! Clock

    type(Orb_state), pointer           :: self         ! Legacy state
    type(ESMF_Grid),     intent(out)    :: GRID         ! Grid
    type(ESMF_Config),   intent(out)    :: CF           ! Universal Config 
    type(ESMF_TIME), intent(out)        :: Time         ! Time
    type(ESMF_TimeInterval), intent(out) :: TimeInterval ! Time Intervale
    integer, intent(out)                :: nymd, nhms   ! date, time
    integer, intent(out), optional      :: rc

!                                      ---

    character(len=ESMF_MAXSTR) :: comp_name
    
                                 __Iam__('extract_')

    type(MAPL_MetaComp), pointer  :: MC
    type(Orb_Wrap)               :: wrap
    integer                       :: iyr, imm, idd, ihr, imn, isc

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, NAME=comp_name, __RC__ )
    Iam = trim(COMP_NAME) // '::extract_'

    rc = 0

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(gc, 'Orb_state', WRAP, STATUS)
    VERIFY_(STATUS)
    self => wrap%ptr

!   Get the configuration
!   ---------------------
    call ESMF_GridCompGet ( GC, config=CF, __RC__ )

!   Extract time as simple integers from clock
!   ------------------------------------------
    call ESMF_ClockGet(CLOCK,currTIME=TIME,timeStep=TimeInterval,__RC__)
    call ESMF_TimeGet(TIME ,yy=iyr, mm=imm, dd=idd, h=ihr, m=imn, s=isc, __RC__)
    call MAPL_PackTime(nymd,iyr,imm,idd)
    call MAPL_PackTime(nhms,ihr,imn,isc)

!   Extract the ESMF Grid
!   ---------------------
    call ESMF_GridCompGet ( GC, grid=GRID, __RC__)

    RETURN_(ESMF_SUCCESS)

   end subroutine extract_


       subroutine DoMasking_ (field, im, jm, lons, lats, undef, &
                              sat_name, nymd, nhms, dt, swath,  &
                              ihalo, jhalo, rc )

       use MAPL_NominalOrbitsMod

       implicit NONE

! !INPUT PARAMETERS:

       integer, intent(in)            :: im, jm
       integer, intent(in)            :: ihalo(2), jhalo(2)
       real, intent(in) :: lons(im,jm)
       real, intent(in) :: lats(im,jm)
       real, intent(in) :: swath(3)
       real, intent(in) :: undef

       character(len=*), intent(in) :: sat_name     ! Satellite name
       integer, intent(in) :: dt      ! delta t in secs
       integer, intent(in) :: nymd(2) ! Beginning/ending date: YYYYMMDD
       integer, intent(in) :: nhms(2) ! Beginning/ending time: HHMMSS

! !OUTPUT PARAMETERS:

       real,    intent(inout) :: field(im,jm)
       integer, intent(out), optional   :: rc    ! Error return code
                                       ! = 0  all is well
                                       ! = 3  memory allocation error

!                               ----

       INTEGER, PARAMETER :: dp=SELECTED_REAL_KIND(15,307)

!      Local workspace
!      ---------------
       integer :: isegs, jsegs, segs, mask(im,jm)
       real(dp)::  V_mean, xlons(im), xlats(jm)
       real(dp) :: SwathWidth(2) = (/ 0.0, 0.0 /)

       integer i,j, nobs
       real(dp), pointer :: tlons(:)   => null()
       real(dp), pointer :: tlats(:)   => null()
       real(dp), pointer :: slons(:,:) => null()
       real(dp), pointer :: slats(:,:) => null()

       SwathWidth(1:2) = swath(1:2) ! type conversion

!      Interpoaltion parameters; swath(3) is the swath resolution, in km
!      -----------------------------------------------------------------
       V_mean = 7.5             ! Mean sat speed in km/s; assumes a 90 minute period
                                ! this should be a function of satellite.
       isegs = nint((swath(1)+swath(2)) / swath(3) ) ! segments across-track
       jsegs =  V_mean * dt / swath(3)               ! segments along-track

!      Simple masking without swath
!      ----------------------------
       if ( swath(1) .eq. 0 .AND. swath(2) .eq. 0 ) then

!         Compute tracks
!         --------------
          call orbits_track(tlons,tlats, sat_name, nymd, nhms, dt, rc)

!         Simple masking
!         --------------
          mask=0
          call orb_mask_lonlat(mask,im,jm,lons,lats,tlons,tlats,size(tlons),jsegs,-180.,180.)
 
          deallocate(tlons,tlats)

       else

!         Compute tracks
!         --------------
          call orbits_swath(slons,slats, sat_name, nymd, nhms, dt,SwathWidth, rc, wrap=.true.)

!         Swath masking
!         -------------
          nobs = size(slons)/3
          mask = 0

          call orb_swath_mask_lonlat(mask,im,jm,lons,lats,slons,slats,nobs,isegs,jsegs,-180.,180.)

          deallocate(slons,slats)

       end if

!      Add optional halo
!      -----------------
       if ( (sum(ihalo) + sum(jhalo)) > 0) then
          call orb_halo(im,jm,mask,ihalo,jhalo)
       end if

!      Apply mask to field
!      -------------------
       field = 1.0
       where ( mask /= 1 ) field = undef

!      All done
!      --------
     end subroutine DoMasking_

       subroutine DoMasking_CS (field, im, jm, x, y, undef, &
                              sat_name, nymd, nhms, dt, swath,  &
                              ihalo, jhalo, face, rc )

       use MAPL_NominalOrbitsMod

       implicit NONE

! !INPUT PARAMETERS:

       integer, intent(in)            :: im, jm
       integer, intent(in)            :: ihalo(2), jhalo(2)
       real, intent(in) :: x(im+1,jm+1)
       real, intent(in) :: y(im+1,jm+1)
       real, intent(in) :: swath(3)
       real, intent(in) :: undef

       character(len=*), intent(in) :: sat_name     ! Satellite name
       integer, intent(in) :: dt      ! delta t in secs
       integer, intent(in) :: nymd(2) ! Beginning/ending date: YYYYMMDD
       integer, intent(in) :: nhms(2) ! Beginning/ending time: HHMMSS
       integer, intent(in) :: face
 
! !OUTPUT PARAMETERS:

       real,    intent(inout) :: field(im,jm)
       integer, optional   :: rc    ! Error return code
                                       ! = 0  all is well
                                       ! = 3  memory allocation error

!                               ----

       INTEGER, PARAMETER :: dp=SELECTED_REAL_KIND(15,307)

!      Local workspace
!      ---------------
       integer :: isegs, jsegs, segs, mask(im,jm)
       real(dp)::  V_mean
       real(dp) :: SwathWidth(2) = (/ 0.0, 0.0 /)

       integer i,j, nobs, status
       real(dp), pointer :: tlons(:)   => null()
       real(dp), pointer :: tlats(:)   => null()
       real(dp), pointer :: slons(:,:) => null()
       real(dp), pointer :: slats(:,:) => null()

       character*(12)       :: Iam="DoMasking_CS"

       SwathWidth(1:2) = swath(1:2) ! type conversion

!      Interpoaltion parameters; swath(3) is the swath resolution, in km
!      -----------------------------------------------------------------
       V_mean = 7.5             ! Mean sat speed in km/s; assumes a 90 minute period
                                ! this should be a function of satellite.
       isegs = nint((swath(1)+swath(2)) / swath(3) ) ! segments across-track
       jsegs =  V_mean * dt / swath(3)               ! segments along-track

!      Simple masking without swath
!      ----------------------------
       if ( swath(1) .eq. 0 .AND. swath(2) .eq. 0 ) then
          mask = 0
!         Compute tracks
!         --------------
          call orbits_track(tlons,tlats, sat_name, nymd, nhms, dt, rc)

!         Simple masking
!         --------------
          call orb_mask_xy(mask,im,jm,x,y,tlons,tlats,size(tlons),jsegs,0.,360.,face,rc=status)
!          VERIFY_(STATUS)

          deallocate(tlons,tlats)

       else

          mask = 0
!         Compute tracks
!         --------------
          call orbits_swath(slons,slats, sat_name, nymd, nhms, dt,SwathWidth, rc, wrap=.true.)

!         Swath masking
!         -------------
          nobs = size(slons)/3
          call orb_swath_mask_xy(mask,im,jm,x,y,slons,slats,nobs,isegs,jsegs,0.,360.,face)


          deallocate(slons,slats)

       end if

!      Add optional halo
!      -----------------
       if ( (sum(ihalo) + sum(jhalo)) > 0) then
          call orb_halo(im,jm,mask,ihalo,jhalo)
       end if

!      Apply mask to field
!      -------------------
       field = 1.0
       where ( mask /= 1 ) field = undef

!      All done
!      --------
     end subroutine DoMasking_CS


      subroutine flatten_latlon(lats,lons,lats_1d,lons_1d,im,jm)
      implicit none
      integer, intent(in) :: im,jm
      real, intent(in) :: lats(im,jm), lons(im,jm)
      real, intent(inout) :: lats_1d(jm), lons_1d(im)
      integer :: i
      do i =1,im
       lons_1d(i)=lons(i,1)*180./MAPL_PI
      enddo
      do i =1,jm
       lats_1d(i)=lats(1,i)*180./MAPL_PI
      enddo
      return
      end subroutine flatten_latlon

      subroutine flatten_xy(x,y,x_1d,y_1d,im,jm,im_1d,jm_1d,switch)
      implicit none
      integer, intent(in) :: im,jm, im_1d, jm_1d
      real, intent(in) :: x(im,jm), y(im,jm)
      real, intent(inout) :: y_1d(jm_1d), x_1d(im_1d)
      logical, intent(in) :: switch
      integer :: i
      if (.not.switch) then
       do i =1,im_1d
        x_1d(i)=x(i,1)
       enddo
       do i =1,jm_1d
        y_1d(i)=y(1,i)
       enddo
      else if (switch) then
       do i =1,im_1d
        x_1d(i)=x(1,i)
       enddo
       do i =1,jm_1d
        y_1d(i)=y(i,1)
       enddo
      endif 
      return
      end subroutine flatten_xy

      subroutine orb_edges_1d(ecoords,coords,idim)
      implicit NONE
      integer, intent(in) :: idim
      real, intent(in)  :: coords(idim)
      real, intent(out) :: ecoords(idim+1)
      ecoords(1)  = coords(1) - 0.5 * ( coords(2) - coords(1) )
      ecoords(2:idim) = 0.5 * ( coords(1:idim-1)+coords(2:idim) )
      ecoords(idim+1) = coords(idim) + 0.5 * (coords(idim) - coords(idim-1))
      return
      end subroutine orb_edges_1d

      subroutine orb_mask_xy(mask,im,jm,x,y,tlons,tlats,nobs,jsegs,lb,ub,face,rc)
      implicit NONE
      INTEGER, PARAMETER :: dp=SELECTED_REAL_KIND(15,307)

      integer, intent(in) :: im, jm, nobs, jsegs
      real, intent(in) :: x(im+1,jm+1)
      real, intent(in) :: y(im+1,jm+1)
      real, intent(in) :: lb,ub
      integer, intent(in) :: face
      integer, optional :: rc

      real, pointer  :: ex(:), ey(:)
      real(dp) :: tlons(nobs), tlats(nobs) 
      real(dp) :: beta, d2r, dx, dy, tol = 0.1
      integer, intent(out) :: mask(im,jm)
      real :: x_loc, y_loc
     
      integer i, j, m, n, nfail, inbox, imp1, jmp1, face_pnt, status
      integer im_1d, jm_1d, itmp
      logical periodic,switch
      real :: wcorner_x(4),wcorner_y(4) ! corners of world for this proc
      real :: lat,lon
      character*(11)       :: Iam="orb_mask_xy"
 
      switch = .false.
      if ( abs(x(1,1)-x(2,1)) < abs(x(1,1)-x(1,2)) ) switch = .true.
      if (.not.switch) then
        allocate(ex(im+1),ey(jm+1))
        im_1d = im+1
        jm_1d = jm+1
        imp1 = im+1
        jmp1 = jm+1
      endif
      if (switch) then
       allocate(ex(jm+1),ey(im+1))
       im_1d = jm+1
       jm_1d = im+1
       imp1 = im+1
       jmp1 = jm+1
      end if
      call flatten_xy(x,y,ex,ey,imp1,jmp1,im_1d,jm_1d,switch)
      nfail = 0
!     loop over each point
!     first define corners of the world
      wcorner_x(1)=minval(ex)
      wcorner_x(4)=minval(ex)
      wcorner_x(2)=maxval(ex)
      wcorner_x(3)=maxval(ex)
      wcorner_y(1)=minval(ey)
      wcorner_y(4)=maxval(ey)
      wcorner_y(2)=minval(ey)
      wcorner_y(3)=maxval(ey)

      do n = 2, nobs
        do m = 1, jsegs ! interpolate track along orbit for better resoluation between points
         beta = (m - 1.0 ) / ( jsegs - 1.0 )
         if ( abs(tlons(n)-tlons(n-1))<180. ) then
          lon = (1.0-beta) * tlons(n-1) + beta * tlons(n)
         else if ( tlons(n) > tlons(n-1) ) then
          lon = (1.0-beta) * tlons(n-1) + beta * (tlons(n)-360.)
         else
          lon = (1.0-beta) * (tlons(n-1)-360.) + beta * tlons(n)
         end if
         if (lon < lb) lon=lon+360.
         if (lon > ub) lon=lon-360.
         lat = (1.0-beta) * tlats(n-1) + beta * tlats(n)
!        check if lat and lon is in the piece of world on this processor
         lat = lat * MAPL_PI/180.0
         lon = lon * MAPL_PI/180.0
         call check_face_pnt(LON,LAT,face_pnt)
         if (face_pnt == face) then
          call cube_xy_point(x_loc,y_loc,LAT,LON,face_pnt)
          inbox = pnt_in_rect(x_loc,y_loc,wcorner_x,wcorner_y)
          ! if we found the point in the box
          if (inbox == 1) then
           i = ijsearch(ex,im_1d,x_loc,.false.)
           j = ijsearch(ey,jm_1d,y_loc,.false.)
           ! fill in mask for i,j
             if (switch) then
                itmp = i
                i = j
                j = itmp
             endif
             if ( i>0 .and. j>0 .and. i<=im .and. j<=jm) then
               mask(i,j)=1
             end if
          endif
         endif
       enddo
      enddo
      deallocate(ex,ey)

      end subroutine orb_mask_xy

      subroutine orb_mask_lonlat(mask,im,jm,lons,lats,tlons,tlats,nobs,jsegs,lb,ub)
      implicit NONE
      INTEGER, PARAMETER :: dp=SELECTED_REAL_KIND(15,307)

      integer, intent(in) :: im, jm, nobs, jsegs
      real, intent(in) :: lons(im,jm)
      real, intent(in) :: lats(im,jm)
      real, intent(in) :: lb,ub

      real :: lons_1d(im), lats_1d(jm)
      real :: elons(im+1), elats(jm+1)
      real(dp) :: tlons(nobs), tlats(nobs) 
      real(dp) :: beta, d2r, dx, dy, tol = 0.1
      integer, intent(out) :: mask(im,jm)

     
      integer i, j, m, n, nfail, inbox, imp1, jmp1
      logical periodic
      real :: wcorner_lat(4),wcorner_lon(4) ! corners of world for this proc
      real :: lat,lon
 
!     Build edge coords
!     -----------------
      call flatten_latlon(lats,lons,lats_1d,lons_1d,im,jm) 
      call orb_edges_1d(elons,lons_1d,im)
      call orb_edges_1d(elats,lats_1d,jm)
!     since we will need these
      imp1=im+1
      jmp1=jm+1

!     first define corners of the world
      wcorner_lat(1)=elats(1)
      wcorner_lat(4)=elats(jmp1)
      wcorner_lat(2)=elats(1)
      wcorner_lat(3)=elats(jmp1)
      wcorner_lon(1)=elons(1)
      wcorner_lon(4)=elons(1)
      wcorner_lon(2)=elons(imp1)
      wcorner_lon(3)=elons(imp1)

      do n = 2, nobs
        do m = 1, jsegs ! interpolate track along orbit for better resoluation between points
         beta = (m - 1.0 ) / ( jsegs - 1.0 )
         if ( abs(tlons(n)-tlons(n-1))<180. ) then
          lon = (1.0-beta) * tlons(n-1) + beta * tlons(n)
         else if ( tlons(n) > tlons(n-1) ) then
          lon = (1.0-beta) * tlons(n-1) + beta * (tlons(n)-360.)
         else
          lon = (1.0-beta) * (tlons(n-1)-360.) + beta * tlons(n)
         end if
         if (lon < lb) lon=lon+360.
         if (lon > ub) lon=lon-360.
         lat = (1.0-beta) * tlats(n-1) + beta * tlats(n)
!        check if lat and lon is in the piece of world on this processor
         inbox = pnt_in_rect(lat,lon,wcorner_lat,wcorner_lon)
         ! if we found the point in the box
         if (inbox == 1) then
           i = ijsearch(elons,im+1,lon,.false.)
           j = ijsearch(elats,jm+1,lat,.false.)
           ! fill in mask for i,j
           if (i >0 .and. j > 0) then
            mask(i,j)=1
           endif
         endif
       enddo
      enddo
      end subroutine orb_mask_lonlat

      subroutine orb_swath_mask_xy(mask,im,jm,x,y,slons,slats,nobs,isegs,jsegs,lb,ub,face)
      implicit NONE
      INTEGER, PARAMETER :: dp=SELECTED_REAL_KIND(15,307)

      integer, intent(in) :: im, jm, nobs, isegs, jsegs
      real, intent(in) :: x(im+1,jm+1)
      real, intent(in) :: y(im+1,jm+1)
      real, intent(in) :: lb,ub
      integer, intent(in) :: face

      real, pointer :: ex(:), ey(:)
      real(dp) :: slons(3,nobs), slats(3,nobs) 
      real(dp) :: alpha, beta, d2r, r2d, lon1, lon2, lat1, lat2
      real(dp) :: dx, dy, tol = 0.1
      integer, intent(out) :: mask(im,jm)
      real :: x_loc, y_loc
     
      integer :: i, j, k, m, n, nfail, imp1, jmp1, inbox, itmp
      integer :: im_1d, jm_1d
      logical periodic
      real :: wcorner_x(4),wcorner_y(4)
      real :: lat,lon
      real :: sdnom1,sdnom2,eplonl1,eplonl2,eplonr1,eplonr2
      real :: eplatl1,eplatl2,eplatr1,eplatr2
      real :: sp1,sp2
      real :: sdnom,eplon1,eplon2,eplat1,eplat2
      real :: latf
      integer :: face_pnt, status
      logical :: switch

      d2r = MAPL_PI/180.
      r2d = 180./MAPL_PI
!     find indices have constant values of coordinate
      switch = .false.
      if ( abs(x(1,1)-x(2,1)) < abs(x(1,1)-x(1,2)) ) switch = .true.
      if (.not.switch) then
         allocate(ex(im+1),ey(jm+1))
         im_1d = im+1
         jm_1d = jm+1
         imp1=im+1
         jmp1=jm+1
      endif
      if (switch) then
         allocate(ex(jm+1),ey(im+1))
         im_1d = jm+1
         jm_1d = im+1
         imp1=im+1
         jmp1=jm+1
      end if
      call flatten_xy(x,y,ex,ey,imp1,jmp1,im_1d,jm_1d,switch)

!     first define corners of the world
      wcorner_x(1)=minval(ex)
      wcorner_x(4)=minval(ex)
      wcorner_x(2)=maxval(ex)
      wcorner_x(3)=maxval(ex)
      wcorner_y(1)=minval(ey)
      wcorner_y(4)=maxval(ey)
      wcorner_y(2)=minval(ey)
      wcorner_y(3)=maxval(ey)
      do k = 1, isegs   ! cros-track
         alpha = (k - 1.0 ) / ( isegs - 1.0 )
         do n = 2, nobs
            if (abs(slons(1,n-1)-slons(3,n-1)) < 180.) then
             lon1 = (1.0-alpha) * slons(1,n-1) + alpha * slons(3,n-1)  
             eplonl1 = slons(1,n-1)
             eplonr1 = slons(3,n-1)
            else if (slons(1,n-1) > slons(3,n-1)) then
             lon1 = (1.0-alpha) * slons(1,n-1) + alpha * (slons(3,n-1)+360.)
             eplonl1 = slons(1,n-1)
             eplonr1 = slons(3,n-1)+360.
            else
             lon1 = (1.0-alpha) * (slons(1,n-1)+360.) + alpha * slons(3,n-1)  
             eplonl1 = slons(1,n-1)+360.
             eplonr1 = slons(3,n-1)
            endif
            if (abs(slons(1,n)-slons(3,n)) < 180.) then
             lon2 = (1.0-alpha) * slons(1,n) + alpha * slons(3,n)  
             eplonl2 = slons(1,n)
             eplonr2 = slons(3,n)
            else if (slons(1,n) > slons(3,n)) then
             lon2 = (1.0-alpha) * slons(1,n) + alpha * (slons(3,n)+360.)
             eplonl2 = slons(1,n)
             eplonr2 = slons(3,n)+360.
            else
             lon2 = (1.0-alpha) * (slons(1,n)+360.) + alpha * slons(3,n)
             eplonl2 = slons(1,n)+360.
             eplonr2 = slons(3,n)
            endif

            ! interpolate along great circle unless endpoints of interpolation have same lon
            eplatl1 = slats(1,n-1) 
            eplatr1 = slats(3,n-1) 
            eplatl2 = slats(1,n) 
            eplatr2 = slats(3,n) 
            sdnom1 = sin((eplonl1-eplonr1)*d2r)
            sdnom2 = sin((eplonl2-eplonr2)*d2r)  
            if (abs(sdnom1) /= 0.) then
             sp1 = sin((lon1-eplonr1)*d2r)/sdnom1
             sp2 = sin((lon1-eplonl1)*d2r)/sdnom1
             lat1 = atan(tan(eplatl1*d2r)*sp1 - tan(eplatr1*d2r)*sp2)
             lat1 = lat1*r2d
            else
             lat1 = (1.0-alpha) * slats(1,n-1) + alpha * slats(3,n-1)  
            endif
            if (abs(sdnom2) /= 0.) then
             sp1 = sin((lon2-eplonr2)*d2r)/sdnom2
             sp2 = sin((lon2-eplonl2)*d2r)/sdnom2
             lat2 = atan(tan(eplatl2*d2r)*sp1 - tan(eplatr2*d2r)*sp2)
             lat2 = lat2*r2d 
            else
             lat2 = (1.0-alpha) * slats(1,n) + alpha * slats(3,n) 
            endif

            do m = 1, jsegs ! along track refinement
               beta = (m - 1.0 ) / ( jsegs - 1.0 )
               if (abs(lon2-lon1) < 180.) then
                lon = (1.0-beta) * lon1 + beta * lon2
                eplon1=lon1
                eplon2=lon2
               else if (lon2 > lon1) then
                lon = (1.0-beta) * (lon1+360.) + beta * lon2
                eplon1=lon1+360.
                eplon2=lon2
               else
                lon = (1.0-beta) * lon1 + beta * (lon2+360.)
                eplon1=lon1
                eplon2=lon2+360.
               endif
               eplat1=lat1
               eplat2=lat2
               sdnom=sin((eplon1-eplon2)*d2r)
               if (abs(sdnom) /= 0. ) then
                sp1=sin((lon-eplon2)*d2r)/sdnom 
                sp2=sin((lon-eplon1)*d2r)/sdnom
                latf = atan(tan(eplat1*d2r)*sp1-tan(eplat2*d2r)*sp2)
                latf = latf*r2d
                lat = latf
               else 
                lat = (1.0-beta) * lat1 + beta * lat2
               endif
               if (lon < lb) lon=lon+360.
               if (lon > ub) lon=lon-360.

               lat = lat * MAPL_PI/180.0
               lon = lon * MAPL_PI/180.0
               call check_face_pnt(LON,LAT,face_pnt)
               if (face_pnt == face) then
                call cube_xy_point(x_loc,y_loc,LAT,LON,face)
                inbox = pnt_in_rect(x_loc,y_loc,wcorner_x,wcorner_y)
                if (inbox == 1) then
                 i = ijsearch(ex,im_1d,x_loc,.false.)
                 j = ijsearch(ey,jm_1d,y_loc,.false.)
                 if (switch) then 
                    itmp = i
                    i = j
                    j = itmp
                 endif
                 if ( i>0 .and. j>0 .and. i<=im .and. j<=jm) then
                   mask(i,j)=1
                 end if
                end if
               endif
            end do ! msegs
         end do    ! nobs
      end do       ! ksegs

      deallocate(ex,ey)

      end subroutine orb_swath_mask_xy
!...........................................................................................

      subroutine orb_swath_mask_lonlat(mask,im,jm,lons,lats,slons,slats,nobs,isegs,jsegs,lb,ub)
      implicit NONE
      INTEGER, PARAMETER :: dp=SELECTED_REAL_KIND(15,307)

      integer, intent(in) :: im, jm, nobs, isegs, jsegs
      real, intent(in) :: lons(im,jm)
      real, intent(in) :: lats(im,jm)
      real, intent(in) :: lb,ub

      real :: lons_1d(im),lats_1d(jm)
      real :: elons(im+1), elats(jm+1)
      real(dp) :: slons(3,nobs), slats(3,nobs) 
      real(dp) :: alpha, beta, d2r, r2d, lon1, lon2, lat1, lat2
      real(dp) :: dx, dy, tol = 0.1
      integer, intent(out) :: mask(im,jm)

     
      integer i, j, k, m, n, nfail, imp1, jmp1, inbox
      logical periodic
      real :: wcorner_lat(4),wcorner_lon(4)
      real :: lat,lon
      real :: sdnom1,sdnom2,eplonl1,eplonl2,eplonr1,eplonr2
      real :: eplatl1,eplatl2,eplatr1,eplatr2
      real :: sp1,sp2
      real :: sdnom,eplon1,eplon2,eplat1,eplat2
      real :: latf

      d2r = MAPL_PI/180.
      r2d = 180./MAPL_PI

!     Build edge coords
!     -----------------
      call flatten_latlon(lats,lons,lats_1d,lons_1d,im,jm)
      call orb_edges_1d(elons,lons_1d,im)
      call orb_edges_1d(elats,lats_1d,jm)
      imp1=im+1
      jmp1=jm+1
      nfail = 0
!     first define corners of the world
      wcorner_lat(1)=elats(1)
      wcorner_lat(4)=elats(jmp1)
      wcorner_lat(2)=elats(1)
      wcorner_lat(3)=elats(jmp1)
      wcorner_lon(1)=elons(1)
      wcorner_lon(4)=elons(1)
      wcorner_lon(2)=elons(imp1)
      wcorner_lon(3)=elons(imp1)
      do k = 1, isegs   ! cros-track
         alpha = (k - 1.0 ) / ( isegs - 1.0 )
         do n = 2, nobs
            if (abs(slons(1,n-1)-slons(3,n-1)) < 180.) then
             lon1 = (1.0-alpha) * slons(1,n-1) + alpha * slons(3,n-1)  
             eplonl1 = slons(1,n-1)
             eplonr1 = slons(3,n-1)
            else if (slons(1,n-1) > slons(3,n-1)) then
             lon1 = (1.0-alpha) * slons(1,n-1) + alpha * (slons(3,n-1)+360.)
             eplonl1 = slons(1,n-1)
             eplonr1 = slons(3,n-1)+360.
            else
             lon1 = (1.0-alpha) * (slons(1,n-1)+360.) + alpha * slons(3,n-1)  
             eplonl1 = slons(1,n-1)+360.
             eplonr1 = slons(3,n-1)
            endif
            if (abs(slons(1,n)-slons(3,n)) < 180.) then
             lon2 = (1.0-alpha) * slons(1,n) + alpha * slons(3,n)  
             eplonl2 = slons(1,n)
             eplonr2 = slons(3,n)
            else if (slons(1,n) > slons(3,n)) then
             lon2 = (1.0-alpha) * slons(1,n) + alpha * (slons(3,n)+360.)
             eplonl2 = slons(1,n)
             eplonr2 = slons(3,n)+360.
            else
             lon2 = (1.0-alpha) * (slons(1,n)+360.) + alpha * slons(3,n)
             eplonl2 = slons(1,n)+360.
             eplonr2 = slons(3,n)
            endif

            ! interpolate along great circle unless endpoints of interpolation have same lon
            eplatl1 = slats(1,n-1) 
            eplatr1 = slats(3,n-1) 
            eplatl2 = slats(1,n) 
            eplatr2 = slats(3,n) 
            sdnom1 = sin((eplonl1-eplonr1)*d2r)
            sdnom2 = sin((eplonl2-eplonr2)*d2r)  
            if (abs(sdnom1) /= 0.) then
             sp1 = sin((lon1-eplonr1)*d2r)/sdnom1
             sp2 = sin((lon1-eplonl1)*d2r)/sdnom1
             lat1 = atan(tan(eplatl1*d2r)*sp1 - tan(eplatr1*d2r)*sp2)
             lat1 = lat1*r2d
            else
             lat1 = (1.0-alpha) * slats(1,n-1) + alpha * slats(3,n-1)  
            endif
            if (abs(sdnom2) /= 0.) then
             sp1 = sin((lon2-eplonr2)*d2r)/sdnom2
             sp2 = sin((lon2-eplonl2)*d2r)/sdnom2
             lat2 = atan(tan(eplatl2*d2r)*sp1 - tan(eplatr2*d2r)*sp2)
             lat2 = lat2*r2d 
            else
             lat2 = (1.0-alpha) * slats(1,n) + alpha * slats(3,n) 
            endif

            do m = 1, jsegs ! along track refinement
               beta = (m - 1.0 ) / ( jsegs - 1.0 )
               if (abs(lon2-lon1) < 180.) then
                lon = (1.0-beta) * lon1 + beta * lon2
                eplon1=lon1
                eplon2=lon2
               else if (lon2 > lon1) then
                lon = (1.0-beta) * (lon1+360.) + beta * lon2
                eplon1=lon1+360.
                eplon2=lon2
               else
                lon = (1.0-beta) * lon1 + beta * (lon2+360.)
                eplon1=lon1
                eplon2=lon2+360.
               endif
               eplat1=lat1
               eplat2=lat2
               sdnom=sin((eplon1-eplon2)*d2r)
               if (abs(sdnom) /= 0. ) then
                sp1=sin((lon-eplon2)*d2r)/sdnom 
                sp2=sin((lon-eplon1)*d2r)/sdnom
                latf = atan(tan(eplat1*d2r)*sp1-tan(eplat2*d2r)*sp2)
                latf = latf*r2d
                lat = latf
               else 
                lat = (1.0-beta) * lat1 + beta * lat2
               endif
               if (lon < lb) lon=lon+360.
               if (lon > ub) lon=lon-360.
               inbox = pnt_in_rect(lat,lon,wcorner_lat,wcorner_lon)
               if (inbox == 1) then
                i = ijsearch(elons,im+1,lon,.false.)
                j = ijsearch(elats,jm+1,lat,.false.)
                if ( i>0 .and. i<=im .and. j>0 .and. j<=jm ) mask(i,j) = 1
               end if
            end do ! msegs
         end do    ! nobs
      end do       ! ksegs
      end subroutine orb_swath_mask_lonlat

      integer function pnt_in_rect(x0,y0,x,y)
      implicit none
      real, intent(in) :: x0,y0
      real, intent(in), dimension(4) :: x,y
      ! local variable
      integer :: i, j, i1,i2, nintersect
      real :: s,t,x1,y1, denom, tol
      real, dimension(2) :: p0,p1,q0,q1,w,v,u,p0_old,p1_old

      tol=0.
      !  for now set x1,y1
      x1=x0+0.2
      y1=y0+0.2
      q0(1)=x0
      q0(2)=y0
      q1(1)=x1
      q1(2)=y1
      nintersect = 0
      ! loop over four sides
      do i = 1, 4
       i1 = i
       i2 = i+1-(4*(i/4))
       ! define edge
       p0(1)=x(i1)
       p0(2)=y(i1)
       p1(1)=x(i2)
       p1(2)=y(i2)
       ! define vectors w,v,u
       do j=1,2
        w(j)=p0(j)-q0(j)
        v(j)=q1(j)-q0(j)
        u(j)=p1(j)-p0(j)
       enddo
       denom = v(1)*u(2)-v(2)*u(1)
       ! if the ray and edge are parallel they probably don't cross, should really
       ! check if they are on top of each other but probably won't happen
       if (abs(denom).gt.tol) then
        s = (v(2)*w(1)-v(1)*w(2))/denom
        t = -(u(1)*w(2)-u(2)*w(1))/denom
        ! now test if ray intersects segment t > 0 and 0 < s < 1
        if (0. <= s .and. s <= 1. .and. t >= 0.) nintersect = nintersect + 1
        ! in future put in test if t = 0, i.e. the point is on one of the edges
       endif
      enddo
      if (mod(nintersect,2) == 1) then
!      if (nintersect == 1) then
       pnt_in_rect = 1
      else
       pnt_in_rect = 0
      endif
      end function pnt_in_rect

      integer function ijsearch(coords,idim,value,periodic) ! fast bisection version
            implicit NONE
            integer, intent(in) :: idim
            real, intent(in) :: coords(idim)
            real, intent(inout) :: value
            logical, intent(in)  :: periodic
            integer i, i1, i2, k
            if ( periodic ) then
                 if ( value>coords(idim) ) value = value - 360.
            endif
            if ( value .eq. coords(idim) ) then
                 ijsearch = idim
                 return
            endif
            ijsearch = -1
            i1 = 1
            i2 = idim
            if (coords(idim) > coords(1)) then
             do k = 1, idim  ! it should never take take long
               i = (i1 + i2) / 2
               if ( (value .ge. coords(i)) ) then
                  if (value .lt. coords(i+1) ) then
                    ijsearch = i
                    exit
                  else
                    i1 = i
                  end if
               else
                    i2 = i
               endif
             end do
            else 
             do k = 1, idim  ! it should never take take long
               i = (i1 + i2) / 2
               if ( (value .lt. coords(i)) ) then
                  if (value .ge. coords(i+1) ) then
                    ijsearch = i
                    exit
                  else
                    i1 = i
                  end if
               else
                    i2 = i
               endif
             end do
            endif
      end function 


      subroutine check_face(IM,JM,LONS,LATS,face)
      integer, intent(in) :: im,jm
      real(ESMF_KIND_R8), intent(in) :: LONS(IM,JM),LATS(IM,JM)
      integer, intent(inout) :: face
      real :: lon,lat
      integer :: i,j,k
      real :: s(6),smin, xyz(3), rsq3
      integer :: fmin,ifmin(6),imax,imaxt
      rsq3=1.0/sqrt(3.)
      do i=1,IM
       do j=1,JM
        smin = 30.0
        lon=LONS(i,j)+MAPL_PI/18.0
        lat=LATS(i,j)
        xyz(1)=cos(lon)*cos(lat)
        xyz(2)=sin(lon)*cos(lat)
        xyz(3)=sin(lat)
        if (xyz(1) /= 0.0) then
         s(1)=rsq3/xyz(1)
         s(2)=-rsq3/xyz(1)
        else 
         s(1)=1000.0
         s(2)=1000.0
        endif
        if (xyz(2) /= 0.0) then
         s(3)=rsq3/xyz(2)
         s(4)=-rsq3/xyz(2)
        else
         s(3)=1000.0
         s(4)=1000.0
        endif
        if (xyz(3) /= 0.0) then
         s(5)=rsq3/xyz(3)
         s(6)=-rsq3/xyz(3)
        else
         s(5)=1000.0
         s(6)=1000.0
        endif
        do k=1,6
         if (s(k) > 0) then 
          if (s(k) < smin) then
           smin = s(k)
           fmin = k
          endif
         endif
        enddo
        ifmin(fmin) = ifmin(fmin)+1
       enddo
      enddo
      imax = 0
      do k=1,6
       imaxt=ifmin(k)
       if (imaxt > imax) then
        imax = imaxt
        face = k
       endif
      enddo
      end subroutine check_face

      subroutine cube_xy(IM,JM,x,y,LONS,LATS,face)
      integer, intent(in) :: IM,JM
      real, intent(inout) :: x(IM,JM),y(IM,JM)
      real(ESMF_KIND_R8), intent(in) :: LATS(IM,JM),LONS(IM,JM)
      integer, intent(in) :: face

      real :: rsq3,LAT,LON
      integer :: i,j
      rsq3 = 1.0/sqrt(3.0)
      do i=1,IM
       do j=1,JM
        LAT=LATS(I,J)
        LON=LONS(I,J)+MAPL_PI/18.0
        select case(face)
         case (1)
          x(I,J) =  rsq3*tan(LON)
          y(I,J) =  rsq3*tan(LAT)/cos(LON)
         case (2)
          x(I,J) =  rsq3*tan(LON)
          y(I,J) = -rsq3*tan(LAT)/cos(LON)
         case (3)
          x(I,J) = -rsq3*cos(LON)/sin(LON)
          y(I,J) =  rsq3*tan(LAT)/sin(LON)
         case (4)
          x(I,J) = -rsq3*cos(LON)/sin(LON)
          y(I,J) = -rsq3*tan(LAT)/sin(LON)
         case (5)
          x(I,J) =  rsq3*sin(LON)*cos(LAT)/sin(LAT)
          y(I,J) = -rsq3*cos(LON)*cos(LAT)/sin(LAT)
         case (6)
          x(I,J) = -rsq3*sin(LON)*cos(LAT)/sin(LAT)
          y(I,J) = -rsq3*cos(LON)*cos(LAT)/sin(LAT)
        end select
       enddo
      enddo
      
      end subroutine cube_xy

      subroutine cube_xy_point(x,y,LAT,LON,face)
      real, intent(inout) :: x,y
      real, intent(in) :: LAT,LON
      integer, intent(in) :: face

      real :: rsq3,llon,llat
      rsq3 = 1.0/sqrt(3.0)
      LLAT=LAT
      LLON=LON+MAPL_PI/18.0
      select case(face)
       case (1)
        x =  rsq3*tan(LLON)
        y =  rsq3*tan(LLAT)/cos(LLON)
       case (2)
        x =  rsq3*tan(LLON)
        y = -rsq3*tan(LLAT)/cos(LLON)
       case (3)
        x = -rsq3*cos(LLON)/sin(LLON)
        y =  rsq3*tan(LLAT)/sin(LLON)
       case (4)
        x = -rsq3*cos(LLON)/sin(LLON)
        y = -rsq3*tan(LLAT)/sin(LLON)
       case (5)
        x =  rsq3*sin(LLON)*cos(LLAT)/sin(LLAT)
        y = -rsq3*cos(LLON)*cos(LLAT)/sin(LLAT)
       case (6)
        x = -rsq3*sin(LLON)*cos(LLAT)/sin(LLAT)
        y = -rsq3*cos(LLON)*cos(LLAT)/sin(LLAT)
      end select

      end subroutine cube_xy_point 

      subroutine check_face_pnt(LON,LAT,face)
      real, intent(in) :: LON,LAT
      integer, intent(inout) :: face
      real :: llon,llat
      integer :: k
      real :: s(6),smin, xyz(3), rsq3
      integer :: fmin
      character(len=ESMF_MAXSTR) :: Iam
      Iam = 'check_face_pnt'

      rsq3=1.0/sqrt(3.)
      smin = 30.0
      llon=lon+MAPL_PI/18.0
      llat=lat
      xyz(1)=cos(llon)*cos(llat)
      xyz(2)=sin(llon)*cos(llat)
      xyz(3)=sin(llat)
      fmin = 7
      if (xyz(1) /= 0.) then
       s(1)=rsq3/xyz(1)
       s(2)=-rsq3/xyz(1)
      else
       s(1)=1000.
       s(2)=1000.
      endif
      if (xyz(2) /= 0.) then
       s(3)=rsq3/xyz(2)
       s(4)=-rsq3/xyz(2)
      else
       s(3)=1000.
       s(4)=1000.
      endif
      if (xyz(3) /= 0.) then
       s(5)=rsq3/xyz(3)
       s(6)=-rsq3/xyz(3)
      else
       s(5)=1000.
       s(6)=1000.
      endif
      do k=1,6
       if (s(k) > 0) then
        if (s(k) < smin) then
         smin = s(k)
         fmin = k
        endif
       endif
      enddo
      if (fmin /= 7) then 
       face = fmin
      endif       
      end subroutine check_face_pnt

      subroutine orb_halo(im,jm,mask,ihalo,jhalo,rc)
         integer,           intent(in   ) :: im
         integer,           intent(in   ) :: jm
         integer,           intent(inout) :: mask(im,jm)
         integer,           intent(in   ) :: ihalo(2)
         integer,           intent(in   ) :: jhalo(2)
         integer, optional, intent(out  ) :: rc

         character(len=ESMF_MAXSTR) :: Iam
         integer :: status
         integer :: i, j, is, js
         integer :: tmask(im,jm)

         Iam = "orb_halo"
         tmask = 0
         do i = 1, im
            do j = 1, jm
               if (mask(i,j) == 1) then
                  do is = i-ihalo(1),i+ihalo(2)
                     if (is >= 1 .and. is <= im) then
                        do js = j-jhalo(1),j+jhalo(2)
                           if (js >= 1 .and. js <=jm) tmask(is,js) = 1
                        end do ! js loop
                     end if ! is in range
                  end do ! is loop
               end if ! (i,j) has mask = 1
            end do ! j loop
         end do ! i loop 

         mask = tmask

         RETURN_(ESMF_SUCCESS)

      end subroutine orb_halo

end module MAPL_OrbGridCompMod
