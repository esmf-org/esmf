!
! Implements a wrapper class around the ESMF_FieldBundle. By construction, this
! is NOT an opaque object.
!
! Arlindo da Silva <arlindo.dasilva@nasa.gov>, April 2010
! Todling - 11Feb2011 - remove ESMFL_BundleAddState since in MAPL_CFIO
!----------------------------------------------------------------------------

#ifndef __PROTEX__
#  include "MAPL_Generic.h"
#endif

!----------------------------------------------------------------------------
!BOP

! !MODULE: MAPL_SimpleBundle --- A Transparent ESMF Field Bundle Wrapper Class

! !INTERFACE:

   module MAPL_SimpleBundleMod

! !USES:

   use ESMF
   use ESMFL_Mod
   use MAPL_BaseMod
   use MAPL_CFIOMod
   use MAPL_MaxMinMod
   use MAPL_CommsMod, only: MAPL_AM_I_ROOT
   use MAPL_ConstantsMod, only: MAPL_PI
   use m_StrTemplate

   implicit NONE
   private

! !PUBLIC MEMBER FUNCTIONS:
!
   public MAPL_SimpleBundleCreate
   public MAPL_SimpleBundlePrint
   public MAPL_SimpleBundleGetIndex
   public MAPL_SimpleBundleDestroy

   public MAPL_SimpleBundleRead
   public MAPL_SimpleBundleWrite

! !PUBLIC TYPES:

   public MAPL_SimpleBundle

   type SimpleArray_1D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer   :: q(:) => null()      ! alias for qr4
      real(kind=ESMF_KIND_R4), pointer   :: qr4(:) => null()
      real(kind=ESMF_KIND_R8), pointer   :: qr8(:) => null()
   end type SimpleArray_1D

   type SimpleArray_2D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer   :: q(:,:) => null()    ! alias for qr4
      real(kind=ESMF_KIND_R4), pointer   :: qr4(:,:) => null()
      real(kind=ESMF_KIND_R8), pointer   :: qr8(:,:) => null()
   end type SimpleArray_2D

   type SimpleArray_3D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer   :: q(:,:,:) => null()   ! alias for qr4
      real(kind=ESMF_KIND_R4), pointer   :: qr4(:,:,:) => null()
      real(kind=ESMF_KIND_R8), pointer   :: qr8(:,:,:) => null()
   end type SimpleArray_3D

   type LcvGrid
      real(kind=ESMF_KIND_R4)          :: ptop = 0.01
      real(kind=ESMF_KIND_R4), pointer :: delp(:,:,:) => null()
   end type LcvGrid

   type SimpleGrid
      real(kind=ESMF_KIND_R4), pointer   :: Lons(:,:) => null() ! Longitudes in degrees
      real(kind=ESMF_KIND_R4), pointer   :: Lats(:,:) => null() ! Latitudes in degrees
      real(kind=ESMF_KIND_R4), pointer   :: Levs(:)   => null() ! Vertical Levels
      character(len=ESMF_MAXSTR)         :: LevUnits = '1'      ! Vertical Level units
      type(LcvGrid)                      :: lcv                 ! Lagrangian Control Volume
   end type SimpleGrid
 
   type MAPL_SimpleBundle
      character(len=ESMF_MAXSTR) :: name
      type(ESMF_FieldBundle), pointer    :: Bundle  ! Associated ESMF bundle
      type(ESMF_Grid)                    :: grid    ! Associated ESMF grid
      type(SimpleGrid)                   :: coords  ! Coordinate variables
      integer :: n1d=-1
      integer :: n2d=-1
      integer :: n3d=-1
      logical :: bundleAlloc = .false.
      type(SimpleArray_1D), pointer :: r1(:) => null()
      type(SimpleArray_2D), pointer :: r2(:) => null()
      type(SimpleArray_3D), pointer :: r3(:) => null()
   end type MAPL_SimpleBundle

! !DESCRIPTION: This module implements the MAPL SimpleBundle class which
!                is a transparent (read: non-opaque) representation of an ESMF
!  Field Bundle.
!
!EOP
!----------------------------------------------------------------------------

   integer, parameter :: READ_ONLY=1

   interface MAPL_SimpleBundleWrite
      module procedure MAPL_SimpleBundleWrite1
      module procedure MAPL_SimpleBundleWrite2
   end interface MAPL_SimpleBundleWrite

   interface MAPL_SimpleBundleCreate
      module procedure MAPL_SimpleBundleCreateFromBundle
      module procedure MAPL_SimpleBundleCreateFromState
   end interface MAPL_SimpleBundleCreate

CONTAINS

!............................................................................................

!-----------------------------------------------------------------------------
!BOP
 
! !IROUTINE: MAPL_SimpleBundleCreate --- Create Simple Bundle
!
! !IIROUTINE: MAPL_SimpleBundleCreate --- Create Simple Bundle from ESMF Bundle
!
! !INTERFACE:
!

  Function MAPL_SimpleBundleCreateFromBundle ( Bundle, rc,     &
                                               Levs, LevUnits, &
                                               ptop, delp,     &
                                               only_vars,      &
                                               strict,         &
                                               name) result (self)

! !ARGUMENTS:

    type(MAPL_SimpleBundle)                        :: self     ! Simple Bundle

    type(ESMF_FieldBundle), target, intent(inout)  :: Bundle   ! ESMF Bundle
    integer, OPTIONAL,              intent(out)    :: rc
                                                    ! Vertical coordinates
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: Levs(:)       ! Constant levels
    character(len=*), OPTIONAL,     intent(in)     :: LevUnits      ! Level units
                                                    ! Lagrangian Control Volume Info
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: ptop          ! top pressure (Pa)
    real(ESMF_KIND_R4), OPTIONAL, pointer, &
                                    intent(in)     :: delp(:,:,:)   ! layer thickness (Pa)
    character(len=*), OPTIONAL,     intent(in)     :: only_vars     ! comma separated field names
    logical, OPTIONAL,              intent(in)     :: strict        ! force name matching, ignored if only_vars is not present
    character(len=*), OPTIONAL,     intent(in)     :: name          ! name

! !DESCRIPTION: Given an ESMF Bundle, creates a corresponding Simple Bundle. The
!               specification of a vertical grid is optional but useful in many
!  cases. The 1-D {\tt Levs} will default to the layer number, and units of "1".
!  Input parameters {\tt (ptop,delp)} can be used to record the corresponding 
!  Lagrangian Control Volume Grid. When {\tt delp} is not specified, variables
!  {\tt DELP} or {\tt delp} are used if present inside the bundle. 
!
!EOP

!                           ------

    type(ESMF_Field) :: Field
    type(ESMF_Array) :: Array
    type(ESMF_TypeKind_Flag) :: typeKind
    real(ESMF_KIND_R8), pointer :: LonsRad(:,:), LatsRad(:,:)

    integer :: arrayRank, I, n, n1d, n2d, n3d, NumVars, myKind_
    integer :: im, jm, km, dims(3)
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    

    logical :: strict_match
    logical :: isPresent
    integer :: n_vars
    character(len=ESMF_MAXSTR) :: message
    logical, allocatable       :: isRequested(:)
    character(len=ESMF_MAXSTR), allocatable :: var_list(:)

    character(len=ESMF_MAXSTR) :: bundleName
    character(len=ESMF_MAXSTR) :: fieldName

                                __Iam__('MAPL_SimpleBundleCreate')

    self%Bundle => Bundle ! remember where it came from

    self%bundleAlloc = .false. ! this is the default. We can overwrite it later

    call ESMF_FieldBundleGet (BUNDLE, name=bundleName, &
                                      grid=self%grid, &
                                      FieldCount=NumVars, __RC__ )

    if (present(name)) then
       if (len_trim(name) > ESMF_MAXSTR) then
           message = 'string "'//trim(name)//'" is too long to be used '// &
                     'as a Simple Bundle name'
           __raise__(MAPL_RC_ERROR, message)
       end if

       self%name = trim(name)
    else
       self%name = bundleName
    end if

!                             -------------------
!                             Requested variables
!                             -------------------

    if (present(strict)) then
       strict_match = strict
    else
       strict_match = .true.  ! by default do strict name matching
    end if

    allocate(isRequested(NumVars), __STAT__)
    isRequested = .false.

    if (present(only_vars)) then
       n_vars = csv_tokens_count_(only_vars)
       ASSERT_(n_vars <= NumVars)

       allocate(var_list(n_vars), __STAT__)

       var_list = '__NONE__'
       call csv_tokens_get_(only_vars, var_list, __RC__)
 
       do i = 1, size(var_list)
          isPresent = .false.

          do n = 1, NumVars
             call MAPL_FieldBundleGet(BUNDLE, n, FIELD, __RC__)
             call ESMF_FieldGet (FIELD, name=fieldName, __RC__)

             if (fieldName == var_list(i)) then
                isPresent = .true.
                exit
             end if
          end do
         
          if (isPresent) then
             isRequested(n) = .true.
          else
             if (strict_match) then
                message = 'could not find field '//trim(var_list(i))// &
                          ' in Simple Bundle <'//trim(self%name)//'>'
                __raise__(MAPL_RC_ERROR, message)
             end if   
          end if
       end do

       deallocate(var_list)
    else
       isRequested = .true.
    end if

!                             --------------------
!                             Coordinate variables
!                             --------------------

    call MAPL_GridGet(self%Grid, localCellCountPerDim = dims, __RC__)
    im = dims(1);  jm = dims(2);  km = dims(3)
    allocate(self%coords%Lons(im,jm), self%coords%Lats(im,jm), self%coords%Levs(km), __STAT__) 
    
!   Retrieve the lat/lon from Grid and convert to degrees
!   -----------------------------------------------------
   call ESMF_GridGetCoord (self%Grid, coordDim=1, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=LonsRad, __RC__)
   call ESMF_GridGetCoord (self%Grid, coordDim=2, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=LatsRad, __RC__)
   self%coords%Lons(:,:) = ( 180. / MAPL_PI) * LonsRad(:,:)
   self%coords%Lats(:,:) = ( 180. / MAPL_PI) * LatsRad(:,:)

!  1-D Levels
!  ----------
   if ( present(Levs) ) then
      if ( size(Levs) == km ) then
         self%coords%Levs(:) = Levs(:)
      else
         STATUS = 77
         VERIFY_(STATUS)
      end if
   else
      self%coords%Levs(:) = (/ (i, i = 1, km) /)
   end if
   if ( present(LevUnits) ) then
      self%coords%LevUnits = LevUnits
   else
      self%coords%LevUnits = '1'
   end if

!  Optional Lagrangian Control Volume info
!  ---------------------------------------
   if ( present(ptop) ) self%coords%lcv%ptop = ptop
   if ( present(delp) ) then ! User specied
!ALT      self%coords%lcv%delp = delp
      self%coords%lcv%delp => delp
   else ! Look inside bundle for delp or DELP
      self%coords%lcv%delp => NULL() 
      call ESMF_FieldBundleGet (Bundle, fieldName='DELP', field=Field, RC=STATUS)
      if ( STATUS /= 0 ) then
           call ESMF_FieldBundleGet (Bundle, fieldName='delp', field=Field, RC=STATUS)
      end if
      if ( STATUS == 0 ) then
         call ESMF_FieldGet(Field, status=fieldStatus, __RC__)
         if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
            call ESMF_FieldGet(Field, 0, self%coords%lcv%delp, __RC__)
         end if
      end if
   end if

!                             --------------------
!                                 Data Arrays
!                             --------------------

!   A little overestimate for the sizes, but why bother
!   ---------------------------------------------------
    allocate(self%r1(NumVars), &
             self%r2(NumVars), &
             self%r3(NumVars), &
             __STAT__)

    n1d = 0
    n2d = 0
    n3d = 0
    DO I = 1, NumVars

       call MAPL_FieldBundleGet(BUNDLE, I, FIELD, __RC__)
       call ESMF_FieldGet (FIELD, name=fieldName, status=fieldStatus, __RC__)

       if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE .and. isRequested(I)) then
          call ESMF_FieldGet (FIELD, ARRAY=array, __RC__ )
          call ESMF_ArrayGet (array, rank=arrayRank, typeKind = typeKind, __RC__ )
       else
          cycle
       end if

!      Real*4
!      ------
       if ( typeKind == ESMF_TYPEKIND_R4 ) then
          if ( arrayRank == 1 ) then
             n1d = n1d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r1(n1d)%qr4, __RC__ )
             self%r1(n1d)%name = trim(fieldName)
             self%r1(n1d)%myKind = ESMF_KIND_R4
             self%r1(n1d)%q => self%r1(n1d)%qr4 ! convenience alias
          else if ( arrayRank == 2 ) then
             n2d = n2d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r2(n2d)%qr4, __RC__ )
             self%r2(n2d)%name = trim(fieldName)
             self%r2(n2d)%myKind = ESMF_KIND_R4
             self%r2(n2d)%q => self%r2(n2d)%qr4 ! convenience alias
         else if ( arrayRank == 3 ) then
             n3d = n3d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r3(n3d)%qr4, __RC__ )
             self%r3(n3d)%name = trim(fieldName)
             self%r3(n3d)%myKind = ESMF_KIND_R4
             self%r3(n3d)%q => self%r3(n3d)%qr4 ! convenience alias
          else
             STATUS = 77
             VERIFY_(STATUS)
          end if

!      Real*8
!      ------
       else if ( typeKind == ESMF_TYPEKIND_R8 ) then
          if ( arrayRank == 1 ) then
             n1d = n1d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r1(n1d)%qr8, __RC__ )
             self%r1(n1d)%name = trim(fieldName)
             self%r1(n1d)%myKind = ESMF_KIND_R8
          else if ( arrayRank == 2 ) then
             n2d = n2d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r2(n2d)%qr8, __RC__ )
             self%r2(n2d)%name = trim(fieldName)
             self%r2(n2d)%myKind = ESMF_KIND_R8
          else if ( arrayRank == 3 ) then
             n3d = n3d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r3(n3d)%qr8, __RC__ )
             self%r3(n3d)%name = trim(fieldName)
             self%r3(n3d)%myKind = ESMF_KIND_R8
          else
             STATUS = 77
             VERIFY_(STATUS)
          end if

!      Unknown kind
!      ------------
       else
          STATUS = 88
          VERIFY_(STATUS)
       end if

    end do 

    self%n1d = n1d
    self%n2d = n2d
    self%n3d = n3d

    deallocate(isRequested, __STAT__)
  
  contains

    function csv_tokens_count_(str, delimiter) result(n)
      implicit none

      integer                         :: n
      character(len=*), intent(in)    :: str
      character, optional, intent(in) :: delimiter

      ! local
      character, parameter :: char_comma = ','
      character            :: c
      integer              :: i 

      if (present(delimiter)) then
         c = delimiter
      else
         c = char_comma
      end if

      n = 1
      do i = 1, len_trim(str)
         if (str(i:i) == c) then
            n = n + 1
         end if   
      end do
    end function csv_tokens_count_ 

    subroutine csv_tokens_get_(str, list, delimiter, ignore, rc)
      implicit none

      character(len=*), intent(in)     :: str
      character(len=*), intent(inout)  :: list(:)
      character, optional, intent(in)  :: delimiter
      character, optional, intent(in)  :: ignore
      integer, optional, intent(inout) :: rc

      ! local
      character, parameter :: char_empty = ''
      character, parameter :: char_space = ' '     
      character, parameter :: char_comma = ','

      character :: c_dlm, c_ign
      integer   :: i, j, n, err

      if (present(delimiter)) then
         c_dlm = delimiter
      else
         c_dlm = char_comma
      end if

      if (present(ignore)) then
         c_ign = ignore
      else
         c_ign = char_space 
      end if

      
      list = char_empty
      j = 1
      n = 1

      err = 0

      do i = 1, len_trim(str)

         if (str(i:i) /= c_dlm) then
            if (n > size(list)) then
               err = 99
               exit
            end if   
            
            if (str(i:i) /= c_ign) then
               list(n)(j:j) = str(i:i)
               j = j + 1
            end if   
         else
            j = 1
            n = n + 1
         end if

      end do

      if (present(rc)) then
         rc = err
      end if   
    end subroutine csv_tokens_get_

  end Function MAPL_SimpleBundleCreateFromBundle

!-----------------------------------------------------------------------------
!BOP
 
!
! !IIROUTINE: MAPL_SimpleBundleCreate --- Create Simple Bundle from ESMF State
!
! !INTERFACE:
!

  Function MAPL_SimpleBundleCreateFromState ( State, rc,      &
                                              Levs, LevUnits, &
                                              ptop, delp,     &
                                              only_vars,      &
                                              strict,         &
                                              name) result (self)

! !ARGUMENTS:

    type(MAPL_SimpleBundle)                        :: self     ! Simple Bundle

    type(ESMF_State), target, intent(inout)        :: State    ! ESMF State
    integer, OPTIONAL,              intent(out)    :: rc
                                                    ! Vertical coordinates
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: Levs(:)       ! Constant levels
    character(len=*), OPTIONAL,     intent(in)     :: LevUnits      ! Level units
                                                    ! Lagrangian Control Volume Info
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: ptop          ! top pressure (Pa)
    real(ESMF_KIND_R4), OPTIONAL, pointer, &
                                    intent(in)     :: delp(:,:,:)   ! layer thickness (Pa)
    character(len=*), OPTIONAL,     intent(in)     :: only_vars     ! comma separated field names
    logical, OPTIONAL,              intent(in)     :: strict        ! force name maching, ignored if only_vars is not present
    character(len=*), OPTIONAL,     intent(in)     :: name          ! name

! !DESCRIPTION: Given an ESMF Stae=te, creates a corresponding Simple Bundle. The
!               specificatiopn of a vertical grid is optional but useful in many
!  cases. The 1-D {\tt Levs} will default to the layer number, and units of "1".
!  Input parameters {\tt (ptop,delp)} can be used to record the corresponding 
!  Lagrangian Control Volume Grid. When {\tt delp} is not specified, variables
!  {\tt DELP} or {\tt delp} are used if present inside the bundle. 
!
!  {\bf IMPORTANT: } It is assumed that the ESMF State has a single grid.

!EOP

    character(len=ESMF_MAXSTR) :: stateName
    character(len=ESMF_MAXSTR) :: bundleName
    character(len=ESMF_MAXSTR) :: message
    type (ESMF_FieldBundle)    :: Bundle

                   __Iam__('MAPL_SimpleBundleCreateFromState')

    call ESMF_StateGet(State, name=stateName, __RC__)

    if (present(name)) then
       if (len_trim(name) > ESMF_MAXSTR) then
          message = 'string "'//trim(name)//'" is too long to be used '// &
                    'as a Simple Bundle name'
          __raise__(MAPL_RC_ERROR, message)
       end if   

       bundleName = trim(name)
    else
       bundleName = stateName
    end if

    Bundle = ESMF_FieldBundleCreate(name=bundleName, __RC__)
    call ESMFL_BundleAddState ( Bundle, State, __RC__)
    self = MAPL_SimpleBundleCreateFromBundle ( Bundle, Levs=Levs, LevUnits=LevUnits, &
                                               ptop=ptop, delp=delp, only_vars=only_vars, &
                                               strict=strict, name=name, __RC__ )

  end Function MAPL_SimpleBundleCreateFromState

!-----------------------------------------------------------------------------
!BOP
 
! !IROUTINE: MAPL_SimpleBundleDestroy --- Destroy a Simple Bundle
!
! !INTERFACE:
!
  subroutine MAPL_SimpleBundleDestroy (self, rc ) 

! !ARGUMENTS:

    type(MAPL_SimpleBundle)                    :: self ! Simple Bundle

    integer, OPTIONAL,           intent(out)   :: rc

!  !DESCRIPTION:
!
!   Destructor for the MAPL Simple Bundle.  It is assumed that the bundle has been created from 
!   an ESMF Field Bundle.

!EOP
!-----------------------------------------------------------------------------

    __Iam__('MAPL_SimpleBundleDestroy')

    deallocate(self%coords%Lons, self%coords%Lats, self%coords%Levs, __STAT__) 
    deallocate(self%r1, self%r2, self%r3, __STAT__)

    call MAPL_FieldBundleDestroy(self%bundle, __RC__)

    if (self%bundleAlloc) then
       deallocate(self%bundle, __STAT__)
    end if

  end subroutine MAPL_SimpleBundleDestroy

!-----------------------------------------------------------------------------
!BOP
 
! !IROUTINE: MAPL_SimpleBundleRead - Reads a Simple Bundle from File
!
! !IIROUTINE: MAPL_SimpleBundleRead - Reads a Simple Bundle given Config
!
! !INTERFACE:
!

  Function MAPL_SimpleBundleRead (filename, grid, time, verbose, &
                                  only_vars, expid, rc ) result (self)

! !ARGUMENTS:

    type(MAPL_SimpleBundle)                    :: self ! Simple Bundle

    character(len=*),            intent(in)    :: filename
    type(ESMF_Time),             intent(inout) :: Time
    type(ESMF_Grid),             intent(in)    :: Grid
    logical, OPTIONAL,           intent(in)    :: verbose
    character(len=*), optional,  intent(IN)    :: only_vars 
    character(len=*), optional,  intent(IN)    :: expid
    integer, OPTIONAL,           intent(out)   :: rc

!  !DESCRIPTION:
!
!   Given an ESMF Config object and a filename, reads the corresponding file into
!    a MAPL SimpleBundle.
!
!EOP
!-----------------------------------------------------------------------------

    __Iam__('MAPL_SimpleBundleRead')
    type(ESMF_FieldBundle),  pointer :: Bundle
    integer                          :: k,n
    character(len=ESMF_MAXSTR)       :: fname

    allocate(Bundle, stat=STATUS)
    VERIFY_(STATUS)

!ALT: ESMF object name cannot exceed length of ESMF_MAXSTR(=128) 
    k = len_trim(filename)
    if (k > ESMF_MAXSTR) then
       n = index(filename,'/',back=.true.)
       ! An attempt to trim the absolute path
       if (n >= k) then ! n+1 has potential to overflow
          fname = filename
       else
          fname = filename(n+1+max(0,k-n-ESMF_MAXSTR):k)
       end if
    else
       fname = filename
    end if

    Bundle = ESMF_FieldBundleCreate ( name=fname, __RC__ )
    call ESMF_FieldBundleSet ( bundle, grid=Grid, __RC__ )
    call MAPL_CFIORead  ( filename, Time, Bundle, verbose=verbose, &
                          ONLY_VARS=only_vars, expid=expid, __RC__ )
    self = MAPL_SimpleBundleCreate ( Bundle, __RC__ )
    self%bundleAlloc = .true.

  end function MAPL_SimpleBundleRead

!-----------------------------------------------------------------------------
!BOP
 
! !IROUTINE: MAPL_SimpleBundleWrite - Writes a Simple Bundle to File
!
! !IIROUTINE: MAPL_SimpleBundleWrite1 - Writes a Simple Bundle to File given Clock
!
! !INTERFACE:
!
  subroutine MAPL_SimpleBundleWrite1 ( self, filename, clock, verbose, rc )

! !ARGUMENTS:

    type(MAPL_SimpleBundle)                 :: self

    character(len=*),           intent(in) :: filename
    type(ESMF_Clock),           intent(inout) :: Clock

    logical, OPTIONAL,          intent(in)  :: verbose
    integer, OPTIONAL,          intent(out) :: rc

!  !DESCRIPTION:
!
!  Writes a MAPL SimpleBundle to file fiven an ESMF Clock object. The file opened,
!  written to, and closed.

!EOP

!                                ---
    type(MAPL_CFIO)            :: cfio
    __Iam__ ('MAPL_SimpleBundleWrite0')

    call MAPL_CFIOCreate ( cfio, filename, clock, self%Bundle, __RC__)
    call MAPL_CFIOWrite  ( cfio, Clock, self%Bundle, verbose=verbose, __RC__)
    call MAPL_CFIODestroy ( cfio, __RC__ )

  end subroutine MAPL_SimpleBundleWrite1

!............................................................................................

!-----------------------------------------------------------------------------
!BOP
! 
! !IIROUTINE: MAPL_SimpleBundleWrite2 - Writes a Simple Bundle to File given Time
!
! !INTERFACE:
!
  subroutine MAPL_SimpleBundleWrite2 ( self, filename, time, verbose, rc )
!
    type(MAPL_SimpleBundle)                 :: self

    character(len=*),           intent(in) :: filename
    type(ESMF_Time),            intent(in)  :: time

    logical, OPTIONAL,          intent(in)  :: verbose
    integer, OPTIONAL,          intent(out) :: rc

!  !DESCRIPTION:
!
!   Writes a MAPL SimpleBundle to file fiven an ESMF Time object. The file opened,
!   written to, and closed. A fake timestep of 30 minutes is assumed.
!
!EOP

!                                ---
    type(ESMF_TimeInterval)    :: TimeStep 
    type(ESMF_Clock)           :: Clock
    type(MAPL_CFIO)            :: cfio
    __Iam__ ('MAPL_SimpleBundleWrite1')

    call ESMF_TimeIntervalSet( TimeStep, h=0, m=30, s=0, __RC__ )
    CLOCK = ESMF_ClockCreate ( name="Clock", timeStep=TimeStep, startTime=Time, __RC__ )

    call MAPL_CFIOCreate ( cfio, filename, clock, self%Bundle, __RC__)
    call MAPL_CFIOWrite  ( cfio, Clock, self%Bundle, verbose=verbose, __RC__)
    call MAPL_CFIODestroy ( cfio, __RC__ )

  end subroutine MAPL_SimpleBundleWrite2

!............................................................................................

!-----------------------------------------------------------------------------
!BOP
! 
! !IIROUTINE: MAPL_SimpleBundlePrint --- Prints Global Max/Min
!
! !INTERFACE:
!
  subroutine MAPL_SimpleBundlePrint ( self )

! !ARGUMENTS:

    type(MAPL_SimpleBundle) :: self
    
!  !DESCRIPTION: Prints the global max/min for each variable in the
!                Simple Bundle.
!
!EOP
!-----------------------------------------------------------------------------

    integer :: i 

    if ( MAPL_AM_I_ROOT() ) then
       print *
       print *, 'Simple Bundle: ', trim(self%name)
    end if

!   1-D
!   ---
    do i = 1, self%n1d
       if ( self%r1(i)%myKind == ESMF_KIND_R4 ) then
          if ( associated(self%r1(i)%qr4) ) then
             call MAPL_MaxMin(trim(self%r1(i)%name), self%r1(i)%qr4)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r1(i)%name)//' ------    ------'
          endif
       else if ( self%r1(i)%myKind == ESMF_KIND_R8 ) then
          if ( associated(self%r1(i)%qr8) ) then
             call MAPL_MaxMin(trim(self%r1(i)%name), self%r1(i)%qr8)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r1(i)%name)//' ------    ------'
          endif
       end if
    end do

!   2-D
!   ---
    do i = 1, self%n2d
       if ( self%r2(i)%myKind == ESMF_KIND_R4 ) then
          if ( associated(self%r2(i)%qr4) ) then
             call MAPL_MaxMin(trim(self%r2(i)%name), self%r2(i)%qr4)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r2(i)%name)//' ------    ------'
          endif
       else if ( self%r2(i)%myKind == ESMF_KIND_R8 ) then
          if ( associated(self%r2(i)%qr8) ) then
             call MAPL_MaxMin(trim(self%r2(i)%name), self%r2(i)%qr8)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r2(i)%name)//' ------    ------'
          endif
       end if
    end do

!   3-D
!   ---
    do i = 1, self%n3d
       if ( self%r3(i)%myKind == ESMF_KIND_R4 ) then
          if ( associated(self%r3(i)%qr4) ) then
             call MAPL_MaxMin(trim(self%r3(i)%name), self%r3(i)%qr4)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r3(i)%name)//' ------    ------'
          endif
       else if ( self%r3(i)%myKind == ESMF_KIND_R8 ) then
          if ( associated(self%r3(i)%qr8) ) then
             call MAPL_MaxMin(trim(self%r3(i)%name), self%r3(i)%qr8)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r3(i)%name)//' ------    ------'
          endif
       end if
    end do
    if ( MAPL_AM_I_ROOT() ) then
       print *
    end if
end subroutine MAPL_SimpleBundlePrint

!-----------------------------------------------------------------------------
!BOP
! 
! !IIROUTINE: MAPL_SimpleBundleGetIndex ---- Get Index of a Variable
!
! !INTERFACE:
!
  function MAPL_SimpleBundleGetIndex ( self, name, rank, rc, quiet ) result(iq)

! !ARGUMENTS:

    integer                                   :: iq   ! index of variable

    type(MAPL_SimpleBundle)                   :: self
    character(len=*), intent(in)              :: name ! variable name 
    integer,                     intent(in)   :: rank
    integer, OPTIONAL,           intent(out) :: rc
    logical, OPTIONAL,           intent(in)   :: quiet
    
!  !DESCRIPTION: Finds the index of the first variable with name {\tt vname}.
!                This routine is case insensitive.
!
!EOP
!-----------------------------------------------------------------------------

    character(len=ESMF_MAXSTR) :: message
    logical :: quiet_
    integer :: i

                     __Iam__("MAPL_SimpleBundleGetIndex")

    if ( present(quiet) ) then
       quiet_ = quiet
    else
       quiet_ = .FALSE.
    end if

    iq = -1
    if ( rank == 1 ) then
       do i = 1, self%n1d
          if (trim(self%r1(i)%name) == trim(name) ) then
              iq = i
              exit
           endif
        end do
    else if ( rank == 2 ) then
       do i = 1, self%n2d
          if (trim(self%r2(i)%name) == trim(name) ) then
              iq = i
              exit
           endif
        end do
    else if ( rank == 3 ) then
       do i = 1, self%n3d
          if (trim(self%r3(i)%name) == trim(name) ) then
              iq = i
              exit
           endif
        end do
    else
       if ( present(rc) ) then
          __raise__(MAPL_RC_ERROR,"invalid rank")
       end if
    end if

    if ( present(rc) ) then
       if ( iq <= 0 ) then
          if ( quiet_ ) then
             rc = MAPL_RC_ERROR
             return
          else
             message = "could not find index for "//trim(name)// &
                  ' in Simple Bundle <'//trim(self%name)//'>'
             __raise__(MAPL_RC_ERROR,message)
          end if
       else
          RETURN_(ESMF_SUCCESS)
       end if
    end if

  end function MAPL_SimpleBundleGetIndex

end module MAPL_SimpleBundleMod
