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

! $Id: MAPL_HistoryGridComp.F90,v 1.51.8.16.2.9.4.1.2.8.2.2.4.1 2014-04-03 20:16:40 bmauer Exp $

#include "MAPL_Generic.h"

module MAPL_HistoryGridCompMod

!BOP

! !MODULE: MAPL_HistoryGridCompMod

! !USES:

  use ESMF
  use ESMFL_Mod
  use MAPL_BaseMod
  use MAPL_VarSpecMod
  use MAPL_ConstantsMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use MAPL_LocStreamMod
  use MAPL_CFIOMod
  use MAPL_GenericCplCompMod
  use MAPL_NewArthParserMod
  use MAPL_SortMod
  use ESMF_CFIOMOD, only:  StrTemplate => ESMF_CFIOstrTemplate
  use m_chars,  only: uppercase
  use MAPL_CFIOServerMod
  !use ESMF_CFIOMOD

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

! !DESCRIPTION: 
!                \input{MAPL_HistoryDescr.tex}
!
!EOP

! Define History Lists for Output
! -------------------------------
  type    history_list
     character(len=ESMF_MAXSTR)         :: collection
     character(len=ESMF_MAXSTR)         :: filename
     character(len=ESMF_MAXSTR)         :: template
     character(len=ESMF_MAXSTR)         :: format
     character(len=ESMF_MAXSTR)         :: mode
     character(len=ESMF_MAXSTR)         :: descr
     character(len=ESMF_MAXSTR),pointer :: fields (:,:)
     integer                            :: frequency
     integer                            :: acc_interval
     integer                            :: ref_date
     integer                            :: ref_time
     integer                            :: end_date
     integer                            :: end_time
     integer                            :: duration
     type(ESMF_Alarm)                   :: his_alarm ! when to write file
     type(ESMF_Alarm)                   :: seg_alarm ! segment alarm controls when to write to new file
     type(ESMF_Alarm)                   :: mon_alarm
     type(ESMF_Alarm)                   :: end_alarm
     integer,pointer                    :: expSTATE (:)
     integer                            :: unit
     integer                            :: nfield
     type(ESMF_FieldBundle)             :: bundle
     type(MAPL_CFIO)                    :: MCFIO
     real   , pointer                   :: levels(:)     => null()
     integer, pointer                   :: resolution(:) => null()
     real,    pointer                   :: subset(:) => null()
     integer,    pointer                :: chunksize(:) => null()
     integer, pointer                   :: peAve(:)
     integer                            :: verbose
     integer                            :: xyoffset
     logical                            :: disabled
     logical                            :: subVm
     real                               :: vscale
     character(len=ESMF_MAXSTR)         :: vunit
     character(len=ESMF_MAXSTR)         :: vvars(2)
     integer                            :: conservative
     integer                            :: voting
     integer                            :: nbits
     integer                            :: deflate 
     integer                            :: slices
     integer                            :: Root
     integer                            :: Psize
     integer                            :: tm
     ! Adding Arithemtic Field Rewrite
     character(len=ESMF_MAXSTR),pointer :: tmpfields(:) => null()
     logical, pointer                   :: ReWrite(:) => null()
     integer                            :: nPExtraFields
     character(len=ESMF_MAXSTR),pointer :: PExtraFields(:) => null()
     character(len=ESMF_MAXSTR),pointer :: PExtraGridComp(:) => null() 
     character(len=ESMF_MAXSTR),pointer :: vectorList(:,:) => null() 
  endtype history_list

  type SpecWrapper
     type (MAPL_VarSpec),              pointer :: SPEC(:)
  end type SpecWrapper

  type ExchangeRegridType
     type(MAPL_LocStreamXform) :: XFORM
     type(MAPL_LocStreamXform) :: XFORMntv
     type(MAPL_LocStream)      :: LocIn
     type(MAPL_LocStream)      :: LocOut
     type(MAPL_LocStream)      :: LocNative
     type(ESMF_State)          :: state_out
     integer                   :: ntiles_in
     integer                   :: ntiles_out
!ALT: this will not be needed when we modify LocStream to take vm instead of layout
     character(len=ESMF_MAXSTR)     :: tilefile
     character(len=ESMF_MAXSTR)     :: gridname
     logical                        :: noxform
     logical                        :: ontiles
     integer                        :: regridType
  end type ExchangeRegridType

  type ExchangeRegrid
     type(ExchangeRegridType), pointer :: PTR
  end type ExchangeRegrid

  type HISTORY_STATE
     type (history_list),        pointer :: list(:)       => null()
     type (ExchangeRegrid),      pointer :: Regrid(:)     => null()
!     character(len=ESMF_MAXSTR), pointer :: GCNameList(:) => null()
!     type (ESMF_GridComp),       pointer :: gcs(:)        => null()
     type (ESMF_State),          pointer :: GIM(:)        => null()
     type (ESMF_State),          pointer :: GEX(:)        => null()
     type (ESMF_CplComp),        pointer :: CCS(:)        => null()
     type (ESMF_State),          pointer :: CIM(:)        => null()
     type (ESMF_State),          pointer :: CEX(:)        => null()
     type (ESMF_TimeInterval),   pointer :: STAMPOFFSET(:) => null()
     logical,                    pointer :: LCTL(:)       => null()
     logical,                    pointer :: average(:)    => null()
     type (SpecWrapper),         pointer :: SRCS(:)       => null()
     type (SpecWrapper),         pointer :: DSTS(:)       => null()
     character(len=ESMF_MAXSTR)          :: expid
     character(len=ESMF_MAXSTR)          :: expdsc
     integer                             :: CoresPerNode, mype, npes
     integer                             :: AvoidRootNodeThreshold
     integer                             :: blocksize
     integer                             :: MarkDone
  end type HISTORY_STATE
  
  type HISTORY_wrap
     type (HISTORY_STATE), pointer :: PTR
  end type HISTORY_wrap

  type HISTORY_ExchangeListType
     integer*8, pointer                  :: lsaddr_ptr(:) => null()
  end type HISTORY_ExchangeListType

  type HISTORY_ExchangeListWrap
     type(HISTORY_ExchangeListType), pointer :: PTR
  end type HISTORY_ExchangeListWrap

  integer, parameter :: MAPL_G2G = 1
  integer, parameter :: MAPL_T2G = 2
  integer, parameter :: MAPL_T2G2G = 3

  public HISTORY_ExchangeListWrap

  include "mpif.h"

contains

!=====================================================================
  subroutine SetServices ( gc, rc )
    type(ESMF_GridComp), intent(inout) :: gc     ! composite gridded component
    integer, optional               :: rc     ! return code
    
    integer                         :: status
    character(len=ESMF_MAXSTR)      :: IAm="History:SetServices" 
    type (HISTORY_wrap)             :: wrap
    type (HISTORY_STATE), pointer   :: internal_state

! Register services for this component
! ------------------------------------

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE, Initialize, rc=status)
    VERIFY_(status)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,   Run,       rc=status)
    VERIFY_(status)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_FINALIZE, Finalize,  rc=status)
    VERIFY_(status)

! Allocate an instance of the private internal state...
!------------------------------------------------------

    allocate(internal_state, stat=status)
    VERIFY_(status)

! and save its pointer in the GC
!-------------------------------

    wrap%ptr => internal_state
    call ESMF_GridCompSetInternalState(gc, wrap, status)
    VERIFY_(status)

! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd (gc,name="Initialize"     ,rc=status)
    call MAPL_TimerAdd (gc,name="Finalize"       ,rc=status)
    call MAPL_TimerAdd (gc,name="Run"            ,rc=status)
    call MAPL_TimerAdd (gc,name="--Couplers"     ,rc=status)
    call MAPL_TimerAdd (gc,name="--I/O"          ,rc=status)
    call MAPL_TimerAdd (gc,name="----IO Create"  ,rc=status)
    call MAPL_TimerAdd (gc,name="----IO Write"   ,rc=status)
    call MAPL_TimerAdd (gc,name="-----IO Post"   ,rc=status)
    call MAPL_TimerAdd (gc,name="-----IO Wait"   ,rc=status)
    call MAPL_TimerAdd (gc,name="-----IO Write"  ,rc=status)
    call MAPL_TimerAdd (gc,name="-ParserRun"     ,rc=status)

! Generic Set Services
! --------------------
    call MAPL_GenericSetServices ( gc,RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine SetServices

!======================================================
! BOP
! !IROUTINE: Initialize -- Initializes MAPL History Lists for Diagnostic Output

! !INTERFACE:

  subroutine Initialize ( gc, import, dumexport, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout)    :: gc     ! composite gridded component 
    type(ESMF_State),       intent(inout) :: import ! import state
    type(ESMF_State),       intent(inout) :: dumexport ! export state
    type(ESMF_Clock),       intent(inout) :: clock  ! the clock
      integer, intent(out), OPTIONAL        :: rc     ! Error code:

! !DESCRIPTION:
! Initialize initializes MAPL History Lists for Diagnostic Output.
! Diagnostics have the following attributes:
!
! \begin{description}
! \item[1)] Diagnostics may be "instantaneous" or "time-averaged"
! \item[2)] Diagnostics have a "frequency" and an associated "ref_date" and "ref_time"
!           from which the frequency is based.  An "end_date" and "end_time" may also be used
!           to turn off diagnostics after a given date and time.
! \item[3)] Time-Averaged Diagnostics have an associated accumulation interval, "acc_interval",
!           which may be <= to the diagnostic "frequency"
! \item[4)] Diagnostics are "time-stamped" with the center of the time-averaged period.
! \item[5)] The default "acc_interval" is the diagnostic "frequency"
! \item[6)] The default "ref_date" is the beginning date of the experiment
! \item[7)] The default "ref_time" is 0z
! \item[8)] The default "end_date" and "end_time" is disabled
! \end{description}
!
! Through the use of History Lists, the user may define the type of diagnostic output desired.
! History Lists contain the following attributes:
!
! \begin{description}
! \item[filename]     Character string defining the filename of a particular diagnostic output stream.
! \item[template]     Character string defining the time stamping template following GrADS convensions. The default value depends on the duration of the file.
! \item[format]       Character string defining file format ("flat" or "CFIO" or "CFIOasync"). Default = "flat".
! \item[mode]         Character string equal to "instantaneous" or "time-averaged". Default = "instantaneous".
! \item[descr]        Character string equal to the list description. Defaults to "expdsc".
! \item[frequency]    Integer (HHMMSS) for the frequency of output.  Default = 060000.
! \item[acc_interval] Integer (HHMMSS) for the acculation interval (<= frequency) for time-averaged diagnostics.
!                     Default = Diagnostic Frequency.
! \item[ref_date]     Integer (YYYYMMDD) reference date from which the frequency is based.
!                     Default is the Experiment beginning date.
! \item[ref_time]     Integer (HHMMSS) reference time from which the frequency is based.
!                     Default is 000000.
! \item[end_date]     Integer (YYYYMMDD) ending date to stop diagnostic output.  Default is disabled.
! \item[end_time]     Integer (HHMMSS) ending time to stop diagnostic output. Default is disabled.
! \item[duration]     Integer (HHMMSS) for the duration of each file.  Default = frequency (1 time-record per file).
! \item[fields]       Paired character strings for the diagnostic Name and its associated Gridded Component.
! \item[resolution]   Optional resolution (IM JM) for the ouput stream. Default is the native resolution.
! \item[subset]       Optional subset (lonMin lonMax latMin latMax) for the output
! \item[xyoffset]     Optional Flag for Grid Staggering (0:DcPc, 1:DePc, 2:DcPe, 3:DePe)
! \item[levels]       Optional list of output levels (Default is all levels on Native Grid).
! \item[vvars]        Optional Field (and Transform) to use for Vertical Interpolation (eg., 'log(PLE)' , 'DYN' ).
! \item[vunit]        Optional Units to use for Vertical Index of Output File.
! \item[vscale]       Optional Scaling to use between Output Unit and VVARS unit.
! \end{description}
!
! !REVISION HISTORY:
!   14Jan2005 Todling  Implemented GRADS template-ready CFIO filename.
! EOP

    integer                         :: status
    character(len=ESMF_MAXSTR)      :: IAm="History:Initalize" 

    logical                         :: errorFound
    logical                         :: found
    type(history_list), pointer     :: list(:)
    type(HISTORY_wrap)              :: wrap
    type (HISTORY_STATE), pointer   :: IntState
    type(HISTORY_ExchangeListWrap)  :: lswrap

    type(ESMF_State), pointer      :: export (:)
    type(ESMF_State), pointer      :: exptmp (:)
    type(ESMF_Time)                :: StartTime
    type(ESMF_Time)                :: CurrTime
    type(ESMF_Time)                ::  RingTime
    type(ESMF_Time)                ::   RefTime
    type(ESMF_TimeInterval)        :: Frequency
    type(ESMF_Array)               :: array
    type(ESMF_Field)               :: field
    type(ESMF_Field)               :: f
    type(ESMF_Calendar)            ::  cal
    type(ESMF_Config)              :: config
    type(ESMF_DELayout)            :: layout
    type(MAPL_MetaComp), pointer   :: GENSTATE

    character(len=ESMF_MAXSTR)     :: string
    character(len=ESMF_MAXSTR)     :: tmpstring
    character(len=ESMF_MAXSTR)     :: tilefile
    character(len=ESMF_MAXSTR)     :: gridname
    character(len=ESMF_MAXSTR), pointer :: gnames(:)
    character(len=2)               :: POLE, DTLN
    integer                        :: L, LM, layers
    integer                        :: NMLEN
    integer                        :: NG
    integer                        :: NGRIDS
    integer                        :: COUNTS(ESMF_MAXDIM)
    integer                        :: DECOUNT(2)
    integer                        :: NX, NY
    integer                        :: I1,IN,J1,JN
    integer, pointer               :: gridim(:), gridjm(:)
    integer, pointer               :: IMS(:)=>null(), JMS(:)=>null()
    integer                        :: ndes
    integer                        :: dimCount
    integer, allocatable           :: minindex(:,:)
    integer, allocatable           :: maxindex(:,:)
    real, pointer                  :: VAR_2D(:,:)
    real, pointer                  :: levels(:)
    real(ESMF_KIND_R8)             :: X0, Y0, deltaX, deltaY, deltaZ
    real(ESMF_KIND_R8), pointer    :: coordX(:,:)
    real(ESMF_KIND_R8), pointer    :: coordY(:,:)
    real(ESMF_KIND_R8), allocatable :: cornerX(:)
    real(ESMF_KIND_R8), allocatable :: cornerY(:)
    integer                        :: DIMS
    integer                        :: VLOCATION
    integer                        :: FIELD_TYPE
    character(ESMF_MAXSTR)         :: FRIENDLYTO
    integer                        :: avgint
    integer                        :: REFRESH
    integer                        :: NumSlices
    character(ESMF_MAXSTR)         :: SHORT_NAME
    character(ESMF_MAXSTR)         :: LONG_NAME
    character(ESMF_MAXSTR)         :: UNITS
    character(ESMF_MAXSTR), pointer:: VVARn(:)
    character(ESMF_MAXSTR)         :: VVAR
    character(ESMF_MAXSTR), pointer:: fields (:,:)
    character(ESMF_MAXSTR)         :: fields1
    character(ESMF_MAXSTR)         :: fields2
    character(ESMF_MAXSTR)         :: fields3
    character(ESMF_MAXSTR)         :: fields4
    logical                        :: tend
    character(len=ESMF_MAXSTR),allocatable :: statelist(:)
    logical,                   allocatable :: statelistavail(:)
    character(len=ESMF_MAXSTR),allocatable ::   tmplist(:)
    
    integer :: nlist,unit,nsecf,nfield,nstatelist
    integer :: k,m,n,sec,rank,nhms,size0
    integer :: year,month,day,hour,minute,second,nymd0,nhms0
    integer :: ref_time(6)
    integer :: len, i, j, mype, npes

    type (ESMF_Grid)                          :: grid
    type (ESMF_Grid)                          :: grid_attached
    type (ESMF_DistGrid)                      :: distgrid
    type (ESMF_Grid)                          :: grid_in, grid_out
    type (ESMF_Grid), pointer                 :: grids(:)
    type (MAPL_LocStream)                     :: exch
    type (MAPL_LocStream)                     :: locstream
    type (ESMF_VM)                            :: vm
    logical                                   :: use_this_gridname
    logical                                   :: ontiles
    logical                                   :: disableSubVmChecks
    character(len=ESMF_MAXSTR)                :: tmpstr, attachedName
    integer                                   :: localStatus, globalStatus
    integer, pointer :: allPes(:)
    integer          :: localPe(1), nactual, minactual
    integer*8                                 :: ADDR
    integer*8, pointer                        :: LSADDR_PTR(:) => null()
    type(ESMF_State)                          :: state_out
    integer                                   :: fieldRank, gridRank
    integer                                   :: undist
    integer, allocatable                      :: ungrd(:)
    integer                                   :: ungridDims
    integer                                   :: notGridded
    logical                                   :: hasUngridDims
    integer, allocatable                      :: gridToFieldMap(:)
    integer, allocatable                      :: ungriddedLBound(:)
    integer, allocatable                      :: ungriddedUBound(:)
    type (ESMF_LocalArray), target            :: larrayList(1)
    type (ESMF_LocalArray), pointer           :: larray
    integer                                   :: c
    logical                                   :: isFileName
    logical                                   :: fileExists
    real                                      :: lvl

    integer                                   :: unitr, unitw
    integer                                   :: tm
    logical                                   :: match, contLine
    character(len=2048)                       :: line
    type(ESMF_Config)                         :: cfg
    character(len=ESMF_MAXSTR)                :: HIST_CF
    character(len=ESMF_MAXSTR)                :: collection_rc
    character(len=ESMF_MAXSTR)                :: BLANK=""

!   Parser Variables
    integer, pointer :: tmprank(:),tmploc(:)
    logical          :: DoCopy
    type(ESMF_State) :: parser_state
    type(ESMF_Field) :: parser_field
    real, pointer    :: ptr3d(:,:,:) => null()

!   Async cfio option
    type(MAPL_Communicators)       :: maplComm
    logical                        :: Async, doAsync

!   Single colum flag used to set different defalut for TM
    integer                        :: snglcol
    integer                        :: tm_default

!   variable for vector handling
    logical                        :: vectorDone
    integer                        :: idx, nvec
    character(len=ESMF_MAXSTR)     :: f1copy, f3copy
    character(len=ESMF_MAXSTR),pointer :: vectorList(:,:) => null() 
    
! Fortran statement function
    nsecf(nhms) = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)

! Begin
!------

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Initialize")

! Retrieve the pointer to the state
    call ESMF_GridCompGetInternalState(gc, wrap, status)
    VERIFY_(status)
    IntState => wrap%ptr

    call ESMF_UserCompGetInternalState(GC, 'MAPL_LocStreamList', &
        lswrap, STATUS)
    if (status == ESMF_SUCCESS) then
       lsaddr_ptr => lswrap%ptr%lsaddr_ptr
    end if

    call ESMF_GridCompGet(gc, vm=vm, rc=status)
    VERIFY_(status)

    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(status)
    call ESMF_VMGet       (VM, localpet=MYPE, petcount=NPES,  RC=STATUS)
    VERIFY_(STATUS)

    IntState%mype = mype
    IntState%npes = npes


! Get Clock StartTime for Default ref_date, ref_time
! --------------------------------------------------
    call ESMF_ClockGet ( clock,     calendar=cal,       rc=STATUS ) ; VERIFY_(STATUS)
    call ESMF_ClockGet ( clock,     currTime=CurrTime,  rc=STATUS ) ; VERIFY_(STATUS)
    call ESMF_ClockGet ( clock,     StartTime=StartTime,rc=STATUS ) ; VERIFY_(STATUS)
    call ESMF_TimeGet  ( StartTime, TimeString=string  ,rc=STATUS ) ; VERIFY_(STATUS)
    
    read(string( 1: 4),'(i4.4)') year
    read(string( 6: 7),'(i2.2)') month
    read(string( 9:10),'(i2.2)') day
    read(string(12:13),'(i2.2)') hour
    read(string(15:16),'(i2.2)') minute
    read(string(18:18),'(i2.2)') second
    
    nymd0 =  year*10000 +  month*100 + day
    nhms0 =  hour*10000 + minute*100 + second

! Read User-Supplied History Lists from Config File
! -------------------------------------------------
    call ESMF_GridCompGet( gc, config=config, rc=STATUS ) ; VERIFY_(STATUS)

    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expid, &
                                   label ='EXPID:', default='', rc=status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expdsc, &
                                   label ='EXPDSC:', default='', rc=status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%CoresPerNode, &
                                   label ='CoresPerNode:', default=min(npes,8), rc=status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=disableSubVmChecks, &
                                   label ='DisableSubVmChecks:', default=.false., rc=status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%AvoidRootNodeThreshold, &
                                   label ='AvoidRootNodeThreshold:', default=1024, rc=status )
    VERIFY_(STATUS)

    call ESMF_ConfigGetAttribute(config, value=INTSTATE%blocksize,         &
                                         label='BlockSize:', default=10, rc=status)
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(config, value=INTSTATE%MarkDone,          &
                                         label='MarkDone:', default=0, rc=status)
    VERIFY_(STATUS)

    call ESMF_ConfigGetAttribute(config, value=snglcol,          &
                                         label='SINGLE_COLUMN:', default=0, rc=status)
    VERIFY_(STATUS)
    if( MAPL_AM_I_ROOT() ) then
       print *
       print *, 'EXPID: ',trim(INTSTATE%expid)
       print *, 'Descr: ',trim(INTSTATE%expdsc)
       print *, 'DisableSubVmChecks:', disableSubVmChecks
       print *, 'BlockSize: '        , INTSTATE%blocksize
       print *, 'MarkDone:  '        , INTSTATE%MarkDone
       print *
    endif

! Determine Number of Output Streams
! ----------------------------------
    if( MAPL_AM_I_ROOT() ) then
       print *, 'Reading HISTORY RC Files:'
       print *, '-------------------------'
    endif

    call ESMF_ConfigFindLabel ( config,'COLLECTIONS:',rc=STATUS )
    VERIFY_(STATUS)
    tend  = .false.
    nlist = 0
    allocate(IntState%list(nlist), stat=status)
    VERIFY_(STATUS)
    do while (.not.tend)
          call ESMF_ConfigGetAttribute ( config,value=tmpstring,default='',rc=STATUS) !ALT: we don't check return status!!!
          if (tmpstring /= '')  then
             nlist = nlist + 1
             allocate( list(nlist), stat=status )
             VERIFY_(STATUS)
             list(1:nlist-1)=IntState%list
             list(nlist)%collection = tmpstring
             list(nlist)%filename = list(nlist)%collection
             deallocate(IntState%list)
             IntState%list => list
          end if
          call ESMF_ConfigNextLine     ( config,tableEnd=tend,rc=STATUS )
          VERIFY_(STATUS)
    enddo

    if (nlist == 0) then
       RETURN_(ESMF_SUCCESS)
    end if

    allocate(IntState%Regrid(nlist), stat=STATUS)
    VERIFY_(STATUS)
    allocate(          Vvarn(nlist), stat=STATUS)
    VERIFY_(STATUS)

! We are parsing HISTORY config file to split each collection into separate RC
! ----------------------------------------------------------------------------

    if( MAPL_AM_I_ROOT(vm) ) then

       call ESMF_ConfigGetAttribute(config, value=HIST_CF, &
            label="HIST_CF:", default="HIST.rc", RC=STATUS ) 
       VERIFY_(STATUS)
       unitr = GETFILE(HIST_CF, FORM='formatted', RC=status)
       VERIFY_(STATUS)

!       for each collection
       do n = 1, nlist
         rewind(unitr)
         string = trim( list(n)%collection ) // '.'
         unitw = GETFILE(trim(string)//'rcx', FORM='formatted', RC=status)

         match = .false.
         contLine = .false.

         do while (.true.)
            read(unitr, '(A)', end=1234) line 
            j = index( adjustl(line), trim(adjustl(string)) )
            match = (j == 1)
            if (match) then
               j = index(line, trim(string)//'fields:')
               contLine = (j > 0)
            end if
            if (match .or. contLine) then
               write(unitw,'(A)') trim(line)
            end if
            if (contLine) then
               if (adjustl(line) == '::') contLine = .false.
            end if

         end do

1234     continue
         call free_file(unitw, rc=status)
         VERIFY_(STATUS)
      end do

      call free_file(unitr, rc=status)
      VERIFY_(STATUS)

    end if


    call ESMF_VMbarrier(vm, RC=status)
    VERIFY_(STATUS)

! Initialize History Lists
! ------------------------
 
    LISTLOOP: do n=1,nlist

       list(n)%unit = 0

       string = trim( list(n)%collection ) // '.'

       if (trim(list(n)%filename) == "/dev/null") then
          list(n)%disabled = .true.
       else
          list(n)%disabled = .false.
       end if
       
       cfg = ESMF_ConfigCreate(rc=STATUS)
       VERIFY_(STATUS)

       call ESMF_ConfigLoadFile(cfg, filename = trim(string)//'rcx', rc=status)
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%template, default="", &
                                      label=trim(string) // 'template:' ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%format,default='flat', &
                                      label=trim(string) // 'format:' ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%mode,default='instantaneous', &
                                      label=trim(string) // 'mode:' ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%descr, &
                                      default=INTSTATE%expdsc, &
                                      label=trim(string) // 'descr:' ,rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%frequency, default=060000, &
	                              label=trim(string) // 'frequency:',rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%acc_interval, default=list(n)%frequency, &
	                              label=trim(string) // 'acc_interval:',rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%ref_date, default=nymd0, &
	                              label=trim(string) // 'ref_date:',rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%ref_time, default=000000, &
                                      label=trim(string) // 'ref_time:',rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%end_date, default=-999, &
	                              label=trim(string) // 'end_date:',rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%end_time, default=-999, &
                                      label=trim(string) // 'end_time:',rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%duration, default=list(n)%frequency, &
	                              label=trim(string) // 'duration:'  ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%verbose, default=0, &
	                              label=trim(string) // 'verbose:'  ,rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%vscale, default=1.0, &
	                              label=trim(string) // 'vscale:'  ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%vunit, default="", &
	                              label=trim(string) // 'vunit:'  ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%nbits, default=100, &
                                      label=trim(string) // 'nbits:' ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%deflate, default=0, &
                                      label=trim(string) // 'deflate:' ,rc=status )
       VERIFY_(STATUS)

       tm_default = -1
       if (snglcol == 1) then
          tm_default = 0
       end if
       call ESMF_ConfigGetAttribute ( cfg, list(n)%tm, default=tm_default, &
                                      label=trim(string) // 'tm:', rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%conservative, default=0, &
	                              label=trim(string) // 'conservative:'  ,rc=status )
       VERIFY_(STATUS)

!      Disable streams when frequencies, times are negative
!      ----------------------------------------------------
       if ( list(n)%frequency < 0 .OR. &
            list(n)%ref_date  < 0 .OR. &
            list(n)%ref_time  < 0 .OR. &
            list(n)%duration  < 0      )   list(n)%disabled = .true.

       call ESMF_ConfigFindLabel ( cfg,trim(string) // 'fields:',rc=STATUS )
       tend = .false.
       m = 0
       do while (.not.tend)
          m = m+1

! Get EXPORT Name
! ---------------
          call ESMF_ConfigGetAttribute ( cfg,value=fields1,rc=STATUS)
          if (status /= ESMF_SUCCESS)  then
              if( MAPL_AM_I_ROOT(vm) ) then
                  print *
                  print *, '**************************************************************'
                  print *, 'Attributes NOT set for Collection: ',trim( list(n)%collection )
                  print *, '**************************************************************'
                  print *
              endif
          endif
          VERIFY_(STATUS)
          i = index(fields1(  1:),"'")
          j = index(fields1(i+1:),"'")+i
          if( i.ne.0 ) then
              fields1 = adjustl( fields1(i+1:j-1) )
          else
              fields1 = adjustl( fields1 )
          endif

! Get GC Name
! ------------
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
          VERIFY_(STATUS)
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=fields2,rc=STATUS)
              VERIFY_(STATUS)
          else
              fields2 = tmpstring
          endif
          i = index(fields2(  1:),"'")
          j = index(fields2(i+1:),"'")+i
          if( i.ne.0 ) then
              fields2 = adjustl( fields2(i+1:j-1) )
          else
              fields2 = adjustl( fields2 )
          endif

! Get Possible ALIAS Name
! -----------------------
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=fields3,default=fields1,rc=STATUS)
          else
              if( trim(tmpstring) /= ' ' )  then
                  fields3 = tmpstring
              else
                  fields3 = fields1
              endif
          endif
          i = index(fields3(  1:),"'")
          j = index(fields3(i+1:),"'")+i
          if( i.ne.0 ) then
              fields3 = adjustl( fields3(i+1:j-1) )
          else
              fields3 = adjustl( fields3 )
          endif
!         if this is a bundle and we did not provide alias, strip off bundle name
          i = index(fields3(1:),"%")
          if (i.ne.0 .and. scan(trim(fields3),'()^/*+-')==0 ) fields3 = adjustl( fields3(i+1:) )

! Get Possible COUPLER Function
! -----------------------------
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=fields4,default=BLANK,rc=STATUS)
          else
              if( trim(tmpstring) /= ' ' )  then
                  fields4 = tmpstring
              else
                  fields4 = BLANK
              endif
          endif
          i = index(fields4(  1:),"'")
          j = index(fields4(i+1:),"'")+i
          if( i.ne.0 ) then
              fields4 = adjustl( fields4(i+1:j-1) )
          else
              fields4 = adjustl( fields4 )
          endif
! convert to uppercase
          tmpstring = UPPERCASE(fields4)
          fields4 = tmpstring
! -------------

          call ESMF_ConfigNextLine  ( cfg,tableEnd=tend,rc=STATUS )
          VERIFY_(STATUS)
          vectorDone=.false.
          VECTORPAIR: do while(.not.vectorDone)
             allocate( fields(4,m), stat=status )
             VERIFY_(STATUS)

             idx = index(fields1,";")
             if (idx == 0) then
                vectorDone=.true.
             else
                f1copy = fields1(idx+1:)
                fields1 = fields1(1:idx-1)
                idx = index(fields3,";")
                ASSERT_(idx > 0)
                f3copy = fields3(idx+1:)
                fields3 = fields3(1:idx-1)
             end if

             if( m==1 ) then
                fields(1,m)     = fields1
                fields(2,m)     = fields2
                fields(3,m)     = fields3
                fields(4,m)     = fields4
                allocate( list(n)%fields(4,m), stat=status)
                VERIFY_(STATUS)
                list(n)%fields = fields
             else
                fields(1,1:m-1) = list(n)%fields(1,:)
                fields(2,1:m-1) = list(n)%fields(2,:)
                fields(3,1:m-1) = list(n)%fields(3,:)
                fields(4,1:m-1) = list(n)%fields(4,:)
                fields(1,m)     = fields1
                fields(2,m)     = fields2
                fields(3,m)     = fields3
                fields(4,m)     = fields4
                deallocate (list(n)%fields)
                allocate( list(n)%fields(4,m), stat=status )
                VERIFY_(STATUS)
                list(n)%fields = fields
             endif
             deallocate (fields)
             if (.not.vectorDone) then
!ALT: next if-block builds a vectorList for proper processing of vectors
!     by MAPL_HorzTransformRun done in MAPL_CFIO. 
!     The logic of construction the vectorList is somewhat flawed
!     it works for vectors with two components (i.e. U;V), 
!     but ideally should be more general
                if (.not.associated(list(n)%vectorList)) then
                   allocate(list(n)%vectorList(2,0), stat=status)
                   VERIFY_(STATUS)
                end if
                nvec = size(list(n)%vectorList,2)
                allocate(vectorList(2,nvec+1), stat=status)
                VERIFY_(STATUS)
                vectorList(:,1:nvec) = list(n)%vectorList(:,:)
                deallocate(list(n)%vectorList)
                nvec = nvec+1
                vectorList(1,nvec)=fields3
                vectorList(2,nvec)=f3copy
                list(n)%vectorList => vectorList

                fields1 = f1copy
                fields3 = f3copy
                m = m + 1
             end if
          end do VECTORPAIR
       enddo

       list(n)%nfield = m

! Get an optional list of output levels
! -------------------------------------

       list(n)%vvars = ""
 
       len = ESMF_ConfigGetLen( cfg, label=trim(trim(string) // 'levels:'), rc = status )

       LEVS: if( status == ESMF_SUCCESS ) then
          call ESMF_ConfigFindLabel( config, label=trim(trim(string) // 'levels:'), rc = status )
          VERIFY_(STATUS)
             j = 0
          do i = 1, len
             call ESMF_ConfigGetAttribute ( config,value=tmpstring ,rc=STATUS)
             VERIFY_(STATUS)
             if( trim(tmpstring) == ',' )  cycle
             j = j + 1

             ! Allow for possibility that levels could point to a file
             isFileName = .false.
             if (j == 1) then 
                !ALT: only the first non-comma entry could be filename
                tmpstring = trim(adjustl(tmpstring))
                l = len_trim(tmpstring)
                do k = 1,l
                   c = ichar(tmpstring(k:k))
                   if((c > 64 .and. c < 91) .or. (c>96 .and. c < 123)) then
                      isFileName = .true.
                      exit
                   end if
                end do

                if (isFileName) then
                   INQUIRE ( FILE=trim(tmpstring), EXIST=fileExists )
                   ASSERT_(fileExists)

                   unit = GETFILE(trim(tmpstring), form='formatted', rc=status)
                   VERIFY_(STATUS)

                   if (MAPL_Am_I_Root(vm)) then
                      k=0
                      do while (.true.)
                         read(unit, *, end=987) lvl
                         k = k+1
                      end do
987                   continue

                   end if
             
                   call MAPL_CommsBcast(vm, DATA=k, N=1, ROOT=MAPL_Root, RC=status)
                   VERIFY_(STATUS)

                   allocate( list(n)%levels(k), stat = status )  
                   VERIFY_(STATUS)

                   if (MAPL_Am_I_Root(vm)) then
                      rewind(unit)
                      do l=1,k
                         read(unit, *) list(n)%levels(l)
                      end do
                   end if
             
                   call MAPL_CommsBcast(vm, DATA=list(n)%levels, N=k, &
                        ROOT=MAPL_Root, RC=status)
                   VERIFY_(STATUS)

                   call FREE_FILE(UNIT)
                end if
             end if

             if(isFileName) cycle

             allocate( levels(j), stat = status )
             VERIFY_(STATUS)
                     i1 = index(tmpstring(:),",")
                 if( i1.eq.1 )  tmpstring = adjustl( tmpstring(2:)   )
                     j1 = index(tmpstring(:),",")-1
                 if( j1.gt.0 )  tmpstring = adjustl( tmpstring(1:j1) )
             read(tmpstring,*)  levels(j)
             if( j.eq.1 ) then
                 allocate( list(n)%levels(j), stat = status )  
                 VERIFY_(STATUS)
                 list(n)%levels(j) = levels(j)
             else
                 levels(1:j-1) = list(n)%levels(:)
                 deallocate( list(n)%levels )
                   allocate( list(n)%levels(j), stat = status )  
                   VERIFY_(STATUS)
                   list(n)%levels(:) = levels(:)
             endif
             deallocate( levels )
          enddo

! Get an interpolating variable
! -----------------------------

          call ESMF_ConfigFindLabel ( cfg,trim(string) // 'vvars:',rc=STATUS )
          VINTRP: if( status == ESMF_SUCCESS ) then

             call ESMF_ConfigGetAttribute ( cfg,value=list(n)%vvars(1), rc=STATUS)
             VERIFY_(STATUS) 
             i = index(list(n)%vvars(1)(  1:),"'")
             j = index(list(n)%vvars(1)(i+1:),"'")+i
             if( i.ne.0 ) then
                 list(n)%vvars(1) = adjustl( list(n)%vvars(1)(i+1:j-1) )
             else
                 list(n)%vvars(1) = adjustl( list(n)%vvars(1) )
             endif

             call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
             VERIFY_(STATUS)
             if( trim(tmpstring) == ',' )  then
                 call ESMF_ConfigGetAttribute ( cfg,value=list(n)%vvars(2),rc=STATUS)
                 VERIFY_(STATUS)
             else
                 list(n)%vvars(2) = tmpstring
             endif
             i = index(list(n)%vvars(2)(  1:),"'")
             j = index(list(n)%vvars(2)(i+1:),"'")+i
             if( i.ne.0 ) then
                 list(n)%vvars(2) = adjustl( list(n)%vvars(2)(i+1:j-1) )
             else
                 list(n)%vvars(2) = adjustl( list(n)%vvars(2) )
             endif

! Add Vertical Coordinate Variables to Field List (if not already present)
! ------------------------------------------------------------------------

             list(n)%vvars(1) = trim(adjustl(list(n)%vvars(1)))
             vvar = adjustl(list(n)%vvars(1))
             if(vvar/="") then
                if    (Vvar(1:3)=='log') then
                   Vvar  = adjustl(Vvar(index(vvar,'(')+1:index(vvar,')')-1))
                elseif(Vvar(1:3)=='pow') then 
                   Vvar  = adjustl(Vvar(index(vvar,'(')+1:index(vvar,',')-1))
                endif

                do i=1,list(n)%nfield
                   found = list(n)%fields(1,i).eq.vvar   .and. &
                        list(n)%fields(2,i).eq.list(n)%vvars(2)
                   if(found)exit
                enddo

                if( .not.found ) then
                   list(n)%nfield = list(n)%nfield + 1
                   allocate( fields(4,  list(n)%nfield), stat=status )
                   fields(1,1:list(n)%nfield-1) = list(n)%fields(1,:)
                   fields(2,1:list(n)%nfield-1) = list(n)%fields(2,:)
                   fields(3,1:list(n)%nfield-1) = list(n)%fields(3,:)
                   fields(4,1:list(n)%nfield-1) = list(n)%fields(4,:)
                   fields(1,  list(n)%nfield  ) = Vvar
                   fields(2,  list(n)%nfield  ) = list(n)%vvars (2)
                   fields(3,  list(n)%nfield  ) = Vvar
                   fields(4,  list(n)%nfield  ) = BLANK
                   deallocate( list(n)%fields, stat=status )
                   VERIFY_(STATUS)
                   list(n)%fields => fields
                endif
             end if
          endif VINTRP ! Vertical interp var

       endif LEVS ! selected levels

       vvarn(n) = vvar

! Get an optional 2-D output resolution
! -------------------------------------
       list(n)%xyoffset = 0
       len = ESMF_ConfigGetLen ( cfg, label=trim(trim(string) // 'resolution:'), rc = status )
       if( status == ESMF_SUCCESS ) then
          call  ESMF_ConfigFindLabel( cfg, label=trim(trim(string) // 'resolution:'), rc = status )
          allocate( list(n)%resolution(2), stat = status )  
          VERIFY_(STATUS)
             j = 0
          do i = 1,len
             call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
             VERIFY_(STATUS)
             if( trim(tmpstring) == ',' )  cycle
             j = j + 1
             ASSERT_(j<=2)
                     i1 = index(tmpstring(:),",")
                 if( i1.eq.1 )  tmpstring = adjustl( tmpstring(2:)   )
                     j1 = index(tmpstring(:),",")-1
                 if( j1.gt.0 )  tmpstring = adjustl( tmpstring(1:j1) )
             read(tmpstring,*)  list(n)%resolution(j)
          enddo
          call ESMF_ConfigGetAttribute ( cfg, list(n)%xyoffset, default=0, &
                                         label=trim(string) // 'xyoffset:' ,rc=status )
          VERIFY_(STATUS)
       end if

! Get an optional 2-D output subset
! ---------------------------------

       len = ESMF_ConfigGetLen(cfg, label=trim(trim(string) // 'subset:'), rc = status)
       if ( status == ESMF_SUCCESS ) then
          call ESMF_ConfigFindLabel( cfg, label=trim(trim(string) // 'subset:'), rc =status)
          VERIFY_(STATUS)
          allocate( list(n)%subset(4), stat = status)
          VERIFY_(STATUS)
          j=0
          do i=1,len
             call ESMF_ConfigGetAttribute( cfg,value=tmpstring, rc=status)
             VERIFY_(STATUS)
             if (trim(tmpstring) == ',' ) cycle
             j = j + 1
             ASSERT_(j<=6)
             i1 = index(tmpstring(:),",")
             if (i1.eq.1) tmpstring = adjustl( tmpstring(2:)  )
             j1 = index(tmpstring(:),",")-1
             if (j1.gt.0) tmpstring = adjustl( tmpstring(1:j1) )
             if (j<=4) read(tmpstring,*) list(n)%subset(j)
          enddo
          if (.not. associated(list(n)%resolution)) then
             call WRITE_PARALLEL('A resolution must be specified for this subset history output')
             ASSERT_(associated(list(n)%resolution))
          endif
       end if

! Get an optional chunk size
! --------------------------
       len = ESMF_ConfigGetLen(cfg, label=trim(trim(string) // 'chunksize:'), rc = status)
       if ( status == ESMF_SUCCESS ) then
          call ESMF_ConfigFindLabel( cfg, label=trim(trim(string) // 'chunksize:'), rc =status)
          VERIFY_(STATUS)
          allocate( list(n)%chunksize(4), stat = status)
          VERIFY_(STATUS)
          j=0
          do i=1,len
             call ESMF_ConfigGetAttribute( cfg,value=tmpstring, rc=status)
             VERIFY_(STATUS)
             if (trim(tmpstring) == ',' ) cycle
             j = j + 1
             ASSERT_(j<=6)
             i1 = index(tmpstring(:),",")
             if (i1.eq.1) tmpstring = adjustl( tmpstring(2:)  )
             j1 = index(tmpstring(:),",")-1
             if (j1.gt.0) tmpstring = adjustl( tmpstring(1:j1) )
             if (j<=4) read(tmpstring,*) list(n)%chunksize(j)
          enddo
       end if

! Get an optional tile file for regridding the output
! ---------------------------------------------------
       call ESMF_ConfigGetAttribute ( cfg, value=tilefile, default="", &
                                      label=trim(string) // 'regrid_exch:' ,rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( config, value=gridname, default="", &
                                      label=trim(string) // 'regrid_name:' ,rc=status )
       VERIFY_(STATUS)

       NULLIFY(IntState%Regrid(n)%PTR)
       if (tilefile /= '' .OR. gridname /= '') then
          allocate(IntState%Regrid(n)%PTR, stat=status)
          VERIFY_(STATUS)
          IntState%Regrid(n)%PTR%tilefile = tilefile
          IntState%Regrid(n)%PTR%gridname = gridname
       end if
           
! Set Alarms
! ----------

       if (list(n)%disabled) cycle

! His and Seg Alarms based on Reference Date and Time
! ---------------------------------------------------
       REF_TIME(1) =     list(n)%ref_date/10000
       REF_TIME(2) = mod(list(n)%ref_date,10000)/100
       REF_TIME(3) = mod(list(n)%ref_date,100)
       REF_TIME(4) =     list(n)%ref_time/10000
       REF_TIME(5) = mod(list(n)%ref_time,10000)/100
       REF_TIME(6) = mod(list(n)%ref_time,100)
       
       call ESMF_TimeSet( RefTime, YY = REF_TIME(1), &
                                   MM = REF_TIME(2), &
                                   DD = REF_TIME(3), &
                                   H  = REF_TIME(4), &
                                   M  = REF_TIME(5), &
                                   S  = REF_TIME(6), calendar=cal, rc=rc )

       sec = nsecf( list(n)%frequency )
       call ESMF_TimeIntervalSet( Frequency, S=sec, calendar=cal, rc=status ) ; VERIFY_(STATUS)
       RingTime = RefTime

! Added Logic to eliminate BEG_DATE = cap_restart date problem
! ------------------------------------------------------------
       if (RefTime == startTime) then
           RingTime = RefTime + Frequency
       end if

       if (RingTime < currTime .and. sec /= 0 ) then
           RingTime = RingTime + (INT((currTime - RingTime)/frequency)+1)*frequency
       endif
       list(n)%his_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
       VERIFY_(STATUS)
       
       if( list(n)%duration.ne.0 ) then
          sec = nsecf( list(n)%duration )
          call ESMF_TimeIntervalSet( Frequency, S=sec, calendar=cal, rc=status ) ; VERIFY_(STATUS)
          RingTime = RefTime
          if (RingTime < currTime) then
              RingTime = RingTime + (INT((currTime - RingTime)/frequency)+1)*frequency
          endif
          list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
          VERIFY_(STATUS)
       else
          list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, rc=status )
          VERIFY_(STATUS)
       endif

! Mon Alarm based on 1st of Month 00Z
! -----------------------------------
       REF_TIME(1) =     list(n)%ref_date/10000
       REF_TIME(2) = mod(list(n)%ref_date,10000)/100
       REF_TIME(3) = 1
       REF_TIME(4) = 0
       REF_TIME(5) = 0
       REF_TIME(6) = 0

       call ESMF_TimeSet( RefTime, YY = REF_TIME(1), &
                                   MM = REF_TIME(2), &
                                   DD = REF_TIME(3), &
                                   H  = REF_TIME(4), &
                                   M  = REF_TIME(5), &
                                   S  = REF_TIME(6), calendar=cal, rc=rc )

       call ESMF_TimeIntervalSet( Frequency, MM=1, calendar=cal, rc=status ) ; VERIFY_(STATUS)
       RingTime = RefTime
       do while ( RingTime < currTime )
          RingTime = RingTime + Frequency
       enddo
       list(n)%mon_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
       VERIFY_(STATUS)
       
! End Alarm based on end_date and end_time
! ----------------------------------------
       if( list(n)%end_date.ne.-999 .and. list(n)%end_time.ne.-999 ) then
           REF_TIME(1) =     list(n)%end_date/10000
           REF_TIME(2) = mod(list(n)%end_date,10000)/100
           REF_TIME(3) = mod(list(n)%end_date,100)
           REF_TIME(4) =     list(n)%end_time/10000
           REF_TIME(5) = mod(list(n)%end_time,10000)/100
           REF_TIME(6) = mod(list(n)%end_time,100) + 1 ! Add 1 second to make end_time inclusive
       
           call ESMF_TimeSet( RingTime, YY = REF_TIME(1), &
                                        MM = REF_TIME(2), &
                                        DD = REF_TIME(3), &
                                        H  = REF_TIME(4), &
                                        M  = REF_TIME(5), &
                                        S  = REF_TIME(6), calendar=cal, rc=rc )

           list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, rc=status )
           VERIFY_(STATUS)
        else
           list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=CurrTime, rc=status )
           VERIFY_(STATUS)
           call  ESMF_AlarmRingerOff(list(n)%end_alarm, rc=status )
           VERIFY_(STATUS)
       endif

       call ESMF_ConfigDestroy(cfg, rc=status)
       VERIFY_(STATUS)
    enddo LISTLOOP

    if( MAPL_AM_I_ROOT() ) print *

! START OF PARSER STUFF
    size0 = 1 !size( export )
    nstatelist = 0
    allocate( statelist(size0), stat=status )
    VERIFY_(STATUS)
    statelist(1) = ''


    do n=1,nlist
       do m=1,list(n)%nfield
          k=1
          if (scan(trim(list(n)%fields(1,m)),'()^/*+-')==0)then
          do while ( k.le.nstatelist )
             if (statelist(k) == '') statelist(k) = list(n)%fields(2,m)
             if( statelist(k).ne.list(n)%fields(2,m)) then
                k=k+1
             else
                exit
             end if
          enddo
          if(k.eq.nstatelist+1) then
             allocate( tmplist (nstatelist), stat=status )
             VERIFY_(STATUS)
             tmplist = statelist
             nstatelist = k
             deallocate( statelist )
             allocate( statelist(nstatelist), stat=status )
             VERIFY_(STATUS)
             statelist(1:k-1) = tmplist
             statelist(k)     = list(n)%fields(2,m)
             deallocate(   tmplist )
          endif
       endif
       enddo
    enddo

! Get Output Export States
! ------------------------

    allocate ( exptmp (size0), stat=status )
    VERIFY_(STATUS)
    exptmp(1) = import
!    deallocate ( export )
    allocate ( export(nstatelist), stat=status )
    VERIFY_(STATUS)
    errorFound = .false.
    allocate ( stateListAvail(nstatelist), stat=status )
    VERIFY_(STATUS)
    stateListAvail = .true.
    if (disableSubVmChecks) then
!ALT: setting disableSubVmChecks to .true. automatically assumes that subVm = .false.
       do n=1,nstatelist
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
          if( STATUS/= ESMF_SUCCESS ) then
             call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
             errorFound = .true.
          endif
       enddo
    else
       do n=1,nstatelist
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
          call ESMF_VMAllReduce(vm, sendData=status, recvData=globalStatus, &
               reduceflag=ESMF_REDUCE_MAX, rc=localStatus)
          VERIFY_(localStatus)

          if( STATUS/= ESMF_SUCCESS ) then
             stateListAvail(n) = .false.
          end if

          if( globalSTATUS/= ESMF_SUCCESS ) then
             call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
             errorFound = .true.
          endif

       enddo
    end if
    ASSERT_(.not. errorFound)
    deallocate ( exptmp )

! Associate Output Names with EXPORT State Index
! ----------------------------------------------
    list(:)%subVm = .false.
    do n=1,nlist
       allocate( list(n)%expSTATE(list(n)%nfield), stat=status )
       VERIFY_(STATUS)
       do m=1,list(n)%nfield
       if (scan(trim(list(n)%fields(1,m)),'()^/*+-')==0)then
          do k=1,nstatelist
             if( trim(list(n)%fields(2,m)) .eq. trim(statelist(k)) ) then
                if (.not. stateListAvail(k)) then
                   list(n)%subVm = .true.
                   cycle
                end if
                list(n)%expSTATE(m) = k
             end if
          enddo
       endif 
       enddo
    enddo

! Ensure Diagnostic Output has been Allocated
! -------------------------------------------
    errorFound = .false.
    do n=1,nlist
       if (list(n)%disabled) cycle
       if (list(n)%subVm) cycle
       do m=1,list(n)%nfield
          if (scan(trim(list(n)%fields(1,m)),'()^/*+-')==0)then
             call MAPL_StateGet( export(list(n)%expSTATE(m)), &
                  trim(list(n)%fields(1,m)), Field, rc=status )
             IF (STATUS /= ESMF_SUCCESS) then
                call WRITE_PARALLEL( "ERROR: cannot find output " // &
                     trim(list(n)%fields(1,m)) // " in " // &
                     trim(list(n)%fields(2,m)))
                errorFound = .true.
             else
                if (index(list(n)%fields(1,m),'%') ==0) then
                   call MAPL_AllocateCoupling(Field, rc=status)
                   VERIFY_(STATUS)
                end if

             end IF
          end if
       enddo
    enddo

    ASSERT_(.not. errorFound)

PARSER: do n=1,nlist

       allocate(tmprank(list(n)%nfield), stat=status);            VERIFY_(STATUS)
       allocate(tmploc(list(n)%nfield), stat=status);             VERIFY_(STATUS)

       do m=1,list(n)%nfield
       if (scan(trim(list(n)%fields(1,m)),'()^/*+-')==0)then
          call MAPL_StateGet( export(list(n)%expSTATE(m)),trim(list(n)%fields(1,m)),field,rc=status )
          IF (STATUS /= ESMF_SUCCESS) then
             call WRITE_PARALLEL( "ERROR: cannot find output " // &
                  trim(list(n)%fields(1,m)) // " in " // &
                  trim(list(n)%fields(2,m)))
             errorFound = .true.
             status=ESMF_SUCCESS
          else
             call ESMF_FieldGet(field,rank=dims,rc=status)
             VERIFY_(STATUS)
             tmprank(m)=dims
             if (dims > 2) then
                call ESMF_FieldGet(field,localDE=0,farrayPtr=ptr3d,rc=status)
                VERIFY_(STATUS)
                tmploc(m) = size(ptr3d,3)
             else
                tmploc(m) = 0
             end if
          endif
      endif
      enddo

      allocate(list(n)%tmpfields(list(n)%nfield), stat=status)
      VERIFY_(STATUS)
      allocate(list(n)%ReWrite(list(n)%nfield), stat=status)
      VERIFY_(STATUS)

      list(n)%tmpfields=''
      list(n)%ReWrite= .FALSE.

      call MAPL_SetExpression(list(n)%nfield,list(n)%fields,list(n)%tmpfields,list(n)%rewrite,  &
                              tmprank,tmploc, list(n)%nPExtraFields, &
                              list(n)%PExtraFields, list(n)%PExtraGridComp, import,rc=STATUS)
      VERIFY_(STATUS)
      deallocate(tmprank)
      deallocate(tmploc)

ENDDO PARSER
    deallocate(stateListAvail)
    deallocate(export)
    deallocate(statelist)
    do n=1,nlist
     deallocate(list(n)%expSTATE)
    enddo

! END OF PARSER STUFF

! Extract List of Unique Export State Names
! -----------------------------------------

    size0 = 1 !size( export )
    nstatelist = 0
    allocate( statelist(size0), stat=status )
    VERIFY_(STATUS)
    statelist(1) = ''

    
    do n=1,nlist
       do m=1,list(n)%nfield
          k=1
          do while ( k.le.nstatelist )
             if (statelist(k) == '') statelist(k) = list(n)%fields(2,m)
             if( statelist(k).ne.list(n)%fields(2,m)) then
                k=k+1
             else
                exit
             end if
          enddo
          if(k.eq.nstatelist+1) then
             allocate( tmplist (nstatelist), stat=status )
             VERIFY_(STATUS)
             tmplist = statelist
             nstatelist = k
             deallocate( statelist )
             allocate( statelist(nstatelist), stat=status )
             VERIFY_(STATUS)
             statelist(1:k-1) = tmplist
             statelist(k)     = list(n)%fields(2,m)
             deallocate(   tmplist )
          endif
       enddo
    enddo
 
! Get Output Export States
! ------------------------

    allocate ( exptmp (size0), stat=status )
    VERIFY_(STATUS)
    exptmp(1) = import
!    deallocate ( export )
    allocate ( export(nstatelist), stat=status )
    VERIFY_(STATUS)
    errorFound = .false.
    allocate ( stateListAvail(nstatelist), stat=status )
    VERIFY_(STATUS)
    stateListAvail = .true.
    if (disableSubVmChecks) then
!ALT: setting disableSubVmChecks to .true. automatically assumes that subVm = .false.
       do n=1,nstatelist
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
          if( STATUS/= ESMF_SUCCESS ) then
             call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
             errorFound = .true.
          endif
       enddo
    else
       do n=1,nstatelist
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
          call ESMF_VMAllReduce(vm, sendData=status, recvData=globalStatus, &
               reduceflag=ESMF_REDUCE_MAX, rc=localStatus)
          VERIFY_(localStatus)

          if( STATUS/= ESMF_SUCCESS ) then
             stateListAvail(n) = .false.
          end if

          if( globalSTATUS/= ESMF_SUCCESS ) then
             call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
             errorFound = .true.
          endif

       enddo
    end if
    ASSERT_(.not. errorFound)
    deallocate ( exptmp )

! Associate Output Names with EXPORT State Index
! ----------------------------------------------
    list(:)%subVm = .false.
    do n=1,nlist
       allocate( list(n)%expSTATE(list(n)%nfield), stat=status )
       VERIFY_(STATUS)
       do m=1,list(n)%nfield
          do k=1,nstatelist
             if( trim(list(n)%fields(2,m)) .eq. trim(statelist(k)) ) then
                if (.not. stateListAvail(k)) then
                   list(n)%subVm = .true.
                   cycle
                end if
                list(n)%expSTATE(m) = k
             end if
          enddo
       enddo
    enddo

   allocate(INTSTATE%AVERAGE    (nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%STAMPOFFSET(nlist), stat=status)
   VERIFY_(STATUS)

   IntState%average = .false.
   do n=1, nlist
      if (list(n)%disabled) cycle
      if(list(n)%mode == "instantaneous") then
         sec = 0
      else
         IntState%average(n) = .true.
         sec = nsecf(list(n)%acc_interval) / 2
      endif
      call ESMF_TimeIntervalSet( INTSTATE%STAMPOFFSET(n), S=sec, rc=status )
      VERIFY_(STATUS)
   end do

   nactual = npes
   if (.not. disableSubVmChecks) then
      allocate(allPes(npes), stat=status)
      VERIFY_(STATUS)
      minactual = npes
      do n=1, nlist
         NULLIFY(list(n)%peAve)
         if (list(n)%disabled) cycle
         localPe(1) = mype
         if (list(n)%subVm) localPe(1) = -1
         call ESMF_VMAllGather(vm, sendData=localPe, recvData=allPEs, &
              count=1, rc=status)
         VERIFY_(STATUS)
         nactual = count(allPEs >= 0)
         minactual = min(minactual, nactual)
         allocate(list(n)%peAve(nactual), stat=status)
         VERIFY_(STATUS)
         list(n)%peAve = pack(allPEs, allPEs>=0)
      end do
   
      IntState%npes = minactual
      deallocate(allPEs)
   end if

   allocate(INTSTATE%CCS(nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%GIM(nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%CIM(nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%SRCS(nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%DSTS(nlist), stat=status)
   VERIFY_(STATUS)
!   allocate(INTSTATE%GEX(nlist), stat=status)
!   VERIFY_(STATUS)
!   allocate(INTSTATE%GCNameList(nlist), stat=status)
!   VERIFY_(STATUS)

! Initialize Logical for Grads Control File
! -----------------------------------------

   allocate( INTSTATE%LCTL(nlist), stat=status )
   VERIFY_(STATUS)
   do n=1,nlist
      if (list(n)%disabled) cycle
      if( list(n)%format == 'flat' ) then
         INTSTATE%LCTL(n) = .true.
      else
         INTSTATE%LCTL(n) = .false.
      endif
   enddo

   do n=1, nlist
      if (list(n)%disabled) cycle
      if (list(n)%subVm) cycle
      
      IntState%GIM(n) = ESMF_StateCreate ( name=trim(list(n)%filename), &
           stateIntent = ESMF_STATEINTENT_IMPORT, &
           rc=status )
      VERIFY_(STATUS)
      if(list(n)%mode == "instantaneous") then
         IntState%average(n) = .false.
      else
         IntState%average(n) = .true.
         IntState%CIM(n) = ESMF_StateCreate ( name=trim(list(n)%filename), &
              stateIntent = ESMF_STATEINTENT_IMPORT, &
              rc=status )
         VERIFY_(STATUS)
         NULLIFY(INTSTATE%SRCS(n)%SPEC)
         NULLIFY(INTSTATE%DSTS(n)%SPEC)
      endif

      if (associated(IntState%Regrid(n)%PTR)) then
         ASSERT_(.not. list(n)%subVm) ! ALT: currently we are not supporting regridding on subVM
! query a field from export (arbitrary first field in the stream) for grid_in
         ASSERT_(size(export(list(n)%expSTATE)) > 0)
         call MAPL_StateGet( export(list(n)%expSTATE(1)), &
                             trim(list(n)%fields(1,1)), field, rc=status )
         VERIFY_(STATUS)
         IntState%Regrid(n)%PTR%state_out = ESMF_StateCreate ( name=trim(list(n)%filename)//'regrid_in', &
              stateIntent = ESMF_STATEINTENT_IMPORT, &
              rc=status )
         VERIFY_(STATUS)

! get grid name, layout, dims
         call ESMF_FieldGet(field, grid=grid_in, rc=status)
         VERIFY_(STATUS)
         call ESMF_GridGet(grid_in, name=gridname, distgrid=distgrid, rc=status)
         VERIFY_(STATUS)
         call ESMF_DistGridGet(distgrid, delayout=layout, rc=status)
         VERIFY_(STATUS)

         IntState%Regrid(n)%PTR%noxform = .false.

!        Check if is is tile variable: we could go the same grid attached to LS 
!        and use T2G or go to the "other" grid in the LS. In the later case, 
!        we need to find then "other LS" from the list of available LS in 
!        History, and calculate Xform, then do T2T, followed by T2G


         if (gridname(1:10) == 'tile_grid_') then

            ontiles = .true.

            ASSERT_(IntState%Regrid(n)%PTR%gridname /= '')

!ALT:       here we are getting the address of LocStream from the TILEGRID 
!           as INTEGER*8 attribute and we are using a C routine to 
!           set the pointer to LocStream

            call ESMF_AttributeGet(grid_in, name='TILEGRID_LOCSTREAM_ADDR', &
                 value=ADDR, rc=status)
            VERIFY_(STATUS)
            call c_MAPL_LocStreamRestorePtr(exch, ADDR)

!           Get the attached grid
            call MAPL_LocStreamGet(EXCH, ATTACHEDGRID=GRID_ATTACHED, RC=STATUS)
            VERIFY_(STATUS)

            call ESMF_GridGet(grid_attached, name=attachedName, rc=status)
            VERIFY_(STATUS)
            
            if (attachedName == IntState%Regrid(n)%PTR%gridname) then
!              T2G
               IntState%Regrid(n)%PTR%regridType = MAPL_T2G

               IntState%Regrid(n)%PTR%locOut = exch 

               IntState%Regrid(n)%PTR%noxform = .true.
               grid_out = grid_attached
               use_this_gridname = .true.
            else
!              this is also T2G but the grid is not the attached grid
!              done as T2T followed by T2G
               IntState%Regrid(n)%PTR%locIn = exch 
               IntState%Regrid(n)%PTR%regridType = MAPL_T2G
               IntState%Regrid(n)%PTR%noxform = .false.

! find the "other" locstream
               found = .false.
               ASSERT_(associated(LSADDR_PTR))
               do i = 1, size(LSADDR_PTR)
                  call c_MAPL_LocStreamRestorePtr(locStream, LSADDR_PTR(i))
                  call MAPL_LocStreamGet(locStream, ATTACHEDGRID=GRID, RC=STATUS)
                  VERIFY_(STATUS)
                  call ESMF_GridGet(grid, name=tmpstr, rc=status)
                  VERIFY_(STATUS)
                  if (tmpstr == IntState%Regrid(n)%PTR%gridname) then
                     found = .true.
                     exit
                  end if
               end do

               if (found) then
                  IntState%Regrid(n)%PTR%locOut = locStream
                  grid_out = grid
               else
!ALT: added new logic by Max request: if not found 
! open tile file get gridnames, make sure that "output" grid and "attached" grid are 2
! grids assoc with tile file, else ERROR
! do T2G on "internal" locstream, followed by G2G (G2T on "output" LS(attached grid),
! followed by T2T (Xform), and finally G2T on "output" LS("output" grid)

                  IntState%Regrid(n)%PTR%regridType = MAPL_T2G2G
                  ASSERT_(IntState%Regrid(n)%PTR%tilefile /= '')

                  ontiles = .false. !ALT: this is needed to force execution of G2G part

!>>>
!           get gridnames from exch
                  call MAPL_LocStreamGet(exch, GRIDNAMES = GNAMES, RC=STATUS)
                  VERIFY_(STATUS)

                  ngrids = size(gnames)
                  ASSERT_(ngrids==2)

                  ! find "complement" of attached grid
                  found = .false.
                  DO I = 1, NGRIDS
                     IF (GNAMES(I) == attachedNAME) THEN
                        FOUND = .TRUE.
                        exit
                     ENDIF
                  ENDDO
                  ASSERT_(FOUND)
                  NG = 3-I

                  ! find "complement" of exch
                  found = .false.
                  do i = 1, size(LSADDR_PTR)
                     call c_MAPL_LocStreamRestorePtr(locStream, LSADDR_PTR(i))
                     call MAPL_LocStreamGet(locStream, ATTACHEDGRID=GRID, RC=STATUS)
                     VERIFY_(STATUS)
                     call ESMF_GridGet(grid, name=tmpstr, rc=status)
                     VERIFY_(STATUS)
                     if (tmpstr == gnames(NG)) then
                        found = .true.
                        exit
                     end if
                  end do
                  ASSERT_(FOUND)
!<<<
                  grid_in = grid                               ! grid_attached
                  IntState%Regrid(n)%PTR%locNative = locStream ! exch
!XFORM create exch+locStream; and store it!
                  call MAPL_LocStreamCreateXform(XFORM=INTSTATE%Regrid(n)%PTR%XFORMntv, &
                       LocStreamOut=locStream, &
                       LocStreamIn=exch, &
                       NAME='historyXFORMnative', &
                       UseFCollect=.true., &
                       RC=STATUS )
                  VERIFY_(STATUS)

                  ! get the name and layout of attached grid
                  call ESMF_GridGet(grid_in, name=gridname, distgrid=distgrid, rc=status)
                  VERIFY_(STATUS)
                  call ESMF_DistGridGet(distgrid, delayout=layout, rc=status)
                  VERIFY_(STATUS)

                  call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locIn, &
                       layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
                       NAME='history_in', MASK=(/MAPL_Ocean/), grid=grid_in, RC=STATUS)
                  VERIFY_(STATUS)
               end if

            end if

         else
!           this is G2G done as G2T followed by T2T and then T2G
            IntState%Regrid(n)%PTR%regridType = MAPL_G2G
            ASSERT_(IntState%Regrid(n)%PTR%tilefile /= '')

            ontiles = .false.

            call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locIn, &
                 layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
                 NAME='history_in', MASK=(/MAPL_Ocean/), grid=grid_in, RC=STATUS)
            VERIFY_(STATUS)

         end if

         IntState%Regrid(n)%PTR%ontiles = ontiles

         if (.not. ontiles) then
!           get gridnames from loc_in
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 GRIDNAMES = GNAMES, RC=STATUS)
            VERIFY_(STATUS)
! query loc_in for ngrids
            ngrids = size(gnames)
            ASSERT_(ngrids==2)

            use_this_gridname = .false.
            IntState%Regrid(n)%PTR%noxform = .false.
! validate that gridname_in is there
            found = .false.
            DO I = 1, NGRIDS
               IF (GNAMES(I) == GRIDNAME) THEN
                  FOUND = .TRUE.
                  exit
               ENDIF
            ENDDO
            ASSERT_(FOUND)

! pick gridname_out
! we pick the "other" gridname. this works only when ngrids==2; 3-1=2;3-2=1
            NG = 3 - I 

!@@            if (use_this_gridname) then
!@@               NG = I
!@@            else
!@@               NG = 3 - I 
!@@            end if
! create grid_out

            call MAPL_GridGet(grid_in, globalCellCountPerDim=COUNTS, RC=STATUS)
            VERIFY_(STATUS)

            LM = COUNTS(3)

!ALT: for now we parse the grid name to figure out the origin of the grid
            POLE = GNAMES(NG)(1:2)
            NMLEN = LEN_TRIM(GNAMES(NG))
            DTLN = GNAMES(NG)(NMLEN-1:NMLEN)
         
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 GRIDIM = GRIDIM, GRIDJM=GRIDJM, RC=STATUS)
            VERIFY_(STATUS)

            DeltaX = 2.0*MAPL_PI/GRIDIM(NG)
            DeltaZ = 1.0D0
            select case (DTLN)
            case ('DE')
               X0 = -MAPL_PI                 ! dateline edge
            
            case ('DC')
               X0 = -MAPL_PI - DeltaX/2      ! dateline center
            
            case default
               RETURN_(ESMF_FAILURE)
            end select

            select case (POLE)
            case ('PE')
               DeltaY = MAPL_PI/GRIDJM(NG)
               Y0 = -0.5*MAPL_PI             ! South Pole edge
            
            case ('PC')
               DeltaY = MAPL_PI/(GRIDJM(NG)-1)
               Y0 = -0.5*MAPL_PI - DeltaY/2  ! South Pole center

            case default
               RETURN_(ESMF_FAILURE)
            end select

! Find out NX, NY from distgrid
!------------------------------
            call ESMF_GridGet(grid_in, distgrid=distgrid, &
                 dimCount=dimCount, rc=status)
            VERIFY_(STATUS)

!ALT: in concurrent env. we should get VM from distgrid
            call ESMF_VmGet(VM, petCount=ndes, rc=status)
            VERIFY_(STATUS)

            allocate(minindex(dimCount,ndes), maxindex(dimCount,ndes), stat=status)
            VERIFY_(STATUS)

            call ESMF_DistGridGet(distgrid, &
                 minIndexPDe=minindex, &
                 maxIndexPDe=maxindex, rc=status)
            VERIFY_(STATUS)

            call MAPL_GetImsJms(Imins=minindex(1,:),Imaxs=maxindex(1,:),&
                 Jmins=minindex(2,:),Jmaxs=maxindex(2,:),&
                 Ims=ims,Jms=jms,rc=status)
            VERIFY_(STATUS)

            deallocate(maxindex, minindex)

            NX = size(ims)
            NY = size(jms)

! Set the IMS and JMS vector
! --------------------------
            call MAPL_DecomposeDim ( GRIDIM(NG),ims,nx )
            call MAPL_DecomposeDim ( GRIDJM(NG),jms,ny )

            grid_out = ESMF_GridCreate(         &
                 name=GNAMES(NG),               &
                 countsPerDEDim1=ims,           &
                 countsPerDEDim2=jms,           &
                 indexFlag = ESMF_INDEX_USER,   &
                 gridMemLBound = (/1,1/),       &
                 gridEdgeLWidth = (/0,0/),      &
                 gridEdgeUWidth = (/0,0/),      &
                 coordDep1 = (/1,2/),           &
                 coordDep2 = (/1,2/),           &
                 rc=status)
            VERIFY_(STATUS)

            call ESMF_AttributeSet(grid_out, name='GRID_LM', value=LM, rc=status)
            VERIFY_(STATUS)

            deallocate(jms, ims)
! coordinates

! Allocate coords at default stagger location
            call ESMF_GridAddCoord(grid_out, rc=status)
            VERIFY_(STATUS)

            call ESMF_GridGetCoord(grid_out, coordDim=1, localDE=0, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, &
                 farrayPtr=coordX, rc=status)
            VERIFY_(STATUS)

            call ESMF_GridGetCoord(grid_out, coordDim=2, localDE=0, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, &
                 farrayPtr=coordY, rc=status)
            VERIFY_(STATUS)


            allocate(cornerX(GRIDIM(NG)+1),cornerY(GRIDJM(NG)+1), stat=status)
            VERIFY_(STATUS)
    
            cornerX(1) = X0
            do i = 1,GRIDIM(NG)
               cornerX(i+1) = cornerX(i) + deltaX
            enddo
       
            cornerY(1) = Y0
            do j = 1,GRIDJM(NG)
               cornerY(j+1) = cornerY(j) + deltaY
            enddo
    
            call ESMF_GRID_INTERIOR(grid_out,I1,IN,J1,JN)
            
            do i = 1,size(coordX,1)
               coordX(I,:) = 0.5d0*(cornerX(I+I1-1)+cornerX(I+I1))
            end do

            do j = 1,size(coordY,2)
               coordY(:,J) = 0.5d0*(cornerY(J+J1-1)+cornerY(J+J1))
            enddo

            deallocate(cornerY,cornerX)

! create and attach loc_out to grid_out
            call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locOut, &
                 layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
                 NAME='history_out', MASK=(/MAPL_Ocean/), Grid=grid_out, RC=STATUS)
            VERIFY_(STATUS)

!@@         else
!@@            grid_out = grids(NG)
         endif

! query ntiles
         call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locOut, &
              NT_LOCAL = IntState%Regrid(n)%PTR%ntiles_out, rc=status)
         VERIFY_(STATUS)

         if (.not.INTSTATE%Regrid(n)%PTR%noxform) then
! query ntiles
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 NT_LOCAL = IntState%Regrid(n)%PTR%ntiles_in, rc=status)
            VERIFY_(STATUS)

! create XFORM
            call MAPL_LocStreamCreateXform ( XFORM=INTSTATE%Regrid(n)%PTR%XFORM, &
                 LocStreamOut=INTSTATE%Regrid(n)%PTR%LocOut, &
                 LocStreamIn=INTSTATE%Regrid(n)%PTR%LocIn, &
                 NAME='historyXFORM', &
                 UseFCollect=.true., &
                 RC=STATUS )
            VERIFY_(STATUS)
         end if

      endif


      do m=1,list(n)%nfield
         call MAPL_StateGet( export(list(n)%expSTATE(m)), &
                             trim(list(n)%fields(1,m)), field, rc=status )
         VERIFY_(STATUS)

         if (.not.list(n)%rewrite(m) .or.list(n)%fields(4,m) /= BLANK ) then
          f = MAPL_FieldCreate(field, name=list(n)%fields(3,m), rc=status) 
         else
          DoCopy=.True.
          f = MAPL_FieldCreate(field, name=list(n)%fields(3,m), DoCopy=DoCopy, rc=status)
         endif
         VERIFY_(STATUS)
         if (list(n)%fields(4,m) /= BLANK) then
            if (list(n)%fields(4,m) == 'MIN') then
               call ESMF_AttributeSet(f, NAME='MINMAX', VALUE=MAPL_CplMin, RC=STATUS)
               VERIFY_(STATUS)
            else if (list(n)%fields(4,m) == 'MAX') then
               call ESMF_AttributeSet(f, NAME='MINMAX', VALUE=MAPL_CplMax, RC=STATUS)
               VERIFY_(STATUS)
            else
               call WRITE_PARALLEL("Functionality not supported yet")
            end if
         end if

         if (IntState%average(n)) then
            call MAPL_StateAdd(IntState%CIM(N), f, rc=status)
            VERIFY_(STATUS)

            ! borrow SPEC from FIELD
            ! modify SPEC to reflect accum/avg
            call ESMF_FieldGet(f, name=short_name, grid=grid, rc=status)
            VERIFY_(STATUS)

            call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='VLOCATION', VALUE=VLOCATION, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='FIELD_TYPE', VALUE=FIELD_TYPE, RC=STATUS)
            VERIFY_(STATUS)

            call ESMF_AttributeGet(FIELD, NAME='REFRESH_INTERVAL', VALUE=REFRESH, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='AVERAGING_INTERVAL', VALUE=avgint, RC=STATUS)
            VERIFY_(STATUS)

            call ESMF_FieldGet(FIELD, dimCount=fieldRank, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
            VERIFY_(STATUS)
            allocate(gridToFieldMap(gridRank), stat=status)
            VERIFY_(STATUS)
            call ESMF_FieldGet(FIELD, gridToFieldMap=gridToFieldMap, RC=STATUS)
            VERIFY_(STATUS)

            notGridded = count(gridToFieldMap==0)
            unGridDims = fieldRank - gridRank + notGridded

            hasUngridDims = .false.
            if (unGridDims > 0) then
               hasUngridDims = .true.
!ALT: special handling for 2d-MAPL grid (the vertical is treated as ungridded)
               if ((gridRank == 2) .and. (DIMS == MAPL_DimsHorzVert) .and. &
                    (unGridDims == 1)) then
                  hasUngridDims = .false.
               end if
            endif

            if (hasUngridDims) then
               allocate(ungriddedLBound(unGridDims), &
                        ungriddedUBound(unGridDims), &
                        ungrd(unGridDims),           &
                        stat=status)
               VERIFY_(STATUS)

!@               call ESMF_FieldGet(FIELD, &
!@                    ungriddedLBound=ungriddedLBound, &
!@                    ungriddedUBound=ungriddedUBound, &
!@                    RC=STATUS)
!@               VERIFY_(STATUS)
               

               call ESMF_FieldGet(field, Array=array, rc=status)
               VERIFY_(STATUS)

               call ESMF_ArrayGet(array, rank=rank, dimCount=dimCount, rc=status)
               VERIFY_(STATUS)
               undist = rank-dimCount
               ASSERT_(undist == ungridDims)

               call ESMF_ArrayGet(array, undistLBound=ungriddedLBound, &
                    undistUBound=ungriddedUBound, rc=status)
               VERIFY_(STATUS)

               ungrd = ungriddedUBound - ungriddedLBound + 1

               deallocate(ungriddedLBound,ungriddedUBound)

               call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,          &
                    SHORT_NAME = SHORT_NAME,                                 &
                    LONG_NAME  = LONG_NAME,                                  &
                    UNITS      = UNITS,                                      &
                    DIMS       = DIMS,                                       &
                    UNGRIDDED_DIMS = UNGRD,                                  &
                    ACCMLT_INTERVAL= avgint,                                 &
                    COUPLE_INTERVAL= REFRESH,                                &
                    VLOCATION  = VLOCATION,                                  &
                    FIELD_TYPE = FIELD_TYPE,                                 &
                    RC=STATUS  )
               VERIFY_(STATUS)

               call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,          &
                    SHORT_NAME = list(n)%fields(3,m),                        &
                    LONG_NAME  = LONG_NAME,                                  &
                    UNITS      = UNITS,                                      &
                    DIMS       = DIMS,                                       &
                    UNGRIDDED_DIMS = UNGRD,                                  &
                    ACCMLT_INTERVAL= nsecf(list(n)%acc_interval),            &
                    COUPLE_INTERVAL= nsecf(list(n)%frequency   ),            &
                    VLOCATION  = VLOCATION,                                  &
                    GRID       = GRID,                                       &
                    FIELD_TYPE = FIELD_TYPE,                                 &
                    RC=STATUS  )
               VERIFY_(STATUS)
               deallocate(ungrd)

            else

               call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,    &
                    SHORT_NAME = SHORT_NAME,                                 &
                    LONG_NAME  = LONG_NAME,                                  &
                    UNITS      = UNITS,                                      &
                    DIMS       = DIMS,                                       &
                    ACCMLT_INTERVAL= avgint,                                 &
                    COUPLE_INTERVAL= REFRESH,                                &
                    VLOCATION  = VLOCATION,                                  &
                    FIELD_TYPE = FIELD_TYPE,                                 &
                    RC=STATUS  )
               VERIFY_(STATUS)

               call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,    &
                    SHORT_NAME = list(n)%fields(3,m),                        &
                    LONG_NAME  = LONG_NAME,                                  &
                    UNITS      = UNITS,                                      &
                    DIMS       = DIMS,                                       &
                    ACCMLT_INTERVAL= nsecf(list(n)%acc_interval),            &
                    COUPLE_INTERVAL= nsecf(list(n)%frequency   ),            &
                    VLOCATION  = VLOCATION,                                  &
                    GRID       = GRID,                                       &
                    FIELD_TYPE = FIELD_TYPE,                                 &
                    RC=STATUS  )
               VERIFY_(STATUS)

            endif ! has_ungrid
            deallocate(gridToFieldMap)

         else ! else for if averaged

            REFRESH = nsecf(list(n)%acc_interval)
            AVGINT  = nsecf( list(n)%frequency )
            call ESMF_AttributeSet(F, NAME='REFRESH_INTERVAL', VALUE=REFRESH, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(F, NAME='AVERAGING_INTERVAL', VALUE=AVGINT, RC=STATUS)
            VERIFY_(STATUS)
            call MAPL_StateAdd(IntState%GIM(N), f, rc=status)
            VERIFY_(STATUS)

         endif

! Handle possible regridding through user supplied exchange grid
!---------------------------------------------------------------
         if (associated(IntState%Regrid(n)%PTR)) then
! replace field with newly created fld on grid_out
            field = MAPL_FieldCreate(f, grid_out, rc=status)
            VERIFY_(STATUS)
! add field to state_out
            call MAPL_StateAdd(IntState%Regrid(N)%PTR%state_out, &
                 field, rc=status)
            VERIFY_(STATUS)
         endif

      end do

! Handle possible extra fields needed for the parser
      if (list(n)%nPExtraFields > 0) then

         allocate ( exptmp (1), stat=status )
         VERIFY_(STATUS)
         exptmp(1) = import

         do m=1,list(n)%nPExtraFields
            call MAPL_ExportStateGet(exptmp,list(n)%PExtraGridComp(m),parser_state,rc=status)
            VERIFY_(STATUS)
            call MAPL_StateGet(parser_state,list(n)%PExtraFields(m),parser_field,rc=status)
            VERIFY_(STATUS)
            f = MAPL_FieldCreate(parser_field, name=list(n)%PExtraFields(m), rc=status)
            VERIFY_(STATUS)
            if (IntState%average(n)) then
               call MAPL_StateAdd(IntState%CIM(N), f, rc=status)
               VERIFY_(STATUS)
            else
               call MAPL_StateAdd(IntState%GIM(N), f, rc=status)
               VERIFY_(STATUS)
            end if                  
         end do

         deallocate(exptmp)

      end if

   end do

   do n=1, nlist
      if (list(n)%disabled) cycle
      if (IntState%average(n)) then
         
         call MAPL_StateCreateFromSpec(IntState%GIM(n), &
              IntState%DSTS(n)%SPEC,   &
              RC=STATUS  )
         VERIFY_(STATUS)

!         create CC
         if (nactual == npes) then
            IntState%CCS(n) = ESMF_CplCompCreate (                        &
                 NAME       = 'History', & 
                 RC=STATUS )
            VERIFY_(STATUS)
         else
            IntState%CCS(n) = ESMF_CplCompCreate (                        &
                 NAME       = 'History', & 
                 petList    = list(n)%peAve, &
                 RC=STATUS )
            VERIFY_(STATUS)
         end if

!         CCSetServ
         call ESMF_CplCompSetServices (IntState%CCS(n), &
                                       GenericCplSetServices, RC=STATUS )
         VERIFY_(STATUS)

         call MAPL_CplCompSetVarSpecs(IntState%CCS(n), &
                                      INTSTATE%SRCS(n)%SPEC,&
                                      INTSTATE%DSTS(n)%SPEC,RC=STATUS)
         VERIFY_(STATUS)

!         CCInitialize
         call ESMF_CplCompInitialize (INTSTATE%CCS(n), &
                                      importState=INTSTATE%CIM(n), &
                                      exportState=INTSTATE%GIM(n), &
                                      clock=CLOCK,           &
                                      userRC=STATUS)
         VERIFY_(STATUS)
      end if

   end do

    do n=1,nlist
       if (list(n)%disabled) cycle
       if (list(n)%subVm) list(n)%disabled = .true.
    end do


! CFIO
    do n=1,nlist
       if (list(n)%disabled) cycle

!ALT do rhis all the time       if (list(n)%format == 'CFIO') then
          write(string,'(a,i3.0)') 'STREAM',n

          list(n)%bundle = ESMF_FieldBundleCreate(NAME=string, RC=STATUS)
          VERIFY_(STATUS)

          if(associated(list(n)%levels)) then
             LM = size(list(n)%levels)
          else
             call ESMF_StateGet(INTSTATE%GIM(n), &
                  trim(list(n)%fields(3,1)), field, rc=status )
             VERIFY_(STATUS)
             call ESMF_FieldGet(field, grid=grid,   rc=status )
             VERIFY_(STATUS)
             call MAPL_GridGet(GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
             VERIFY_(STATUS)
             LM = counts(3)
          endif

          list(n)%slices = 0

          if (associated(IntState%Regrid(n)%PTR)) then
             state_out = INTSTATE%REGRID(n)%PTR%state_out
          else
             state_out = INTSTATE%GIM(n)
          end if

          do m=1,list(n)%nfield
             call ESMF_StateGet( state_out, &
                  trim(list(n)%fields(3,m)), field, rc=status )
             VERIFY_(STATUS)

             call MAPL_FieldBundleAdd( list(n)%bundle, field, rc=status )
             VERIFY_(STATUS)

             call ESMF_FieldGet(field, Array=array, rc=status)
             VERIFY_(STATUS)
             call ESMF_ArrayGet(array, rank=rank, rc=status)
             VERIFY_(STATUS)
             call ESMF_ArrayGet(array, localarrayList=larrayList, rc=status)
             VERIFY_(STATUS)
             larray => lArrayList(1) ! alias

             call ESMF_LocalArrayGet(larray, totalCount=counts, rc=status)
             VERIFY_(STATUS)

             if(list(n)%fields(3,m)/=vvarn(n)) then
                if(rank==2) then
                   list(n)%slices = list(n)%slices + 1
                elseif(rank==3) then
                   if(associated(list(n)%levels)) then
                      list(n)%slices = list(n)%slices + LM
                   else
                      list(n)%slices = list(n)%slices + COUNTS(3)
                   end if
                endif
             endif
          end do

!       endif
    enddo

    do n=1,nlist
       if (associated(list(n)%peAve)) then
          deallocate(list(n)%peAve)
          NULLIFY(list(n)%peAve)
       end if
    end do
    deallocate(Vvarn)

    do n=1,nlist
       if (list(n)%disabled) cycle
       if (list(n)%format == 'CFIO' .or. list(n)%format == 'CFIOasync') then
          call Get_Tdim (list(n), clock, tm)
          async = .false.
          if (list(n)%format == 'CFIOasync') async = .true.
          call MAPL_CFIOCreate(                        &
               MCFIO      = list(n)%MCFIO,             &
               NAME       = trim(list(n)%filename),    &
               CLOCK      = clock,                     &
               BUNDLE     = list(n)%bundle,            &
               OFFSET     = IntState%stampoffset(n),   &
               RESOLUTION = list(n)%resolution,        &
               SUBSET     = list(n)%subset,            &
               CHUNKSIZE  = list(n)%chunksize,         &
               FREQUENCY  = nsecf(list(n)%frequency),  &
               LEVELS     = list(n)%levels,            &
               DESCR      = list(n)%descr,             &
               XYOFFSET   = list(n)%xyoffset,          &
               VCOORD     = list(n)%VVARS(1),          &
               VUNIT      = list(n)%VUNIT,             &
               VSCALE     = list(n)%VSCALE,            &
               DEFLATE    = list(n)%deflate,           &
               NBITS      = list(n)%nbits,             &
               NUMCORES   = IntState%CoresPerNode,     &
               TM         = TM,                        &
               Conservative = list(n)%conservative,    &
               async      = async,                     &
               vectorList = list(n)%vectorList,        &
                                             RC=status )
          VERIFY_(STATUS)
       end if
   end do

! Echo History List Data Structure
! --------------------------------

   if( MAPL_AM_I_ROOT() ) then

      print *
      print *, 'Independent Output Export States:'
      print *, '---------------------------------'
      do n=1,nstatelist
         print *, n,trim(statelist(n))
      enddo
      print *

      do n=1,nlist
         if (list(n)%disabled) cycle
         print *, 'Initializing Output Stream: ',  trim(list(n)%filename)
         print *, '--------------------------- '
         print *, '      Format: ',  trim(list(n)%format)
         print *, '        Mode: ',  trim(list(n)%mode)
         print *, '       Nbits: ',       list(n)%nbits
         print *, '      Slices: ',       list(n)%Slices
         print *, '     Deflate: ',       list(n)%deflate
         print *, '   Frequency: ',       list(n)%frequency
         if(IntState%average(n) ) &
              print *, 'Acc_Interval: ',  list(n)%acc_interval
         print *, '    Ref_Date: ',       list(n)%ref_date
         print *, '    Ref_Time: ',       list(n)%ref_time
         print *, '    Duration: ',       list(n)%duration
         if( list(n)%end_date.ne.-999 ) then
         print *, '    End_Date: ',       list(n)%end_date
         print *, '    End_Time: ',       list(n)%end_time
         endif

         if( associated  ( list(n)%resolution ) ) then
                           print *, ' Output RSLV: ',list(n)%resolution
         endif
         select case ( list(n)%xyoffset   )
                case (0)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DcPc: Dateline Center, Pole Center)'
                case (1)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DePc: Dateline Edge, Pole Center)'
                case (2)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DcPe: Dateline Center, Pole Edge)'
                case (3)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DePe: Dateline Edge, Pole Edge)'
                case default
                ASSERT_(.false.)
         end select

         !print *, '      Fields: ',((trim(list(n)%fields(3,m)),' '),m=1,list(n)%nfield)
         do m=1,list(n)%nfield
            if( trim(list(n)%fields(3,m)).ne.BLANK ) then
               print *, '      Fields: ',trim(list(n)%fields(3,m)),' '
            endif
         enddo
         do m=1,list(n)%nfield
            if( trim(list(n)%fields(4,m)).ne.BLANK ) then
                print *, '   MINMAX Variable: ',trim(list(n)%fields(3,m)),'  Function: ',trim(list(n)%fields(4,m))
            endif
         enddo

         if( list(n)%vvars(1)/="" ) then
                                           print *, '   Vert Interp  Var: ',  trim(list(n)%vvars(1))
            if( trim(list(n)%vunit)/=""  ) print *, '   Vertical    Unit: ',  trim(list(n)%vunit)
            if(      list(n)%vscale/=1.0 ) print *, '   Vertical Scaling: ',       list(n)%vscale
                                           print *, '   Vertical  Levels: ',       list(n)%levels
         elseif(associated(list(n)%levels)) then
                                           print *, '   Vertical  Levels: ',  nint(list(n)%levels)
         endif

         print *
         print *
      enddo
   endif

    doAsync = .false.
    do n=1,nlist
       if (list(n)%format == 'CFIOasync') then
          doAsync = .true.
          exit
       end if
    enddo

    if (doAsync) then
       call MAPL_Get(GENSTATE,maplcomm=maplcomm,rc=status)
       VERIFY_(STATUS)
       if (maplcomm%iocommsize == 0) then
          call WRITE_PARALLEL('You requested the asynchronous option but did not allocate any resources')
          ASSERT_(.false.)
       end if
    end if

    deallocate(stateListAvail)
    deallocate( statelist )

    call MAPL_TimerOff(GENSTATE,"Initialize")
    call MAPL_TimerOff(GENSTATE,"TOTAL")

   RETURN_(ESMF_SUCCESS)
 end subroutine Initialize

!======================================================


 subroutine Run ( gc, import, export, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp),    intent(inout) :: gc     
    type(ESMF_State),       intent(inout) :: import 
    type(ESMF_State),       intent(inout) :: export 
    type(ESMF_Clock),       intent(inout) :: clock  
    integer, optional,      intent(  out) :: rc     
                                                    
                                                    
! Locals

    type(MAPL_MetaComp),  pointer  :: GENSTATE
    type(history_list),   pointer  :: list(:)
    type(HISTORY_STATE),  pointer  :: IntState
    type(HISTORY_wrap)             :: wrap
    integer                        :: nlist
    character(len=ESMF_MAXSTR)     :: fntmpl
    character(len=ESMF_MAXSTR),pointer     :: filename(:)
    integer                        :: n,m
    logical                        :: NewSeg
    logical, allocatable           :: Writing(:)
    type(ESMF_State)               :: state_out
    integer                        :: nymd, nhms
    character(len=ESMF_MAXSTR)     :: DateStamp
    integer                        :: n1, n2, nn, CollBlock
    type(MAPL_Communicators)       :: mapl_Comm
    integer                        :: IM,JM,IOnode
    character(len=ESMF_MAXSTR)     :: timestring

    character(len=ESMF_MAXSTR)     :: IAm="HistoryRun" 
    integer                        :: status

!=============================================================================

! Begin...

! Retrieve the pointer to the state
!----------------------------------

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    VERIFY_(status)
    IntState => wrap%ptr

! the collections
!----------------

    list => IntState%list
    nlist = size(list)

    CollBlock = IntState%blocksize
! Retrieve the pointer to the generic state
!------------------------------------------

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_Get(GENSTATE,maplcomm=mapl_comm,rc=status)
    VERIFY_(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Run"  )

! Couplers are done here for now
!-------------------------------

!  Perform arithemetic parser operations
   do n=1,nlist
    if ( Any(list(n)%ReWrite) ) then 
     call MAPL_TimerOn(GENSTATE,"-ParserRun")
     if( (.not.list(n)%disabled .and. IntState%average(n)) ) then
      call MAPL_RunExpression(IntState%CIM(n),list(n)%fields,list(n)%tmpfields, &
         list(n)%ReWrite,list(n)%nfield,RC=STATUS)
      VERIFY_(STATUS)
     end if
     if( (.not.list(n)%disabled) .and. (.not.IntState%average(n)) ) then
      call MAPL_RunExpression(IntState%GIM(n),list(n)%fields,list(n)%tmpfields, &
         list(n)%ReWrite,list(n)%nfield,RC=STATUS)
      VERIFY_(STATUS)
     end if
     call MAPL_TimerOff(GENSTATE,"-ParserRun")
    endif
   end do

    call MAPL_TimerOn(GENSTATE,"--Couplers")
    do n = 1, nlist
       if (.not.list(n)%disabled .and. IntState%average(n)) then
          
          call ESMF_CplCompRun (INTSTATE%CCS(n), &
                                importState=INTSTATE%CIM(n), &
                                exportState=INTSTATE%GIM(n), &
                                clock=CLOCK,           &
                                userRC=STATUS)
          VERIFY_(STATUS)
       end if
    end do
    call MAPL_TimerOff(GENSTATE,"--Couplers")
            
! Check for History Output
! ------------------------

   allocate(Writing (nlist), stat=status)
   VERIFY_(STATUS)
   allocate(filename(nlist), stat=status)
   VERIFY_(STATUS)

  ! decide if we are writing based on alarms

   do n=1,nlist
      if (list(n)%disabled .or. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then
         list(n)%disabled = .true.
         Writing(n) = .false.
      else
         Writing(n) = ESMF_AlarmIsRinging ( list(n)%his_alarm )
      endif

      call ESMF_AlarmRingerOff( list(n)%his_alarm,rc=status )
      VERIFY_(STATUS)
   end do

   if(any(Writing)) call WRITE_PARALLEL("")

   n1 = 1
   IOTIME: do while(n1<=nlist)

      nn=0
      do n=n1,nlist
         if(Writing(n)) nn = nn+1
         if(nn==CollBlock) exit
      enddo

      if(nn==0) exit

      n2=n1+nn-1

! Write Id and time
! -----------------

      call MAPL_TimerOn(GENSTATE,"--I/O")

! Set strategy for apportioning node "partitions" to active collections

      call SetPsizeStrategy(list(n1:n2)%Slices, Writing(n1:n2), n2-n1+1,  &
                            IntState%npes/IntState%CoresPerNode,    &
                            list(n1:n2)%Psize,  list(n1:n2)%Root         )

! The new strategy works on nodes, the code wants cores.

      do n=n1,n2
         if (writing(n)) then
            list(n)%Psize = IntState%CoresPerNode* list(n)%Psize
            list(n)%Root  = IntState%CoresPerNode*(list(n)%Root - 1)
         end if
      enddo
 
      do n=n1,n2
         if( Writing(n) ) then

            call get_DateStamp ( clock, DateStamp=DateStamp,  &
                 OFFSET = INTSTATE%STAMPOFFSET(n),            &
                                                    rc=status )
            VERIFY_(STATUS)

            if (trim(INTSTATE%expid) == "") then
               fntmpl =          trim(list(n)%filename)
            else
               fntmpl = "%s." // trim(list(n)%filename)
            endif

            if (trim(list(n)%template) /= "") then
               fntmpl = trim(fntmpl) // "." //trim(list(n)%template)
            endif

            read(DateStamp( 1: 8),'(i8.8)') nymd
            read(DateStamp(10:15),'(i6.6)') nhms

            call StrTemplate ( filename(n), fntmpl, 'GRADS', &
                 xid=trim(INTSTATE%expid), &
                 nymd=nymd, nhms=nhms, stat=status ) ! here is where we get the actual filename of file we will write
            VERIFY_(STATUS)
 
         endif
      enddo

      call MAPL_TimerOn(GENSTATE,"----IO Create")
      VERIFY_(STATUS)

      OPENLOOP: do n=n1,n2
         if( Writing(n) ) then

            call MAPL_CFIOSet(list(n)%mCFIO, Root=list(n)%Root, &
                              Psize=list(n)%Psize,     rc=status)
            VERIFY_(STATUS)

! Check for new segment
!----------------------

            NewSeg = ESMF_AlarmIsRinging ( list(n)%seg_alarm )

            if( NewSeg) then 
               call ESMF_AlarmRingerOff( list(n)%seg_alarm,rc=status )
               VERIFY_(STATUS)
            endif

            if( NewSeg .and. list(n)%unit /= 0 .and. list(n)%duration /= 0 ) then
               if (list(n)%unit > 0 ) then
                  call FREE_FILE( list(n)%unit )
               end if
               list(n)%unit = 0
            endif

            if( list(n)%unit.eq.0 ) then
               if (list(n)%format == 'CFIO') then
                  call MAPL_CFIOSet( list(n)%MCFIO, fName=filename(n), RC=status )
                  VERIFY_(STATUS)
                  call MAPL_CFIOCreateWrite(list(n)%MCFIO, RC=status )
                  VERIFY_(STATUS)
                  list(n)%unit = -1
               else if (list(n)%format == 'CFIOasync') then
                  call MAPL_CFIOSet( list(n)%MCFIO, fName=filename(n), RC=status )
                  VERIFY_(STATUS)
                  call MAPL_CFIOCreateWrite(list(n)%MCFIO, RC=status )
                  VERIFY_(STATUS)
                  call MAPL_CFIOClose (list(n)%mcfio, rc=status)
                  VERIFY_(STATUS)
                  list(n)%unit = -2
               else
                  list(n)%unit = GETFILE( trim(filename(n)))
               end if
            end if

            if(  MAPL_AM_I_ROOT() ) then
                 write(6,'(1X,"Writing: ",i6," Slices (",i3," Nodes, ",i2," CoresPerNode) to File:  ",a)') &
                       list(n)%Slices,list(n)%Psize/INTSTATE%CoresPerNode,INTSTATE%CoresPerNode, &
                       trim(MAPL_CFIOGetFilename(list(n)%MCFIO))
            endif

         end if

      enddo OPENLOOP
      call MAPL_TimerOff(GENSTATE,"----IO Create")

      call MAPL_TimerOn(GENSTATE,"----IO Write")
      call MAPL_TimerOn(GENSTATE,"-----IO Post")
      POSTLOOP: do n=n1,n2

         OUTTIME: if( Writing(n) ) then

            if (associated(IntState%Regrid(n)%PTR)) then
               state_out = INTSTATE%REGRID(n)%PTR%state_out

               if (.not. IntState%Regrid(n)%PTR%ontiles) then
                  if (IntState%Regrid(n)%PTR%regridType == MAPL_T2G2G) then
                     call RegridTransformT2G2G(IntState%GIM(n), &
                          IntState%Regrid(n)%PTR%xform, &
                          IntState%Regrid(n)%PTR%xformNtv, &
                          state_out, &
                          IntState%Regrid(n)%PTR%LocIn, &
                          IntState%Regrid(n)%PTR%LocOut, &
                          IntState%Regrid(n)%PTR%LocNative, &
                          IntState%Regrid(n)%PTR%ntiles_in, &
                          IntState%Regrid(n)%PTR%ntiles_out,&
                          rc=status)
                  else
                     call RegridTransform(IntState%GIM(n), &
                          IntState%Regrid(n)%PTR%xform, &
                          state_out, &
                          IntState%Regrid(n)%PTR%LocIn, &
                          IntState%Regrid(n)%PTR%LocOut, &
                          IntState%Regrid(n)%PTR%ntiles_in, &
                          IntState%Regrid(n)%PTR%ntiles_out,&
                          rc=status)
                  end if
               else
                  if (IntState%Regrid(n)%PTR%noxform) then
                     call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                          STATE_OUT=state_out, &
                          LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                          NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                          rc=status)
                  else
                     call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                          XFORM=IntState%Regrid(n)%PTR%xform, &
                          STATE_OUT=state_out, &
                          LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                          NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                          rc=status)
                  end if
               end if
               VERIFY_(STATUS)
            else
               state_out = INTSTATE%GIM(n)
            end if

            IOTYPE: if (list(n)%unit < 0) then    ! CFIO 
               call MAPL_CFIOWriteBundlePost( list(n)%MCFIO,  RC=status)
               VERIFY_(STATUS)

            else

               if( INTSTATE%LCTL(n) ) then
                  call MAPL_GradsCtlWrite ( clock, state_out, list(n), &
                       filename(n), INTSTATE%expid, &
                       list(n)%descr, rc )
                  INTSTATE%LCTL(n) = .false.
               endif

               do m=1,list(n)%nfield
                  call MAPL_VarWrite ( list(n)%unit, STATE=state_out, &
                       NAME=trim(list(n)%fields(3,m)), &
                       forceWriteNoRestart=.true., rc=status )
                  VERIFY_(STATUS)
               enddo
               call WRITE_PARALLEL("Wrote GrADS Output for File: "//trim(filename(n)))

            end if IOTYPE

         endif OUTTIME

      enddo POSTLOOP

      call MAPL_TimerOff(GENSTATE,"-----IO Post")
      call MAPL_TimerOff(GENSTATE,"----IO Write")

      call MAPL_TimerOn(GENSTATE,"----IO Write")
      call MAPL_TimerOn(GENSTATE,"-----IO Wait")

      IOSERVERLOOP: do n=n1,n2

         if (writing(n) .and. list(n)%unit <0 .and. list(n)%format == 'CFIOasync' ) then
            if (mapl_comm%myGlobalRank == list(n)%root) then

               call MAPL_CFIOStartAsyncColl(list(n)%mcfio,clock,mapl_comm,filename(n) &
                   ,intstate%markdone,rc=status)
               VERIFY_(STATUS)

            end if
            call MAPL_CFIOSet(list(n)%mcfio,GlobalComm=mapl_comm%maplcomm,rc=status)
            VERIFY_(STATUS)
            call MAPL_CFIOBcastIONode(list(n)%mcfio,list(n)%root,mapl_comm%esmfcomm,rc=status)
            VERIFY_(STATUS)
         end if

      enddo IOSERVERLOOP

      WAITLOOP: do n=n1,n2

         if( Writing(n) .and. list(n)%unit < 0) then

            call MAPL_CFIOWriteBundleWait(list(n)%MCFIO, CLOCK, RC=status)
            VERIFY_(STATUS)

         end if

      enddo WAITLOOP

      call MAPL_TimerOff(GENSTATE,"-----IO Wait")
      call MAPL_TimerOff(GENSTATE,"----IO Write")

      call MAPL_TimerOn(GENSTATE,"----IO Write")
      call MAPL_TimerOn(GENSTATE,"-----IO Write")

      WRITELOOP: do n=n1,n2

         if( Writing(n) .and. list(n)%unit < 0) then

               if (list(n)%format == 'CFIO' .and. list(n)%unit == -2 ) then
                  call MAPL_CFIOOpenWrite(list(n)%MCFIO, RC=status)
               end if

            call MAPL_CFIOWriteBundleWrite(list(n)%MCFIO, CLOCK, RC=status)
            VERIFY_(STATUS)

            if (INTSTATE%MarkDone /=0) then
               if (list(n)%format =='CFIO') then
                  call MAPL_CFIOClose (list(n)%mcfio, filename(n), rc=status)
                  VERIFY_(STATUS)
               end if
            else
               if (list(n)%format == 'CFIO') then
                  call MAPL_CFIOClose (list(n)%mcfio, rc=status)
                  VERIFY_(STATUS)
               end if
            endif

            list(n)%unit = -2

         end if

      enddo WRITELOOP

      call MAPL_TimerOff(GENSTATE,"-----IO Write")
      call MAPL_TimerOff(GENSTATE,"----IO Write")

      call MAPL_TimerOff(GENSTATE,"--I/O"       )

      n1=n2+1

   enddo IOTIME

   if(any(Writing)) call WRITE_PARALLEL("")


   deallocate(filename)
   deallocate(Writing)

   call MAPL_TimerOff(GENSTATE,"Run"         )
   call MAPL_TimerOff(GENSTATE,"TOTAL"       )

   RETURN_(ESMF_SUCCESS)
 end subroutine Run

!======================================================

  subroutine Finalize ( gc, import, export, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout)    :: gc     ! composite gridded component 
    type(ESMF_State),       intent(inout) :: import ! import state
    type(ESMF_State),       intent(  out) :: export ! export state
    type(ESMF_Clock),       intent(inout) :: clock  ! the clock
  
    integer, intent(out), OPTIONAL        :: rc     ! Error code:
                                                     ! = 0 all is well
                                                     ! otherwise, error

    character(len=ESMF_MAXSTR)      :: IAm="Finalize" 
    integer                         :: status
    type(history_list), pointer     :: list(:)
    type(HISTORY_wrap)              :: wrap
    type (HISTORY_STATE), pointer   :: IntState
    integer                         :: nlist, n
    type (MAPL_MetaComp), pointer :: GENSTATE
    logical                       :: doAsync
    type(MAPL_Communicators)      :: mapl_Comm
 

! Begin...

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_Get(GENSTATE,maplcomm=mapl_comm,rc=status)
    VERIFY_(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Finalize")

! Retrieve the pointer to the state

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    VERIFY_(status)
    IntState => wrap%ptr
    list => IntState%list
    nlist = size(list)

! Close UNITs of GEOSgcm History Data
! -----------------------------------

   do n=1,nlist
      if (list(n)%disabled) cycle
      IF (list(n)%format == 'CFIO' .or. list(n)%format == 'GRADS') then
         if( MAPL_CFIOIsCreated(list(n)%mcfio) ) then
            CALL MAPL_CFIOdestroy (list(n)%mcfio, rc=STATUS)
            VERIFY_(STATUS)
         end if
      ELSE
         if( list(n)%unit.ne.0 ) call FREE_FILE( list(n)%unit )
      END if
   enddo

#if 0
   do n=1,nlist
      IF (IntState%average(n)) then
         call MAPL_StateDestroy(IntState%gim(n), rc=status)
         VERIFY_(STATUS)
         call MAPL_StateDestroy(IntState%cim(n), rc=status)
         VERIFY_(STATUS)
      end IF
   enddo
#endif

    if (mapl_comm%iocommsize > 0) then
       if (mapl_am_i_root()) then
          call MPI_Send(mapl_comm%myGlobalRank,1,MPI_INTEGER,mapl_comm%ioCommRoot,MAPL_TAG_NORMALEXIT, &
           mapL_comm%maplcomm,status)
          VERIFY_(STATUS)
          call MPI_Recv(n,1,MPI_INTEGER,mapl_comm%ioCommRoot,MAPL_TAG_WORKEREXIT, &
           mapl_comm%maplcomm,MPI_STATUS_IGNORE,status)
       end if
    end if

    call MAPL_TimerOff(GENSTATE,"Finalize")
    call MAPL_TimerOff(GENSTATE,"TOTAL")

    call  MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK, RC=status )
    VERIFY_(STATUS)


    RETURN_(ESMF_SUCCESS)
  end subroutine Finalize

!======================================================
 subroutine MAPL_GradsCtlWrite ( clock, state,list,fname,expid,expdsc,rc )
   
   type(ESMF_Clock),  intent(inout) :: clock
   type(ESMF_State)                 :: state
   type(history_list)               :: list
   character(len=*)                 :: expid
   character(len=*)                 :: expdsc
   character(len=*)                 :: fname
   integer, optional, intent(out)   :: rc
   
   type(ESMF_Array)               :: array
   type(ESMF_LocalArray)          :: larraylist(1)
   type(ESMF_Field)               :: field
   type(ESMF_Grid)                :: grid
   type(ESMF_Time)                :: CurrTime
   type(ESMF_Time)                :: StopTime
   type(ESMF_Calendar)            :: cal
   type(ESMF_TimeInterval)        :: ti, Frequency
   integer                        :: nsteps
   integer, dimension(ESMF_MAXDIM):: lbounds, ubounds
   integer, allocatable           :: vdim(:)
   character(len=ESMF_MAXSTR)     :: TimeString
   character(len=ESMF_MAXSTR)     :: filename
   character(len=ESMF_MAXSTR)     :: options
   integer                        :: DIMS(3)
   integer                        :: COUNTS(3)
   integer                        :: IM,JM,LM
   
   character*3                    :: months(12)
   data months /'JAN','FEB','MAR','APR','MAY','JUN', &
                'JUL','AUG','SEP','OCT','NOV','DEC'/
   
   integer      :: unit,nfield
   character(len=ESMF_MAXSTR)      :: IAm="MAPL_GradsCtlWrite" 
   integer      :: k,m,n,nsecf,nhms,rank,status
   integer      :: year,month,day,hour,minute
   real*8   LONBEG,DLON
   real*8   LATBEG,DLAT
   integer  mass, freq,zero
   real(KIND=4),      pointer :: LATS(:,:), LONS(:,:)

   
! Mass-Weighted Diagnostics
! -------------------------
   integer     km
   parameter ( km = 4 )
   character(len=ESMF_MAXSTR) :: name(2,km)
   data name / 'THIM'     , 'PHYSICS'    , & 
               'SIT'      , 'PHYSICS'    , & 
               'DTDT'     , 'PHYSICS'    , &
               'DTDT'     , 'GWD'        /

   nsecf(nhms) = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)

   call ESMF_ClockGet ( clock,  currTime=CurrTime ,rc=STATUS ) ; VERIFY_(STATUS)
   call ESMF_ClockGet ( clock,  StopTime=StopTime ,rc=STATUS ) ; VERIFY_(STATUS)
   call ESMF_ClockGet ( clock,  Calendar=cal      ,rc=STATUS ) ; VERIFY_(STATUS)
   
   call ESMF_TimeGet  ( CurrTime, timeString=TimeString, rc=status ) ; VERIFY_(STATUS)
   
   read(timestring( 1: 4),'(i4.4)') year
   read(timestring( 6: 7),'(i2.2)') month
   read(timestring( 9:10),'(i2.2)') day
   read(timestring(12:13),'(i2.2)') hour
   read(timestring(15:16),'(i2.2)') minute
   
   ti = StopTime-CurrTime
   freq = nsecf( list%frequency )
   call ESMF_TimeIntervalSet( Frequency, S=freq, calendar=cal, rc=status ) ; VERIFY_(STATUS)
   
   nsteps =  ti/Frequency + 1
   
   if( trim(expid) == "" ) then
       filename =                       trim(list%collection)
   else
       filename = trim(expid) // '.' // trim(list%collection)
   endif
           unit = GETFILE( trim(filename) // '.ctl', form="formatted" )

   if( list%template == "" .or. list%duration == 0 ) then
       options  = 'options sequential'
       filename = trim(fname)
   else
       options  = 'options sequential template'
       filename = trim(filename) // '.' // trim(list%template)
   endif

! Get Global Horizontal Dimensions
! --------------------------------
   call ESMF_StateGet ( state,trim(list%fields(3,1)),field,rc=status )
   VERIFY_(STATUS)
   call ESMF_FieldGet ( field, grid=grid, rc=status )
   VERIFY_(STATUS)
   
   call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
   VERIFY_(STATUS)

   ZERO   =  0
   IM     =  DIMS(1)
   JM     =  DIMS(2)
   LM     =  DIMS(3)
   if (LM == 0) LM = 1 ! needed for tilegrids

   if (IM /= 1) then
      DLON   =  360._8/ IM
   else
      DLON = 1.0
   end if

   if (JM /= 1) then
      DLAT   =  180._8/(JM-1)
   else
      DLAT   =  1.0
   end if

  call ESMFL_GridCoordGet(   GRID, LATS       , &
                             Name     = "Latitude"              , &
                             Location = ESMF_STAGGERLOC_CENTER  , &
                             Units    = ESMFL_UnitsRadians      , &
                             RC       = STATUS                    )
  VERIFY_(STATUS)

  call ESMFL_GridCoordGet(   GRID, LONS       , &
                             Name     = "Longitude"             , &
                             Location = ESMF_STAGGERLOC_CENTER  , &
                             Units    = ESMFL_UnitsRadians      , &
                             RC       = STATUS                    )
  VERIFY_(STATUS)

!ALT: Note: the LATS(1,1) and LONS(1,1) are correct ONLY on root
   if( MAPL_AM_I_ROOT() ) then
       LONBEG = LONS(1,1)*(180._8/MAPL_PI_R8)
       if (size(LONS,1) > 1) then
          DLON = (LONS(2,1)-LONS(1,1))*(180._8/MAPL_PI_R8)
       end if

       LATBEG = LATS(1,1)*(180._8/MAPL_PI_R8)
       if (size(LATS,2) > 1) then
          DLAT = (LATS(1,2)-LATS(1,1))*(180._8/MAPL_PI_R8)
       end if
   endif

!
! Check if changing resolution
! -------------------------------------------------------------------------
   if (associated( list%resolution )) then
      IM = list%resolution(1)
      JM = list%resolution(2)
      DLON   =  360._8/float(IM)
      if (JM /= 1) then
         DLAT   =  180._8/float(JM-1)
      else
         DLAT   =  1.0
      end if
      LONBEG = -180._8
      LATBEG =  -90._8
   endif

! Compute Vertical Dimension for each Field (Augment nfield for VDIMS > LM)
! -------------------------------------------------------------------------
   allocate( vdim(list%nfield), stat=status )
   VERIFY_(STATUS)
   vdim = 0
   nfield =   list%nfield
   do m = 1,list%nfield
      call ESMFL_StateGetFieldArray( state,trim(list%fields(3,m)),array,status )
      VERIFY_(STATUS)
      call ESMF_ArrayGet( array, localarrayList=larrayList, rc=status )
      VERIFY_(STATUS)
      call ESMF_LocalArrayGet( larrayList(1), RANK=rank, totalLBound=lbounds, &
           totalUBound=ubounds, rc=status )
      VERIFY_(STATUS)
      if( rank==3 ) then
         vdim(m) = ubounds(3)-lbounds(3)+1
         if( vdim(m).gt.LM ) nfield = nfield+1
      else if( rank==4 ) then
         vdim(m) = -(ubounds(3)-lbounds(3)+1)*(ubounds(4)-lbounds(4)+1)
      endif
   enddo

! Create Grads Control File
! -------------------------
   if( MAPL_AM_I_ROOT() ) then
      print *
      if ( freq < 3600 ) then
         write(unit,201) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/60, nfield
      else if ( freq < 86400 ) then
         write(unit,202) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/3600, nfield
      else if ( freq < 30*86400 ) then
         write(unit,203) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/86400, nfield
      else 
         write(unit,204) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/(30*86400), nfield
      endif
      do m=1,list%nfield
         mass = 0
         do k=1,km
            if( trim(list%fields(1,m)).eq.trim(name(1,k))  .and. &
                 trim(list%fields(2,m)).eq.trim(name(2,k)) ) mass = 1  ! Check for Mass-Weighted Diagnostics
         enddo
         if( vdim(m).le.LM ) then
            write(unit,102) trim(list%fields(3,m)),abs(vdim(m)),mass,trim(list%fields(3,m))
         else
            write(unit,102) trim(list%fields(3,m)),LM     ,mass,trim(list%fields(3,m))
            if( trim(list%fields(1,m)).eq.'PLE' ) then
               write(unit,102) 'PS',zero,mass,'PS'
            else
               write(unit,102) trim(list%fields(3,m)) // 's',zero,mass,trim(list%fields(3,m)) // 's'
            endif
         endif
      enddo
      write(unit,103)
   endif
   call FREE_FILE( unit )
   deallocate( vdim )
   
201     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mn',/, &
	       'vars  ',i3)
202     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'hr',/, &
	       'vars  ',i3)
203     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'dy',/, &
	       'vars  ',i3)
204     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mo',/, &
	       'vars  ',i3)
102     format(a,i3,2x,i3,2x,"'",a,"'")
103     format('endvars')

   RETURN_(ESMF_SUCCESS)
 end subroutine MAPL_GradsCtlWrite


  subroutine get_DateStamp (clock, DateStamp, offset, rc)
    type (ESMF_Clock)                   :: clock
    character(len=ESMF_MAXSTR),optional :: DateStamp
    type(ESMF_TimeInterval),   optional :: offset
    integer, optional                   :: rc

    type(ESMF_Time)                   :: currentTime
    type(ESMF_Alarm)                  :: PERPETUAL
    character(len=ESMF_MAXSTR)        :: TimeString
    character(len=ESMF_MAXSTR)        :: TimeStamp
    character(len=ESMF_MAXSTR)        :: clockname
    character                         :: String(ESMF_MAXSTR)
    logical                           :: LPERP
    integer                           :: YY,MM,DD,H,M,S
    integer                           :: noffset

    character*4 year
    character*2 month
    character*2 day
    character*2 hour
    character*2 minute
    character*2 second

    integer                    :: STATUS
    character(len=ESMF_MAXSTR) :: Iam="get_DateStamp"

    equivalence ( string(01),TimeString )
    equivalence ( string(01),year       )
    equivalence ( string(06),month      )
    equivalence ( string(09),day        )
    equivalence ( string(12),hour       )
    equivalence ( string(15),minute     )
    equivalence ( string(18),second     )

    call ESMF_ClockGet ( clock, name=clockname, currTime=currentTime, rc=status)
    VERIFY_(STATUS)

    if (present(offset)) then
        call ESMF_TimeIntervalGet( OFFSET, S=noffset, rc=status )
        VERIFY_(STATUS)
        if( noffset /= 0 ) then
            LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( clock, AlarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
            VERIFY_(STATUS)
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
!
! Month has already been set back to PERPETUAL Month, therefore
! Time-Averaged Files (i.e., non-zero offset) need Month to be advanced for proper offset calculation
! ---------------------------------------------------------------------------------------------------
                call ESMF_TimeGet ( CurrentTime, YY = YY, &
                                                 MM = MM, &
                                                 DD = DD, &
                                                 H  = H , &
                                                 M  = M , &
                                                 S  = S, rc=status )
                                                 MM = MM + 1
                call ESMF_TimeSet ( CurrentTime, YY = YY, &
                                                 MM = MM, &
                                                 DD = DD, &
                                                 H  = H , &
                                                 M  = M , &
                                                 S  = S, rc=status )
#ifdef DEBUG
      if( MAPL_AM_I_ROOT() ) write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside HIST GetDate: ",YY,MM,DD,H,M,S
#endif
            endif
        endif
        endif
        currentTime = currentTime - offset
    end if

    call ESMF_TimeGet (currentTime, timeString=TimeString, rc=status)
    VERIFY_(STATUS)

    if(present(DateStamp)) then
       DateStamp = year//month//day//'_'//hour//minute//second //'z'
    end if
    
    RETURN_(ESMF_SUCCESS)
  end subroutine get_DateStamp

  subroutine RegridTransform(STATE_IN, XFORM, STATE_OUT, LS_IN, LS_OUT, NTILES_IN, NTILES_OUT, RC)
    type (ESMF_State)        , intent(IN   ) :: STATE_IN
    type (ESMF_State)        , intent(INOUT) :: STATE_OUT
    type(MAPL_LocStreamXform), intent(IN   ) :: XFORM
    type(MAPL_LocStream)     , intent(IN   ) :: LS_IN, LS_OUT
    integer                  , intent(IN   ) :: NTILES_IN, NTILES_OUT
    integer, optional        , intent(  OUT) :: RC

    integer                    :: STATUS
    character(len=ESMF_MAXSTR), parameter :: Iam='RegridTransform'

    integer                         :: L, LM
    integer                         :: LL, LU
    integer                         :: I
    integer                         :: rank_in
    integer                         :: rank_out
    integer                         :: itemcount, itemcount_in, itemcount_out
    real, allocatable, dimension(:) :: tile_in, tile_out
    real, pointer                   :: ptr2d_in(:,:)
    real, pointer                   :: ptr2d_out(:,:)
    real, pointer                   :: ptr3d_in(:,:,:)
    real, pointer                   :: ptr3d_out(:,:,:)
    type(ESMF_Array)                :: array_in
    type(ESMF_Array)                :: array_out
    type(ESMF_Field)                :: field
    type (ESMF_StateItem_Flag), pointer  :: ITEMTYPES_IN(:), ITEMTYPES_OUT(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES_IN(:), ITEMNAMES_OUT(:)

    allocate(tile_in (ntiles_in ), stat=status)
    VERIFY_(STATUS)
    allocate(tile_out(ntiles_out), stat=status)
    VERIFY_(STATUS)


    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, RC=STATUS)
    VERIFY_(STATUS)

    ASSERT_(ITEMCOUNT_IN == ITEMCOUNT_OUT)

    ITEMCOUNT = ITEMCOUNT_IN
    ASSERT_(ITEMCOUNT>0)

    allocate(ITEMNAMES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       ITEMTYPELIST=ITEMTYPES_IN, RC=STATUS)
    VERIFY_(STATUS)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       ITEMTYPELIST=ITEMTYPES_OUT, RC=STATUS)
    VERIFY_(STATUS)

    DO I=1, ITEMCOUNT
       ASSERT_(ITEMTYPES_IN (I) == ESMF_StateItem_Field)
       ASSERT_(ITEMTYPES_OUT(I) == ESMF_StateItem_Field)

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_out, rc=status)
       VERIFY_(STATUS)

       call ESMF_ArrayGet(array_in , rank=rank_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_ArrayGet(array_out, rank=rank_out, rc=status)
       VERIFY_(STATUS)
       ASSERT_(rank_in == rank_out)
       ASSERT_(rank_in >=2 .and. rank_in <= 3)

       if (rank_in == 2) then
          LM = 1
          LL = 1
          LU = 1
          call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , rc=status)
          VERIFY_(STATUS)
          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, rc=status)
          VERIFY_(STATUS)
       else
          call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , rc=status)
          VERIFY_(STATUS)
          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, rc=status)
          VERIFY_(STATUS)
          LM = size(ptr3d_in,3)
          LL = lbound(ptr3d_in,3)
          LU = ubound(ptr3d_in,3)
          ASSERT_(size(ptr3d_out,3) == LM)
          ASSERT_(lbound(ptr3d_out,3) == LL)
          ASSERT_(ubound(ptr3d_out,3) == LU)
       end if

       DO L=LL,LU
          if (rank_in == 3) then
             ptr2d_in  => ptr3d_in (:,:,L)
             ptr2d_out => ptr3d_out(:,:,L)
          end if

          call MAPL_LocStreamTransform(LS_IN, TILE_IN, PTR2d_IN, RC=STATUS)
          VERIFY_(STATUS)

          call MAPL_LocStreamTransform( tile_out, XFORM, tile_in, RC=STATUS ) 
          VERIFY_(STATUS)

          call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, RC=STATUS)
          VERIFY_(STATUS)

       ENDDO

    ENDDO

    deallocate(itemtypes_out)
    deallocate(itemnames_out)
    deallocate(itemtypes_in)
    deallocate(itemnames_in)
    deallocate(tile_out)
    deallocate(tile_in )

    RETURN_(ESMF_SUCCESS)
  end subroutine RegridTransform

  subroutine RegridTransformT2G2G(STATE_IN, XFORM, XFORMntv, STATE_OUT, LS_IN, LS_OUT, LS_NTV, NTILES_IN, NTILES_OUT, RC)
    type (ESMF_State)        , intent(IN   ) :: STATE_IN
    type (ESMF_State)        , intent(INOUT) :: STATE_OUT
    type(MAPL_LocStreamXform), intent(IN   ) :: XFORM, XFORMntv
    type(MAPL_LocStream)     , intent(IN   ) :: LS_IN, LS_OUT, LS_NTV
    integer                  , intent(IN   ) :: NTILES_IN, NTILES_OUT
    integer, optional        , intent(  OUT) :: RC

    integer                    :: STATUS
    character(len=ESMF_MAXSTR), parameter :: Iam='RegridTransformT2G2G'

    integer                         :: L, LM, K, KM
    integer                         :: I
    integer                         :: rank_in
    integer                         :: rank_out
    integer                         :: itemcount, itemcount_in, itemcount_out
    integer                         :: sizett
    real, pointer                   :: tile1d(:) => null()
    real, pointer                   :: tt(:)
    real, pointer                   :: tt_in(:)
    real, pointer                   :: G2d_in(:,:)
    real, pointer                   :: ptr1d_in(:)
    real, pointer                   :: ptr2d_in(:,:)
    real, pointer                   :: ptr3d_in(:,:,:)
    real*8, pointer                 :: p1dr8_in(:)
    real*8, pointer                 :: p2dr8_in(:,:)
    real*8, pointer                 :: p3dr8_in(:,:,:)
    real, pointer                   :: ptr2d_out(:,:)
    real, pointer                   :: ptr3d_out(:,:,:)
    real, pointer                   :: ptr4d_out(:,:,:,:)
    real, pointer                   :: tile_in(:)
    real, pointer                   :: tile_out(:)
    real, pointer                   :: out2d(:,:)
    type(ESMF_Array)                :: array_in
    type(ESMF_Array)                :: array_out
    type(ESMF_Field)                :: field
    type(ESMF_Grid)                 :: grid
    type(ESMF_TypeKind_Flag)        :: tk
    integer                         :: counts(3)
    type (ESMF_StateItem_Flag), pointer  :: ITEMTYPES_IN(:), ITEMTYPES_OUT(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES_IN(:), ITEMNAMES_OUT(:)

    allocate(tt_in (ntiles_in ), stat=status)
    VERIFY_(STATUS)
    allocate(tile_out(ntiles_out), stat=status)
    VERIFY_(STATUS)


    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, RC=STATUS)
    VERIFY_(STATUS)

    ASSERT_(ITEMCOUNT_IN == ITEMCOUNT_OUT)

    ITEMCOUNT = ITEMCOUNT_IN
    ASSERT_(ITEMCOUNT>0)

    allocate(ITEMNAMES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       ITEMTYPELIST=ITEMTYPES_IN, RC=STATUS)
    VERIFY_(STATUS)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       ITEMTYPELIST=ITEMTYPES_OUT, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_LocStreamGet(LS_NTV, ATTACHEDGRID=GRID, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GridGet(grid, localCellCountPerDim=COUNTS, RC=STATUS)
    VERIFY_(STATUS)
    allocate(G2d_in(COUNTS(1),COUNTS(2)), stat=status)
    VERIFY_(STATUS)

    call MAPL_LocStreamGet(LS_ntv, NT_LOCAL = sizett, rc=status)
    VERIFY_(STATUS)
    allocate(tt(sizett), stat=status)
    VERIFY_(STATUS)

    DO I=1, ITEMCOUNT
       ASSERT_(ITEMTYPES_IN (I) == ESMF_StateItem_Field)
       ASSERT_(ITEMTYPES_OUT(I) == ESMF_StateItem_Field)

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_out, rc=status)
       VERIFY_(STATUS)

       call ESMF_ArrayGet(array_in , rank=rank_in , typekind=tk, rc=status)
       VERIFY_(STATUS)
       call ESMF_ArrayGet(array_out, rank=rank_out, rc=status)
       VERIFY_(STATUS)

       ASSERT_(rank_in+1 == rank_out)
       ASSERT_(rank_in >=1 .and. rank_in <= 3)

       KM = 1
       if (rank_in == 1) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr1d_in , rc=status)
             VERIFY_(STATUS)
             tile_in => ptr1d_in
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p1dr8_in , rc=status)
             VERIFY_(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p1dr8_in)), stat=status)
                VERIFY_(STATUS)
             end if
             tile1d = p1dr8_in
             tile_in => tile1d
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, rc=status)
          VERIFY_(STATUS)
          out2d   => ptr2d_out
          LM = 1
       else if (rank_in == 2) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , rc=status)
             VERIFY_(STATUS)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p2dr8_in , rc=status)
             VERIFY_(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p2dr8_in,1)), stat=status)
                VERIFY_(STATUS)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, rc=status)
          VERIFY_(STATUS)
          LM = size(ptr3d_out,3)
       else if (rank_in == 3) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , rc=status)
             VERIFY_(STATUS)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p3dr8_in , rc=status)
             VERIFY_(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p3dr8_in,1)), stat=status)
                VERIFY_(STATUS)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr4d_out, rc=status)
          VERIFY_(STATUS)
          LM = size(ptr4d_out,3)
          KM = size(ptr4d_out,4)
       else
          RETURN_(ESMF_FAILURE)
       end if

       DO K=1,KM
          DO L=1,LM
             if (rank_out == 3) then
                if (tk == ESMF_TypeKind_R4) then
                   tile_in  => ptr2d_in (:,L)
                else if (tk == ESMF_TypeKind_R8) then
                   tile1d = p2dr8_in(:,L)
                   tile_in => tile1d
                end if
                out2d    => ptr3d_out(:,:,L)
             else if (rank_out == 4) then
                if (tk == ESMF_TypeKind_R4) then
                   tile_in  => ptr3d_in (:,L,K)
                else if (tk == ESMF_TypeKind_R8) then
                   tile1d = p3dr8_in(:,L,K)
                   tile_in => tile1d
                end if
                out2d    => ptr4d_out(:,:,L,K)
             end if

             ! T2T
             call MAPL_LocStreamTransform( tt, XFORMntv, tile_in, RC=STATUS ) 
             VERIFY_(STATUS)
             ! T2G
             call MAPL_LocStreamTransform(LS_NTV, G2d_IN, tt, RC=STATUS)
             VERIFY_(STATUS)

             ! G2T
             call MAPL_LocStreamTransform(LS_IN, TT_IN, G2d_IN, RC=STATUS)
             VERIFY_(STATUS)
             ! T2T
             call MAPL_LocStreamTransform( tile_out, XFORM, tt_in, RC=STATUS ) 
             VERIFY_(STATUS)
             ! T2G
             call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, RC=STATUS)
             VERIFY_(STATUS)

          ENDDO
       END DO

    ENDDO

    deallocate(G2d_in)
    deallocate(itemtypes_out)
    deallocate(itemnames_out)
    deallocate(itemtypes_in)
    deallocate(itemnames_in)
    deallocate(tile_out)
    deallocate(tt_in )
    deallocate(tt )
    if (associated(tile1d)) deallocate(tile1d)

    RETURN_(ESMF_SUCCESS)
  end subroutine RegridTransformT2G2G

  subroutine RegridTransformT2G(STATE_IN, XFORM, STATE_OUT, LS_OUT, NTILES_OUT, RC)
    type (ESMF_State)        , intent(IN   ) :: STATE_IN
    type (ESMF_State)        , intent(INOUT) :: STATE_OUT
    type(MAPL_LocStreamXform), optional, intent(IN   ) :: XFORM
    type(MAPL_LocStream)     , intent(IN   ) :: LS_OUT
    integer                  , intent(IN   ) :: NTILES_OUT
    integer, optional        , intent(  OUT) :: RC

    integer                    :: STATUS
    character(len=ESMF_MAXSTR), parameter :: Iam = "RegridTransformT2G"

    integer                         :: I, L, K, LM, KM
    integer                         :: rank_in
    integer                         :: rank_out
    integer                         :: itemcount, itemcount_in, itemcount_out
    real, pointer                   :: tile_in(:), tile_out(:)
    real, pointer                   :: ptr1d_in(:)
    real, pointer                   :: ptr2d_in(:,:)
    real, pointer                   :: ptr3d_in(:,:,:)
    real*8, pointer                 :: p1dr8_in(:)
    real*8, pointer                 :: p2dr8_in(:,:)
    real*8, pointer                 :: p3dr8_in(:,:,:)
    real, pointer                   :: ptr2d_out(:,:)
    real, pointer                   :: ptr3d_out(:,:,:)
    real, pointer                   :: ptr4d_out(:,:,:,:)
    real, pointer                   :: out2d(:,:)
    real, pointer                   :: out3d(:,:,:)
    real, pointer                   :: tile1d(:) => null()
    type(ESMF_Array)                :: array_in
    type(ESMF_Array)                :: array_out
    type(ESMF_Field)                :: field
    type (ESMF_TypeKind_Flag)       :: tk
    type (ESMF_StateItem_Flag),  pointer :: ITEMTYPES_IN(:), ITEMTYPES_OUT(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES_IN(:), ITEMNAMES_OUT(:)

    if (present(XFORM)) then
       allocate(tile_out(ntiles_out), stat=status)
       VERIFY_(STATUS)
    end if

    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, RC=STATUS)
    VERIFY_(STATUS)

    ASSERT_(ITEMCOUNT_IN == ITEMCOUNT_OUT)

    ITEMCOUNT = ITEMCOUNT_IN
    ASSERT_(ITEMCOUNT>0)

    allocate(ITEMNAMES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       ITEMTYPELIST=ITEMTYPES_IN, RC=STATUS)
    VERIFY_(STATUS)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       ITEMTYPELIST=ITEMTYPES_OUT, RC=STATUS)
    VERIFY_(STATUS)

    DO I=1, ITEMCOUNT
       ASSERT_(ITEMTYPES_IN (I) == ESMF_StateItem_Field)
       ASSERT_(ITEMTYPES_OUT(I) == ESMF_StateItem_Field)

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_out, rc=status)
       VERIFY_(STATUS)

       call ESMF_ArrayGet(array_in , rank=rank_in , typekind=tk, rc=status)
       VERIFY_(STATUS)
       call ESMF_ArrayGet(array_out, rank=rank_out, rc=status)
       VERIFY_(STATUS)
       ASSERT_(rank_out == rank_in + 1)

       KM = 1
       if (rank_in == 1) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr1d_in , rc=status)
             VERIFY_(STATUS)
             tile_in => ptr1d_in
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p1dr8_in , rc=status)
             VERIFY_(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p1dr8_in)), stat=status)
                VERIFY_(STATUS)
             end if
             tile1d = p1dr8_in
             tile_in => tile1d
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, rc=status)
          VERIFY_(STATUS)
          out2d   => ptr2d_out
          LM = 1
       else if (rank_in == 2) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , rc=status)
             VERIFY_(STATUS)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p2dr8_in , rc=status)
             VERIFY_(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p2dr8_in,1)), stat=status)
                VERIFY_(STATUS)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, rc=status)
          VERIFY_(STATUS)
          LM = size(ptr3d_out,3)
       else if (rank_in == 3) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , rc=status)
             VERIFY_(STATUS)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p3dr8_in , rc=status)
             VERIFY_(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p3dr8_in,1)), stat=status)
                VERIFY_(STATUS)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr4d_out, rc=status)
          VERIFY_(STATUS)
          LM = size(ptr4d_out,3)
          KM = size(ptr4d_out,4)
       else
          RETURN_(ESMF_FAILURE)
       end if

       DO K=1,KM
          DO L=1,LM
             if (rank_out == 3) then
                if (tk == ESMF_TypeKind_R4) then
                   tile_in  => ptr2d_in (:,L)
                else if (tk == ESMF_TypeKind_R8) then
                   tile1d = p2dr8_in(:,L)
                   tile_in => tile1d
                end if
                out2d    => ptr3d_out(:,:,L)
             else if (rank_out == 4) then
                if (tk == ESMF_TypeKind_R4) then
                   tile_in  => ptr3d_in (:,L,K)
                else if (tk == ESMF_TypeKind_R8) then
                   tile1d = p3dr8_in(:,L,K)
                   tile_in => tile1d
                end if
                out2d    => ptr4d_out(:,:,L,K)
             end if

             if (present(XFORM)) then
                call MAPL_LocStreamTransform( tile_out, XFORM, tile_in, RC=STATUS ) 
                VERIFY_(STATUS)
             else
                tile_out => tile_in
             endif

             call MAPL_LocStreamTransform(LS_OUT, OUT2d, TILE_OUT, RC=STATUS)
             VERIFY_(STATUS)

          END DO
       END DO

    ENDDO

    deallocate(itemtypes_out)
    deallocate(itemnames_out)
    deallocate(itemtypes_in)
    deallocate(itemnames_in)
    if (present(XFORM)) then
       deallocate(tile_out)
    end if
    if (associated(tile1d)) deallocate(tile1d)

    RETURN_(ESMF_SUCCESS)
  end subroutine RegridTransformT2G

  subroutine SetPsizeStrategy(Slices, Writing, NumColls, NumNodes, Psize,Root)
    integer, intent(IN ) :: Slices(NumColls), NumColls, NumNodes
    logical, intent(IN ) :: Writing(NumColls)
    integer, intent(OUT) :: Psize(NumColls), Root(NumColls)

    integer :: MaxSlicesPerNode, n
    integer :: CurrNode, Len, SlicesInNode

    integer, dimension(NumColls) :: SortedSlices, CollNo

!!!  Returns Psize and Root, the size (in nodes) and root node 
!!!  of each node partition assigned to active collections.

!!!  NumNodes is the number of nodes being dealt out to the partitions
!!!  NumColls is the total number of collection, some of which may be inactive
!!!  Writing  identifies active collection
!!!  Slices   is the number of 2-D sections or slices in each collection
!!!           The number in inactive collections is ignored.

!!! Begin
!!!------

!!! Make sure all outputs are initialized.
!!! Needed only for inactive collections.
!!!---------------------------------------

    Psize = 0
    Root  = 1

!!! Sort the collection sizes (# of slices) in ascending order.
!!!  Also sort the collection index the same way, to fill the 
!!!  correct ones later.
!!!------------------------------------------------------------
    where(writing)
       SortedSlices = Slices
    elsewhere
       SortedSlices = 0
    endwhere

    do n=1,NumColls
       CollNo(n) = n
    end do

    call MAPL_Sort(SortedSlices, CollNo)

!!! This is the maximum number of slices in a node if all slices
!!!  were uniformly distributed without honoring collection and
!!!  node boundaries. Since every collection boundary must also be
!!!  a node boundary, this is a lower bound on MaxSlicesPerNode
!!! and is used as our initial guess.

    MaxSlicesPerNode = (sum(Slices,mask=Writing)-1)/NumNodes + 1

!!! We try to distribute the slices in active collections as uniformly
!!!  as possible. "Small" collections (<= MaxSlicesPerNode) are
!!!  assigned to a single node, others span multiple nodes. 
!!!  Small collections are grouped in a node without 
!!!  exceeding MaxSlicesPerNode. Multi-node collections are
!!!  not grouped in nodes. Since MaxSlicesPerNode  is generally
!!!  too small to fit all the collections, it is then increased,until
!!!  all the active collections fit in the given nodes.
!!!--------------------------------------------------------------------

    do
       CurrNode     = 1
       SlicesInNode = 0

       COLLECTIONS: do n=1,NumColls
          ACTIVE: if(Writing(CollNo(n))) then

             if(SortedSlices(n)<MaxSlicesPerNode) then ! A single-node collection
                SlicesInNode = SlicesInNode + SortedSlices(n)

                if(SlicesInNode > MaxSlicesPerNode) then ! Current Coll oveerfills node
                   CurrNode     = CurrNode + 1
                   SlicesInNode = SortedSlices(n)
                end if

                Psize(CollNo(n)) = 1
                Root (CollNo(n)) = (CurrNode-1) + 1

             else                                      ! A multi-node collection
                Len = (SortedSlices(n)-1)/MaxSlicesPerNode + 1

                Psize(CollNo(n)) = Len
                Root (CollNo(n)) = (CurrNode-1) + 1

                CurrNode = CurrNode + len
             endif

          endif ACTIVE
       end do COLLECTIONS

       if(CurrNode<=NumNodes) exit

       MaxSlicesPerNode = MaxSlicesPerNode + 1
    enddo

    return
  end subroutine SetPsizeStrategy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Get_Tdim (list, clock, tdim)

! !IROUTINE: Get_Tdim -- Returns Time Dimension (Number of Records) in a HISTORY.rc collection file

! !USES:
    use ESMF
    use MAPL_CommsMod, only: MAPL_AM_I_ROOT

    implicit none

! !ARGUMENTS:

    type (history_list),  intent(IN ) :: list
    type (ESMF_Clock),    intent(IN ) :: clock
    integer,              intent(OUT) :: tdim

! ESMF stuff
!-----------
    type (ESMF_Time)            :: currTime
    type (ESMF_Time)            :: stopTime
    type (ESMF_TimeInterval)    :: tint

! Misc locals
!------------
    real                         :: rfreq
    real                         :: rdelt
    real                         :: rfrac
    integer                      :: nfreq
    integer                      :: ndelt
    integer                      :: STATUS

! NSECF Function
!---------------
    integer  nhms, nsecf
    nsecf(nhms) =  nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)

!  Initialize TDIM=-1 (UNLIMITED)
!--------------------------------
    tdim = -1

    if( list%tm == 0) then  ! Dynamic calculation of time dimension

       if( list%duration == 0 ) then
          ! compute duration from the ESMF clock
          call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, &
               RC=status)
          if (status /= ESMF_SUCCESS) goto 200
          tint = stopTime - currTime
          call ESMF_TimeIntervalGet(tint, s=ndelt, RC=status)
          if (status /= ESMF_SUCCESS) goto 200
         
          nfreq = nsecf( list%frequency )
          rfreq = real(nfreq)
          rdelt = real(ndelt)
          rfrac = rdelt/rfreq - ndelt/nfreq
          if( rfrac.ne.0 ) rfrac = 1.0 - rfrac
          ndelt = ndelt  + rfrac*nfreq

       else
          ndelt = nsecf( list%duration )
       endif
       
       nfreq = nsecf( list%frequency )
       if (nfreq /=0) then
          tdim  = ndelt/nfreq
       end if

    else
       tdim = list%tm
    endif  ! End TM=0 Test
    
! Debug Prints
! ------------
200 continue
    if( MAPL_AM_I_ROOT() ) then
       write(6,100) list%frequency, list%duration, tdim, trim(list%collection)
100    format(1x,'Freq: ',i6.6,'  Dur: ',i6.6,'  TM: ',i4,'  Collection: ',a)
    endif

    return
  end subroutine Get_Tdim

  subroutine MAPL_SetExpression(nfield,fields,tmpfields,rewrite,tmprank,tmploc,nPExtraFields, &
           ExtraFields,ExtraGridComp,ExpState,rc)

  integer,intent(in)::nfield
  character*(*),     intent(inout) :: fields(:,:)
  character*(*),     intent(inout) :: tmpfields(:)
  logical,           intent(inout) :: rewrite(:)
  integer,           intent(inout) :: tmprank(:)
  integer,           intent(inout) :: tmploc(:)
  integer,           intent(inout) :: nPExtraFields
  character*(*), pointer, intent(inout) :: ExtraFields(:)
  character*(*), pointer, intent(inout) :: ExtraGridComp(:)
  type(ESMF_State),  intent(inout) :: ExpState
  integer, optional, intent(out  ) :: rc

! Local variables:

  integer:: i,j,m,k,status,largest_rank,iRepField,ivLoc
  logical :: ifound_vloc
  character(len=ESMF_MAXSTR) :: Iam='MAPL_SetExpression'
  character(len=ESMF_MAXSTR) :: replace_field, replace_comp, tmpList
  character(len=ESMF_MAXSTR) :: VarName
  integer                    :: idx
  character(len=ESMF_MAXSTR), allocatable :: VarNames(:)
  logical,                    allocatable :: VarNeeded(:)
  integer                                 :: iRealFields
  character(len=256)                      :: ExtVars
  integer                                 :: nExtraFields,nUniqueExtraFields
  character(len=ESMF_MAXSTR), allocatable :: NonUniqueVarNames(:,:)

  character(len=ESMF_MAXSTR), allocatable :: TotVarNames(:)
  character(len=ESMF_MAXSTR), allocatable :: TotCmpNames(:)
  character(len=ESMF_MAXSTR), allocatable :: TotAliasNames(:)
  integer,                    allocatable :: totRank(:)
  integer,                    allocatable :: totLoc(:)
  integer                                 :: totFields
  type(ESMF_State), pointer               :: exptmp (:)
  type(ESMF_State)                        :: state
  type(ESMF_Field)                        :: field
  integer                                 :: dims
  real, pointer                           :: ptr3d(:,:,:)

! Set rewrite flag and tmpfields.
! To keep consistency, all the arithmetic parsing output fields must
! only be combinations of the alias output field variables (i.e., fields(3,:))
! rather than the actual output field variables (i.e., fields(1,:)).
! Also do check that there are no illegal operations
!-------------------------------------------------------------------
  ! check which fields are actual exports or expressions
  nPExtraFields = 0
  iRealFields = 0
  do m=1,nfield
    if (scan(trim(fields(1,m)),'()^*/+-.')/=0) then
       rewrite(m)= .TRUE.
       tmpfields(m)= trim(fields(1,m))
    else
       iRealFields = iRealFields + 1
       rewrite(m)= .FALSE.
       tmpfields(m)= trim(fields(1,m))
    endif
  enddo

  ! now that we know this allocated a place to store the names of the real fields
  allocate(VarNames(iRealFields),stat=status)
  VERIFY_(STATUS)
  allocate(VarNeeded(iRealFields),stat=status)
  VERIFY_(STATUS)
  k=0
  do m=1,nfield
     if (.not. rewrite(m)) then
        k=k+1
        VarNames(k)=fields(3,m)
     endif
  enddo

  ! now we can have extra fields that are not in collection if they are in the component
  ! we specify with the expression we get the number of these

  nExtraFields=0
  do m=1,nfield
     if (rewrite(m)) then

         ExtVars = ""
         call CheckSyntax(tmpfields(m),VarNames,VarNeeded,ExtVar=ExtVars,rc=status)
         VERIFY_(STATUS)

         tmpList=ExtVars
         do i=1,len_trim(tmpList)
            idx=index(tmpList,',')
            if (idx /= 0) then
               varName = tmpList(1:idx-1)
               nExtraFields=nExtraFields+1
               tmpList = tmpList(idx+1:)
            else
               exit
            end if
         end do

      end if
   end do

  allocate(NonUniqueVarNames(nExtraFields,2)) 

  ! get the number of extra fields, after this we will have to check for duplicates
  nExtraFields=0
  do m=1,nfield
     if (rewrite(m)) then

         ExtVars = ""
         call CheckSyntax(tmpfields(m),VarNames,VarNeeded,ExtVar=ExtVars,rc=status)
         VERIFY_(STATUS)

         tmpList=ExtVars
         do i=1,len_trim(tmpList)
            idx=index(tmpList,',')
            if (idx /= 0) then
               varName = tmpList(1:idx-1)
               nExtraFields=nExtraFields+1
               NonUniqueVarNames(nExtraFields,1) = trim(VarName)
               NonUniqueVarNames(nExtraFields,2) = fields(2,m)
               tmpList = tmpList(idx+1:)
            else
               exit
            end if
         end do

      end if
   end do


   deallocate(VarNames)
   deallocate(VarNeeded)

   ! blank out any duplicates
   do i=1,nExtraFields
      VarName = NonUniqueVarNames(i,1)
      do j=i+1,nExtraFields
         if (trim(VarName) == trim(NonUniqueVarNames(j,1))) then
            NonUniqueVarNames(j,1)="DUPLICATE"
         end if
      end do
   end do

   nUniqueExtraFields = 0
   do i=1,nExtraFields
      if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") nUniqueExtraFields = nUniqueExtraFields + 1
   end do

  totFields = iRealFields + nUniqueExtraFields
  allocate(TotVarNames(totFields),stat=status)
  VERIFY_(STATUS)
  allocate(TotCmpNames(totFields),stat=status)
  VERIFY_(STATUS)
  allocate(TotAliasNames(totFields),stat=status)
  VERIFY_(STATUS)
  allocate(TotRank(totFields),stat=status)
  VERIFY_(STATUS)
  allocate(TotLoc(totFields),stat=status)
  VERIFY_(STATUS)

  iRealFields = 0
  do i=1,nfield
    if (.not.rewrite(i)) then
       iRealFields = iRealFields + 1
       TotVarNames(iRealFields) = trim(fields(1,i))
       TotCmpNames(iRealFields) = trim(fields(2,i))
       TotAliasNames(iRealFields) = trim(fields(3,i))
       TotRank(iRealFields) = tmpRank(i)
       TotLoc(iRealFields) = tmpLoc(i) 
    endif
  enddo
  allocate ( exptmp (1), stat=status )
  VERIFY_(STATUS)
  exptmp(1) = ExpState
  nUniqueExtraFields = 0
  do i=1, nExtraFields
     if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") then
        nUniqueExtraFields = nUniqueExtraFields + 1
        TotVarNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,1)
        TotCmpNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,2)
        TotAliasNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,1)
        call MAPL_ExportStateGet ( exptmp,NonUniqueVarNames(i,2),state,rc=status )
        VERIFY_(STATUS)
        call MAPL_StateGet(state, NonUniqueVarNames(i,1),field,rc=status)
        VERIFY_(STATUS)

!       if this is user created bundle, might not have set these attributes
!       will instead get actual rank and vertical dimension
        call ESMF_FieldGet(field,rank=dims,rc=status)
        !call ESMF_AttributeGet(field,name='DIMS',value=dims,rc=status)
        VERIFY_(STATUS)
        TotRank(iRealFields+nUniqueExtraFields) = dims
        if (dims > 2) then
           call ESMF_FieldGet(field,localDE=0,farrayPtr=ptr3d,rc=status)
           VERIFY_(STATUS)
           TotLoc(iRealFields+nUniqueExtraFields) = size(ptr3d,3)
        else
           TotLoc(iRealFields+nUniqueExtraFields) = 0
        end if
        !call ESMF_AttributeGet(field,name='VLOCATION',value=dims,rc=status)
        !VERIFY_(STATUS)
        !TotLoc(iRealFields+nUniqueExtraFields) = dims
     end if
  end do 

  allocate(extraFields(nUniqueExtraFields),stat=status)
  VERIFY_(STATUS)
  allocate(extraGridComp(nUniqueExtraFields),stat=status)
  VERIFY_(STATUS)
  nPExtraFields = nUniqueExtraFields
  nUniqueExtraFields = 0
  do i=1,nExtraFields
     if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") then
        nUniqueExtraFields = nUniqueExtraFields + 1
        extraFields(nUniqueExtraFields) = NonUniqueVarNames(i,1)
        extraGridComp(nUniqueExtraFields) = NonUniqueVarNames(i,2)
     end if
  end do

  deallocate(NonUniqueVarNames)
  deallocate(exptmp) 
! Change the arithmetic parsing field containing mutiple variables
! to the dummy default field containing a single field variable.
! Since MAPL_HistoryGridCompMod does not understand arithmetic parsing field variable,
! we need to change the arithmetic parsing field variable to the dummy field to allocate memory.
! But the actual arithmetic parsing field already has been copied to the temporialy field.
! Also we will do some syntax checking here since this is a good place
!----------------------------------------------------------------------
 allocate(VarNeeded(TotFields),stat=status)
 VERIFY_(STATUS)

 do m=1,nfield
     if (Rewrite(m)) then
         largest_rank =0
         ifound_vloc=.false.
         call CheckSyntax(tmpfields(m),TotAliasNames,VarNeeded,rc=status)
         VERIFY_(STATUS)
         do i=1,TotFields
            if (VarNeeded(i)) then
               if (TotRank(i)> largest_rank) then
                  largest_rank=TotRank(i)
                  iRepField=i
               end if

               if (ifound_vloc) then
                  if (ivLoc /= Totloc(i) .and. totloc(i) /= MAPL_VLocationNone) then
                     if (mapl_am_I_root()) write(*,*)'arithmetic expression has two different vlocations'
                     ASSERT_(.false.)
                  end if
               else
                  if (totloc(i) /= 0) then
                     ivloc = totloc(i)
                     ifound_vloc = .true.
                  endif
               end if
            end if
         end do
         fields(1,m)= TotVarNames(iRepField)
         fields(2,m)= TotCmpNames(iRepField)

     endif
 enddo

 deallocate(VarNeeded)
 deallocate(TotVarNames)
 deallocate(TotCmpNames)
 deallocate(TotAliasNames)
 deallocate(TotRank)
 deallocate(TotLoc)

 RETURN_(ESMF_SUCCESS)

 end subroutine MAPL_SetExpression

  subroutine MAPL_RunExpression(state,fields,tmpfields,rewrite,nfield,rc)

  type (ESMF_State),  intent(in)    :: state
  character*(*), intent(in):: fields(:,:),tmpfields(:)
  logical, intent(inout) :: rewrite(:)
  integer, intent(in):: nfield
  integer, optional, intent(out) :: rc

! Local variables:
  character(len=ESMF_MAXSTR)     :: Iam='MAPL_RunExpression'
  character(len=ESMF_MAXSTR)     :: fname,fexpr
  integer:: m,STATUS
  type(ESMF_Field) :: field

  do m=1,nfield
     if (rewrite(m)) then
        fname = trim(fields(3,m))
        call MAPL_StateGet(state,fname,field,rc=status)
        VERIFY_(STATUS)
        fexpr = tmpfields(m)
        call MAPL_StateEval(state,fexpr,field,rc=status)
        VERIFY_(STATUS)
     end if
  enddo

  RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_RunExpression

#if 0
  subroutine MAPL_StateDestroy(State, RC)
    type(ESMF_State), intent(inout) :: state
    integer, optional,intent(  out) :: rc

! Local variables:
    character(len=ESMF_MAXSTR) :: Iam='MAPL_StateDestroy'
    integer                    :: STATUS

    type(ESMF_Field)                      :: field
    type(ESMF_FieldBundle)                :: bundle
    type (ESMF_StateItem_Flag),  pointer  :: itemTypeList(:)
    character(len=ESMF_MAXSTR ), pointer  :: itemNameList(:)

    integer                               :: I, J, N, NF

    call ESMF_StateGet(state, ITEMCOUNT=N,  RC=STATUS)
    VERIFY_(STATUS)

    allocate(itemNameList(N), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(itemtypeList(N), STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(state,ITEMNAMELIST=itemNamelist,ITEMTYPELIST=itemtypeList,RC=STATUS)
    VERIFY_(STATUS)

    do I=1,N
       if(itemtypeList(I)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state,itemNameList(I),FIELD,RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_FieldDestroy(FIELD, rc=status)
          VERIFY_(STATUS)
       else if(itemtypeList(I)==ESMF_STATEITEM_FieldBundle) then
          call ESMF_StateGet(state,itemNameList(I), BUNDLE, RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_FieldBundleGet(BUNDLE,FieldCount=NF, RC=STATUS)
          VERIFY_(STATUS)
          DO J=1,NF
             call ESMF_FieldBundleGet(BUNDLE, J, FIELD, RC=STATUS)
             VERIFY_(STATUS)
             call ESMF_FieldDestroy(field, rc=status)
             VERIFY_(STATUS)
          END DO
          call ESMF_FieldBundleDestroy(BUNDLE, RC=STATUS)
          VERIFY_(STATUS)
       else if(itemtypeList(I)==ESMF_STATEITEM_State) then
!ALT we ingore nested states for now, they will get destroyed by their GC
       end if
    end do
    call ESMF_StateDestroy(STATE, RC=STATUS)
    VERIFY_(STATUS)

    deallocate(itemNameList, STAT=STATUS)
    VERIFY_(STATUS)
    deallocate(itemtypeList, STAT=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_StateDestroy
#endif

  subroutine MAPL_StateGet(state,name,field,rc)
    type(ESMF_State), intent(in) :: state
    character(len=*), intent(in) :: name
    type(ESMF_Field), intent(inout) :: field
    integer, optional, intent(out  ) :: rc

    character(len=ESMF_MAXSTR) :: Iam
    integer :: status
    character(len=ESMF_MAXSTR) :: bundlename, fieldname
    type(ESMF_FieldBundle) :: bundle

    integer :: i

    i = index(name,"%")
    if (i.ne.0) then
        bundlename = name(:i-1)
        fieldname = name(i+1:)
        call ESMF_StateGet(state,trim(bundlename),bundle,rc=status)
        VERIFY_(STATUS)
        call ESMF_FieldBundleGet(bundle,trim(fieldname),field=field,rc=status)
        VERIFY_(STATUS)
    else
       call ESMF_StateGet(state,trim(name),field,rc=status)
       VERIFY_(STATUS)
    end if

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_StateGet

end module MAPL_HistoryGridCompMod

