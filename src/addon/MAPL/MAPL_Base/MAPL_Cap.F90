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
!  $Id: MAPL_Cap.F90,v 1.37.12.4.14.6 2014-04-01 18:08:20 bmauer Exp $

#include "MAPL_Generic.h"

module MAPL_CapMod

!BOP

! !MODULE: MAPL_CapMod --- Implements the top entry point for MAPL components

! !USES:

  use ESMF
  use MAPL_BaseMod
  use MAPL_ConstantsMod
  use MAPL_ProfMod
  use MAPL_MemUtilsMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use MAPL_LocStreamMod
  use ESMFL_Mod
  use MAPL_ShmemMod
  use MAPL_HistoryGridCompMod, only : Hist_SetServices => SetServices
  use MAPL_HistoryGridCompMod, only : HISTORY_ExchangeListWrap
  use MAPL_ExtDataGridCompMod, only : ExtData_SetServices => SetServices
  use MAPL_CFIOServerMod

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public MAPL_Cap

! !DESCRIPTION: 

! \input{MAPL_CapIntro.tex}
 
!EOP

  interface MAPL_ConfigSetAttribute
   
! !PRIVATE MEMBER FUNCTIONS:
     module procedure MAPL_ConfigSetString
     module procedure MAPL_ConfigSetIntI4
  end interface

  include "mpif.h"

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOPI

! !IROUTINE: MAPL_Cap -- Implements generic Cap functionality

! !INTERFACE:

  subroutine MAPL_CAP(ROOT_SetServices, Name, AmIRoot, FinalFile, RC)

! !ARGUMENTS:

    external                             :: ROOT_SetServices
    character*(*), optional, intent(IN ) :: Name
    logical,       optional, intent(OUT) :: AmIRoot
    character*(*), optional, intent(IN ) :: FinalFile
    integer,       optional, intent(OUT) :: rc

!EOPI

! Handles to the CAP's Gridded Components GCs
! -------------------------------------------

   integer                      :: ROOT
   integer                      :: HIST
   integer                      :: EXTDATA
   character(len=ESMF_MAXSTR)   :: ROOT_NAME

! A MAPL object for the cap
!--------------------------

   type(MAPL_MetaComp)          :: MAPLOBJ

! The children's GCs and IM/Ex states
!------------------------------------

   type(ESMF_GridComp), pointer :: GCS(:)
   type(ESMF_State),    pointer :: IMPORTS(:)
   type(ESMF_State),    pointer :: EXPORTS(:)

! ESMF stuff
!-----------

   type(ESMF_VM)                :: VM
   type(ESMF_Config)            :: config
   type(ESMF_Config)            :: cf_root
   type(ESMF_Config)            :: cf_hist
   type(ESMF_Config)            :: cf_ext
   type(ESMF_Clock)             :: clock
   type(ESMF_Clock)             :: clock_HIST
   type(ESMF_Time)              :: CurrTime
   type(ESMF_Alarm)             :: PERPETUAL
   type(ESMF_TimeInterval)      :: Frequency

   
! ErrLog variables
!-----------------

   integer                      :: STATUS
   character(len=ESMF_MAXSTR)   :: Iam="MAPL_Cap"

! Misc locals
!------------

   character(len=ESMF_MAXSTR)   :: ROOT_CF
   character(len=ESMF_MAXSTR)   :: HIST_CF
   character(len=ESMF_MAXSTR)   :: EXTDATA_CF
   character(len=ESMF_MAXSTR)   :: enableTimers
   character(len=ESMF_MAXSTR)   :: enableMemUtils
   character(len=ESMF_MAXSTR)   :: clockname
   character(len=ESMF_MAXSTR)   :: EXPID
   character(len=ESMF_MAXSTR)   :: EXPDSC

   logical                      :: done
   logical                      :: LPERP
   logical                      :: AmIRoot_
   integer                      :: printSpec

   integer                      :: HEARTBEAT_DT
   integer                      :: RUN_DT
   integer                      :: PERPETUAL_YEAR
   integer                      :: PERPETUAL_MONTH
   integer                      :: PERPETUAL_DAY
   integer                      :: AGCM_YY, AGCM_MM, AGCM_DD, AGCM_H, AGCM_M, AGCM_S
   integer                      :: N,NSTEPS
   integer                      :: NPES,CoresPerNode


   integer*8, pointer           :: LSADDR(:) => null()
   type(HISTORY_ExchangeListWrap) :: lswrap

   integer                               :: i, itemcount, comm
   type (ESMF_StateItem_Flag), pointer   :: ITEMTYPES(:)
   character(len=ESMF_MAXSTR ), pointer  :: ITEMNAMES(:)
   type (ESMF_Field)                     :: field
   type (ESMF_FieldBundle)               :: bundle
   integer                               :: useShmem
   integer                               :: esmfcommsize,MaxMem,nnodes
   integer                               :: myRank, ioColor, esmfColor, key, esmfComm, ioComm,intercomm
   type(MAPL_Communicators)              :: mapl_Comm
   integer                               :: ioMyRank,IOnPes,IOcounter
   logical                               :: lexist
   namelist / ioserver / nnodes, CoresPerNode, maxMem
   character(len=ESMF_MAXSTR )           :: DYCORE
   integer                               :: snglcol


! Begin
!------

!  Initialize ESMF
!-----------------

   call mpi_init(status)
   VERIFY_(STATUS) 
   call mpi_comm_size(MPI_COMM_WORLD,nPes,status)
   VERIFY_(STATUS)
   call mpi_comm_rank(MPI_COMM_WORLD,myRank,status)
   VERIFY_(STATUS)
   mapl_comm%myGlobalRank = myRank

   if (myRank == 0) then
      inquire(file="ioserver.nml",exist=lexist)
      if (lexist) then
         open(99,file="ioserver.nml",status='old')
         read(unit=99,NML=ioserver)
         close(99)
         write(*,'(A,I5,A,I5,A,I6)')'Running ioserver on ',(nPes/CoresPerNode)-nnodes,' nodes with ',CoresPerNode,' CoresPerNode and maxMem ',MaxMem
         write(*,'(A,I5,A)')'Runing model on ',nnodes,' nodes'
         esmfcommsize = nnodes*CoresPerNode
      else
         esmfcommsize = nPes
         coresPerNode = 0
         maxMem = 0
      end if
   end if
   call MPI_BCAST(esmfcommsize, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, status)
   VERIFY_(STATUS)
   call MPI_BCAST(corespernode, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, status)
   VERIFY_(STATUS)
   call MPI_BCAST(maxMem, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, status)
   VERIFY_(STATUS)
   mapl_comm%maxMem = maxMem
   mapl_comm%corespernode = corespernode
   ioColor = MPI_UNDEFINED
   esmfColor = MPI_UNDEFINED
   if (myRank < esmfcommsize) then
      esmfColor = 0
   else
      ioColor = 0
   end if
   mapl_comm%ioCommSize = nPes-esmfcommsize
 
   call mpi_comm_split(MPI_COMM_WORLD,esmfColor,myRank,esmfComm,status)
   VERIFY_(STATUS)
   call mpi_comm_split(MPI_COMM_WORLD,ioColor,myRank,ioComm,status)
   VERIFY_(STATUS)

   mapl_comm%esmfCommSize = esmfcommsize
   mapl_comm%MaplCommSize = nPes

   mapl_Comm%maplComm = MPI_COMM_WORLD
   mapl_comm%esmfComm = esmfComm
   mapl_comm%ioComm = ioComm
   mapl_comm%ioCommRoot=esmfcommsize 
 
   call MAPL_CFIOServerInitMpiTypes()
  
   ESMFCOMMIF: if (esmfComm /= MPI_COMM_NULL) then
#if defined(ENABLE_ESMF_ERR_LOGGING)
      call ESMF_Initialize (vm=vm, mpiCommunicator=esmfComm, rc=status)
#else
      call ESMF_Initialize (vm=vm, logKindFlag=ESMF_LOGKIND_NONE, mpiCommunicator=esmfComm, rc=status)
#endif
      VERIFY_(STATUS)
      call ESMF_VMGet      (VM, petcount=NPES, mpiCommunicator=comm, rc=status)
      VERIFY_(STATUS)


      call MAPL_GetNodeInfo (comm=comm, rc=status)
      VERIFY_(STATUS)

      AmIRoot_ = MAPL_Am_I_Root(vm)

      if (present(AmIRoot)) then
         AmIRoot = AmIRoot_
      end if

   !  Open the CAP's configuration from CAP.rc
   !------------------------------------------

      config = ESMF_ConfigCreate (                   rc=STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigLoadFile   ( config, 'CAP.rc', rc=STATUS )
      VERIFY_(STATUS)

   !  CAP's MAPL MetaComp
   !---------------------

      call MAPL_Set(MAPLOBJ, maplComm=mapl_Comm, rc=status)
      VERIFY_(STATUS)

      if(present(Name)) then
         call MAPL_Set (MAPLOBJ, name= Name, cf=CONFIG,    rc=STATUS )
         VERIFY_(STATUS)
      else
         call MAPL_Set (MAPLOBJ, name='CAP', cf=CONFIG,    rc=STATUS )
         VERIFY_(STATUS)
      end if

   ! Check if user wants to use node shared memory (default is no)
   !--------------------------------------------------------------
      call MAPL_GetResource( MAPLOBJ, useShmem,  label='USE_SHMEM:',  default = 0, rc=STATUS )
      if (useShmem /= 0) then
         call MAPL_InitializeShmem (rc=status)
         VERIFY_(STATUS)
      end if

   !  Create Clock. This is a private routine that sets the start and 
   !   end times and the time interval of the clock from the configuration.
   !   The start time is temporarily set to 1 interval before the time in the
   !   configuration. Once the Alarms are set in intialize, the clock will
   !   be advanced to guarantee it and its alarms are in the same state as they
   !   were after the last advance before the previous Finalize.
   !---------------------------------------------------------------------------

      call MAPL_ClockInit ( MAPLOBJ, clock, NSTEPS,          rc=STATUS )
      VERIFY_(STATUS)

      clock_HIST = ESMF_ClockCreate ( clock, rc=STATUS )  ! Create copy for HISTORY
      VERIFY_(STATUS)

      CoresPerNode = MAPL_CoresPerNodeGet(comm,rc=status)
      VERIFY_(STATUS)

      ! We check resource for CoresPerNode (no longer needed to be in CAR.rc)
      ! If it is set in the resource, we issue an warning if the
      ! value does not agree with the detected CoresPerNode

      call ESMF_ConfigGetAttribute(config, value=N, Label="CoresPerNode:", rc=status)
      if(STATUS==ESMF_SUCCESS) then
         if (CoresPerNode /= N) then
            call WRITE_PARALLEL("WARNING: CoresPerNode set, but does NOT match detected value")
         end if
      end if

      ASSERT_(CoresPerNode<=NPES)

      call ESMF_ConfigGetAttribute(config, value=HEARTBEAT_DT, Label="HEARTBEAT_DT:", rc=status)
      VERIFY_(STATUS)
      call ESMF_TimeIntervalSet( Frequency, S=HEARTBEAT_DT, rc=status )
      VERIFY_(STATUS)


      PERPETUAL = ESMF_AlarmCreate( clock=clock_HIST, name='PERPETUAL', ringinterval=Frequency, rc=status )
      VERIFY_(STATUS)
      call ESMF_AlarmRingerOff( PERPETUAL, rc=status )
      VERIFY_(STATUS)

   ! Set CLOCK for AGCM
   ! ------------------

      call MAPL_GetResource( MAPLOBJ, PERPETUAL_YEAR,  label='PERPETUAL_YEAR:',  default = -999, rc=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetResource( MAPLOBJ, PERPETUAL_MONTH, label='PERPETUAL_MONTH:', default = -999, rc=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetResource( MAPLOBJ, PERPETUAL_DAY,   label='PERPETUAL_DAY:',   default = -999, rc=STATUS )
      VERIFY_(STATUS)

      LPERP = ( ( PERPETUAL_DAY   /= -999 ) .or. &
                ( PERPETUAL_MONTH /= -999 ) .or. &
                ( PERPETUAL_YEAR  /= -999 ) )

      if(         PERPETUAL_DAY   /= -999 ) then
         ASSERT_( PERPETUAL_MONTH /= -999 )
         ASSERT_( PERPETUAL_YEAR  /= -999 )
      endif

      if( LPERP ) then
          if (AmIRoot_) then
              if( PERPETUAL_YEAR  /= -999 ) print *, 'Using Perpetual  Year: ',PERPETUAL_YEAR
              if( PERPETUAL_MONTH /= -999 ) print *, 'Using Perpetual Month: ',PERPETUAL_MONTH
              if( PERPETUAL_DAY   /= -999 ) print *, 'Using Perpetual   Day: ',PERPETUAL_DAY
          endif

          call ESMF_ClockGet ( clock, name=clockname, rc=status )
          clockname = trim( clockname ) // '_PERPETUAL'
          call ESMF_Clockset ( clock, name=clockname, rc=status )

          call ESMF_ClockGet ( clock_HIST, name=clockname, rc=status )
          clockname = trim( clockname ) // '_PERPETUAL'
          call ESMF_Clockset ( clock_HIST, name=clockname, rc=status )

          call Perpetual_Clock ( clock, clock_HIST, PERPETUAL_YEAR, PERPETUAL_MONTH, PERPETUAL_DAY, STATUS )
          VERIFY_(STATUS)
      endif

   !  Get configurable info to create HIST 
   !  and the ROOT of the computational hierarchy
   !---------------------------------------------

   !BOR

   ! !RESOURCE_ITEM: string :: Name of ROOT's config file
      call MAPL_GetResource(MAPLOBJ, ROOT_CF,      "ROOT_CF:", default="ROOT.rc",       RC=STATUS ) 
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Name to assign to the ROOT component
      call MAPL_GetResource(MAPLOBJ, ROOT_NAME,    "ROOT_NAME:", default="ROOT",           RC=STATUS ) 
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Name of HISTORY's config file 
      call MAPL_GetResource(MAPLOBJ, HIST_CF,      "HIST_CF:", default="HIST.rc",        RC=STATUS ) 
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Name of ExtData's config file
      call MAPL_GetResource(MAPLOBJ, EXTDATA_CF,   "EXTDATA_CF:", default='ExtData.rc',     RC=STATUS ) 
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Control Timers 
      call MAPL_GetResource(MAPLOBJ, enableTimers, "MAPL_ENABLE_TIMERS:", default='NO',             RC=STATUS )
      VERIFY_(STATUS)

   ! !RESOURCE_ITEM: string :: Control Memory Diagnostic Utility 
      call MAPL_GetResource(MAPLOBJ, enableMemUtils, "MAPL_ENABLE_MEMUTILS:", default='NO',             RC=STATUS )
      VERIFY_(STATUS)
   !EOR
      if (enableTimers /= 'YES' .and. enableTimers /= 'yes') then
         call MAPL_ProfDisable( rc=STATUS )
         VERIFY_(STATUS)
      end if

     if (enableMemUtils /= 'YES' .and. enableMemUtils /= 'yes') then
        call MAPL_MemUtilsDisable( rc=STATUS )
        VERIFY_(STATUS)
     else
        call MAPL_MemUtilsInit( rc=STATUS )
        VERIFY_(STATUS)
     end if

      call MAPL_GetResource( MAPLOBJ, printSpec, label='PRINTSPEC:', default = 0, rc=STATUS )
      VERIFY_(STATUS)

   ! Handle RUN_DT in ROOT_CF
   !-------------------------

      cf_root = ESMF_ConfigCreate(rc=STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigLoadFile(cf_root, ROOT_CF, rc=STATUS )
      VERIFY_(STATUS)

      call ESMF_ConfigGetAttribute(cf_root, value=RUN_DT, Label="RUN_DT:", rc=status)
      if (STATUS == ESMF_SUCCESS) then
         if (heartbeat_dt /= run_dt) then
            if (AmIRoot_) then
               print *, "ERROR: inconsistent values of HEATBEAT_DT and RUN_DT"
            end if
            call ESMF_VMBarrier(VM)
            RETURN_(ESMF_FAILURE)
         end if
      else
         call MAPL_ConfigSetAttribute(cf_root, value=heartbeat_dt, Label="RUN_DT:", rc=status)
         VERIFY_(STATUS)
      endif
      
   ! Add EXPID and EXPDSC from HISTORY.rc to AGCM.rc
   !------------------------------------------------
      cf_hist = ESMF_ConfigCreate(rc=STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigLoadFile(cf_hist, HIST_CF, rc=STATUS )
      VERIFY_(STATUS)

      call MAPL_ConfigSetAttribute(cf_hist, value=HIST_CF, Label="HIST_CF:", rc=status)
      VERIFY_(STATUS)

      call ESMF_ConfigGetAttribute(cf_hist, value=EXPID,  Label="EXPID:",  rc=status)
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute(cf_hist, value=EXPDSC, Label="EXPDSC:", rc=status)
      VERIFY_(STATUS)

      call MAPL_ConfigSetAttribute(cf_root, value=EXPID,  Label="EXPID:",  rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_root, value=EXPDSC, Label="EXPDSC:", rc=status)
      VERIFY_(STATUS)
      
   ! Add CoresPerNode from CAP.rc to HISTORY.rc and AGCM.rc
   !-------------------------------------------------------
      call MAPL_ConfigSetAttribute(cf_root, value=CoresPerNode,  Label="CoresPerNode:",  rc=status)
      VERIFY_(STATUS)
      call MAPL_ConfigSetAttribute(cf_hist, value=CoresPerNode,  Label="CoresPerNode:",  rc=status)
      VERIFY_(STATUS)

   ! Add a SINGLE_COLUMN flag in HISTORY.rc based on DYCORE value(from AGCM.rc)
   !---------------------------------------------------------------------------
      call ESMF_ConfigGetAttribute(cf_root, value=DYCORE,  Label="DYCORE:",  rc=status)
      VERIFY_(STATUS)
      if (DYCORE == 'DATMO') then
         snglcol = 1
         call MAPL_ConfigSetAttribute(cf_hist, value=snglcol,  Label="SINGLE_COLUMN:",  rc=status)
         VERIFY_(STATUS)
      end if

   ! Register the children with MAPL
   !--------------------------------

   !  Create Root child
   !-------------------
      call MAPL_Set(MAPLOBJ, CF=CF_ROOT, RC=STATUS)
      VERIFY_(STATUS)

      ROOT = MAPL_AddChild ( MAPLOBJ,     &
           name       = ROOT_NAME,        &
           SS         = ROOT_SetServices, &
                                rc=STATUS )  
      VERIFY_(STATUS)

   !  Create History child
   !----------------------

      call MAPL_Set(MAPLOBJ, CF=CF_HIST, RC=STATUS)
      VERIFY_(STATUS)

      HIST = MAPL_AddChild ( MAPLOBJ,        &
           name       = 'HIST',           &
           SS         = HIST_SetServices, &
                                rc=STATUS )  
      VERIFY_(STATUS)


   !  Create ExtData child
   !----------------------
      cf_ext = ESMF_ConfigCreate(rc=STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigLoadFile(cf_ext, EXTDATA_CF, rc=STATUS )
      VERIFY_(STATUS)

      call ESMF_ConfigGetAttribute(cf_ext, value=RUN_DT, Label="RUN_DT:", rc=status)
      if (STATUS == ESMF_SUCCESS) then
         if (heartbeat_dt /= run_dt) then
            if (AmIRoot_) then
               print *, "ERROR: inconsistent values of HEATBEAT_DT and RUN_DT"
            end if
            call ESMF_VMBarrier(VM)
            RETURN_(ESMF_FAILURE)
         end if
      else
         call MAPL_ConfigSetAttribute(cf_ext, value=heartbeat_dt, Label="RUN_DT:", rc=status)
         VERIFY_(STATUS)
      endif

      call MAPL_Set(MAPLOBJ, CF=CF_EXT, RC=STATUS)
      VERIFY_(STATUS)

      EXTDATA = MAPL_AddChild ( MAPLOBJ,        &
           name       = 'EXTDATA',           &
           SS         = ExtData_SetServices, &
                                rc=STATUS )  
      VERIFY_(STATUS)

   !  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
   !-------------------------------------------------------------

      call MAPL_Get ( MAPLOBJ, GCS=GCS, GIM=IMPORTS, GEX=EXPORTS,      RC=STATUS )
      VERIFY_(STATUS)

   ! Run as usual unless PRINTSPEC> 0 as set in CAP.rc. If set then
   ! model will not run completely and instead it will simply run MAPL_SetServices
   ! and print out the IM/EX specs. This step uses MAPL_StatePrintSpecCSV found
   ! in MAPL_Generic.F90.


      if (printSpec>0) then

         call MAPL_StatePrintSpecCSV(GCS(ROOT), printSpec, RC=status)
         VERIFY_(STATUS)
         call ESMF_VMBarrier       ( VM,                            RC=STATUS )
         VERIFY_(STATUS)

      else


   !  Initialize the Computational Hierarchy
   !----------------------------------------

      call ESMF_GridCompInitialize ( GCS(ROOT), importState=IMPORTS(ROOT), &
           exportState=EXPORTS(ROOT), clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)

   ! All the EXPORTS of the Hierachy are made IMPORTS of History
   !------------------------------------------------------------

      call ESMF_StateAdd ( IMPORTS(HIST), (/EXPORTS(ROOT)/), RC=STATUS )
      VERIFY_(STATUS)

      allocate(lswrap%ptr, stat=status)
      VERIFY_(STATUS)
      call ESMF_UserCompSetInternalState(GCS(HIST), 'MAPL_LocStreamList', &
           lswrap, STATUS)
      VERIFY_(STATUS)
      call MAPL_GetAllExchangeGrids(GCS(ROOT), LSADDR, RC=STATUS)
      VERIFY_(STATUS)
      lswrap%ptr%LSADDR_PTR => LSADDR

   ! Initialize the History
   !------------------------

      call ESMF_GridCompInitialize (   GCS(HIST), importState=IMPORTS(HIST), &
           exportState=EXPORTS(HIST), clock=CLOCK_HIST,  userRC=STATUS )
      VERIFY_(STATUS)
    
   ! Prepare EXPORTS for ExtData
   ! ---------------------------
       call ESMF_StateGet(IMPORTS(ROOT), ITEMCOUNT=ITEMCOUNT, RC=STATUS)
       VERIFY_(STATUS)
       allocate(ITEMNAMES(ITEMCOUNT), STAT=STATUS)
       VERIFY_(STATUS)
       allocate(ITEMTYPES(ITEMCOUNT), STAT=STATUS)
       VERIFY_(STATUS)

       call ESMF_StateGet(IMPORTS(ROOT), ITEMNAMELIST=ITEMNAMES, &
                          ITEMTYPELIST=ITEMTYPES, RC=STATUS)
       VERIFY_(STATUS)

       DO I=1, ITEMCOUNT
          if(ItemTypes(I) == ESMF_StateItem_Field) then
             call ESMF_StateGet(IMPORTS(ROOT), ItemNames(i), field, rc=status)
             VERIFY_(STATUS)
             
             call MAPL_StateAdd(EXPORTS(EXTDATA), field, rc=status)
             VERIFY_(STATUS)
          else if(ItemTypes(I) == ESMF_StateItem_FieldBundle) then
             call ESMF_StateGet(IMPORTS(ROOT), ItemNames(i), bundle, rc=status)
             VERIFY_(STATUS)
             call MAPL_StateAdd(EXPORTS(EXTDATA), bundle, rc=status)
             VERIFY_(STATUS)
          end if
       END DO
       deallocate(itemtypes)
       deallocate(itemnames)


   ! Initialize the ExtData
   !------------------------

      call ESMF_GridCompInitialize (   GCS(EXTDATA), importState=IMPORTS(EXTDATA), &
           exportState=EXPORTS(EXTDATA), & 
           clock=CLOCK,  userRC=STATUS )
      VERIFY_(STATUS)
    
   ! Time Loop starts by checking for Segment Ending Time
   !-----------------------------------------------------
      TIME_LOOP: do n=1,nsteps

         call MAPL_MemUtilsWrite(vm, 'MAPL_Cap:TimeLoop',           RC=STATUS )
         VERIFY_(STATUS)

         if( .not.LPERP ) then
              DONE = ESMF_ClockIsStopTime( CLOCK_HIST, RC=STATUS )
              VERIFY_(STATUS)
              if ( DONE ) exit
         endif

   ! Call Record for intermediate checkpoint (if desired)
   !  Note that we are not doing a Record for History.
   ! ------------------------------------------------------

         call ESMF_GridCompWriteRestart( GCS(ROOT), importState=IMPORTS(ROOT), &
              exportState=EXPORTS(ROOT), clock=CLOCK_HIST, userRC=STATUS )
         VERIFY_(STATUS)

   ! Run the ExtData Component
   ! --------------------------

         call ESMF_GridCompRun     ( GCS(EXTDATA), importState=IMPORTS(EXTDATA), &
                                     exportState=EXPORTS(EXTDATA), &
                                     clock=CLOCK, userRC=STATUS )
         VERIFY_(STATUS)

   ! Run the Gridded Component
   ! --------------------------

         call ESMF_GridCompRun( GCS(ROOT), importState=IMPORTS(ROOT), &
              exportState=EXPORTS(ROOT), clock=CLOCK, userRC=STATUS )
         VERIFY_(STATUS)

   ! Synchronize for Next TimeStep
   ! -----------------------------

         call ESMF_VMBarrier( VM, RC=STATUS )
         VERIFY_(STATUS)

   ! Advance the Clock before running History and Record
   ! ---------------------------------------------------

         call ESMF_ClockAdvance     ( CLOCK,                        RC=STATUS )
         VERIFY_(STATUS)
         call ESMF_ClockAdvance     ( CLOCK_HIST,                   RC=STATUS )
         VERIFY_(STATUS)

   ! Update Perpetual Clock
   ! ----------------------

         if( LPERP ) then
             call Perpetual_Clock ( clock, clock_HIST, PERPETUAL_YEAR, PERPETUAL_MONTH, PERPETUAL_DAY, STATUS )
             VERIFY_(STATUS)
         endif

         call ESMF_ClockGet ( clock, CurrTime=currTime, rc=status )
         VERIFY_(STATUS)
         call ESMF_TimeGet  ( CurrTime, YY = AGCM_YY, &
                                        MM = AGCM_MM, &
                                        DD = AGCM_DD, &
                                        H  = AGCM_H , &
                                        M  = AGCM_M , &
                                        S  = AGCM_S, rc=status )
         VERIFY_(STATUS)
         if( AmIRoot_ ) write(6,1000) AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S
    1000 format(1x,'AGCM Date: ',i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)

   ! Call History Run for Output
   ! ---------------------------

         call ESMF_GridCompRun( GCS(HIST), importState=IMPORTS(HIST), &
              exportState=EXPORTS(HIST), clock=CLOCK_HIST, userRC=STATUS )
         VERIFY_(STATUS)

      enddo TIME_LOOP ! end of time loop

   !  Finalize
   !  --------

      call ESMF_GridCompFinalize( GCS(ROOT), importState=IMPORTS(ROOT),&
           exportState=EXPORTS(ROOT), clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)
      call ESMF_GridCompFinalize( GCS(HIST), importState=IMPORTS(HIST),&
           exportState=EXPORTS(HIST), clock=CLOCK_HIST,userRC=STATUS )
      VERIFY_(STATUS)
      call ESMF_GridCompFinalize( GCS(EXTDATA), importState=IMPORTS(EXTDATA),&
            exportState=EXPORTS(EXTDATA), clock=CLOCK, userRC=STATUS )
      VERIFY_(STATUS)

   !  Finalize itselt
   ! ----------------

      call CAP_Finalize(CLOCK_HIST, "cap_restart", rc=STATUS)
      VERIFY_(STATUS)

      call ESMF_ConfigDestroy(cf_ext, RC=status)
      VERIFY_(STATUS)
      call ESMF_ConfigDestroy(cf_hist, RC=status)
      VERIFY_(STATUS)
      call ESMF_ConfigDestroy(cf_root, RC=status)
      VERIFY_(STATUS)
      call ESMF_ConfigDestroy(config, RC=status)
      VERIFY_(STATUS)

      end if ! PRINTSPEC

      call MAPL_FinalizeShmem (rc=status)
      VERIFY_(STATUS)

   ! Write EGRESS file
   !------------------
      call ESMF_VMBarrier(VM)
      if(present(FinalFile)) then
         if (AmIRoot_) then
            close(99)
            open (99,file=FinalFile,form='formatted')
            close(99)
         end if
      end if

   end if ESMFCOMMIF

   IOCOMMIF: if (ioComm /= MPI_COMM_NULL) then
      call MPI_comm_rank(mapl_comm%iocomm,mapl_comm%myIoRank,status)
      VERIFY_(STATUS)
      call MAPL_CFIOServerStart(mapl_Comm,rc=status)
      VERIFY_(STATUS)
   end if IOCOMMIF

   call MPI_Barrier(MPI_COMM_WORLD,status)
   VERIFY_(STATUS) 
!  Finalize framework
!  ------------------
   
#if 0
!ALT due to a bug in MAPL (or in the garbage collection of ESMF_VMFinalize)
!we have to bypass next line
   call ESMF_Finalize (RC=status)
   VERIFY_(STATUS)
#else
   call mpi_finalize(status)
   VERIFY_(STATUS)

#endif

   RETURN_(ESMF_SUCCESS)

 end subroutine MAPL_CAP

   subroutine Perpetual_Clock ( clock, clock_HIST, PERPETUAL_YEAR, PERPETUAL_MONTH, PERPETUAL_DAY, rc )
     type(ESMF_Clock),intent(inout) :: clock
     type(ESMF_Clock),intent(inout) :: clock_HIST
     integer,         intent(in)    :: PERPETUAL_YEAR
     integer,         intent(in)    :: PERPETUAL_MONTH
     integer,         intent(in)    :: PERPETUAL_DAY
     integer,         intent(out)   :: rc

     type(ESMF_Time)                :: currTime
     type(ESMF_Alarm)               :: PERPETUAL
     type(ESMF_Calendar)            :: cal
     integer                        :: status
     integer                        :: HIST_YY, HIST_MM, HIST_DD, HIST_H, HIST_M, HIST_S
     integer                        :: AGCM_YY, AGCM_MM, AGCM_DD, AGCM_H, AGCM_M, AGCM_S

     character(len=ESMF_MAXSTR), parameter :: IAm="Perpetual_Clock"

     call ESMF_ClockGetAlarm ( clock_HIST, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
     VERIFY_(STATUS)
     call ESMF_AlarmRingerOff( PERPETUAL, rc=status )
     VERIFY_(STATUS)

         call ESMF_ClockGet ( clock, currTime=currTime, calendar=cal, rc=status )
         VERIFY_(STATUS)
         call ESMF_TimeGet  ( CurrTime, YY = AGCM_YY, &
                                        MM = AGCM_MM, &
                                        DD = AGCM_DD, &
                                        H  = AGCM_H , &
                                        M  = AGCM_M , &
                                        S  = AGCM_S, rc=status )
         VERIFY_(STATUS)

         call ESMF_ClockGet ( clock_HIST, CurrTime=CurrTime, calendar=cal, rc=status )
         VERIFY_(STATUS)
         call ESMF_TimeGet  ( CurrTime, YY = HIST_YY, &
                                        MM = HIST_MM, &
                                        DD = HIST_DD, &
                                        H  = HIST_H , &
                                        M  = HIST_M , &
                                        S  = HIST_S, rc=status )
         VERIFY_(STATUS)
#ifdef DEBUG
         if( AmIRoot_ ) then
            write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside PERP M0: ",AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S
            write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside PERP H0: ",HIST_YY,HIST_MM,HIST_DD,HIST_H,HIST_M,HIST_S
         endif
#endif
         if( (PERPETUAL_YEAR  /= -999)  .and. &
             (PERPETUAL_MONTH == -999)  .and. &
             (PERPETUAL_DAY   == -999) ) then
                                         AGCM_YY  = PERPETUAL_YEAR
         endif

         if( (PERPETUAL_YEAR  /= -999)  .and. &
             (PERPETUAL_MONTH /= -999)  .and. &
             (PERPETUAL_DAY   == -999) ) then
                                         AGCM_YY  = PERPETUAL_YEAR
                                         AGCM_MM  = PERPETUAL_MONTH
                                     if( HIST_MM /= PERPETUAL_MONTH ) then 
                                         HIST_MM  = PERPETUAL_MONTH
                                                if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
                                         call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
                                         VERIFY_(STATUS)
                                     endif
#ifdef DEBUG
      if( AmIRoot_ ) then
          write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside PERP M1: ",AGCM_YY,AGCM_MM,AGCM_DD,AGCM_H,AGCM_M,AGCM_S
          write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside PERP H1: ",HIST_YY,HIST_MM,HIST_DD,HIST_H,HIST_M,HIST_S
      endif
#endif
         endif

         if( (PERPETUAL_YEAR  == -999)  .and. &
             (PERPETUAL_MONTH /= -999)  .and. &
             (PERPETUAL_DAY   == -999) ) then
                                         AGCM_MM  = PERPETUAL_MONTH
                                     if( HIST_MM /= PERPETUAL_MONTH ) then 
                                         HIST_MM  = PERPETUAL_MONTH
                                                if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
                                                                           AGCM_YY  = HIST_YY
                                         call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
                                         VERIFY_(STATUS)
                                     endif
         endif

         if( (PERPETUAL_YEAR  /= -999)  .and. &
             (PERPETUAL_MONTH /= -999)  .and. &
             (PERPETUAL_DAY   /= -999) ) then
                                         AGCM_YY  = PERPETUAL_YEAR
                                         AGCM_MM  = PERPETUAL_MONTH
                                         AGCM_DD  = PERPETUAL_DAY
                                     if( HIST_MM /= PERPETUAL_MONTH ) then 
                                         HIST_MM  = PERPETUAL_MONTH
                                                if( PERPETUAL_MONTH /= 12) HIST_YY  = HIST_YY + 1
                                         call ESMF_AlarmRingerOn( PERPETUAL, rc=status )
                                         VERIFY_(STATUS)
                                     endif
         endif

         call ESMF_TimeSet( CurrTime, YY = AGCM_YY, &
                                      MM = AGCM_MM, &
                                      DD = AGCM_DD, &
                                       H = AGCM_H , &
                                       M = AGCM_M , &
                                       S = AGCM_S , &
                       calendar=cal,  rc = STATUS  )
         VERIFY_(STATUS)
         call ESMFL_ClockSet ( clock, CurrTime=CurrTime, rc=status )
         VERIFY_(STATUS)

         call ESMF_TimeSet( CurrTime, YY = HIST_YY, &
                                      MM = HIST_MM, &
                                      DD = HIST_DD, &
                                       H = HIST_H , &
                                       M = HIST_M , &
                                       S = HIST_S , &
                       calendar=cal,  rc = STATUS  )
         VERIFY_(STATUS)
         call ESMFL_ClockSet ( clock_HIST, CurrTime=CurrTime, rc=status )
         VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine Perpetual_Clock

  subroutine ESMFL_ClockSet(clock, currTime, rc)
! Args
    type (ESMF_Clock)                :: clock
    type (ESMF_Time),  intent(IN   ) :: currTime
    integer, optional, intent(  OUT) :: rc

! ErrLog vars
    integer                                :: status
    character(len=ESMF_MAXSTR), parameter  :: IAm='ESMFL_ClockCreate'

! Local Vars    
    type(ESMF_Time)                        :: targetTime
    type(ESMF_Time)                        :: cTime
    type(ESMF_TimeInterval)                :: zero
    type(ESMF_TimeInterval)                :: delt
    type(ESMF_Time)                        :: ringTime
    type(ESMF_TimeInterval)                :: ringInterval
    type(ESMF_Alarm), allocatable          :: AlarmList(:)
    logical                                :: ringing
    integer                                :: I
    integer                                :: nalarms


    targetTime = currTime

! get the CurrentTime from the clock
    call ESMF_ClockGet(clock, alarmCount = nalarms, currTime=cTime, rc=status)
    VERIFY_(STATUS)

    delt = targetTime - cTime

    call ESMF_TimeIntervalSet(zero, rc=status)
    VERIFY_(STATUS)

! Get the list of current alarms in the clock
    allocate (alarmList(nalarms), stat = status)
    VERIFY_(STATUS)
    call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
         alarmList=alarmList, alarmCount = nalarms, rc=status)
    VERIFY_(STATUS)

! Loop over all alarms
    DO I = 1, nalarms
       call ESMF_AlarmGet(alarmList(I), ringTime=ringTime, ringInterval=ringInterval, &
            ringing=ringing, rc=status)
       VERIFY_(STATUS)

! skip alarms with zero ringing interval
       if (ringInterval == zero) cycle

! Make sure the time-shift is multiple of ringing interval
       ASSERT_(MOD(delt,ringInterval) == zero)
       ringTime=ringTime + delt

       call ESMF_AlarmSet(alarmList(I), ringTime=ringTime, ringing=ringing, rc=status)
       VERIFY_(STATUS)

    END DO

! Protection in case we reset the clock outside of StopTime
    call ESMF_ClockStopTimeDisable(clock, rc=status)
    VERIFY_(STATUS)

    call ESMF_ClockSet(clock, currTime=targetTime, rc=status)
    VERIFY_(STATUS)

! We do not need the protection anymore
    call ESMF_ClockStopTimeEnable(clock, rc=status)
    VERIFY_(STATUS)

! clean-up
    deallocate(alarmList)

    RETURN_(ESMF_SUCCESS)
  end subroutine ESMFL_ClockSet

 subroutine CAP_FINALIZE ( clock,filen, rc )

   type(ESMF_Clock),    intent(in   ) :: clock
   character(len=*),    optional      :: filen
   integer, optional,   intent(  out) :: rc

   integer        :: UNIT
   integer        :: datetime(2)
   integer        :: YY, MM, DD, H, M, S
   integer        :: status
   character(len=ESMF_MAXSTR), parameter :: IAm="CAP_FINALIZE"
   character(len=ESMF_MAXSTR)            :: filen_

   type(ESMF_Time)     :: CurrentTime
   
   filen_ = "cap_restart"
   if (present(filen))     filen_ = trim(filen )
   
! Retrieve Current Time for Cap Restart
! -------------------------------------

   call ESMF_ClockGet ( clock, currTime=currentTime, rc=status )
   VERIFY_(STATUS)
   call ESMF_TimeGet  ( CurrentTime, YY = YY, &
                                     MM = MM, &
                                     DD = DD, &
                                     H  = H , &
                                     M  = M , &
                                     S  = S, rc=status )
   VERIFY_(STATUS)

   CALL MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)

! Write CAP Restart File and Ending Time for Current Segment
! ----------------------------------------------------------

    if( MAPL_AM_I_ROOT() ) then
       UNIT = GETFILE( filen_, form="formatted" )
       write(unit,100) datetime
100    format(i8.8,1x,i6.6)
       call FREE_FILE (UNIT)
    endif

    RETURN_(ESMF_SUCCESS)
  end subroutine CAP_FINALIZE


! !IROUTINE: MAPL_ClockInit -- Sets the clock

! !INTERFACE: 

  subroutine MAPL_ClockInit ( MAPLOBJ, Clock, nsteps, rc)

! !ARGUMENTS:

     type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
     type(ESMF_Clock),    intent(  out) :: Clock
     integer,             intent(  out) :: nsteps
     integer, optional,   intent(  out) :: rc

!  !DESCRIPTION:

!   This is a private routine that sets the start and 
!   end times and the time interval of the application clock from the configuration.
!   This time interal is the ``heartbeat'' of the application.
!   The Calendar is set to Gregorian by default. 
!   The start time is temporarily set to 1 interval before the time in the
!   configuration. Once the Alarms are set in intialize, the clock will
!   be advanced to guarantee it and its alarms are in the same state as they
!   were after the last advance before the previous Finalize.
!


     type(ESMF_Time)          :: StartTime    ! Initial     Begin  Time of Experiment
     type(ESMF_Time)          :: EndTime      ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: StopTime     ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: CurrTime     ! Current     Current Time of Experiment
     type(ESMF_TimeInterval)  :: timeStep     ! HEARTBEAT
     type(ESMF_TimeInterval)  :: duration
     type(ESMF_Calendar)      :: cal
     character(ESMF_MAXSTR)   :: CALENDAR

     integer                  :: STATUS
     character(ESMF_MAXSTR)   :: IAM="MAPL_ClockInit"

     integer        :: BEG_YY
     integer        :: BEG_MM
     integer        :: BEG_DD
     integer        :: BEG_H
     integer        :: BEG_M
     integer        :: BEG_S

     integer        :: CUR_YY
     integer        :: CUR_MM
     integer        :: CUR_DD
     integer        :: CUR_H
     integer        :: CUR_M
     integer        :: CUR_S

     integer        :: END_YY
     integer        :: END_MM
     integer        :: END_DD
     integer        :: END_H
     integer        :: END_M
     integer        :: END_S

     integer        :: DUR_YY
     integer        :: DUR_MM
     integer        :: DUR_DD
     integer        :: DUR_H
     integer        :: DUR_M
     integer        :: DUR_S

     integer        :: HEARTBEAT_DT
     integer        :: NUM_DT
     integer        :: DEN_DT

     integer        :: UNIT
     integer        :: datetime(2)

! Begin
!------

! Read Times From Config
! ----------------------

!BOR

     call MAPL_GetResource( MAPLOBJ, datetime, label='BEG_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, BEG_YY, BEG_MM, BEG_DD, BEG_H, BEG_M, BEG_S)
     else

! !RESOURCE_ITEM: year :: Beginning year (integer)
        call MAPL_GetResource( MAPLOBJ, BEG_YY, label='BEG_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Beginning month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, BEG_MM, label='BEG_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Beginning day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, BEG_DD, label='BEG_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Beginning hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, BEG_H , label='BEG_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Beginning minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_M , label='BEG_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: second :: Beginning second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_S , label='BEG_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

     call MAPL_GetResource( MAPLOBJ, datetime, label='END_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, END_YY, END_MM, END_DD, END_H, END_M, END_S)
     else
! !RESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, END_YY, label='END_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, END_MM, label='END_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, END_DD, label='END_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, END_H , label='END_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_M , label='END_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_S , label='END_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! Replace JOB_DURATION with JOB_SGMT as prefered RC parameter
! -----------------------------------------------------------
     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_SGMT:',     rc=STATUS )
     if(STATUS/=ESMF_SUCCESS) then
     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_DURATION:', rc=STATUS )
     end if

     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, DUR_YY, DUR_MM, DUR_DD, DUR_H, DUR_M, DUR_S)
     else
! !RESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, DUR_YY, label='DUR_YY:', DEFAULT=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, DUR_MM, label='DUR_MM:', default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, DUR_DD, label='DUR_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, DUR_H , label='DUR_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !RESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_M , label='DUR_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_S , label='DUR_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! !RESOURCE_ITEM: seconds :: Interval of the application clock (the Heartbeat)
     call MAPL_GetResource( MAPLOBJ, HEARTBEAT_DT, label='HEARTBEAT_DT:',            rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: 1 :: numerator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, NUM_DT, label='NUM_DT:', default=0, rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: 1 :: denominator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, DEN_DT, label='DEN_DT:', default=1, rc=STATUS )
     VERIFY_(STATUS)
! !RESOURCE_ITEM: string :: Calendar type
     call MAPL_GetResource( MAPLOBJ, CALENDAR, label='CALENDAR:', default="GREGORIAN", rc=STATUS )
     VERIFY_(STATUS)

!EOR

     ASSERT_(NUM_DT>=0)
     ASSERT_(DEN_DT> 0)
     ASSERT_(HEARTBEAT_DT>=0)
!     ASSERT_(NUM_DT*HEARTBEAT_DT>0)
     ASSERT_(NUM_DT<DEN_DT)

! initialize calendar to be Gregorian type
! ----------------------------------------

     if    (CALENDAR=="GREGORIAN") then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="JULIAN"   ) then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_JULIAN, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_JULIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="NOLEAP"   ) then
        cal = ESMF_CalendarCreate( ESMF_CALKIND_NOLEAP, name="ApplicationCalendar", rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP, RC=STATUS)
        VERIFY_(STATUS)
     else
        ASSERT_(.false.)
     endif

! initialize start time for Alarm frequencies
! -------------------------------------------

     call ESMF_TimeSet( StartTime, YY = BEG_YY, &
                                   MM = BEG_MM, &
                                   DD = BEG_DD, &
                                    H = BEG_H , &
                                    M = BEG_M , &
                                    S = BEG_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

     call ESMF_TimeSet(   EndTime, YY = END_YY, &
                                   MM = END_MM, &
                                   DD = END_DD, &
                                    H = END_H , &
                                    M = END_M , &
                                    S = END_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)  

! Read CAP Restart File for Current Time
! --------------------------------------

     CUR_YY = BEG_YY
     CUR_MM = BEG_MM
     CUR_DD = BEG_DD
     CUR_H  = BEG_H
     CUR_M  = BEG_M
     CUR_S  = BEG_S

     UNIT = GETFILE ( "cap_restart", form="formatted", ALL_PES=.true., rc=status )
     VERIFY_(STATUS)

     rewind(UNIT)
     read(UNIT,100,err=999,end=999) datetime
100  format(i8.8,1x,i6.6)

     CALL MAPL_UnpackDateTime(DATETIME, CUR_YY, CUR_MM, CUR_DD, CUR_H, CUR_M, CUR_S)

     if( MAPL_AM_I_ROOT() ) then
         write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2)") 'Read CAP restart properly, Current Date = ', CUR_YY,CUR_MM,CUR_DD
         write(6,"(a,2x,i2.2,':',i2.2,':',i2.2)") '                           Current Time = ', CUR_H ,CUR_M ,CUR_S
         print *
     endif


999  continue  ! Initialize Current time

     call FREE_FILE (UNIT)

     call ESMF_TimeSet( CurrTime, YY = CUR_YY, &
                                  MM = CUR_MM, &
                                  DD = CUR_DD, &
                                   H = CUR_H , &
                                   M = CUR_M , &
                                   S = CUR_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

! initialize final stop time
! --------------------------

     call ESMF_TimeIntervalSet(  duration, YY = DUR_YY, &
                                   MM = DUR_MM, &
                                    D = DUR_DD, &
                                    H = DUR_H , &
                                    M = DUR_M , &
                                    S = DUR_S , &
                                    startTime = currTime, &
                                    rc = STATUS  )
     VERIFY_(STATUS)

     stopTime = currTime + duration

! initialize model time step
! --------------------------

     call ESMF_TimeIntervalSet( timeStep, S=HEARTBEAT_DT, sN=NUM_DT, sD=DEN_DT, rc=STATUS )
     VERIFY_(STATUS)

     nsteps = duration/timestep

! Create Clock and set it to one time step before StartTime.
! After Initialize has created all alarms, we will advance the
! clock to ensure the proper ringing state of all alarms
!-------------------------------------------------------------

     if (endTime < stopTime) then
        clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
             startTime=StartTime, stopTime=EndTime, rc=STATUS )
     else
        clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
             startTime=StartTime, stopTime=StopTime, rc=STATUS )
     end if
     VERIFY_(STATUS)

     call ESMF_ClockSet ( clock, CurrTime=CurrTime, rc=status )
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine MAPL_ClockInit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)
     integer, intent(IN   ) :: YY, MM, DD, H, M, S
     integer, intent(  OUT) :: DATETIME(:)

     datetime(1) = 10000*YY + 100*MM + DD
     datetime(2) = 10000* H + 100* M + S
     return
   end subroutine MAPL_PackDateTime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine MAPL_UnpackDateTime(DATETIME, YY, MM, DD, H, M, S)
     integer, intent(IN   ) :: DATETIME(:)
     integer, intent(  OUT) :: YY, MM, DD, H, M, S

     YY =     datetime(1)/10000
     MM = mod(datetime(1),10000)/100
     DD = mod(datetime(1),100)
     H  =     datetime(2)/10000
     M  = mod(datetime(2),10000)/100
     S  = mod(datetime(2),100)
     return
   end subroutine MAPL_UnpackDateTime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! BOPI -------------------------------------------------------------------
!
! !IROUTINE: MAPL_ConfigSetAttribute - Set a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigSetAttribute()
      subroutine MAPL_ConfigSetString( config, value, label, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)             :: config     
      character(len=*), intent(in)                 :: value
      character(len=*), intent(in), optional       :: label 
      integer, intent(out), optional               :: rc   

!
! !DESCRIPTION: 
!  Sets an integer {\tt value} in the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Integer value to set. 
!   \item [{[label]}]
!     Identifying attribute label. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! EOPI -------------------------------------------------------------------

       integer,   parameter :: LSZ = 256  ! Maximum line size
       integer,   parameter :: MSZ = 512  ! Used to size buffer; this is
                                          ! usually *less* than the number
                                          ! of non-blank/comment lines
                                          ! (because most lines are shorter
                                          ! then LSZ)
 
       integer,   parameter :: NBUF_MAX = MSZ*LSZ ! max size of buffer
       integer,   parameter :: NATT_MAX = NBUF_MAX/16 ! max # attributes;  
                                                  ! assumes an average line
                                                  ! size of 16, the code
                                                  ! will do a bound check

       character, parameter :: BLK = achar(32)   ! blank (space)
       character, parameter :: TAB = achar(09)   ! TAB
#ifdef ESMF_HAS_ACHAR_BUG
       character, parameter :: EOL = achar(12)   ! end of line mark (cr)
#else
       character, parameter :: EOL = achar(10)   ! end of line mark (newline)
#endif
       character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
       character, parameter :: NUL = achar(00)   ! what it says

      character(len=ESMF_MAXSTR) :: Iam = 'MAPL_ConfigSetString'

      character(len=ESMF_MAXSTR) :: logmsg
      character(len=LSZ) :: curVal, newVal
      integer :: iret, i, j, k, m, nchar, ninsert, ndelete, lenThisLine

      ! Initialize return code; assume routine not implemented
      iret = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
!ALT      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! Set config buffer at desired attribute
      if ( present (label) ) then
         call ESMF_ConfigGetAttribute( config, value=curVal, label=label, rc = iret )
      else
         call ESMF_ConfigGetAttribute( config, value=curVal, rc = iret )
      endif

      if ( iret .ne. ESMF_SUCCESS ) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) then
          ! set config buffer at end for appending
          i = config%cptr%nbuf
        else
          if ( present( rc ) ) then
            rc = iret
          endif
          return
        endif
      else ! attribute found
        ! set config buffer for overwriting/inserting
        i = config%cptr%value_begin
        curVal = BLK // trim(curVal) // BLK // EOL ! like config%cptr%this_line
      endif

      ! for appending, create new attribute string with label and value
      if ( i .eq. config%cptr%nbuf .and. present(label) ) then
        write(newVal, *) label, trim(value)
        newVal = trim(adjustl(newVal)) // EOL
        j = i + len_trim(newVal)

        ! check to ensure len of newVal doesn't exceed LSZ
        if ( (j-i) .gt. LSZ) then
           write(logmsg, *) ", attribute label, value & EOL are ", j-i, &
               " characters long, only ", LSZ, " characters allowed per line"
           RETURN_(ESMC_RC_LONG_STR)
        endif

        ! check if enough space left in config buffer
        if (j .ge. NBUF_MAX) then   ! room for EOB if necessary
           write(logmsg, *) ", attribute label & value require ", j-i+1, &
               " characters (including EOL & EOB), only ", NBUF_MAX-i, &
               " characters left in config buffer"
           RETURN_(ESMC_RC_LONG_STR)
        endif
      endif

      ! overwrite, with possible insertion or deletion of extra characters
      if (i .eq. config%cptr%value_begin) then
         write(newVal, *) value
         newVal = BLK // trim(adjustl(newVal)) // EOL
         j = i + len_trim(newVal) - 1

         !  check if we need more space to insert new characters;
         !  shift buffer down (linked-list redesign would be better!)
         nchar = j-i+1
         lenThisLine = len_trim(curVal) - 1
         if ( nchar .gt. lenThisLine) then

            ! check to ensure length of extended line doesn't exceed LSZ
            do m = i, 1, -1
              if (config%cptr%buffer(m:m) .eq. EOL) then
                exit
              endif
            enddo
            if (j-m+1 .gt. LSZ) then
               write(logmsg, *) ", attribute label, value & EOL are ", j-m+1, &
                  " characters long, only ", LSZ, " characters allowed per line"
               RETURN_(ESMC_RC_LONG_STR)
            endif

            ! check if enough space left in config buffer to extend line
            if (j+1 .ge. NBUF_MAX) then   ! room for EOB if necessary
               write(logmsg, *) ", attribute label & value require ", j-m+1, &
                   " characters (including EOL & EOB), only ", NBUF_MAX-i, &
                   " characters left in config buffer"
               RETURN_(ESMC_RC_LONG_STR)
            endif

            ninsert = nchar - lenThisLine
            do k = config%cptr%nbuf, j, -1
               config%cptr%buffer(k+ninsert:k+ninsert) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf + ninsert

         ! or if we need less space and remove characters;
         ! shift buffer up
         elseif ( nchar .lt. lenThisLine ) then
           ndelete = lenThisLine - nchar
            do k = j+1, config%cptr%nbuf
               config%cptr%buffer(k-ndelete:k-ndelete) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf - ndelete
         endif
      endif

      ! write new attribute value into config
      config%cptr%buffer(i:j) = newVal(1:len_trim(newVal))

      ! if appended, reset EOB marker and nbuf
      if (i .eq. config%cptr%nbuf) then
!@@        j = j + 1
!@@        config%cptr%buffer(j:j) = EOB
        config%cptr%nbuf = j
      endif

      if( present( rc )) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) iret = ESMF_SUCCESS
        rc = iret
      endif
      
      return
    end subroutine MAPL_ConfigSetString

! BOPI -------------------------------------------------------------------
!
! !IROUTINE: MAPL_ConfigSetAttribute - Set a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigSetAttribute()
      subroutine MAPL_ConfigSetIntI4( config, value, label, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)             :: config     
      integer(ESMF_KIND_I4), intent(in)            :: value
      character(len=*), intent(in), optional       :: label 
      integer, intent(out), optional               :: rc   

!
! !DESCRIPTION: 
!  Sets an integer {\tt value} in the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Integer value to set. 
!   \item [{[label]}]
!     Identifying attribute label. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! EOPI -------------------------------------------------------------------

       integer,   parameter :: LSZ = 256  ! Maximum line size
       integer,   parameter :: MSZ = 512  ! Used to size buffer; this is
                                          ! usually *less* than the number
                                          ! of non-blank/comment lines
                                          ! (because most lines are shorter
                                          ! then LSZ)
 
       integer,   parameter :: NBUF_MAX = MSZ*LSZ ! max size of buffer
       integer,   parameter :: NATT_MAX = NBUF_MAX/16 ! max # attributes;  
                                                  ! assumes an average line
                                                  ! size of 16, the code
                                                  ! will do a bound check

       character, parameter :: BLK = achar(32)   ! blank (space)
       character, parameter :: TAB = achar(09)   ! TAB
#ifdef ESMF_HAS_ACHAR_BUG
       character, parameter :: EOL = achar(12)   ! end of line mark (cr)
#else
       character, parameter :: EOL = achar(10)   ! end of line mark (newline)
#endif
       character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
       character, parameter :: NUL = achar(00)   ! what it says

      character(len=ESMF_MAXSTR) :: Iam = 'MAPL_ConfigSetString'

      character(len=ESMF_MAXSTR) :: logmsg
      character(len=LSZ) :: curVal, newVal
      integer :: iret, i, j, k, m, nchar, ninsert, ndelete, lenThisLine

      ! Initialize return code; assume routine not implemented
      iret = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
!ALT      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! Set config buffer at desired attribute
      if ( present (label) ) then
         call ESMF_ConfigGetAttribute( config, value=curVal, label=label, rc = iret )
      else
         call ESMF_ConfigGetAttribute( config, value=curVal, rc = iret )
      endif

      if ( iret .ne. ESMF_SUCCESS ) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) then
          ! set config buffer at end for appending
          i = config%cptr%nbuf
        else
          if ( present( rc ) ) then
            rc = iret
          endif
          return
        endif
      else ! attribute found
        ! set config buffer for overwriting/inserting
        i = config%cptr%value_begin
        curVal = BLK // trim(curVal) // BLK // EOL ! like config%cptr%this_line
      endif

      ! for appending, create new attribute string with label and value
      if ( i .eq. config%cptr%nbuf .and. present(label) ) then
        write(newVal, *) label, value
        newVal = trim(adjustl(newVal)) // EOL
        j = i + len_trim(newVal)

        ! check to ensure len of newVal doesn't exceed LSZ
        if ( (j-i) .gt. LSZ) then
           write(logmsg, *) ", attribute label, value & EOL are ", j-i, &
               " characters long, only ", LSZ, " characters allowed per line"
           RETURN_(ESMC_RC_LONG_STR)
        endif

        ! check if enough space left in config buffer
        if (j .ge. NBUF_MAX) then   ! room for EOB if necessary
           write(logmsg, *) ", attribute label & value require ", j-i+1, &
               " characters (including EOL & EOB), only ", NBUF_MAX-i, &
               " characters left in config buffer"
           RETURN_(ESMC_RC_LONG_STR)
        endif
      endif

      ! overwrite, with possible insertion or deletion of extra characters
      if (i .eq. config%cptr%value_begin) then
         write(newVal, *) value
         newVal = BLK // trim(adjustl(newVal)) // EOL
         j = i + len_trim(newVal) - 1

         !  check if we need more space to insert new characters;
         !  shift buffer down (linked-list redesign would be better!)
         nchar = j-i+1
         lenThisLine = len_trim(curVal) - 1
         if ( nchar .gt. lenThisLine) then

            ! check to ensure length of extended line doesn't exceed LSZ
            do m = i, 1, -1
              if (config%cptr%buffer(m:m) .eq. EOL) then
                exit
              endif
            enddo
            if (j-m+1 .gt. LSZ) then
               write(logmsg, *) ", attribute label, value & EOL are ", j-m+1, &
                  " characters long, only ", LSZ, " characters allowed per line"
               RETURN_(ESMC_RC_LONG_STR)
            endif

            ! check if enough space left in config buffer to extend line
            if (j+1 .ge. NBUF_MAX) then   ! room for EOB if necessary
               write(logmsg, *) ", attribute label & value require ", j-m+1, &
                   " characters (including EOL & EOB), only ", NBUF_MAX-i, &
                   " characters left in config buffer"
               RETURN_(ESMC_RC_LONG_STR)
            endif

            ninsert = nchar - lenThisLine
            do k = config%cptr%nbuf, j, -1
               config%cptr%buffer(k+ninsert:k+ninsert) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf + ninsert

         ! or if we need less space and remove characters;
         ! shift buffer up
         elseif ( nchar .lt. lenThisLine ) then
           ndelete = lenThisLine - nchar
            do k = j+1, config%cptr%nbuf
               config%cptr%buffer(k-ndelete:k-ndelete) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf - ndelete
         endif
      endif

      ! write new attribute value into config
      config%cptr%buffer(i:j) = newVal(1:len_trim(newVal))

      ! if appended, reset EOB marker and nbuf
      if (i .eq. config%cptr%nbuf) then
!@@        j = j + 1
!@@        config%cptr%buffer(j:j) = EOB
        config%cptr%nbuf = j
      endif

      if( present( rc )) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) iret = ESMF_SUCCESS
        rc = iret
      endif
      
      return
    end subroutine MAPL_ConfigSetIntI4

 end module MAPL_CapMod
