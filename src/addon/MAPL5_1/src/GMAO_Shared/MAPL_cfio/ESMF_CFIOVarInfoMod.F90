!==============================================================================
!BOP
! !MODULE: ESMF_CFIOVarInfoMod.F90 - Source file for CFIO VarInfo

       module ESMF_CFIOVarInfoMod

!
! !DESCRIPTION:
!
! The code in this file provides data type definitions and interface
! specifications
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_CFIOUtilMod, only: MVARLEN, MLEN, iNode, rNode, cNode,   &
                                  addList, err
      use ESMF_CFIOGridMod

! !REVISION HISTORY:
!
!  Feb 2007    Yin     Separated from ESMF_CFIOMod
!
      implicit none
!------------------------------------------------------------------------------
! !PUBLIC DATA TYPES:
!
      public :: ESMF_CFIOVarInfo         ! A CFIO variable object

! !PUBLIC MEMBER FUNCTIONS:

      public :: ESMF_CFIOVarInfoCreate   ! constructor
      public :: ESMF_CFIOVarInfoSet      ! set info for a CFIOVarInfo object
      public :: ESMF_CFIOVarInfoGet      ! Get info from a CFIOVarInfo object
      public :: ESMF_CFIOVarInfoDestroy  ! destructor
!EOP
!------------------------------------------------------------------------------
! Define a new data type "CFIO_VarInfo" -- contains variable information

      type ESMF_CFIOVarInfo
!         private
         character(len=MVARLEN) :: vName       ! variable short name
         type(ESMF_CFIOGrid) :: grid           ! grid used for this var
         character(len=MLEN) :: vTitle         ! variable long name, e.g.,
                                               ! "Geopotential Height"
         character(len=MVARLEN):: vUnits       ! variable units, e.g.,
                                               ! "meter/second"
         real :: validRange(2)                 ! Variable valid range
         real :: packingRange(2)
         real :: amiss                         ! Missing value such as 1.0E15
         real :: addOffSet                     ! optional
         real :: scaleFactor                   ! optional
         character(len=MLEN) :: standardName   ! optional, standard name
                                               ! following CF convention
         logical :: twoDimVar     ! True for 2D; false for 3D
         logical :: timAve        ! True for time averaging file
         logical :: isGridSet     ! True only if grid was passed in
         character :: aveMethod   ! 'c' for center averaging for time
                                  ! [-0.5*timeInc+time, 0.5*timeInc+time]
                                  ! Default: 'c'
                                  ! 'd' for downstream averaging
                                  ! [time, time+timeInc]
                                  ! 'u' for upstream averaging
                                  ! [time-timeInc, time]
         character(len=MVARLEN) :: cellMthd   ! Cell methmod
         integer :: nVarAttInt    ! number of variable int attributes
         integer :: nVarAttReal   ! number of variable real attributes
         integer :: nVarAttChar   ! number of variable char attributes
         integer, pointer :: attCharCnts(:) => NULL()        ! length of char attributes
         integer, pointer :: attRealCnts(:) => NULL()        ! length of real attributes
         integer, pointer :: attIntCnts(:) => NULL()         ! length of int attributes
         character(len=MLEN), pointer :: attCharNames(:) => NULL() ! char attribute name
         character(len=MLEN), pointer :: attRealNames(:) => NULL() ! Real attribute name
         character(len=MLEN), pointer :: attIntNames(:) => NULL()  ! int attribute name
         character(len=MLEN), pointer :: varAttChars(:) => NULL()  ! char attributes
         real, pointer :: varAttReals(:,:) => NULL()    ! User defined real attributes
         integer, pointer :: varAttInts(:,:) => NULL()  ! User defined integer attributes
         character(len=MVARLEN) :: ordering ! (time, lev, lat, lon) (default)
                                            ! can be any combination of xyzt
         type(iNode), pointer :: iList=>NULL()
         type(rNode), pointer :: rList=>NULL()
         type(cNode), pointer :: cList=>NULL()
         integer, pointer  :: ChunkSize(:) => NULL()   ! ChunkSize for each variable

      end type ESMF_CFIOVarInfo

      contains

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarInfoCreate -- ESMF_CFIOVarInfo object constructor

! !INTERFACE:
      type(ESMF_CFIOVarInfo) function ESMF_CFIOVarInfoCreate (vName, rc)   
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      character(len=*), intent(in), OPTIONAL :: vName     ! variable name
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                                                ! -1  problem in creating Grid
!
! !DESCRIPTION:
!     Create a CFIO varInfo object and initialize variables
!EOP
!------------------------------------------------------------------------------
      type(ESMF_CFIOVarInfo) :: varObj ! a CFIO grid object
      integer :: rtcode = 0
      
      varObj%grid = ESMF_CFIOGridCreate(rc=rtcode)
      if (rtcode .ne. 0) then 
         print *, "problem in getting ESMF_CFIOGridCreate:lon"
         rtcode = -1
         if ( present(rc) ) rc = rtcode
         return
      end if

      varObj%nVarAttInt = 0
      varObj%nVarAttChar = 0
      varObj%nVarAttReal = 0

      varObj%twoDimVar = .false.
      varObj%timAve = .false.
      varObj%isGridSet = .false.
      varObj%aveMethod = 'c'
      varObj%cellMthd = 'mean'
      varObj%amiss = 1.E15
      varObj%addOffSet = 0
      varObj%scaleFactor = 1
      varObj%validRange = 1.E15
      varObj%packingRange = 1.E15
      varObj%ordering = 'tzyx'

      varObj%vTitle = 'unknown'
      varObj%vUnits = 'unknown'
      varObj%standardName = 'unknown'

!      allocate(varObj%iList, varObj%rList, varObj%cList)
      nullify(varObj%iList)
      nullify(varObj%rList)
      nullify(varObj%cList)

      if ( present(rc) ) rc = 0

      ESMF_CFIOVarInfoCreate = varObj

      end function ESMF_CFIOVarInfoCreate



!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarInfoSet  -- Set meta data for a CFIO variable

! !INTERFACE:
      subroutine ESMF_CFIOVarInfoSet (varObj, vName, grid, vTitle, vUnits,  &
                              twoDimVar, validRange, amiss, addOffSet,      &
                              scaleFactor, standardName, attCharNames,      &
                              vAttCharCnts, varAttChars, attRealNames,      &
                              vAttRealCnts, varAttReals, attIntNames,       &
                              vAttIntCnts, varAttInts, ordering,            &
                              attCharName, attChar, attRealName, attReal,   &
                              attIntName, attInt, packingRange, timAve,     &
                              aveMethod, cellMthd, ChunkSize, rc )
       implicit NONE

! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
       character(len=*), intent(in), OPTIONAL :: vName  ! variable name 
       type(ESMF_CFIOGrid), intent(in), OPTIONAL :: grid  ! grid 
       character(len=*), intent(in), OPTIONAL :: vTitle ! variable long name 
       character(len=*), intent(in), OPTIONAL :: vUnits ! variable units      
       logical, OPTIONAL :: twoDimVar                 ! True for 2D
       logical, OPTIONAL :: timAve                    ! True for time average
       character, OPTIONAL :: aveMethod  ! 'c': center, 'd': downstream
                                         ! 'u': upstream
       character(len=*), OPTIONAL :: cellMthd    ! Cell methmod units
       real, intent(in), OPTIONAL :: validRange(2)    ! Variable valid range
       real, intent(in), OPTIONAL :: packingRange(2)  ! Variable valid range
       real, intent(in), OPTIONAL :: amiss            ! FILL value
       real, intent(in), OPTIONAL :: addOffSet    
       real, intent(in), OPTIONAL :: scaleFactor
       character(len=*), intent(in), OPTIONAL :: standardName 

       character(len=*), intent(in), OPTIONAL :: attCharNames(:)
       character(len=*), intent(in), OPTIONAL :: attRealNames(:)
       character(len=*), intent(in), OPTIONAL :: attIntNames(:)

       integer, intent(in), OPTIONAL :: vAttCharCnts(:) ! length of attributes
       integer, intent(in), OPTIONAL :: vAttRealCnts(:) ! length of attributes
       integer, intent(in), OPTIONAL :: vAttIntCnts(:)  ! length of attributes

       character(len=*), intent(in), OPTIONAL :: varAttChars(:)
       real, intent(in), OPTIONAL :: varAttReals(:,:)
       integer, intent(in), OPTIONAL :: varAttInts(:,:)
       
       character(len=*), OPTIONAL :: ordering 
                                 ! (time, lev, lat, lon) (default)
                                 ! can be any combination of xyzt

       character(len=*), intent(in), OPTIONAL :: attCharName
                                 ! User defined variable attribute name
       character(len=*), intent(in), OPTIONAL :: attRealName
                                 ! User defined variable real attribute name
       character(len=*), intent(in), OPTIONAL :: attIntName
                                 ! User defined variable int attribute name
       character(len=*), intent(in), OPTIONAL :: attChar
                                 ! User defined variable char attribute
       real,    intent(in), OPTIONAL :: attReal(:)
                                 ! User defined variable real attribute
       integer, intent(in), OPTIONAL :: attInt(:)
                                 ! User defined variable int attribute
       integer, intent(in), OPTIONAL :: ChunkSize(:)
                                 ! User defined Chunksize int 

!
! !OUTPUT PARAMETERS:
!
       integer, intent(out), OPTIONAL :: rc   
                                 ! 0  all is well
                                 ! -1 Allocation for attCharCnts failed
                                 ! -2 Allocation for attRealCnts failed
                                 ! -3 Allocation for attIntCnts failed
                                 ! -4 Allocation for varAttChars failed
                                 ! -5 Allocation for varAttReals failed
                                 ! -6 Allocation for varAttInts failed
                                 ! -7 Allocation for attCharNames failed
                                 ! -8 Allocation for attRealNames failed
                                 ! -9 Allocation for attIntNames failed
                                               

! !INPUT/OUTPUT PARAMETERS:
!
       type(ESMF_CFIOVarInfo), intent(inout) :: varObj        ! variable obj 
 
!
! !DESCRIPTION:
!     Set meta data for a CFIO variable         
!EOP
!------------------------------------------------------------------------------
       integer :: iCnt, jCnt, count, rtcode = 0
                                                                  
       if ( present(vName) ) varObj%vName = vName
       if ( present(grid) ) then
          varObj%grid = grid 
          varObj%isGridSet = .true.
       end if
       if ( present(vTitle) ) varObj%vTitle = vTitle
       if ( present(vUnits) ) varObj%vUnits = vUnits
       if ( present(twoDimVar) ) varObj%twoDimVar = twoDimVar
       if ( present(timAve) ) varObj%timAve = timAve
       if ( present(aveMethod) ) varObj%aveMethod = aveMethod
       if ( present(cellMthd) ) varObj%cellMthd = trim(cellMthd) 
       if ( present(validRange) ) varObj%validRange = validRange
       if ( present(packingRange) ) varObj%packingRange = packingRange
       if ( present(amiss) ) varObj%amiss = amiss
       if ( present(addOffSet) ) varObj%addOffSet = addOffSet
       if ( present(scaleFactor) ) varObj%scaleFactor = scaleFactor
       if ( present(standardName) )  varObj%standardName = standardName
       if ( present(ordering) ) varObj%ordering = ordering 
       
!      user provide int/real/char attribute counts as arrays 
       if ( present(vAttCharCnts) ) then
          allocate(varObj%attCharCnts(size(vAttCharCnts)), stat=rtcode)
          if (err("Allocation for attCharCnts failed",rtcode,-1) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if

          varObj%attCharCnts = vAttCharCnts
       end if
       if ( present(vAttRealCnts) ) then
          allocate(varObj%attRealCnts(size(vAttRealCnts)), stat=rtcode)
          if (err("Allocation for attRealCnts failed",rtcode,-2) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%attRealCnts = vAttRealCnts
          varObj%nVarAttReal = size(vAttRealCnts)
       end if
       if ( present(vAttIntCnts) ) then
          allocate(varObj%attIntCnts(size(vAttIntCnts)), stat=rtcode)
          if (err("Allocation for attIntCnts failed",rtcode,-3) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%attIntCnts = vAttIntCnts
          varObj%nVarAttInt = size(vAttIntCnts)
       end if

!      user provide int/real/char attribute data as arrays 
       if ( present(varAttChars) ) then
          allocate(varObj%varAttChars(size(varAttChars)), stat=rtcode)
          if (err("Allocation for varAttChars failed",rtcode,-4) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%varAttChars = varAttChars
          varObj%nVarAttChar = size(vAttCharCnts)
       end if
       if ( present(varAttReals) ) then
          iCnt = size(varObj%attRealCnts)
          jCnt = size(varAttReals)/size(varObj%attRealCnts)
          allocate(varObj%varAttReals(iCnt, jCnt), stat=rtcode)
          if (err("Allocation for varAttReals failed",rtcode,-5) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%varAttReals = varAttReals
       end if
       if ( present(varAttInts) ) then
          iCnt = size(varObj%attIntCnts)
          jCnt = size(varAttInts)/size(varObj%attIntCnts)
          allocate(varObj%varAttInts(iCnt, jCnt), stat=rtcode)
          if (err("Allocation for varAttInts failed",rtcode,-6) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%varAttInts = varAttInts  
       end if
    
!      user provide int/real/char attribute names as arrays 
       if ( present(attCharNames)) then
          allocate(varObj%attCharNames(size(attCharNames)), stat=rtcode)
          if (err("Allocation for attCharNames failed",rtcode,-7) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%attCharNames = attCharNames
       end if
       if ( present(attRealNames)) then
          allocate(varObj%attRealNames(size(attRealNames)), stat=rtcode)
          if (err("Allocation for attRealNames failed",rtcode,-8) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%attRealNames = attRealNames
       end if
       if ( present(attIntNames)) then
          allocate(varObj%attIntNames(size(attIntNames)), stat=rtcode)
          if (err("Allocation for attIntNames failed",rtcode,-9) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%attIntNames = attIntNames
       end if

!      set ChunkSize array
       if( present(ChunkSize) ) then
          allocate(varObj%ChunkSize(size(ChunkSize)), stat=rtcode)
          if (err("can't allocate mem: ChunkSize",rtcode,-12) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if
          varObj%ChunkSize=ChunkSize
       end if


!      user provides real attribute name and data. Put them into rList
       if ( present(attRealName) .and. present(attReal) ) then
          count = size(attReal)
          call addList(attRealName, count, attReal=attReal, &
                       rList=varObj%rList)
       end if

!      user provides int attribute name and data. Put them into iList
       if ( present(attIntName) .and. present(attInt) ) then
          count = size(attInt)
          call addList(attIntName, count, attInt=attInt, &
                       iList=varObj%iList)
       end if

!      user provides char attribute name and data. Put them into cList
       if ( present(attCharName) .and. present(attChar) ) then
          call addList(attCharName, len(attChar), attChar=attChar, &
                       cList=varObj%cList)
       end if

       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOVarInfoSet 


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarInfoGet -- get information from a CFIO variable object

! !INTERFACE:
      subroutine ESMF_CFIOVarInfoGet (varObj, vName, grid, vTitle, vUnits,  &
                              twoDimVar, validRange, amiss, addOffSet,      &
                              scaleFactor, standardName, nVarAttChar,       &
                              attCharNames,  vAttCharCnts, varAttChars,     &
                              nVarAttReal, attRealNames, vAttRealCnts,      &
                              varAttReals, nVarAttInt, attIntNames,         &
                              vAttIntCnts, varAttInts, ordering,            &
                              attCharName, attCharCnt, attChar, attRealName,&
                              attRealCnt, attReal, attIntName, attIntCnt,   &
                              attInt, packingRange, timAve, aveMethod,      &
                              cellMthd, ChunkSize, rc )
       implicit NONE

! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
       type(ESMF_CFIOVarInfo), intent(in) :: varObj      ! variable obj 
       character(len=*), intent(in), OPTIONAL :: attCharName
                                    ! User defined  char attribute name
       character(len=*), intent(in), OPTIONAL :: attRealName
                                    ! User defined  real attribute name
       character(len=*), intent(in), OPTIONAL :: attIntName
                                    ! User defined  int attribute name


! !OUTPUT PARAMETERS:
!
       character(len=*), intent(out), OPTIONAL :: vName  ! variable short name
       type(ESMF_CFIOGrid), intent(out), OPTIONAL :: grid  ! grid 
       character(len=*), intent(out), OPTIONAL :: vTitle ! variable long name 
       character(len=*), intent(out), OPTIONAL :: vUnits ! variable units      
       logical, OPTIONAL :: twoDimVar                  ! True for 2D
       logical, OPTIONAL :: timAve                    ! True for time average
       character, OPTIONAL :: aveMethod  ! 'c': center, 'd': downstream
                                         ! 'u': upstream
       character(len=MVARLEN), OPTIONAL :: cellMthd    ! Cell methmod units
       real, intent(out), OPTIONAL :: validRange(2)    ! Variable valid range
       real, intent(out), OPTIONAL :: packingRange(2)
       real, intent(out), OPTIONAL :: amiss            ! FILL value
       real, intent(out), OPTIONAL :: addOffSet    
       real, intent(out), OPTIONAL :: scaleFactor
       character(len=*), intent(out), OPTIONAL :: standardName 
       character(len=*), OPTIONAL :: ordering
                                 ! (time, lev, lat, lon) (default)
                                 ! can be any combination of xyzt
       integer, intent(out), OPTIONAL :: nVarAttInt
       integer, intent(out), OPTIONAL :: nVarAttReal
       integer, intent(out), OPTIONAL :: nVarAttChar

       character(len=*), pointer, OPTIONAL :: attCharNames(:)
       character(len=*), pointer, OPTIONAL :: attRealNames(:)
       character(len=*), pointer, OPTIONAL :: attIntNames(:)

       integer, pointer,OPTIONAL::vAttCharCnts(:) ! length of attributes
       integer, pointer,OPTIONAL::vAttRealCnts(:) ! length of attributes
       integer, pointer,OPTIONAL::vAttIntCnts(:)  ! length of attributes

       character(len=*), pointer, OPTIONAL :: varAttChars(:)
       real, pointer, OPTIONAL :: varAttReals(:,:)
       integer, pointer, OPTIONAL :: varAttInts(:,:)
                                                                                       
       integer, intent(out), OPTIONAL :: attIntCnt
       integer, intent(out), OPTIONAL :: attRealCnt
       integer, intent(out), OPTIONAL :: attCharCnt
       character(len=*), intent(out), OPTIONAL :: attChar
                                    ! User defined  char attribute
       real,    pointer, OPTIONAL :: attReal(:)
                                    ! User defined  real attribute
       integer, pointer, OPTIONAL :: attInt(:)
                                    ! User defined  int attribute
       integer, pointer, OPTIONAL :: ChunkSize(:)
                                 ! User defined Chunksize int 

       integer, intent(out), OPTIONAL :: rc 
                                    ! Error return code:
                                    ! 0   all is well
                                    ! -1  Allocation for attCharNames failed
                                    ! -2  Allocation for attRealNames failed
                                    ! -3  Allocation for attIntNames failed 
                                    ! -4  Allocation for vAttCharCnts failed
                                    ! -5  Allocation for vAttRealCnts failed 
                                    ! -6  Allocation for vAttIntCnts failed 
                                    ! -7  Allocation for varAttChars failed 
                                    ! -8  Allocation for varAttReals failed 
                                    ! -9  Allocation for varAttInts failed  

!
! !DESCRIPTION:
!     get information from a CFIO variable object
!EOP
!------------------------------------------------------------------------------
       integer :: rtcode = 0
       integer :: i
                                                                                     
       if ( present(vName) ) vName = varObj%vName
       if ( present(grid) ) grid = varObj%grid 
       if ( present(vTitle) ) vTitle = varObj%vTitle 
       if ( present(vUnits) ) vUnits = varObj%vUnits
       if ( present(twoDimVar) ) twoDimVar = varObj%twoDimVar 
       if ( present(timAve) ) timAve = varObj%timAve
       if ( present(aveMethod) ) aveMethod = varObj%aveMethod
       if ( present(cellMthd) ) cellMthd = varObj%cellMthd  
       if ( present(validRange) ) validRange = varObj%validRange 
       if ( present(packingRange) ) packingRange = varObj%packingRange
       if ( present(amiss) ) amiss = varObj%amiss 
       if ( present(addOffSet) ) addOffSet = varObj%addOffSet
       if ( present(scaleFactor) ) scaleFactor = varObj%scaleFactor 
       if ( present(standardName) )  standardName = varObj%standardName 
       if ( present(ordering) ) ordering = varObj%ordering 
                                                                                     
       if ( present(nVarAttInt) ) nVarAttInt = varObj%nVarAttInt
       if ( present(nVarAttReal) ) nVarAttReal = varObj%nVarAttReal
       if ( present(nVarAttChar) ) nVarAttChar = varObj%nVarAttChar

!      get all attribute names
       if ( present(attCharNames) ) then
          allocate(attCharNames(varObj%nVarAttChar), stat=rtcode)
          if (err("Allocation for attCharNames failed",rtcode,-1) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          attCharNames = varObj%attCharNames
       end if
       if ( present(attRealNames) ) then
          allocate(attRealNames(varObj%nVarAttReal), stat=rtcode)
          if (err("Allocation for attRealNames failed",rtcode,-2) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          attRealNames = varObj%attRealNames
       end if
       if ( present(attIntNames) ) then
          allocate(attIntNames(varObj%nVarAttInt), stat=rtcode)
          if (err("Allocation for attIntNames failed",rtcode,-3) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          attIntNames = varObj%attIntNames
       end if

!      get all attribute counts
       if ( present(vAttCharCnts) ) then
          allocate(vAttCharCnts(varObj%nVarAttChar), stat=rtcode)
          if (err("Allocation for vAttCharCnts failed",rtcode,-4) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          vAttCharCnts = varObj%attCharCnts
       end if
       if ( present(vAttRealCnts) ) then
          allocate(vAttRealCnts(varObj%nVarAttReal), stat=rtcode)
          if (err("Allocation for vAttRealCnts failed",rtcode,-5) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          vAttRealCnts = varObj%attRealCnts
       end if
       if ( present(vAttIntCnts) ) then
          allocate(vAttIntCnts(varObj%nVarAttInt), stat=rtcode)
          if (err("Allocation for vAttIntCnts failed",rtcode,-6) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          vAttIntCnts = varObj%attIntCnts
       end if

!      get all attribute data   
       if ( present(varAttChars) ) then 
          allocate(varAttChars(varObj%nVarAttChar), stat=rtcode)
          if (err("Allocation for varAttChars failed",rtcode,-7) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varAttChars = varObj%varAttChars 
       end if
       if ( present(varAttReals) ) then
          allocate(varAttReals(varObj%nVarAttReal,size(varObj%varAttReals) &
                   / varObj%nVarAttReal), stat=rtcode)
          if (err("Allocation for varAttReals failed",rtcode,-8) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varAttReals = varObj%varAttReals 
       end if
       if ( present(varAttInts) ) then 
          allocate(varAttInts(varObj%nVarAttInt,size(varObj%varAttInts) &
                   / varObj%nVarAttInt), stat=rtcode)
          if (err("Allocation for varAttInts failed",rtcode,-9) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          varAttInts = varObj%varAttInts  
       end if

!      Get Chunk Size
       if ( present(ChunkSize) ) then
          if(associated(varObj%ChunkSize))  then 
             allocate( ChunkSize(size(varObj%ChunkSize)), stat = rtcode )
             if (err("Allocation for ChunkSize fialed", rtcode, -10) .lt. 0) then
                if ( present(rc) ) rc = rtcode
                return
             end if
             ChunkSize = varObj%ChunkSize
          end if
       end if

!      user provides integer attribute name to get its count and data 
       if ( present(attIntName) ) then
          if ( present(attIntCnt) ) then
             do i = 1, varObj%nVarAttInt
                if (trim(attIntName) .eq. trim(varObj%attIntNames(i))) &
                    then
                   attIntCnt = varObj%attIntCnts(i)
                end if
             end do
          end if
          if ( present(attInt) ) then
             do i = 1, varObj%nVarAttInt
                if (trim(attIntName) .eq. trim(varObj%attIntNames(i)))&
                    then
                   allocate(attInt(varObj%attIntCnts(i)))
                   attInt = varObj%varAttInts(i,1:varObj%attIntCnts(i))
                end if
             end do
          end if
       end if

!      user provides real attribute name to get its count and data 
       if ( present(attRealName) ) then
          if ( present(attRealCnt) ) then
             do i = 1, varObj%nVarAttReal
                if (trim(attRealName) .eq. trim(varObj%attRealNames(i))) &
                    then
                   attRealCnt = varObj%attRealCnts(i)
                end if
             end do
          end if
          if ( present(attReal) ) then
             do i = 1, varObj%nVarAttReal
                if (trim(attRealName) .eq. trim(varObj%attRealNames(i)))&
                    then
                   allocate(attReal(varObj%attRealCnts(i)))
                   attReal = varObj%varAttReals(i,1:varObj%attRealCnts(i))
                end if
             end do
          end if
       end if

!      user provides char attribute name to get its count and data 
       if ( present(attCharName) ) then
          if ( present(attCharCnt) ) then
             do i = 1, varObj%nVarAttChar
                if (trim(attCharName) .eq. trim(varObj%attCharNames(i))) &
                    then
                   attCharCnt = varObj%attCharCnts(i)
                end if
             end do
          end if
          if ( present(attChar) ) then
             do i = 1, varObj%nVarAttChar
                if (trim(attCharName) .eq. trim(varObj%attCharNames(i)))&
                    then
                   attChar = trim(varObj%varAttChars(i))
                end if
             end do
          end if
       end if

       if ( present(rc) ) rc = rtcode


      end subroutine ESMF_CFIOVarInfoGet

!------------------------------------------------------------------------------!BOP
! !ROUTINE: ESMF_CFIOVarInfoDestroy -- destructor for a CFIO varInfo object

! !INTERFACE:
      subroutine ESMF_CFIOVarInfoDestroy (varObj, rc)
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
      type(ESMF_CFIOVarInfo), intent(inout) :: varObj ! CFIOVarInfo object

!
! !DESCRIPTION:
!     destructor for a CFIO varInfo object
!EOP
!------------------------------------------------------------------------------
      integer :: rtcode = 0

      if ( associated(varObj%attCharCnts) ) deallocate(varObj%attCharCnts, &
                                             stat=rtcode)
      if ( associated(varObj%attRealCnts) ) deallocate(varObj%attRealCnts, &
                                             stat=rtcode)
      if ( associated(varObj%attIntCnts) ) deallocate(varObj%attIntCnts,   &
                                             stat=rtcode)

      if ( associated(varObj%attCharNames) ) deallocate(varObj%attCharNames,&
                                             stat=rtcode)
      if ( associated(varObj%attRealNames) ) deallocate(varObj%attRealNames,&
                                             stat=rtcode)
      if ( associated(varObj%attIntNames) ) deallocate(varObj%attIntNames,  &
                                             stat=rtcode)

      if ( associated(varObj%varAttChars) ) deallocate(varObj%varAttChars,  &
                                             stat=rtcode)
      if ( associated(varObj%varAttReals) ) deallocate(varObj%varAttReals,  &
                                             stat=rtcode)
      if ( associated(varObj%varAttInts) ) deallocate(varObj%varAttInts,    &
                                             stat=rtcode)

      if ( associated(varObj%ChunkSize ) ) deallocate(varObj%ChunkSize,       &
                                             stat=rtcode)

      if (.not. varObj%isGridSet) then
         call ESMF_CFIOGridDestroy(varObj%grid, rc=rtcode)
      end if
      
      if ( associated(varObj%iList) ) call iNodeDestroy(varObj%iList)
      if ( associated(varObj%rList) ) call rNodeDestroy(varObj%rList)
      if ( associated(varObj%cList) ) call cNodeDestroy(varObj%cList)

      if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOVarInfoDestroy

   subroutine iNodeDestroy(List)
     type(iNode), pointer :: List, p, q

     if (.not. associated(List)) return
     q => List
     p => List%next
     do while ( associated(p) )  
        if (associated(q)) then
           if (associated(q%intData)) deallocate(q%intData)
           deallocate(q)
        end if
        q => p
        p => p%next
     end do

     if (associated(q)) then 
        if (associated(q%intData)) deallocate(q%intData)
        deallocate(q)
     end if
   end subroutine iNodeDestroy

   subroutine rNodeDestroy(List)
     type(rNode), pointer :: List, p, q

     if (.not. associated(List)) return
     q => List
     p => List%next
     do while ( associated(p) )  
        if (associated(q)) then
           if (associated(q%realData)) deallocate(q%realData)
           deallocate(q)
        end if
        q => p
        p => p%next
     end do

     if (associated(q)) then 
        if (associated(q%realData)) deallocate(q%realData)
        deallocate(q)
     end if

   end subroutine rNodeDestroy

   subroutine cNodeDestroy(List)
     type(cNode), pointer :: List, p, q

     if (.not. associated(List)) return
     q => List
     p => List%next
     do while ( associated(p) )  
        if (associated(q)) then
           deallocate(q)
        end if
        q => p
        p => p%next
     end do

     if (associated(q)) then 
        deallocate(q)
     end if

   end subroutine cNodeDestroy
 end module ESMF_CFIOVarInfoMod
