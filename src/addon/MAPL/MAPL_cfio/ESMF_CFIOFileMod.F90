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
!==============================================================================
!BOP
! !MODULE: ESMF_CFIOFileMod.F90 - Source file for CFIO File

       module ESMF_CFIOFileMod

!
! !DESCRIPTION:
!
! The code in this file provides ESMF_CFIOFile type definitions and interface
! specifications
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_CFIOUtilMod
      use ESMF_CFIOGridMod
      use ESMF_CFIOVarInfoMod
      use ESMF_CFIOwGrADSMod, only : CFIO_wGrADS
      use ESMF_CFIOrGrADSMod, only : CFIO_rGrADS

! !REVISION HISTORY:
!
!  Feb 2007    Yin     Separated from ESMF_CFIOMod
!
      implicit none
!------------------------------------------------------------------------------
! !PUBLIC DATA TYPES:
!
      public :: ESMF_CFIO                ! A CFIO File object

! !PUBLIC MEMBER FUNCTIONS:

      public :: ESMF_CFIOCreate          ! constructor for a CFIO object
      public :: ESMF_CFIOSet             ! set meta data for a CFIO object
      public :: ESMF_CFIOGet             ! Get meta data
      public :: ESMF_CFIODestroy         ! destructor for a CFIO object

!EOP
!------------------------------------------------------------------------------
! Define a new data type "ESMF_CFIO" -- a CFIO object(file) with file name,
! CFIO variable objects, time, grid index and global attributes.

      type ESMF_CFIO
!         private
         character(len=MLEN) :: cfioObjName   ! name for this CFIO object
         character(len=MLEN) :: fName         ! file name in this CFIO obj.
         character(len=MLEN) :: fNameTmplt    ! file name in this CFIO obj.
         character(len=MLEN) :: expid         ! Experiment I
         integer :: mVars                     ! total number of variables
         type(ESMF_CFIOVarInfo), pointer :: varObjs(:)=>null() ! CFIO variable objects
         integer :: mGrids                    ! total number of grids
         type(ESMF_CFIOGrid), pointer :: grids(:)=>null()     ! CFIO variable grid
         integer :: date                      ! yyyymmdd
         integer :: begTime                   ! hhmmss
         integer :: timeInc                   ! time step increment
         integer :: tSteps                    ! total time steps
         integer :: deflate                   ! gzip compress level
         character(len=MLEN) :: title    ! A title for the data set
         character(len=MLEN) :: source   ! Source of data, e.g. NASA/GMAO
         character(len=MLEN) :: contact  ! Who to contact about the data set
         character(len=MLEN) :: history  !
         character(len=MLEN) :: convention ! CFIO
         character(len=MLEN) :: institution
         character(len=MLEN) :: references
         character(len=MLEN) :: comment

         integer :: nAttChar ! Number of char attributes
         integer :: nAttReal ! Number of Real attributes
         integer :: nAttInt  ! Number of int attributes
         integer, pointer :: attCharCnts(:)       ! length of char attributes
         integer, pointer :: attRealCnts(:)       ! length of real attributes
         integer, pointer :: attIntCnts(:)        ! length of int attributes
         character(len=MLEN), pointer :: attCharNames(:)! User defined char
                                                       ! attribute name
         character(len=MLEN), pointer :: attRealNames(:)! Real attribute name
         character(len=MLEN), pointer :: attIntNames(:) ! int attribute name
         character(len=MLEN), pointer :: attChars(:) ! char attributes
         real, pointer :: attReals(:,:)           ! global real attributes
         integer, pointer :: attInts(:,:)         ! global integer attributes

         integer :: prec                         ! Desired precision of data
         integer :: fid                          ! file ID for internal use
         integer :: sd_id                        ! file ID for EOS
         logical :: isGridSet           ! True only if grid was passed in
         type(iNode), pointer :: iList
         type(rNode), pointer :: rList
         type(cNode), pointer :: cList
         logical :: isOpen              ! flag to check fName is opened or not
!         integer :: nSteps
         logical :: isCyclic            ! flag for cyclic for input files
         character(len=16) :: format    ! output/input format -- GrADS or SDF(HDF)
                                        ! default is SDF.
         type(CFIO_wGrADS) :: gw
         type(CFIO_rGrADS) :: gr
      end type ESMF_CFIO

!
!EOP
      contains

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOCreate -- ESMF_CFIO object constructor   

! !INTERFACE:
      type (ESMF_CFIO) function ESMF_CFIOCreate (cfioObjName, rc) 
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      character(len=*), intent(in), OPTIONAL :: cfioObjName  ! object name
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
!
! !DESCRIPTION:
!     Create a CFIO object and initialize vars . The required global metadata
!     title, institution, source, history, references, and comment are set to 
!     unknown.
!EOP
!------------------------------------------------------------------------------
      type(ESMF_CFIO) :: cfio                   ! a CFIO object
      integer :: rtcode

      if ( present(cfioObjName) ) then 
         cfio%cfioObjName = cfioObjName
      else
         cfio%cfioObjName = 'CFIO'
      end if

! Initializing variables

      cfio%nAttChar = 0
      cfio%nAttReal = 0
      cfio%nAttInt = 0

      cfio%fName = 'unknown'
      cfio%title = 'unknown'
      cfio%source = 'unknown'
      cfio%contact = 'unknown'
      cfio%history = 'unknown'
      cfio%convention = 'unknown'
      cfio%institution = 'unknown'
      cfio%references = 'unknown'
      cfio%comment = 'unknown'
      cfio%prec = 0
      cfio%date = -999
      cfio%begTime = 0
      cfio%timeInc = 60000
      cfio%mVars = 1
      cfio%mGrids = 1
      cfio%fNameTmplt = ''
      cfio%isOpen = .false.
      cfio%isCyclic = .false.
      cfio%isGridSet = .false.
      cfio%format = 'SDF'
      cfio%expid = ''
!      allocate(cfio%iList, cfio%rList, cfio%cList)
!      nullify(cfio%iList)
!      nullify(cfio%rList)
!      nullify(cfio%cList)

      rtcode = 0

      if ( present(rc) ) rc = rtcode

      ESMF_CFIOCreate = cfio

      end function ESMF_CFIOCreate

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSet  -- Set meta data for a CFIO object

! !INTERFACE:
      subroutine ESMF_CFIOSet(cfio, cfioObjName, varObjs, grids, grid,      &
                              fName, title, source, contact, history,       &
                              convention, institution, references, comment, &
                              date, begTime, timeInc, timeString, prec,     &
                              attCharNames, attCharCnts, attChars,          &
                              attRealNames, attRealCnts, attReals,          &
                              attIntNames, attIntCnts, attInts,             &
                              attCharName, attChar, attRealName, attReal,   &
                              attIntName, attInt, gw, gr, format,           &
                              expid, isCyclic, isOpen, nSteps, fNameTmplt,  &
                              deflate,                                   rc )
       implicit NONE

! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
       character(len=*), intent(in), OPTIONAL :: cfioObjName ! object name
       type(ESMF_CFIOVarInfo), OPTIONAL :: varObjs(:)! variable objects 
       type(ESMF_CFIOGrid), OPTIONAL :: grids(:)     ! grid array
       type(ESMF_CFIOGrid), OPTIONAL :: grid         

       character(len=*), intent(in), OPTIONAL :: fName      ! File name
       character(len=*), intent(in), OPTIONAL :: fNameTmplt ! File name
       character(len=*), intent(in), OPTIONAL :: title      
       character(len=*), intent(in), OPTIONAL :: source     ! Source of data
       character(len=*), intent(in), OPTIONAL :: contact    ! Who to contact 
       character(len=*), intent(in), OPTIONAL :: history    !
       character(len=*), intent(in), OPTIONAL :: convention ! CFIO or COARDS
       character(len=*), intent(in), OPTIONAL :: institution! File name
       character(len=*), intent(in), OPTIONAL :: references
       character(len=*), intent(in), OPTIONAL :: comment

       integer, intent(in), OPTIONAL :: date          ! yyyymmdd
       integer, intent(in), OPTIONAL :: begTime       ! hhmmss
       integer, intent(in), OPTIONAL :: timeInc       ! time step increment
       character(len=*), intent(in), OPTIONAL :: timeString 
                                ! string expression of date and time  
       integer, intent(in), OPTIONAL :: prec      ! Desired precision of data:
                                                  ! 0 = 32 bit; 1 = 64 bit

       character(len=*), intent(in), OPTIONAL :: attCharNames(:) 
                                    ! User defined global char attribute names
       character(len=*), intent(in), OPTIONAL :: attRealNames(:)
                                    ! User defined global real attribute names 
       character(len=*), intent(in), OPTIONAL :: attIntNames(:)
                                    ! User defined global int attribute names
       integer, intent(in), OPTIONAL :: attCharCnts(:)! length of attributes
       integer, intent(in), OPTIONAL :: attRealCnts(:)! length of attributes
       integer, intent(in), OPTIONAL :: attIntCnts(:) ! length of attributes

       character(len=*), intent(in), OPTIONAL :: attChars(:) 
                                    ! User defined global char attribute 
       real,      intent(in), OPTIONAL :: attReals(:,:) 
                                    ! User defined global real attribute 
       integer,   intent(in), OPTIONAL :: attInts(:,:)
                                    ! User defined global int attribute 

       character(len=*), intent(in), OPTIONAL :: attCharName 
                                    ! User defined global char attribute name
       character(len=*), intent(in), OPTIONAL :: attRealName
                                    ! User defined global real attribute name
       character(len=*), intent(in), OPTIONAL :: attIntName
                                    ! User defined global int attribute name
       character(len=*), intent(in), OPTIONAL :: attChar 
                                    ! User defined global char attribute 
       real,    intent(in), OPTIONAL :: attReal(:)
                                    ! User defined global real attribute 
       integer, intent(in), OPTIONAL :: attInt(:)
                                    ! User defined global int attribute 
       type(CFIO_wGrADS), intent(in), OPTIONAL :: gw
       type(CFIO_rGrADS), intent(in), OPTIONAL :: gr
       character(len=*), intent(in), OPTIONAL :: format
       character(len=*), intent(in), OPTIONAL :: expid
       logical, intent(in), OPTIONAL :: isCyclic
       logical, intent(in), OPTIONAL :: isOpen
       integer, intent(in), OPTIONAL :: nSteps
       integer, intent(in), OPTIONAL :: deflate
!
! !OUTPUT PARAMETERS:
!
       integer, intent(out), OPTIONAL :: rc 
                                    ! Error return code:
                                    ! 0   all is well
                                    ! -1  can't allocate memory for grid(s)
                                    ! -2  can't allocate memory: varObjs    
                                    ! -3  can't allocate mem: attIntCnts   
                                    ! -4  can't allocate mem: attIntNames  
                                    ! -5  can't allocate memory: attInts    
                                    ! -6  can't allocate mem: attRealCnts   
                                    ! -7  can't allocate mem: attRealNames  
                                    ! -8  can't allocate memory: attReals  
                                    ! -9  can't allocate mem: attCharCnts  
                                    ! -10  can't allocate mem: attCharNames  
                                    ! -11  can't allocate memory: attChars  
! !INPUT/OUTPUT PARAMETERS:
!
       type(ESMF_CFIO), intent(inout) :: cfio    ! a CFIO object
!
! !DESCRIPTION:
!     Set meta data for a CFIO object with detailed information. 
!EOP
!------------------------------------------------------------------------------
       integer :: iCnt, jCnt, count, rtcode

!      set required global meta data

       if ( present(cfioObjName) ) cfio%cfioObjName = cfioObjName
       if ( present(fName) ) cfio%fName = fName
       if ( present(fNameTmplt) ) cfio%fNameTmplt = fNameTmplt
       if ( present(title) ) cfio%title = title
       if ( present(source) ) cfio%source = source
       if ( present(contact) ) cfio%contact = contact
       if ( present(history) ) cfio%history = history
       if ( present(convention) ) cfio%convention = convention
       if ( present(institution) ) cfio%institution = institution
       if ( present(references) ) cfio%references = references
       if ( present(comment) ) cfio%comment = comment
       if ( present(date) ) cfio%date = date    
       if ( present(begTime) ) cfio%begTime = begTime 
       if ( present(timeInc) ) cfio%timeInc = timeInc 
       if ( present(gw) ) then
          cfio%gw = gw
       end if
       if ( present(gr) ) then
          cfio%gr = gr
       end if
       if ( present(format) ) cfio%format = format
       if ( present(expid) ) cfio%expid = expid
       if ( present(isCyclic) ) cfio%isCyclic = isCyclic
       if ( present(isOpen) ) cfio%isOpen = isOpen
       if ( present(nSteps) ) cfio%tSteps = nSteps
       if ( present(deflate) ) cfio%deflate = deflate

       if ( present(timeString) ) then
          call strToInt(timeString, cfio%date, cfio%begTime)
       end  if
       if ( present(prec) ) cfio%prec = prec

!      set grid information
       if ( present(grids) ) then
          cfio%mGrids = size(grids) 
          allocate( cfio%grids(cfio%mGrids), stat = rtcode)
          if (err("can't allocate memory for grids",rtcode,-1) .lt. 0 ) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          cfio%grids = grids   
          cfio%isGridSet = .true.
       end if
       if ( present(grid) ) then
          cfio%mGrids = 1
          allocate( cfio%grids(cfio%mGrids), stat = rtcode)
          if (err("can't allocate memory for grid",rtcode,-1) .lt. 0 ) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          cfio%grids = grid   
          cfio%isGridSet = .true.
       end if

!      set variable
       if ( present(varObjs) ) then
          cfio%mVars = size(varObjs) 
          allocate( cfio%varObjs(cfio%mVars), stat = rtcode)
          if (err("can't allocate memory: varObjs",rtcode,-2) .lt. 0 ) then
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%varObjs = varObjs 
       end if

!      set integer names, counts and data
       if ( present(attIntCnts) )  then 
          allocate(cfio%attIntCnts(size(attIntCnts)), stat=rtcode)
          if (err("can't allocate mem: attIntCnts",rtcode,-3) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attIntCnts = attIntCnts 
          cfio%nAttInt = size(attIntCnts)
       end if
       if ( present(attIntNames) )  then 
          cfio%nAttInt = size(attIntNames)
          allocate(cfio%attIntNames(cfio%nAttInt), stat=rtcode)
          if (err("can't allocate mem: attIntNames",rtcode,-4) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attIntNames = attIntNames
       end if
       if ( present(attInts) )  then 
          iCnt = size(cfio%attIntCnts)
          jCnt = size(attInts)/size(cfio%attIntCnts)
          allocate(cfio%attInts(iCnt, jCnt), stat=rtcode)
          rtcode = err("can't allocate memory for attInts", rtcode, -1)
          if (err("can't allocate memory: attInts",rtcode,-5) .lt. 0) then   
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attInts= attInts
       end if

!      set real names, counts and data with array
       if ( present(attRealCnts) )  then 
          allocate(cfio%attRealCnts(size(attRealCnts)), stat=rtcode)
          rtcode = err("can't allocate memory for attRealCnts", rtcode, -1)
          if (err("can't allocate mem: attRealCnts",rtcode,-6) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attRealCnts = attRealCnts 
          cfio%nAttReal = size(attRealCnts)
       end if
       if ( present(attRealNames) )  then 
          cfio%nAttReal = size(attRealNames)
          allocate(cfio%attRealNames(cfio%nAttReal), stat=rtcode)
          if (err("can't allocate mem: attRealNames",rtcode,-7) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attRealNames = attRealNames
       end if
       if ( present(attReals) )  then 
          iCnt = size(cfio%attRealCnts)
          jCnt = size(attReals)/size(cfio%attRealCnts)
          allocate(cfio%attReals(iCnt, jCnt), stat=rtcode)
          if (err("can't allocate memory: attReals",rtcode,-8) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attReals= attReals
       end if

!      set character names, counts and data with array
       if ( present(attCharCnts) )  then 
          allocate(cfio%attCharCnts(size(attCharCnts)), stat=rtcode)
          if (err("can't allocate mem: attCharCnts",rtcode,-9) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attCharCnts = attCharCnts 
          cfio%nAttChar = size(attCharCnts)
       end if
       if ( present(attCharNames) )  then 
          cfio%nAttChar = size(attCharNames)
          allocate(cfio%attCharNames(cfio%nAttChar), stat=rtcode)
          if (err("can't allocate mem: attCharNames",rtcode,-10) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attCharNames = attCharNames
       end if
       if ( present(attChars) )  then 
          allocate(cfio%attChars(cfio%nAttChar), stat=rtcode)
          if (err("can't allocate memory: attChars",rtcode,-11) .lt. 0) then   
             if ( present(rc) ) rc = rtcode
             return
          end if

          cfio%attChars= attChars
       end if

!      set integer name, count and data into a list
       if ( present(attRealName) .and. present(attReal) ) then
          count = size(attReal)
          call addList(attRealName, count, attReal=attReal, &
                       rList=cfio%rList)
       end if

!      set real attribute name, count and data into a list
       if ( present(attIntName) .and. present(attInt) ) then
          count = size(attInt)
          call addList(attIntName, count, attInt=attInt, &
                       iList=cfio%iList)
       end if

!      set character attribute name, count and data into a list
       if ( present(attCharName) .and. present(attChar) ) then
          call addList(attCharName, len(attChar), attChar=attChar, &
                       cList=cfio%cList)
       end if

       rtcode = 0
       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOSet


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGet -- Get meta data from a CFIO object

! !INTERFACE:
      subroutine ESMF_CFIOGet (cfio, cfioObjName, nVars, varObjs, grid,     &
                              nGrids, grids, fName, title, source, contact, &
                              history, convention, institution, references, &
                              comment, date, begTime, timeInc, nSteps, prec,&
                              attCharNames, nAttChar, attCharCnts, attChars,&
                              attRealNames, nAttReal, attRealCnts, attReals,&
                              attIntNames, nAttInt, attIntCnts, attInts,    &
                              attCharName, attCharCnt, attChar, attRealName,&
                              attRealCnt, attReal, attIntName, attIntCnt,   &
                              attInt, gw, gr, isOpen, format, fNameTmplt, rc )
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
       type(ESMF_CFIO), intent(in) :: cfio       ! a CFIO object
       character(len=*), intent(in), OPTIONAL :: attCharName
                                    ! User defined global char attribute name
       character(len=*), intent(in), OPTIONAL :: attRealName
                                    ! User defined global real attribute name
       character(len=*), intent(in), OPTIONAL :: attIntName
                                    ! User defined global int attribute name
!
! !OUTPUT PARAMETERS:
!
       character(len=*), intent(out), OPTIONAL :: cfioObjName ! CFIO Obj name
       integer, OPTIONAL :: nVars          ! number of variable objects
       type(ESMF_CFIOVarInfo), pointer, OPTIONAL :: varObjs(:)! var objects
       integer, OPTIONAL :: nGrids                   ! number of grids
       type(ESMF_CFIOGrid), pointer, OPTIONAL :: grids(:)    ! grid array
       type(ESMF_CFIOGrid), pointer, OPTIONAL :: grid
       character(len=*), intent(out), OPTIONAL :: fName      ! File name
       character(len=*), intent(out), OPTIONAL :: fNameTmplt ! File name
       character(len=*), intent(out), OPTIONAL :: title
       character(len=*), intent(out), OPTIONAL :: source     ! Source of data
       character(len=*), intent(out), OPTIONAL :: contact    ! Who to contact
       character(len=*), intent(out), OPTIONAL :: history    !
       character(len=*), intent(out), OPTIONAL :: convention ! CFIO or COARDS
       character(len=*), intent(out), OPTIONAL :: institution! File name
       character(len=*), intent(out), OPTIONAL :: references
       character(len=*), intent(out), OPTIONAL :: comment
       integer, intent(out), OPTIONAL :: date          ! yyyymmdd
       integer, intent(out), OPTIONAL :: begTime       ! hhmmss
       integer, intent(out), OPTIONAL :: timeInc      ! time step increment
       integer, intent(out), OPTIONAL :: nSteps       ! number of time steps
       integer, intent(out), OPTIONAL :: prec     ! Desired precision of data:
                                                  ! 0 = 32 bit; 1 = 64 bit
       integer, intent(out), OPTIONAL :: nAttChar ! Number of char attributes
       integer, intent(out), OPTIONAL :: nAttReal ! Number of Real attributes
       integer, intent(out), OPTIONAL :: nAttInt  ! Number of int attributes
       character(len=*), pointer, OPTIONAL :: attCharNames(:)
                                    ! User defined global char attribute names
       character(len=*), pointer, OPTIONAL :: attRealNames(:)
                                    ! User defined global real attribute names
       character(len=*), pointer, OPTIONAL :: attIntNames(:)
                                    ! User defined global int attribute names
       integer, pointer, OPTIONAL :: attCharCnts(:)! length of attributes
       integer, pointer, OPTIONAL :: attRealCnts(:)! length of attributes
       integer, pointer, OPTIONAL :: attIntCnts(:) ! length of attributes
       character(len=*), pointer, OPTIONAL :: attChars(:)
                                    ! User defined global char attribute
       real,      pointer, OPTIONAL :: attReals(:,:)
                                    ! User defined global real attribute
       integer,   pointer, OPTIONAL :: attInts(:,:)
                                    ! User defined global int attribute

       integer, intent(out), OPTIONAL :: attIntCnt
       integer, intent(out), OPTIONAL :: attRealCnt
       integer, intent(out), OPTIONAL :: attCharCnt
       character(len=*), intent(out), OPTIONAL :: attChar
                                    ! User defined global char attribute
       real,    pointer, OPTIONAL :: attReal(:)
                                    ! User defined global real attribute
       integer, pointer, OPTIONAL :: attInt(:)
                                    ! User defined global int attribute
       type(CFIO_wGrADS), intent(out), OPTIONAL :: gw
       type(CFIO_rGrADS), intent(out), OPTIONAL :: gr
       logical, intent(out), OPTIONAL :: isOpen
       character(len=*), intent(out), OPTIONAL :: format

       integer, intent(out), OPTIONAL :: rc      ! Error return code:
                         !  0   all is well
                         ! -1  can't allocate memory for grid(s)
                         ! -2  can't allocate memory: varObjs
                         ! -3  can't allocate mem: attCharNames
                         ! -4  can't allocate mem: attRealNames
                         ! -5  can't allocate mem: attIntNames
                         ! -6  can't allocate mem: attCharCnts
                         ! -7  can't allocate mem: attRealCnts
                         ! -8  can't allocate mem: attIntCnts
                         ! -9  can't allocate mem: attChars
                         ! -10  can't allocate mem: attReals
                         ! -11  can't allocate mem: attInts
                         ! -12  can't allocate mem: attInt
                         !  rc = -19  unable to identify coordinate variable
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -47  error from ncdid or ncdinq (time)
                         !  rc = -48  error from ncinq
                         !  rc = -53  error from ncagtc/ncagt
!
! !DESCRIPTION:
!     Get meta data from a CFIO file
!EOP
!------------------------------------------------------------------------------
       integer :: rtcode
       integer :: i

       if ( present(cfioObjName) ) cfioObjName =cfio%cfioObjName

       if ( present(fName) ) fName =cfio%fName 
       if ( present(fNameTmplt) ) fNameTmplt =cfio%fNameTmplt
       if ( present(title) ) title = cfio%title
       if ( present(source) ) source = cfio%source 
       if ( present(contact) ) contact = cfio%contact
       if ( present(history) ) history = cfio%history 
       if ( present(convention) ) convention = cfio%convention
       if ( present(institution) ) institution = cfio%institution 
       if ( present(references) ) references = cfio%references 
       if ( present(comment) ) comment = cfio%comment 
       if ( present(date) ) date = cfio%date 
       if ( present(begTime) ) begTime = cfio%begTime
       if ( present(timeInc) ) timeInc = cfio%timeInc
       if ( present(prec) ) prec = cfio%prec
       if ( present(gw) ) gw = cfio%gw
       if ( present(gr) ) gr = cfio%gr
       if ( present(isOpen) ) isOpen = cfio%isOpen
       if ( present(format) ) format = cfio%format
       if ( present(nSteps) ) nSteps = cfio%tSteps
       if ( present(nVars) ) nVars  = cfio%mVars   
       if ( present(nGrids) ) nGrids = cfio%mGrids  
       if ( present(grids) ) then
          allocate(grids(size(cfio%grids)), stat=rtcode)
          if (err("can't allocate memory for grids",rtcode,-1) .lt. 0 ) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          grids = cfio%grids
       end if
       if ( present(grid) ) then
          allocate(grid, stat=rtcode)
          if (err("can't allocate memory for grid",rtcode,-1) .lt. 0 ) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          grid = cfio%grids(1)
       end if
                                                                                     
       if ( present(varObjs) ) then
          allocate(varObjs(size(cfio%varObjs)), stat=rtcode)
          if (err("can't allocate memory: varObjs",rtcode,-2) .lt. 0 ) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          varObjs = cfio%varObjs
       end if

       if ( present(nAttChar) ) nAttChar = cfio%nAttChar
       if ( present(nAttReal) ) nAttReal = cfio%nAttReal
       if ( present(nAttInt) ) nAttInt = cfio%nAttInt

!      get global attribute names as an array.
       if ( present(attCharNames) ) then
          allocate(attCharNames(cfio%nAttChar), stat=rtcode)
          if (err("can't allocate mem: attCharNames",rtcode,-3) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attCharNames = cfio%attCharNames
       end if
       if ( present(attRealNames) ) then
          allocate(attRealNames(cfio%nAttReal), stat=rtcode)
          if (err("can't allocate mem: attRealNames",rtcode,-4) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attRealNames = cfio%attRealNames
       end if
       if ( present(attIntNames) ) then
          allocate(attIntNames(cfio%nAttInt), stat=rtcode)
          if (err("can't allocate mem: attIntNames",rtcode,-5) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attIntNames = cfio%attIntNames
       end if

!      get global attribute counts as an array.
       if ( present(attCharCnts) ) then
          allocate(attCharCnts(cfio%nAttChar), stat=rtcode)
          if (err("can't allocate mem: attCharCnts",rtcode,-6) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attCharCnts = cfio%attCharCnts
       end if
       if ( present(attRealCnts) ) then
          allocate(attRealCnts(cfio%nAttReal), stat=rtcode)
          if (err("can't allocate mem: attRealCnts",rtcode,-7) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attRealCnts = cfio%attRealCnts
       end if
       if ( present(attIntCnts) ) then
          allocate(attIntCnts(cfio%nAttInt), stat=rtcode)
          if (err("can't allocate mem: attIntCnts",rtcode,-8) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attIntCnts = cfio%attIntCnts
       end if

!      get global attributes as an array.
       if ( present(attChars) ) then
          allocate(attChars(cfio%nAttChar), stat=rtcode)
          if (err("can't allocate mem: attChars",rtcode,-9) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attChars= cfio%attChars
       end if
       if ( present(attReals) ) then
          allocate(attReals(cfio%nAttReal,size(cfio%attReals)/  &
                   cfio%nAttReal), stat=rtcode)
          if (err("can't allocate mem: attReals",rtcode,-10) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attReals= cfio%attReals
       end if
       if ( present(attInts) ) then
          allocate(attInts(cfio%nAttInt,size(cfio%attInts)/  &
                   cfio%nAttInt), stat=rtcode)
          if (err("can't allocate mem: attInts",rtcode,-11) .lt. 0) then  
             if ( present(rc) ) rc = rtcode
             return
          end if

          attInts= cfio%attInts
       end if

!      provide attIntName and get its count and data
       if ( present(attIntName) ) then
          if ( present(attIntCnt) ) then
             do i = 1, cfio%nAttInt
                if (trim(attIntName) .eq. trim(cfio%attIntNames(i))) &
                    then
                   attIntCnt = cfio%attIntCnts(i)
                end if
             end do
          end if
          if ( present(attInt) ) then
             do i = 1, cfio%nAttInt
                if (trim(attIntName) .eq. trim(cfio%attIntNames(i)))&
                    then
                   allocate(attInt(cfio%attIntCnts(i)))
                   if (err("can't allocate mem: attInt",rtcode,-12) .lt. 0) &
                      then  
                      if ( present(rc) ) rc = rtcode
                      return
                   end if

                   attInt = cfio%attInts(i,1:cfio%attIntCnts(i))
                end if
             end do
          end if
       end if

!      provide attRealName and get its count and data
       if ( present(attRealName) ) then
          if ( present(attRealCnt) ) then
             do i = 1, cfio%nAttReal
                if (trim(attRealName) .eq. trim(cfio%attRealNames(i))) &
                    then
                   attRealCnt = cfio%attRealCnts(i)
                end if
             end do
          end if
          if ( present(attReal) ) then
             do i = 1, cfio%nAttReal
                if (trim(attRealName) .eq. trim(cfio%attRealNames(i)))&
                    then
                   allocate(attReal(cfio%attRealCnts(i)))
                   attReal = cfio%attReals(i,1:cfio%attRealCnts(i))
                end if
             end do
          end if
       end if

!      provide attCharName and get its count and data
       if ( present(attCharName) ) then
          if ( present(attCharCnt) ) then
             do i = 1, cfio%nAttChar
                if (trim(attCharName) .eq. trim(cfio%attCharNames(i))) &
                    then
                   attCharCnt = cfio%attCharCnts(i)
                end if
             end do
          end if
          if ( present(attChar) ) then
             do i = 1, cfio%nAttChar
                if (trim(attCharName) .eq. trim(cfio%attCharNames(i)))&
                    then
                   attChar = trim(cfio%attChars(i))
                end if
             end do
          end if
       end if

       rtcode = 0
       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOGet

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIODestroy -- destructor for a CFIO object

! !INTERFACE:
      subroutine ESMF_CFIODestroy (cfio, rc)
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
      type(ESMF_CFIO), intent(inout) :: cfio       ! CFIO object

!
! !DESCRIPTION:
!     destructor for a CFIO object
!EOP
!------------------------------------------------------------------------------
      integer :: rtcode
      integer :: i

      if ( cfio%isOpen ) call CFIO_Close(cfio%fid, rtcode)
      if ( associated(cfio%varObjs) ) then
         do i = 1, size(cfio%varObjs)
            call ESMF_CFIOVarInfoDestroy (cfio%varObjs(i), rtcode)
         end do
         deallocate(cfio%varObjs, stat=rtcode)
      end if
      if ( associated(cfio%grids) ) then
         if (cfio%isGridSet) then
            do i = 1, size(cfio%grids)
               call ESMF_CFIOGridDestroy (cfio%grids(i), rtcode)
            end do
         end if

         deallocate(cfio%grids, stat=rtcode)
      end if

      if (associated(cfio%attCharCnts)) deallocate(cfio%attCharCnts,         &
         stat=rtcode)
      if (associated(cfio%attRealCnts)) deallocate(cfio%attRealCnts,         &
         stat=rtcode)
      if (associated(cfio%attIntCnts)) deallocate(cfio%attIntCnts,           &
         stat=rtcode)

      if (associated(cfio%attCharNames)) deallocate(cfio%attCharNames,       &
         stat=rtcode)
      if (associated(cfio%attRealNames)) deallocate(cfio%attRealNames,       &
         stat=rtcode)
      if (associated(cfio%attIntNames)) deallocate(cfio%attIntNames,         &
         stat=rtcode)

      if (associated(cfio%attChars)) deallocate(cfio%attChars, stat=rtcode)
      if (associated(cfio%attReals)) deallocate(cfio%attReals, stat=rtcode)
      if (associated(cfio%attInts)) deallocate(cfio%attInts, stat=rtcode)

      if (associated(cfio%iList)) deallocate(cfio%iList, stat=rtcode)
      if (associated(cfio%rList)) deallocate(cfio%rList, stat=rtcode)
      if (associated(cfio%cList)) deallocate(cfio%cList, stat=rtcode)

      rtcode = 0
      if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIODestroy



      end module ESMF_CFIOFileMod
