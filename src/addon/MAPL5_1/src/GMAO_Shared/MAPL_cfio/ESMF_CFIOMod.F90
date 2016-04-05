!==============================================================================
!BOP
! !MODULE: ESMF_CFIOMod.F90 - Source file for CFIO

       module ESMF_CFIOMod
!
! !DESCRIPTION:
!
! The code in this file provides interface specifications
!
! This module provides all the necessary subroutines for users to write/read
! HDF or GrADS format output using CF convention.
!
! !REVISION HISTORY:
!
!  Jan2004  Baoyu Yin  Initial design and prototyping.
!  Apr2004  Baoyu Yin  Implementation
!  Sep2004  Baoyu Yin  Modified return codes to make it more specific.
!  Sep2004  Baoyu Yin  Moved some utility routines to ESMF_CFIOUtil.F90.
!  Sep2004  Baoyu Yin  Modified station grid metadata.
!  Sep2004  Baoyu Yin  Added ptopUnit to ptop for eta and sigma coordinates.
!  Oct2004  Baoyu Yin  Migrated to Halem and fixed some bugs.
!  Oct2004  Baoyu Yin  Added timeString to ESMF_CFIOSet and ESMF_CFIOVarWrite.
!                      Rearranged the argument order in ESMF_CFIOVarWrite.
!  Jan2005  Baoyu Yin  Fixed some memory problems. Fixed scaleFactor and offset
!                      problem. Fixed standard_name problem in reading GFIO files.
!  Mar2005  Baoyu Yin  Moved some utility routines into ESMF_CFIOUtil.F90
!                      Modified error return codes.
!  Mar2005  Baoyu Yin  Added file name template
!  Apr2005  Baoyu Yin  Added time interpolation routine VarReadT
!  Apr2006  da Silva   Eliminated mpeu dependency.
!  Jun2006  Baoyu Yin  Added cyclic option for VarReadT
!  Jun2006  Baoyu Yin  Added reading 2D variable with VarReadT
!  Jul2006  da Silva   Eliminated read(str,fmt) to parse time; replaced
!                      with more robust mod() calculations.
!                      Made StrTemplate public.
!  Aug2006  da Silva   Added alternative interfaces to VarReadT.
!                      Included Baoyu patches in FileOpen() to handle double
!                      coordinate variables; previous merge of VarRead()
!                      and VarReadT() has been rolled back.
!  Dec2006  da Silva   Added ESMF_CFIODownBit() to downgrade precision for
!                      better gzipping.
!  Feb2007  Baoyu Yin  This is a new wrapper module for handling SDF or GrADS 
!                      format output.
!  Mar2008  Dan Kokron Replace some code in ESMF_CFIOVarReadT2D__ that prevent
!                      time increment larger than 99 hours 
!  Jun2008  Dan Kokron Replace read(str,fmt) to parse time in VarReadT3D with
!                      call to parseIntTime
!------------------------------------------------------------------------------
! !USES:
      use ESMF_CFIOUtilMod
      use ESMF_CFIOGridMod
      use ESMF_CFIOVarInfoMod
      use ESMF_CFIOFileMod
      use ESMF_CFIOSdfMod
#if defined(HDFEOS)
      use ESMF_CFIOEOSMod
#endif
      use ESMF_CFIOGrADSMod
      use m_chars
      implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

! !PUBLIC MEMBER FUNCTIONS:

      public :: ESMF_CFIO                ! Main CFIO object

      public :: ESMF_CFIOFileCreate      ! Create a CFIO file for writing 
      public :: ESMF_CFIOFileOpen        ! Open a CFIO file 
      public :: ESMF_CFIOVarWrite        ! Write a variable to a file 
      public :: ESMF_CFIOVarRead         ! Read a variable from a file
      public :: ESMF_CFIOVarReadT        ! Read a variable from a file
      public :: ESMF_CFIOVarReadT2       ! Read a variable from a file
      public :: ESMF_CFIOFileClose       ! Close an existing CFIO file. 

      public :: ESMF_CFIOstrTemplate     ! replacement for the one in mpeu

      public :: ESMF_CFIODownBit         ! Downgrade precision for better
                                         !  lossless compression

      interface ESMF_CFIOVarWrite; module procedure   &
        ESMF_CFIOVarWrite3D_,  &
        ESMF_CFIOVarWrite2D_,  &
        ESMF_CFIOVarWrite1D_
      end interface

      interface ESMF_CFIOVarRead; module procedure   &
        ESMF_CFIOVarRead3D_,   &
        ESMF_CFIOVarRead2D_,   &
        ESMF_CFIOVarRead1D_
      end interface 

!     AMS: These were split because *D_ and *D__ routines
!          had the same signature!
      interface ESMF_CFIOVarReadT; module procedure   &
        ESMF_CFIOVarReadT3D_,  &
        ESMF_CFIOVarReadT2D_ 
      end interface

      interface ESMF_CFIOVarReadT2; module procedure   &
        ESMF_CFIOVarReadT3D__, &
        ESMF_CFIOVarReadT2D__
      end interface

      interface ESMF_CFIOstrTemplate; module procedure   &
        strTemplate_
      end interface

      interface ESMF_CFIODownBit
        module procedure ESMF_CFIODownBit3D_
        module procedure ESMF_CFIODownBit2D_
      end interface

!
!EOP
      contains

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOFileCreate -- Create a CFIO output file with meta data

! !INTERFACE:
      subroutine ESMF_CFIOFileCreate (cfio, rc, expid, format)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inout) :: cfio       ! a CFIO object
      character(len=*), intent(in), OPTIONAL  :: expid    ! Experiment ID
      character(len=*), intent(in), OPTIONAL  :: format   ! GrADS or SDF
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                      ! 0   all is well
                      ! -1 Time increment is 0
                      ! -2  allocate memory error
                      ! -3  Num of int/char/real elements and Cnt don't match
                      ! -12  error determining default precision
                      ! -18 incorrect time increment
                      ! -30 can't open file
                      ! -31 error from NF90_DEF_DIM
                      ! -32 error from NF90_DEF_VAR (dimension variable)
                      ! -33 error from NF90_PUT_ATT (dimension attribute)
                      ! -34 error from NF90_DEF_VAR (variable)
                      ! -35  error from NF90_PUT_ATT (variable attribute)
                      ! -36  error from NF90_PUT_ATT (global attribute)
                      ! -37  error from NF90_ENDDEF
                      ! -38  error from NF90_PUT_VAR (dimension variable)
                      ! -39 Num of real var elements and Cnt differ
                      ! -55  error from NF90_REDEF (enter define mode)
                      ! -56  error from NF90_ENDDEF (exit define mode)
!
! !DESCRIPTION:
!     Create a CFIO output file with meta data
!EOP
!------------------------------------------------------------------------------
      integer :: rtcode
      character (len=16) :: myFormat
 
      myFormat = 'SDF'
      if (present(format)) then
         if (trim(uppercase(format)) .eq. 'GRADS' )  then
             call ESMF_CFIOSet(cfio, format='GRADS')
             myFormat = 'GRADS' 
         end if
#if defined(HDFEOS)
         if (trim(uppercase(format)) .eq. 'EOS' )  then
             call ESMF_CFIOSet(cfio, format='EOS')
             myFormat = 'EOS' 
         end if
#endif
      end if
      select case (myFormat)
      case ('GRADS')
         if (present(expid)) then
            call ESMF_CFIOGrADSFileCreate (cfio, rtcode, expid)
         else
            call ESMF_CFIOGrADSFileCreate (cfio, rtcode)
         end if
         return
      case ('SDF')
         if (present(expid)) then
            call ESMF_CFIOSdfFileCreate (cfio, rtcode, expid)
         else
            call ESMF_CFIOSdfFileCreate (cfio, rtcode)
         end if
         if (present(rc)) rc = rtcode
         return
#if defined(HDFEOS)
      case ('EOS')
         call ESMF_CFIOEOSFileCreate (cfio, rtcode)
         if (present(rc)) rc = rtcode
         return
#endif
      end select

    end subroutine ESMF_CFIOFileCreate

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarRead3D_ -- Read a variable from an existing file

! !INTERFACE:
      subroutine ESMF_CFIOVarRead3D_(cfio, vName, field, date, curTime, &
                                     kBeg, kount, xBeg, xCount, yBeg,   &
                                     yCount, timeString, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to write
      integer, intent(in), OPTIONAL :: kount      ! number of levels to write
      integer, intent(in), OPTIONAL :: xBeg       ! first point for lon
      integer, intent(in), OPTIONAL :: xCount     ! number of points to read
      integer, intent(in), OPTIONAL :: yBeg       ! first point for lat
      integer, intent(in), OPTIONAL :: yCount     ! number of points to read
      character(len=*), intent(in), OPTIONAL :: timeString
                                  ! string expression for date and time

!
! !OUTPUT PARAMETERS:
!
      real, pointer :: field(:,:,:)             ! array contains data
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------
      integer :: curStep, i, j, k, rtcode
      integer :: myKbeg, myKount
      integer :: myXbeg, myXount
      integer :: myYbeg, myYount
      integer :: myDate, myBegDate, myCurTime, myTimeInc, begTime
      integer :: im, jm, km
      integer :: nVars
      type(ESMF_CFIOGrid) :: grid
      type(ESMF_CFIOVarInfo), pointer :: vars(:)
      character(len=MLEN) :: myName
      logical :: twoD
      character(len=MLEN) :: format
      character(len=16) :: myFormat

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)

      call ESMF_CFIOGet(cfio, nVars=nVars,  rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOGet"
      !ALT allocate(vars(nVars), stat=rtcode)
      !ALT if ( rtcode .ne. 0 ) print *, "problem in allocate in GrADS_read"
      call ESMF_CFIOGet(cfio, varObjs=vars, format=format, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOGet"

      do i = 1, nVars
         call ESMF_CFIOVarInfoGet(vars(i), vName=myName, grid=grid,  &
                                  twoDimVar=twoD, rc=rtcode)
         if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOVarInfoGet"
         call ESMF_CFIOGridGet(grid, im=im, jm=jm, km=km, rc=rtcode)
         if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOGridGet"
         if (km .lt. 1) km = 1
         if ( trim(vName) .eq. trim(myName) ) exit
      end do
      deallocate(vars)

      myKbeg = 1
      myKount = km
      if ( present(kbeg) ) myKbeg = kbeg
      if ( present(kount) ) myKount = kount
      if (twoD) myKount = 1
      myXbeg = 1
      myXount = im
      myYbeg = 1
      myYount = jm
      if ( present(xBeg) ) myXbeg=xBeg
      if ( present(yBeg) ) myYbeg=yBeg
      if ( present(xCount) ) myXount = xCount
      if ( present(yCount) ) myYount = yCount

      myFormat = trim(format)
      select case (myFormat)
      case ('GRADS')
        call ESMF_CFIOGrADSVarRead(cfio, vName, field, date=myDate,          &
                                   curTime=myCurTime, rc=rtcode)
        if ( rtcode .ne. 0 ) print *, "problem in GrADS_read"
        if ( present(rc) ) rc= rtcode
        return
      case ('SDF')
        call ESMF_CFIOSdfVarRead(cfio, vName, field, date=myDate,            &
                                 curTime=myCurTime, kBeg=myKbeg,             &
                                 kount=myKount, xBeg=myXbeg, xCount=myXount, &
                                 yBeg=myYbeg, yCount=myYount, rc=rtcode)
        if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOSdfVarRead"
        if ( present(rc) ) rc= rtcode
        return
      end select

      print *, "Format is not known"

      end subroutine ESMF_CFIOVarRead3D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarRead2D_ -- Read a variable from an existing file

! !INTERFACE:
      subroutine ESMF_CFIOVarRead2D_(cfio, vName, field, date, curTime, &
                                     kBeg, kount, xBeg, xCount, yBeg,   &
                                     yCount, timeString, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to write
      integer, intent(in), OPTIONAL :: kount      ! number of levels to write
      integer, intent(in), OPTIONAL :: xBeg       ! first point for lon
      integer, intent(in), OPTIONAL :: xCount     ! number of points to read
      integer, intent(in), OPTIONAL :: yBeg       ! first point for lat
      integer, intent(in), OPTIONAL :: yCount     ! number of points to read
      character(len=*), intent(in), OPTIONAL :: timeString
                                  ! string expression for date and time

!
! !OUTPUT PARAMETERS:
!
      real, pointer :: field(:,:)             ! array contains data
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------
      integer :: curStep, i, j, k, rtcode
      integer :: myKbeg, myKount
      integer :: myXbeg, myXount
      integer :: myYbeg, myYount
      integer :: myDate, myBegDate, myCurTime, myTimeInc, begTime
      integer :: im, jm, km
      integer :: nVars
      type(ESMF_CFIOGrid) :: grid
      type(ESMF_CFIOVarInfo), pointer :: vars(:)
      character(len=MLEN) :: myName
      logical :: twoD
      character(len=MLEN) :: format
      character(len=16) :: myFormat

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)

      call ESMF_CFIOGet(cfio, nVars=nVars, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOGet"
!ALT      allocate(vars(nVars), stat=rtcode)
!ALT      if ( rtcode .ne. 0 ) print *, "problem in allocate in GrADS_read"
      call ESMF_CFIOGet(cfio, varObjs=vars, format=format, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOGet"

      do i = 1, nVars
         call ESMF_CFIOVarInfoGet(vars(i), vName=myName, grid=grid,  &
                                  twoDimVar=twoD, rc=rtcode)
         if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOVarInfoGet"
         call ESMF_CFIOGridGet(grid, im=im, jm=jm, km=km, rc=rtcode)
         if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOGridGet"
         if (km .lt. 1) km = 1
         if ( trim(vName) .eq. trim(myName) ) exit
      end do

      deallocate(vars)

      myKbeg = 1
      myKount = km
      if ( present(kbeg) ) myKbeg = kbeg
      if ( present(kount) ) myKount = kount
      if (twoD) myKount = 1
      myXbeg = 1
      myXount = im
      myYbeg = 1
      myYount = jm
      if ( present(xBeg) ) myXbeg=xBeg
      if ( present(yBeg) ) myYbeg=yBeg
      if ( present(xCount) ) myXount = xCount
      if ( present(yCount) ) myYount = yCount

      myFormat = trim(format)
      select case (myFormat)
      case ('GRADS')
        call ESMF_CFIOGrADSVarRead(cfio, vName, field, date=myDate,            &
                                   curTime=myCurTime, rc=rtcode)
        if ( rtcode .ne. 0 ) print *, "problem in GrADS_read"
        if ( present(rc) ) rc= rtcode
        return
      case ('SDF')
        call ESMF_CFIOSdfVarRead(cfio, vName, field, date=myDate,            &
                                 curTime=myCurTime, kBeg=myKbeg,             &
                                 kount=myKount, xBeg=myXbeg, xCount=myXount, &
                                 yBeg=myYbeg, yCount=myYount, rc=rtcode)
        if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOSdfVarRead"
        if ( present(rc) ) rc= rtcode
        return
      end select

      print *, "Format is not known"

      end subroutine ESMF_CFIOVarRead2D_


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarRead1D_ -- Read a variable from an existing file

! !INTERFACE:
      subroutine ESMF_CFIOVarRead1D_(cfio, vName, field, date, curTime, &
                                     xBeg, xCount, timeString, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      integer, intent(in), OPTIONAL :: xBeg       ! first point for lon
      integer, intent(in), OPTIONAL :: xCount     ! number of points to read
      character(len=*), intent(in), OPTIONAL :: timeString
                                  ! string expression for date and time

!
! !OUTPUT PARAMETERS:
!
      real, pointer :: field(:)             ! array contains data
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------
      integer :: curStep, i, j, k, rtcode
      integer :: myXbeg, myXount
      integer :: myDate, myCurTime, myTimeInc, begTime
      integer :: im
      integer :: nVars
      type(ESMF_CFIOGrid), pointer :: grid
      type(ESMF_CFIOVarInfo), pointer :: vars(:)
      character(len=MLEN) :: myName
      character(len=MLEN) :: format


      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)

      allocate(grid, stat=rtcode)
      call ESMF_CFIOGet(cfio, nVars=nVars, grid=grid, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in GrADS_read in ESMF_CFIOGet"
!ALT      allocate(vars(nVars), stat=rtcode)
!ALT      if ( rtcode .ne. 0 ) print *, "problem in allocate in ESMF_CFIORead"
      call ESMF_CFIOGet(cfio, varObjs=vars, format=format, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in allocate in ESMF_CFIOGet"
      call ESMF_CFIOGridGet(grid, im=im, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in calling ESMF_CFIORead"

      deallocate(vars)

      myXbeg = 1
      myXount = im
      if ( present(xBeg) ) myXbeg=xBeg
      if ( present(xCount) ) myXount = xCount

      select case (format)
      case ('GRADS')
        print *, "Read 1D GrADS file is not supportted."
      case ('SDF')
        call ESMF_CFIOSdfVarRead(cfio, vName, field, date=myDate,            &
                                 curTime=myCurTime,  xBeg=myXbeg,            &
                                 xCount=myXount, rc=rtcode)
        if ( rtcode .ne. 0 ) print *, "problem in calling ESMF_CFIORead"
        if ( present(rc) ) rc= rtcode
        return
      end select

      print *, "Format is not known"

      end subroutine ESMF_CFIOVarRead1D_


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarWrite3D_ -- Write a variable to a output file

! !INTERFACE:
      subroutine ESMF_CFIOVarWrite3D_(cfio, vName, field, date, curTime, &
                                      kbeg, kount, timeString, doComp,   &
                                      doChunk, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! Variable name
      real, intent(in) :: field(:,:,:)            ! array contains data
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to write
      integer, intent(in), OPTIONAL :: kount      ! number of levels to write
      character(len=*), intent(in), OPTIONAL :: timeString
                                  ! string expression for date and time
      logical, intent(in), OPTIONAL :: doComp     ! do compression
      logical, intent(in), OPTIONAL :: doChunk    ! do chunk compression


!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -15  data outside of valid range
                         !  rc = -16  data outside of packing range
                         !  rc = -17  data outside of pack and valid range
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -45  error from NF90_PUT_VAR
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE
                         !  rc = -53  error from NF90_GET_ATT
                         !  rc = -54  Format is not known

!
! !DESCRIPTION:
!     Write a variable to file
!EOP
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      integer :: i, rtcode
      integer :: myDate, myCurTime
      character(len=16) :: format
      logical :: do_comp, do_chunk

      do_chunk = .false.
      do_comp = .false.
      if ( present(doComp) ) do_comp = doComp
      if ( present(doChunk) ) do_chunk = doChunk
      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)

      call ESMF_CFIOGet(cfio, format=format, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOWrite in calling ESMF_CFIOGet"

      select case (format)
      case ('GRADS')
         call ESMF_CFIOGrADSVarWrite(cfio, vName, field, date, curTime, rc=rtcode)
         if ( rtcode .ne. 0 ) print *, "problem in wGrADS_write"
         if (present(rc)) rc = rtcode
         return
      case ('SDF')
         if (present(kbeg) .and. present(kount)) then
            call ESMF_CFIOSdfVarWrite(cfio, vName, field, date=myDate,  &
                        curTime=myCurTime, kbeg=kbeg, kount=kount,      &
                        rc=rtcode)
         else
            call ESMF_CFIOSdfVarWrite(cfio, vName, field, date=myDate,  &
                        curTime=myCurTime, rc=rtcode)
         end if
         if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOVarWrite"
         if (present(rc)) rc = rtcode
         return
#if defined(HDFEOS)
      case ('EOS')
         if (present(kbeg) .and. present(kount)) then
            call ESMF_CFIOEOSVarWrite(cfio, vName, field, date=myDate,  &
                        curTime=myCurTime, kbeg=kbeg, kount=kount,      &
                        doComp = do_comp, doChunk = do_chunk,           &
                        rc=rtcode)
         else
            call ESMF_CFIOEOSVarWrite(cfio, vName, field, date=myDate,  &
                        doComp = do_comp, doChunk = do_chunk,           &
                        curTime=myCurTime, rc=rtcode)
         end if
         if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOEOSVarWrite"
         if (present(rc)) rc = rtcode
         return
#endif
      end select

         print *, "CFIO%FORMAT is not known"
         if (present(rc)) rc = -54    
         return 
      end subroutine ESMF_CFIOVarWrite3D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarWrite2D_ -- Write a variable to a output file

! !INTERFACE:
      subroutine ESMF_CFIOVarWrite2D_(cfio, vName, field, date, curTime, &
                                      kbeg, kount, timeString, doComp,   &
                                      doChunk, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! Variable name
      real, intent(in) :: field(:,:)            ! array contains data
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to write
      integer, intent(in), OPTIONAL :: kount      ! number of levels to write
      character(len=*), intent(in), OPTIONAL :: timeString
                                  ! string expression for date and time
      logical, intent(in), OPTIONAL :: doComp     ! do compression
      logical, intent(in), OPTIONAL :: doChunk    ! do chunk compression


!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -15  data outside of valid range
                         !  rc = -16  data outside of packing range
                         !  rc = -17  data outside of pack and valid range
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -45  error from NF90_PUT_VAR
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE
                         !  rc = -53  error from NF90_GET_ATT
                         !  rc = -54  Format is not known

!
! !DESCRIPTION:
!     Write a variable to file
!EOP
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      integer :: i, rtcode
      integer :: myKbeg, myKount
      integer :: myDate, myCurTime
      character(len=16) :: format
      logical :: do_comp, do_chunk

      do_chunk = .false.
      do_comp = .false.
      if ( present(doComp) ) do_comp = doComp
      if ( present(doChunk) ) do_chunk = doChunk

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)

      call ESMF_CFIOGet(cfio, format=format, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOWrite in calling ESMF_CFIOGet"

      select case (format)
      case ('GRADS')
         call ESMF_CFIOGrADSVarWrite(cfio, vName, field, date, curTime, rc=rtcode)
         if ( rtcode .ne. 0 ) print *, "problem in wGrADS_write"
         if (present(rc)) rc = rtcode
         return
      case ('SDF')
         if (present(kbeg) .and. present(kount)) then
           call ESMF_CFIOSdfVarWrite(cfio, vName, field, date=myDate,  &
                        curTime=myCurTime, kbeg=kbeg, kount=kount,     &
                        rc=rtcode)
         else
           call ESMF_CFIOSdfVarWrite(cfio, vName, field, date=myDate,  &
                        curTime=myCurTime, rc=rtcode)
         end if
         if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOVarWrite"
         if (present(rc)) rc = rtcode
         return 
#if defined(HDFEOS)
      case ('EOS')
         if (present(kbeg) .and. present(kount)) then
           call ESMF_CFIOEosVarWrite(cfio, vName, field, date=myDate,  &
                        curTime=myCurTime, kbeg=kbeg, kount=kount,     &
                        doComp = do_comp, doChunk = do_chunk,           &
                        rc=rtcode)
         else
           call ESMF_CFIOEosVarWrite(cfio, vName, field, date=myDate,  &
                        doComp = do_comp, doChunk = do_chunk,           &
                        curTime=myCurTime, rc=rtcode)
         end if
         if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOEosVarWrite"
         if (present(rc)) rc = rtcode
         return
#endif
      end select

         print *, "CFIO%FORMAT is not known"
         if (present(rc)) rc = -54    
         return 
      end subroutine ESMF_CFIOVarWrite2D_
!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarWrite1D_ -- Write a variable to a output file

! !INTERFACE:
      subroutine ESMF_CFIOVarWrite1D_(cfio, vName, field, date, curTime, &
                                      timeString, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! Variable name
      real, intent(in) :: field(:)            ! array contains data
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      character(len=*), intent(in), OPTIONAL :: timeString
                                  ! string expression for date and time


!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -15  data outside of valid range
                         !  rc = -16  data outside of packing range
                         !  rc = -17  data outside of pack and valid range
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -45  error from NF90_PUT_VAR
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE
                         !  rc = -53  error from NF90_GET_ATT
                         !  rc = -54  Format is not known

!
! !DESCRIPTION:
!     Write a variable to file
!EOP
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      integer :: rtcode
      integer :: myDate, myCurTime
      character(len=MLEN) :: fNameTmp     ! file name
      character(len=16) :: format

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)

      call ESMF_CFIOGet(cfio, format=format, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOWrite in calling ESMF_CFIOGet"
      select case (format)
      case ('GRADS')
         print *, "No way to write 1D GrADS file."
         if (present(rc)) rc = rtcode
         return
      case ('SDF')
         call ESMF_CFIOSdfVarWrite(cfio, vName, field, date=myDate,  &
                        curTime=myCurTime, rc=rtcode)
         if ( rtcode .ne. 0 ) print *, "problem in ESMF_CFIOVarWrite"
         if (present(rc)) rc = rtcode
         return 
      end select

         print *, "CFIO%FORMAT is not known"
         if (present(rc)) rc = -54    
         return 
      end subroutine ESMF_CFIOVarWrite1D_
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOFileOpen -- open a CFIO file, and get CFIO meta data
!                                into a cfio Object.

! !INTERFACE:
      subroutine ESMF_CFIOFileOpen (cfio, fmode, rc, expid, cyclic)

!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      integer, intent(in) :: fmode              ! 0 for READ-WRITE
                                                ! non-zero for READ-ONLY
      character(len=*), intent(in), OPTIONAL :: expid   ! Experiment ID
      logical, intent(in), OPTIONAL :: cyclic           ! cyclic input file
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                         ! -1   invalid count
                         ! -2   type mismatch
                         ! -12  error determining default precision
                         ! -10  ngatts is incompatible with file
                         ! -11  character string not long enough
                         ! -19  unable to identify coordinate variable
                         ! -36  error from NF90_PUT_ATT (global attribute)
                         ! -39  error from NF90_OPEN (file open)
                         ! -40  error from NF90_INQ_VARID
                         ! -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         ! -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         ! -43  error from NF90_INQ_VARID (time variable)
                         ! -47  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (time)
                         ! -48  error from NF90_INQUIRE
                         ! -51  error from NF90_GET_ATT (global attribute)
                         ! -52  error from NF90_INQUIRE_VARIABLE
                         ! -53  error from NF90_GET_ATT
                         ! -57  error from NF90_INQ_ATTNAME
                         ! -58  error from NF90_INQUIRE_ATTRIBUTE

!
! !INPUT/OUTPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inout) :: cfio    ! a CFIO object
!
! !DESCRIPTION:
!     open a CFIO file, and get CFIO meta data into a cfio Object.
!EOP
!------------------------------------------------------------------------------
      integer :: rtcode, i
      character(len=MVARLEN),dimension(:),pointer :: grads_vars
      character(len=16) :: dset
      character(len=16) :: format 
      logical :: ex
      character(len=MLEN) :: fileName
      character(len=MVARLEN) :: varName
      integer :: im, jm, km
      integer :: nVars, nSteps
      type(ESMF_CFIOGrid) :: grid
      type(ESMF_CFIOVarInfo), pointer :: vars(:)
      logical :: twoD
      real :: amiss
      real, pointer :: lon(:), lat(:), lev(:)
      logical :: myCyclic 


      if (present(expid)) call ESMF_CFIOSet(cfio, expid = expid)
      myCyclic = .false.
      if (present(cyclic)) then
         call ESMF_CFIOSet(cfio, isCyclic = cyclic)
         myCyclic = cyclic
      end if

      call ESMF_CFIOGet(cfio, fName=fileName)
      inquire(file=fileName, EXIST=ex)
      if ( .not. ex ) then
         print *, trim(fileName), "doesn't exist"
         return
      end if
      open(11, file=fileName)
      read(11, '(a)') dset
      close(11)
      format = 'SDF'
      if (index(dset,'DSET') .ge. 1 .or. index(dset,'dset') .ge. 1  &
          .or. index(dset,'Dset') .ge. 1 ) then
         call ESMF_CFIOSet(cfio, format='GRADS')
         format = 'GRADS'
      end if
 
      select case (format)
      case ('GRADS')
         if ( present(expid) ) then
            call ESMF_CFIOGrADSFileOpen (cfio, fmode, rc=rtcode, expid=expid, cyclic=myCyclic)
         else
            call ESMF_CFIOGrADSFileOpen (cfio, fmode, rc=rtcode, cyclic=myCyclic)
         end if
         if (rtcode .ne. 0) print *, "Error in ESMF_CFIOGrADSFileOpen"
         if ( present(rc) ) rc = rtcode
         return

      case ('SDF')
         if ( present(expid) ) then
            call ESMF_CFIOSdfFileOpen (cfio, fmode, rc=rtcode, expid=expid, cyclic=myCyclic)
         else
            call ESMF_CFIOSdfFileOpen (cfio, fmode, rc=rtcode, cyclic=myCyclic)
         end if
         if (rtcode .ne. 0) print *, "Error in ESMF_CFIOSdfFileOpen"
         if ( present(rc) ) rc = rtcode
         return
      end select

      end subroutine ESMF_CFIOFileOpen

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOFileClose -- close an open CFIO stream

! !INTERFACE:
      subroutine ESMF_CFIOFileClose (cfio, rc)
!
! !ARGUMENTS:
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                       ! 0   all is well
                                       ! -54  error from ncclos (file close)
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inout) :: cfio       ! CFIO object


!
! !DESCRIPTION:
!     close an open CFIO stream
!EOP
!------------------------------------------------------------------------------
       integer :: rtcode
       character(len=16) format
      call ESMF_CFIOGet(cfio, format=format, rc=rtcode)
      if (rtcode .ne. 0) print *, "Error in ESMF_CFIOGet in FileClose"

      select case (format)
      case ('GRADS')
          call ESMF_CFIOGrADSFileClose(cfio,rtcode)
          if (rtcode .ne. 0) print *, "wGrADS_close failed"
          if ( present(rc) ) rc = rtcode
          return
      case ('SDF')
          call ESMF_CFIOSdfFileClose(cfio,rtcode)
          if (rtcode .ne. 0) print *, "Error in ESMF_CFIOFileClose"
          if ( present(rc) ) rc = rtcode
          return
#if defined(HDFEOS)
      case ('EOS')
          call ESMF_CFIOEosFileClose(cfio,rtcode)
          if (rtcode .ne. 0) print *, "Error in ESMF_CFIOEosFileClose"
          if ( present(rc) ) rc = rtcode
          return
#endif
      end select


      end subroutine ESMF_CFIOFileClose

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ESMF_CFIODownBit - GRIB-based compression pre-conditioner 
!
! !INTERFACE:

   subroutine ESMF_CFIODownBit3D_ ( x, xr, nbits, undef, flops, rc )

     implicit NONE

!
! !INPUT PARAMETERS:
!
     real, intent(in)    ::  x(:,:,:)       ! input array 
     integer, intent(in) :: nbits           ! number of bits per word to retain
                                            ! - no action if nbits<1
     real, OPTIONAL, intent(in) :: undef    ! missing value
     logical, OPTIONAL, intent(in) :: flops ! if true, uses slower float point
                                            !  based algorithm
!
! !OUTPUT PARAMETERS:
!
     real*4, intent(out)   :: xr(:,:,:) ! precision reduced array; can
!                                       ! share storage with input array
                                        ! if it has same kind
     integer, intent(out)  :: rc        ! error code
                                        !  = 0 - all is well
                                        ! /= 0 - something went wrong 
!
! !DESCRIPTION:  
!
!  This routine returns a lower precision version of the input array
!  {\tt x} which retains {\tt nbits} of precision. See routine
!  {\tt ESMF\_CFIODownBit2D} for additional details. This version for
!  rank 3 arrays, calls {\tt ESMF\_CFIODownBit2D()} for each vertical
!  level.
!
! !REVISION HISTORY:
!
!  06Dec2006  da Silva  Initial version.
!
!EOP
!------------------------------------------------------------------------------

   integer :: k

   do k = lbound(x,3), ubound(x,3)
      call ESMF_CFIODownBit2D_ ( x(:,:,k), xr(:,:,k), nbits, &
                                 undef=undef, flops=flops, rc=rc )
   end do

   end subroutine ESMF_CFIODownBit3D_


!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ESMF_CFIODownBit - GRIB-based compression pre-conditioner 
!
! !INTERFACE:

   subroutine ESMF_CFIODownBit2D_ ( x, xr, nbits, undef, flops, rc )

     implicit NONE

!
! !INPUT PARAMETERS:
!
     real, intent(in)    ::  x(:,:)         ! input array 
     integer, intent(in) :: nbits           ! number of bits per word to retain
     real, OPTIONAL, intent(in) :: undef    ! missing value
     logical, OPTIONAL, intent(in) :: flops ! if true, uses slower float point
                                            !  based algorithm
!
! !OUTPUT PARAMETERS:
!
     real*4, intent(out)   :: xr(:,:)   ! precision reduced array; can
!                                       !  share storage with input array
!                                       !  if it has same kind
     integer, intent(out)  :: rc        ! error code
                                        !  = 0 - all is well
                                        ! /= 0 - something went wrong 
!
! !DESCRIPTION:  
!
!  This routine returns a lower precision version of the input array
!  {\tt x} which retains {\tt nbits} of precision. Two algorithms are
!  implemented: 1) a fast one writen in C which downgrades precision
!  by shifting {\tt xbits = 24 - nbits} bits of the mantissa, and 2) a slower
!  float point based algorithm which is the same algorithm as GRIB 
!  with fixed number of bits packing. Notice that as in GRIB the scaling 
!  factor is forced to be a power of 2 rather than a generic float.  
!  Using this power of 2 binary scaling has the advantage of improving 
!  the GZIP compression rates.
!
!  This routine returns an array of the same type and kind as the input array, 
!  so no data compression has taken place. The goal here is to reduce the
!  entropy in the input array, thereby improving compression rates 
!  by the lossless algorithms implemented internally by HDF-4/5 when writing 
!  these data to a file. In fact, these GZIP'ed and pre-conditioned files 
!  have sizes comparable to the equivalent GRIB file, while being a bonafide 
!  self-describing HDF/NetCDF file.
!
! !TO DO:
!
!  Perhaps implement GRIB decimal scaling (variable number of bits).
!
! !REVISION HISTORY:
!
!  06Dec2006  da Silva  Initial version.
!
!EOP
!------------------------------------------------------------------------------

    integer   :: E, xbits, has_undef
    real*4    :: scale, xmin, xmax, tol, undef_
    logical   :: shave_mantissa
    integer, external :: ShaveMantissa32

    rc = 0

!   Defaults for optinal arguments
!   ------------------------------
    if ( present(undef) ) then
         undef_ = undef
         has_undef = 1
    else
         undef_ = 1.0
         undef_ = huge(undef_)   ! why not?
         has_undef = 0
    endif
    if ( present(flops) ) then
         shave_mantissa = .not. flops
    else
         shave_mantissa = .true.
    endif

!   Fast, bit shifting in C
!   -----------------------
    if ( shave_mantissa ) then

       xr = x   ! compiled r8 this will convert to r4.
       xbits = 24 - nbits
       rc = ShaveMantissa32 ( xr, xr, size(x), xbits, has_undef, undef_, size(x) )
       return

!   Slow, flops in FORTRAN (GRIB inspired)
!   --------------------------------------
    else 

       if ( nbits < 1 ) then
          xr = x
          rc = 1
          return
       end if

       tol = 0.0001 * undef_
       xmin = minval(x,mask=(abs(undef_-x)>tol))
       xr = x - xmin     ! As in GRIB, force non-negative values 
       xmax = maxval(xr,mask=(abs(undef_-x)>tol)) ! max of positive

       if ( xmax <= 0.0 ) then
            xr = x
            rc = 0
            return  ! this means field is constant 
       end if

       E = nint(log(xmax)/log(2.)) - nbits ! GRIB binary scale factor
       scale = 2.**E                       ! GRIB requires power of 2

       if ( present(undef) ) then
          where ( abs(x - undef_) > tol )
             xr  = xmin + nint(xr/scale) * scale
          endwhere
       else
          xr  = xmin + nint(xr/scale) * scale
       end if

    end if

   end subroutine ESMF_CFIODownBit2D_

!..........................................................................


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarReadT3D_ -- Read a variable from an existing file
                                                                                                              
! !INTERFACE:
      subroutine ESMF_CFIOVarReadT3D_ ( cfio, vName, field, date, curTime, &
                                        kbeg, kount, timeString, cfio2, rc )
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to read
      integer, intent(in), OPTIONAL :: kount      ! number of levels to read
      type(ESMF_CFIO), intent(inOut), OPTIONAL :: cfio2  ! second CFIO obj
      character(len=*), intent(in), OPTIONAL :: timeString
                                  ! string expression for date and time
                                                                                                        
!
! !OUTPUT PARAMETERS:
!
      real, pointer :: field(:,:,:)             ! array contains data
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -48  error from NF90_INQUIRE
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE
                         !  rc = -99  must specify date/curTime of timeString
!
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------

    integer :: date_, curTime_

!     Resolve date/time
!     -----------------
      date_ = -1
      curTime_ = -1
      if ( present(date) )     date_     = date
      if ( present(curTime) )  curTime_  = curTime
      if ( present(timeString) ) call strToInt(timeString,date_,curTime_)
      if ( date_ < 0 .OR. curTime_ < 0 ) then
           if ( present(rc) ) rc = -99
           return
      end if

      call ESMF_CFIOVarReadT3D__ ( cfio, vName, date_, curTime_, field, & 
                                   kbeg, kount, cfio2=cfio2, rc=rc )

    end subroutine ESMF_CFIOVarReadT3D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarReadT3D_ -- Read a variable from an existing file
                                                                                                              
! !INTERFACE:

      subroutine ESMF_CFIOVarReadT3D__(cfio, vName, date, curTime, field, &
                                       kbeg, kount, rc, cfio2)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in) :: date                 ! yyyymmdd
      integer, intent(in) :: curTime              ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to read
      integer, intent(in), OPTIONAL :: kount      ! number of levels to read
      type(ESMF_CFIO), intent(inOut), OPTIONAL :: cfio2  ! second CFIO obj
                                                                                                              
!
! !OUTPUT PARAMETERS:
!
      real, pointer :: field(:,:,:)             ! array contains data
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !  rc = -20  unable to find variable
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -48  error from NF90_INQUIRE
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE
!
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------

      integer rtcode
      integer begDate, begTime, incSecs, timeIndex1, timeIndex2
      integer secs, secs1, secs2, nymd1, nymd2, nhms1, nhms2
      integer i, j, k
      integer im, jm, km
      character*8 :: strBuf
      integer :: hour, min, sec
      logical ialloc,foundvar
                                                                                         
      real    alpha, amiss
      real, pointer ::  field2(:,:,:) => null() ! workspace for interpolation

      rtcode = 0

!     find the right variable obj.
      foundvar=.false.
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) then
            foundvar=.true.
            exit
         endif
      end do
      if (.not.foundvar) then
         rc=-20
         return
      endif
      im = cfio%varObjs(i)%grid%im
      jm = cfio%varObjs(i)%grid%jm
      km = cfio%varObjs(i)%grid%km
      if (km .lt. 1) km = 1

      ialloc=.false.
      if ( .not. associated(field) ) then
         allocate(field(im,jm,km))
         ialloc=.true.
      endif

!     Get beginning time & date.  Calculate offset seconds from start.
!     ----------------------------------------------------------------
      begDate = cfio%date
      begTime = cfio%begTime

      call CFIO_parseIntTime ( cfio%timeInc, hour, min, sec )
      incSecs = sec + 60 * ( min + 60 * hour ) 
      secs = DiffDate (begDate, begTime, date, curTime)
                                                                                         
!     Determine brackting times
!     -------------------------
      if ( secs >= 0 ) then
         timeIndex1 = secs/incSecs + 1
      else
         timeIndex1 = secs/incSecs
      end if
      timeIndex2 = timeIndex1 + 1
      secs1 = (timeIndex1-1) * incSecs
      secs2 = (timeIndex2-1) * incSecs
      call GetDate ( begDate, begTime, secs1, nymd1, nhms1, rtcode )
      call GetDate ( begDate, begTime, secs2, nymd2, nhms2, rtcode )
 
!     Read grids at first time with GetVar()
!     --------------------------------------
      call ESMF_CFIOVarRead(cfio, vName, field, date=nymd1, curtime=nhms1,  kbeg=kbeg, kount=kount, rc=rtcode)
      if ( rtcode .ne. 0 .and. present(cfio2) ) then
         call ESMF_CFIOVarRead(cfio2, vName, field, date=nymd1, curtime=nhms1, kbeg=kbeg, kount=kount, rc=rtcode)
      end if
      if ( rtcode .ne. 0 ) goto 999
                                                                    
      if ( secs1 .eq. secs ) goto 999   ! no interpolation needed

      allocate(field2(size(field,1),size(field,2),size(field,3))) 
!     Read grids at second time with GetVar()
!     ---------------------------------------
      call ESMF_CFIOVarRead(cfio, vName, field2, date=nymd2, curtime=nhms2, kbeg=kbeg, kount=kount, rc=rtcode)
      if ( rtcode .ne. 0 ) then
         if ( present(cfio2) )     &
            call ESMF_CFIOVarRead(cfio2, vName, field2, &
                                  date=nymd2, curtime=nhms2, kbeg=kbeg, kount=kount, rc=rtcode)
         if ( rtcode .ne. 0 ) then
            if(ialloc) deallocate(field)
            deallocate(field2)
            return
         endif
      end if
                                                                                         
!     Get missing value
!     -----------------
      amiss = cfio%varObjs(1)%amiss

!     Do interpolation
!     ----------------
      alpha = float(secs - secs1)/float(secs2 - secs1)
      do k = 1, size(field,3)!km
         do j = 1, size(field,2)!jm
            do i = 1, size(field,1)!im
               if ( abs(field(i,j,k)-amiss) .gt. 0.001 .and.   &
                    abs(field2(i,j,k)-amiss) .gt. 0.001 ) then
                  field(i,j,k) = field(i,j,k)        &
                             + alpha * (field2(i,j,k) - field(i,j,k))
               else
                  field(i,j,k) = amiss
               end if
            end do
         end do
      end do
                                                        
      if ( associated(field2) ) deallocate(field2)
      if ( ialloc ) deallocate(field)
      rtcode = 0

!     All done
!     --------
999   continue
      if ( present(rc) ) rc = rtcode
                                                                         
      end subroutine ESMF_CFIOVarReadT3D__


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarReadT2D_ -- Read a variable from an existing file
                                                                                                              
! !INTERFACE:
      subroutine ESMF_CFIOVarReadT2D_ ( cfio, vName, field, date, curTime, &
                                        kbeg, kount, timeString, cfio2, rc )
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to read
      integer, intent(in), OPTIONAL :: kount      ! number of levels to read
      type(ESMF_CFIO), intent(inOut), OPTIONAL :: cfio2  ! second CFIO obj
      character(len=*), intent(in), OPTIONAL :: timeString
                                  ! string expression for date and time
                                                                                                        
!
! !OUTPUT PARAMETERS:
!
      real, pointer :: field(:,:)               ! array contains data
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -48  error from NF90_INQUIRE
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE
                         !  rc = -99  must specify date/curTime of timeString
!
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------

    integer :: date_, curTime_

!     Resolve date/time
!     -----------------
      date_ = -1
      curTime_ = -1
      if ( present(date) )     date_     = date
      if ( present(curTime) )  curTime_  = curTime
      if ( present(timeString) ) call strToInt(timeString,date_,curTime_)
      if ( date_ < 0 .OR. curTime_ < 0 ) then
           if ( present(rc) ) rc = -99
           return
      end if

      call ESMF_CFIOVarReadT2D__ ( cfio, vName, date_, curTime_, field, &
                                   kbeg=kbeg, kount=kount, cfio2=cfio2, rc=rc )

    end subroutine ESMF_CFIOVarReadT2D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOVarReadT2D_ -- Read a variable from an existing file
                                                                                                              
! !INTERFACE:

      subroutine ESMF_CFIOVarReadT2D__(cfio, vName, date, curTime, field, &
                                       kbeg, kount, rc, cfio2)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in) :: date                 ! yyyymmdd
      integer, intent(in) :: curTime              ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to read
      integer, intent(in), OPTIONAL :: kount      ! number of levels to read
      type(ESMF_CFIO), intent(inOut), OPTIONAL :: cfio2  ! second CFIO obj
                                                                                                              
!
! !OUTPUT PARAMETERS:
!
      real, pointer :: field(:,:)             ! array contains data
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -48  error from NF90_INQUIRE
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE
!
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------

      integer rtcode
      integer begDate, begTime, incSecs, timeIndex1, timeIndex2
      integer secs, secs1, secs2, nymd1, nymd2, nhms1, nhms2
      integer i, j, k
      integer im, jm, km
      character*8 :: strBuf
      integer :: hour, min, sec
                                                                                         
      real    alpha, amiss
      real, pointer ::  field2(:,:) => null() ! workspace for interpolation

      rtcode = 0

!     find the right variable obj.
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do
      im = cfio%varObjs(i)%grid%im
      jm = cfio%varObjs(i)%grid%jm
      km = cfio%varObjs(i)%grid%km
      if (km .lt. 1) km = 1

      if ( .not. associated(field) ) allocate(field(im,jm))

!     Get beginning time & date.  Calculate offset seconds from start.
!     ----------------------------------------------------------------
      begDate = cfio%date
      begTime = cfio%begTime

      call CFIO_parseIntTime ( cfio%timeInc, hour, min, sec )
      incSecs = sec + 60 * ( min + 60 * hour ) 
      secs = DiffDate (begDate, begTime, date, curTime)
                                                                                !     Determine brackting times
!     -------------------------
      if ( secs >= 0 ) then
         timeIndex1 = secs/incSecs + 1
      else
         timeIndex1 = secs/incSecs
      end if
      timeIndex2 = timeIndex1 + 1
      secs1 = (timeIndex1-1) * incSecs
      secs2 = (timeIndex2-1) * incSecs
      call GetDate ( begDate, begTime, secs1, nymd1, nhms1, rtcode )
      call GetDate ( begDate, begTime, secs2, nymd2, nhms2, rtcode )
 
!     Read grids at first time with GetVar()
!     --------------------------------------
      call ESMF_CFIOVarRead(cfio, vName, field, date=nymd1, curtime=nhms1,  kbeg=kbeg, kount=kount, rc=rtcode)
      if ( rtcode .ne. 0 .and. present(cfio2) ) then
         call ESMF_CFIOVarRead(cfio2, vName, field, date=nymd1, curtime=nhms1,  kbeg=kbeg, kount=kount, rc=rtcode)
      end if
      if ( rtcode .ne. 0 ) goto 999
                                                                    
      if ( secs1 .eq. secs ) goto 999   ! no interpolation needed

      allocate(field2(im,jm))
                                                                                     
!     Read grids at second time with GetVar()
!     ---------------------------------------
      call ESMF_CFIOVarRead(cfio, vName, field2, date=nymd2, curtime=nhms2, kbeg=kbeg, kount=kount, rc=rtcode)
      if ( rtcode .ne. 0 ) then
         if ( present(cfio2) )     &
            call ESMF_CFIOVarRead(cfio2, vName, field2, &
                                  date=nymd2, curtime=nhms2, kbeg=kbeg, kount=kount, rc=rtcode)
         if ( rtcode .ne. 0 ) return
      end if
                                                                                         
!     Get missing value
!     -----------------
      amiss = cfio%varObjs(1)%amiss

!     Do interpolation
!     ----------------
      alpha = float(secs - secs1)/float(secs2 - secs1)
      do j = 1, jm
         do i = 1, im
            if ( abs(field(i,j)-amiss) .gt. 0.001 .and.   &
                 abs(field2(i,j)-amiss) .gt. 0.001 ) then
               field(i,j) = field(i,j) + alpha * (field2(i,j) - field(i,j))
            else
               field(i,j) = amiss
            end if
         end do
      end do
                                                        
      rtcode = 0

!     All done
!     --------
999   continue
      if ( associated(field2) ) deallocate(field2)
      if ( present(rc) ) rc = rtcode
                                                                         
      end subroutine ESMF_CFIOVarReadT2D__
      end module ESMF_CFIOMod

