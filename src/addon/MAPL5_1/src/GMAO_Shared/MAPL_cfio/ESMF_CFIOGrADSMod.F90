!==============================================================================
! !MODULE: ESMF_CFIOGrADSMod.F90 - Source file for CFIO GrADS format

       module ESMF_CFIOGrADSMod
!
! !DESCRIPTION:
!
! The code in this file provides data type definitions and interface 
! specifications
!
! This module provides all the necessary subroutines for users to write/read
! GrADS format using CF convention.
!
! !REVISION HISTORY:
!
!  Feb2007  Baoyu Yin  Modified from ESMF_CFIOGrADSMod.F90. This is the GrADS
!                      module for CFIO.
!------------------------------------------------------------------------------
! !USES:
      use ESMF_CFIOUtilMod
      use ESMF_CFIOGridMod
      use ESMF_CFIOVarInfoMod
      use ESMF_CFIOFileMod
      use ESMF_CFIOwGrADSMod
      use ESMF_CFIOrGrADSMod
      implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public :: ESMF_CFIOGrADSFileCreate      ! Create a CFIO file for writing 
      public :: ESMF_CFIOGrADSFileOpen        ! Open a CFIO file 
      public :: ESMF_CFIOGrADSVarWrite        ! Write a variable to a file 
      public :: ESMF_CFIOGrADSVarRead         ! Read a variable from a file
      public :: ESMF_CFIOGrADSFileClose       ! Close an existing CFIO file. 

      interface ESMF_CFIOGrADSVarWrite; module procedure   &
        ESMF_CFIOGrADSVarWrite3D_,                         &
        ESMF_CFIOGrADSVarWrite2D_
      end interface
                                                                                                 
      interface ESMF_CFIOGrADSVarRead; module procedure   &
        ESMF_CFIOGrADSVarRead3D_,                         &
        ESMF_CFIOGrADSVarRead2D_
      end interface

!
!EOP
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGrADSFileCreate -- Create a CFIO output file with meta data

! !INTERFACE:
      subroutine ESMF_CFIOGrADSFileCreate (cfio, rc, expid)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inout) :: cfio       ! a CFIO object
      character(len=*), intent(in), OPTIONAL  :: expid    ! Experiment ID
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                      ! 0   all is well
!
! !DESCRIPTION:
!     Create a CFIO output file with meta data
!EOP
!------------------------------------------------------------------------------
       integer :: i, rtcode, maxLen, cnt
       integer :: im, jm, km
       integer :: nVars
       integer :: date, begTime, timeInc
       real, pointer :: lev(:)
       character(len=MLEN) :: fNameTmp     ! file name 
       character(len=MLEN) :: fileName     ! file name 
       type(ESMF_CFIOGrid), pointer :: grid
       character(len=MLEN), pointer :: gCharNames(:)
       character(len=MLEN), pointer :: gCharAtts(:)
       integer, pointer :: gCharAttCnts(:)
       character(len=MLEN), pointer :: gRealNames(:)
       real, pointer :: gRealAtts(:,:)
       integer, pointer :: gRealAttCnts(:)
       character(len=MLEN), pointer :: gIntNames(:)
       integer, pointer :: gIntAtts(:,:)
       integer, pointer :: gIntAttCnts(:)
       type(iNode), pointer :: myIntList
       type(rNode), pointer :: myRealList
       type(cNode), pointer :: myCharList
       type(ESMF_CFIOVarInfo), pointer :: myVarInfo(:)
       character(len=MLEN) :: myString
       character(len=16) :: missChar, minRChar, maxRChar, scaleChar, offSetChar

!      checking file name template
       if (present(expid)) then 
          call ESMF_CFIOSet(cfio, expid=expid)
       end if

       allocate(grid, stat=rtcode)
       call ESMF_CFIOGet(cfio, nVars=nVars, date=date, begTime=begTime,  &
                         timeInc=timeInc, fName=fileName, grid=grid)

!      checking file name template
       call strTemplate_(fNameTmp,fileName,nymd=date, nhms=begTime, stat=rtcode)

       if (trim(fNameTmp) .ne. trim(fileName)) then
          call ESMF_CFIOSet(cfio, fNameTmplt=fileName, fName=fNameTmp)
          fileName = fNameTmp
       end if

       call ESMF_CFIOGridGet(grid, im=im, jm=jm, km=km)
       if (km .lt. 1) km=1
       call ESMF_CFIOGridGet(grid, lev=lev)
       if (size(lev) .lt. 1) then
          allocate(lev(km),stat=rtcode)
          lev = 1000.
       end if
       call CFIO_wGrADS_open(cfio%gw, fileName, im, jm, km, lev, nVars, date,    &
                        nhms=begTime, nh00=timeInc, stat=rtcode)

       allocate(myVarInfo(nVars))
       call ESMF_CFIOGet(cfio, varObjs=myVarInfo)
       cfio%gw%wGrADS_meta_ct = 1
       allocate(cfio%gw%wGrADS_meta(nVars*11+20), stat=rtcode)
       cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = ' global String title '//trim(cfio%title)
       cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
       cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = ' global String contact '//trim(cfio%contact)
       cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
       cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = ' global String history '//trim(cfio%history)
       cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
       cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = ' global String convention '//trim(cfio%convention)
       cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
       cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = ' global String institution '//trim(cfio%institution)
       cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
       cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = ' global String references '//trim(cfio%references)
       cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
       cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = ' global String comment '//trim(cfio%comment)
       cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
      
!       call addList('global String title', len(trim(cfio%title)), attChar=cfio%title, cList=myCharList)
!       call addList('global String source', len(trim(cfio%source)), attChar=cfio%source, cList=myCharList)
!       call addList('global String contact', len(trim(cfio%contact)), attChar=cfio%contact, cList=myCharList)
!       call addList('global String history', len(trim(cfio%history)), attChar=cfio%history, cList=myCharList)
!       call addList('global String Conventions', len(trim(cfio%convention)), attChar=cfio%convention, cList=myCharList)
!       call addList('global String institution', len(trim(cfio%institution)), attChar=cfio%institution, cList=myCharList)
!       call addList('global String references', len(trim(cfio%references)), attChar=cfio%references, cList=myCharList)
!       call addList('global String comment', len(trim(cfio%comment)), attChar=cfio%comment, cList=myCharList)

       if ( associated(cfio%cList) ) then
          call getMaxLenCnt(maxLen, cnt, cList=cfio%cList)
          allocate(gCharNames(cnt))
          allocate(gCharAtts(cnt))
          allocate(gCharAttCnts(cnt))
          call getList(cList=cfio%cList, charAttNames=gCharNames,  &
                 charAttCnts=gCharAttCnts, charAtts=gCharAtts)
          do i = 1, cnt
             call addList('global String '//trim(gCharNames(i)),gCharAttCnts(i), &
                    attChar=trim(gCharAtts(i)), cList=myCharList)
          end do
          call setGrADSList(cfio%gw, cList=myCharList)
       end if
       do i = 1, nVars
          myString = trim(myVarInfo(i)%vName)//" String  long_name"
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' String  long_name '//trim(myVarInfo(i)%vTitle)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' String  standard_name '//trim(myVarInfo(i)%standardName)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' String  vUnits '//trim(myVarInfo(i)%vUnits)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
          write (missChar, '(E15.4)') myVarInfo(i)%amiss
          write (minRChar, '(E15.4)') myVarInfo(i)%validRange(1)
          write (maxRChar, '(E15.4)') myVarInfo(i)%validRange(2)
          write (scaleChar, '(E15.4)') myVarInfo(i)%scaleFactor
          write (offSetChar, '(E15.4)') myVarInfo(i)%addOffSet
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' Float32 _FillValue '//trim(missChar)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' Float32 valid_range '//trim(minRChar)//' '//trim(maxRChar)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' Float32 missing_value '//trim(missChar)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' Float32 fmissing_value '//trim(missChar)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' Float32 scale_factor '//trim(scaleChar)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1
          cfio%gw%wGrADS_meta(cfio%gw%wGrADS_meta_ct) = trim(myVarInfo(i)%vName)//' Float32 addOffSet '//trim(offSetChar)
          cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct + 1

!          myString = trim(myVarInfo(i)%vName)//" String  long_name"
!          call addList(myString, len(trim(myVarInfo(i)%vTitle)), &
!                       attChar=trim(myVarInfo(i)%vTitle), cList=myCharList)
!          myString = trim(myVarInfo(i)%vName)//" String  standard_name"
!          call addList(myString, len(trim(myVarInfo(i)%standardName)), &
!                       attChar=trim(myVarInfo(i)%standardName), cList=myCharList)
!          myString = trim(myVarInfo(i)%vName)//" String  units"
!          call addList(myString, len(trim(myVarInfo(i)%vUnits)), &
!                       attChar=trim(myVarInfo(i)%vUnits), cList=myCharList)
       end do
       cfio%gw%wGrADS_meta_ct = cfio%gw%wGrADS_meta_ct - 1
!       call setGrADSList(cfio%gw, cList=myCharList)

       if ( associated(cfio%rList) ) then
          call getMaxLenCnt(maxLen, cnt, rList=cfio%rList)
          if (cnt .gt. 0) then
          allocate(gRealNames(cnt))
          allocate(gRealAtts(cnt,maxLen))
          allocate(gRealAttCnts(cnt))
          call getList(rList=cfio%rList, realAttNames=gRealNames,  &
                 realAttCnts=gRealAttCnts, realAtts=gRealAtts)
          do i = 1, cnt
             call addList('global Float32 '//trim(gRealNames(i)),gRealAttCnts(i), &
                    attReal=gRealAtts(i,1:gRealAttCnts(i)), rList=myRealList)
          end do
          call setGrADSList(cfio%gw, rList=myRealList)
          end if
       end if
       do i = 1, nVars
!         call addList(trim(myVarInfo(i)%vName)//" Float32 _FillValue",1, &
!               attReal=myVarInfo(i)%amiss, rList=myRealList)
!         call addList(trim(myVarInfo(i)%vName)//" Float32 valid_range",2, &
!               attReal=myVarInfo(i)%validRange, rList=myRealList)
!         call addList(trim(myVarInfo(i)%vName)//" Float32 missing_value",1, &
!               attReal=myVarInfo(i)%amiss, rList=myRealList)
!         call addList(trim(myVarInfo(i)%vName)//" Float32 fmissing_value",1, &
!               attReal=myVarInfo(i)%amiss, rList=myRealList)
!         call addList(trim(myVarInfo(i)%vName)//" Float32 scale_factor",1, &
!               attReal=myVarInfo(i)%scaleFactor, rList=myRealList)
!         call addList(trim(myVarInfo(i)%vName)//" Float32 addOffSet",1, &
!               attReal=myVarInfo(i)%addOffSet, rList=myRealList)
       end do
!       call setGrADSList(cfio%gw, rList=myRealList)

       if ( associated(cfio%iList) ) then
          call getMaxLenCnt(maxLen, cnt, iList=cfio%iList)
          if (cnt .gt. 0) then
          allocate(gIntNames(cnt))
          allocate(gIntAtts(cnt,maxLen))
          allocate(gIntAttCnts(cnt))
          call getList(iList=cfio%iList, intAttNames=gIntNames,  &
                 intAttCnts=gIntAttCnts, intAtts=gIntAtts)
          do i = 1, cnt
             call addList('global Int32 '//trim(gIntNames(i)),gIntAttCnts(i), &
                    attInt=gIntAtts(i,1:gIntAttCnts(i)), iList=myIntList)
          end do
          call setGrADSList(cfio%gw, iList=myIntList)
          end if
       end if

       if ( rtcode .ne. 0 ) print *, "Error form CFIO opening GrADS ctl."
       call ESMF_CFIOSet(cfio, isOpen=.true.)
       if ( present(rc) ) rc = rtcode
       return

      end subroutine ESMF_CFIOGrADSFileCreate

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGrADSVarWrite3D_ -- Write a variable to a output file

! !INTERFACE:
      subroutine ESMF_CFIOGrADSVarWrite3D_(cfio, vName, field, date, curTime, rc) 
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
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
!
! !DESCRIPTION:
!     Write a variable to file
!EOP
!------------------------------------------------------------------------------
      integer :: i, rtcode
      integer :: myDate, myCurTime

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime

      call CFIO_wGrADS_write(cfio%gw,vName,Field,rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in CFIO_wGrADS_write"
      if (present(rc)) rc = rtcode

      end subroutine ESMF_CFIOGrADSVarWrite3D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGrADSVarWrite2D_ -- Write a variable to a output file

! !INTERFACE:
      subroutine ESMF_CFIOGrADSVarWrite2D_(cfio, vName, field, date, curTime, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! Variable name
      real, intent(in) :: field(:,:)              ! array contains data
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
!
! !DESCRIPTION:
!     Write a variable to file
!EOP
!------------------------------------------------------------------------------
      integer :: i, rtcode
      integer :: myDate, myCurTime

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime

      call CFIO_wGrADS_write(cfio%gw,vName,Field,rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in CFIO_wGrADS_write"
      if (present(rc)) rc = rtcode

      end subroutine ESMF_CFIOGrADSVarWrite2D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGrADSVarRead3D_ -- Read a variable from an existing file

! !INTERFACE:
      subroutine ESMF_CFIOGrADSVarRead3D_(cfio, vName, field, date, curTime, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
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
      integer :: curStep, rtcode
      integer :: myDate, myBegDate, myCurTime, myTimeInc, begTime

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      call ESMF_CFIOGet(cfio, date=myBegDate, begTime=begTime,  &
                           timeInc=myTimeInc, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in CFIO_rGrADS_read in ESMF_CFIOGet"

      curStep = 1
      if ( myTimeInc .le. 0 ) then
         curStep = 1
      else
         curStep = 1+((myDate-myBegDate)*240000 + myCurTime-begTime)/myTimeInc
         if (curStep .le. 0) curStep = 1
      end if
      call CFIO_rGrADS_read(cfio%gr, vName, curStep, field, stat=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in CFIO_rGrADS_read"
      if ( present(rc) ) rc= rtcode

    end subroutine ESMF_CFIOGrADSVarRead3D_


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGrADSVarRead2D_ -- Read a variable from an existing file

! !INTERFACE:
      subroutine ESMF_CFIOGrADSVarRead2D_(cfio, vName, field, date, curTime, rc) 
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
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
      integer :: curStep, rtcode
      integer :: myDate, myBegDate, myCurTime, myTimeInc, begTime

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      call ESMF_CFIOGet(cfio, date=myBegDate, begTime=begTime,  &
                           timeInc=myTimeInc, rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in CFIO_rGrADS_read in ESMF_CFIOGet"

      curStep = 1
      if ( myTimeInc .le. 0 ) then
         curStep = 1
      else
         curStep = 1+((myDate-myBegDate)*240000 + myCurTime-begTime)/myTimeInc
         if (curStep .le. 0) curStep = 1
      end if
      call CFIO_rGrADS_read(cfio%gr, vName, curStep, field, stat=rtcode)
      if ( rtcode .ne. 0 ) print *, "problem in CFIO_rGrADS_read"
      if ( present(rc) ) rc= rtcode

    end subroutine ESMF_CFIOGrADSVarRead2D_
!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGrADSFileOpen -- open a CFIO file, and get CFIO meta data
!                                into a cfio Object.

! !INTERFACE:
      subroutine ESMF_CFIOGrADSFileOpen (cfio, fmode, rc, expid, cyclic)

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
!
! !INPUT/OUTPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inout) :: cfio    ! a CFIO object
!
! !DESCRIPTION:
!     open a CFIO file, and get CFIO meta data into a cfio Object.
!EOP
!------------------------------------------------------------------------------
      integer :: i, j
      real :: amiss
      real, pointer :: lon(:), lat(:), lev(:)
      character(len=MVARLEN),dimension(:),pointer :: grads_vars
      character(len=MLEN) :: fileName
      integer :: rtcode
      integer :: im, jm, km
      integer :: nVars, nSteps
      integer :: begDate, begTime, timeInc
      type(ESMF_CFIOGrid) :: grid
      type(ESMF_CFIOVarInfo), pointer :: vars(:)
      character(len=MVARLEN) :: varName
      logical :: twoD
      type(iNode), pointer :: iList
      type(rNode), pointer :: rList
      type(cNode), pointer :: cList
      integer :: maxLen, cntI, cntR, cntC
      character(len=MLEN), pointer :: charNames(:), charAtts(:)
      character(len=MLEN), pointer :: vCNames(:), vRNames(:), vINames(:)
      integer, pointer :: charAttCnts(:), intAttCnts(:), realAttCnts(:)
      character(len=MLEN), pointer :: intNames(:)
      integer, pointer ::  intAtts(:,:)
      character(len=MLEN), pointer :: realNames(:)
      real, pointer ::  realAtts(:,:)
      logical :: isIList, isRList, isCList

      cntC = 0
      cntR = 0
      cntI = 0
      call ESMF_CFIOGet(cfio, fName=fileName)
      call CFIO_rGrADS_open(cfio%gr, fileName, stat=rtcode)
      if ( rtcode .ne. 0 ) print *, "Error form CFIO opening GrADS ctl."
      call CFIO_rGrADS_inquire(cfio%gr,nlon=im,nlat=jm,nlev=km, nvar=nVars, ntim=nSteps, &
                   nymd=begDate, nhms=begTime, incr=timeInc, stat=rtcode)
      if ( rtcode .ne. 0 ) print *, "Error form CFIO calling GrADS_inquire."
      allocate(grads_vars(nVars), stat = rtcode)
      if ( rtcode .ne. 0 ) print *, "Error in allocate grads_vars in FileOpen"
      grads_vars = CFIO_ptr_vars(cfio%gr)
      if (km .ge. 1) allocate(lev(km), stat = rtcode)
      if ( rtcode .ne. 0 ) print *, "Error in allocate lev in FileOpen"
      lev = CFIO_ptr_zdef(cfio%gr)
      allocate(lon(im), stat = rtcode)
      if ( rtcode .ne. 0 ) print *, "Error in allocate lon in FileOpen"
      do i = 1, im
        lon(i) = -180. + 360./im * (i-1)
      end do
      allocate(lat(jm), stat = rtcode)
      if ( rtcode .ne. 0 ) print *, "Error in allocate lat in FileOpen"
      do i = 1, jm
        lat(i) = -90. + 180./(jm-1) * (i-1)
      end do

!print *, "isIList, isRList, isCList: ", isIList, isRList, isCList
      call getCFIO_rGrADSList (cfio%gr, isIList=isIList, isRList=isRList, isCList=isCList)
!print *, "after isIList, isRList, isCList: ", isIList, isRList, isCList
!      allocate(iList,rList,cList)

!      call getCFIO_rGrADSList (cfio%gr, iList=iList, rList=rList, cList=cList)
      if ( isCList ) then
         allocate(cList)
         call getCFIO_rGrADSList (cfio%gr, cList=cList)
         call getMaxLenCnt(maxLen, cntC, cList=cList)
         allocate(charNames(cntC))
         allocate(vCNames(cntC))
         allocate(charAtts(cntC))
         allocate(charAttCnts(cntC))
         charAttCnts = 0
         call getList(cList=cList, charAttNames=charNames,  &
                charAttCnts=charAttCnts, charAtts=charAtts, vNames=vCNames)
!print *, "charNames: ", trim(charNames(1))
!print *, "vCNames: ", trim(vCNames(1))
!print *, "charAtts: ", trim(charAtts(1))
      end if

      if ( isRList ) then
         allocate(rList)
         call getCFIO_rGrADSList (cfio%gr, rList=rList)
         call getMaxLenCnt(maxLen, cntR, rList=rList)
         allocate(realNames(cntR))
         allocate(vRNames(cntR))
         allocate(realAtts(cntR,maxLen))
         allocate(realAttCnts(cntR))
         realAttCnts = 0
         call getList(rList=rList, realAttNames=realNames,  &
                realAttCnts=realAttCnts, realAtts=realAtts, vNames=vRNames)
!print *, "realNames: ", trim(realNames(1))
!print *, "vRNames: ", trim(vRNames(1))
!print *, "realAttCnts: ", realAttCnts
!print *, "realAtts: ", realAtts(1, 1:realAttCnts(1))
!print *, "realAtts: ", realAtts(2, 1:realAttCnts(2))
      end if

      if ( isIList ) then
         allocate(iList)
         call getCFIO_rGrADSList (cfio%gr, iList=iList)
         call getMaxLenCnt(maxLen, cntI, iList=iList)
         allocate(intNames(cntI))
         allocate(vINames(cntI))
         allocate(intAtts(cntI,maxLen))
         allocate(intAttCnts(cntI))
         intAttCnts = 0
         call getList(iList=iList, intAttNames=intNames,  &
                intAttCnts=intAttCnts, intAtts=intAtts, vNames=vINames)
!print *, "intNames: ", trim(intNames(1))
!print *, "vINames: ", trim(vINames(1))
!print *, "intAttCnts: ", intAttCnts
!print *, "intAtts: ", intAtts(1, 1:intAttCnts(1))
!print *, "intAtts: ", intAtts(2, 1:intAttCnts(2))
      end if

      call ESMF_CFIOGridSet(grid, im=im,jm=jm,km=km,lon=lon,lat=lat,lev=lev, &
                            levUnit='hPa', rc=rtcode)
      if ( rtcode .ne. 0 ) print *, "Error in ESMF_CFIOGridSet in FileOpen"
      allocate(vars(nVars), stat=rtcode)
      if ( rtcode .ne. 0 ) print *, "Error in allocate vars in FileOpen"
      do i =1, nVars
         varName = grads_vars(i)
         twoD = .false.
         call CFIO_rGrADS_inquire(cfio%gr, nlev=km, var=varName, udef=amiss, stat=rtcode)
         if (rtcode .ne. 0) print *, "Error form CFIO calling GrADS_inquire."
         if (km .le. 1) twoD = .true.
         vars(i) = ESMF_CFIOVarInfoCreate(vName=varName, rc=rtcode)
         if (rtcode .ne. 0) print *, "Error in calling ESMF_CFIOVarInfoCreate in FileOpen"
         call ESMF_CFIOVarInfoSet(vars(i), vName=varName, grid=grid, &
                                  amiss=amiss, twoDimVar=twoD, rc=rtcode)
         if (rtcode .ne. 0) print *, "Error in calling ESMF_CFIOVarInfoSet in FileOpen"
         do j = 1, cntC
           if (trim(charNames(j)) .eq. trim(varName)) then
              select case(trim(vCNames(j)))
              case ("aveMethod")
                call ESMF_CFIOVarInfoSet(vars(i), aveMethod=trim(charAtts(j)))
              case ("cellMthd")
                call ESMF_CFIOVarInfoSet(vars(i), cellMthd=trim(charAtts(j)))
              case ("standard_name")
                call ESMF_CFIOVarInfoSet(vars(i), standardName=trim(charAtts(j)))
              case ("ordering")
                call ESMF_CFIOVarInfoSet(vars(i), ordering=trim(charAtts(j)))
              case ("units")
                call ESMF_CFIOVarInfoSet(vars(i), vUnits=trim(charAtts(j)))
              case ("long_name")
                call ESMF_CFIOVarInfoSet(vars(i), vTitle=trim(charAtts(j)))
              case default 
                call ESMF_CFIOVarInfoSet(vars(i), attCharName=vCNames(j),  &
                         attChar=trim(charAtts(j)), rc=rtcode)
              end select
           end if 
         end do
         do j = 1, cntR
           if (trim(realNames(j)) .eq. trim(varName)) then
              select case(trim(vRNames(j)))
              case ("_FillValue", "missing_value", "fmissing_value")
                 call ESMF_CFIOVarInfoSet(vars(i), amiss=realAtts(j,1))
              case ("addOffSet")
                 call ESMF_CFIOVarInfoSet(vars(i), addOffSet=realAtts(j,1))
!print *, "addOffSet: ", realAtts(j,1)
              case ("scale_factor")
                 call ESMF_CFIOVarInfoSet(vars(i), scaleFactor=realAtts(j,1))
!print *, "scale_factor:", realAtts(j,1)
              case ("valid_range")
                 call ESMF_CFIOVarInfoSet(vars(i), validRange=realAtts(j,1:2))
              case ("packingRange")
                 call ESMF_CFIOVarInfoSet(vars(i), packingRange=realAtts(j,1:2))
              case default
                 call ESMF_CFIOVarInfoSet(vars(i), attRealName=vRNames(j),  &
                      attReal=realAtts(j,1:realAttCnts(j)), rc=rtcode)
              end select
           end if 
         end do
!         do j = 1, cntI
!           if (trim(intNames(j)) .eq. trim(varName)) then
!              call ESMF_CFIOVarInfoSet(vars(i), attIntName=vINames(j),  &
!                         attInt=intAtts(j,1:intAttCnts(j)), rc=rtcode)
!           end if 
!         end do
      enddo
      do j = 1, cntC
        if (trim(charNames(j)) .eq. 'global') then
           select case(trim(vCNames(j)))
              case ("title")
                 call ESMF_CFIOSet(cfio, title=trim(charAtts(j)), rc=rtcode)
!print *, "vCNames(j), charAtts(j): ", trim(vCNames(j)), trim(charAtts(j))
              case ("source")
                 call ESMF_CFIOSet(cfio, source=trim(charAtts(j)), rc=rtcode)
              case ("contact")
                 call ESMF_CFIOSet(cfio, contact=trim(charAtts(j)), rc=rtcode)
              case ("history")
                 call ESMF_CFIOSet(cfio, history=trim(charAtts(j)), rc=rtcode)
              case ("Conventions")
                 call ESMF_CFIOSet(cfio, convention=trim(charAtts(j)), rc=rtcode)
              case ("institution")
                 call ESMF_CFIOSet(cfio, institution=trim(charAtts(j)), rc=rtcode)
              case ("references")
                 call ESMF_CFIOSet(cfio, references=trim(charAtts(j)), rc=rtcode)
              case ("comment")
                 call ESMF_CFIOSet(cfio, comment=trim(charAtts(j)), rc=rtcode)
              case default     
!print *, "default vCNames(j), charAtts(j): ", trim(vCNames(j)), trim(charAtts(j))
                 call ESMF_CFIOSet(cfio, attCharName=vCNames(j), attChar=charAtts(j), rc=rtcode)
           end select
        end if 
      end do
      do j = 1, cntR
        if (trim(realNames(j)) .eq. 'global') then
           call ESMF_CFIOSet(cfio, attRealName=vRNames(j),  &
                      attReal=realAtts(j,1:realAttCnts(j)), rc=rtcode)
        end if 
      end do
      do j = 1, cntI
        if (trim(intNames(j)) .eq. 'global') then
           call ESMF_CFIOSet(cfio, attIntName=vINames(j),  &
                      attInt=intAtts(j,1:intAttCnts(j)), rc=rtcode)
        end if 
      end do
      call ESMF_CFIOSet(cfio, varObjs=vars, nSteps=nSteps, grid=grid, &
                        date=begDate, begTime=begTime, timeInc=timeInc,rc=rtcode)
      if (rtcode .ne. 0) print *, "Error in calling ESMF_CFIOSet in FileOpen"

      if ( present(rc) ) rc = rtcode

      return
      end subroutine ESMF_CFIOGrADSFileOpen

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOGrADSFileClose -- close an open CFIO stream

! !INTERFACE:
      subroutine ESMF_CFIOGrADSFileClose (cfio, rc)
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
       logical :: isOpen

       call ESMF_CFIOGet(cfio, isOpen=isOpen, rc=rtcode)
       if ( isOpen ) then 
          call CFIO_wGrADS_close(cfio%gw,rtcode)
          if (rtcode .ne. 0) then 
             print *, "CFIO_wGrADS_close failed"
          else
             call ESMF_CFIOSet(cfio, isOpen=.false., rc=rtcode)
          end if
       else
          call CFIO_rGrADS_close(cfio%gr,rtcode)
       end if

       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOGrADSFileClose


!------------------------------------------------------------------------------

      end module ESMF_CFIOGrADSMod

