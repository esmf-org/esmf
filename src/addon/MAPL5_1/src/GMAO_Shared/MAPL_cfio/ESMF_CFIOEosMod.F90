!==============================================================================
!BOP
! !MODULE: ESMF_CFIOMod.F90 - Source file for CFIO

       module ESMF_CFIOEosMod
!
! !DESCRIPTION:
!
! The code in this file provides data type definitions and interface 
! specifications
!
! This module provides all the necessary subroutines for users to write/read
! HDF format output using CF convention.
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
!  Oct2004  Baoyu Yin  Added timeString to ESMF_CFIOSet and ESMF_CFIOEosVarWrite.
!                      Rearranged the argument order in ESMF_CFIOEosVarWrite.
!  Jan2005  Baoyu Yin  Fixed some memory problems. Fixed scaleFactor and offset
!                      problem. Fixed standard_name problem in reading CFIO files.
!  Mar2005  Baoyu Yin  Moved some utility routines into ESMF_CFIOUtil.F90
!                      Modified error return codes.
!------------------------------------------------------------------------------
! !USES:
      use ESMF_CFIOUtilMod
      use ESMF_CFIOGridMod
      use ESMF_CFIOVarInfoMod
      use ESMF_CFIOFileMod
      use ESMF_CFIOwGrADSMod, only : CFIO_wGrADS
      use ESMF_CFIOrGrADSMod, only : CFIO_rGrADS
      implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
      public :: ESMF_CFIOEosFileCreate      ! Create a CFIO file for writing
      public :: ESMF_CFIOEosVarWrite        ! Write a variable to a file
      public :: ESMF_CFIOEosFileClose       ! Close an existing CFIO file.

      interface ESMF_CFIOEosVarWrite; module procedure   &
        ESMF_CFIOEosVarWrite3D_,  &
        ESMF_CFIOEosVarWrite2D_,  &
        ESMF_CFIOEosVarWrite1D_
      end interface
!
!EOP
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOEosFileCreate -- Create a CFIO output file with meta data

! !INTERFACE:
      subroutine ESMF_CFIOEosFileCreate (cfio, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inout) :: cfio       ! a CFIO object
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
       integer :: i, n, maxLen, rtcode
       character (len=MVARLEN), pointer :: vname(:), vtitle(:), vunits(:)
       integer, pointer :: kmvar(:)
       real, pointer :: valid_range(:,:), packing_range(:,:)

       allocate(vname(cfio%mVars), vtitle(cfio%mVars), vunits(cfio%mVars), &
                kmvar(cfio%mVars), valid_range(2,cfio%mVars), packing_range(2,cfio%mVars))

       do i = 1, cfio%mVars
          vname(i) = trim(cfio%varObjs(i)%vName)
          vtitle(i) = trim(cfio%varObjs(i)%vTitle)
          vunits(i) = trim(cfio%varObjs(i)%vUnits)
          kmvar(i) = cfio%grids(1)%km
          if ( cfio%varObjs(i)%twoDimVar ) kmvar(i) = 0
          valid_range(1,i) = cfio%varObjs(i)%validRange(1)
          valid_range(2,i) = cfio%varObjs(i)%validRange(2)
          packing_range(1,i) = cfio%varObjs(i)%packingRange(1)
          packing_range(2,i) = cfio%varObjs(i)%packingRange(2)
       enddo 
 
       call EOS_Create_ (cfio, trim(cfio%fName), trim(cfio%title), trim(cfio%source),   &
               trim(cfio%contact), cfio%varObjs(1)%amiss,                         &
               cfio%grids(1)%im, cfio%grids(1)%jm, cfio%grids(1)%km, cfio%grids(1)%lon,  &
               cfio%grids(1)%lat, cfio%grids(1)%lev, trim(cfio%grids(1)%levUnits),       &
               cfio%date, cfio%begTime,  cfio%timeInc, cfio%mVars, vname, vtitle,        &
               vunits, kmvar, valid_range, packing_range, cfio%prec, cfio%fid, rtcode )

!      put global attributes
       call EOS_PutCharAtt(cfio%fid,'Conventions',len(trim(cfio%convention))&
                             ,cfio%convention, rtcode )
       if (err("can't write Conventions",rtcode,rtcode) .lt. 0) then
          if ( present(rc) ) rc = rtcode
          return
       end if
                                                                                                                       
       call EOS_PutCharAtt(cfio%fid, 'title', len(trim(cfio%title)),        &
                             cfio%title, rtcode )
       if (err("can't write title",rtcode,rtcode) .lt. 0) then
          if ( present(rc) ) rc = rtcode
          return
       end if
                                                                                                                       
       call EOS_PutCharAtt(cfio%fid, 'history', len(trim(cfio%history)),    &
                             cfio%history, rtcode )
       if (err("can't write history",rtcode,rtcode) .lt. 0) then
          if ( present(rc) ) rc = rtcode
          return
       end if
                                                                                                                       
       call EOS_PutCharAtt(cfio%fid,'institution',                          &
                            len(trim(cfio%institution)),                     &
                            cfio%institution, rtcode )
       if (err("can't write institution",rtcode,rtcode) .lt. 0) then
          if ( present(rc) ) rc = rtcode
          return
       end if

       call EOS_PutCharAtt(cfio%fid, 'source', len(trim(cfio%source)),      &
                             cfio%source, rtcode )
       if (err("can't write source",rtcode,rtcode) .lt. 0) then
          if ( present(rc) ) rc = rtcode
          return
       end if

       call EOS_PutCharAtt(cfio%fid,'references',len(trim(cfio%references)),&
                             cfio%references, rtcode )
       if (err("can't write references",rtcode,rtcode) .lt. 0) then
          if ( present(rc) ) rc = rtcode
          return
       end if
                                                                                                                       
       call EOS_PutCharAtt(cfio%fid,'comment',len(trim(cfio%comment)),      &
                             cfio%comment, rtcode )
       if (err("can't write comment",rtcode,rtcode) .lt. 0) then
          if ( present(rc) ) rc = rtcode
          return
       end if

       call EOS_PutCharAtt(cfio%fid, 'contact', len(trim(cfio%contact)),    &
                             cfio%contact, rtcode )
       if (err("can't write contact",rtcode,rtcode) .lt. 0) then
          if ( present(rc) ) rc = rtcode
          return
       end if
                                                                                                                       
!      get integer attributes from iList
       if ( associated(cfio%iList) ) then
          call getMaxLenCnt(maxLen, cfio%nAttInt, iList=cfio%iList)
          allocate(cfio%attIntNames(cfio%nAttInt),                           &
                   cfio%attIntCnts(cfio%nAttInt),                            &
                   cfio%attInts(cfio%nAttInt,maxLen), stat=rtcode)
          if (err("can't allocate mem: attIntCnts",rtcode,-2) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
                                                                                                                       
          call getList(iList=cfio%iList, intAttNames=cfio%attIntNames,       &
                       intAttCnts=cfio%attIntCnts, intAtts=cfio%attInts )
       end if
                                                                                                                       
!      write user defined integer attributes
       if ( cfio%nAttInt .gt. 0 ) then
          do i = 1, cfio%nAttInt
             if ( cfio%attIntCnts(i) .gt. size(cfio%attInts(i,:)) )  then
                rtcode=err("EosFileCreate: Num of int elements and Cnt differ"  &
                            ,-3,-3)
                if ( present(rc) ) rc = rtcode
                return
             end if
                                                                                                                       
             call EOS_PutIntAtt(cfio%fid, cfio%attIntNames(i),              &
                                 cfio%attIntCnts(i), cfio%attInts(i,:),      &
                                 cfio%prec, rtcode )
             if (err("error in EOS_PutIntAtt",rtcode,rtcode) .lt. 0) then
                if ( present(rc) ) rc = rtcode
                return
             end if
                                                                                                                       
          end do
       end if
                                                                                                                       
!      get real attributes from rList
       if ( associated(cfio%rList) ) then
          call getMaxLenCnt(maxLen, cfio%nAttReal, rList=cfio%rList)
          allocate(cfio%attRealNames(cfio%nAttReal),                       &
                   cfio%attRealCnts(cfio%nAttReal),                        &
                   cfio%attReals(cfio%nAttReal,maxLen), stat=rtcode)
          if (err("can't allocate mem: attRealNames",rtcode,-2) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
                                                                                                                       
          call getList(rList=cfio%rList, realAttNames=cfio%attRealNames,   &
                       realAttCnts=cfio%attRealCnts, realAtts=cfio%attReals )
          do i = 1, cfio%nAttReal
          end do
       end if
                                                                                                                       
!      write user defined real attributes
       if ( cfio%nAttReal .gt. 0 ) then
          do i = 1, cfio%nAttReal
             if ( cfio%attRealCnts(i) .gt. size(cfio%attReals(i,:)) )  then
                rtcode=err("EosFileCreate: Num of real elements and Cnt differ" &
                            ,-3,-3)
                if ( present(rc) ) rc = rtcode
                return
             end if
             call EOS_PutRealAtt(cfio%fid, cfio%attRealNames(i),            &
                                 cfio%attRealCnts(i),                        &
                                 cfio%attReals(i,1:cfio%attRealCnts(i)),     &
                                 cfio%prec, rtcode )
             if (err("error in EOS_PutRealAtt",rtcode,rtcode) .lt. 0) then
                if ( present(rc) ) rc = rtcode
                return
             end if
          end do
       end if
                                                                                                                       
!      get char attributes from cList
       if ( associated(cfio%cList) ) then
          call getMaxLenCnt(maxLen, cfio%nAttChar, cList=cfio%cList)
          allocate(cfio%attCharNames(cfio%nAttChar),                      &
                   cfio%attCharCnts(cfio%nAttChar),                       &
                   cfio%attChars(cfio%nAttChar), stat=rtcode)
          if (err("can't allocate mem: attCharNames",rtcode,-2) .lt. 0) then
             if ( present(rc) ) rc = rtcode
             return
          end if
          call getList(cList=cfio%cList, charAttNames=cfio%attCharNames,  &
                       charAttCnts=cfio%attCharCnts, charAtts=cfio%attChars )
       end if
                                                                                                                       
!      write user defined char attributes
       if ( cfio%nAttChar .gt. 0 ) then
          do i = 1, cfio%nAttChar
             call EOS_PutCharAtt(cfio%fid, cfio%attCharNames(i),       &
                                 cfio%attCharCnts(i), cfio%attChars(i), &
                                 rtcode )
             if (err("error in EOS_PutCharAtt",rtcode,rtcode) .lt. 0) then
                if ( present(rc) ) rc = rtcode
                return
             end if
          end do
       end if
     
       cfio%isOpen = .true.

       rtcode = 0

       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOEosFileCreate

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOEosVarWrite3D_ -- Write a variable to a output file

! !INTERFACE:
      subroutine ESMF_CFIOEosVarWrite3D_(cfio, vName, field, date, curTime, kbeg, &
                                      kount, timeString, doComp, doChunk, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(in) :: cfio         ! a CFIO obj  
      character(len=*), intent(in) :: vName       ! Variable name  
      real, intent(in) :: field(:,:,:)            ! array contains data
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg       ! first level to write
      integer, intent(in), OPTIONAL :: kount      ! number of levels to write
      character(len=*), intent(in), OPTIONAL :: timeString
      logical, intent(in), OPTIONAL :: doComp     ! do szip compression
      logical, intent(in), OPTIONAL :: doChunk    ! do szip compression
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

!
! !DESCRIPTION:
!     Write a variable to file
!EOP
!------------------------------------------------------------------------------
      integer :: i, rtcode
      integer :: myKbeg, myKount
      integer :: myDate, myCurTime
      logical :: do_comp, do_chunk
      
      do_comp = .false.
      do_chunk = .false.

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)
      if ( present(doComp) ) do_comp = doComp
      if ( present(doChunk) ) do_chunk = doChunk

!
!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do

!     write 2D variable
      if ( cfio%varObjs(i)%twoDimVar ) then 
         call EOS_PutVar (cfio%fid, vName, myDate, myCurTime,             &
                        cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                        0, 1, field, do_comp, do_chunk, rtcode )
         if (err("EOS_PutVar failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
!     write 3D variable
      else
         myKbeg = 1
         myKount = cfio%varObjs(i)%grid%km

         if ( present(kbeg) ) myKbeg = kbeg 
         if ( present(kount) ) myKount = kount

         call EOS_PutVar (cfio%fid, vName, myDate, myCurTime,             &
                        cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                        myKbeg, myKount, field,  do_comp, do_chunk, rtcode )
         if (err("EOS_PutVar failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
      end if

      if ( cfio%varObjs(i)%timAve ) then
         call writeBnds(cfio, vName, myDate, myCurTime, rtcode)
      end if

      if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOEosVarWrite3D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOEosVarWrite1D_ -- Write a variable to a output file
                                                                                
! !INTERFACE:
      subroutine ESMF_CFIOEosVarWrite1D_(cfio, vName, field, date, curTime,  &
                                      timeString, doComp, doChunk, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(in) :: cfio         ! a CFIO obj
      character(len=*), intent(in) :: vName       ! Variable name
      real, intent(in) :: field(:)            ! array contains data
      integer, intent(in), OPTIONAL :: date       ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime    ! hhmmss
      character(len=*), intent(in), OPTIONAL :: timeString
      logical, intent(in), OPTIONAL :: doComp     ! do szip compression
      logical, intent(in), OPTIONAL :: doChunk    ! do szip compression
                                  ! string expression for date and time
                                                                                
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
      logical :: do_comp, do_chunk
                                                                                         
      do_comp = .false.
      do_chunk = .false.

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)
      if ( present(doComp) ) do_comp = doComp
      if ( present(doChunk) ) do_chunk = doChunk
!
!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do
                                                                                
!     NEED WORK HERE
      if (index(cfio%varObjs(i)%grid%gName,'station') .gt. 0) then
!         call CFIO_SPutVar (cfio%fid, vName, myDate, myCurTime,      &
         call EOS_PutVar (cfio%fid, vName, myDate, myCurTime,      &
                  cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                  0, 1, field,  do_comp, do_chunk, rtcode )
         if (err("CFIO_SPutVar failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
      else
         if (err("It isn't 1D station grid",rtcode,-1) .lt. 0 ) return
      end if

      if ( cfio%varObjs(i)%timAve ) then
         call writeBnds(cfio, vName, myDate, myCurTime, rtcode)
      end if

      if ( present(rc) ) rc = rtcode
                                                                                
      end subroutine ESMF_CFIOEosVarWrite1D_
                                                                                
                                                                                
!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOEosVarWrite2D_ -- Write a variable to a output file
                                                                                
! !INTERFACE:
      subroutine ESMF_CFIOEosVarWrite2D_(cfio, vName, field, date, curTime, kbeg, &
                                      kount, timeString, doComp, doChunk, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(in) :: cfio         ! a CFIO obj
      character(len=*), intent(in) :: vName       ! Variable name
      real, intent(in) :: field(:,:)            ! array contains data
      integer, intent(in), OPTIONAL :: date     ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime  ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg     ! first level to write
      integer, intent(in), OPTIONAL :: kount    ! number of levels to write
      character(len=*), intent(in), OPTIONAL :: timeString
      logical, intent(in), OPTIONAL :: doComp     ! do szip compression
      logical, intent(in), OPTIONAL :: doChunk    ! do szip compression
                                  ! string expression for date and time
                                                                                
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
      integer :: myKbeg, myKount
      integer :: myDate, myCurTime
      logical :: do_comp, do_chunk
                                                                                             
      do_comp = .false.
      do_chunk = .false.
                                                                                         
      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)
      if ( present(doComp) ) do_comp = doComp
      if ( present(doChunk) ) do_chunk = doChunk
!
!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do
                                                                                
!     write 2D variable
      if (index(cfio%varObjs(i)%grid%gName,'station') .gt. 0) then
         if ( cfio%varObjs(i)%twoDimVar ) then
!            call CFIO_SPutVar (cfio%fid, vName, myDate, myCurTime,      &
            call EOS_PutVar (cfio%fid, vName, myDate, myCurTime,      &
                     cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                     0, 1, field,  do_comp, do_chunk, rtcode )
            if (err("CFIO_SPutVar failed",rtcode,rtcode) .lt. 0) then
               if ( present(rc) ) rc = rtcode
               return
            end if
         else
            myKbeg = 1
            myKount = cfio%varObjs(i)%grid%km
            if ( present(kbeg) ) myKbeg = kbeg
            if ( present(kount) ) myKount = kount

!            call CFIO_SPutVar (cfio%fid, vName, myDate, myCurTime,          &
            call EOS_PutVar (cfio%fid, vName, myDate, myCurTime,          &
                     cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                     myKbeg, myKount, field,  do_comp, do_chunk, rtcode )
            if (err("CFIO_SPutVar failed",rtcode,rtcode) .lt. 0) then
               if ( present(rc) ) rc = rtcode
               return
            end if
         end if
      else
         call EOS_PutVar (cfio%fid, vName, myDate, myCurTime,              &
                     cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                     0, 1, field,  do_comp, do_chunk, rtcode )
         if (err("EOS_PutVar failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if

      end if
                                                         
      if ( cfio%varObjs(i)%timAve ) then
         call writeBnds(cfio, vName, myDate, myCurTime, rtcode)
      end if

      if ( present(rc) ) rc = rtcode
                                                                                
      end subroutine ESMF_CFIOEosVarWrite2D_
                                                                                

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOEosFileClose -- close an open CFIO stream

! !INTERFACE:
      subroutine ESMF_CFIOEosFileClose (cfio, rc)
!
! !ARGUMENTS:
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                       ! 0   all is well
                                       ! -54  error from NF90_CLOSE (file close)
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

       if ( cfio%isOpen ) then
          call EOS_Close(cfio%fid, rtcode)
          if (rtcode .ne. 0) then
             print *, "CFIO_Close failed"
          else
             cfio%isOpen = .false.
          end if
        
       else
          rtcode = 0
       end if

       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOEosFileClose


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: writeBnds -- write time bounds

! !INTERFACE:
      subroutine writeBnds(cfio, vName, date, curTime, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type (ESMF_CFIO), intent(in) :: cfio
      character(len=*), intent(in) :: vName
      integer, intent(in) :: date 
      integer, intent(in) :: curTime
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out), OPTIONAL :: rc      ! Error return code:
                                                ! 0   all is well
                                                ! 1   ...
!
!
! !DESCRIPTION:
!     write time bounds for time averaging variable
!EOP
!------------------------------------------------------------------------------
      integer :: sds_index, sfwdata, sfselect, sfn2index    
      integer :: sfsnatt, sfendacc
      integer :: sds_id, corner(4), edges(4), stride(4)
      integer :: hour, min, sec, incSecs, timeIndex
      integer :: seconds, timeinc, curSecs
      real*4 :: bndsdata(2)
      character*8 :: strBuf
      integer :: i, rtcode=0

!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do
      if ( cfio%varObjs(i)%timAve ) then
         seconds = DiffDate (cfio%date, cfio%begTime, date, curTime)
         timeinc = cfio%timeInc
         write (strBuf,203) timeinc
203      format (I6)
         read (strBuf,204) hour, min, sec
204      format (3I2)
         incSecs = hour*3600 + min*60 + sec
                                                                                       
         write (strBuf,203) curTime
         read (strBuf,204) hour, min, sec
         curSecs = hour*3600 + min*60 + sec
                                                                                       
         timeIndex = seconds/incSecs + 1
         corner(1) = 0
         corner(2) = timeIndex-1
         stride(1) = 1
         stride(2) = 1
         edges(1) = 2
         edges(2) = 1
         bndsdata(1) = (-incSecs + curSecs)/60.
         bndsdata(2) = curSecs/60.
         if ( cfio%varObjs(i)%aveMethod .eq. 'c' ) then
            bndsdata(1) = (-incSecs/2. + curSecs)/60.
            bndsdata(2) = (incSecs/2. + curSecs)/60.
         end if
         if ( cfio%varObjs(i)%aveMethod .eq. 'd' ) then
            bndsdata(1) = curSecs/60.
            bndsdata(2) = (incSecs + curSecs)/60.
         end if
 
         sds_index = sfn2index(cfio%sd_id, 'time_bnds')
         sds_id = sfselect(cfio%sd_id, sds_index)
         rtcode = sfwdata (sds_id, corner, stride, edges, bndsdata)
         if ( rtcode .ne. 0 ) then 
            print *, "sfwdata failed in time_bnds"
            if ( present(rc) ) rc = rtcode
            return
         end if
         rtcode = sfendacc(sds_id)
      end if
 
      if ( present(rc) ) rc = rtcode

      end subroutine writeBnds

 

!------------------------------------------------------------------------------
      subroutine EOS_Create_(cfio, fname, title, source, contact, amiss, &
                              im, jm, km, lon, lat, levs, levunits, &
                              yyyymmdd_beg, hhmmss_beg, timinc,&
                              nvars, vname, vtitle, vunits, kmvar,&
                              valid_range, packing_range, prec,&
                              fid, rc )
!
! !USES:
!
      Implicit NONE  
!
! !INPUT PARAMETERS: 
!
                                    ! ------- Global Metadata ------
      character*(*)   fname         ! File name
      character*(*)   title         ! A title for the data set
      character*(*)   source        ! Source of data, e.g. NASA/DAO
      character*(*)   contact       ! Who to contact about the data set, e.g.,
                                    ! 'Contact data@dao.gsfc.nasa.gov'
      real            amiss         ! Missing value such as 1.0E15

                                    ! ------- Dimension Metadata -------
      integer         im            ! size of longitudinal dimension
      integer         jm            ! size of latitudinal  dimension
      integer         km            ! size of vertical     dimension 
                                    ! (surface only=1)
      real*8          lon(im)       ! longitude of center of gridbox in 
                                    ! degrees east of Greenwich (can be 
                                    ! -180 -> 180 or 0 -> 360)
      real*8          lat(jm)       ! latitude of center of gridbox in 
                                    ! degrees north of equator
      real*8          levs(km)      ! Level (units given by levunits) of
                                    !   center of gridbox
      character*(*)   levunits      ! units of level dimension, e.g.,
                                    !   "millibar", "hPa", or "sigma_level"
      integer        yyyymmdd_beg   ! First year-month-day to be written 
      integer          hhmmss_beg   ! First hour-minute-second to be written
      integer         timinc        ! Increment between output times (HHMMSS)

                                    ! ------- Variable Metadata -------
      integer         nvars         ! number of variables in file
      character*(*)   vname(nvars)  ! variable short name, e.g., "hght"
      character*(*)   vtitle(nvars) ! variable long name, e.g.,
                                    !   "Geopotential Height"
      character*(*)   vunits(nvars) ! variable units, e.g., "meter/second"
      integer         kmvar(nvars)  ! number of levels for variable; it can
                                    !  either be 0 (2-D fields) or equal to km

      real    valid_range(2,nvars)  ! Variable valid range; EOS_PutVar
                                    ! will return a non-fatal error if a value is 
                                    ! outside of this range. IMPORTANT: If packing
                                    ! is not desired for a given variable, YOU MUST
                                    ! set both components of valid_range to amiss.
                                    ! ------ Packing Metadata ----
      real   packing_range(2,nvars) ! Packing range to be used for 16-bit packing 
                                    ! of each variable. IMPORTANT: If packing is not 
                                    ! desired for a given variable, YOU MUST set both
                                    ! components of packing_range to amiss.
                                    ! NOTE:
                                    ! * The packing algorithm sets all values
                                    !    outside the packing range to missing.
                                    ! * The larger the packing range, the greater
                                    !    the loss of precision.
      integer        prec           ! Desired precision of data:
                                    !   0 = 32 bit
                                    !   1 = 64 bit
                                    !   NOTE: mixing precision in the same 
                                    !   * Mixing 32 and 64 bit precision in the 
                                    !      same file is not supported.
                                    !   * If packing is turned on for a variable,
                                    !      the prec flag is ignored.
    
!
! !OUTPUT PARAMETERS:
!
      integer        fid     ! File handle
      integer        rc      ! Error return code:
                             !  rc = 0   all is well
                             !  rc = -1  time increment is 0
                             !  rc = -18 incorrect time increment
                             !
                             !  NetCDF Errors
                             !  -------------
                             !  rc = -30  error creating file
                             !  rc = -31  error defining a coordinate (dimension)
                             !  rc = -32  error detaching from grid
                             !  rc = -33  error associating a dimension with a variable
                             !  rc = -34  error defining a variable
                             !  rc = -35  error defining a variable attribute
                             !  rc = -36  error creating a global attribute
                             !  rc = -37  error attaching to grid (HDFEOS)
                             !  rc = -38  error writing a coordinate (dimension)
                             !  rc = -59  variable name contains only blanks     
!
! !INPUT/OUTPUT PARAMETERS:
!
     type(ESMF_CFIO), intent(inout) :: cfio
!

! !REVISION HISTORY: 
!
!  1997.09.13  da Silva/Lucchesi  Initial interface design.
!  1997.09.22  Lucchesi           Added timinc to interface.
!  1998.02.10  Lucchesi           Added support for applications running with
!                                 64-bit reals.
!  1998.02.17  Lucchesi           Added time_inc, begin_time, and begin_date 
!                                 attributes to the time dimension.
!  1998.03.30  Lucchesi           Documentation expanded.  Clean-up of code.
!  1998.07.07  Lucchesi           Removed vids from argument list
!  1998.07.09  Lucchesi           Converted timinc to seconds before saving
!  1998.10.09  Lucchesi           Precision flag, documentation changes.
!  1998.10.27  Lucchesi           Added support for packing and range checks
!  1999.01.29  Lucchesi           Converted API to SD for HDFEOS
!  1999.04.13  Lucchesi           Added "missing_value" as a global attritute
!                                 and a HDF-EOS Grid attribute to be consistent
!                                 with Dan Ziskin's earlier work.
!  1999.05.27  Lucchesi           Added error checking and updated error codes.
!
!EOP
!-------------------------------------------------------------------------

      ! REAL*4 variables for 32-bit output to netCDF file.

      real*4 amiss_32
      real*4 lon_32(im), lat_32(jm), levs_32(km)
      real*8 lon_64(im), lat_64(jm), levs_64(km)
      real*4 scale_32, offset_32
      real*4 high_32,low_32
      integer vid(nvars)
      integer i, j, idx, dimIdx
      integer timeid, latid, lonid, levid, dimid
      integer timedim, latdim, londim, levdim
      integer dims3D(4), dims2D(3), bnd_dim(2)
      integer bnd_id, tim_id
      integer corner(4), edges(4)
      character*80 timeUnits 
      character*(MAXCHR) dimName, dimUnits
      logical surfaceOnly
      character*8 strBuf
      character*14 dateString
      integer year,mon,day,hour,min,sec
      integer rct
      integer timeSteps


! Variables for packing

      integer*2 amiss_16
      real*4 pRange_32(2,nvars),vRange_32(2,nvars)
      logical packflag

! Set metadata strings.  These metadata values are specified in the 
! COARDS conventions

      character (len=50) :: lonName = "longitude"
      character (len=50) :: lonUnits = "degrees_east"
      character (len=50) :: latName = "latitude"
      character (len=50) :: latUnits = "degrees_north"
      character (len=50) :: levName = "vertical level"
!                           levunits: specified by user in argument list
      character (len=50) :: timeName = "time"
!                           timeUnits: string is built below
      character (len=50) :: conventions = "COARDS"
      character (len=50) :: history = "File written by CFIO v2.0.1"
      character (len=50) :: missing = "missing_value"

! NEW VARIABLES FOR SD INTERFACE
  
      ! Functions

        integer sfstart
        integer sfend
        integer sfcreate
        integer sfid2ref
        integer sfsnatt, sfscatt
        integer sfwdata
        integer sfsdmname, sfdimid
        integer sfgdinfo
        integer sfsdscale

      ! Variables

        integer sd_id
        integer sds_ref
        integer rank
        integer, allocatable :: dim_size(:)
        integer stride(4)
        integer dim_id
        integer dimSize, numAttr, type
        real*8 :: initialTime = 0.0

#if defined (HDFEOS)

! Variables for HDF-EOS

      integer GDopen
      integer EHidinfo
      integer GDcreate
      integer GDdefproj
      integer GDdeforigin
      integer GDattach
      integer GDdetach
      integer GDfldinfo
      integer GDdefdim
      integer GDdeffld
      integer GDsetfill
      integer GDwrfld
      integer GDwrattr
      integer GDclose
#endif

      integer hdfFid
      integer gridId
      real*8, dimension(2) :: uplft = (/-180000000.00, 90000000.00/)
      real*8, dimension(2) :: lwrgt = (/180000000.00, -90000000.00/)
      character*100 cdims2D
      character*100 cdims3D
      character*100 cdims
      integer dims, numType
      integer start(4), edge(4)
      

! Internal CFIO functions


      character (len=60) :: lonStr 
      character (len=60) :: latStr 
      character (len=60) :: levStr 
      character (len=60) :: timStr 
      logical :: aveFile = .false.
      character cellMthd
      amiss_16 = PACK_FILL
      surfaceOnly = .TRUE.

      if (cfio%tSteps .gt. 0) then
         timeSteps = cfio%tSteps
      else 
         timeSteps = SD_UNLIMITED
      end if

      if (lon(1) .eq. 0 )  uplft(1) = 0.0
      if (lon(1) .eq. 0 )  lwrgt(1) = 360000000.0

! Basic error-checking.

      ! rc initialized to 0.  It should be changed only in the event
      ! of an error.

      rc = 0

      if (timinc .eq. 0) then
        rc=-1
        return
      endif

! Check to see if there is only surface data in this file definition

      do i=1,nvars
        if (kmvar(i) .NE. 0) then
          surfaceOnly = .FALSE.
          exit
        endif
      enddo

      do i=1,nvars
        if ( cfio%varObjs(i)%timAve ) then
            aveFile = .true.
            cellMthd = cfio%varObjs(i)%cellMthd   
        end if
      enddo


! Convert double-precision output variables to single-precision

      do i=1,im
         lon_32(i) = lon(i)
         lon_64(i) = lon(i)
      enddo
      do i=1,jm
         lat_32(i) = lat(i)
         lat_64(i) = lat(i)
      enddo
      do i=1,km
         levs_32(i) = levs(i)
         levs_64(i) = levs(i)
      enddo
      do j=1,nvars
        do i=1,2
           vRange_32(i,j) = valid_range(i,j)
           pRange_32(i,j) = packing_range(i,j)
        enddo
      enddo

      amiss_32 = amiss

! Convert time increment to seconds.

      write (strBuf,203) timinc
203   format (I6)
      read (strBuf,204) hour, min, sec
204   format (3I2)
      if ( sec .NE. 0) then
        print *, 'CFIO_Create: Time increments not on minute', &
                ' boundaries are not currently allowed.'
        rc = -18
        return
      endif

! Open new file.

#if defined(HDFSD)   
     ! Create file.

      sd_id = sfstart (fname, DFACC_CREATE)
      if (err("Create: error in sfstart",rc,-30).NE.0) then
        print *, 'Error details: Could not create ',fname
        return
      endif
#endif

#if defined(HDFEOS)  
      ! Create file, define projection, define origin

      fid = GDopen (fname, DFACC_CREATE)
      if (err("Create: error in GDopen",rc,-30) .NE. 0) then
        print *, 'Error details: Could not create ',fname
        return
      endif

      rct = EHidinfo (fid, hdfFid, sd_id)
      if (err("Create: error in EHidinfo",rc,-30) .NE. 0) return
      cfio%sd_id = sd_id

      gridId = GDcreate (fid, GRID_NAME, im, jm, uplft, lwrgt) 
      if (err("Create: error in GDcreate",rc,-30) .NE. 0) return
      rct = GDdefproj (gridId, GCTP_GEO, 0, 0, 0)
      if (err("Create: error in GDdefproj",rc,-30) .NE. 0) return
      rct = GDdeforigin (gridId, HDFE_GD_LL)
      if (err("Create: error in GDdeforigin",rc,-30) .NE. 0) return
      gridId = GDattach (fid, GRID_NAME)
      if (err("Create: error in GDattach",rc,-37) .NE. 0) return
#endif
      

#if defined(HDFEOS)

      ! NOTE: X and Y dimensions are created implicitly by the 
      !       GD interface.  These are single-precision coordinate 
      !       variables that satisfy the needs of COARDS.
      !       The double-precision coordinate variable required by
      !       HDF-EOS are defined later.

      rct = GDdefdim (gridId, "TIME", timeSteps)
      if (err("Create: error defining TIME",rc,-31) .NE. 0) goto 999
      if (.NOT. surfaceOnly) then
        rct = GDdefdim (gridId, "Height", km)
        if (err("Create: error defining Height",rc,-31) .NE. 0) &
         goto 999
      endif
#endif

! Prepare arrays (and strings) that define dimensions.  These will
! be passed to the routines that define variables.

      dims3D(4) = timeSteps
      dims3D(3) = km
      dims3D(2) = jm
      dims3D(1) = im
#if defined(HDFEOS)
      cdims3D = "XDim,YDim,Height,TIME"
#endif

      dims2D(3) = timeSteps
      dims2D(2) = jm
      dims2D(1) = im
#if defined(HDFEOS)
      cdims2D = "XDim,YDim,TIME"     
#endif

      scale_32 = 1.0     ! No packing for now.
      offset_32 = 0.0    ! No packing for now.

! Variable definition loop

      do i=1,nvars

        if (LEN_TRIM(vname(i)) .EQ. 0) then
          print *, "CFIO_Create: Error, a variable name contains only blanks."
          rc = -59
          goto 999
        endif
        if (pRange_32(1,i) .NE. amiss_32 .OR. pRange_32(2,i) .NE. &
        amiss_32) then
          packflag = .TRUE.
        else
          packflag = .FALSE.
        endif
 
        if ( kmvar(i) .eq. 0 ) then

#if defined(HDFSD)
          if (packflag) then
            vid(i) = sfcreate (sd_id,vname(i),DFNT_INT16,3,dims2D)
          else if (prec .EQ. 1) then
            vid(i) = sfcreate (sd_id,vname(i),DFNT_FLOAT64,3,dims2D)
          else
            vid(i) = sfcreate (sd_id,vname(i),DFNT_FLOAT32,3,dims2D)
          endif
          if (err("Create: error defining variable",rc,-34).NE.0) &
         then   
            print *, 'Error details: Could not define ',vname(i)
            goto 999
          endif

          dim_id = sfdimid(vid(i),0)
          if (err("Create: error in sfdimid",rc,-33) goto 999
          rct = sfsdmname(dim_id, 'XDim') 
          if (err("Create: error in sfsdmname",rc,-33) goto 999
          dim_id = sfdimid (vid(i),1)
          if (err("Create: error in sfdimid",rc,-33) goto 999
          rct = sfsdmname(dim_id, 'YDim') 
          if (err("Create: error in sfsdmname",rc,-33) goto 999
          dim_id = sfdimid (vid(i),2)
          if (err("Create: error in sfdimid",rc,-33) goto 999
          rct = sfsdmname(dim_id, 'time') 
          if (err("Create: error in sfsdmname",rc,-33) goto 999
#endif

#if defined(HDFEOS)
          if (packflag) then
            rct = GDdeffld (gridId, vname(i), cdims2D, DFNT_INT16, &
                            HDFE_NOMERGE)
            if (err("Create: error defining variable",rc,-34).NE.0) &
           then   
              print *, 'Error details: Could not define ',vname(i)
              goto 999
            endif
            rct = GDsetfill (gridId, vname(i), amiss_32)    ! amiss_16 ?
            if (err("Create: error in GDsetfill",rc,-34).NE.0) &
             goto 999
            vid(i) = GetSDSid (fid, vname(i))             
          else if (prec .EQ. 1) then
            rct = GDdeffld (gridId, vname(i), cdims2D, DFNT_FLOAT64, &
                            HDFE_NOMERGE)
            if (err("Create: error defining variable",rc,-34).NE.0) &
           then   
              print *, 'Error details: Could not define ',vname(i)
              goto 999
            endif
            rct = GDsetfill (gridId, vname(i), amiss_32)    ! amiss_64 ?
            if (err("Create: error in GDsetfill",rc,-34).NE.0) &
             goto 999
            vid(i) = GetSDSid (fid, vname(i))
          else
            rct = GDdeffld (gridId, vname(i), cdims2D, DFNT_FLOAT32, &
                            HDFE_NOMERGE)
            if (err("Create: error defining variable",rc,-34).NE.0) &
           then   
              print *, 'Error details: Could not define ',vname(i)
              goto 999
            endif
            rct = GDsetfill (gridId, vname(i), amiss_32)
            if (err("Create: error in GDsetfill",rc,-34).NE.0) &
             goto 999
            vid(i) = GetSDSid (fid, vname(i))
          endif
#endif

        else

#if defined(HDFSD)
          if (packflag) then
            vid(i) = sfcreate (sd_id,vname(i),DFNT_INT16,4,dims3D)
          else if (prec .EQ. 1) then
            vid(i) = sfcreate (sd_id,vname(i),DFNT_FLOAT64,4,dims3D)
          else
            vid(i) = sfcreate (sd_id,vname(i),DFNT_FLOAT32,4,dims3D)
          endif
          if (err("Create: error defining variable",vid(i),rc,-34).NE.0) &
         then
            print *, 'Error details: Could not define ',vname(i)
            goto 999
          endif

          dim_id = sfdimid (vid(i),0)
          if (err("Create: error in sfdimid",rc,-33) goto 999
          rct = sfsdmname(dim_id, 'XDim')
          if (err("Create: error in sfsdmname",rc,-33) goto 999
          dim_id = sfdimid (vid(i),1)
          if (err("Create: error in sfdimid",rc,-33) goto 999
          rct = sfsdmname(dim_id, 'YDim')
          if (err("Create: error in sfsdmname",rc,-33) goto 999
          dim_id = sfdimid (vid(i),2)
          if (err("Create: error in sfdimid",rc,-33) goto 999
          rct = sfsdmname(dim_id, 'Height')
          if (err("Create: error in sfsdmname",rc,-33) goto 999
          dim_id = sfdimid (vid(i),3)
          if (err("Create: error in sfdimid",rc,-33) goto 999
          rct = sfsdmname(dim_id, 'time')
          if (err("Create: error in sfsdmname",rc,-33) goto 999
#endif

#if defined(HDFEOS)
          if (packflag) then
            rct = GDdeffld (gridId, vname(i), cdims3D, DFNT_INT16, &
                            HDFE_NOMERGE)
            if (err("Create: error defining variable",rc,-34).NE.0) &
           then
              print *, 'Error details: Could not define ',vname(i)
              goto 999
            endif
            rct = GDsetfill (gridId, vname(i), amiss_32)  ! amiss_16 ?
            if (err("Create: error in GDsetfill",rc,-34).NE.0) &
             goto 999
            vid(i) = GetSDSid (fid, vname(i))
          else if (prec .EQ. 1) then
            rct = GDdeffld (gridId, vname(i), cdims3D, DFNT_FLOAT64, &
                            HDFE_NOMERGE)
            if (err("Create: error defining variable",rc,-34).NE.0) &
           then
              print *, 'Error details: Could not define ',vname(i)
              goto 999
            endif
            rct = GDsetfill (gridId, vname(i), amiss_32)  ! amiss_64 ?
            if (err("Create: error in GDsetfill",rc,-34).NE.0) &
             goto 999
            vid(i) = GetSDSid (fid, vname(i))
          else
            rct = GDdeffld (gridId, vname(i), cdims3D, DFNT_FLOAT32, &
                            HDFE_NOMERGE)
            if (err("Create: error defining variable",rc,-34).NE.0) &
           then
              print *, 'Error details: Could not define ',vname(i)
              goto 999
            endif
            rct = GDsetfill (gridId, vname(i), amiss_32)
            if (err("Create: error in GDsetfill",rc,-34).NE.0) &
             goto 999
            vid(i) = GetSDSid (fid, vname(i))
          endif
#endif

        endif

        if (LEN_TRIM(vtitle(i)) .NE. 0) then
          rct = sfscatt( vid(i),'long_name',DFNT_CHAR8,LEN_TRIM(vtitle(i)),TRIM(vtitle(i)) )
        else
          print *, 'CFIO_Create: Warning, a variable title string is blank.'
          rct = sfscatt( vid(i),'long_name',DFNT_CHAR8,LEN(vtitle(i)),vtitle(i) )
        endif
        if (err("Create: error in sfscatt",rc,-35).NE.0) then
          print *, "Error details: Can't set long_name to ",vtitle(i)
          goto 999
        endif

!        if (TRIM(cfio%varObjs(i)%standardName) .NE. 'unknown' .and. &
!            len(TRIM(cfio%varObjs(i)%standardName)) .gt. 0) then
          rct = sfscatt( vid(i),'standard_name',DFNT_CHAR8,LEN_TRIM(cfio%varObjs(i)%standardName), &
                TRIM(cfio%varObjs(i)%standardName) )
!        else
!          rct = sfscatt( vid(i),'standard_name',DFNT_CHAR8,LEN(vtitle(i)),trim(vtitle(i)) )
!   print *, "vtitle(i) ", vtitle(i)
!        endif
        if (err("Create: error in sfscatt",rc,-35).NE.0) then
          print *, "Error details: Can't set long_name to ",vtitle(i)
          goto 999
        endif
 
        if (LEN_TRIM(vunits(i)) .NE. 0) then
          rct = sfscatt(vid(i),'units',DFNT_CHAR8,LEN_TRIM(vunits(i)),TRIM(vunits(i)) )
        else
          print *, 'CFIO_Create: Warning, a variable units string is blank.'
          rct = sfscatt(vid(i),'units',DFNT_CHAR8,LEN(vunits(i)),vunits(i) )
        endif
        if (err("Create: error in sfscatt",rc,-35).NE.0) then
          print *, "Error details: Can't set units to ",vtitle(i)
          goto 999
        endif

!
!       Set up packing info.
!

        if (packflag) then
          if (pRange_32(1,i) .GT. pRange_32(2,i)) then
            high_32 = pRange_32(1,i)
            low_32  = pRange_32(2,i)
          else
            high_32 = pRange_32(2,i)
            low_32  = pRange_32(1,i)
          endif
          scale_32 = (high_32 - low_32)/PACK_BITS*2
          offset_32 = high_32 - scale_32*PACK_BITS
          if (scale_32 .EQ. 0.0) then              ! If packing range is 0, 
             scale_32 = 1.0                        ! default to no packing.
             offset_32 = 0.0
          endif
          rct = sfscatt (vid(i),'scale_factor',DFNT_FLOAT32,1,scale_32)
          if (err("Create: error setting scale_factor",rc,-35).NE.0)&
           goto 999
          rct = sfscatt (vid(i),'add_offset',DFNT_FLOAT32,1,offset_32)
          if (err("Create: error setting add_offset",rc,-35).NE.0)&
           goto 999
          rct = sfscatt (vid(i),'packmin',DFNT_FLOAT32,1,low_32)
          if (err("Create: error setting packmin",rc,-35).NE.0) &
           goto 999
          rct = sfscatt (vid(i),'packmax',DFNT_FLOAT32,1,high_32)
          if (err("Create: error setting packmax",rc,-35).NE.0) &
           goto 999
          rct = sfscatt (vid(i),'missing_value',DFNT_INT16,1,amiss_16)
          if (err("Create: error setting missing_value",rc,-35) &
           .NE.0) goto 999
          rct=sfscatt (vid(i),'fmissing_value',DFNT_FLOAT32,1,amiss_32)
          if (err("Create: error setting fmissing_value",rc,-35) &
           .NE.0) goto 999
        else      ! NOT packflag
          scale_32 = 1.0     ! No packing.
          offset_32 = 0.0    ! No packing.
          rct = sfscatt (vid(i),'scale_factor',DFNT_FLOAT32,1,scale_32)
          if (err("Create: error setting scale_factor",rc,-35).NE.0)&
            goto 999
          rct = sfscatt (vid(i),'add_offset',DFNT_FLOAT32,1,offset_32)
          if (err("Create: error setting add_offset",rc,-35).NE.0)&
           goto 999
          rct = sfscatt (vid(i),'missing_value',DFNT_FLOAT32,1,amiss_32)
          if (err("Create: error setting missing_value",rc,-35) &
           .NE.0) goto 999
          rct=sfscatt (vid(i),'fmissing_value',DFNT_FLOAT32,1,amiss_32)
          if (err("Create: error setting fmissing_value",rc,-35)&
           .NE.0) goto 999
        endif
        if (vRange_32(1,i) .NE. amiss_32 .OR. vRange_32(2,i) .NE. &
           amiss_32) then
          if (vRange_32(1,i) .GT. vRange_32(2,i)) then
            high_32 = vRange_32(1,i)
            low_32  = vRange_32(2,i)
          else
            high_32 = vRange_32(2,i)
            low_32  = vRange_32(1,i)
          endif
          rct = sfscatt (vid(i),'vmin',DFNT_FLOAT32,1,low_32)
          if (err("Create: error setting vmin",rc,-35).NE.0)goto 999
          rct = sfscatt (vid(i),'vmax',DFNT_FLOAT32,1,high_32)
          if (err("Create: error setting vmax",rc,-35).NE.0)goto 999
          rct = sfscatt (vid(i),'valid_range',DFNT_FLOAT32,2,vRange_32(:,i))
          if (err("Create: error setting valid_range",rc,-35).NE.0)goto 999
        else
          rct = sfscatt (vid(i),'vmin',DFNT_FLOAT32,1,amiss_32)
          if (err("Create: error setting vmin",rc,-35).NE.0)goto 999
          rct = sfscatt (vid(i),'vmax',DFNT_FLOAT32,1,amiss_32)
          if (err("Create: error setting vmax",rc,-35).NE.0)goto 999
        endif
      enddo

! Build time strings for COARDS conventions.

      write (dateString,200) yyyymmdd_beg, hhmmss_beg
200   format (I8,I6)
      read (dateString,201) year,mon,day,hour,min,sec
201   format (I4,5I2)
      write (timeUnits,202) year,mon,day,hour,min,sec
202   format ('minutes since ',I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':', &
              I2.2,':',I2.2)

! If there are upper-air variables in this file, scan the variables
! until the first UA is found and save the index.  We will use the ID
! for this variable to set dimension information.

      if (surfaceOnly) then
        idx=1
      else
        do idx=1,nvars  
          if (kmvar(idx) .EQ. km) then
            exit  
          endif
        enddo
!        print *, 'idx=',idx,' nvars=',nvars,'km=',km
        if (idx .GT. nvars) then
          print *, 'CFIO_Create: error finding upper air variable.'
          idx=nvars
        endif
      endif

! Write Dimension Scale Values.

      dimUnits='dummy'
      do i=0,NDIMS_MAX-1
        dimid = sfdimid (vid(idx), i)
        rct = sfgdinfo (dimid, dimName, dimSize, type, numAttr)
        if (rct .NE. 0) then                   ! No dimensions left.
          exit
        endif
        dimIdx = IdentifyDim (dimName, dimUnits)
        if ( dimIdx .EQ. 0 ) then
          lonid = dimid
          rct = sfsdscale ( lonid, im, DFNT_FLOAT64, lon_64 )
          if (err("Create: error writing lons",rc,-38).NE.0)goto 999

          rct = sfscatt (lonid,'long_name',DFNT_CHAR8,LEN_TRIM(lonName),TRIM(lonName))
          if (err("Create: error lon attribute",rc,-35).NE.0) goto 999

          rct = sfscatt (lonid,'units',DFNT_CHAR8,LEN_TRIM(lonUnits),lonUnits)
          if (err("Create: error lon attribute",rc,-35).NE.0) goto 999

        else if ( dimIdx .EQ. 1 ) then
          latid = dimid
          rct = sfsdscale ( latid, jm, DFNT_FLOAT64, lat_64 )
          if (err("Create: error writing lats",rc,-38).NE.0)goto 999

          rct = sfscatt (latid,'long_name',DFNT_CHAR8,LEN_TRIM(latName),TRIM(latName))
          if (err("Create: error lat attribute",rc,-35).NE.0) goto 999

          rct = sfscatt (latid,'units',DFNT_CHAR8,LEN_TRIM(latUnits),TRIM(latUnits))
          if (err("Create: error lat attribute",rc,-35).NE.0) goto 999

        else if ( dimIdx .EQ. 2 ) then
          levid = dimid
          rct = sfsdscale ( levid, km, DFNT_FLOAT64, levs_64 )
          if (err("Create: error writing levs",rc,-38).NE.0)goto 999

          rct = sfscatt (levid,'long_name',DFNT_CHAR8,LEN_TRIM(levName),TRIM(levName))
          if (err("Create: error lev attribute",rc,-35).NE.0) goto 999

          ! Check for blanks in levunits because this string is passed in 
          ! by the user.

          if ( LEN_TRIM(levunits) .NE. 0) then
            rct = sfscatt (levid,'units',DFNT_CHAR8,LEN_TRIM(levunits),TRIM(levunits))
          else
            print *, 'CFIO_Create: Warning, levunits string is blank.'
            rct = sfscatt (levid,'units',DFNT_CHAR8,LEN(levunits),levUnits)
          endif
          if (err("Create: error lev attribute",rc,-35).NE.0) goto 999

          rct = sfscatt (levid,'positive',DFNT_CHAR8,LEN('down'),'down')
          if (err("Create: error lev attribute",rc,-35).NE.0) goto 999

          rct = sfscatt (levid,'coordinate',DFNT_CHAR8,LEN_TRIM(cfio%grids(1)%coordinate), &
                         TRIM(cfio%grids(1)%coordinate))
          rct = sfscatt (levid,'standard_name',DFNT_CHAR8,LEN_TRIM(cfio%grids(1)%standardName),&
                         TRIM(cfio%grids(1)%standardName))
          rct = sfscatt (levid,'formula_term',DFNT_CHAR8,LEN_TRIM(cfio%grids(1)%formulaTerm), &
                         TRIM(cfio%grids(1)%formulaTerm))
        else if ( dimIdx .EQ. 3 ) then
          timeid = dimid
          rct = sfsdscale ( timeid, 1, DFNT_FLOAT64, initialTime )
          if (err("Create: error writing times",rc,-38).NE.0) goto 999

          rct = sfscatt (timeid,'long_name',DFNT_CHAR8,LEN_TRIM(timeName),TRIM(timeName))
          if (err("Create: error time attribute",rc,-35).NE.0) goto 999

          rct = sfscatt (timeid,'units',DFNT_CHAR8,LEN_TRIM(timeUnits),TRIM(timeUnits))
          if (err("Create: error time attribute",rc,-35).NE.0) goto 999

          rct = sfscatt (timeid,'time_increment',DFNT_INT32,1,timInc)
          if (err("Create: error time attribute",rc,-35).NE.0) goto 999

          rct = sfscatt (timeid,'begin_date',DFNT_INT32,1,yyyymmdd_beg)
          if (err("Create: error time attribute",rc,-35).NE.0) goto 999

          rct = sfscatt (timeid,'begin_time',DFNT_INT32,1,hhmmss_beg)
          if (err("Create: error time attribute",rc,-35).NE.0) goto 999

          if ( aveFile ) then
             rct = sfscatt (timeid,'bound',DFNT_CHAR8,9,'time_bnds')
             if (err("Create: error time attribute",rc,-35).NE.0) goto 999
             bnd_dim(2) = 1
             bnd_dim(1) = 2
             bnd_id = sfcreate (sd_id,'time_bnds',DFNT_FLOAT32,2,bnd_dim)
             tim_id = sfdimid(bnd_id,1)
             rct = sfsdmname(tim_id, 'TIME:EOSGRID')
             tim_id = sfdimid(bnd_id,0)
             rct = sfsdmname(tim_id, 'nv')
          end if
        else
          print *, 'CFIO_Create: WARNING. Dimension ',TRIM(dimName), &
                  ' is unknown.'
        endif
      enddo

#if defined(HDFEOS)
      rct = GDdeffld (gridId,'XDim','XDim',DFNT_FLOAT64,HDFE_NOMERGE)
      if (err("Create: error defining XDim",rc,-31).NE.0)goto 999
      start(1) = 0
      stride(1) = 1
      edge(1) = im
      rct = GDwrfld (gridId,'XDim',start,stride,edge,lon_64)
      if (err("Create: error writing XDim",rc,-38).NE.0)goto 999

      rct = GDdeffld (gridId,'YDim','YDim',DFNT_FLOAT64,HDFE_NOMERGE)
      if (err("Create: error defining YDim",rc,-31).NE.0)goto 999
      edge(1) = jm
      rct = GDwrfld (gridId,'YDim',start,stride,edge,lat_64)
      if (err("Create: error writing YDim",rc,-38).NE.0)goto 999

      if (.NOT. surfaceOnly) then
        rct = GDdeffld (gridId,'Height','Height',DFNT_FLOAT64, &
                      HDFE_NOMERGE)
        if (err("Create: error defining Height",rc,-31).NE.0) &
           goto 999
        edge(1)=km
        rct = GDwrfld (gridId,'Height',start,stride,edge,levs_64)
        if (err("Create: error writing Height",rc,-38).NE.0)goto 999
      endif

      rct = GDdeffld (gridId,'Time','TIME',DFNT_FLOAT64,HDFE_NOMERGE)
      if (err("Create: error defining TIME",rc,-31).NE.0) goto 999
      edge(1)=1
      rct = GDwrfld (gridId,'Time',start,stride,edge,0.0)
      if (err("Create: error writing TIME",rc,-38).NE.0) goto 999
#endif

 
! Define global file attributes.  Check for strings containing only blanks.

#if defined(HDFEOS)
       i = 1
       rct = GDwrattr (gridId,missing,DFNT_FLOAT32,i,amiss_32)
       if (err("Create: error in GDwrattr",rc,-36).NE.0) goto 999
#endif

       rct = sfscatt (sd_id,missing,DFNT_FLOAT32,1,amiss_32)
       if (err("Create: error defining missing",rc,-36).NE.0) goto 999
       if (LEN_TRIM(conventions) .NE. 0) then
          rct = sfscatt (sd_id,'Conventions',DFNT_CHAR8,LEN_TRIM(conventions),&
                        TRIM(conventions))
       else
          print *, 'CFIO_Create: Warning, conventions string is blank.'
          rct = sfscatt (sd_id,'Conventions',DFNT_CHAR8,LEN(conventions),conventions)
       endif
       if (err("Create: error defining Conventions",rc,-36).NE.0) goto 999

!       if (LEN_TRIM(title) .NE. 0) then
!         rct = sfscatt (sd_id,'Title',DFNT_CHAR8,LEN_TRIM(title),TRIM(title))
!       else
!         print *, 'CFIO_Create: Warning, title string is blank.'
!         rct = sfscatt (sd_id,'Title',DFNT_CHAR8,LEN(title),title)
!       endif
!       if (err("Create: error defining Title",rc,-36).NE.0)goto 999

!       if (LEN_TRIM(source) .NE. 0) then
!         rct = sfscatt (sd_id,'Source',DFNT_CHAR8,LEN_TRIM(source),TRIM(source))
!       else
!         print *, 'CFIO_Create: Warning, source string is blank.'
!         rct = sfscatt (sd_id,'Source',DFNT_CHAR8,LEN(source),source)
!       endif
!       if (err("Create: error defining Source",rc,-36).NE.0)goto 999

!       if (LEN_TRIM(contact) .NE. 0) then
!         rct = sfscatt (sd_id,'Contact',DFNT_CHAR8,LEN_TRIM(contact),TRIM(contact))
!       else
!         print *, 'CFIO_Create: Warning, contact string is blank.'
!         rct = sfscatt (sd_id,'Contact',DFNT_CHAR8,LEN(contact),contact)
!       endif
!       if (err("Create: error defining Contact",rc,-36).NE.0) goto 999

!       if (LEN_TRIM(history) .NE. 0) then
!         rct = sfscatt (sd_id,'History',DFNT_CHAR8,LEN_TRIM(history),TRIM(history))
!       else
!         print *, 'CFIO_Create: Warning, history string is blank.'
!         rct = sfscatt (sd_id,'History',DFNT_CHAR8,LEN(history), history)
!       endif
!       if (err("Create: error defining History",rc,-36).NE.0) goto 999

999    continue

#if defined (HDFSD)
      fid = sd_id
#endif

#if defined (HDFEOS)
      rct = GDdetach (gridId)
#endif

      return
      end subroutine EOS_Create_


!------------------------------------------------------------------------------

      end module ESMF_CFIOEosMod

