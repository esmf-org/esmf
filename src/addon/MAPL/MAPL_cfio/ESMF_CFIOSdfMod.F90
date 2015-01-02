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
! !MODULE: ESMF_CFIOSdfMod.F90 - Source file for CFIO

       module ESMF_CFIOSdfMod
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
!  Feb2007  Baoyu Yin  Modified from ESMF_CFIOSdfMod.F90. This is the SDF
!                      module for CFIO.
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

      public :: ESMF_CFIOSdfFileCreate      ! Create a CFIO file for writing 
      public :: ESMF_CFIOSdfFileOpen        ! Open a CFIO file 
      public :: ESMF_CFIOSdfVarWrite        ! Write a variable to a file 
      public :: ESMF_CFIOSdfVarRead         ! Read a variable from a file
      public :: ESMF_CFIOSdfVarReadT        ! Read a variable from two files 
                                         ! with time interpolation
      public :: ESMF_CFIOSdfFileClose       ! Close an existing CFIO file. 

      interface ESMF_CFIOSdfVarWrite; module procedure   &
        ESMF_CFIOSdfVarWrite3D_,  &
        ESMF_CFIOSdfVarWrite2D_,  &
        ESMF_CFIOSdfVarWrite1D_
      end interface
                                                                                                 
      interface ESMF_CFIOSdfVarRead; module procedure   &
        ESMF_CFIOSdfVarRead3D_,  &
        ESMF_CFIOSdfVarRead2D_,  &
        ESMF_CFIOSdfVarRead1D_
      end interface

      interface ESMF_CFIOSdfVarReadT; module procedure   &
        ESMF_CFIOSdfVarReadT3D_,  &
        ESMF_CFIOSdfVarReadT2D_,  &
        ESMF_CFIOSdfVarReadT3D__, &
        ESMF_CFIOSdfVarReadT2D__
      end interface

!
!EOP
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfFileCreate -- Create a CFIO output file with meta data

! !INTERFACE:
      subroutine ESMF_CFIOSdfFileCreate (cfio, rc, expid)
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
                      ! -1 Time increment is 0
                      ! -2  allocate memory error
                      ! -3  Num of int/char/real elements and Cnt don't match
                      ! -12  error determining default precision
                      ! -18 incorrect time increment
                      ! -30 can't open file
                      ! -31 error from ncddef
                      ! -32 error from ncvdef (dimension variable)
                      ! -33 error from ncapt(c) (dimension attribute)
                      ! -34 error from ncvdef (variable)
                      ! -35  error from ncapt(c) (variable attribute)
                      ! -36  error from ncaptc/ncapt (global attribute)
                      ! -37  error from ncendf
                      ! -38  error from ncvpt (dimension variable)
                      ! -39 Num of real var elements and Cnt differ
                      ! -55  error from ncredf (enter define mode)
                      ! -56  error from ncedf (exit define mode)
!
! !DESCRIPTION:
!     Create a CFIO output file with meta data
!EOP
!------------------------------------------------------------------------------
       integer :: i, n, rtcode
       integer :: maxLen
       character(len=MLEN) :: fNameTmp     ! file name 
       integer :: date, begTime
       character(len=MLEN) :: fName

       call ESMF_CFIOGet(cfio, date=date, begTime=begTime, fName=fName, rc=rtcode)
       if (rtcode .ne. 0) print *, "Problems in ESMF_CFIOGet"
!      checking file name template
       if (present(expid)) then 
          call ESMF_CFIOSet(cfio, expid=expid)
          call strTemplate_(fNameTmp,fName,xid=expid,nymd=date, &
                            nhms=begTime, stat=rtcode)
       else
          call strTemplate_(fNameTmp,fName,nymd=date, nhms=begTime, stat=rtcode)
       end if

       if (trim(fNameTmp) .ne. trim(fName)) then
          call ESMF_CFIOSet(cfio, fNameTmplt=fName, fName=fNameTmp)
       end if

       call CFIO_Create_(cfio, rtcode)
       if (err("Error form CFIO_Create_",rtcode,rtcode) .lt. 0) then  
          if ( present(rc) ) rc = rtcode
          return
       end if

!      put global attributes
       call CFIO_PutCharAtt(cfio%fid, 'History', len(trim(cfio%history)),    &
                             cfio%history, rtcode )
       if (err("can't write History",rtcode,rtcode) .lt. 0) then  
          if ( present(rc) ) rc = rtcode
          return
       end if

       call CFIO_PutCharAtt(cfio%fid, 'Source', len(trim(cfio%source)),      &
                             cfio%source, rtcode )
       if (err("can't write Source",rtcode,rtcode) .lt. 0) then  
          if ( present(rc) ) rc = rtcode
          return
       end if

       call CFIO_PutCharAtt(cfio%fid, 'Title', len(trim(cfio%title)),        &
                             cfio%title, rtcode )
       if (err("can't write Title",rtcode,rtcode) .lt. 0) then  
          if ( present(rc) ) rc = rtcode
          return
       end if

       call CFIO_PutCharAtt(cfio%fid, 'Contact', len(trim(cfio%contact)),    &
                             cfio%contact, rtcode )
       if (err("can't write Contact",rtcode,rtcode) .lt. 0) then  
          if ( present(rc) ) rc = rtcode
          return
       end if

       call CFIO_PutCharAtt(cfio%fid,'Conventions',len(trim(cfio%convention))&
                             ,cfio%convention, rtcode )
       if (err("can't write Conventions",rtcode,rtcode) .lt. 0) then  
          if ( present(rc) ) rc = rtcode
          return
       end if

       call CFIO_PutCharAtt(cfio%fid,'Institution',                          &
                            len(trim(cfio%institution)),                     &
                            cfio%institution, rtcode )
       if (err("can't write Institution",rtcode,rtcode) .lt. 0) then  
          if ( present(rc) ) rc = rtcode
          return
       end if

       call CFIO_PutCharAtt(cfio%fid,'References',len(trim(cfio%references)),&
                             cfio%references, rtcode )
       if (err("can't write References",rtcode,rtcode) .lt. 0) then  
          if ( present(rc) ) rc = rtcode
          return
       end if

       call CFIO_PutCharAtt(cfio%fid,'Comment',len(trim(cfio%comment)),      &
                             cfio%comment, rtcode )
       if (err("can't write Comment",rtcode,rtcode) .lt. 0) then  
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
                rtcode=err("FileCreate: Num of int elements and Cnt differ"  &
                            ,-3,-3)
                if ( present(rc) ) rc = rtcode
                return
             end if

             call CFIO_PutIntAtt(cfio%fid, cfio%attIntNames(i),              &
                                 cfio%attIntCnts(i), cfio%attInts(i,:),      &
                                 cfio%prec, rtcode )
             if (err("error in CFIO_PutIntAtt",rtcode,rtcode) .lt. 0) then
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
                rtcode=err("FileCreate: Num of real elements and Cnt differ" &
                            ,-3,-3)
                if ( present(rc) ) rc = rtcode
                return
             end if
             call CFIO_PutRealAtt(cfio%fid, cfio%attRealNames(i),            &
                                 cfio%attRealCnts(i),                        &
                                 cfio%attReals(i,1:cfio%attRealCnts(i)),     &
                                 cfio%prec, rtcode )
             if (err("error in CFIO_PutRealAtt",rtcode,rtcode) .lt. 0) then
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
             call CFIO_PutCharAtt(cfio%fid, cfio%attCharNames(i),       &
                                 cfio%attCharCnts(i), cfio%attChars(i), &
                                 rtcode )
             if (err("error in CFIO_PutCharAtt",rtcode,rtcode) .lt. 0) then
                if ( present(rc) ) rc = rtcode
                return
             end if
          end do
       end if

       cfio%isOpen = .true.
 
       rtcode = 0
       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOSdfFileCreate

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfFileOpen -- open a CFIO file, and get CFIO meta data
!                                into a cfio Object.

! !INTERFACE:
      subroutine ESMF_CFIOSdfFileOpen (cfio, fmode, rc, expid, cyclic)

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
                         ! -36  error from ncaptc/ncapt (global attribute)
                         ! -39  error from ncopn (file open)
                         ! -40  error from ncvid
                         ! -41  error from ncdid or ncdinq (lat or lon)
                         ! -42  error from ncdid or ncdinq (lev)
                         ! -43  error from ncvid (time variable)
                         ! -47  error from ncdid or ncdinq (time)
                         ! -48  error from ncinq
                         ! -51  error from ncagtc/ncagt (global attribute)
                         ! -52  error from ncvinq
                         ! -53  error from ncagtc/ncagt
                         ! -57  error from ncanam
                         ! -58  error from ncainq

!
! !INPUT/OUTPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inout) :: cfio    ! a CFIO object
!
! !DESCRIPTION:
!     open a CFIO file, and get CFIO meta data into a cfio Object.
!EOP
!------------------------------------------------------------------------------
      integer :: ngatts, lm, i, ii, iv
      integer :: fileNameLen
      real*4 :: amiss
      real*4 :: vRange32(2)
      real*4, pointer :: lon(:), lat(:), lev(:)
      real*8, pointer :: lon_64(:), lat_64(:), lev_64(:)
      integer :: coXType = NCFLOAT
      integer :: coYType = NCFLOAT
      integer :: coZType = NCFLOAT
      character(len=MVARLEN) :: levunits
      character(len=MVARLEN) :: vAttName
      character(len=MVARLEN), pointer :: vname(:) 
      character(len=MLEN), pointer :: vtitle(:) 
      character(len=MVARLEN), pointer :: vunits(:) 
      integer, pointer :: kmvar(:)
      real, pointer :: valid_range(:,:), packing_range(:,:) 
      integer, pointer :: yyyymmdd(:), hhmmss(:)
      character(len=MLEN), pointer :: attNames(:)
      integer :: iCnt, rCnt, cCnt
      integer :: iMaxLen, rMaxLen, cMaxLen
      integer :: type, count, rtcode
      integer :: dimId
      integer :: varId
      integer :: datatype         ! variable type
      integer :: vtype            ! variable type
      integer :: nvDims           ! number of dimensions
      integer :: vDims(MAXVDIMS)  ! variable shape
      integer :: nvatts           ! number of attributes
      real*4, pointer :: rtmp(:)
      integer, pointer :: itmp(:)
      character(len=MVARLEN), pointer :: ctmp(:)
      logical :: esmf_file = .false.
      logical :: tmpLog
      logical :: new_grid
      integer :: nDims, allVars, recdim
      integer :: im, jm, km
      integer :: hour, minute, seconds
      integer :: fid, nVars, dimSize(4), myIndex
      character(len=MVARLEN) :: dimName(4), dimUnits(4), vnameTemp
      character(len=MVARLEN) :: nameAk, nameBk, namePtop
      integer :: loc1, loc2
      integer :: akid, bkid, ptopid
      integer :: icount
      real*4, pointer :: ak(:), bk(:)
      real*4 :: ptop
      real*4 :: scale, offset
      character, pointer ::  globalAtt(:)
      character(len=MLEN) :: fNameTmp     ! file name
      character(len=MVARLEN),dimension(:),pointer :: grads_vars
  
      call ncpopt(0)

      fNameTmp = ''                                                                                   
!     checking file name template
      if (present(expid)) cfio%expid = expid
      if (present(cyclic)) cfio%isCyclic = cyclic
      if (present(expid) .and. cfio%date .gt. 0 .and. cfio%begTime .ge. 0) then
         call strTemplate_(fNameTmp,cfio%fName,xid=expid,nymd=cfio%date, &
                           nhms=cfio%begTime, stat=rtcode)
      else
         if (cfio%date .gt. 0 .and. cfio%begTime .ge. 0) then
            call strTemplate_(fNameTmp,cfio%fName,nymd=cfio%date, &
                              nhms=cfio%begTime, stat=rtcode)
         else   
            if (present(expid)) then
               call strTemplate_(fNameTmp,cfio%fName,xid=expid, stat=rtcode)
            end if
         end if
      end if
      if (trim(fNameTmp) .ne. trim(cfio%fName) .and. len(trim(fNameTmp)) .gt. 0) then
         cfio%fNameTmplt = cfio%fName
         cfio%fName = fNameTmp
      end if

!     open a cfio file
      call CFIO_Open ( cfio%fName, fmode, cfio%fid, rtcode )
      if (err("problem in CFIO_Open",rtcode,rtcode) .lt. 0 ) then
         if ( present(rc) ) rc = rtcode
         return
      end if
      cfio%isOpen = .true.
      if (fmode == 0) then
         rc = 0
         return
      endif
      fid =cfio%fid

!     get grid information and global meta data
                                                                                          
      call CFIO_DimInquire (cfio%fid, im, jm, km, lm, &
                            cfio%mVars, ngatts, rtcode)
      if (err("CFIO_DimInquire failed",rtcode,rtcode) .lt. 0) then  
         if ( present(rc) ) rc = rtcode
         return
      end if
      cfio%tSteps = lm

      call ncinq (cfio%fid,nDims,allVars,ngatts,recdim,rtcode)
      if (err("FileOpen: ncinq failed",rtcode,-48) .NE. 0) then  
         if ( present(rc) ) rc = rtcode
         return
      end if

      allocate(cfio%varObjs(cfio%mVars))
      do i=1,cfio%mVars
         cfio%varObjs(i)=ESMF_CFIOVarInfoCreate(rc=rc)
      end do
      nVars = 0
      cfio%mGrids = 0
      do i=1,allVars
        call ncvinq (fid,i,vnameTemp,vtype,nvDims,vDims,nvAtts,rtcode)
        if (err("Inquire: variable inquire error",rtcode,-52) .NE. 0) then  
           if ( present(rc) ) rc = rtcode
           return
        end if
        if (nvDims .EQ. 1 .and. (index(vnameTemp, 'lon') .gt. 0 .or.  &
            index(vnameTemp, 'XDim:EOSGRID') .gt. 0) ) then
           coXType = vtype
           cfio%mGrids = cfio%mGrids + 1
        end if
        if (nvDims .EQ. 1 .and. (index(vnameTemp, 'lat') .gt. 0 .or.  &
            index(vnameTemp, 'YDim:EOSGRID') .gt. 0) ) then
           coYType = vtype
        end if
        if (nvDims .EQ. 1 .and. (index(vnameTemp, 'lev') .gt. 0 .or.  &
            index(vnameTemp, 'Height:EOSGRID') .gt. 0) ) then
           coZType = vtype
        end if

        cfio%varObjs(nVars+1)%timAve = .false.
        if (trim(vnameTemp) .eq. 'time_bnds') then 
           cfio%varObjs(nVars)%timAve = .true.
           cycle
        end if
        if (nvDims .EQ. 1) cycle
        nVars = nVars + 1
        cfio%varObjs(nVars)%vName = trim(vnameTemp)
        cfio%varObjs(nVars)%grid%km = 0
!        cfio%varObjs(nVars)%grid%km = 1
        cfio%varObjs(nVars)%grid%stnGrid = .false.
        do iv = 1, nvDims
           call ncdinq(fid, vDims(iv), dimName(iv), dimSize(iv), rtcode)
           if (err("problem in ncdinq",rtcode,-41) .NE. 0) then  
              if ( present(rc) ) rc = rtcode
              return
           end if
           if (index(dimName(iv),'station') .gt. 0) then
              cfio%varObjs(nVars)%grid%im = dimSize(iv)
              cfio%varObjs(nVars)%grid%jm = dimSize(iv)
              cfio%varObjs(nVars)%grid%stnGrid = .true.
              cycle
           end if
           varId = ncvid (fid, dimName(iv), rtcode)
           dimUnits(iv) = ' '
           call ncagtc(fid,varId,'units',dimUnits(iv),MAXCHR,rtcode)
           if (err("problem in ncagtc",rtcode,-53) .NE. 0) then  
              if ( present(rc) ) rc = rtcode
              return
           end if
           myIndex = IdentifyDim (dimName(iv), dimUnits(iv))
           if (myIndex .EQ. 0) then
              cfio%varObjs(nVars)%grid%im = dimSize(iv)
              if (.not. associated(cfio%varObjs(nVars)%grid%lon)) then
                 allocate(cfio%varObjs(nVars)%grid%lon(dimSize(iv)))
              end if
              allocate(lon(dimSize(iv)))
!              call ncvgt (fid, vDims(iv), 1, dimSize(iv), lon, rtcode)
              if ( coXType .eq. NCFLOAT ) then
                 call ncvgt (fid, varId, 1, dimSize(iv), lon, rtcode)
              else
                 allocate(lon_64(dimSize(iv)))
                 call ncvgt (fid, varId, 1, dimSize(iv), lon_64, rtcode)
                 lon =lon_64
                 deallocate(lon_64)
              end if
              if (err("problem in ncvgt",rtcode,-53) .NE. 0) then  
                 if ( present(rc) ) rc = rtcode
                 return
              end if
              cfio%varObjs(nVars)%grid%lon = lon
              deallocate(lon)
           end if
           if (myIndex .EQ. 1) then
              cfio%varObjs(nVars)%grid%jm = dimSize(iv)
              if (.not. associated(cfio%varObjs(nVars)%grid%lat)) then
                 allocate(cfio%varObjs(nVars)%grid%lat(dimSize(iv)))
              end if
              allocate(lat(dimSize(iv)))
              if ( coYType .eq. NCFLOAT ) then
                 call ncvgt (fid, varId, 1, dimSize(iv), lat, rtcode)
              else
                 allocate(lat_64(dimSize(iv)))
                 call ncvgt (fid, varId, 1, dimSize(iv), lat_64, rtcode)
                 lat = lat_64
                 deallocate(lat_64)
              end if
!              call ncvgt (fid, vDims(iv), 1, dimSize(iv), lat, rtcode)
!print *, "vDims(iv) varId: ", vDims(iv), varId
!print *, "dimName dimUnits: ", trim(dimName(iv)), trim(dimUnits(iv))
              if (err("problem in ncvgt",rtcode,-51) .NE. 0) then  
                 if ( present(rc) ) rc = rtcode
                 return
              end if
              cfio%varObjs(nVars)%grid%lat = lat
              deallocate(lat)
           end if
           if (myIndex .EQ. 2) then
              cfio%varObjs(nVars)%grid%km = dimSize(iv)
              call ncpopt(0)
              call ncagtc(fid,varId,'standard_name',                   &
                          cfio%varObjs(nVars)%grid%standardName,           &
                          MAXCHR, rtcode)
              if (rtcode /= 0) cfio%varObjs(nVars)%grid%standardName="pressure"
              if ( index(cfio%varObjs(nVars)%grid%standardName,        &
                   'atmosphere_sigma_coordinate') .gt. 0  .or.         &
                   index(cfio%varObjs(nVars)%grid%standardName,        &
                   'atmosphere_hybrid_sigma_pressure_coordinate' )     &
                   .gt.  0 ) then

                 call ncagtc(fid,varId,'formula_term',                 &
                          cfio%varObjs(nVars)%grid%formulaTerm,            &
                          MAXCHR, rtcode)
                 if ( index(cfio%varObjs(nVars)%grid%standardName,     &
                   'atmosphere_sigma_coordinate') .gt. 0 ) then
                    loc1 = index(cfio%varObjs(nVars)%grid%formulaTerm,'ptop:')
                    icount = loc1  + 5
                    do icount = loc1+5, len(cfio%varObjs(nVars)%grid%formulaTerm)
                      if (cfio%varObjs(nVars)%grid%formulaTerm(icount:icount) &
                           .ne. ' ') exit
                    end do
                    namePtop=trim(cfio%varObjs(nVars)%grid%formulaTerm    &
                         (icount:len(cfio%varObjs(nVars)%grid%formulaTerm)))
                    ptopid = ncvid(cfio%fid, trim(namePtop), rtcode)
                    if (rtcode .ne. 0) print *, "problem in getting ptopid in ncvid"
                    if (rtcode .eq. 0) call ncvgt(cfio%fid,ptopid,1, 1, ptop, rtcode)
                    if (rtcode .eq. 0) cfio%varObjs(nVars)%grid%ptop = ptop
                 end if
              end if
              if (index(cfio%varObjs(nVars)%grid%standardName,             &
                        'atmosphere_hybrid_sigma_pressure_coordinate')     &
                        .gt. 0)  then
                 loc1 = index(cfio%varObjs(nVars)%grid%formulaTerm,'a:')
                 loc2 = index(cfio%varObjs(nVars)%grid%formulaTerm,'b:')
                 icount = 0
                 do icount = loc1+2, loc2
                   if (cfio%varObjs(nVars)%grid%formulaTerm(icount:icount) &
                        .ne. ' ') exit
                 end do
                 nameAk=trim(cfio%varObjs(nVars)%grid%formulaTerm          &
                             (icount:loc2-1))
                 loc1 = index(cfio%varObjs(nVars)%grid%formulaTerm,'b:')
                 loc2 = index(cfio%varObjs(nVars)%grid%formulaTerm,'ps:')
                 do icount = loc1+2, loc2
                   if (cfio%varObjs(nVars)%grid%formulaTerm(icount:icount) &
                        .ne. ' ') exit
                 end do
                 nameBk=trim(cfio%varObjs(nVars)%grid%formulaTerm          &
                             (icount:loc2-1))
                 loc1 = index(cfio%varObjs(nVars)%grid%formulaTerm,'p0:')
                 icount = loc1  + 4
                 namePtop=trim(cfio%varObjs(nVars)%grid%formulaTerm        &
                         (icount:len(cfio%varObjs(nVars)%grid%formulaTerm)))

                 akid = ncvid(cfio%fid, trim(nameAk), rtcode)
                 if (rtcode .ne. 0) print *, "problem in getting akid in ncvid"

                 allocate(cfio%varObjs(nVars)%grid%ak                      &
                          (cfio%varObjs(nVars)%grid%km+1),                 &
                          ak(cfio%varObjs(nVars)%grid%km+1))
                 call ncvgt(cfio%fid,akid,1,cfio%varObjs(nVars)%grid%km+1, &
                            ak, rtcode)
                 if (rtcode .ne. 0) print *, "problem in getting ak in ncvgt"
                 cfio%varObjs(nVars)%grid%ak = ak
                 deallocate(ak)
                 bkid = ncvid(cfio%fid, trim(nameBk), rtcode)
                 if (rtcode .ne. 0) print *, "problem in getting bkid in ncvid"
                 allocate(cfio%varObjs(nVars)%grid%bk                      &
                          (cfio%varObjs(nVars)%grid%km+1),                 &
                          bk(cfio%varObjs(nVars)%grid%km+1))
                 call ncvgt(cfio%fid,bkid,1,cfio%varObjs(nVars)%grid%km+1, &
                            bk, rtcode)
                 if (rtcode .ne. 0) print *, "problem in getting bk in ncvgt"
                 cfio%varObjs(nVars)%grid%bk = bk
                 deallocate(bk)

                 ptopid = ncvid(cfio%fid, trim(namePtop), rtcode)
                 if (rtcode .ne. 0) print *, "problem in getting ptopid in ncvid"
                 call ncvgt(cfio%fid,ptopid,1, 1, ptop, rtcode)
                 if (rtcode .ne. 0) print *, "problem in getting ptop in ncvgt"
                 cfio%varObjs(nVars)%grid%ptop = ptop
             end if
              call ncpopt(0)
              call ncagtc(fid,varId,'coordinate',                      &
                          cfio%varObjs(nVars)%grid%coordinate,             &
                          MAXCHR, rtcode)
              if (rtcode .ne. 0) cfio%varObjs(nVars)%grid%coordinate = "pressure"  
              cfio%varObjs(nVars)%grid%levUnits = trim(dimUnits(iv))

              allocate(cfio%varObjs(nVars)%grid%lev(dimSize(iv)), &
                       lev(dimSize(iv)))
              call ncpopt(0)
              if ( coZType .eq. NCFLOAT ) then
                 call ncvgt (fid, varId, 1, dimSize(iv), lev, rtcode) 
!print *, "Lev from CFIO SDFFileOpen: ", lev
              else
                 allocate(lev_64(dimSize(iv)))
                 call ncvgt (fid, varId, 1, dimSize(iv), lev_64, rtcode) 
                 lev =lev_64
                 deallocate(lev_64)
              end if
              cfio%varObjs(nVars)%grid%lev = 0.0
              cfio%varObjs(nVars)%grid%lev = lev
!print *, "cfio%varObjs(nVars)%grid%lev from CFIO SDFFileOpen: ", cfio%varObjs(nVars)%grid%lev
              deallocate(lev)
           end if
        end do
        varId = ncvid (cfio%fid, cfio%varObjs(nVars)%vName, rtcode)
        if (rtcode .ne. 0) then 
           print *, "problem in getting varId in ncvid"
           if ( present(rc) ) rc = -40    
           return
        end if
        call ncagtc(fid,varId,'units',cfio%varObjs(nVars)%vunits,            &
                    MAXCHR,rtcode)
        if (rtcode .ne. 0) then
           print *, "ncagtc failed for units"
           if ( present(rc) ) rc = -53   
           return
        end if
        cfio%varObjs(nVars)%vtitle = ' '
        call ncpopt(0)
        call ncagtc(fid,varId,'long_name',cfio%varObjs(nVars)%vtitle,        &
                    MLEN,rtcode)
        call ncagtc(fid,varId,'standard_name',cfio%varObjs(nVars)%standardName,        &
                    MLEN,rtcode)
        if ( cfio%varObjs(nVars)%grid%km .gt. 0 ) then
            cfio%varObjs(nVars)%twoDimVar = .false.
        else
            cfio%varObjs(nVars)%twoDimVar = .true.
        end if
        call ncagt (fid, varId, '_FillValue', amiss, rtcode)
        if (rtcode .NE. 0) then
           call ncagt (fid, varId, 'missing_value', amiss, rtcode)
        end if
        cfio%varObjs(nVars)%amiss = amiss
        call ncpopt(0)
        call ncagt (fid, varId, 'scale_factor', scale, rtcode)
        if (rtcode .NE. 0) then
           cfio%varObjs(nVars)%scaleFactor = 1.0
        else
           cfio%varObjs(nVars)%scaleFactor = scale
        end if
        call ncpopt(0)
        call ncagt (fid, varId, 'add_offset', offset, rtcode)
        if (rtcode .NE. 0) then
           cfio%varObjs(nVars)%addOffset = 0.0
        else
           cfio%varObjs(nVars)%addOffset = offset
        end if
        call ncagt (fid, varId, 'vmin', vRange32(1), rtcode)
        if (rtcode .NE. 0) then
          cfio%varObjs(nVars)%validRange(1) = cfio%varObjs(nVars)%amiss
        else
          cfio%varObjs(nVars)%validRange(1) = vRange32(1)
        endif
        call ncagt (fid, varId, 'vmax', vRange32(2), rtcode)
        if (rtcode .NE. 0) then
          cfio%varObjs(nVars)%validRange(2) = cfio%varObjs(nVars)%amiss
        else
          cfio%varObjs(nVars)%validRange(2) = vRange32(2)
        endif
        
      end do
     
      call GetBegDateTime(fid,cfio%date,cfio%begTime,cfio%timeInc,rtcode)
      if (rtcode .ne. 0) then
         print *, "GetBegDateTime failed to get data/time/timeInc"
         if ( present(rc) ) rc = rtcode
         return
      end if

      hour = cfio%timeInc/3600
      minute = (cfio%timeInc-(3600*hour))/60
      seconds = cfio%timeInc-(3600*hour) - (60 * minute)
      cfio%timeInc = hour*10000 + minute*100 + seconds

      allocate(attNames(ngatts))
      attNames = " "
      call CFIO_GetAttNames ( cfio%fid, ngatts, attNames, rtcode )
      if (err("CFIO_GetAttNames failed",rtcode,rtcode) .lt. 0) then  
         if ( present(rc) ) rc = rtcode
         return
      end if
 
      iCnt = 0
      rCnt = 0
      cCnt = 0
      iMaxLen = 0
      rMaxLen = 0
      cMaxLen = 0

!     get how many int/real/char attributes in attNames
      do i =1, ngatts
         call CFIO_AttInquire (cfio%fid, attNames(i), type, count, rtcode)
         if (err("CFIO_AttInquire failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
         select case  (type)
            case ( 0 )
               iCnt = iCnt + 1
               if ( count .gt. iMaxLen ) iMaxLen = count
            case ( 1 )
               rCnt = rCnt + 1
               if ( count .gt. rMaxLen ) rMaxLen = count
            case ( 2 )
               cCnt = cCnt + 1
               if ( count .gt. cMaxLen ) cMaxLen = count
            case ( 3 )
               rCnt = rCnt + 1
               if ( count .gt. rMaxLen ) rMaxLen = count
            case ( 4 )
               iCnt = iCnt + 1
               if ( count .gt. iMaxLen ) iMaxLen = count
         end select
      end do

      cfio%nAttChar = cCnt
      cfio%nAttReal = rCnt
      cfio%nAttInt = iCnt

      allocate(cfio%attCharCnts(cCnt), cfio%attRealCnts(rCnt), &
               cfio%attIntCnts(iCnt))
      allocate(cfio%attCharNames(cCnt), cfio%attRealNames(rCnt), &
               cfio%attIntNames(iCnt))

      iCnt = 0
      rCnt = 0
      cCnt = 0
!     get attNames and count, then put them into a cfio obj
      do i =1, ngatts
         call CFIO_AttInquire (cfio%fid, attNames(i), type, count, rtcode)
         if (err("CFIO_AttInquire failed",rtcode,rtcode) .lt. 0) then  
            if ( present(rc) ) rc = rtcode
            return
         end if
         select case  (type)
            case ( 0 )
               iCnt = iCnt + 1
               cfio%attIntNames(iCnt) = attNames(i)         
               cfio%attIntCnts(iCnt) = count
            case ( 1 )
               rCnt = rCnt + 1
               cfio%attRealNames(rCnt) = attNames(i)
               cfio%attRealCnts(rCnt) = count
            case ( 2 )
               cCnt = cCnt + 1
               cfio%attCharNames(cCnt) = attNames(i)
               cfio%attCharCnts(cCnt) = count
            case ( 3 )
               rCnt = rCnt + 1
               cfio%attRealNames(rCnt) = attNames(i)
               cfio%attRealCnts(rCnt) = count
            case ( 4 )
               iCnt = iCnt + 1
               cfio%attIntNames(iCnt) = attNames(i)
               cfio%attIntCnts(iCnt) = count
         end select
      end do

      deallocate(attNames)

      allocate(cfio%attReals(rCnt, rMaxLen), cfio%attInts(iCnt, iMaxLen),    &
               cfio%attChars(cCnt))
!     get global integer attributes
      do i = 1, iCnt
         call CFIO_GetIntAtt(cfio%fid,cfio%attIntNames(i),cfio%attIntCnts(i) &
                            , cfio%attInts(i,:), rtcode)
         if (err("CFIO_GetIntAtt failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
      end do

!     get global real attributes
      do i = 1, rCnt
         call CFIO_GetRealAtt(cfio%fid,cfio%attRealNames(i),               &
                              cfio%attRealCnts(i),                         &
                              cfio%attReals(i,:), rtcode)
         if (err("CFIO_GetRealAtt",rtcode,rtcode) .lt. 0) then  
            if ( present(rc) ) rc = rtcode
            return
         end if
      end do
     
!     get global char attributes
      do i = 1, cCnt
         allocate(globalAtt(cfio%attCharCnts(i)))
         call CFIO_GetCharAtt(cfio%fid,cfio%attCharNames(i),   &
                              cfio%attCharCnts(i),             &
                              globalAtt, rtcode)
         if (err("GetCharAtt",rtcode,rtcode) .lt. 0) then  
            if ( present(rc) ) rc = rtcode
            return
         end if
!        cfio%attChars(i) can only hold MLEN characters.
         do ii = 1, cfio%attCharCnts(i)
            cfio%attChars(i)(ii:ii) = globalAtt(ii)
            if (ii .ge. MLEN) then
               print *,"global attribute ",trim(cfio%attCharNames(i)), &
                       " is longer than MLEN"
               exit
            end if
         end do
         cfio%attChars(i)(cfio%attCharCnts(i)+1:MLEN) = ' '
         if (index(cfio%attCharNames(i),'Conventions') .gt. 0 .and.  &
             index(cfio%attChars(i), 'ESMF') .gt. 0) esmf_file=.true.

         if (index(cfio%attCharNames(i),'History') .gt. 0)  &
            cfio%History=cfio%attChars(i)
         if (index(cfio%attCharNames(i),'Source') .gt. 0)  &
            cfio%source=cfio%attChars(i)
         if (index(cfio%attCharNames(i),'Title') .gt. 0)  &
            cfio%title=cfio%attChars(i)
         if (index(cfio%attCharNames(i),'Contact') .gt. 0)  &
            cfio%contact=cfio%attChars(i)
         if (index(cfio%attCharNames(i),'Conventions') .gt. 0)  &
            cfio%convention=cfio%attChars(i)
         if (index(cfio%attCharNames(i),'Institution') .gt. 0)  &
            cfio%institution=cfio%attChars(i)
         if (index(cfio%attCharNames(i),'References') .gt. 0)  &
            cfio%references=cfio%attChars(i)
         if (index(cfio%attCharNames(i),'Comment') .gt. 0)  &
            cfio%comment=cfio%attChars(i)
         deallocate(globalAtt)
      end do


!     get variable meta data
      do i = 1, cfio%mVars
         varId = ncvid (cfio%fid, cfio%varObjs(i)%vName, rtcode)
         if (err("ncvid failed for vName",rtcode,rtcode) .lt. 0) then   
            if ( present(rc) ) rc = -40
            return
         end if
         call ncvinq(cfio%fid, varId, cfio%varObjs(i)%vName, datatype, &
                     nvdims, vdims, nvatts, rtcode)
         if (err("ncvinq failed for vName",rtcode,rtcode) .lt. 0) then  
            if ( present(rc) ) rc = -52
            return
         end if
         iCnt = 0
         rCnt = 0
         cCnt = 0
         iMaxLen = 0
         rMaxLen = 0
         cMaxLen = 0

!        get variable int/real/char attribute count
         do iv =1, nvatts
            call ncanam (cfio%fid, varId, iv, vAttName, rtcode)
            if (err("ncanam failed for vName",rtcode,rtcode) .lt. 0) then  
               if ( present(rc) ) rc = -57   
               return
            end if
            call ncainq (cfio%fid,varId,vAttName,vtype,count,rtcode)
            if (err("ncainq failed for vName",rtcode,rtcode) .lt. 0) then
               if ( present(rc) ) rc = -58   
               return
            end if
            select case  (vtype)
               case ( NCSHORT )
                  iCnt = iCnt + 1
                  if ( count .gt. iMaxLen ) iMaxLen = count
               case ( NCFLOAT )
                  rCnt = rCnt + 1
                  if ( count .gt. rMaxLen ) rMaxLen = count
               case ( NCCHAR )
                  cCnt = cCnt + 1
                  if ( count .gt. cMaxLen ) cMaxLen = count
               case ( NCDOUBLE )
                  rCnt = rCnt + 1
                  if ( count .gt. rMaxLen ) rMaxLen = count
               case ( NCLONG )
                  iCnt = iCnt + 1
                  if ( count .gt. iMaxLen ) iMaxLen = count
            end select
         end do
                                                                                            
         cfio%varObjs(i)%nVarAttChar = cCnt
         cfio%varObjs(i)%nVarAttReal = rCnt
         cfio%varObjs(i)%nVarAttInt = iCnt
                                                                                            
         allocate(cfio%varObjs(i)%attCharCnts(cCnt),  &
                  cfio%varObjs(i)%attRealCnts(rCnt),  &
                  cfio%varObjs(i)%attIntCnts(iCnt))     
         allocate(cfio%varObjs(i)%attCharNames(cCnt), &
                  cfio%varObjs(i)%attRealNames(rCnt),&
                  cfio%varObjs(i)%attIntNames(iCnt))

         iCnt = 0
         rCnt = 0
         cCnt = 0
!        get variable int/real/char attribute names and counts
         do iv =1, nvatts
            call ncanam (cfio%fid, varId, iv, vAttName, rtcode)
            if (err("ncanam failed for vName",rtcode,rtcode) .lt. 0) then  
               if ( present(rc) ) rc = -57
               return
            end if
            call ncainq (cfio%fid,varId,vAttName,vtype,count,rtcode)
            if (err("ncainq failed for vName",rtcode,rtcode) .lt. 0) then   
               if ( present(rc) ) rc = -58
               return
            end if
            select case  (vtype)
               case ( NCSHORT )
                  iCnt = iCnt + 1
                  cfio%varObjs(i)%attIntNames(iCnt) = vAttName
                  cfio%varObjs(i)%attIntCnts(iCnt) = count   
               case ( NCFLOAT )
                  rCnt = rCnt + 1
                  cfio%varObjs(i)%attRealNames(rCnt) = vAttName
                  cfio%varObjs(i)%attRealCnts(rCnt) = count   
               case ( NCCHAR )
                  cCnt = cCnt + 1
                  cfio%varObjs(i)%attCharNames(cCnt) = vAttName
                  cfio%varObjs(i)%attCharCnts(cCnt) = count   
               case ( NCDOUBLE )
                  rCnt = rCnt + 1
                  cfio%varObjs(i)%attRealNames(rCnt) = vAttName
                  cfio%varObjs(i)%attRealCnts(rCnt) = count   
               case ( NCLONG )
                  iCnt = iCnt + 1
                  cfio%varObjs(i)%attIntNames(iCnt) = vAttName
                  cfio%varObjs(i)%attIntCnts(iCnt) = count   
            end select
         end do
   
         allocate(cfio%varObjs(i)%varAttReals(rCnt, rMaxLen), &
                  cfio%varObjs(i)%varAttInts(iCnt, iMaxLen),  &
                  cfio%varObjs(i)%varAttChars(cCnt))

!        get int variable attributes
         do ii = 1, iCnt
            allocate(itmp(cfio%varObjs(i)%attIntCnts(ii)))
            call ncagt(cfio%fid,varId,cfio%varObjs(i)%attIntNames(ii),&
                       itmp, rtcode)
            if (err("ncagt failed for attIntNames",rtcode,rtcode) .lt. 0) then
               if ( present(rc) ) rc = -53   
               return
            end if
            cfio%varObjs(i)%varAttInts(ii,1:cfio%varObjs(i)%attIntCnts(ii))&
                       = itmp
            deallocate(itmp)
         end do

!        get real variable attributes
         do ii = 1, rCnt
            allocate(rtmp(cfio%varObjs(i)%attRealCnts(ii)))
            call ncagt(cfio%fid,varId,cfio%varObjs(i)%attRealNames(ii),      &
                       rtmp, rtcode)
            if (err("ncagt failed for attRealNames",rtcode,rtcode) .lt. 0) then
               if ( present(rc) ) rc = -53
               return
            end if
            cfio%varObjs(i)%varAttReals(ii,1:cfio%varObjs(i)%attRealCnts(ii))&
                       = rtmp
            deallocate(rtmp)
         end do

!        get char variable attributes
         do ii = 1, cCnt
            call ncagtc(cfio%fid,varId,cfio%varObjs(i)%attCharNames(ii),     &
                       cfio%varObjs(i)%varAttChars(ii),                      &
                       cfio%varObjs(i)%attCharCnts(ii), rtcode)
            if (err("ncagt failed for attCharNames",rtcode,rtcode) .lt. 0) then
               if ( present(rc) ) rc = -53   
               return
            end if
            cfio%varObjs(i)%varAttChars(ii)  &
                 (cfio%varObjs(i)%attCharCnts(ii)+1:MLEN) = ' '             
         end do

      end do

!     set grids objects in a CFIO object
      allocate( cfio%grids(cfio%mGrids), stat = rtcode)
      cfio%grids(1) = cfio%varObjs(1)%grid
      if ( cfio%mGrids .eq. 1 .and. cfio%varObjs(1)%grid%km .eq. 0) &
         cfio%grids(1)%km = km
      
      if ( cfio%mGrids .gt. 1 ) then
        do i = 2, cfio%mGrids
           iCnt = 1
           do iv = 2, cfio%mVars
              new_grid = .true.
              iCnt = iCnt + 1
              do ii = 2, i
                if (cfio%varObjs(iv)%grid%im .eq. cfio%grids(ii-1)%im .and.  &
                  cfio%varObjs(iv)%grid%jm .eq. cfio%grids(ii-1)%jm .and.  &
                  cfio%varObjs(iv)%grid%km .eq. cfio%grids(ii-1)%km ) then 
                  new_grid = .false.
                end if
              end do
              if ( new_grid ) exit
           end do
           cfio%grids(i) = cfio%varObjs(iCnt)%grid
        end do
      end if 

      rtcode = 0
      if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOSdfFileOpen

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarWrite3D_ -- Write a variable to a output file

! !INTERFACE:
      subroutine ESMF_CFIOSdfVarWrite3D_(cfio, vName, field, date, curTime, &
                                      kbeg, kount, timeString, rc)
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
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -45  error from ncvpt
                         !  rc = -46  error from ncvgt
                         !  rc = -52  error from ncvinq
                         !  rc = -53  error from ncagtc/ncagt

!
! !DESCRIPTION:
!     Write a variable to file
!EOP
!------------------------------------------------------------------------------
      integer :: i, rtcode
      integer :: myKbeg, myKount
      integer :: myDate, myCurTime
      character(len=MLEN) :: fNameTmp     ! file name 
                                                                                         
      fNameTmp = ''
      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)

      if (len(trim(cfio%fNameTmplt)) .gt. 1) then
         call strTemplate_(fNameTmp,cfio%fNameTmplt,xid=cfio%expid,nymd=myDate, &
                           nhms=myCurTime, stat=rtcode)
         if (trim(fNameTmp) .ne. trim(cfio%fName)) then
            call ESMF_CFIOSdfFileClose(cfio)
            cfio%fName = fNameTmp
            call ESMF_CFIOSet(cfio, fName=cfio%fName)
            call ESMF_CFIOSet(cfio, date=myDate, begTime=myCurTime)
            if (len(trim(cfio%expid)) .gt. 0) then
               call ESMF_CFIOSdfFileCreate(cfio, expid=cfio%expid)
            else
               call ESMF_CFIOSdfFileCreate(cfio)
            end if
         end if
      end if
          
!
!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do

!     write 2D variable
      if ( cfio%varObjs(i)%twoDimVar ) then 
         call CFIO_PutVar (cfio%fid, vName, myDate, myCurTime,             &
                        cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                        0, 1, field, rtcode )
         if (err("CFIO_PutVar failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
!     write 3D variable
      else
         myKbeg = 1
         myKount = cfio%varObjs(i)%grid%km

         if ( present(kbeg) ) myKbeg = kbeg 
         if ( present(kount) ) myKount = kount

         call CFIO_PutVar (cfio%fid, vName, myDate, myCurTime,             &
                        cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                        myKbeg, myKount, field, rtcode )
         if (err("CFIO_PutVar failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
      end if

      if ( cfio%varObjs(i)%timAve ) then
         call writeBnds(cfio, vName, myDate, myCurTime, rtcode)
      end if

      if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOSdfVarWrite3D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarWrite1D_ -- Write a variable to a output file
                                                                                
! !INTERFACE:
      subroutine ESMF_CFIOSdfVarWrite1D_(cfio, vName, field, date, curTime,  &
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
!
! !DESCRIPTION:
!     Write a variable to file
!EOP
!------------------------------------------------------------------------------
      integer :: i, rtcode
      integer :: myDate, myCurTime
      character(len=MLEN) :: fNameTmp     ! file name
                                                                                         
      fNameTmp = ''
      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)
                                                                                         
      if (len(trim(cfio%fNameTmplt)) .gt. 1) then
         call strTemplate_(fNameTmp,cfio%fNameTmplt,xid=cfio%expid,nymd=myDate, &
                           nhms=myCurTime, stat=rtcode)
         if (trim(fNameTmp) .ne. trim(cfio%fName)) then
            call ESMF_CFIOSdfFileClose(cfio)
            cfio%fName = fNameTmp
            call ESMF_CFIOSet(cfio, fName=cfio%fName)
            call ESMF_CFIOSet(cfio, date=myDate, begTime=myCurTime)
            if (len(trim(cfio%expid)) .gt. 0) then
               call ESMF_CFIOSdfFileCreate(cfio, expid=cfio%expid)
            else
               call ESMF_CFIOSdfFileCreate(cfio)
            end if
         end if
      end if
!
!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do
                                                                                
!     NEED WORK HERE
      if (index(cfio%varObjs(i)%grid%gName,'station') .gt. 0) then
         call CFIO_SPutVar (cfio%fid, vName, myDate, myCurTime,      &
                  cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                  0, 1, field, rtcode )
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
                                                                                
      end subroutine ESMF_CFIOSdfVarWrite1D_
                                                                                
                                                                                
!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarWrite2D_ -- Write a variable to a output file
                                                                                
! !INTERFACE:
      subroutine ESMF_CFIOSdfVarWrite2D_(cfio, vName, field, date, curTime, &
                                      kbeg, kount, timeString, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio    ! a CFIO obj
      character(len=*), intent(in) :: vName     ! Variable name
      real, intent(in) :: field(:,:)            ! array contains data
      integer, intent(in), OPTIONAL :: date     ! yyyymmdd
      integer, intent(in), OPTIONAL :: curTime  ! hhmmss
      integer, intent(in), OPTIONAL :: kbeg     ! first level to write
      integer, intent(in), OPTIONAL :: kount    ! number of levels to write
      character(len=*), intent(in), OPTIONAL :: timeString
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
      character(len=MLEN) :: fNameTmp     ! file name
                                                                                         
      fNameTmp = ''
      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)
                                                                                         
      if (len(trim(cfio%fNameTmplt)) .gt. 1) then
         call strTemplate_(fNameTmp,cfio%fNameTmplt,xid=cfio%expid,nymd=myDate, &
                           nhms=myCurTime, stat=rtcode)
         if (trim(fNameTmp) .ne. trim(cfio%fName)) then
            call ESMF_CFIOSdfFileClose(cfio)
            cfio%fName = fNameTmp
            call ESMF_CFIOSet(cfio, fName=cfio%fName)
            call ESMF_CFIOSet(cfio, date=myDate, begTime=myCurTime)
            if (len(trim(cfio%expid)) .gt. 0) then
               call ESMF_CFIOSdfFileCreate(cfio, expid=cfio%expid)
            else
               call ESMF_CFIOSdfFileCreate(cfio)
            end if
         end if
      end if

!
!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do
                                                                                
!     write 2D variable
      if (index(cfio%varObjs(i)%grid%gName,'station') .gt. 0) then
         if ( cfio%varObjs(i)%twoDimVar ) then
            call CFIO_SPutVar (cfio%fid, vName, myDate, myCurTime,      &
                     cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                     0, 1, field, rtcode )
            if (err("CFIO_SPutVar failed",rtcode,rtcode) .lt. 0) then
               if ( present(rc) ) rc = rtcode
               return
            end if
         else
            myKbeg = 1
            myKount = cfio%varObjs(i)%grid%km
            if ( present(kbeg) ) myKbeg = kbeg
            if ( present(kount) ) myKount = kount

            call CFIO_SPutVar (cfio%fid, vName, myDate, myCurTime,          &
                     cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                     myKbeg, myKount, field, rtcode )
            if (err("CFIO_SPutVar failed",rtcode,rtcode) .lt. 0) then
               if ( present(rc) ) rc = rtcode
               return
            end if
         end if
      else
         call CFIO_PutVar (cfio%fid, vName, myDate, myCurTime,              &
                     cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,  &
                     0, 1, field, rtcode )
         if (err("CFIO_PutVar failed",rtcode,rtcode) .lt. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if

      end if
                                                         
      if ( cfio%varObjs(i)%timAve ) then
         call writeBnds(cfio, vName, myDate, myCurTime, rtcode)
      end if

      if ( present(rc) ) rc = rtcode
                                                                                
      end subroutine ESMF_CFIOSdfVarWrite2D_
                                                                                

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarRead3D_ -- Read a variable from an existing file

! !INTERFACE:
      subroutine ESMF_CFIOSdfVarRead3D_(cfio, vName, field, date, curTime, &
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
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -8  vname miss-match
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -46  error from ncvgt
                         !  rc = -48  error from ncinq
                         !  rc = -52  error from ncvinq
!
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------
      integer :: i, j, k, rtcode, curStep
      integer :: myKbeg, myKount
      integer :: myXbeg, myXount
      integer :: myYbeg, myYount
      integer :: myDate, myCurTime
      real, pointer :: tmp(:,:,:)          ! array contains data
      character(len=MLEN) :: fNameTmp     ! file name
                                                                                         
      fNameTmp = ''
                                                                                         
      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)

      if (len(trim(cfio%fNameTmplt)) .gt. 1) then
         call strTemplate_(fNameTmp,cfio%fNameTmplt,xid=cfio%expid,nymd=MYdate, &
                           nhms=MYcurTime, stat=rtcode)
         if (trim(fNameTmp) .ne. trim(cfio%fName)) then
            call ESMF_CFIOSdfFileClose(cfio)
            cfio%fName = fNameTmp
            if (len(trim(cfio%expid)) .gt. 0) then
               call ESMF_CFIOSdfFileOpen(cfio, 1, expid=cfio%expid, cyclic=cfio%isCyclic)
            else
               call ESMF_CFIOSdfFileOpen(cfio, 1, cyclic=cfio%isCyclic)
            end if
         end if
      end if

!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do

!     make sure we match something
      if ( i > cfio%mVars ) then
         if (trim(vName) .ne. trim(cfio%varObjs(i-1)%vName) ) then
            print*,'ESMF_CFIOSdfVarRead3D: Variable name mismatch for ',trim(vName), ' in file ',trim(cfio%fName)
            rc = -8
            return
         endif
      endif

      myKbeg = 1
      myKount = 1

!     read 3D variable
      if ( cfio%varObjs(i)%grid%km .gt. 1 .and.                          &
           (.not. cfio%varObjs(i)%twoDimVar) ) then

         myKbeg = 1
         myKount = cfio%varObjs(i)%grid%km
         if ( present(kbeg) ) myKbeg = kbeg
         if ( present(kount) ) myKount = kount

         allocate(tmp(cfio%varObjs(i)%grid%im,cfio%varObjs(i)%grid%jm,   &
               myKount), stat=rtcode)
         if (rtcode /= 0) print *, "cannot allocate tmp in ESMF_CFIOSdfVarRead3D"

         call CFIO_GetVar(cfio%fid,vName,mydate,mycurTime,                   &
                       cfio%varObjs(i)%grid%im,                          &
                       cfio%varObjs(i)%grid%jm,myKbeg,myKount,           &
                       cfio%tSteps, tmp, cfio%isCyclic, rtcode )
         if (rtcode .ne. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
!     read 2D variable
      else
         allocate(tmp(cfio%varObjs(i)%grid%im,cfio%varObjs(i)%grid%jm,1),&
                  stat=rtcode)
         if (rtcode /= 0) print *, "cannot allocate tmp in ESMF_CFIOSdfVarRead3D"
         
         call CFIO_GetVar(cfio%fid,vName,mydate,MYcurTime,                   &
                       cfio%varObjs(i)%grid%im,                          &
                       cfio%varObjs(i)%grid%jm, 0, 1, cfio%tSteps, tmp,  &
                       cfio%isCyclic, rtcode )
         if (rtcode .ne. 0) then
            if ( present(rc) ) rc = rtcode
            return
         end if
      end if

      myXbeg = 1
      myXount = cfio%varObjs(i)%grid%im
      myYbeg = 1
      myYount = cfio%varObjs(i)%grid%jm
      if ( present(xBeg) ) myXbeg=xBeg
      if ( present(yBeg) ) myYbeg=yBeg
      if ( present(xCount) ) myXount = xCount
      if ( present(yCount) ) myYount = yCount

      if (associated(field) ) then
         if (size(field,1) < myXount .or. size(field,2) < myYount .or. size(field,3) < myKount) then 
            print *, "Field is not Large Enough in VarRead3D"
            if (size(field,1) < myXount) rtcode = -4
            if (size(field,2) < myXount) rtcode = -5
            if (size(field,3) < myKount) rtcode = -3
            if ( present(rc) ) rc = rtcode
            return
         end if
      else
         allocate(field(myXount,myYount,myKount),stat=rtcode)
      end if
!      allocate(field(myXount,myYount,myKount), stat=rtcode)
      if (rtcode /= 0) print *, "cannot allocate field in ESMF_CFIOSdfVarRead3D_"
      do k = 1, myKount
         do j = 1, myYount
           do i = 1, myXount
              field(i,j,k) = tmp(myXbeg+i-1,myYbeg+j-1,k)
           end do
         end do
      end do

      deallocate(tmp)
      if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOSdfVarRead3D_ 

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarRead2D_ -- Read a variable from an existing file
                                                                                
! !INTERFACE:
      subroutine ESMF_CFIOSdfVarRead2D_(cfio, vName, field, date, curTime, &
                                     kbeg, kount, xBeg, xCount, yBeg,   &
                                     yCount, timeString, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inout) :: cfio         ! a CFIO obj
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
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -8  vname miss-match
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -46  error from ncvgt
                         !  rc = -48  error from ncinq
                         !  rc = -52  error from ncvinq

!
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------
      integer :: i, j, k, rtcode, curStep
      integer :: myKbeg, myKount
      integer :: myXbeg, myXount
      integer :: myYbeg, myYount
      integer :: myDate, myCurTime
      real, pointer :: tmp(:,:)          ! array contains data
      character(len=MLEN) :: fNameTmp     ! file name
                                                                                              
      fNameTmp = ''

      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)
                                                                                              
      if (len(trim(cfio%fNameTmplt)) .gt. 1) then
         call strTemplate_(fNameTmp,cfio%fNameTmplt,xid=cfio%expid,nymd=MYdate, &
                           nhms=MYcurTime, stat=rtcode)
         if (trim(fNameTmp) .ne. trim(cfio%fName)) then
            call ESMF_CFIOSdfFileClose(cfio)
            cfio%fName = fNameTmp
!            call ESMF_CFIOSet(cfio, fName=cfio%fName)
            if (len(trim(cfio%expid)) .gt. 0) then
               call ESMF_CFIOSdfFileOpen(cfio, 1, expid=cfio%expid, cyclic=cfio%isCyclic)
            else
               call ESMF_CFIOSdfFileOpen(cfio, 1, cyclic=cfio%isCyclic)
            end if
         end if
      end if

!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do

!     make sure we match something
      if ( i > cfio%mVars ) then
         if (trim(vName) .ne. trim(cfio%varObjs(i-1)%vName) ) then
            print*,'ESMF_CFIOSdfVarRead2D: Variable name mismatch for ',trim(vName), ' in file ',trim(cfio%fName)
            rc = -8
            return
         endif
      endif
                                                                                
      myXbeg = 1
      myXount = cfio%varObjs(i)%grid%im
      myYbeg = 1
      myYount = cfio%varObjs(i)%grid%jm
      myKbeg = 1
      myKount = cfio%varObjs(i)%grid%km
      if ( present(xBeg) ) myXbeg=xBeg
      if ( present(yBeg) ) myYbeg=yBeg
      if ( present(kbeg) ) myKbeg = kbeg
      if ( present(kount) ) myKount = kount
      if ( present(xCount) ) myXount = xCount
      if ( present(yCount) ) myYount = yCount

!     read 2D variable
      if ( cfio%varObjs(i)%twoDimVar .and.                              &
              .not. cfio%varObjs(i)%grid%stnGrid) then
        allocate(tmp(cfio%varObjs(i)%grid%im,cfio%varObjs(i)%grid%jm),  &
               stat=rtcode)
        call CFIO_GetVar(cfio%fid,vName,MYdate,MYcurTime,                   &
                    cfio%varObjs(i)%grid%im,                            &
                    cfio%varObjs(i)%grid%jm, 0, 1, cfio%tSteps, tmp,    &
                    cfio%isCyclic, rtcode )
        if (err("CFIO_GetVar failed",rtcode,rtcode) .lt. 0) then  
           if ( present(rc) ) rc = rtcode
           return
        end if
 
        if(associated(field)) then
           if (size(field,1) < myXount .or. size(field,2) < myYount) then 
              print *, "Field is not Large Enough in VarRead2D"
              if (size(field,1) < myXount) rtcode = -4
              if (size(field,2) < myXount) rtcode = -5
              if ( present(rc) ) rc = rtcode
              return
           end if
        else
           allocate(field(myXount,myYount))
        end if
        do j = 1, myYount
           do i = 1, myXount
              field(i,j) = tmp(myXbeg+i-1,myYbeg+j-1)
           end do
        end do

      else
        if (cfio%varObjs(i)%twoDimVar ) then
           allocate(tmp(cfio%varObjs(i)%grid%im,1), stat=rtcode)
           call CFIO_SGetVar(cfio%fid,vName,MYdate,MYcurTime,               &
                    cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,   &
                    0,1, cfio%tSteps, tmp, cfio%isCyclic, rtcode )
           if (err("CFIO_SGetVar failed",rtcode,rtcode) .lt. 0) then  
              if ( present(rc) ) rc = rtcode
              return
           end if
           if(associated(field)) then
              if (size(field,1) < myXount .or. size(field,2) < 1) then 
                 print *, "Field is not Large Enough in VarRead2D"
                 if (size(field,1) < myXount) rtcode = -4
                 if (size(field,2) < 1) rtcode = -5
                 if ( present(rc) ) rc = rtcode
                 return
              end if
           else
              allocate(field(myXount,1))
           end if
           do i = 1, myXount
              field(i,1) = tmp(myXbeg+i-1,1)
           end do

        else
           allocate(tmp(cfio%varObjs(i)%grid%im,myKount),stat=rtcode)
           call CFIO_SGetVar(cfio%fid,vName,MYdate,MYcurTime,               &
                    cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,   &
                    myKbeg, myKount, cfio%tSteps, tmp, cfio%isCyclic, rtcode )
           if (err("CFIO_GetVar failed",rtcode,rtcode) .lt. 0) then  
              if ( present(rc) ) rc = rtcode
              return
           end if
           if(associated(field)) then
              if (size(field,1) < myXount .or. size(field,2) < myKount) then 
                 print *, "Field is not Large Enough in VarRead2D"
                 if (size(field,1) < myXount) rtcode = -4
                 if (size(field,2) < myKount) rtcode = -3
                 if ( present(rc) ) rc = rtcode
                 return
              end if
           else
              allocate(field(myXount,myKount))
           end if
           do k = 1, myKount
             do i = 1, myXount
                field(i,k) = tmp(myXbeg+i-1,k)
             end do
           end do

        end if
      end if
 
      deallocate(tmp)
                                                                                
      if ( present(rc) ) rc = rtcode
                                                                                
      end subroutine ESMF_CFIOSdfVarRead2D_
                                                                                
!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarRead1D_ -- Read a variable from an existing file
                                                                                
! !INTERFACE:
      subroutine ESMF_CFIOSdfVarRead1D_(cfio, vName, field, date, curTime, &
                                     xBeg, xCount, timestring, rc)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio         ! a CFIO obj
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
!
! !DESCRIPTION:
!     Read a variable from an existing file
!EOP
!------------------------------------------------------------------------------

      integer :: i, j, rtcode
      integer :: myXbeg, myXount      
      integer :: myDate, myCurTime
      real, pointer :: tmp(:)          ! array contains data
      character(len=MLEN) :: fNameTmp     ! file name
                                                                                              
      fNameTmp = ''
      if ( present(date) ) myDate = date
      if ( present(curTime) ) myCurTime = curTime
      if ( present(timeString) ) call strToInt(timeString,myDate,myCurTime)
                                                                                              
      if (len(trim(cfio%fNameTmplt)) .gt. 1) then
         call strTemplate_(fNameTmp,cfio%fNameTmplt,xid=cfio%expid,nymd=MYdate, &
                           nhms=MYcurTime, stat=rtcode)
         if (trim(fNameTmp) .ne. trim(cfio%fName)) then
            call ESMF_CFIOSdfFileClose(cfio)
            cfio%fName = fNameTmp
!            call ESMF_CFIOSet(cfio, fName=cfio%fName)
            if (len(trim(cfio%expid)) .gt. 0) then
               call ESMF_CFIOSdfFileOpen(cfio, 1, expid=cfio%expid, cyclic=cfio%isCyclic)
            else
               call ESMF_CFIOSdfFileOpen(cfio, 1, cyclic=cfio%isCyclic)
            end if
         end if
      end if

                                                                                
!     make sure user provides the right variable name
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do
                                                                                
      myXbeg = 1
      myXount = cfio%varObjs(i)%grid%im

      if (present(xBeg)) myXbeg = xBeg
      if (present(xCount)) myXount = xCount

!     read 1D variable
      allocate(tmp(cfio%varObjs(i)%grid%im), stat=rtcode)
      call CFIO_SGetVar(cfio%fid,vName,MYdate,MYcurTime,               &
               cfio%varObjs(i)%grid%im, cfio%varObjs(i)%grid%jm,   &
               0,1, cfio%tSteps, tmp, cfio%isCyclic, rtcode )

      do i = 1, myXount
         field(i) = tmp(myXbeg+i-1)
      end do

      deallocate(tmp)
                                                                                
      if ( present(rc) ) rc = rtcode
                                                                                
      end subroutine ESMF_CFIOSdfVarRead1D_
                                                                                

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfFileClose -- close an open CFIO stream

! !INTERFACE:
      subroutine ESMF_CFIOSdfFileClose (cfio, rc)
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

       if ( cfio%isOpen ) then 
          call CFIO_Close(cfio%fid, rtcode)
          if (rtcode .ne. 0) then 
             print *, "CFIO_Close failed"
          else
             cfio%isOpen = .false.
          end if
       else
          rtcode = 0
       end if

       if ( present(rc) ) rc = rtcode

      end subroutine ESMF_CFIOSdfFileClose


!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_Create_ -- Creates a DAO gridded file for writing
! 
! !DESCRIPTION: This routine is used to open a new file for a CFIO stream.
!
!, im, jm, km !INTERFACE:
!
      subroutine CFIO_Create_ ( cfio, rc )
!
! !USES:
!
      Implicit NONE  
!
! !INPUT PARAMETERS: 
!
!
! !OUTPUT PARAMETERS:
!
      integer        fid     ! File handle
      integer        rc      ! Error return code:
                             ! 0  All is well
                             ! -1 Time increment is 0
                             ! -18 incorrect time increment
                             ! -30 can't open file
                             ! -31 error from ncddef
                             ! -32 error from ncvdef (dimension variable)
                             ! -33 error from ncapt(c) (dimension attribute)
                             ! -34 error from ncvdef (variable)
                             ! -35  error from ncapt(c) (variable attribute)
                             ! -36  error from ncaptc/ncapt (global attribute)
                             ! -37  error from ncendf
                             ! -38  error from ncvpt (dimension variable)
                             ! -39 Num of real var elements and Cnt differ
                             ! -40 error setting deflate compression routine
                             ! -41 error setting fletcher checksum routine

!
! !INPUT/OUTPUT PARAMETERS:
!
     type(ESMF_CFIO), intent(inout) :: cfio 
!
! !REVISION HISTORY: 
!
!EOP
!-------------------------------------------------------------------------

      ! REAL*4 variables for 32-bit output to netCDF file.

      integer :: im, jm, km, tm, nst
      real*8, pointer :: lon_64(:), lat_64(:), levs_64(:)
      real*8, pointer :: lon2_64(:), lat2_64(:)
      character(len=MVARLEN) :: levunits
      integer :: yyyymmdd_beg, hhmmss_beg, timinc
      real :: missing_val
      integer :: nvars
      character(len=MLEN), pointer :: vname(:)
      character(len=MVARLEN), pointer :: vtitle(:)
      character(len=MVARLEN), pointer :: vunits(:)
      integer, pointer :: kmvar(:), station(:)
      real, pointer :: valid_range(:,:), packing_range(:,:)
      integer, pointer :: akid(:), bkid(:), ptopid(:)
      integer :: prec
      integer, pointer ::  vid(:)

      real*4 amiss_32
      real*4 scale_32, offset_32
      real*4 high_32,low_32
      real*4, pointer :: ak_32(:), bk_32(:), layer(:)
      real*4 :: ptop_32(1)
      integer i, j
      integer timeid, timedim
      integer, pointer :: latid(:), lonid(:), stationid(:)
      integer, pointer :: levid(:), layerid(:)
      integer, pointer :: latdim(:), londim(:), stationdim(:)
      integer, pointer :: levdim(:), layerdim(:)
      integer, pointer :: gDims3D(:,:), gDims2D(:,:)
      integer dims3D(4), dims2D(3), dims1D(1), ptopdim
      integer corner(1), edges(1)
      integer :: corner2d(2), edges2d(2)
      integer, pointer :: lat2id(:), lon2id(:)
!      integer corner(4), edges(4)
      character*80 timeUnits 
      logical surfaceOnly
      character*8 strBuf
      character*14 dateString
      integer year,mon,day,hour,minute,sec
      integer count
      integer maxLen
      integer rtcode
      logical :: aveFile = .false.
      character cellMthd
!      real*4 bndsdata(2)
      integer bndsid, dimsbnd(2), bndsdim
      integer ig
      integer ndim
      character cig
      integer nDefaultChunksize(4) ! Set Chunksize to im,jm,1,1 by default

! Variables for packing

      integer*2 amiss_16
      real*4, pointer ::  pRange_32(:,:),vRange_32(:,:)
      logical packflag
      integer min
! Set metadata strings.  These metadata values are specified in the 
! COARDS conventions

      character (len=50), pointer :: lonDimName
      character (len=50), pointer :: latDimName
      character (len=50), target :: lonName = "longitude"
      character (len=50), target :: lon2Name = "Nominal longitude at South pole"
      character (len=50) :: lonUnits = "degrees_east"
      character (len=50), target :: latName = "latitude"
      character (len=50), target :: lat2Name = "Nominal latitude at dateline"
      character (len=50) :: latUnits = "degrees_north"
      character (len=50) :: levName = "vertical level"
!                           levUnits: specified by user in argument list
      character (len=50) :: layerName = "edges"
      character (len=50) :: layerUnits = "layer"
      character (len=50) :: timeName = "time"
!                           timeUnits: string is built below
      character (len=50) :: coordinatesName
      integer :: iCnt
      real*4, pointer :: realVarAtt(:)
      integer, pointer :: intVarAtt(:)
      real*4 :: scale_factor, add_offset
      character (len=50) :: nameLatDim, nameLonDim
      character (len=50) :: nameLat, nameLon, nameLev, nameEdge
      character (len=50) :: nameAk, nameBk, namePtop, nameStation 
      logical  bTimeSet
      integer :: sz_lon, sz_lat

      nvars = cfio%mVars
      yyyymmdd_beg = cfio%date
      hhmmss_beg = cfio%begTime
      timinc = cfio%timeInc
      missing_val = cfio%varObjs(1)%amiss
      allocate(vname(nvars), vtitle(nvars), vunits(nvars), kmvar(nvars), &
            valid_range(2,nvars), packing_range(2,nvars), vid(nvars),    &
            vRange_32(2,nvars), pRange_32(2,nvars), stat = rtcode)

      allocate(latid(cfio%mGrids), lonid(cfio%mGrids),                   &
               lat2id(cfio%mGrids), lon2id(cfio%mGrids),                 &
               levid(cfio%mGrids), layerid(cfio%mGrids),                 &
               latdim(cfio%mGrids), londim(cfio%mGrids),                 &
               levdim(cfio%mGrids), layerdim(cfio%mGrids),               &
               akid(cfio%mGrids),bkid(cfio%mGrids),ptopid(cfio%mGrids),  &
               gDims3D(4,cfio%mGrids), gDims2D(3,cfio%mGrids),           &
               stationdim(cfio%mGrids), stationid(cfio%mGrids) )

      do i=1,nvars
         vname(i) = cfio%varObjs(i)%vName
         vtitle(i) = cfio%varObjs(i)%vTitle
         vunits(i) = cfio%varObjs(i)%vUnits
         kmvar(i) = cfio%varObjs(i)%grid%km
         if ( cfio%varObjs(i)%twoDimVar ) kmvar(i) = 0
         valid_range(1, i) = cfio%varObjs(i)%validRange(1)
         valid_range(2, i) = cfio%varObjs(i)%validRange(2)
         packing_range(1, i) = cfio%varObjs(i)%packingRange(1)
         packing_range(2, i) = cfio%varObjs(i)%packingRange(2)
         if ( cfio%varObjs(i)%timAve ) then
            aveFile = .true.
            cellMthd = cfio%varObjs(i)%aveMethod
         end if
      enddo

      do j=1,nvars
        do i=1,2
           vRange_32(i,j) = valid_range(i,j)
           pRange_32(i,j) = packing_range(i,j)
        enddo
      enddo

      amiss_32 = cfio%varObjs(1)%amiss
      amiss_16 = PACK_FILL

! Variable initialization

      surfaceOnly = .TRUE.

! Basic error-checking.

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

! Make NetCDF errors non-fatal, and do not warning messages.

      call ncpopt(0)

! Create the new NetCDF file. [ Enter define mode. ]

#if defined(HAS_NETCDF4)
      if (isFileExtensionNetCDF4(trim(cfio%fName))) then
         rc = nf_create (trim(cfio%fName), IOR(NF_CLOBBER,NF_NETCDF4), fid)
      else
         rc = nf_create (trim(cfio%fName), IOR(IOR(NF_CLOBBER,NF_NETCDF4),NF_CLASSIC_MODEL), fid) !NETCDF4/HDF5
      end if
#else
      fid = nccre (trim(cfio%fName), NCCLOB, rc)
#endif

      if (err("Create: can't create file",rc,-30) .LT. 0) return

!     Time Variable is defaulted to UNLIMITED
      bTimeSet = .FALSE.
      tm = 0
! Convert double-precision output variables to single-precision
   do ig = 1, cfio%mGrids
      im = cfio%grids(ig)%im
      jm = cfio%grids(ig)%jm
      km = cfio%grids(ig)%km
      tm = max(tm,cfio%grids(ig)%tm)
     
      if ( index(cfio%grids(ig)%gName, 'station') .gt. &
           0 ) then
         if (im .ne. jm) rtcode = err("It isn't station grid",-1,-1)
         nst = im
      end if

      levunits = trim(cfio%grids(ig)%levUnits)

      allocate(station(im))
      do i=1,im
         station(i) = i
      enddo

! Define dimensions.

      lonDimName => lonName
      latDimName => latName
      if (cfio%grids(ig)%twoDimLat) then
         nameLonDim = 'LON'
         nameLatDim = 'LAT'
         coordinatesName = trim(nameLonDim) // ' ' // trim(nameLatDim)
         lonDimName => lon2Name
         latDimName => lat2Name
      endif
      if ( ig .eq. 1 ) then
         if (cfio%mGrids .eq. 1) then
            nameLon = 'lon'
            nameLat = 'lat'
            nameLev = 'lev'
            nameEdge = 'edges'
            nameStation = 'station'
         else
            nameLon = 'lon0'
            nameLat = 'lat0'
            nameLev = 'lev0'
            nameEdge = 'edges0'
            nameStation = 'station0'
         end if
      else
        write (cig,"(I1)") ig-1
        nameLon = 'lon'//cig
        nameLat = 'lat'//cig
        nameLev = 'lev'//cig
        nameEdge = 'edges'//cig
        nameStation = 'station'//cig
      end if

      if (index(cfio%grids(ig)%gName,'station') .gt. 0) then
         stationdim(ig) = ncddef (fid, nameStation, im, rc)
         if (err("Create: error defining station",rc,-31) .LT. 0) return
!         londim(ig) = ncddef (fid, nameLon, im, rc)
!         if (err("Create: error defining lon",rc,-31) .LT. 0) return
!         latdim(ig) = ncddef (fid, nameLat, jm, rc)
!         if (err("Create: error defining lat",rc,-31) .LT. 0) return
      else
         londim(ig) = ncddef (fid, nameLon, im, rc)
         if (err("Create: error defining lon",rc,-31) .LT. 0) return
         latdim(ig) = ncddef (fid, nameLat, jm, rc)
         if (err("Create: error defining lat",rc,-31) .LT. 0) return
      end if

      if (.NOT. surfaceOnly) then
        levdim(ig) = ncddef (fid, nameLev, km, rc)
        if (err("Create: error defining lev",rc,-31) .LT. 0) return
      endif
      if ( trim(cfio%grids(ig)%standardName) .eq. &
           'atmosphere_hybrid_sigma_pressure_coordinate' ) then
         layerdim(ig) = ncddef (fid, nameEdge, km+1, rc)
         if (err("Create: error defining edges",rc,-31) .LT. 0) return
      endif


! Define dimension variables.

      if (index(cfio%grids(ig)%gName,'station') .gt. 0) then
!         stationid(ig) = ncvdef (fid, nameStation, NCDOUBLE, 1,       &
!                                 stationdim(ig), rc)
!         if (err("Create: error defining station",rc,-32) .LT. 0) return
         lonid(ig) = ncvdef (fid, nameLon, NCDOUBLE, 1, stationdim(ig), rc)
         if (err("Create: error creating lon",rc,-32) .LT. 0) return
         latid(ig) = ncvdef (fid, nameLat, NCDOUBLE, 1, stationdim(ig), rc)
         if (err("Create: error creating lat",rc,-32) .LT. 0) return
      else
         lonid(ig) = ncvdef (fid, nameLon, NCDOUBLE, 1, londim(ig), rc)
         if (err("Create: error creating lon",rc,-32) .LT. 0) return
         latid(ig) = ncvdef (fid, nameLat, NCDOUBLE, 1, latdim(ig), rc)
         if (err("Create: error creating lat",rc,-32) .LT. 0) return
         if (cfio%grids(ig)%twoDimLat) then
            dims2D(2) = latdim(ig)
            dims2D(1) = londim(ig)
            lon2id(ig) = ncvdef (fid, nameLonDim, NCDOUBLE, 2, dims2D(1:2), rc)
            if (err("Create: error creating lon2d",rc,-32) .LT. 0) return
            lat2id(ig) = ncvdef (fid, nameLatDim, NCDOUBLE, 2, dims2D(1:2), rc)
            if (err("Create: error creating lat2d",rc,-32) .LT. 0) return
         end if
      end if

      if (.NOT. surfaceOnly) then
        levid(ig) = ncvdef (fid, nameLev, NCDOUBLE, 1, levdim(ig), rc)
        if (err("Create: error creating lev",rc,-32) .LT. 0) return
      endif
      if ( trim(cfio%grids(ig)%standardName) .eq. &
           'atmosphere_hybrid_sigma_pressure_coordinate' ) then
         layerid(ig) = ncvdef (fid, nameEdge, NCDOUBLE, 1, layerdim(ig), rc)
         if (err("Create: error creating edges",rc,-32) .LT. 0) return
      endif

! Set attributes for dimensions.

      call ncaptc (fid,lonid(ig),'long_name',NCCHAR,LEN_TRIM(lonDimName), &
                  lonDimName,rc)
      if (err("Create: error creating lon attribute",rc,-33) .LT. 0) &
        return
      call ncaptc (fid,lonid(ig),'units',NCCHAR,LEN_TRIM(lonUnits), &
                  lonUnits,rc)
      if (err("Create: error creating lon attribute",rc,-33) .LT. 0)  &
        return

      call ncaptc (fid,latid(ig),'long_name',NCCHAR,LEN_TRIM(latDimName),&
                  latDimName,rc)
      if (err("Create: error creating lat attribute",rc,-33) .LT. 0) &
        return
      call ncaptc (fid,latid(ig),'units',NCCHAR,LEN_TRIM(latUnits),&
                  latUnits,rc)
      if (err("Create: error creating lat attribute",rc,-33) .LT. 0) &
        return

      if (cfio%grids(ig)%twoDimLat) then
         call ncaptc (fid,lon2id(ig),'long_name',NCCHAR,LEN_TRIM(lonName), &
                      lonName,rc)
         if (err("Create: error creating lon2 attribute",rc,-33) .LT. 0) &
              return
         call ncaptc (fid,lon2id(ig),'units',NCCHAR,LEN_TRIM(lonUnits), &
                      lonUnits,rc)
         if (err("Create: error creating lon2 attribute",rc,-33) .LT. 0)  &
              return

         call ncaptc (fid,lat2id(ig),'long_name',NCCHAR,LEN_TRIM(latName),&
                      latName,rc)
         if (err("Create: error creating lat2 attribute",rc,-33) .LT. 0) &
              return
         call ncaptc (fid,lat2id(ig),'units',NCCHAR,LEN_TRIM(latUnits),&
                      latUnits,rc)
         if (err("Create: error creating lat2 attribute",rc,-33) .LT. 0) &
              return
      end if

      if ( trim(cfio%grids(ig)%standardName) .eq. &
           'atmosphere_hybrid_sigma_pressure_coordinate' ) then
         call ncaptc (fid,layerid(ig),'long_name',NCCHAR,LEN_TRIM(layerName),&
                    layerName,rc)
         if (err("Create: error creating layer attribute",rc,-33) .LT. 0)&
            return
         call ncaptc (fid,layerid(ig),'units',NCCHAR,LEN_TRIM(layerUnits),&
                      layerUnits, rc)
         if (err("Create: error creating layer attribute",rc,-33) .LT. 0)&
           return
      endif
      if (.NOT. surfaceOnly) then
        call ncaptc (fid,levid(ig),'long_name',NCCHAR,LEN_TRIM(levName),&
                    levName,rc)
        if (err("Create: error creating lev attribute",rc,-33) .LT. 0)&
           return
        call ncaptc (fid,levid(ig),'units',NCCHAR,LEN_TRIM(levunits),&
                    levunits,rc)
        if (err("Create: error creating lev attribute",rc,-33) .LT. 0)&
           return
        call ncaptc (fid,levid(ig),'positive',NCCHAR,LEN_TRIM('down'),&
                    'down',rc)
        if (err("Create: error creating lev attribute",rc,-33) .LT. 0)&
           return
        call ncaptc (fid,levid(ig),'coordinate',NCCHAR,LEN_TRIM(  & 
                     cfio%grids(ig)%coordinate), cfio%grids(ig)%coordinate &
                     , rc)
        if (err("Create: error creating lev attribute",rc,-33) .LT. 0)&
           return
        call ncaptc (fid,levid(ig),'standard_name',NCCHAR,LEN_TRIM(  & 
                     cfio%grids(ig)%standardName),cfio%grids(ig)%standardName&
                     , rc)
        if (err("Create: error creating lev attribute",rc,-33) .LT. 0)&
           return
        if ( len(cfio%grids(ig)%formulaTerm) .gt. 0 .and. &
             trim(cfio%grids(ig)%formulaTerm) .ne. 'unknown') then
           call ncaptc (fid,levid(ig),'formula_term',NCCHAR,LEN_TRIM(  & 
                     cfio%grids(ig)%formulaTerm), cfio%grids(ig)%formulaTerm &
                     , rc)
           if (err("Create: error creating lev attribute",rc,-33) .LT. 0)&
           return
        end if
      endif
! end of mGrid loop
  end do

      if( tm .LE. 0 ) then 
         timedim = ncddef(fid, 'time', NCUNLIM, rc)
      else
         timedim = ncddef(fid, 'time', tm, rc)
         bTimeSet = .TRUE.
      endif
      if (err("Create: error defining time",rc,-31) .LT. 0) return
      if ( aveFile ) then
         bndsdim = ncddef(fid, 'nv', 2, rc)
         if (err("Create: error defining time bounds",rc,-31) .LT. 0)&
              return
      end if
      do ig =1, cfio%mGrids
        if ( trim(cfio%grids(ig)%standardName) .eq.           &
           'atmosphere_hybrid_sigma_pressure_coordinate' .or. &
             trim(cfio%grids(ig)%standardName) .eq.           &
           'atmosphere_sigma_coordinate' ) then
            if (ig .eq. 1) then
              if (cfio%mGrids .eq. 1) then
                 ptopdim = ncddef (fid, "ptop", 1, rc)
              else
                 ptopdim = ncddef (fid, "ptop0", 1, rc)
              end if
            end if
        endif
      end do

      timeid = ncvdef (fid, 'time', NCLONG, 1, timedim, rc)
      if (err("Create: error creating time",rc,-32) .LT. 0) return
      call ncaptc (fid, timeid, 'long_name', NCCHAR, LEN_TRIM(timeName),&
                  timeName, rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0)&
        return

!ams       write (dateString,200) yyyymmdd_beg, hhmmss_beg
!ams 200   format (I8,I6)
!ams       read (dateString,201) year,mon,day,hour,minute,sec
!ams 201   format (I4,5I2)

      call CFIO_parseIntTime ( yyyymmdd_beg, year, mon, day )
      call CFIO_parseIntTime ( hhmmss_beg, hour,minute,sec )

      write (timeUnits,202) year,mon,day,hour,minute,sec
202   format ('minutes since ',I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':', &
              I2.2,':',I2.2)
      call ncaptc (fid, timeid, 'units', NCCHAR, LEN_TRIM(timeUnits),  &
                  timeUnits, rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0) &
        return
      
!ams       write (strBuf,203) timinc
!ams 203   format (I6)
!ams       read (strBuf,204) hour, minute, sec
!ams 204   format (3I2)

      call CFIO_parseIntTime ( timinc, hour, minute, sec ) 

      if ( sec .NE. 0) then
        print *, 'CFIO_Create: Time increments not on minute', &
                ' boundaries are not currently allowed.'
        rc = -18
        return
      endif
      call ncapt (fid, timeid, 'time_increment', NCLONG, 1, timInc, rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0) &
        return
      call ncapt (fid,timeid,'begin_date',NCLONG,1,yyyymmdd_beg,rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0) &
        return
      call ncapt (fid,timeid,'begin_time',NCLONG,1,hhmmss_beg,rc)
      if (err("Create: error creating time attribute",rc,-33) .LT. 0) &
        return

      if ( aveFile ) then
         call ncaptc (fid,timeid,'bounds',NCCHAR,9,'time_bnds',rc)
         if (err("Create: error creating time attribute",rc,-33) .LT. 0) &
             return
      end if

  do ig = 1, cfio%mGrids
      im = cfio%grids(ig)%im
      jm = cfio%grids(ig)%jm
      km = cfio%grids(ig)%km
      tm = max(tm,cfio%grids(ig)%tm)
      if ( index(cfio%grids(ig)%gName, 'station') .gt. &
           0 ) then
         if (im .ne. jm) rtcode = err("It isn't station grid",-1,-1)
         nst = im
      end if

      gDims3D(4,ig) = timedim
      gDims3D(3,ig) = levdim(ig)
      gDims3D(2,ig) = latdim(ig)
      gDims3D(1,ig) = londim(ig)
      
      gDims2D(3,ig) = timedim
      gDims2D(2,ig) = latdim(ig)
      gDims2D(1,ig) = londim(ig)

      if (index(cfio%grids(ig)%gName,'station') .gt. 0) then
         gDims3D(4,ig) = 0
         gDims3D(3,ig) = timedim
         gDims3D(2,ig) = levdim(ig)
         gDims3D(1,ig) = stationdim(ig)
      
         gDims2D(3,ig) = 0
         gDims2D(2,ig) = timedim
         gDims2D(1,ig) = stationdim(ig)
      end if

      if ( ig .eq. 1 ) then
         if (cfio%mGrids .eq. 1) then
           nameAk = 'ak'
           nameBk = 'bk'
           namePtop = 'ptop'
         else
           nameAk = 'ak0'
           nameBk = 'bk0'
           namePtop = 'ptop0'
         end if
      else
        write (cig,"(I1)") ig-1
        nameAk = 'ak'//cig
        nameBk = 'bk'//cig
        namePtop = 'ptop'//cig
      end if

      if ( trim(cfio%grids(ig)%standardName) .eq. &
           'atmosphere_hybrid_sigma_pressure_coordinate' ) then

         dims1D = layerdim(ig)

         akid(ig) = ncvdef (fid, nameAk, NCFLOAT, 1, dims1D, rc)
         call ncaptc (fid,akid(ig),'long_name',NCCHAR,34,&
                     'ak component of hybrid coordinate',rc)
         if (err("Create: error creating ak attribute",rc,-33) .LT. 0)&
            return
         call ncaptc (fid,akid(ig),'units',NCCHAR,14,&
                     'dimensionless',rc)
         if (err("Create: error creating ak attribute",rc,-33) .LT. 0)&
            return

         bkid(ig) = ncvdef (fid, nameBk, NCFLOAT, 1, dims1D, rc)
         call ncaptc (fid,bkid(ig),'long_name',NCCHAR,34,&
                     'bk component of hybrid coordinate',rc)
         if (err("Create: error creating bk attribute",rc,-33) .LT. 0)&
            return
         call ncaptc (fid,bkid(ig),'units',NCCHAR,14,&
                     'dimensionless',rc)
         if (err("Create: error creating bk attribute",rc,-33) .LT. 0)&
            return

         ptopid(ig) = ncvdef (fid, namePtop, NCFLOAT, 1, ptopdim, rc)
         if (err("Create: error define ptopid",rc,-34) .LT. 0) return
         call ncaptc (fid,ptopid(ig),'long_name',NCCHAR,36,&
                     'ptop component of hybrid coordinate',rc)
         if (err("Create: error creating ptop attribute",rc,-33) .LT. 0)&
            return
         call ncaptc (fid,ptopid(ig),'units',NCCHAR,       &
                      len(trim(cfio%grids(ig)%ptopUnit)), &
                     trim(cfio%grids(ig)%ptopUnit),rc)
         if (err("Create: error creating ptop attribute",rc,-33) .LT. 0)&
            return
      end if

      if ( trim(cfio%grids(ig)%standardName) .eq. &
           'atmosphere_sigma_coordinate' ) then
         ptopid(ig) = ncvdef (fid, namePtop, NCFLOAT, 1, ptopdim, rc)
         if (err("Create: error define ptopid",rc,-34) .LT. 0) return
         call ncaptc (fid,ptopid(ig),'long_name',NCCHAR,36,&
                     'ptop component of sigma coordinate',rc)
         if (err("Create: error creating ptop attribute",rc,-33) .LT. 0)&
            return
         call ncaptc (fid,ptopid(ig),'units',NCCHAR,      &
                     len(trim(cfio%grids(ig)%ptopUnit)), &
                     trim(cfio%grids(ig)%ptopUnit),rc)
         if (err("Create: error creating ptop attribute",rc,-33) .LT. 0)&
            return
      end if

! end of mGrids loop
  end do

      scale_32 = 1.0     ! No packing for now.
      offset_32 = 0.0    ! No packing for now.

! Set up packing attributes for each variable.  
! Define physical variables.  Set attributes for physical variables.

      do i=1,nvars
        scale_32 = 1.0                        ! default to no packing.
        offset_32 = 0.0

        if (pRange_32(1,i) .NE. amiss_32 .OR. pRange_32(2,i) .NE.  &
       amiss_32) then
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
             scale_32 = 1.0                        ! no packing.
             offset_32 = 0.0
             packflag = .FALSE.
          else
             packflag = .TRUE.
          endif
        else
          packflag = .FALSE.
        endif
        do ig = 1, cfio%mGrids
           if (trim(cfio%varObjs(i)%grid%gName) .eq.              &
               trim(cfio%grids(ig)%gName)) then
              dims3D = gDims3D(:,ig)
              dims2D = gDims2D(:,ig)
           end if
        end do

        if ( kmvar(i) .eq. 0 ) then
          ndim = 3
          if (index(cfio%varObjs(i)%grid%gName,'station') .gt. 0) ndim = 2
          if (packflag) then
            vid(i) = ncvdef (fid, vname(i), NCSHORT, ndim, dims2D, rc)
          else if (cfio%prec .EQ. 1) then
            vid(i) = ncvdef (fid, vname(i), NCDOUBLE, ndim, dims2D, rc)
          else
            vid(i) = ncvdef (fid, vname(i), NCFLOAT, ndim, dims2D, rc)
          endif
        else
          ndim = 4
          if (index(cfio%varObjs(i)%grid%gName,'station') .gt. 0) ndim = 3
          if (packflag) then
            vid(i) = ncvdef (fid, vname(i), NCSHORT, ndim, dims3D, rc)
          else if (cfio%prec .EQ. 1) then
            vid(i) = ncvdef (fid, vname(i), NCDOUBLE, ndim, dims3D, rc)
          else
            vid(i) = ncvdef (fid, vname(i), NCFLOAT, ndim, dims3D, rc)
          endif
        endif
        if (err("Create: error defining variable",rc,-34) .LT. 0)  &
         return

#if defined(HAS_NETCDF4)

!
! Chunksize is set to IM,JM,1,1 works for 2D and 3D variables
!
        if ( (associated(cfio%varObjs(i)%ChunkSize)) ) then 
           rc=NF_DEF_VAR_CHUNKING(fid, vid(i), &
                NF_CHUNKED, cfio%varObjs(i)%ChunkSize)
           if (err("Create: error setting Chunked variable",rc,-40) .LT. 0) &
           return
        else
!
! Set Chunsize to IM,JM,1,1 by default 
! If Time (tm) has been set in grid, set the file to contiguous
!
           if( bTimeSet .eq. .FALSE.) then
              nDefaultChunkSize(1)=im
              nDefaultChunkSize(2)=jm
              nDefaultChunkSize(3)=1
              nDefaultChunkSize(4)=1
              rc=NF_DEF_VAR_CHUNKING(fid, vid(i), NF_CHUNKED,  & 
                   nDefaultChunkSize)
           endif

        end if
!
! Handle deflation
!
        if (cfio%deflate > 0 .and. cfio%deflate <= 9) then
           rc = nf_def_var_deflate(fid, vid(i), 1, 1, cfio%deflate)
           if (err("Create: error setting deflate filter",rc,-40) .LT. 0) return
        end if

! enable error checking
!        rc = nf_def_var_fletcher32(fid, vid(i), 1)
!        if (err("Create: error setting fletcher",rc,-41) .LT. 0) return
#endif

        call ncaptc (fid, vid(i), 'long_name', NCCHAR,  &
                    LEN_TRIM(vtitle(i)),vtitle(i), rc)
        if (err("Create: error defining long_name attribute",rc,-35) &
          .LT. 0) return
        call ncaptc (fid, vid(i), 'units', NCCHAR,  &
                    LEN_TRIM(vunits(i)),vunits(i), rc)
        if (err("Create: error defining units attribute",rc,-35) &
          .LT. 0) return

        if (packflag) then
          call ncapt (fid,vid(i),'_FillValue',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining FillValue attribute",rc,-35) &
          .LT. 0) return
          if ( scale_32 .ne. 1.0 .or. offset_32 .ne. 0.0 ) then
          call ncapt (fid,vid(i),'scale_factor',NCFLOAT,1,scale_32,rc)
          if (err("Create: error defining scale_factor attribute",rc,-35) &
              .LT. 0) return
          call ncapt (fid,vid(i),'add_offset',NCFLOAT,1,offset_32,rc)
          if (err("Create: error defining add_offset attribute",rc,-35) &
              .LT. 0) return
          call ncapt (fid,vid(i),'packmin',NCFLOAT,1,low_32,rc)
          if (err("Create: error defining packmin attribute",rc,-35)  &
             .LT. 0) return
          call ncapt (fid,vid(i),'packmax',NCFLOAT,1,high_32,rc)
          if (err("Create: error defining packmax attribute",rc,-35) &
             .LT. 0) return
          end if
          call ncapt (fid,vid(i),'missing_value',NCSHORT,1,amiss_16,rc)
          if (err("Create: error defining missing_value attribute",rc,-35) &
          .LT. 0) return
          call ncapt (fid,vid(i),'fmissing_value',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining fmissing_value attribute",rc,-35) &
          .LT. 0) return
        else
          call ncapt (fid,vid(i),'_FillValue',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining FillValue attribute",rc,-35) &
          .LT. 0) return
          if ( scale_32 .ne. 1.0 .or. offset_32 .ne. 0.0 ) then
          call ncapt (fid,vid(i),'scale_factor',NCFLOAT,1,scale_32,rc)
          if (err("Create: error defining scale_factor attribute",rc,-35) &
              .LT. 0) return
          call ncapt (fid,vid(i),'add_offset',NCFLOAT,1,offset_32,rc)
          if (err("Create: error defining add_offset attribute",rc,-35) &
              .LT. 0) return
          end if
          call ncapt (fid,vid(i),'missing_value',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining missing_value attribute",rc,-35) &
          .LT. 0) return
          call ncapt (fid,vid(i),'fmissing_value',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining fmissing_value attribute",rc,-35) &
          .LT. 0) return

! ADDED BY BYIN for more variable meta data
         cfio%fid = fid
!        get real variable attributes from rList
         do iCnt = 1, cfio%mVars
            if ( associated(cfio%varObjs(i)%rList) ) then
               call getMaxLenCnt(maxLen, cfio%varObjs(i)%nVarAttReal, &
                                 rList=cfio%varObjs(i)%rList)
               count = cfio%varObjs(i)%nVarAttReal
               allocate(cfio%varObjs(i)%attRealNames(count),     &
                     cfio%varObjs(i)%attRealCnts(count),      &
                     cfio%varObjs(i)%varAttReals(count,maxLen), stat=rtcode)
               call getList(rList=cfio%varObjs(i)%rList,    &
                       realAttNames=cfio%varObjs(i)%attRealNames, &
                       realAttCnts=cfio%varObjs(i)%attRealCnts,   &
                       realAtts=cfio%varObjs(i)%varAttReals)
            end if
         end do

!        write real variable attributes to output file
         do iCnt = 1, cfio%varObjs(i)%nVarAttReal
            allocate(realVarAtt(size(cfio%varObjs(i)%varAttReals)/ &
                     cfio%varObjs(i)%nVarAttReal), stat=rc)
            realVarAtt = cfio%varObjs(i)%varAttReals(iCnt,:)
            if (cfio%varObjs(i)%attRealCnts(iCnt) .ne. size(realVarAtt)) then
              rc=err("FileCreate: Num of real var elements and Cnt differ",-39,-39) 
              return
            end if
            call ncapt (cfio%fid,vid(i),cfio%varObjs(i)%attRealNames(iCnt),&
                        NCFLOAT, cfio%varObjs(i)%attRealCnts(iCnt),        &
                        realVarAtt, rc)
            if (err("FileCreate: error from ncapt for real att",rc,-35) &
                .LT. 0) return
            deallocate(realVarAtt)
         end do

!        get integer variable attributes from iList
         do iCnt = 1, cfio%mVars
            if ( associated(cfio%varObjs(i)%iList) ) then
               call getMaxLenCnt(maxLen, cfio%varObjs(i)%nVarAttInt, &
                                 iList=cfio%varObjs(i)%iList)
               count = cfio%varObjs(i)%nVarAttInt
               allocate(cfio%varObjs(i)%attIntNames(count),     &
                     cfio%varObjs(i)%attIntCnts(count),      &
                     cfio%varObjs(i)%varAttInts(count,maxLen), stat=rtcode)
               call getList(iList=cfio%varObjs(i)%iList,    &
                       intAttNames=cfio%varObjs(i)%attIntNames, &
                       intAttCnts=cfio%varObjs(i)%attIntCnts,   &
                       intAtts=cfio%varObjs(i)%varAttInts)
            end if
         end do

!        write int variable attributes to output file
         do iCnt = 1, cfio%varObjs(i)%nVarAttInt
            allocate(intVarAtt(size(cfio%varObjs(i)%varAttInts)/ &
                     cfio%varObjs(i)%nVarAttInt), stat=rc)
            intVarAtt = cfio%varObjs(i)%varAttInts(iCnt,:)
            if (cfio%varObjs(i)%attIntCnts(iCnt) .gt. size(intVarAtt)) then
              rc=err("FileCreate: Num of int var elements and Cnt differ",-39,-39) 
              return
            end if
            call ncapt (cfio%fid,vid(i),cfio%varObjs(i)%attIntNames(iCnt),&
                        NCLONG, cfio%varObjs(i)%attIntCnts(iCnt),         &
                        intVarAtt, rc)
            if (err("FileCreate: error from ncapt for int att",rc,-35) &
                .LT. 0) return
            deallocate(intVarAtt)
         end do

!        get char variable attributes from cList
         do iCnt = 1, cfio%mVars
            if ( associated(cfio%varObjs(i)%cList) ) then
               call getMaxLenCnt(maxLen, cfio%varObjs(i)%nVarAttChar, &
                                 cList=cfio%varObjs(i)%cList)
               count = cfio%varObjs(i)%nVarAttChar
               allocate(cfio%varObjs(i)%attCharNames(count),     &
                     cfio%varObjs(i)%attCharCnts(count),      &
                     cfio%varObjs(i)%varAttChars(count), stat=rtcode)
               call getList(cList=cfio%varObjs(i)%cList,    &
                       charAttNames=cfio%varObjs(i)%attCharNames, &
                       charAttCnts=cfio%varObjs(i)%attCharCnts,   &
                       charAtts=cfio%varObjs(i)%varAttChars)
            end if
         end do

!        write char variable attributes to output file
         do iCnt = 1, cfio%varObjs(i)%nVarAttChar
            call ncapt (cfio%fid,vid(i),cfio%varObjs(i)%attCharNames(iCnt),&
                        NCCHAR, cfio%varObjs(i)%attCharCnts(iCnt),         &
                        cfio%varObjs(i)%varAttChars(iCnt), rc)
            if (err("FileCreate: error from ncapt for char att",rc,-35) &
                .LT. 0) return
         end do

!         write scaleFactor, addOffSet, and standardName to output

!         if ( cfio%varObjs(i)%scaleFactor /= 0 ) then
            scale_factor = cfio%varObjs(i)%scaleFactor 
            call ncapt (cfio%fid, vid(i), 'scale_factor', NCFLOAT,   &
                        1, scale_factor, rc)
            if (err("FileCreate: error from ncapt for scale_factor",rc,-35) &
                .LT. 0) return
!         end if
!         if ( cfio%varObjs(i)%addOffSet /= 0 ) then
            add_offset = cfio%varObjs(i)%addOffSet   
            call ncapt (cfio%fid, vid(i), 'add_offset', NCFLOAT,   &
                        1, add_offset, rc)
            if (err("FileCreate: error from ncapt for add_offset",rc,-35) &
                .LT. 0) return
!           end if
              
         if ( LEN_TRIM(cfio%varObjs(i)%standardName) .gt. 0 ) then
            call ncaptc (cfio%fid, vid(i), 'standard_name', NCCHAR,   &
                        LEN_TRIM(cfio%varObjs(i)%standardName),       &
                        cfio%varObjs(i)%standardName, rc)
            if (err("FileCreate: error from ncapt for standard_name",rc,-35) &
                .LT. 0) return
         end if
        end if

        if (vRange_32(1,i) .NE. amiss_32 .OR. vRange_32(2,i) .NE.  &
           amiss_32) then
          if (vRange_32(1,i) .GT. vRange_32(2,i)) then
            high_32 = vRange_32(1,i)
            low_32  = vRange_32(2,i)
          else
            high_32 = vRange_32(2,i)
            low_32  = vRange_32(1,i)
          endif
          call ncapt (fid,vid(i),'vmin',NCFLOAT,1,low_32,rc)
          if (err("Create: error defining vmin attribute",rc,-35) &
             .LT. 0) return
          call ncapt (fid,vid(i),'vmax',NCFLOAT,1,high_32,rc)
          if (err("Create: error defining vmax attribute",rc,-35) &
             .LT. 0) return
        else
          call ncapt (fid,vid(i),'vmin',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining vmin attribute",rc,-35) &
             .LT. 0) return
          call ncapt (fid,vid(i),'vmax',NCFLOAT,1,amiss_32,rc)
          if (err("Create: error defining vmax attribute",rc,-35) &
             .LT. 0) return

        endif

        call ncapt (fid,vid(i),'valid_range',NCFLOAT,2,vRange_32(:,i),rc)
        if (err("Create: error defining valid_range attribute",rc,-35) &
           .LT. 0) return

        if ( cfio%varObjs(i)%timAve ) then
           call ncaptc (fid, vid(i), 'cell_methods', NCCHAR,  &
                    len(trim(cfio%varObjs(i)%cellMthd))+6,     &
                    'time: '//trim(cfio%varObjs(i)%cellMthd), rc)
           if (err("Create: error defining cell_methods attribute",rc,-35) &
                   .LT. 0) return
        end if

        if (cfio%mGrids == 1) then
           ig = 1
           if (cfio%grids(ig)%twoDimLat) then
              call ncaptc (fid, vid(i), 'coordinates', NCCHAR,  &
                   LEN_TRIM(coordinatesName), coordinatesName, rc)
              if (err("Create: error defining coordinates attribute",rc,-35) &
                   .LT. 0) return
           end if
        end if

      enddo
 
      if ( aveFile ) then
         dimsbnd(1) = bndsdim
         dimsbnd(2) = timedim
         bndsid = ncvdef (fid, 'time_bnds', NCFLOAT, 2, dimsbnd, rc)
      end if

! Exit define mode.

      call ncendf (fid, rc)
      if (err("Create: error exiting define mode",rc,-37) .LT. 0)  &
       return

! Write out dimension variables.

  do ig = 1, cfio%mGrids
      im = cfio%grids(ig)%im
      jm = cfio%grids(ig)%jm
      km = cfio%grids(ig)%km
      tm = max(tm,cfio%grids(ig)%tm)

      if (cfio%grids(ig)%twoDimLat) then
         sz_lon = im*jm
         sz_lat = sz_lon
      else
         sz_lon = im
         sz_lat = jm
      end if
      allocate(lon_64(sz_lon), lat_64(sz_lat), levs_64(km), ak_32(km+1),         &
            bk_32(km+1), layer(km+1), stat = rtcode) 

      ptop_32(1) = cfio%grids(ig)%ptop
      do i=1,sz_lon
         lon_64(i) = cfio%grids(ig)%lon(i)
      enddo
      do i=1,sz_lat
         lat_64(i) = cfio%grids(ig)%lat(i)
      enddo

      if (cfio%grids(ig)%twoDimLat) then
         allocate(lon2_64(im), lat2_64(jm), stat = rtcode) 
         do i=1,im
!            lon2_64(i) = i ! uncomment if index is needed
            lon2_64(i) = cfio%grids(ig)%lon(i) ! lats at South pole
         enddo
         do i=1,jm
!            lat2_64(i) = i ! uncomment if index is needed
            lat2_64(i) = cfio%grids(ig)%lat((i-1)*im + 1) ! lons at date line
         enddo
      end if

      do i=1,km
         levs_64(i) = cfio%grids(ig)%lev(i)
      enddo
      if ( trim(cfio%grids(ig)%standardName) .eq. &
           'atmosphere_hybrid_sigma_pressure_coordinate' ) then
         if (associated(cfio%grids(ig)%ak) .and. &
             associated(cfio%grids(ig)%bk) ) then
            do i=1,km+1
               layer(i) = i
               ak_32(i) = cfio%grids(ig)%ak(i)
               bk_32(i) = cfio%grids(ig)%bk(i)
            enddo
         else
             if (err(": ak or bk is not set",-1,-1) .lt. 0 ) return
         end if
      end if

      if (cfio%grids(ig)%twoDimLat) then
         corner(1) = 1
         edges(1) = im
         call ncvpt (fid, lonid(ig), corner, edges, lon2_64, rc)
         if (err("Create: error writing lons2",rc,-38) .LT. 0) return

         corner(1) = 1
         edges(1) = jm
         call ncvpt (fid, latid(ig), corner, edges, lat2_64, rc)
         if (err("Create: error writing lats2",rc,-38) .LT. 0) return

         corner2d = 1
         edges2d(1) = im
         edges2d(2) = jm
         call ncvpt (fid, lon2id(ig), corner2d, edges2d, lon_64, rc)
         if (err("Create: error writing lons",rc,-38) .LT. 0) return

         corner2d = 1
         edges2d(1) = im
         edges2d(2) = jm
         call ncvpt (fid, lat2id(ig), corner2d, edges2d, lat_64, rc)
         if (err("Create: error writing lats",rc,-38) .LT. 0) return
         deallocate(lon2_64, stat = rtcode)
         deallocate(lat2_64, stat = rtcode)
      else
         corner(1) = 1
         edges(1) = im
         call ncvpt (fid, lonid(ig), corner, edges, lon_64, rc)
         if (err("Create: error writing lons",rc,-38) .LT. 0) return

         corner(1) = 1
         edges(1) = jm
         call ncvpt (fid, latid(ig), corner, edges, lat_64, rc)
         if (err("Create: error writing lats",rc,-38) .LT. 0) return
      end if
      deallocate(lon_64, stat = rtcode)
      deallocate(lat_64, stat = rtcode)

      if (.NOT. surfaceOnly) then
        corner(1) = 1
        edges(1) = km
        call ncvpt (fid, levid(ig), corner, edges, levs_64, rc)
        if (err("Create: error writing levs",rc,-38) .LT. 0) return
      endif
      deallocate(levs_64, stat = rtcode)

      if ( trim(cfio%grids(ig)%standardName) .eq. &
           'atmosphere_hybrid_sigma_pressure_coordinate' ) then
        corner(1) = 1
        edges(1) = 1
        call ncvpt (fid, ptopid(ig), corner, edges, ptop_32, rc)
        if (err("Create: error writing ptopid prs",rc,-38) .LT. 0) return
        corner(1) = 1
        edges(1) = km+1
        call ncvpt (fid, layerid(ig), corner, edges, layer, rc)
        if (err("Create: error writing layers",rc,-38) .LT. 0) return
        call ncvpt (fid, akid(ig), corner, edges, ak_32, rc)
        call ncvpt (fid, bkid(ig), corner, edges, bk_32, rc)
        if (err("Create: error writing ak/bk",rc,-38) .LT. 0) return
      endif
      deallocate(layer, stat = rtcode)
      deallocate(ak_32, stat = rtcode)
      deallocate(bk_32, stat = rtcode)

      if ( trim(cfio%grids(ig)%standardName) .eq. &
           'atmosphere_sigma_coordinate' ) then
        corner(1) = 1
        edges(1) = 1
        call ncvpt (fid, ptopid(ig), corner, edges, ptop_32, rc)
      endif

! end of mGrids loop
 end do
      corner(1) = 1
      edges(1) = 1
      call ncvpt (fid, timeid, corner, edges, 0, rc)
      if (err("Create: error writing times",rc,-38) .LT. 0) return

      deallocate(latid, stat = rtcode)
      deallocate(lonid, stat = rtcode)
      deallocate(lat2id, stat = rtcode)
      deallocate(lon2id, stat = rtcode)
      deallocate(levid, stat = rtcode)
      deallocate(layerid, stat = rtcode)
      deallocate(latdim, stat = rtcode)
      deallocate(londim, stat = rtcode)
      deallocate(levdim, stat = rtcode)
      deallocate(layerdim, stat = rtcode)
      deallocate(akid, stat = rtcode)
      deallocate(bkid, stat = rtcode)
      deallocate(ptopid, stat = rtcode)
      deallocate(gDims3D, stat = rtcode)
      deallocate(gDims2D, stat = rtcode)
      deallocate(stationdim, stat = rtcode)
      deallocate(stationid, stat = rtcode)

      deallocate(station, stat = rtcode)
      deallocate(vname, stat = rtcode)
      deallocate(vtitle, stat = rtcode)
      deallocate(vunits, stat = rtcode)
      deallocate(kmvar, stat = rtcode)
      deallocate(valid_range, stat = rtcode)
      deallocate(packing_range, stat = rtcode)
      deallocate(vid, stat = rtcode)
      deallocate(vRange_32, stat = rtcode)
      deallocate(pRange_32, stat = rtcode)

      rc=0
      return
contains
  logical function isFileExtensionNetCDF4(fileName)
    character(len=*) :: fileName
    
    character(len=len(fileName)) :: ext
    integer :: i
    
    isFileExtensionNetCDF4 = .false.
    ext = ''
    i = index(fileName,'.',back=.true.)
    if (i==0) return
    ext = fileName(i+1:)

    if (ext == 'nc4' .or. ext == 'h5') then
       isFileExtensionNetCDF4 = .true.
    end if

    return
  end function isFileExtensionNetCDF4

      end subroutine CFIO_Create_


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

      integer :: vid, corner(4), edges(4)
      integer :: hour, minute, sec, incSecs, timeIndex
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

!ams          write (strBuf,203) timeinc
!ams 203      format (I6)
!ams          read (strBuf,204) hour, minute, sec
!ams 204      format (3I2)

         call CFIO_parseIntTime ( timeinc, hour, minute, sec ) 

         incSecs = hour*3600 + minute*60 + sec

!ams         write (strBuf,203) curTime
!ams         read (strBuf,204) hour, minute, sec

         call CFIO_parseIntTime ( curTime, hour, minute, sec ) 

         curSecs = hour*3600 + minute*60 + sec
                                                                     
         timeIndex = seconds/incSecs + 1
         corner(1) = 1
         corner(2) = timeIndex
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

         vid = ncvid (cfio%fid, 'time_bnds', rtcode)
         if ( rtcode .ne. 0 ) then 
            print *, "ncvid failed in ncvid for time_bnds"
            if ( present(rc) ) rc = rtcode
            return
         end if
         call ncvpt (cfio%fid, vid, corner, edges, bndsdata, rtcode)
         if ( rtcode .ne. 0 ) then 
            print *, "ncvid failed in ncvpt for time_bnds"
            if ( present(rc) ) rc = rtcode
            return
         end if
      end if
 
      if ( present(rc) ) rc = rtcode

      end subroutine writeBnds

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarReadT3D_ -- Read a variable from an existing file
                                                                                                              
! !INTERFACE:
      subroutine ESMF_CFIOSdfVarReadT3D_ ( cfio, vName, field, &
                                        timeString, cfio2, rc )
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      type(ESMF_CFIO), intent(inOut), OPTIONAL :: cfio2  ! second CFIO obj
      character(len=*), intent(in) :: timeString
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
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -46  error from ncvgt
                         !  rc = -48  error from ncinq
                         !  rc = -52  error from ncvinq
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
      call strToInt(timeString,date_,curTime_)

      if ( date_ < 0 .OR. curTime_ < 0 ) then
           if ( present(rc) ) rc = -99
           return
      end if

      call ESMF_CFIOSdfVarReadT3D__ ( cfio, vName, date_, curTime_, field, & 
                                   cfio2=cfio2, rc=rc )

    end subroutine ESMF_CFIOSdfVarReadT3D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarReadT3D_ -- Read a variable from an existing file
                                                                                                              
! !INTERFACE:

      subroutine ESMF_CFIOSdfVarReadT3D__(cfio, vName, date, curTime, field, rc, cfio2)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in) :: date                 ! yyyymmdd
      integer, intent(in) :: curTime              ! hhmmss
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
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -46  error from ncvgt
                         !  rc = -48  error from ncinq
                         !  rc = -52  error from ncvinq
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
                                                                                         
      real    alpha, amiss
      real, pointer ::  field2(:,:,:) => null() ! workspace for interpolation

      rtcode = 0

!     find the right variable obj.
      do i = 1, cfio%mVars
         if ( trim(vName) .eq. trim(cfio%varObjs(i)%vName) ) exit
      end do
      im = cfio%varObjs(i)%grid%im
      jm = cfio%varObjs(i)%grid%jm
      km = cfio%varObjs(i)%grid%km
      if (km .lt. 1) km = 1

      if ( .not. associated(field) ) allocate(field(im,jm,km))

!     Get beginning time & date.  Calculate offset seconds from start.
!     ----------------------------------------------------------------
      call GetBegDateTime ( cfio%fid, begDate, begTime, incSecs, rtcode )
      if (err("GetVar: could not determine begin_date/begin_time",rtcode,-44)&
         .NE. 0) go to 999
                                                                                         
      secs = DiffDate (begDate, begTime, date, curTime)
                                                                                         
!      if (date .LT. begDate .OR. (begDate .EQ. date .AND.  &
!         curTime .LT. begTime) .or. secs .LT. 0) then
!         rc = -7
!         return
!      endif
 
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
      call ESMF_CFIOSdfVarRead(cfio, vName, field, date=nymd1, curtime=nhms1,  rc=rtcode)
      if ( rtcode .ne. 0 ) goto 999
                                                                    
      if ( secs1 .eq. secs ) goto 999   ! no interpolation needed

      allocate(field2(im,jm,km))
                                                                                     
!     Read grids at second time with GetVar()
!     ---------------------------------------
      call ESMF_CFIOSdfVarRead(cfio, vName, field2, date=nymd2, curtime=nhms2, rc=rtcode)
      if ( rtcode .ne. 0 ) then
         if ( present(cfio2) )     &
            call ESMF_CFIOSdfVarRead(cfio2, vName, field2, &
                                  date=nymd2, curtime=nhms2, rc=rtcode)
         if ( rtcode .ne. 0 ) return
      end if
                                                                                         
!     Get missing value
!     -----------------
      amiss = CFIO_GetMissing ( cfio%fid, rtcode )
      if ( rtcode .ne. 0 ) goto 999

!     Do interpolation
!     ----------------
      alpha = float(secs - secs1)/float(secs2 - secs1)
!ams  print *, ' nymd = ', nymd1, nymd2
!ams  print *, ' nhms = ', nhms1, nhms2
!ams  print *, 'alpha = ', alpha
      do k = 1, km
         do j = 1, jm
            do i = 1, im
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
                                                        
      rtcode = 0

!     All done
!     --------
999   continue
      if ( associated(field2) ) deallocate(field2)
      if ( present(rc) ) rc = rtcode
                                                                         
      end subroutine ESMF_CFIOSdfVarReadT3D__


!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarReadT2D_ -- Read a variable from an existing file
                                                                                                              
! !INTERFACE:
      subroutine ESMF_CFIOSdfVarReadT2D_ ( cfio, vName, field, &
                                        timeString, cfio2, rc )
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      type(ESMF_CFIO), intent(inOut), OPTIONAL :: cfio2  ! second CFIO obj
      character(len=*), intent(in) :: timeString
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
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -46  error from ncvgt
                         !  rc = -48  error from ncinq
                         !  rc = -52  error from ncvinq
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
      call strToInt(timeString,date_,curTime_)
      if ( date_ < 0 .OR. curTime_ < 0 ) then
           if ( present(rc) ) rc = -99
           return
      end if

      call ESMF_CFIOSdfVarReadT2D__ ( cfio, vName, date_, curTime_, field, &
                                   cfio2=cfio2, rc=rc )

    end subroutine ESMF_CFIOSdfVarReadT2D_

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: ESMF_CFIOSdfVarReadT2D_ -- Read a variable from an existing file
                                                                                                              
! !INTERFACE:

      subroutine ESMF_CFIOSdfVarReadT2D__(cfio, vName, date, curTime, field, rc, cfio2)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
      type(ESMF_CFIO), intent(inOut) :: cfio      ! a CFIO obj
      character(len=*), intent(in) :: vName       ! variable name
      integer, intent(in) :: date                 ! yyyymmdd
      integer, intent(in) :: curTime              ! hhmmss
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
                         !  rc = -38  error from ncvpt (dimension variable)
                         !  rc = -40  error from ncvid
                         !  rc = -41  error from ncdid or ncdinq (lat or lon)
                         !  rc = -42  error from ncdid or ncdinq (lev)
                         !  rc = -43  error from ncvid (time variable)
                         !  rc = -44  error from ncagt (time attribute)
                         !  rc = -46  error from ncvgt
                         !  rc = -48  error from ncinq
                         !  rc = -52  error from ncvinq
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
      call GetBegDateTime ( cfio%fid, begDate, begTime, incSecs, rtcode )
      if (err("GetVar: could not determine begin_date/begin_time",rtcode,-44)&
         .NE. 0) go to 999
                                                                                         
      secs = DiffDate (begDate, begTime, date, curTime)
                                                                                         
!      if (date .LT. begDate .OR. (begDate .EQ. date .AND.  &
!         curTime .LT. begTime) .or. secs .LT. 0) then
!         rc = -7
!         return
!      endif
 
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
      call ESMF_CFIOSdfVarRead(cfio, vName, field, date=nymd1, curtime=nhms1,  rc=rtcode)
      if ( rtcode .ne. 0 ) goto 999
                                                                    
      if ( secs1 .eq. secs ) goto 999   ! no interpolation needed

      allocate(field2(im,jm))
                                                                                     
!     Read grids at second time with GetVar()
!     ---------------------------------------
      call ESMF_CFIOSdfVarRead(cfio, vName, field2, date=nymd2, curtime=nhms2, rc=rtcode)
      if ( rtcode .ne. 0 ) then
         if ( present(cfio2) )     &
            call ESMF_CFIOSdfVarRead(cfio2, vName, field2, &
                                  date=nymd2, curtime=nhms2, rc=rtcode)
         if ( rtcode .ne. 0 ) return
      end if
                                                                                         
!     Get missing value
!     -----------------
      amiss = CFIO_GetMissing ( cfio%fid, rtcode )
      if ( rtcode .ne. 0 ) goto 999

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
                                                                         
      end subroutine ESMF_CFIOSdfVarReadT2D__

!..........................................................................

      end module ESMF_CFIOSdfMod

