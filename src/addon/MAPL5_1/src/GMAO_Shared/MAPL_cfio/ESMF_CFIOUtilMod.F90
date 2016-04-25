!==============================================================================
!BOP
! !MODULE: ESMF_CFIOUtil.F90 - utility subroutines for CFIO
                                                                                         
       module ESMF_CFIOUtilMod
!
! !DESCRIPTION:
!
! providing utility subroutines for CFIO.
!
! This module provides all the necessary utility subroutines for CFIO.
!
! !REVISION HISTORY:
!
!  Sep2004  Baoyu Yin  Modified from ESMF_CFIOMod.F90.
!  Mar2005  Baoyu Yin  Integrated all routines of GFIO into CFIO.
!  Apr2005  Baoyu Yin  Added Get_Missing routine
!  Apr2006  da Silva   Incorporated m_StrTemplate to eliminate mpeu dependency.
!  Jun2006  Baoyu yin  Added cyclic option to GetVar.
!  Jul2006  da Silva   Eliminated read(str,fmt) to parse time; replaced
!                      with more robust mod() calculations.
!------------------------------------------------------------------------------
! !USES:

      use ESMF_CFIOBaseMod
      use netcdf
      implicit none

#if defined(HDFEOS) || defined(HDFSD)
      include "hdf.f90"
#endif

!------------------------------------------------------------------------------
!
!EOP
!==============================================================================
      integer, parameter :: GCTP_GEO=0
      integer, parameter :: HDFE_NOMERGE=0
      integer, parameter :: HDFE_GD_LL=2
      integer, parameter :: NDIMS_MAX = 4
      integer, parameter :: MAX_VAR_DIMS = 32
      character*7, parameter :: GRID_NAME='EOSGRID'
      integer, parameter :: NFILES_MAX = 64
      integer, parameter :: NVARS_MAX = 128
      integer, parameter :: MAXCHR = 256
      integer, parameter :: PACK_BITS = 32766
      integer, parameter :: PACK_FILL = 32767
      integer, parameter :: MLEN = 1024     ! Max. length of an attribute
      integer, parameter :: MVARLEN = 256   ! Max. length of a variable name

! Define a new data type "List" -- private data type for variable and 
! global attributes

      type iNode
         integer :: count
         integer, pointer :: intData(:)
         character(len=MVARLEN) :: name
         character(len=MVARLEN) :: vName
         type (iNode), pointer :: next => null()
      end type iNode

      type rNode
         integer :: count
         real, pointer :: realData(:)
         character(len=MVARLEN) :: name
         character(len=MVARLEN) :: vName
         type (rNode), pointer :: next => null()
      end type rNode

      type cNode
         integer :: count
         character(len=MLEN) :: charData
         character(len=MVARLEN) :: name
         character(len=MVARLEN) :: vName
         type (cNode), pointer :: next => null()
      end type cNode

! StrTemplate parameters (adapted from MPEU)

  private :: die, perr

  integer, parameter :: stderr = 6
  
  character(len=*),parameter :: myname='m_StrTemplate'

  character(len=3),parameter,dimension(12) :: mon_lc =	(/	&
	'jan','feb','mar','apr','may','jun',	&
	'jul','aug','sep','oct','nov','dec'	/)

  character(len=3),parameter,dimension(12) :: mon_wd =	(/	&
	'Jan','Feb','Mar','Apr','May','Jun',	&
	'Jul','Aug','Sep','Oct','Nov','Dec'	/)

  character(len=3),parameter,dimension(12) :: mon_uc =	(/	&
	'JAN','FEB','MAR','APR','MAY','JUN',	&
	'JUL','AUG','SEP','OCT','NOV','DEC'	/)

!------------------------------------------------------------------------------

      contains


!------------------------------------------------------------------------------!BOP
! !ROUTINE: addList -- put user defined attribute into a list
!
! !INTERFACE:
!
        subroutine addList(name, count, vName, attInt, attChar, attReal, &
                           iList, rList, cList)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
           character(len=*), intent(in) :: name 
           integer, intent(in) :: count
           character(len=*), intent(in), OPTIONAL :: vName 
           integer, intent(in), OPTIONAL :: attInt(*)
           real, intent(in), OPTIONAL :: attReal(*)
           character(len=MLEN), intent(in), OPTIONAL :: attChar 
!
! !OUTPUT PARAMETERS:
!
           type(iNode), pointer, OPTIONAL :: iList  ! int list
           type(rNode), pointer, OPTIONAL :: rList  ! real list
           type(cNode), pointer, OPTIONAL :: cList  ! char list
!
! !DESCRIPTION:
!     put user defined attribute into a list
!EOP
!------------------------------------------------------------------------------
           type(iNode), pointer :: p, q
           type(rNode), pointer :: rp, rq
           type(cNode), pointer :: cp, cq
           integer :: i

!          put int attribute into an integer list
           if ( present(attInt) .and. present(iList)) then
              if ( .not. associated(iList) ) then
                 allocate(iList)
                 iList%count = count
                 iList%name = name
                 if (present(vName)) iList%vName = vName
                 allocate(iList%intData(count))
                 do i =1, count
                    iList%intData(i) = attInt(i)
                 end do
                 iList%next => null()
              else
                 q => iList
                 p => iList%next
                 do while ( associated(p) ) 
                     q => p
                     p => p%next
                 end do

                 allocate(p)
                 p%count = count
                 p%name = name 
                 if (present(vName)) p%vName = vName
                 allocate(p%intData(count))
                 do i =1, p%count
                    p%intData(i) = attInt(i)
                 end do

                 q%next => p
              end if
           end if

!          put real attribute into a real list
           if ( present(attReal) .and. present(rList)) then
              if ( .not. associated(rList) ) then
                 allocate(rList)
                 rList%count = count
                 rList%name = name
                 if (present(vName)) rList%vName = vName
                 allocate(rList%realData(count))
                 do i =1, count
                    rList%realData(i) = attReal(i)
                 end do
                 rList%next => null()
              else
                 rq => rList
                 rp => rList%next
                 do while ( associated(rp) ) 
                     rq => rp
                     rp => rp%next
                 end do

                 allocate(rp)
                 rp%count = count
                 rp%name = name 
                 if (present(vName)) rp%vName = vName
                 allocate(rp%RealData(count))
                 do i =1, rp%count
                    rp%realData(i) = attReal(i)
                 end do

                 rq%next => rp
              end if
           end if

!          put char attribute into a char list
           if ( present(attChar) .and. present(cList)) then
              if ( .not. associated(cList) ) then
                 allocate(cList)
                 cList%count = count
                 cList%name = name
                 if (present(vName)) cList%vName = vName
                 cList%charData = attChar(1:count)
                 cList%next => null()
              else
                 cq => cList
                 cp => cList%next
                 do while ( associated(cp) ) 
                     cq => cp
                     cp => cp%next
                 end do

                 allocate(cp)
                 cp%count = count
                 cp%name = name 
                 if (present(vName)) cp%vName = vName
                 cp%charData = attChar(1:count)  

                 cq%next => cp
              end if
           end if

        end subroutine addList

!------------------------------------------------------------------------------!BOP
! !ROUTINE: getList -- retrieve attributes from a list
!
! !INTERFACE:
!
   subroutine getList(iList, nIntAtt, intAttNames, intAttCnts, intAtts,     &
                      rList, nRealAtt, realAttNames, realAttCnts, realAtts, &
                      cList, nCharAtt, charAttNames, charAttCnts, charAtts, vNames) 
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
           type(iNode), pointer, OPTIONAL :: iList   ! int list
           type(cNode), pointer, OPTIONAL :: cList   ! real list
           type(rNode), pointer, OPTIONAL :: rList   ! char list
!
! !OUTPUT PARAMETERS:
!
           integer, OPTIONAL :: nIntAtt              ! num of int att
           integer, OPTIONAL :: nRealAtt             ! num of real att
           integer, OPTIONAL :: nCharAtt             ! num of char att
           character(len=MLEN), pointer, OPTIONAL :: intAttNames(:)
           character(len=MLEN), pointer, OPTIONAL :: realAttNames(:)
           character(len=MLEN), pointer, OPTIONAL :: charAttNames(:)
           integer, OPTIONAL, pointer :: intAttCnts(:) !data count in int att
           integer, OPTIONAL, pointer :: realAttCnts(:)!data count in real att
           integer, OPTIONAL, pointer :: charAttCnts(:)!data count in char att
           integer, OPTIONAL, pointer :: intAtts(:,:)  !int attribute
           real, OPTIONAL, pointer :: realAtts(:,:)    !real attribute
           character(len=MLEN), pointer, OPTIONAL :: charAtts(:) ! char att
           character(len=MLEN), pointer, OPTIONAL :: vNames(:)
!
! !DESCRIPTION:
!     retrieve user defined attributes from a list
!EOP
!------------------------------------------------------------------------------
           type(iNode), pointer :: p    ! pointer for integer list
           type(rNode), pointer :: rp   ! pointer for real list
           type(cNode), pointer :: cp   ! pointer for char list
           integer :: maxLen            ! length of a list
           integer :: cnt               ! max number of data in nodes
           integer :: i
           integer :: rtcode

           maxLen = 0
           cnt = 0

!          get attributes from an integer list
           if ( present(iList) ) then
              allocate(p) 
              p = iList
              call getMaxLenCnt(maxLen, cnt, iList=iList)
              if ( present(nIntAtt) ) nIntAtt = cnt
              allocate(intAttCnts(cnt), intAttNames(cnt), intAtts(cnt,maxLen))
              if (present(vNames)) allocate(vNames(cnt))
              i = 1
              do while ( associated(p) )
                  intAttNames(i) = trim(p%name)
                  if (present(vNames)) vNames(i) = trim(p%vName)
                  intAttCnts(i) = p%count
                  intAtts(i,:) = 0
                  intAtts(i,1:size(p%intData)) = p%intData
                  p => p%next
                  i = i + 1
              end do
           end if

!          get attributes from a real list
           if ( present(rList) ) then
              allocate(rp) 
              rp = rList
              call getMaxLenCnt(maxLen, cnt, rList=rList)
              if (present(nRealAtt)) nRealAtt = cnt
              allocate(realAttCnts(cnt),realAttNames(cnt),realAtts(cnt,maxLen))
              if (present(vNames)) allocate(vNames(cnt))
              i = 1
              do while ( associated(rp) )
                  realAttNames(i) = trim(rp%name)
                  realAttCnts(i) = rp%count
                  if (present(vNames)) vNames(i) = trim(rp%vName)
                  realAtts(i,1:size(rp%realData)) = rp%realData
                  rp => rp%next
                  i = i + 1
              end do
           end if

!          get attributes from a char list
           if ( present(cList) ) then
              allocate(cp) 
              cp = cList
              call getMaxLenCnt(maxLen, cnt, cList=cList)
              if ( present(nCharAtt) ) nCharAtt = cnt
              allocate(charAttCnts(cnt), charAttNames(cnt), charAtts(cnt))
              if (present(vNames)) allocate(vNames(cnt))
              i = 1
              do while ( associated(cp) )
                  charAttNames(i) = trim(cp%name)
                  if (present(vNames)) vNames(i) = trim(cp%vName)
                  charAttCnts(i) = cp%count
                  charAtts(i) = cp%charData
                  cp => cp%next
                  i = i + 1
              end do
           end if

        end subroutine getList

!------------------------------------------------------------------------------
!BOP
! !ROUTINE: getMaxLenCnt -- get length of a list and max number of data 
!                           in the nodes
!
! !INTERFACE:
        subroutine getMaxLenCnt(maxLen, count, iList, rList, cList)
!
! !ARGUMENTS:
!
! !INPUT PARAMETERS:
!
           type(iNode), pointer, OPTIONAL :: iList
           type(cNode), pointer, OPTIONAL :: cList
           type(rNode), pointer, OPTIONAL :: rList
!
! !OUTPUT PARAMETERS:
!
           integer, intent(out) :: maxLen
           integer, intent(out) :: count
!
! !DESCRIPTION:
!     get length of a list and max number of data in the nodes so that
!     maxLen/count can been used to allocate array.
!EOP
!------------------------------------------------------------------------------
           type(iNode), pointer :: p     ! int pointer for int list
           type(rNode), pointer :: rp    ! real pointer for real list
           type(cNode), pointer :: cp    ! char pointer for char list

           count = 0
           maxLen = 0

!          get maxLen/count from  a integer list
           if ( present(iList) ) then
              allocate(p) 
              p = iList
              do while ( associated(p) )
                  if (p%count .gt. maxLen) maxLen = p%count
                  count = count + 1
                  p => p%next
              end do
           end if

!          get maxLen/count from  a real list
           if ( present(rList) ) then
              allocate(rp) 
              rp = rList
              do while ( associated(rp) )
                  if (rp%count .gt. maxLen) maxLen = rp%count
                  count = count + 1
                  rp => rp%next
              end do
           end if

!          get maxLen/count from  a character list
           if ( present(cList) ) then
              allocate(cp) 
              cp = cList
              do while ( associated(cp) )
                  if (cp%count .gt. maxLen) maxLen = cp%count
                  count = count + 1
                  cp => cp%next
              end do
           end if

        end subroutine getMaxLenCnt



!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_DimInquire -- Gets dimension information from a CFIO file.
!
! !DESCRIPTION: This routine is used to get dimension information from
!               an existing CFIO file.  This dimension information can 
!               subsequently be used to allocate arrays for reading data
!               from the file.  For more complete information about the 
!               contents of a file, Cfio\_Inquire should be used.

! !INTERFACE:
!
      subroutine CFIO_DimInquire (fid,im,jm,km,lm,nvars,ngatts,rc)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
!
! !OUTPUT PARAMETERS:
!
      integer     im     ! Size of longitudinal dimension
      integer     jm     ! Size of latitudinal dimension
      integer     km     ! Size of vertical dimension
                         !   km=0 if surface-only file
      integer     lm     ! Number of times 
      integer     nvars  ! Number of variables
      integer     ngatts ! Number of global attributes
      integer     rc     ! Error return code:

                         !  rc = 0    all is well
                         !  rc = -19  unable to identify coordinate variable
                         !
                         !  NetCDF Errors
                         !  -------------
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -47  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (time)
                         !  rc = -48  error from NF90_INQUIRE
                         !  rc = -53  error from NF90_GET_ATT



! !REVISION HISTORY:
!
!  1998.07.02  Lucchesi           Initial interface design.
!  1998.08.05  Lucchesi           Added "ngatts"
!  1998.09.24  Lucchesi           Revamped error codes
!  1998.12.22  Lucchesi           Added IdentifyDim and associated code
!  1999.01.04  Lucchesi           Changed variable initialization
!  2008.03.14  Kokron             Initialize stationFile to false
!
!EOP
!-------------------------------------------------------------------------

      integer timeid, dimId, i
      integer attType, attLen
      character*(MAXCHR) dimName
      character*(MAXCHR) dimUnits
      character*(MAXCHR) vname
      integer dimSize
      integer nDims
      logical surfaceOnly
      logical stationFile
      integer myIndex
      integer varType, nvDims, vDims(MAXVDIMS), nvAtts
      integer tmpNvar

! Initialize variables

      surfaceOnly = .FALSE.
      stationFile = .false.

! Check FID here.

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'CFIO_DimInquire warning: MAXNCNAM is larger than ', &
                'dimName array size.'
      endif

! Get basic information from file.
 
    
      rc = NF90_INQUIRE(fid, nDims, nvars, ngatts, dimId)
      if (err("DimInqure: NF90_INQUIRE failed",rc,-48) .NE. 0)return

! Subtract dimension variables from the variable count.

      tmpNvar = nvars
      do i=1,nvars
        rc = NF90_INQUIRE_VARIABLE (fid,i,vname,varType,nvDims,vDims,nvAtts)
        if (err("DimInquire: variable inquire error",rc,-52) .NE. 0) &
           return
        if (nvDims .EQ. 1 .or. trim(vname) .eq. 'time_bnds') then
          tmpNvar = tmpNvar - 1
        endif
      enddo
      nvars = tmpNvar

! Extract dimension information

      do i=1,nDims
        rc = NF90_INQUIRE_DIMENSION (fid, i, dimName, dimSize)  
        if (err("DimInqure: can't get dim info",rc,-41) .NE. 0) return
        if (index(dimName,'station') .gt. 0) then
           stationFile = .true.
           im = dimSize
           jm = dimSize
           cycle
        end if
        if (trim(dimName) .eq. 'nv') cycle 

        rc = NF90_INQ_VARID (fid, dimName, dimId)
        if (err("DimInqure: NF90_INQ_VARID failed",rc,-40) .NE. 0) return
        rc = NF90_GET_ATT(fid,dimId,'units',dimUnits)
        if (err("DimInqure: could not get units for dimension",rc,-53)&
            .NE. 0) return
        myIndex = IdentifyDim (dimName, dimUnits)
        if ( myIndex .EQ. 0 ) then
          im = dimSize
        else if ( myIndex .EQ. 1 ) then
          jm = dimSize
        else if ( myIndex .EQ. 2 ) then
          km = dimSize
        else if ( myIndex .EQ. 3 ) then
          lm = dimSize
!print *, "dimUnits: ", trim(dimUnits)
!print *, "dimName: ", trim(dimName)
!        else
!          print *, 'CFIO_DimInquire: Coordinate variable ',       &
!                  TRIM(dimName),' with units of ',TRIM(dimUnits), &
!                  ' is not understood.'
!          rc = -19
!          return
        endif
      enddo

      if (nDims .EQ. 3 .and. .NOT. stationFile) then
        surfaceOnly = .TRUE.
      endif

      if (surfaceOnly) then
        km = 0
      endif

      rc=0
      return
      end subroutine CFIO_DimInquire


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GetBegDateTime - Get begin date/time of file
!
! !DESCRIPTION: This routine returns the begin date/begin time on file.
!               For a native CFIO file, it simply returns the value of
!  global attributes begin_date/begin_time. If these do no exist then
!  it attempts to parse the COARDS compliant time unit.
!
! !INTERFACE:
!
      subroutine GetBegDateTime ( fid, begDate, begTime, incSecs, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer fid      ! file ID
!
! !OUTPUT PARAMETERS:
!
      integer begDate  ! beginning date
      integer begTime  ! beginning time
      integer incSecs  ! time increment in secs
      integer rc       ! error return code
!
! !REVISION HISTORY:
!
!  1999.11.01  da Silva  Initial code.
!  1999.11.08  da Silva  Generic time coordinate variable (no name assumed).
!  2000.10.18  Lucchesi  Added ParseTimeUnits subroutine to handle a wider
!                        variety of Units formats.
!
!EOP
!-------------------------------------------------------------------------

      integer i, timeId, hour, min, sec, corner(1), timInc
      integer year, month, day
      character(len=MAXCHR) timeUnits, strTmp, dimUnits

      character*(MAXCHR) varName, dimName
      integer type, nvDims, vdims(MAXVDIMS), nvAtts, dimSize
      integer nDims, nvars, ngatts, dimId

!     Time conversion local variables
      real*4    rtime, rtime_array(1) 
      real*8    dtime, dtime_array(1)
      integer*2 itime, itime_array(1)
      integer*4 ltime, ltime_array(1)
      integer   t1, t2


!     Start by determing the ID of the time coordinate variable
!     ---------------------------------------------------------
      timeId = -1
      rc = NF90_INQUIRE (fid, nDims, nvars, ngatts, dimId)
      if (err("GetBegDateTime: NF90_INQUIRE failed",rc,-48) .NE. 0)return
      do i=1,nDims
        rc = NF90_INQUIRE_DIMENSION (fid, i, dimName, dimSize)  
        if (err("GetBegDateTime: can't get dim info",rc,-41) .NE. 0) return
        if (index(dimName,'station')  .gt. 0) cycle
        if (trim(dimName) .eq. 'nv') cycle
        if ( index(dimName,'edges') .gt. 0 ) cycle
        rc = NF90_INQ_VARID (fid, dimName, dimId)
        if (err("GetBegDateTime: NF90_INQ_VARID failed",rc,-40) .NE. 0) return
        rc = NF90_GET_ATT(fid,dimId,'units',dimUnits)
        if (err("GetBegDateTime: could not get units for dimension",rc,-53)&
            .NE. 0) return
        if ( IdentifyDim (dimName, dimUnits) .eq. 3 ) then
             timeId = dimId
             timeUnits = dimUnits
             exit
        end if
      end do

      if ( timeId .lt. 0 ) then
         rc = -43
         print *, "GetBegDateTime: could not find time coord"
         return
      end if

!     Try assuming this file has been written with CFIO
!     -------------------------------------------------
      rc = NF90_GET_ATT(fid,timeId,'begin_date',begDate)
      if ( rc .eq. 0 ) then
           rc = NF90_GET_ATT(fid,timeId,'begin_time',begTime)
      end if

!     Well, it must be a native CFIO file
!     -----------------------------------
      if ( rc .eq. 0 ) then
         rc = NF90_GET_ATT(fid,timeId,'time_increment',timInc)
         if (err("GetBegDateTime: missing time increment",rc,-44) .NE. 0)   &
             return
!ams     write (strTmp,'(i6)') timinc
!ams     read (strTmp,'(3I2)') hour, min, sec
         call CFIO_parseIntTime ( timinc, hour, min, sec )       
        incSecs = hour*3600 + min*60 + sec
!ams     print *, 'begdate, begtime, incsecs: ',begdate, begtime, incsecs
         return                               ! all done.
      end if

!     If could not find begin_date/begin_time attributes
!     then this is not a native CFIO file. In this case
!     attempt to parse the COARDS compliant time units
!     --------------------------------------------------
!ams      rc = NF90_GET_ATT(fid,timeId,'units',timeUnits)
!ams      if (err("GetBegDateTime: missing time.units",rc,-44) .NE. 0) return
      i = index(timeUnits,'since')
      if ( i .le. 0 ) then
          if (err("GetBegDateTime: invalid time units",1,-44) .NE. 0) return
      endif

!     Call to ParseTimeUnits replaces an internal read, that made assumptions
!     about the format of the Time Units string that were not always true.  
!     (RL: 10/2000)

      call ParseTimeUnits ( timeUnits, year, month, day, hour, min, sec, rc )
      begDate = year*10000 + month*100 + day
      begTime = hour*10000 + min*100   + sec

!     Determine time increment.
!     -------------------------
      rc = NF90_INQUIRE_VARIABLE (fid, timeID, varName, type, nvDims, vDims, &
          nvAtts)
      if (err("GetBegDateTime: error in time variable inquire",&
         rc,-52) .NE. 0) return
     
      if ( type .eq. NF90_FLOAT )  then
           corner(1) = 1
           rc = NF90_GET_VAR(fid,timeID,rtime_array,corner,(/1/))
           rtime = rtime_array(1)
           t1 = int(rtime) 
           corner(1) = 2
           rc = NF90_GET_VAR(fid,timeID,rtime_array,corner,(/1/))
           rtime = rtime_array(1)
           t2 = int(rtime)
      else if ( type .eq. NF90_DOUBLE ) then
           corner(1) = 1
           rc = NF90_GET_VAR(fid,timeID,dtime_array,corner,(/1/))
           dtime = dtime_array(1)
           t1 = int(dtime)
!ams       print *, t1, dtime, rc
           corner(1) = 2
           rc = NF90_GET_VAR(fid,timeID,dtime_array,corner,(/1/))
           dtime = dtime_array(1)
           t2 = int(dtime)
!ams       print *, t2, dtime, rc
      else if ( type .eq. NF90_SHORT  ) then
           corner(1) = 1
           rc = NF90_GET_VAR(fid,timeID,itime_array,corner,(/1/))
           itime = itime_array(1)
           t1 = itime
           corner(1) = 2
           rc = NF90_GET_VAR(fid,timeID,itime_array,corner,(/1/))
           itime = itime_array(1)
           t2 = itime
      else if ( type .eq. NF90_INT   ) then
           corner(1) = 1
           rc = NF90_GET_VAR(fid,timeID,ltime_array,corner,(/1/))
           ltime = ltime_array(1)
           t1 = ltime
           corner(1) = 2
           rc = NF90_GET_VAR(fid,timeID,ltime_array,corner,(/1/))
           ltime = ltime_array(1)
           t2 = ltime
      else
           if (err("GetBegDateTime: invalid time data type",&
              1,-44) .NE. 0) return
      endif


!     Convert time increment to seconds if necessary
!     ----------------------------------------------
      incSecs = t2 - t1
      if ( timeUnits(1:6) .eq.  'minute' ) then
           incSecs = incSecs * 60 
      else if ( timeUnits(1:4) .eq. 'hour'   ) then
           incSecs = incSecs * 60 * 60 
      else if ( timeUnits(1:3) .eq.  'day' ) then
           incSecs = incSecs * 60 * 60 * 24
      else
           if (err("GetBegDateTime: invalid time unit name",&
              1,-44) .NE. 0) return
      endif

!ams  print *, 'begdate, begtime, incsecs: ',begdate, begtime, incsecs

      incSecs = max ( 1, incSecs )

      rc = 0 ! all done

      return
      end subroutine GetBegDateTime



!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  IdentifyDim - Identify a cooridate variable
!
! !DESCRIPTION: This function attempts to identify a coordiante variable
!               from the name or units of the variable.  It does so by 
!               attempting to match the units specified in the COARDS 
!               conventions or by checking the name against commonly used
!               names.
! !INTERFACE:
!
      integer function IdentifyDim (dimName, dimUnits)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      character*(*) dimName  ! Name of the coordinate variable
      character*(*) dimUnits ! Units of the coordinate variable
!
! !RETURN VALUES:
!
!     0 = X dimension (longitude)
!     1 = Y dimension (latitude)
!     2 = Z dimension (level)
!     3 = Time
!    -1 = Unable to determine dimension
!
! !REVISION HISTORY:
!
!  1998.12.22  Lucchesi       Initial coding.
!  1999.11.02  da Silva       Made LATS4D compatible,
!  2001.01.02  da Silva       Cimcurventing PGI bugs.
!
!EOP
!-------------------------------------------------------------------------

        if (TRIM(dimUnits) .EQ. "degrees_north" ) then
            IdentifyDim = 1
            return
         end if

        if (TRIM(dimUnits) .EQ. "hPa" ) then
            IdentifyDim = 2
            return
        end if

        if ( trim(dimName) .eq. "time" ) then 
            IdentifyDim = 3
            return
        end if


        if (TRIM(dimUnits) .EQ. "degrees_east" .OR.            &
            trim(dimName)  .eq. "longitude"    .OR.           &
            trim(dimName)  .eq. "lon"  ) then
            IdentifyDim = 0
        else if (TRIM(dimUnits) .EQ. "degrees_north" ) then
            IdentifyDim = 1
        else if (  trim(dimName)  .eq. "latitude"    .OR.      &
                  trim(dimName)  .eq. "lat"  ) then
            IdentifyDim = 1
        else if (INDEX(dimName,"lev") .NE. 0 .OR.              &
                INDEX(dimName,"Height") .NE. 0) then
          IdentifyDim = 2
        else if (TRIM(dimUnits) .EQ. "mb" .OR.                 &
                TRIM(dimUnits) .EQ. "millibar" .OR.           &
                TRIM(dimUnits) .EQ. "sigma_level" .OR.        &
                TRIM(dimUnits) .EQ. "hPa") then
          IdentifyDim = 2
        else if (trim(dimName) .eq. "TIME" .OR.            &
                trim(dimName) .eq. "TIME:EOSGRID" .OR.     &
                trim(dimName) .eq. "time" .OR.             &
                trim(dimName) .eq. "Time") then
          IdentifyDim = 3
        else
          IdentifyDim = -1
        endif

      if (TRIM(dimUnits) .EQ. "degrees_east" .OR. &
     INDEX(dimName,"XDim") .NE. 0 .OR. &
     INDEX(dimName,"lon") .NE. 0) then
        IdentifyDim = 0
      else if (TRIM(dimUnits) .EQ. "degrees_north" .OR. &
     INDEX(dimName,"YDim") .NE. 0 .OR. &
     INDEX(dimName,"lat") .NE. 0) then
          IdentifyDim = 1
      else if (INDEX(dimName,"lev") .NE. 0 .OR. &
     INDEX(dimName,"Height") .NE. 0) then
         IdentifyDim = 2
      else if (TRIM(dimUnits) .EQ. "mb" .OR. &
     TRIM(dimUnits) .EQ. "millibar" .OR. &
     TRIM(dimUnits) .EQ. "sigma_level" .OR. &
     TRIM(dimUnits) .EQ. "hPa") then
          IdentifyDim = 2
      else if (INDEX(dimName,"TIME") .NE. 0 .OR. &
     INDEX(dimName,"time") .NE. 0 .OR. &
     INDEX(dimName,"Time") .NE. 0) then
          IdentifyDim = 3
      else
          IdentifyDim = -1
      endif

        end function IdentifyDim

      subroutine CFIO_parseIntTime ( hhmmss, hour, min, sec )      
         integer, intent(in)  :: hhmmss
         integer, intent(out) :: hour, min, sec 
         hour = hhmmss / 10000
         min  = mod(hhmmss,10000)/100
         sec  = mod(hhmmss,100)
      end subroutine CFIO_parseIntTime

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  strToInt - convert timeString to integer date and time
!
! !DESCRIPTION: This function attempts to identify a coordiante variable
!               from the name or units of the variable.  It does so by
!               attempting to match the units specified in the COARDS
!               conventions or by checking the name against commonly used
!               names.
! !INTERFACE:
!
        subroutine strToInt(timeString, date, begTime)
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      character(len=*), intent(in) :: timeString
                                    ! string expression of data and time
!
!
! !OUTPUT PARAMETERS:
!
      integer, intent(out) :: date
      integer, intent(out) :: begTime
                                                                                         
! !REVISION HISTORY:
!
!  2004.10.04  B. Yin         first version.
!
!EOP
!-------------------------------------------------------------------------
       integer :: iCnt, jCnt, dLen, tLen
       character(len=MVARLEN) :: sDate, sTime
       character(len=MVARLEN) :: strDate, strTime
       character :: char
                                                                                         
       dLen = index(timeString, 'T' )
       tLen = len(trim(timeString)) - dLen
       sDate = timeString(1:dLen-1)
       sTime = timeString(dLen+1:len(trim(timeString)))
       jCnt = 1
       strDate = ''
       do iCnt = 1, len(sDate)
          char = sDate(iCnt:iCnt)
          if (char .ne. ':' .and. char .ne. '-') then
             strDate(jCnt:jCnt) = char
             jCnt = jCnt + 1
          end if
       end do
       jCnt = 1
       strTime = ''
       do iCnt = 1, len(sTime)
          char = sTime(iCnt:iCnt)
          if (char .ne. ':' .and. char .ne. '-') then
             strTime(jCnt:jCnt) = char
             jCnt = jCnt + 1
          end if
       end do
       read(strDate,*) date
       read(strTime,*) begTime
       return
     end subroutine strToInt

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
! !ROUTINE:  CFIO_Open -- Opens an existing DAO gridded file 
!
!
! !DESCRIPTION: This routine opens an existing DAO gridded file.  The file
!               mode will be read/write.  If the application already knows
!               the contents of the file, it may begin interaction with the
!               file using the returned file handle.  Otherwise, the file
!               handle can be used with the "inquire" routines to gather 
!               information about the contents.  A negative return code 
!               indicates there were problems opening the file.
!
!
! !INTERFACE:
!
      subroutine CFIO_Open ( fname, fmode, fid, rc )

!
! !USES:
!

      Implicit NONE

!
! !INPUT PARAMETERS:
!

      character*(*)   fname         ! File name
      integer         fmode         ! File mode:  
                                    !   0 for READ-WRITE 
                                    !   non-zero for READ-ONLY

!
! !OUTPUT PARAMETERS:
!

      integer        fid            ! File handle
      integer        rc             ! Error return code:
                                    !   rc = 0    All is well
                                    !   rc = -39  error from ncopn (file open)
! !REVISION HISTORY:
!
!  1998.07.02   Lucchesi             Initial interface design.
!  1998.07.07   Lucchesi             Initial coding.
!  1998.12.09   Lucchesi             Corrected for ncopn bug.
!EOP
!-------------------------------------------------------------------------


       if ( fmode .EQ. 0) then
         fid = ncopn (fname, NCWRITE, rc)
       else
         fid = ncopn (fname, NCNOWRIT, rc)
       endif
       if (fid .LT. 0) then   ! ncopn has a bug.  error codes should
         rc = fid             ! be returned in rc, but in reality they
       endif                  ! are returned in fid.  

       if (err("Open: error opening file",rc,-39) .NE. 0) return

       rc = 0
       return
       end subroutine CFIO_Open

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_Close -- Closes file
!
! !DESCRIPTION: This routine is used to close an open CFIO stream.

! !INTERFACE:
!
      subroutine CFIO_Close ( fid, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:

                         !   rc = 0    all is well
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -54  error from ncclos (file close)
! !REVISION HISTORY:
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.03.30  Lucchesi           Documentation expanded.  Clean-up of code.
!                                 Added rc.
!
!EOP
!-------------------------------------------------------------------------

      integer i

      call ncclos (fid, rc)
      if (err("Close: error closing file",rc,-54) .NE. 0) return

      rc = 0
      return
      end subroutine CFIO_Close

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_PutIntAtt -- Write a user-defined integer attribute
!
! !DESCRIPTION: This routine allows the user to define an integer
!               attribute in an open CFIO file.
!
! !INTERFACE:
!
      subroutine CFIO_PutIntAtt ( fid, name, count, buf, prec, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of integers to write
      integer        buf(count) ! Buffer with integer values
      integer        prec       ! Desired precision of attribute value:
                                !   0 = 32 bit
                                !   1 = 64 bit
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !   rc = -12  error determining default precision
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from NF90_PUT_ATT (global attribute)
                         !   rc = -55  error from NF90_REDEF (enter define mode)
                         !   rc = -56  error from NF90_ENDDEF (exit define mode)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!  1998.09.28  Lucchesi           Added support for multiple precisions
!
!EOP
!-------------------------------------------------------------------------

      integer*4 dummy32
      integer*8 dummy64
      integer i

      integer*4, allocatable :: buf32(:)
      integer*8, allocatable :: buf64(:)

      rc = NF90_REDEF ( fid )
      if (err("PutIntAtt: could not enter define mode",rc,-55) .NE. 0) &
         return

      if ( HUGE(dummy32) .EQ. HUGE(i) .AND. prec .EQ. 0 ) then     ! -i4
        rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf) ! 32-bit out

      else if ( HUGE(dummy32) .EQ. HUGE(i) .AND. prec .EQ. 1 ) then  ! -i4
        allocate ( buf64(count) )                                    ! 64-bit out
        do i=1,count
          buf64(i) = buf(i)
        enddo
        rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf64 )
        deallocate (buf64)

      else if  (HUGE(dummy64) .EQ. HUGE(i) .AND. prec .EQ. 0 ) then  ! -i8
        allocate ( buf32(count) )                                    ! 32-bit out
        do i=1,count
          buf32(i) = buf(i)
        enddo
        rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf32 )
        deallocate (buf32)

      else if (HUGE(dummy64) .EQ. HUGE(i) .AND. prec .EQ. 1 ) then   ! -i8
        rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf ) ! 64-bit out

      else 
        rc = -12
        return
      endif
      if (err("PutIntAtt: error writing attribute",rc,-36) .NE. 0) &
         return

      rc = NF90_ENDDEF(fid)
      if (err("PutIntAtt: could not exit define mode",rc,-56) .NE. 0) &
         return

      rc = 0
      return
      end subroutine CFIO_PutIntAtt

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_PutRealAtt -- Write a user-defined real attribute
!
! !DESCRIPTION: This routine allows the user to define a real
!               attribute in an open CFIO file.
!
! !INTERFACE:
!
      subroutine CFIO_PutRealAtt ( fid, name, count, buf, prec, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of integers to write
      real           buf(count) ! Buffer with real values
      integer        prec       ! Desired precision of attribute value:
                                !   0 = 32 bit
                                !   1 = 64 bit
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !   rc = -12  error determining default precision
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from NF90_PUT_ATT (global attribute)
                         !   rc = -55  error from NF90_REDEF (enter define mode)
                         !   rc = -56  error from NF90_ENDDEF (exit define mode)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!  1998.09.28  Lucchesi           Added support for multiple precisions
!
!EOP
!-------------------------------------------------------------------------

      real*4 dummy32
      real*8 dummy64
      real r
      integer i
      real*4, allocatable :: buf32(:)
      real*8, allocatable :: buf64(:)

      rc = NF90_REDEF ( fid )
      if (err("PutRealAtt: could not enter define mode",rc,-55) .NE. 0) &
         return

      if (HUGE(dummy32) .EQ. HUGE(r) .AND. prec .EQ. 0) then        ! -r4
        rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf ) ! 32-bit out

      else if (HUGE(dummy32) .EQ. HUGE(r) .AND. prec .EQ. 1) then  ! -r4
        allocate (buf64(count))                                    ! 64-bit out
        do i=1,count
          buf64(i) = buf(i)
        enddo
        rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf64 )
        deallocate (buf64)

      else if (HUGE(dummy64) .EQ. huge(r) .AND. prec .EQ. 0) then  ! -r8
        allocate (buf32(count))                                    ! 32-bit out
        do i=1,count
          buf32(i) = buf(i)
        enddo
        rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf32 )
        deallocate (buf32)
       
      else if (HUGE(dummy64) .EQ. huge(r) .AND. prec .EQ. 1) then    ! -r8
        rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf ) ! 64-bit out
 
      else
        rc = -12
        return
      endif
      if (err("PutRealAtt: error writing attribute",rc,-36) .NE. 0) &
         return

      rc = NF90_ENDDEF(fid)
      if (err("PutRealAtt: could not exit define mode",rc,-56) .NE. 0) &
         return

      rc = 0
      return
      end subroutine CFIO_PutRealAtt


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_PutCharAtt -- Write a user-defined character attribute
!
! !DESCRIPTION: This routine allows the user to define a character (string)
!               attribute in an open CFIO file.
!
! !INTERFACE:
!
      subroutine CFIO_PutCharAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of characters to write
      character(len=MLEN) :: buf ! Buffer containing string
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from NF90_PUT_ATT (global attribute)
                         !   rc = -55  error from NF90_REDEF (enter define mode)
                         !   rc = -56  error from NF90_ENDDEF (exit define mode)
! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!
!EOP
!-------------------------------------------------------------------------

      rc = NF90_REDEF ( fid )
      if (err("PutCharAtt: could not enter define mode",rc,-55) .NE. 0) &
         return
      rc = NF90_PUT_ATT ( fid, NF90_GLOBAL, name, buf )
      if (err("PutCharAtt: error writing attribute",rc,-36) .NE. 0) &
         return
      rc = NF90_ENDDEF(fid)
      if (err("PutCharAtt: could not exit define mode",rc,-56) .NE. 0) &
         return

      rc = 0
      return
      end subroutine CFIO_PutCharAtt

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_GetAttNames -- Get global attribute names
!
! !DESCRIPTION: This routine allows the user to get the names of
!               global attributes.
!
! !INTERFACE:
!
      subroutine CFIO_GetAttNames ( fid, ngatts, aname, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer     ngatts        ! Expected number of attributes (input)
                                ! Actual number of attributes (output if rc=-2)
!
! !OUTPUT PARAMETERS:
!
      character*(*)  aname(ngatts)  ! Array of attribute names
      integer   rc       ! Error return code:
                         !  rc =  0  all is well
                         !  rc = -10  ngatts is incompatible with file
                         !  rc = -11  character string not long enough
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -48  error from NF90_INQUIRE
                         !   rc = -57  error from NF90_INQ_ATTNAME

! !REVISION HISTORY:
!
!  1998.08.05  Lucchesi           Initial interface design.
!  1998.08.05  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!
!EOP
!-------------------------------------------------------------------------

      integer ngattsFile, i
      integer nDims,dimSize,recDim 

! Check number of attributes against file

      rc = NF90_INQUIRE (fid,nDims,dimSize,ngattsFile,recdim)
      if (err("GetAttNames: NF90_INQUIRE failed",rc,-48) .NE. 0) return
      if (ngattsFile .NE. ngatts) then
        rc = -10
        ngatts = ngattsFile
        return
      endif

! Check length of aname string

      if (LEN(aname(1)) .lt. MAXNCNAM) then
        print *,'CFIO_GetAttNames: length of aname array must be at ', &
               'least ',MAXNCNAM,' bytes.'
        rc = -11
        return
      endif

! Read global attribute names

      do i=1,ngatts
        rc = NF90_INQ_ATTNAME(fid,NF90_GLOBAL,i,aname(i))
        if (err("GetAttNames: error reading attribute name",rc,-57) &
           .NE. 0) return
      enddo

      rc = 0
      return
      end subroutine CFIO_GetAttNames

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_AttInquire -- Get information about an attribute
!
! !DESCRIPTION: This routine allows the user to get information about
!               a global attribute of an open CFIO file.  This is most
!               useful for determining the number of values stored in an
!               attribute.
!
! !INTERFACE:
!
      subroutine CFIO_AttInquire ( fid, name, type, count, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
!
! !OUTPUT PARAMETERS:
!
      integer type       ! Code for attribute type
                         !   0 = integer
                         !   1 = real
                         !   2 = character
                         !   3 = 64-bit real
                         !   4 = 64-bit integer
                         !  -1 = other
      integer count      ! Number of items (length of array)
      integer rc         ! Error return code:
                         !   rc = 0    all is well
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -58  error from NF90_INQUIRE_ATTRIBUTE

!
! !NOTES:  The returned integer "type" for 64-bit integer is not supported
!          in the current implementation of netCDF/HDF.  When a user writes a
!          64-bit integer attribute using PutIntAtt, it is actually saved as
!          a 64-bit real by the HDF library.  Thus, upon reading the attribute, 
!          there is no way for HDF/CFIO to distinguish it from a REAL number.  
!          The user must realize this variable is really an integer and call 
!          GetIntAtt to read it.  Even for a 64-bit integer, type=4 will never
!          be returned unless there are changed to HDF/netCDF.
!
!
! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error codes, added type assignment.
!
!EOP
!-------------------------------------------------------------------------

      integer nctype

      rc = NF90_INQUIRE_ATTRIBUTE (fid, NF90_GLOBAL, name, nctype, count)
      if (err("AttInquire: error reading attribute info",rc,-58) &
           .NE. 0) return
      if (nctype .EQ. NF90_INT) then
        type = 0
      elseif (nctype .EQ. NF90_FLOAT) then
        type = 1
      elseif (nctype .EQ. NCCHAR) then
        type = 2
      elseif (nctype .EQ. NF90_DOUBLE) then
        type = 3
      else
        type = -1
      endif

      rc = 0
      return
      end subroutine CFIO_AttInquire

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_GetIntAtt -- Read a user-defined integer attribute
!
! !DESCRIPTION: This routine allows the user to read an integer 
!               attribute from an open CFIO file.
!
! !INTERFACE:
!
      subroutine CFIO_GetIntAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer        count      ! On input: Number of items in attribute
                                ! On output: If rc = -1, count will contain
                                !        the correct count of this attribute
!
! !OUTPUT PARAMETERS:
!
      integer   buf(count) ! Buffer with integer values
      integer   rc         ! Error return code:
                           !   rc = 0    all is well
                           !   rc = -1   invalid count
                           !   rc = -2   type mismatch
                           !   rc = -12  error determining default precision
                           !
                           !  NetCDF Errors
                           !  -------------
                           !   rc = -36  error from NF90_PUT_ATT (global attribute)
                           !   rc = -51  error from NF90_GET_ATT (global attribute)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.29  Lucchesi           Changed error handling.  Added 64-bit support.
!
!EOP
!-------------------------------------------------------------------------

      integer length, type
      integer i
      integer*4 dummy32
      integer*8 dummy64
      integer*4, allocatable :: buf32(:)
      integer*8, allocatable :: buf64(:)

      rc = NF90_INQUIRE_ATTRIBUTE (fid, NF90_GLOBAL, name, type, length)
      if (err("GetIntAtt: error reading attribute info",rc,-58) &
           .NE. 0) return

      if ( count .NE. length ) then
        rc = -1
        count = length
        return
      endif

      if ( type .NE. NF90_INT .AND. type .NE. NF90_DOUBLE) then
        rc = -2
        return
      endif
      if ( HUGE(dummy32) .EQ. HUGE(i)) then
        if ( type .EQ. NF90_INT ) then          ! -i4 32bit
          rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,buf)
        else            ! type .EQ. NF90_DOUBLE
          allocate (buf64(count))             ! -i4 64bit
          rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,buf64)
          do i=1,count
            buf(i) = buf64(i)
          enddo
          deallocate (buf64)
        endif
      else if (HUGE(dummy64) .EQ. HUGE(i)) then
        if ( type .EQ. NF90_INT ) then
          allocate (buf32(count))             ! -i8 32bit
          rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,buf32)
          do i=1,count
            buf(i) = buf32(i)
          enddo
          deallocate (buf32)
        else            ! type .EQ. NF90_DOUBLE
          rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,buf)
        endif
      else
        rc = -12
        return
      endif
      if (err("GetIntAtt: error reading attribute value",rc,-51) &
           .NE. 0) return

      rc = 0
      return
      end subroutine CFIO_GetIntAtt

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_GetRealAtt -- Read a user-defined real attribute
!
! !DESCRIPTION: This routine allows the user to read a real
!               attribute from an open CFIO file.
!
! !INTERFACE:
!
      subroutine CFIO_GetRealAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer        count      ! On input: Number of items in attribute
                                ! On output: If rc = -1, count will contain
                                !        the correct number of attributes
!
! !OUTPUT PARAMETERS:
!
      real     buf(count)  ! Buffer with real values
      integer  rc          ! Error return code:
                           !   rc = 0    all is well
                           !   rc = -1   invalid count
                           !   rc = -2   type mismatch
                           !   rc = -12  error determining default precision
                           !
                           !  NetCDF Errors
                           !  -------------
                           !   rc = -36  error from NF90_PUT_ATT (global attribute)
                           !   rc = -51  error from NF90_GET_ATT (global attribute)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.29  Lucchesi           Changed error handling.  Added 64-bit support.
!  1999.08.23  Lucchesi           Changed .OR. to .AND.
!
!EOP
!-------------------------------------------------------------------------

      integer length, type
      real r
      integer i
      real*4 dummy32
      real*8 dummy64
      real*4, allocatable :: buf32(:)
      real*8, allocatable :: buf64(:)

      rc = NF90_INQUIRE_ATTRIBUTE (fid, NF90_GLOBAL, name, type, length)
      if (err("GetRealAtt: error reading attribute info",rc,-58) &
           .NE. 0) return

      if ( count .NE. length ) then
        rc = -1
        count = length
        return
      endif
      if ( type .NE. NF90_FLOAT .AND. type .NE. NF90_DOUBLE) then
        rc = -2
        return
      endif

      if ( HUGE(dummy32) .EQ. HUGE(r)) then
        if ( type .EQ. NF90_FLOAT ) then         ! -r4 32bit
          rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,buf)
        else            ! type .EQ. NF90_DOUBLE
          allocate (buf64(count))             ! -r4 64bit
          rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,buf64)
          do i=1,count
            buf(i) = buf64(i)
          enddo
          deallocate (buf64)
        endif
      else if (HUGE(dummy64) .EQ. HUGE(r)) then
        if ( type .EQ. NF90_FLOAT ) then
          allocate (buf32(count))             ! -r8 32bit
          rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,buf32)
          do i=1,count
            buf(i) = buf32(i)
          enddo
          deallocate (buf32)
        else            ! type .EQ. NF90_DOUBLE
          rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,buf)
        endif
      else
        rc = -12
        return
      endif
      if (err("GetRealAtt: error reading attribute value",rc,-51) &
           .NE. 0) return

      rc = 0
      return
      end subroutine CFIO_GetRealAtt

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_GetCharAtt -- Read a user-defined character attribute
!
! !DESCRIPTION: This routine allows the user to read a character
!               attribute from an open CFIO file.
!
! !INTERFACE:
!
      subroutine CFIO_GetCharAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
!
! !INPUT/OUTPUT PARAMETERS:
!
      integer        count      ! On input: Number of items in attribute
                                ! On output: If rc = -1, count will contain
                                !        the correct number of attributes
!
! !OUTPUT PARAMETERS:
!
      character :: buf(count) ! Buffer with character values
!      character(len=MLEN) :: buf ! Buffer with character values
      integer   rc         ! Error return code:
                           !   rc = 0    all is well
                           !   rc = -1   invalid count
                           !   rc = -2   type mismatch
                           !
                           !  NetCDF Errors
                           !  -------------
                           !   rc = -36  error from NF90_PUT_ATT (global attribute)
                           !   rc = -51  error from NF90_GET_ATT (global attribute)
! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.29  Lucchesi           Changed error handling.
!
!EOP
!-------------------------------------------------------------------------

      integer length, type
      character(len=count) :: chartmp 

      rc = NF90_INQUIRE_ATTRIBUTE (fid, NF90_GLOBAL, name, type, length)
      if (err("GetCharAtt: error reading attribute info",rc,-58) &
           .NE. 0) return
      if ( count .NE. length ) then
        rc = -1
        count = length
        return
      endif
      if ( type .NE. NCCHAR) then
        rc = -2
        return
      endif

      rc  = NF90_GET_ATT(fid,NF90_GLOBAL,name,chartmp)
      if (err("GetCharAtt: error reading attribute value",rc,-51) &
           .NE. 0) return
 
      buf = chartmp
      rc = 0
      return
      end subroutine CFIO_GetCharAtt


!  The following function was taken from the book "Numerical Recipes in 
!  FORTRAN, the art of scientific computing (2nd Ed.), by William H. Press,
!  Saul A. Teukolsky, William T. Vetterling, and Brian P. Flannery (Cambridge 
!  University Press, 1992). 

      INTEGER FUNCTION julday(mm,id,iyyy)
      INTEGER id,iyyy,mm,IGREG
      PARAMETER (IGREG=15+31*(10+12*1582))
      INTEGER ja,jm,jy
      jy=iyyy
      if (jy.eq.0) then
        print *, 'julday: there is no year zero'
        return
      endif
      if (jy.lt.0) jy=jy+1
      if (mm.gt.2) then
        jm=mm+1
      else
        jy=jy-1
        jm=mm+13
      endif
      julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
      if (id+31*(mm+12*iyyy).ge.IGREG) then
        ja=int(0.01*jy)
        julday=julday+2-ja+int(0.25*ja)
      endif
      return
      END function julday


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOI
!
!  !TITLE: Calculate difference in seconds between two times.
!
!  !AUTHORS: Rob Lucchesi
!
!  !AFFILIATION: Data Assimilation Office, NASA/GSFC, Greenbelt, MD 20771
!
!  !DATE: October 17, 1997
!
!EOI
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: DiffDate --- Calculates the number of seconds between two times.
!
! !INTERFACE:
!

       integer function DiffDate (yyyymmhh_1,hhmmss_1,yyyymmhh_2,hhmmss_2)

! 
! !USES:
!

       implicit none

!
! !INPUT PARAMETERS:
!
       
       integer yyyymmhh_1               ! First date in YYYYYMMDD format
       integer hhmmss_1                 ! First time in HHMMSS format
       integer yyyymmhh_2               ! Second date in YYYYMMDD format
       integer hhmmss_2                 ! Second time in HHMMSS format

!
! !OUTPUT PARAMETERS:
!
!                 Integer function returns number of seconds between the
!                 the times given as input.  -1 is returned in the event 
!                 of an error.  
!
! !DESCRIPTION:   This function returns the number of seconds between two
!                 times.  Each time is specified with two integers, one 
!                 representing a date in the format YYYYMMDD and one 
!                 representing a time in the format HHMMSS.  This function 
!                 determines the Julian day of each date using the "julday"
!                 function from the book "Numerical Recipes in FORTRAN, the 
!                 art of scientific computing (2nd Ed.), by William H. Press, 
!                 Saul A. Teukolsky, William T. Vetterling, and Brian P. 
!                 Flannery (Cambridge University Press, 1992).  The difference
!                 between the two times is then calculated and returned.  The
!                 times need not be in chronological order as the function returns
!                 the abs value.  -1 is returned in the event of an error.
!
! !REVISION HISTORY:
!
!  17Oct97   Lucchesi    Initial version.
!  2010.05.11  Lucchesi  Integer for julian seconds changed to 64-bit.  StartDate
!                        constant no longer needed.
!
!EOP
!-------------------------------------------------------------------------

       integer year1,mon1,day1,hour1,min1,sec1
       integer year2,mon2,day2,hour2,min2,sec2
       integer(kind=8) julian1, julian2, julsec1, julsec2

       character*8 dateString

! Error checking.

!rl    if (yyyymmhh_1 .lt. 19000000 .or. yyyymmhh_1 .gt. 21000000 ) then
!rl      DiffDate=-1
!rl      return
!rl    endif
!rl    if (yyyymmhh_2 .lt. 19000000 .or. yyyymmhh_2 .gt. 21000000 ) then
!rl      DiffDate=-1
!rl      return
!rl    endif
       if (hhmmss_1 .lt. 0 .or. hhmmss_1 .ge. 240000 ) then
         DiffDate=-1
         return
       endif
       if (hhmmss_2 .lt. 0 .or. hhmmss_2 .ge. 240000 ) then
         DiffDate=-1
         return
       endif

! Convert Date/Time strings to integer variables.

!ams       write (dateString, 200) yyyymmhh_1
!ams 200    format (I8)
!ams       read (dateString, 201) 
!ams 201    format (I4,2I2)
!ams       write (dateString, 200) yyyymmhh_2
!ams       read (dateString, 201) year2, mon2, day2

       call CFIO_parseIntTime ( yyyymmhh_1, year1, mon1, day1 )
       call CFIO_parseIntTime ( yyyymmhh_2, year2, mon2, day2 )

!ams       write (dateString, 202) hhmmss_1
!ams 202    format (I6)
!ams        read (dateString, 203) hour1, min1, sec1
!ams 203    format (3I2)
!ams       write (dateString, 202) hhmmss_2
!ams       read (dateString, 203) hour2, min2, sec2

       call CFIO_parseIntTime ( hhmmss_1, hour1, min1, sec1 )
       call CFIO_parseIntTime ( hhmmss_2, hour2, min2, sec2 )

! Get Julian Days and subtract off a constant (Julian days since 7/14/66)
 
       julian1 = julday (mon1, day1, year1)
       julian2 = julday (mon2, day2, year2)
      
! Calculcate Julian seconds

       julsec1 = (julian1-1)*86400 + hour1*3600 + min1*60 + sec1
       julsec2 = (julian2-1)*86400 + hour2*3600 + min2*60 + sec2
       
!!!       DiffDate = iabs (julsec2 - julsec1)
       DiffDate = julsec2 - julsec1

       return
       end function DiffDate

      SUBROUTINE CALDAT (JULIAN,MM,ID,IYYY)                        
!                                                                  
!C   ROUTINE CONVERTS JULIAN DAY TO MONTH, DAY, & YEAR.               
!C   THIS CODE IS LIFTED FROM THE BOOK:                                
!C   W.H. PRESS ET AL., NUMERICAL RECIPES, CAMBRIDGE UNIV. PRESS, 1986.  
!C   THE ONLY MODIFICATION IS THAT REAL ARITHMETIC IS DONE IN R*8.
!C                                                                 
!C   THE ROUTINE OUTPUTS THE MONTH, DAY, AND YEAR ON WHICH THE      
!C   SPECIFIED JULIAN DAY STARTED AT NOON.                           
!C                                                                     
!C   TO CONVERT MODIFIED JULIAN DAY, CALL THIS ROUTINE WITH              
!C     JULIAN = MJD + 2400001                              
!C                                                          
      integer(kind=8) JULIAN
      integer IGREG
      integer JALPHA, JA, JB, JC, JD, JE, ID, MM, IYYY
      PARAMETER (IGREG=2299161)                             
      IF (JULIAN.GE.IGREG) THEN                              
         JALPHA=INT((DBLE(JULIAN-1867216)-0.25D0)/36524.25D0) 
         JA=JULIAN+1+JALPHA-INT(0.25D0*DBLE(JALPHA))           
      ELSE                                                      
         JA=JULIAN                                               
      ENDIF                                               
      JB=JA+1524                                           
      JC=INT(6680.D0+(DBLE(JB-2439870)-122.1D0)/365.25D0)   
      JD=365*JC+INT(0.25D0*DBLE(JC))                         
      JE=INT(DBLE(JB-JD)/30.6001D0)                           
      ID=JB-JD-INT(30.6001D0*DBLE(JE))                         
      MM=JE-1                                                   
      IF (MM.GT.12) MM=MM-12                                     
      IYYY=JC-4715                                                
      IF (MM.GT.2) IYYY=IYYY-1                                     
      IF (IYYY.LE.0) IYYY=IYYY-1                                    
      RETURN                                                         
      END subroutine CALDAT                                                            

      integer function err ( outstring, iret, ec ) 
      character *(*) outstring
      integer ec, rc, iret

      if (iret .EQ. 0) then
        err = iret
      else
        print *,"CFIO_",outstring
        iret = ec
        err = ec
      endif
 
      return
      end function err    

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  ParseTimeUnits -- Parse the COARDS time units string
!
! !DESCRIPTION: This subroutine takes as input the COARDS metadata time
!               units string and parses out the date included in the string.
!               This date typically represents the first time in a COARDS
!               HDF files.
! !INTERFACE:
!
      subroutine ParseTimeUnits ( TimeUnits, year, month, day, hour, min, sec, rc )
!
! !USES:
!
      implicit none
!
! !INPUT PARAMETERS:
!
      character*(MAXCHR) TimeUnits      ! Units metadata string from the Time coord var
!
! !OUTPUT PARAMETERS:
!
      integer        year               ! 4-digit year
      integer        month              ! month
      integer        day                ! day
      integer        hour               ! hour
      integer        min                ! minute
      integer        sec                ! second
      integer        rc                 ! return code
                                        !  0 = no error
                                        ! -1 = problem parsing string
 
! !REVISION HISTORY:
!
!  2000.10.18  Lucchesi       Initial coding.
!  2001.08.13  Lucchesi       Modified to better parse time:units string (needed for lats4d
!support)
!
!EOP
!-------------------------------------------------------------------------
 
 
      ! Local variables
 
      character*(MAXCHR)  NewUnits
      integer ypos(2), mpos(2), dpos(2), hpos(2), minpos(2), spos(2)
      integer i, j, inew, strlen
      integer firstdash, lastdash
      integer firstcolon, lastcolon
      integer lastspace
 
      strlen = LEN_TRIM (TimeUnits)
       
      firstdash = index(TimeUnits, '-')
      lastdash  = index(TimeUnits, '-', BACK=.TRUE.)
 
      if (firstdash .LE. 0 .OR. lastdash .LE. 0) then
        rc = -1
        return
      endif
       
      ypos(2) = firstdash - 1
      mpos(1) = firstdash + 1
      ypos(1) = ypos(2) - 4
 
      mpos(2) = lastdash - 1
      dpos(1) = lastdash + 1
      dpos(2) = dpos(1) + 2
 
      read ( TimeUnits(ypos(1):ypos(2)), * ) year
      read ( TimeUnits(mpos(1):mpos(2)), * ) month
      read ( TimeUnits(dpos(1):dpos(2)), * ) day
 
      firstcolon = index(TimeUnits, ':')
 
      if (firstcolon .LE. 0) then
         
        ! If no colons, check for hour.

        ! Logic below assumes a null character or something else is after the hour
        ! if we do not find a null character add one so that it correctly parses time
        if (TimeUnits(strlen:strlen) /= char(0)) then
           TimeUnits = trim(TimeUnits)//char(0)
           strlen=len_trim(TimeUnits)
        endif 
        lastspace = index(TRIM(TimeUnits), ' ', BACK=.TRUE.)
        if ((strlen-lastspace).eq.2 .or. (strlen-lastspace).eq.3) then
          hpos(1) = lastspace+1
          hpos(2) = strlen-1
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          min  = 0
          sec  = 0
        else
          print *, 'ParseTimeUnits: Assuming a starting time of 00z'
          hour = 0
          min  = 0
          sec  = 0
        endif
 
      else
        hpos(1) = firstcolon - 2
        hpos(2) = firstcolon - 1
        lastcolon =  index(TimeUnits, ':', BACK=.TRUE.)
        if ( lastcolon .EQ. firstcolon ) then
          mpos(1) = firstcolon + 1
          mpos(2) = firstcolon + 2
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          read (TimeUnits(mpos(1):mpos(2)), * ) min
          sec = 0
        else
          mpos(1) = firstcolon + 1
          mpos(2) = lastcolon - 1
          spos(1) = lastcolon + 1
          spos(2) = lastcolon + 2
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          read (TimeUnits(mpos(1):mpos(2)), * ) min
          read (TimeUnits(spos(1):spos(2)), * ) sec
        endif
      endif
        
      rc = 0
      return
      end subroutine ParseTimeUnits
       

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_SPutVar -- Write a variable to the file
! 
! !DESCRIPTION: This routine is used to write a variable to an open CFIO 
!               stream.  Multiple vertical levels can be written at one 
!               time provided they are contiguous in memory.  Date and time 
!               must be consistent with the time increment and the starting 
!               date/time as defined in CFIO\_Create.  Times must fall on 
!               minute boundaries to allow GrADS to work.  Error checking is 
!               done for dimensions that are out of bounds.
!
! !INTERFACE:
!
      subroutine CFIO_SPutVar ( fid, vname, yyyymmdd, hhmmss, &
                               im, jm, kbeg, kount, grid,     &
                               rc )  
!
! !USES:

      Implicit NONE  
!
! !INPUT PARAMETERS: 
!
      integer        fid                 ! File handle
      character*(*)  vname               ! Variable name
      integer        yyyymmdd            ! Year-month-day, e.g., 19971003
      integer        hhmmss              ! Hour-minute-second, e.g., 120000
 
      integer         im                 ! size of longitudinal dimension
      integer         jm                 ! size of latitudinal  dimension
      integer         kbeg               ! first level to write; if 2-D grid
                                         !   use kbeg = 0.
      integer         kount              ! number of levels to write
      real            grid(im,kount)  ! Gridded data to write at this time
                                     

! !OUTPUT PARAMETERS:
 
      integer        rc  ! Error return code:
                         !  rc =  0  all is well
                         !  rc = -2  time is inconsistent with increment 
                         ! rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file
                         !  rc = -5  jm is incompatible with file
                         !  rc = -6  time must fall on a minute boundary    
                         !  rc = -7  error in diffdate              
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -15  data outside of valid range
                         !  rc = -16  data outside of packing range
                         !  rc = -17  data outside of pack and valid range
                         !
                         !  NetCDF Errors
                         !  -------------
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

! !REVISION HISTORY: 
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.02.10 Lucchesi            Added support for applications running with
!                                 64-bit reals.
!  1998.03.30 Lucchesi            Documentation expanded.  Clean-up of code.
!  1998.07.02 Lucchesi            Replaced vid with vname in argument list &
!                                 made related mods to code.
!  1998.09.24 Lucchesi            Changed err codes, removed DIM_CHECK if-def
!  1998.10.27 Lucchesi            Added support for packing and range checks
!  1998.12.15 Lucchesi            Added support for skipping times (allTimes)
!  1999.01.04 Lucchesi            Fixed bug in skipping times (allTimes)/also 
!                                 changed variable initialization.
!  1999.07.13 Lucchesi            Changes for REAL or INT time dimension
!
!EOP
!-------------------------------------------------------------------------

      integer timeid, timeDimId, dimSize, dimId, timeType
      character*(MAXCHR) dimName
      integer corner(3), edges(3)
      integer vid
      integer seconds, timeIndex
      integer minutes                       ! added as a work-around
      integer idx, i, j, k
      integer begDate, begTime, timInc
      character*8 strBuf
      integer hour,min,sec,incSecs
      integer, allocatable ::  allTimes(:)
      integer fillTime

! Variables for dealing with precision

      real*4, allocatable :: grid_32(:,:)
      real*8, allocatable :: grid_64(:,:)
      real*4 dummy32
      real*8 dummy64
      real   dummy

! Variables for NF90_INQUIRE_VARIABLE

      character*(MAXCHR) varName
      integer type, nvDims, vdims(MAXVDIMS), nvAtts

! Variables for packing and range checking

      integer*2, allocatable :: grid_16(:,:)
      real*4, allocatable :: fminutes_32(:)
      real*4 high_32, low_32, amiss_32
      real*4 scale_32, offset_32
      logical outRange
      logical outPRange

! Variable initialization

      outRange = .FALSE.
      outPRange = .FALSE.

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'CFIO_PutVar warning: MAXNCNAM is larger than ', &
                'dimName array size.'
      endif

! Determine NetCDF variable ID.

      rc = NF90_INQ_VARID (fid, vname, vid)
      if (err("PutVar: variable not defined",rc,-40) .NE. 0) return

! Determine number of seconds since starting date/time.

      rc = NF90_INQ_VARID (fid, 'time', timeId)
      if (err("PutVar: time not defined",rc,-43) .NE. 0) return
      rc = NF90_GET_ATT(fid,timeId,'begin_date',begDate)
      if (err("PutVar: missing begin_date",rc,-44) .NE. 0) return
      rc = NF90_GET_ATT(fid,timeId,'begin_time',begTime)
      if (err("PutVar: missing begin_time",rc,-44) .NE. 0) return

      seconds = DiffDate (begDate, begTime, yyyymmdd, hhmmss)

      if (seconds .lt. 0) then
        print *, 'CFIO_PutVar: Error code from diffdate.  Problem with', &
                ' date/time.'
        rc = -7
        return
      endif
      if ( MOD (seconds,60) .eq. 0 ) then 
        minutes = seconds / 60
      else
        print *, 'CFIO_PutVar: Currently, times must fall on minute ',&
                'boundaries.'
        rc = -6
        return
      endif
 
! Confirm that this time is consistent with the starting time coupled with
! the time increment.

      rc = NF90_GET_ATT(fid,timeId,'time_increment',timInc)
      if (err("PutVar: missing time increment",rc,-44) .NE. 0) return
      
! Convert time increment to seconds.

!ams      write (strBuf,203) timinc
!ams 203   format (I6)
!ams       read (strBuf,204) hour, min, sec
!ams 204   format (3I2)
      call CFIO_parseIntTime ( timinc, hour, min, sec ) 
      incSecs = hour*3600 + min*60 + sec

      if ( MOD (seconds, incSecs) .ne. 0 ) then
        print *, 'CFIO_putvar: Absolute time of ',seconds,' not ',&
                'possible with an interval of ',incSecs
        rc = -2
        return
      else
        timeIndex = seconds/incSecs + 1
      endif

! Load starting indicies.

      if ( kbeg .eq. 0 ) then
        corner(1)=1
        corner(2)=timeIndex
        edges(1)=im
        edges(2)=1
      else
        corner(1)=1
        corner(2)=kbeg
        corner(3)=timeIndex
        edges(1)=im
        edges(2)=kount
        edges(3)=1
      endif

! Check variable against valid range.

      rc = NF90_GET_ATT(fid,vid,'vmin',low_32)
      if (err("PutVar: can't get vmin",rc,-53) .NE. 0) return
      rc = NF90_GET_ATT(fid,vid,'vmax',high_32)
      if (err("PutVar: can't get vmax",rc,-53) .NE. 0) return
      rc = NF90_GET_ATT(fid,vid,'fmissing_value',amiss_32)
      if (err("PutVar: can't get fmissing_value",rc,-53) .NE. 0) return
      if (abs(low_32) .NE. amiss_32 .OR. high_32 .NE. amiss_32) then
        do k=1,kount
          do i=1,im
            if (grid(i,k) .GT. high_32 .OR. grid(i,k) .LT. &
                low_32) then
              outRange = .TRUE.
              goto 100
            endif
          enddo
        enddo
100     continue
      endif
      
! Determine if we are writing single- or double-precision.

      rc = NF90_INQUIRE_VARIABLE (fid, vid, varName, type, nvDims, vDims, nvAtts)
      if (err("PutVar: error in variable inquire",rc,-52) .NE. 0) return

! Write variable in the appropriate precision.

      if (HUGE(dummy) .EQ. HUGE(dummy32)) then        ! -r4
        if (type .EQ. NF90_FLOAT) then                     ! 32-bit
          rc = NF90_PUT_VAR(fid,vid,grid,corner,edges)
        else if (type .EQ. NF90_DOUBLE) then               ! 64-bit
          allocate (grid_64(im,kount))
          do k=1,kount
              do i=1,im
                grid_64(i,k) = grid(i,k)
              enddo
          enddo
          rc = NF90_PUT_VAR(fid,vid,grid_64,corner,edges)
          deallocate (grid_64)
        else if (type .EQ. NF90_SHORT) then
          rc = NF90_GET_ATT(fid,vid,'packmax',high_32)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'packmin',low_32)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'scale_factor',scale_32)
          if (err("PutVar: error getting scale",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'add_offset',offset_32)
          if (err("PutVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,kount))
          do k=1,kount
              do i=1,im
                if ( grid(i,k) .LT. low_32 .OR. grid(i,k) .GT. &
                high_32) then
                  grid_16(i,k) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,k) = (grid(i,k) - offset_32)/scale_32
                endif
              enddo
          enddo
          rc = NF90_PUT_VAR(fid,vid,grid_16,corner,edges)
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else if (HUGE(dummy) .EQ. HUGE(dummy64)) then   ! -r8
        if (type .EQ. NF90_FLOAT) then                     ! 32-bit
          allocate (grid_32(im,kount))
          do k=1,kount
              do i=1,im
                grid_32(i,k) = grid(i,k)
              enddo
          enddo
          rc = NF90_PUT_VAR(fid, vid, grid_32, corner, edges)
          deallocate (grid_32)
        else if (type .EQ. NF90_DOUBLE) then                ! 64-bit
          rc = NF90_PUT_VAR(fid,vid,grid,corner,edges)
        else if (type .EQ. NF90_SHORT) then
          rc = NF90_GET_ATT(fid,vid,'packmax',high_32)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'packmin',low_32)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'scale_factor',scale_32)
          if (err("PutVar: error getting scale",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'add_offset',offset_32)
          if (err("PutVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,kount))
          do k=1,kount
              do i=1,im
                if ( grid(i,k) .LT. low_32 .OR. grid(i,k) .GT. &
                high_32) then
                  grid_16(i,k) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,k) = (grid(i,k) - offset_32)/scale_32
                endif
              enddo
          enddo
          rc = NF90_PUT_VAR(fid,vid,grid_16,corner,edges)
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else
        rc = -12
        return
      endif
      if (err("PutVar: error writing variable",rc,-45) .NE. 0) return

! Read time dimension scale and fill all values up to the current time.
! This will insure missing times are defined with the proper time value.

      rc = NF90_INQ_DIMID(fid, 'time', timeDimId)
      rc = NF90_INQUIRE_DIMENSION (fid, timeDimId, dimName, dimSize)
      dimSize = dimSize - 1           ! We've already written the 
                                      ! the new time.
      allocate ( allTimes (MAX(timeIndex,dimSize)) )
      allocate ( fminutes_32 (MAX(timeIndex,dimSize)) )

      if (dimSize .GT. 0) then
        ! Depending on the version of CFIO used to write the file, the Time
        ! dimension variable can either be floating point or integer.

        corner(1)=1
        edges(1)=dimSize

        rc = NF90_INQUIRE_VARIABLE (fid,timeId,dimName,timeType,nvDims,vDims,nvAtts)
        if (timeType .EQ. NF90_FLOAT) then
          rc = NF90_GET_VAR(fid,timeId,fminutes_32,corner,edges)
          do i=1,dimSize
            allTimes(i) = INT(fminutes_32(i))
          enddo
        else if (timeType .EQ. NF90_INT) then
          rc = NF90_GET_VAR(fid,timeId,allTimes,corner,edges)
        endif
        if (err("SPutVar: error reading times from file",rc,-46) .NE. 0) &
            return
      endif

      ! This loop fills the time dimension scale based on the time increment 
      ! specified in CFIO_Create.  If CFIO ever changes to support variable 
      ! time increments, this code MUST be changed.   

      do i=1,timeIndex-1
        fillTime = (i-1) * incSecs/60
        allTimes(i) = fillTime
      enddo
      allTimes(timeIndex) = minutes

! Write filled time array to file.

      corner(1)=1
      edges(1)=timeIndex

      if (timeType .EQ. NF90_FLOAT) then
        do i=1,timeIndex
          fminutes_32(i) = INT(allTimes(i))
        enddo
        rc = NF90_PUT_VAR(fid,timeId,fminutes_32,corner,edges)
      else if (timeType .EQ. NF90_INT) then
        rc = NF90_PUT_VAR(fid,timeId,allTimes,corner,edges)
      endif
      if (err("PutVar: error writing time",rc,-38) .NE. 0) return

      if (outRange .AND. outPRange) then
        rc = -17
      else if (outPRange) then
        rc = -16
      else if (outRange) then
        rc = -15
      else
        rc = 0
      endif

      deallocate ( allTimes )
      deallocate ( fminutes_32 )

      return
      end subroutine CFIO_SPutVar



!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_SGetVar -- Read a variable from the file 
!
! !DESCRIPTION: This routine will read one or more levels of "vname"
!               into the buffer passed in as "grid."  "fid" is the file
!               handle returned by Cfio\_open.
!
! !INTERFACE:
!
      subroutine CFIO_SGetVar ( fid, vname, yyyymmdd, hhmmss,&
                              im, jm, kbeg, kount, lm, grid, cyclic, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
      character*(*)  vname            ! Variable name
      integer        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer          hhmmss         ! Hour-minute-second, e.g., 120000
      integer         im              ! size of longitudinal dimension
      integer         jm              ! size of latitudinal  dimension
      integer         kbeg            ! first level to read; if 2-D grid
                                      !  set kbeg = 0.
      integer         kount           ! number of levels to read
      integer         lm              ! number of time steps
      logical cyclic     !  input file is cyclic or not

!
! !OUTPUT PARAMETERS:
!
      real         grid(im,kount)  ! Gridded data read for this time
      integer  rc        ! Error return code:
                         !  rc = 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file  
                         !  rc = -5  jm is incompatible with file  
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !
                         !  NetCDF Errors
                         !  -------------
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -48  error from NF90_INQUIRE
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE


! !REVISION HISTORY:
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.07.07 Lucchesi            Combined two GetVar routines into this one.
!  1998.09.24 Lucchesi            Updated error codes.
!  1999.06.21 Lucchesi            Bug fixed.  Unable to read HDF-EOS files
!                                 because was still looking for "lon" and "lat"
!  1999.06.21 Lucchesi            Added a check for time too early.
!  1999.11.02 da Silva            Made LATS4D compatible.
!  2006.06.08 byin                Added cyclic option
!
!EOP
!-------------------------------------------------------------------------

      integer timeId, begDate, begTime, seconds, minutes, timInc
      integer corner(3), edges(3), timeIndex
      integer vid
      integer i,j,k
      character*8 strBuf
      integer hour,min,sec,incSecs
      logical stationFile

! Variables for working with dimensions

      character*(MAXCHR) dimName
      character*(MAXCHR) dimUnits 
      character*(MAXCHR) varName
      integer dimSize, dimId
      integer nDims,nvars,ngatts
      integer varType, myIndex
      integer timeShift

! Variables for dealing with precision

      real*4, allocatable :: grid_32(:,:)
      real*8, allocatable :: grid_64(:,:)
      real*4 dummy32
      real*8 dummy64
      real   dummy

! Variables for NF90_INQUIRE_VARIABLE

      integer type, nvDims, vdims(MAXVDIMS), nvAtts

! Variables for packing

      integer*2, allocatable :: grid_16(:,:)
      integer*2 amiss_16
      real*4 amiss_32
      real*4 scale_32, offset_32


! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'CFIO_GetVar warning: MAXNCNAM is larger than ', &
                'dimName array size.'
      endif

! Get basic information from file.

      rc = NF90_INQUIRE (fid, nDims, nvars, ngatts, dimId)
      if (err("DimInqure: NF90_INQUIRE failed",rc,-48) .NE. 0)return

! Subtract dimension variables from the variable count.

      do i=1,nvars
        rc = NF90_INQUIRE_VARIABLE (fid,i,varName,varType,nvDims,vDims,nvAtts)
        if (err("DimInquire: variable inquire error",rc,-52) .NE. 0) &
           return
        if (nvDims .EQ. 1) then
          nvars = nvars - 1
        endif
      enddo

! Extract dimension information

      do i=1,nDims
        rc = NF90_INQUIRE_DIMENSION (fid, i, dimName, dimSize)
        if (err("DimInqure: can't get dim info",rc,-41) .NE. 0) return
        if (index(dimName,'station')  .gt. 0) then
           stationFile = .true.
           im = dimSize
           jm = dimSize
           cycle
        end if
        rc = NF90_INQ_VARID (fid, dimName, dimId)
        if (err("DimInqure: NF90_INQ_VARID failed",rc,-40) .NE. 0) return
        rc = NF90_GET_ATT(fid,dimId,'units',dimUnits)
        if (err("DimInqure: could not get units for dimension",rc,-53) &
            .NE. 0) return
        myIndex = IdentifyDim (dimName, dimUnits)
        if ( myIndex .EQ. 0 ) then
          if (dimSize .ne. im) then
            rc = -4
            im = dimSize
!            return
          endif
        else if ( myIndex .EQ. 1 ) then
          if (dimSize .ne. jm) then
            rc = -5
            jm = dimSize
!            return
          endif
        else if ( myIndex .EQ. 2 ) then
          if (kount .gt. dimSize) then
            rc = -3
!            return
          endif
        else if ( myIndex .EQ. 3 ) then
            timeId = dimId
        else
          print *, 'CFIO_GetVar: Coordinate variable ', &
                  TRIM(dimName),' with units of ',TRIM(dimUnits),&
                  ' is not understood.'
          rc = -19
!          return
        endif
      enddo

! Determine NetCDF variable ID.

      rc = NF90_INQ_VARID (fid, vname, vid)
      if (err("GetVar: variable not defined",rc,-40) .NE. 0) return
 
! Get beginning time & date.  Calculate offset seconds from start.

!ams  rc = NF90_GET_ATT(fid,timeId,'begin_date',begDate)
!ams     if (err("GetVar: missing begin_date",rc,-44) .NE. 0) return
!ams  rc = NF90_GET_ATT(fid,timeId,'begin_time',begTime)
!ams     if (err("GetVar: missing begin_time",rc,-44) .NE. 0) return

      call GetBegDateTime ( fid, begDate, begTime, incSecs, rc )
      if (err("GetVar: could not determine begin_date/begin_time",rc,-44)& 
         .NE. 0) return

      seconds = DiffDate (begDate, begTime, yyyymmdd, hhmmss)

! Make sure input time are valid, if time is not periodic

!ams  print *, '+++ incSecs, begDate, begTime: ', incsecs, begDate, begTime
!ams  print *, '+++ seconds, yyyymmdd, hhmmss: ', seconds, yyyymmdd, hhmmss

      if ( .not. cyclic ) then
         if (seconds .LT. 0) then
            print *, 'CFIO_SGetVar: Error code from diffdate.  Problem with', &
                ' date/time.'
            rc = -7
            return
         endif
         if (yyyymmdd .LT. begDate .OR. (begDate .EQ. yyyymmdd .AND. &
             hhmmss .LT. begTime) ) then
            print *, 'CFIO_GetVar: Requested time earlier than first time.'
            rc = -7
            return
         endif

      end if

      if ( MOD (seconds,60) .eq. 0 ) then
        minutes = seconds / 60
      else
        print *, 'CFIO_GetVar: Currently, times must fall on minute ',&
                'boundaries.'
        rc = -6
        return
      endif

! Determine the time index from the offset and time increment.

!ams      rc = NF90_GET_ATT(fid,timeId,'time_increment',timInc)
!ams      if (err("GetVar: missing time increment",rc,-44) .NE. 0) return

! Convert time increment to seconds.

!ams      write (strBuf,203) timinc
!ams 203   format (I6)
!ams      read (strBuf,204) hour, min, sec
!ams 204   format (3I2)
!ams       incSecs = hour*3600 + min*60 + sec

      if ( MOD (seconds, incSecs) .ne. 0 ) then
        print *, 'CFIO_getvar: Absolute time of ',seconds,' not ',&
                'possible with an interval of ',incSecs
        rc = -2
        return
      else
        timeIndex = seconds/incSecs + 1
      endif

! Wrap time index around if time dimension is periodic

!ams  print *, '--- Time Index: ', timeIndex

      if ( cyclic ) then
         timeShift = mod ( timeIndex, lm )
         if ( timeShift > 0 ) then
            timeIndex = timeShift 
         else 
            timeIndex = lm + timeShift
         end if
      end if

!ams  print *, '+++ Time Index, timeShift: ', timeIndex, timeShift

! Load starting indices.

      if ( kbeg .eq. 0 ) then
        corner(1)=1
        corner(2)=timeIndex
        edges(1)=im
        edges(2)=1
      else
        corner(1)=1
        corner(2)=kbeg
        corner(3)=timeIndex
        edges(1)=im
        edges(2)=kount
        edges(3)=1
      endif

! Determine data type.

      rc = NF90_INQUIRE_VARIABLE (fid, vid, varName, type, nvDims, vDims, nvAtts)
      if (err("GetVar: error in variable inquire",rc,-52) .NE. 0) return

! Read variable in the appropriate precision.

      if (HUGE(dummy) .EQ. HUGE(dummy32)) then        ! -r4
        if (type .EQ. NF90_FLOAT) then                     ! 32-bit
          rc = NF90_GET_VAR(fid,vid,grid,corner,edges)
        else if (type .EQ. NF90_DOUBLE) then               ! 64-bit
          allocate (grid_64(im,kount))
          rc = NF90_GET_VAR(fid,vid,grid_64,corner,edges)
          do k=1,kount
              do i=1,im
                grid(i,k) = grid_64(i,k)
              enddo
          enddo
          deallocate (grid_64)
        else if (type .EQ. NF90_SHORT) then
          rc = NF90_GET_ATT(fid,vid,'scale_factor',scale_32)
          if (err("GetVar: error getting scale",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'add_offset',offset_32)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'missing_value',amiss_16)
          if (err("GetVar: error getting missing",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'fmissing_value',amiss_32)
          if (err("GetVar: error getting fmissing",rc,-53) .NE. 0) return
          allocate (grid_16(im,kount))
          rc = NF90_GET_VAR(fid,vid,grid_16,corner,edges)
          do k=1,kount
              do i=1,im
                if ( grid_16(i,k) .EQ. amiss_16 ) then
                  grid(i,k) = amiss_32
                else
                  grid(i,k) = scale_32*grid_16(i,k) + offset_32
                endif
              enddo
          enddo
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else if (HUGE(dummy) .EQ. HUGE(dummy64)) then   ! -r8
        if (type .EQ. NF90_FLOAT) then                     ! 32-bit
          allocate (grid_32(im,kount))
!          print *, "im,kount, varName,rc: ",im,kount,trim(varName), rc
!          print *, "corner: ",corner
!          print *, "edges: ", edges
          rc = NF90_GET_VAR(fid,vid,grid_32,corner,edges)
!          print *, "ts: ",grid_32
          do k=1,kount
              do i=1,im
                grid(i,k) = grid_32(i,k)
              enddo
          enddo
          deallocate (grid_32)
        elseif (type .EQ. NF90_DOUBLE) then                ! 64-bit
          rc= NF90_GET_VAR(fid,vid,grid,corner,edges)
        else if (type .EQ. NF90_SHORT) then
          rc = NF90_GET_ATT(fid,vid,'scale_factor',scale_32)
          if (err("GetVar: error getting scale",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'add_offset',offset_32)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'missing_value',amiss_16)
          if (err("GetVar: error getting missing",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'fmissing_value',amiss_32)
          if (err("GetVar: error getting fmissing",rc,-53) .NE. 0) return
          allocate (grid_16(im,kount))
          rc = NF90_GET_VAR(fid,vid,grid_16,corner,edges)
          do k=1,kount
              do i=1,im
                if ( grid_16(i,k) .EQ. amiss_16 ) then
                  grid(i,k) = amiss_32
                else
                  grid(i,k) = scale_32*grid_16(i,k) + offset_32
                endif
              enddo
          enddo
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else
        rc = -12
        return
      endif
      if (err("GetVar: error reading variable",rc,-46) .NE. 0) return
 
      rc = 0
      return
      end subroutine CFIO_SGetVar


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_GetVar -- Read a variable from the file 
!
! !DESCRIPTION: This routine will read one or more levels of "vname"
!               into the buffer passed in as "grid."  "fid" is the file
!               handle returned by Cfio\_open.
!
! !INTERFACE:
!
      subroutine CFIO_GetVar ( fid, vname, yyyymmdd, hhmmss,&
                              im, jm, kbeg, kount, lm, grid, cyclic, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
      character*(*)  vname            ! Variable name
      integer        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer          hhmmss         ! Hour-minute-second, e.g., 120000
      integer         im              ! size of longitudinal dimension
      integer         jm              ! size of latitudinal  dimension
      integer         kbeg            ! first level to read; if 2-D grid
                                      !  set kbeg = 0.
      integer         kount           ! number of levels to read
      integer         lm              ! number of time steps
      logical cyclic     !  input file is cyclic or not

!
! !OUTPUT PARAMETERS:
!
      real         grid(im,jm,kount)  ! Gridded data read for this time
      integer  rc        ! Error return code:
                         !  rc = 0   all is well
                         !  rc = -2  time is inconsistent with increment
                         !  rc = -3  number of levels is incompatible with file
                         !  rc = -4  im is incompatible with file  
                         !  rc = -5  jm is incompatible with file  
                         !  rc = -6  time must fall on a minute boundary
                         !  rc = -7  error in diffdate
                         !  rc = -12  error determining default precision
                         !  rc = -13  error determining variable type
                         !  rc = -19  unable to identify coordinate variable
                         !
                         !  NetCDF Errors
                         !  -------------
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable)
                         !  rc = -40  error from NF90_INQ_VARID
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon)
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev)
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error from NF90_GET_ATT (time attribute)
                         !  rc = -46  error from NF90_GET_VAR
                         !  rc = -48  error from NF90_INQUIRE
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE


! !REVISION HISTORY:
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.07.07 Lucchesi            Combined two GetVar routines into this one.
!  1998.09.24 Lucchesi            Updated error codes.
!  1999.06.21 Lucchesi            Bug fixed.  Unable to read HDF-EOS files
!                                 because was still looking for "lon" and "lat"
!  1999.06.21 Lucchesi            Added a check for time too early.
!  1999.11.02 da Silva            Made LATS4D compatible.
!  2006.06.08 byin                Added cyclic option
!
!EOP
!-------------------------------------------------------------------------

      integer timeId, begDate, begTime, seconds, minutes, timInc
      integer timeShift
      integer corner(4), edges(4), timeIndex
      integer vid
      integer i,j,k
      character*8 strBuf
      integer hour,min,sec,incSecs

! Variables for working with dimensions

      character*(MAXCHR) dimName
      character*(MAXCHR) dimUnits 
      character*(MAXCHR) varName
      integer dimSize, dimId
      integer nDims,nvars,ngatts
      integer varType, myIndex

! Variables for dealing with precision

      real*4, allocatable :: grid_32(:,:,:)
      real*8, allocatable :: grid_64(:,:,:)
      real*4 dummy32
      real*8 dummy64
      real   dummy

! Variables for NF90_INQUIRE_VARIABLE

      integer type, nvDims, vdims(MAXVDIMS), nvAtts

! Variables for packing

      integer*2, allocatable :: grid_16(:,:,:)
      integer*2 amiss_16
      real*4 amiss_32
      real*4 scale_32, offset_32

! Initialize these, just in case

      corner = 1
      edges  = 1

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'CFIO_GetVar warning: MAXNCNAM is larger than ',&
                'dimName array size.'
      endif

! Get basic information from file.

      rc = NF90_INQUIRE (fid, nDims, nvars, ngatts, dimId)
      if (err("DimInqure: NF90_INQUIRE failed",rc,-48) .NE. 0)return

! Subtract dimension variables from the variable count.

      do i=1,nvars
        rc = NF90_INQUIRE_VARIABLE (fid,i,varName,varType,nvDims,vDims,nvAtts)
        if (err("DimInquire: variable inquire error",rc,-52) .NE. 0)&
           return
        if (nvDims .EQ. 1 .or. trim(vname) .eq. 'time_bnds') then
          nvars = nvars - 1
        endif
      enddo

! Extract dimension information

      do i=1,nDims
        rc = NF90_INQUIRE_DIMENSION (fid, i, dimName, dimSize)
        if (err("DimInqure: can't get dim info",rc,-41) .NE. 0) return
        if (trim(dimName) .eq. 'nv' ) cycle
        if (index(dimName,'edges') .gt. 0 ) cycle
        if (index(dimName,'station') .gt. 0 ) cycle
        rc = NF90_INQ_VARID (fid, dimName, dimId)
        if (err("DimInqure: NF90_INQ_VARID failed",rc,-40) .NE. 0) return
        rc = NF90_GET_ATT(fid,dimId,'units',dimUnits)
        if (err("DimInqure: could not get units for dimension",rc,-53)&
            .NE. 0) return
!        myIndex = IdentifyDim (dimName, dimUnits)
!        if ( myIndex .EQ. 0 ) then
!          if (dimSize .ne. im) then
!            rc = -4
!            im = dimSize
!            return
!          endif
!        else if ( myIndex .EQ. 1 ) then
!          if (dimSize .ne. jm) then
!            rc = -5
!            jm = dimSize
!            return
!          endif
!        else if ( myIndex .EQ. 2 ) then
!          if (kount .gt. dimSize) then
!            rc = -3
!            return
!          endif
!        else if ( myIndex .EQ. 3 ) then
!            timeId = dimId
!        else
!          print *, 'CFIO_GetVar: Coordinate variable ',&
!                  TRIM(dimName),' with units of ',TRIM(dimUnits),&
!                  ' is not understood.'
!          rc = -19
!          return
!        endif
      enddo

! Determine NetCDF variable ID.

      rc = NF90_INQ_VARID (fid, vname, vid)
      if (err("GetVar: variable not defined",rc,-40) .NE. 0) return
 
! Get beginning time & date.  Calculate offset seconds from start.

!ams     rc = NF90_GET_ATT(fid,timeId,'begin_date',begDate)
!ams     if (err("GetVar: missing begin_date",rc,-44) .NE. 0) return
!ams     rc = NF90_GET_ATT(fid,timeId,'begin_time',begTime)
!ams     if (err("GetVar: missing begin_time",rc,-44) .NE. 0) return

      call GetBegDateTime ( fid, begDate, begTime, incSecs, rc )
      if (err("GetVar: could not determine begin_date/begin_time",rc,-44) &
         .NE. 0) return

      seconds = DiffDate (begDate, begTime, yyyymmdd, hhmmss)

! Make sure input time are valid, if time is not periodic

!ams  print *, '+++ incSecs, begDate, begTime: ', incsecs, begDate, begTime
!ams  print *, '+++ seconds, yyyymmdd, hhmmss: ', seconds, yyyymmdd, hhmmss

      if ( .not. cyclic ) then
         if (seconds .LT. 0) then
            print *, 'CFIO_GetVar: Error code from diffdate.  Problem with', &
                ' date/time.'
            rc = -7
            return
         endif
         if (yyyymmdd .LT. begDate .OR. (begDate .EQ. yyyymmdd .AND. &
             hhmmss .LT. begTime) ) then
            print *, 'CFIO_GetVar: Requested time earlier than first time.'
            rc = -7
            return
         endif

      end if

      if ( MOD (seconds,60) .eq. 0 ) then
        minutes = seconds / 60
      else
        print *, 'CFIO_GetVar: Currently, times must fall on minute ',&
                'boundaries.'
        rc = -6
        return
      endif

! Determine the time index from the offset and time increment.

!ams  rc = NF90_GET_ATT(fid,timeId,'time_increment',timInc)
!ams      if (err("GetVar: missing time increment",rc,-44) .NE. 0) return

! Convert time increment to seconds.

!ams      write (strBuf,203) timinc
!ams 203   format (I6)
!ams      read (strBuf,204) hour, min, sec
!ams 204   format (3I2)
!ams       incSecs = hour*3600 + min*60 + sec

      if ( MOD (seconds, incSecs) .ne. 0 ) then
        print *, 'CFIO_getvar: Absolute time of ',seconds,' not ',&
                'possible with an interval of ',incSecs
        rc = -2
        return
      else
        timeIndex = seconds/incSecs + 1
      endif

! Wrap time index around if time dimension is periodic

!ams  print *, '--- Time Index: ', timeIndex

      if ( cyclic ) then
         timeShift = mod ( timeIndex, lm )
         if ( timeShift > 0 ) then
            timeIndex = timeShift 
         else 
            timeIndex = lm + timeShift
         end if
      end if

!ams  print *, '+++ Time Index, timeShift: ', timeIndex, timeShift

! Load starting indicies.

      if ( kbeg .eq. 0 ) then
        corner(1)=1
        corner(2)=1
        corner(3)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=1
      else
        corner(1)=1
        corner(2)=1
        corner(3)=kbeg
        corner(4)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=kount
        edges(4)=1
      endif

! Determine data type.

      rc = NF90_INQUIRE_VARIABLE (fid, vid, varName, type, nvDims, vDims, nvAtts)
      if (err("GetVar: error in variable inquire",rc,-52) .NE. 0) return

! Read variable in the appropriate precision.

      if (HUGE(dummy) .EQ. HUGE(dummy32)) then        ! -r4
        if (type .EQ. NF90_FLOAT) then                     ! 32-bit
          rc = NF90_GET_VAR(fid, vid, grid, corner, edges)
          if(rc /=0) then
            print*,'Error reading variable using NF90_GET_VAR',rc
            print*, NF_STRERROR(rc)
            return
          endif
        else if (type .EQ. NF90_DOUBLE) then               ! 64-bit
          allocate (grid_64(im,jm,kount))
          rc = NF90_GET_VAR(fid, vid, grid_64, corner, edges)
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid(i,j,k) = grid_64(i,j,k)
              enddo
            enddo
          enddo
          deallocate (grid_64)
        else if (type .EQ. NF90_SHORT) then
          rc = NF90_GET_ATT(fid,vid,'scale_factor',scale_32)
          if (err("GetVar: error getting scale",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'add_offset',offset_32)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'missing_value',amiss_16)
          if (err("GetVar: error getting missing",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'fmissing_value',amiss_32)
          if (err("GetVar: error getting fmissing",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount))
          rc = NF90_GET_VAR(fid, vid, grid_16, corner, edges)
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid_16(i,j,k) .EQ. amiss_16 ) then
                  grid(i,j,k) = amiss_32
                else
                  grid(i,j,k) = scale_32*grid_16(i,j,k) + offset_32
                endif
              enddo
            enddo
          enddo
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else if (HUGE(dummy) .EQ. HUGE(dummy64)) then   ! -r8
        if (type .EQ. NF90_FLOAT) then                     ! 32-bit
          allocate (grid_32(im,jm,kount))
          rc = NF90_GET_VAR(fid, vid, grid_32, corner, edges)
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid(i,j,k) = grid_32(i,j,k)
              enddo
            enddo
          enddo
          deallocate (grid_32)
        elseif (type .EQ. NF90_DOUBLE) then                ! 64-bit
          rc = NF90_GET_VAR(fid, vid, grid, corner, edges)
        else if (type .EQ. NF90_SHORT) then
          rc = NF90_GET_ATT(fid,vid,'scale_factor',scale_32)
          if (err("GetVar: error getting scale",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'add_offset',offset_32)
          if (err("GetVar: error getting offset",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'missing_value',amiss_16)
          if (err("GetVar: error getting missing",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'fmissing_value',amiss_32)
          if (err("GetVar: error getting fmissing",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount))
          rc = NF90_GET_VAR(fid, vid, grid_16, corner, edges)
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid_16(i,j,k) .EQ. amiss_16 ) then
                  grid(i,j,k) = amiss_32
                else
                  grid(i,j,k) = scale_32*grid_16(i,j,k) + offset_32
                endif
              enddo
            enddo
          enddo
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else
        rc = -12
        return
      endif
      if (err("GetVar: error reading variable",rc,-46) .NE. 0) return
 
      rc = 0
      return
      end subroutine CFIO_GetVar


!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_PutVar -- Write a variable to the file
! 
! !DESCRIPTION: This routine is used to write a variable to an open CFIO 
!               stream.  Multiple vertical levels can be written at one 
!               time provided they are contiguous in memory.  Date and time 
!               must be consistent with the time increment and the starting 
!               date/time as defined in CFIO\_Create.  Times must fall on 
!               minute boundaries to allow GrADS to work.  Error checking is 
!               done for dimensions that are out of bounds.
!
! !INTERFACE:
!
      subroutine CFIO_PutVar ( fid, vname, yyyymmdd, hhmmss, &
                              im, jm, kbeg, kount, grid,     &
                              rc )  
!
! !USES:

      Implicit NONE  
!
! !INPUT PARAMETERS: 
!
      integer        fid                 ! File handle
      character*(*)  vname               ! Variable name
      integer        yyyymmdd            ! Year-month-day, e.g., 19971003
      integer        hhmmss              ! Hour-minute-second, e.g., 120000
 
      integer         im                 ! size of longitudinal dimension
      integer         jm                 ! size of latitudinal  dimension
      integer         kbeg               ! first level to write; if 2-D grid
                                         !   use kbeg = 0.
      integer         kount              ! number of levels to write
      real            grid(im,jm,kount)  ! Gridded data to write at this time
                                     

! !OUTPUT PARAMETERS:
 
      integer        rc  ! Error return code:
                         !  rc =  0  all is well
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
                         !
                         !  NetCDF Errors
                         !  -------------
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

! !REVISION HISTORY: 
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.02.10 Lucchesi            Added support for applications running with
!                                 64-bit reals.
!  1998.03.30 Lucchesi            Documentation expanded.  Clean-up of code.
!  1998.07.02 Lucchesi            Replaced vid with vname in argument list &
!                                 made related mods to code.
!  1998.09.24 Lucchesi            Changed error codes, removed DIM_CHECK if-def
!  1998.10.27 Lucchesi            Added support for packing and range checks
!  1998.12.15 Lucchesi            Added support for skipping times (allTimes)
!  1999.01.04 Lucchesi            Fixed bug in skipping times (allTimes)/also 
!                                 changed variable initialization.
!  1999.07.13 Lucchesi            Changes for REAL or INT time dimension
!
!EOP
!-------------------------------------------------------------------------

      integer timeid, timeDimId, dimSize, dimId, timeType
      character*(MAXCHR) dimName
      integer corner(4), edges(4)
      integer vid
      integer seconds, timeIndex
      integer minutes                       ! added as a work-around
      integer idx, i, j, k
      integer begDate, begTime, timInc
      character*8 strBuf
      integer hour,min,sec,incSecs
      integer, allocatable ::  allTimes(:)
      integer fillTime

! Variables for dealing with precision

      real*4, allocatable :: grid_32(:,:,:)
      real*8, allocatable :: grid_64(:,:,:)
      real*4 dummy32
      real*8 dummy64
      real   dummy

! Variables for NF90_INQUIRE_VARIABLE

      character*(MAXCHR) varName
      integer type, nvDims, vdims(MAXVDIMS), nvAtts

! Variables for packing and range checking

      integer*2, allocatable :: grid_16(:,:,:)
      real*4, allocatable :: fminutes_32(:)
      real*4 high_32, low_32, amiss_32
      real*4 scale_32, offset_32
      logical outRange
      logical outPRange

! Variable initialization

      outRange = .FALSE.
      outPRange = .FALSE.

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'CFIO_PutVar warning: MAXNCNAM is larger than ',&
                'dimName array size.'
      endif

! Determine NetCDF variable ID.

      rc = NF90_INQ_VARID (fid, vname, vid)
      if (err("PutVar: variable not defined",rc,-40) .NE. 0) return

! Determine number of seconds since starting date/time.

      rc = NF90_INQ_VARID (fid, 'time', timeId)
      if (err("PutVar: time not defined",rc,-43) .NE. 0) return
      rc = NF90_INQ_DIMID(fid, 'time', timeDimId)
      rc = NF90_INQUIRE_DIMENSION(fid, timeDimId, dimName, dimSize)
      rc = NF90_GET_ATT(fid,timeId,'begin_date',begDate)
      if (err("PutVar: missing begin_date",rc,-44) .NE. 0) return
      rc = NF90_GET_ATT(fid,timeId,'begin_time',begTime)
      if (err("PutVar: missing begin_time",rc,-44) .NE. 0) return

      seconds = DiffDate (begDate, begTime, yyyymmdd, hhmmss)

      if (seconds .lt. 0) then
        print *, 'CFIO_PutVar: Error code from diffdate.  Problem with',&
                ' date/time.'
        rc = -7
        return
      endif
      if ( MOD (seconds,60) .eq. 0 ) then 
        minutes = seconds / 60
      else
        print *, 'CFIO_PutVar: Currently, times must fall on minute ',&
                'boundaries.'
        rc = -6
        return
      endif
 
! Confirm that this time is consistent with the starting time coupled with
! the time increment.

      rc = NF90_GET_ATT(fid,timeId,'time_increment',timInc)
      if (err("PutVar: missing time increment",rc,-44) .NE. 0) return
      
! Convert time increment to seconds.

!ams      write (strBuf,203) timinc
!ams 203   format (I6)
!ams      read (strBuf,204) hour, min, sec
!ams 204   format (3I2)
      call CFIO_parseIntTime ( timinc, hour, min, sec ) 
      incSecs = hour*3600 + min*60 + sec

      if ( MOD (seconds, incSecs) .ne. 0 ) then
        print *, 'CFIO_putvar: Absolute time of ',seconds,' not ',&
                'possible with an interval of ',incSecs
        rc = -2
        return
      else
        timeIndex = seconds/incSecs + 1
      endif

! Load starting indicies.

      if ( kbeg .eq. 0 ) then
        corner(1)=1
        corner(2)=1
        corner(3)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=1
      else
        corner(1)=1
        corner(2)=1
        corner(3)=kbeg
        corner(4)=timeIndex
        edges(1)=im
        edges(2)=jm
        edges(3)=kount
        edges(4)=1
      endif

! Check variable against valid range.

      rc = NF90_GET_ATT(fid,vid,'vmin',low_32)
      if (err("PutVar: can't get vmin",rc,-53) .NE. 0) return
      rc = NF90_GET_ATT(fid,vid,'vmax',high_32)
      if (err("PutVar: can't get vmax",rc,-53) .NE. 0) return
      rc = NF90_GET_ATT(fid,vid,'fmissing_value',amiss_32)
      if (err("PutVar: can't get fmissing_value",rc,-53) .NE. 0) return
      if (abs(low_32) .NE. amiss_32 .OR. high_32 .NE. amiss_32) then
        do k=1,kount
          do j=1,jm
            do i=1,im
              if (grid(i,j,k) .GT. high_32 .OR. grid(i,j,k) .LT. &
             low_32) then
                outRange = .TRUE.
                goto 100
              endif
            enddo
          enddo
        enddo
100     continue
      endif
      
! Determine if we are writing single- or double-precision.

      rc = NF90_INQUIRE_VARIABLE (fid, vid, varName, type, nvDims, vDims, nvAtts)
      if (err("PutVar: error in variable inquire",rc,-52) .NE. 0) return

! Write variable in the appropriate precision.

      if (HUGE(dummy) .EQ. HUGE(dummy32)) then        ! -r4
        if (type .EQ. NF90_FLOAT) then                     ! 32-bit
          rc = NF90_PUT_VAR(fid, vid, grid, corner, edges)
        else if (type .EQ. NF90_DOUBLE) then               ! 64-bit
          allocate (grid_64(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid_64(i,j,k) = grid(i,j,k)
              enddo
            enddo
          enddo
          rc = NF90_PUT_VAR(fid, vid, grid_64, corner, edges)
          deallocate (grid_64)
        else if (type .EQ. NF90_SHORT) then
          rc = NF90_GET_ATT(fid,vid,'packmax',high_32)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'packmin',low_32)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'scale_factor',scale_32)
          if (err("PutVar: error getting scale",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'add_offset',offset_32)
          if (err("PutVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid(i,j,k) .LT. low_32 .OR. grid(i,j,k) .GT. &
               high_32) then
                  grid_16(i,j,k) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,j,k) = (grid(i,j,k) - offset_32)/scale_32
                endif
              enddo
            enddo
          enddo
          rc = NF90_PUT_VAR(fid, vid, grid_16, corner, edges)
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else if (HUGE(dummy) .EQ. HUGE(dummy64)) then   ! -r8
        if (type .EQ. NF90_FLOAT) then                     ! 32-bit
          allocate (grid_32(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid_32(i,j,k) = grid(i,j,k)
              enddo
            enddo
          enddo
          rc= NF90_PUT_VAR(fid, vid, grid_32, corner, edges)
          deallocate (grid_32)
        else if (type .EQ. NF90_DOUBLE) then                ! 64-bit
          rc = NF90_PUT_VAR(fid, vid, grid, corner, edges)
        else if (type .EQ. NF90_SHORT) then
          rc = NF90_GET_ATT(fid,vid,'packmax',high_32)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'packmin',low_32)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'scale_factor',scale_32)
          if (err("PutVar: error getting scale",rc,-53) .NE. 0) return
          rc = NF90_GET_ATT(fid,vid,'add_offset',offset_32)
          if (err("PutVar: error getting offset",rc,-53) .NE. 0) return
          allocate (grid_16(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid(i,j,k) .LT. low_32 .OR. grid(i,j,k) .GT. &
               high_32) then
                  grid_16(i,j,k) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,j,k) = (grid(i,j,k) - offset_32)/scale_32
                endif
              enddo
            enddo
          enddo
          rc = NF90_PUT_VAR(fid, vid, grid_16, corner, edges)
          deallocate (grid_16)
        else
          rc = -13
          return
        endif
      else
        rc = -12
        return
      endif
      if (err("PutVar: error writing variable",rc,-45) .NE. 0) return

! Read time dimension scale and fill all values up to the current time.
! This will insure missing times are defined with the proper time value.

      rc = NF90_INQUIRE_DIMENSION (fid, timeDimId, dimName, dimSize)
      dimSize = dimSize - 1                           ! We've already written the 
                                                      ! the new time.
      allocate ( allTimes (MAX(timeIndex,dimSize)) )
      allocate ( fminutes_32 (MAX(timeIndex,dimSize)) )

      rc = NF90_INQUIRE_VARIABLE (fid,timeId,dimName,timeType,nvDims,vDims,nvAtts)

      if (dimSize .GT. 0) then
        ! Depending on the version of CFIO used to write the file, the Time
        ! dimension variable can either be floating point or integer.

        corner(1)=1
        edges(1)=dimSize

        if (timeType .EQ. NF90_FLOAT) then
          rc = NF90_GET_VAR(fid, timeId, fminutes_32, corner, edges)
          do i=1,dimSize
            allTimes(i) = INT(fminutes_32(i))
          enddo
        else if (timeType .EQ. NF90_INT) then
          rc = NF90_GET_VAR(fid, timeId, allTimes, corner, edges)
        endif
        if (err("PutVar: error reading times from file",rc,-46) .NE. 0)&
           return
      endif

      ! This loop fills the time dimension scale based on the time increment 
      ! specified in CFIO_Create.  If CFIO ever changes to support variable 
      ! time increments, this code MUST be changed.   

      do i=1,timeIndex-1
        fillTime = (i-1) * incSecs/60
        allTimes(i) = fillTime
      enddo
      allTimes(timeIndex) = minutes

! Write filled time array to file.

      corner(1)=1
      edges(1)=timeIndex

      if (timeType .EQ. NF90_FLOAT) then
        do i=1,timeIndex
          fminutes_32(i) = INT(allTimes(i))
        enddo
        rc = NF90_PUT_VAR(fid,timeId,fminutes_32,corner,edges)
      else if (timeType .EQ. NF90_INT) then
        rc = NF90_PUT_VAR(fid,timeId,allTimes,corner,edges)
      endif
      if (err("PutVar: error writing time",rc,-38) .NE. 0) return

      if (outRange .AND. outPRange) then
        rc = -17
      else if (outPRange) then
        rc = -16
      else if (outRange) then
        rc = -15
      else
        rc = 0
      endif

      deallocate ( allTimes )
      deallocate ( fminutes_32 )

      return
      end subroutine CFIO_PutVar

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOI
!
!  !TITLE: Returns a new date/time from an initial date/time and offset
!
!  !AUTHORS: Rob Lucchesi
!
!  !AFFILIATION: Data Assimilation Office, NASA/GSFC, Greenbelt, MD 20771
!
!  !DATE: July 20, 1998   
!
!EOI
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: GetDate --- Returns a new date/time from an initial date/time 
!                       and offset
!
! !INTERFACE:
!

       subroutine GetDate (yyyymmdd_1,hhmmss_1,offset, &
                          yyyymmdd_2,hhmmss_2,rc)

! 
! !USES:
!

       implicit none

!
! !INPUT PARAMETERS:
!
       
       integer yyyymmdd_1               ! Initial date in YYYYYMMDD format
       integer hhmmss_1                 ! Initial time in HHMMSS format
       integer offset                   ! Offset to add (in seconds)

!
! !OUTPUT PARAMETERS:
!
       integer yyyymmdd_2               ! New date in YYYYMMDD format
       integer hhmmss_2                 ! New time in HHMMSS format
       integer rc                       ! Return code. (<0 = error)
!
! !DESCRIPTION:   This subroutine returns a new date and time in yyyymmdd
!                 and hhmmss format given and initial date, time, and
!                 offset in seconds.  The routine converts the input date
!                 and time to julian seconds, adds the offset, and converts
!                 back to yyyymmdd and hhmmss format.  This routine has been
!                 tested for Y2K compiance.
!
! !REVISION HISTORY:
!
!  1998.07.20  Lucchesi    Initial version.
!  2010.05.11  Lucchesi  Integer for julian seconds changed to 64-bit. StartDate
!                        constant no longer needed.
!
!EOP
!-------------------------------------------------------------------------

      integer year1,mon1,day1,hour1,min1,sec1
      integer year2,mon2,day2,hour2,min2,sec2
      integer seconds1, seconds2
      integer(kind=8) julian1, julian2
      integer(kind=8) julsec, remainder
      character*8 dateString

! Error checking.

      if (yyyymmdd_1 .lt. 19000000 .or. yyyymmdd_1 .gt. 21000000 ) then
         rc=-1
         return
      endif
      if (hhmmss_1 .lt. 0 .or. hhmmss_1 .ge. 240000 ) then
         rc=-1
         return
      endif

! Convert Date/Time strings to integer variables.

!ams       write (dateString, 200) yyyymmdd_1
!ams 200   format (I8)
!ams       read (dateString, 201) year1, mon1, day1
!ams 201   format (I4,2I2)
!ams       write (dateString, 202) hhmmss_1
!ams 202   format (I6)
!ams       read (dateString, 203) hour1, min1, sec1
!ams 203   format (3I2)

      call CFIO_parseIntTime ( yyyymmdd_1, year1, mon1, day1 )
      call CFIO_parseIntTime ( hhmmss_1, hour1, min1, sec1 )

! Get Julian Day and subtract off a constant (Julian days since 7/14/66)
 
      julian1 = julday (mon1, day1, year1)
       
! Calculcate Julian seconds

      julsec = (julian1-1)*86400 + hour1*3600 + min1*60 + sec1

! Add offset and calculate new julian day.

      julsec = julsec + offset
      julian1 = INT(julsec/86400) + 1
      remainder = MOD(julsec,86400)
 
! Convert julian day to YYYYMMDD.

      call caldat (julian1, mon2, day2, year2)

! Calculate HHMMSS from the remainder.

      hour2 = INT(remainder/3600)
      remainder = MOD(remainder,3600)
      min2 = INT(remainder/60)
      sec2 = MOD(remainder,60)

! Build YYYYMMDD and HHMMSS variables.

      yyyymmdd_2 = year2*10000 + mon2*100 + day2
      hhmmss_2 = hour2*10000 + min2*100 + sec2

      rc = 0
      return
      end subroutine GetDate

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CFIO_GetMissing --- Return missing value
!
! !DESCRIPTION: Returns missing value on file
!
! !INTERFACE:

      real function CFIO_GetMissing ( fid, rc )

!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:

      integer fid      ! file id
      integer rc       ! Error code

!
! !REVISION HISTORY:
!
!  1999.11.01  da Silva  Initial code.
!
!EOP
!-------------------------------------------------------------------------

      integer nDims, recdim, ngatts, seconds
      integer varType, nvDims, vDims(MAXVDIMS), nvAtts
      character*8 strBuf
      character*(MAXCHR) dimName
      character*(MAXCHR) dimUnits
      character*(MAXCHR) vnameTemp
      integer dimSize
      integer i
      logical surfaceOnly
      logical noTimeInfo
      integer attType, attLen
      integer allVars            ! all variables - includes dimension vars

      real*4 amiss_32

! Get basic information from the file

      rc = NF90_INQUIRE (fid,nDims,allVars,ngatts,recdim)
      if (err("Inqure: NF90_INQUIRE failed",rc,-48) .NE. 0) return

      if (nDims .EQ. 3) then
        surfaceOnly = .TRUE.
      endif

      do i= 1, allVars
        rc = NF90_INQUIRE_VARIABLE (fid,i,vnameTemp,varType,nvDims,vDims,nvAtts)
        if (err("CFIO_GetMissing: variable inquire error",rc,-52) .NE. 0) return
        if (nvDims .EQ. 1) then   ! coord variable
          cycle
        else                      ! noon-coord variable
          rc = NF90_GET_ATT(fid,i,'fmissing_value',amiss_32)
           if (rc .NE. 0) then
              rc = NF90_INQUIRE_ATTRIBUTE (fid, i, 'missing_value', attType, attLen)
              if (rc.eq.0 .and. attType .EQ. NF90_FLOAT) then
                 rc = NF90_GET_ATT(fid,allVars,'missing_value',amiss_32)
                 if (err("CFIO_GetMissing: error getting missing value",rc,-53) &
                     .NE. 0) return
              else
                    print *,  &
                   'CFIO_GetMissing: Cannot find missing value, assuming 1E+15'
                    amiss_32 = 1.0E+15
              end if
           endif
           exit    ! just check first non-ccordinate variable
        endif
      end do

      CFIO_GetMissing = amiss_32

      rc = 0
      end function CFIO_GetMissing

!
! The next routine adapted from mpeu...
!

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_StrTemplate - A template formatting a string with variables
!
! !DESCRIPTION:
!
!	A template resolver formatting a string with a string variable
!   and time variables.  The format descriptors are similar to those
!   used in the GrADS.
!
!	"%y4"	substitute with a 4 digit year
!	"%y2"	a 2 digit year
!	"%m1"	a 1 or 2 digit month
!	"%m2"	a 2 digit month
!	"%mc"	a 3 letter month in lower cases
!	"%Mc"	a 3 letter month with a leading letter in upper case
!	"%MC"	a 3 letter month in upper cases
!	"%d1"	a 1 or 2 digit day
!	"%d2"	a 2 digit day
!	"%h1"	a 1 or 2 digit hour
!	"%h2"	a 2 digit hour
!	"%h3"	a 3 digit hour (?)
!	"%n2"	a 2 digit minute
!	"%s"	a string variable
!	"%%"	a "%"
!
! !INTERFACE:


! !REVISION HISTORY:
! 	19Dec06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- Merged changes between 1.1.2.6 and 1.1.2.9 to 1.2,
!		  including a fix at bug nymd==0 and environment
!		  variable ($env or ${env}) support if getenv() is
!		  available from the system.
! 	01Jun99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: strTemplate_ - expanding a format template to a string
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine strTemplate_(str,tmpl,class,xid,nymd,nhms,stat)
!      use m_chars, only : uppercase
      implicit none

      character(len=*),intent(out) :: str	! the output

      character(len=*),intent(in ) :: tmpl	! a "format"

      character(len=*),intent(in ),optional :: class
			! choose a UNIX or a GrADS(defulat) type format

      character(len=*),intent(in ),optional :: xid
			! a string substituting a "%s".  Trailing
			! spaces will be ignored

      integer,intent(in ),optional :: nymd
			! yyyymmdd, substituting "%y4", "%y2", "%m1",
			! "%m2", "%mc", "%Mc', and "%MC"

      integer,intent(in ),optional :: nhms
			! hhmmss, substituting "%h1", "%h2", "%h3",
			! and "%n2"

      integer,intent(out),optional :: stat
			! error code

! !REVISION HISTORY:
! 	03Jun99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!       08Jan01 - da Silva: moved uppercase() to outside select() to
!                 avoid coredump on Linux/PGI.
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::strTemplate_'
  character(len=16) :: tmpl_class,uc_class

  tmpl_class="GX"
  if(present(class)) tmpl_class=class
!!!  uc_class=uppercase(tmpl_class)
  uc_class=trim(tmpl_class) ! removed this dependency on mpeu

  select case(uc_class)

  case("GX","GRADS")
    call GX_(str,tmpl,xid,nymd,nhms,stat)

  !case("UX","UNIX")	! yet to be implemented
  !  call UX_(str,tmpl,xid,nymd,nhms,stat)

  case default
    write(stderr,'(4a)') myname_,': unknown class, "',trim(tmpl_class),'"'
    if(.not.present(stat)) call die(myname_)
    stat=-1
    return

  end select

end subroutine strTemplate_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: GX_ - evaluate a GrADS style string template
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine GX_(str,tmpl,xid,nymd,nhms,stat)
!!!      use m_stdio,only : stderr
!!!      use m_die,  only : die,perr
      implicit none
      character(len=*),intent(out) :: str
      character(len=*),intent(in ) :: tmpl
      character(len=*),optional,intent(in) :: xid
      integer,optional,intent(in)  :: nymd
      integer,optional,intent(in)  :: nhms
      integer,optional,intent(out) :: stat

! !REVISION HISTORY:
! 	01Jun99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::GX_'

  integer :: iy4,iy2,imo,idy
  integer :: ihr,imn
  integer :: i,i1,i2,m,k
  integer :: ln_tmpl,ln_str
  integer :: istp,kstp
  integer :: ier

  character(len=1) :: c0,c1,c2
  character(len=4) :: sbuf
!________________________________________
	! Determine iyr, imo, and idy
  iy4=-1
  iy2=-1
  imo=-1
  idy=-1
  if(present(nymd)) then
	if(nymd <= 0) then
	  call perr(myname_,'nymd <= 0',nymd)
	  if(.not.present(stat)) call die(myname_)
	  stat=1
	  return
	endif

    i=nymd
    iy4=i/10000
    iy2=mod(iy4,100)
      i=mod(i,10000)
    imo=i/100
      i=mod(i,100)
    idy=i
  endif
!________________________________________
	! Determine ihr and imn
  ihr=-1
  imn=-1
  if(present(nhms)) then
	if(nhms < 0) then
	  call perr(myname_,'nhms < 0',nhms)
	  if(.not.present(stat)) call die(myname_)
	  stat=1
	  return
	endif

    i=nhms
    ihr=i/10000
      i=mod(i,10000)
    imn=i/100
  endif
!________________________________________

  ln_tmpl=len_trim(tmpl)	! size of the format template
  ln_str =len(str)		! size of the output string
!________________________________________

  if(present(stat)) stat=0

str=""

i=0; istp=1
k=1; kstp=1

do while( i+istp <= ln_tmpl )	! A loop over all tokens in (tmpl)

  if(k>ln_Str) exit	! truncate the output here.

  i=i+istp
  c0=tmpl(i:i)

  select case(c0)
  case ("$")
    call genv_(tmpl,ln_tmpl,i,istp,str,ln_str,k,ier)
    if(ier/=0) then
      call perr(myname_,'genv_("'//tmpl(i:ln_tmpl)//'"',ier)
      if(.not.present(stat)) call die(myname_)
      stat=1
      return
    endif

  case ("%")
	!________________________________________

    c1=""
    i1=i+1
    if(i1 <= ln_Tmpl) c1=tmpl(i1:i1)
	!________________________________________

    select case(c1)

    case("s")
      if(.not.present(xid)) then
	write(stderr,'(2a)') myname_,	&
		': optional argument expected, "xid="'
	if(.not.present(stat)) call die(myname_)
	stat=1
	return
      endif

      istp=2
      m=min(k+len_trim(xid)-1,ln_str)
      str(k:m)=xid
      k=m+1
      cycle

    case("%","$")

      istp=2
      str(k:k)=c1
      k=k+1	! kstp=1
      cycle

    case default

      c2=""
      i2=i+2
      if(i2 <= ln_Tmpl) c2=tmpl(i2:i2)
	!________________________________________

      select case(c1//c2)

      case("y4","y2","m1","m2","mc","Mc","MC","d1","d2")
        if(.not.present(nymd)) then
	  write(stderr,'(2a)') myname_,	&
		': optional argument expected, "nymd="'
	  if(.not.present(stat)) call die(myname_)
	  stat=1
	  return
        endif
        istp=3

      case("h1","h2","h3","n2")
        if(.not.present(nhms)) then
	  write(stderr,'(2a)') myname_,	&
		': optional argument expected, "nhms="'
	  if(.not.present(stat)) call die(myname_)
	  stat=1
	  return
        endif
        istp=3

      case default

        write(stderr,'(4a)') myname_,	&
	  ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2
        return

      end select	  ! case(c1//c2)
    end select		! case(c1)
	!________________________________________

    select case(c1)

    case("y")
      select case(c2)
      case("2")
	write(sbuf,'(i2.2)') iy2
	kstp=2
      case("4")
	write(sbuf,'(i4.4)') iy4
	kstp=4
      case default
	write(stderr,'(4a)') myname_,	&
	  ': invalid template entry, "',trim(tmpl(i:)),'"'
	if(.not.present(stat)) call die(myname_)
	stat=2
	return
      end select

    case("m")
      select case(c2)
      case("1")
	if(imo < 10) then
	  write(sbuf,'(i1)') imo
	  kstp=1
	else
	  write(sbuf,'(i2)') imo
	  kstp=2
	endif
      case("2")
	write(sbuf,'(i2.2)') imo
	kstp=2
      case("c")
	sbuf=mon_lc(imo)
	kstp=3
      case default
	write(stderr,'(4a)') myname_,	&
	  ': invalid template entry, "',trim(tmpl(i:)),'"'
	if(.not.present(stat)) call die(myname_)
	stat=2
	return
      end select

    case("M")
      select case(c2)
      case("c")
	sbuf=mon_wd(imo)
	kstp=3
      case("C")
	sbuf=mon_uc(imo)
	kstp=3
      case default
	write(stderr,'(4a)') myname_,	&
	  ': invalid template entry, "',trim(tmpl(i:)),'"'
	if(.not.present(stat)) call die(myname_)
	stat=2
	return
      end select

    case("d")
      select case(c2)
      case("1")
	if(idy < 10) then
	  write(sbuf,'(i1)') idy
	  kstp=1
	else
	  write(sbuf,'(i2)') idy
	  kstp=2
	endif
      case("2")
	write(sbuf,'(i2.2)') idy
	kstp=2
      case default
	write(stderr,'(4a)') myname_,	&
	  ': invalid template entry, "',trim(tmpl(i:)),'"'
	if(.not.present(stat)) call die(myname_)
	stat=2
	return
      end select

    case("h")
      select case(c2)
      case("1")
	if(ihr < 10) then
	  write(sbuf,'(i1)') ihr
	  kstp=1
	else
	  write(sbuf,'(i2)') ihr
	  kstp=2
	endif
      case("2")
	write(sbuf,'(i2.2)') ihr
	kstp=2
      case("3")
	write(sbuf,'(i3.3)') ihr
	kstp=3
      case default
	write(stderr,'(4a)') myname_,	&
	  ': invalid template entry, "',trim(tmpl(i:)),'"'
	if(.not.present(stat)) call die(myname_)
	stat=2
	return
      end select

    case("n")
      select case(c2)
      case("2")
	write(sbuf,'(i2.2)') imn
	kstp=2
      case default
	write(stderr,'(4a)') myname_,	&
	  ': invalid template entry, "',trim(tmpl(i:)),'"'
	if(.not.present(stat)) call die(myname_)
	stat=2
	return
      end select

    case default
	write(stderr,'(4a)') myname_,	&
	  ': invalid template entry, "',trim(tmpl(i:)),'"'
	if(.not.present(stat)) call die(myname_)
	stat=2
      return
    end select	! case(c1)

    m=min(k+kstp-1,ln_Str)
    str(k:m)=sbuf
    k=m+1

  case default

    istp=1
    str(k:k)=tmpl(i:i)
    k=k+1

  end select	! case(c0)
end do

contains
subroutine genv_(tmpl,lnt,i,istp,str,lns,k,ier)
  implicit none
  character(len=*),intent(in) :: tmpl
  integer,intent(in)  :: lnt
  integer,intent(in)  :: i
  integer,intent(out) :: istp
  character(len=*),intent(inout) :: str
  integer         ,intent(in)    :: lns
  integer         ,intent(inout) :: k
  integer,intent(out) :: ier

  integer :: j,jb,je
  integer :: l,m
  logical :: bracket,more
  character(len=256) :: env

  j=i+1		! skip "$"
  ier=0

  if(j>lnt) then
    ier=1
    return
  endif

  bracket = tmpl(j:j)=='{'
  if(bracket) j=j+1

  	! There is at least one a letter (including "_") to start a
	! variable name

  select case(tmpl(j:j))
  case ("A":"Z","a":"z","_")
  case default
    ier=2
    return
  end select

  jb=j
  je=j

  if(bracket) then

    more=.true.
    do while(more)
      select case(tmpl(j:j))
      case ("A":"Z","a":"z","_","0":"9")
	je=j
        j=j+1
      case ("}")	! End if "}" or eos
        j=j+1
        exit
      case default
        ier=3
        return
      end select
      more=j<=lnt
    enddo

  else

    more=.true.
    do while(more)
      select case(tmpl(j:j))
      case ("A":"Z","a":"z","_","0":"9")
        je=j
	j=j+1
      case default
        exit
      end select
      more=j<=lnt
    enddo
  endif

  istp=j-i

  call getenv(tmpl(jb:je),env)
  l=len_trim(env)
  m=min(k+l-1,lns)
  str(k:m)=env
  k=m+1

end subroutine genv_
end subroutine GX_

!
! MPEU Stubs
!

subroutine perr ( name, msg, i )
character(len=*) :: name, msg
integer :: i
print *, trim(name)//':'//trim(msg), i
return
end subroutine perr

subroutine die ( name )
character(len=*) :: name
print *, trim(name)//': fatal error in CFIO, but not aborting...'
return
end subroutine die

#if defined(HDFEOS) || defined(HDFSD)

      subroutine EOS_Close ( fid, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:

                         !   rc = 0    all is well
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -54  error from ncclos (file close)
! !REVISION HISTORY:
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.03.30  Lucchesi           Documentation expanded.  Clean-up of code.
!                                 Added rc.
!
!EOP
!-------------------------------------------------------------------------

      integer i

      integer sfend
      integer GDclose

! Make NetCDF errors non-fatal, but issue warning messages.

#if defined(HDFSD)
      rc = sfend (fid)
#endif

#if defined(HDFEOS)
      rc = GDclose (fid)
#endif

      rc = 0
      return
      end subroutine EOS_Close


      subroutine EOS_PutIntAtt ( fid, name, count, buf, prec, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of integers to write
      integer        buf(count) ! Buffer with integer values
      integer        prec       ! Desired precision of attribute value:
                                !   0 = 32 bit
                                !   1 = 64 bit
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !   rc = -12  error determining default precision
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from NF90_PUT_ATT (global attribute)
                         !   rc = -55  error from NF90_REDEF (enter define mode)
                         !   rc = -56  error from NF90_ENDDEF (exit define mode)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!  1998.09.28  Lucchesi           Added support for multiple precisions
!  1999.01.29  Lucchesi           Converted API to SD for HDFEOS
!
!EOP
!-------------------------------------------------------------------------

      integer*4 dummy32
      integer*8 dummy64
      integer i

      integer*4, allocatable :: buf32(:)
      integer*8, allocatable :: buf64(:)
      integer rct

#if defined(HDFEOS)
      ! Variables for HDF-EOS

      integer hdfFid, hdfeos_fid
      integer gridId

      ! Functions for HDF-EOS

      integer GDattach, EHidinfo, GDdetach
#endif

      ! Functions for SD interface
 
      integer sfsnatt

      rc = 0

#if defined (HDFEOS)
      ! Attach EOS-HDF grid and obtain SD itentifier.  Place this
      ! in 'fid' after saving the HDF-EOS fid in 'hdfeos_fid.'

      hdfeos_fid = fid
      gridId = GDattach (hdfeos_fid, GRID_NAME)
      rct = EHidinfo (hdfeos_fid, hdfFid, fid)
#endif

      if ( HUGE(dummy32) .EQ. HUGE(i) .AND. prec .EQ. 0 ) then     ! -i4
        rct = sfsnatt( fid, name, DFNT_INT32, count, buf)

      else if ( HUGE(dummy32) .EQ. HUGE(i) .AND. prec .EQ. 1 ) then  ! -i4
        allocate ( buf64(count) )                                    ! 64-bit out
        do i=1,count
          buf64(i) = buf(i)
        enddo
        rct = sfsnatt( fid, name, DFNT_FLOAT64, count, buf)
        deallocate (buf64)

      else if  (HUGE(dummy64) .EQ. HUGE(i) .AND. prec .EQ. 0 ) then  ! -i8
        allocate ( buf32(count) )                                    ! 32-bit out
        do i=1,count
          buf32(i) = buf(i)
        enddo
        rct = sfsnatt( fid, name, DFNT_INT32, count, buf)
        deallocate (buf32)

      else if (HUGE(dummy64) .EQ. HUGE(i) .AND. prec .EQ. 1 ) then   ! -i8
        rct = sfsnatt( fid, name, DFNT_FLOAT64, count, buf)

      else 
        rc = -12
        goto 999
      endif
      if (err("PutIntAtt: error writing attribute",rc,-36) .NE. 0) return

999   continue    ! Return processing.  I hate GOTOs, but we need to detach
                  ! the grid if in HDF-EOS mode before returning and this seemed
                  ! like the best way without having messy if-defs all through
                  ! the code.

#if defined (HDFEOS)
      fid = hdfeos_fid
      rct = GDdetach (gridId)
#endif

      return
      end subroutine EOS_PutIntAtt


      subroutine EOS_PutRealAtt ( fid, name, count, buf, prec, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of integers to write
      real           buf(count) ! Buffer with real values
      integer        prec       ! Desired precision of attribute value:
                                !   0 = 32 bit
                                !   1 = 64 bit
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !   rc = -12  error determining default precision
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from NF90_PUT_ATT (global attribute)
                         !   rc = -55  error from NF90_REDEF (enter define mode)
                         !   rc = -56  error from NF90_ENDDEF (exit define mode)

! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!  1998.09.28  Lucchesi           Added support for multiple precisions
!  1999.01.29  Lucchesi           Converted API to SD for HDFEOS
!
!EOP
!-------------------------------------------------------------------------

      real*4 dummy32
      real*8 dummy64
      real r
      integer i
      real*4, allocatable :: buf32(:)
      real*8, allocatable :: buf64(:)
      integer rct

#if defined(HDFEOS)
      ! Variables for HDF-EOS

      integer hdfFid, hdfeos_fid
      integer gridId

      ! Functions for HDF-EOS

      integer GDattach, EHidinfo, GDdetach
#endif

      ! Functions for SD interface

      integer sfsnatt

      rc = 0

#if defined (HDFEOS)
      ! Attach EOS-HDF grid and obtain SD itentifier.  Place this
      ! in 'fid' after saving the HDF-EOS fid in 'hdfeos_fid.'

      hdfeos_fid = fid
      gridId = GDattach (hdfeos_fid,GRID_NAME)
      rct = EHidinfo (hdfeos_fid, hdfFid, fid)
#endif


      if (HUGE(dummy32) .EQ. HUGE(r) .AND. prec .EQ. 0) then        ! -r4
        rct = sfsnatt( fid, name, DFNT_FLOAT32, count, buf)
      else if (HUGE(dummy32) .EQ. HUGE(r) .AND. prec .EQ. 1) then  ! -r4
        allocate (buf64(count))                                    ! 64-bit out
        do i=1,count
          buf64(i) = buf(i)
        enddo
        rct = sfsnatt( fid, name, DFNT_FLOAT64, count, buf64)
        deallocate (buf64)
      else if (HUGE(dummy64) .EQ. huge(r) .AND. prec .EQ. 0) then  ! -r8
        allocate (buf32(count))                                    ! 32-bit out
        do i=1,count
          buf32(i) = buf(i)
        enddo
        rct = sfsnatt( fid, name, DFNT_FLOAT32, count, buf32)
        deallocate (buf32)
      else if (HUGE(dummy64) .EQ. huge(r) .AND. prec .EQ. 1) then    ! -r8
        rct = sfsnatt( fid, name, DFNT_FLOAT64, count, buf)
      else
        rc = -12
        goto 999
      endif

      if (err("PutRealAtt: error writing attribute",rc,-36) .NE. 0) &
         goto 999


999   continue    ! Return processing.  I hate GOTOs, but we need to detach
                  ! the grid if in HDF-EOS mode before returning and this seemed
                  ! like the best way without having messy if-defs all through
                  ! the code.

#if defined (HDFEOS)
      fid = hdfeos_fid
      rct = GDdetach (gridId)
#endif

      return
      end subroutine EOS_PutRealAtt



      subroutine EOS_PutCharAtt ( fid, name, count, buf, rc )
!
! !USES:
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid        ! File handle
      character*(*)  name       ! Name of attribute
      integer        count      ! Number of characters to write
      character(len=MLEN)      buf ! Buffer containing string
!
! !OUTPUT PARAMETERS:
!
      integer     rc     ! Error return code:
                         !   rc = 0    all is well
                         !
                         !  NetCDF Errors
                         !  -------------
                         !   rc = -36  error from NF90_PUT_ATT (global attribute)
                         !   rc = -55  error from NF90_REDEF (enter define mode)
                         !   rc = -56  error from NF90_ENDDEF (exit define mode)
! !REVISION HISTORY:
!
!  1998.07.30  Lucchesi           Initial interface design.
!  1998.07.30  Lucchesi           Initial coding.
!  1998.09.24  Lucchesi           Changed error handling.
!  1999.01.29  Lucchesi           Converted API to SD for HDFEOS
!
!EOP
!-------------------------------------------------------------------------

      integer rct

#if defined(HDFEOS)
      ! Variables for HDF-EOS
      integer hdfFid, hdfeos_fid
      integer gridId

      ! Functions for HDF-EOS
      integer GDattach, EHidinfo, GDdetach
#endif

      ! Functions for SD interface
      integer sfscatt
 
      rc = 0

#if defined (HDFEOS)
      ! Attach EOS-HDF grid and obtain SD itentifier.  Place this
      ! in 'fid' after saving the HDF-EOS fid in 'hdfeos_fid.'

      hdfeos_fid = fid
      gridId = GDattach (hdfeos_fid, GRID_NAME)
      rct = EHidinfo (hdfeos_fid, hdfFid, fid)
#endif

      rct = sfscatt (fid, name, DFNT_CHAR8, count, buf)

999   continue    ! Return processing.  I hate GOTOs, but we need to detach
                  ! the grid if in HDF-EOS mode before returning and this seemed
                  ! like the best way without having messy if-defs all through
                  ! the code.

#if defined (HDFEOS)
      fid = hdfeos_fid
      rct = GDdetach (gridId)
#endif

      return
      end subroutine EOS_PutCharAtt

      subroutine EOS_PutVar ( fid, vname, yyyymmdd, hhmmss, &
                              im, jm, kbeg, kount, grid,     &
                              do_comp, do_chunk, rc )  
!
! !USES:

      Implicit NONE  
!#include "hlimits.h"
!
! !INPUT PARAMETERS: 
!
      integer        fid                 ! File handle
      character*(*)  vname               ! Variable name
      integer        yyyymmdd            ! Year-month-day, e.g., 19971003
      integer        hhmmss              ! Hour-minute-second, e.g., 120000
 
      integer         im                 ! size of longitudinal dimension
      integer         jm                 ! size of latitudinal  dimension
      integer         kbeg               ! first level to write; if 2-D grid
                                         !   use kbeg = 0.
      integer         kount              ! number of levels to write
      real            grid(im,jm,kount)  ! Gridded data to write at this time
      logical         do_comp
      logical         do_chunk
      integer         comp_num           ! 1 -- COMP_CODE_RLE; 2 -- COMP_CODE_NBIT
                                         ! 3 --COMP_CODE_SKPHUFF; 4 -- COMP_CODE_DEFLATE
                                         ! 5 --COMP_CODE_SZIP
                                     

! !OUTPUT PARAMETERS:
 
      integer        rc  ! Error return code:
                         !  rc =  0  all is well
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
                         !
                         !  NetCDF Errors
                         !  -------------
                         !  rc = -32  error detaching from grid
                         !  rc = -37  error attaching to grid (HDFEOS)
                         !  rc = -38  error from NF90_PUT_VAR (dimension variable) NOTUSED
                         !  rc = -40  variable not defined
                         !  rc = -41  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lat or lon) NOTUSED
                         !  rc = -42  error from NF90_INQ_DIMID or NF90_INQUIRE_DIMENSION (lev) NOTUSED
                         !  rc = -43  error from NF90_INQ_VARID (time variable)
                         !  rc = -44  error reading time information
                         !  rc = -45  error writing data
                         !  rc = -52  error from NF90_INQUIRE_VARIABLE NOTUSED
                         !  rc = -53  error getting variable attributes

! !REVISION HISTORY: 
!
!  1997.10.13 da Silva/Lucchesi   Initial interface design.
!  1998.02.10 Lucchesi            Added support for applications running with
!                                 64-bit reals.
!  1998.03.30 Lucchesi            Documentation expanded.  Clean-up of code.
!  1998.07.02 Lucchesi            Replaced vid with vname in argument list &
!                                 made related mods to code.
!  1998.09.24 Lucchesi            Changed error codes, removed DIM_CHECK if-def
!  1998.10.27 Lucchesi            Added support for packing and range checks
!  1999.01.29 Lucchesi            Converted API to SD for HDFEOS
!  1999.05.27 Lucchesi            Updated error codes.
!
!EOP
!-------------------------------------------------------------------------

      integer timeid, dimSize, dimId
      character*(MAXCHR) dimName
      character*(MAXCHR) attrName
      character*(MAXCHR) dimUnits
      integer attrType, attrCount
      integer corner(4), edges(4), stride(4)
      integer dim_chunk(4), origin(4)
      integer vid
      integer seconds, timeIndex
      integer minutes                       ! added as a work-around
      integer idx, i, j, k
      integer begDate, begTime, timInc
      character*8 strBuf
      integer hour,min,sec,incSecs
      integer rct

! Variables for dealing with precision

      real*4, allocatable :: grid_32(:,:,:)
      real*8, allocatable :: grid_64(:,:,:)
      real*4 dummy32
      real*8 dummy64
      real   dummy

! Variables for NF90_INQUIRE_VARIABLE

      character*(MAXCHR) varName
      integer type, nvDims, dimSizes(MAX_VAR_DIMS), nvAtts

! Variables for packing and range checking

      integer*2, allocatable :: grid_16(:,:,:)
      real*4 high_32, low_32, amiss_32
      real*4 scale_32, offset_32
      logical outRange, outPRange

! Variables for SD interface

      integer sdsId, sdsIndex, attrIdx, dsdsId
      integer dataType, numAttr
      real*8, allocatable :: timeScale(:)
      real*8, allocatable :: eosTimeScale(:)
      integer numTimes, fillTime

! Functions for SD interface

      integer sfgainfo
      integer sfgdinfo, sffattr, sfrnatt, sfginfo, sfwdata, sfdimid
      integer sfgdscale
      integer sfsdscale
      integer sfn2index, sfselect
      integer sfrcatt
      integer sfscompress

! Variables for HDF-EOS

      integer hdfFid, hdfeos_fid
      integer gridId
      integer eosTimeId

      integer EHidinfo
      integer GDattach
      integer GDdetach
      integer GDwrfld

      integer comp_type,  comp_arg(2)
      integer   COMP_CODE_SZIP, SZ_NN_OPTION_MASK
      parameter (COMP_CODE_SZIP=5, SZ_NN_OPTION_MASK=32)
      integer   COMP_CODE_DEFLATE
      parameter (COMP_CODE_DEFLATE = 4)
      integer   COMP_CODE_RLE, COMP_CODE_NBIT, COMP_CODE_SKPHUFF
      parameter (COMP_CODE_RLE=1, COMP_CODE_NBIT=2, COMP_CODE_SKPHUFF=3)
      integer   DEFLATE_LEVEL
      parameter (DEFLATE_LEVEL = 6)
      integer pixels_per_block
      parameter (pixels_per_block = 12)
      integer flag, maxcache
      integer sfwchnk, sfscchnk, sfschnk

! Internal CFIO functions
 

      rc = 0
      outRange = .FALSE.
      outPRange = .FALSE.

! Check to make sure max string lengths are large enough.  NetCDF defines
! MAXNCNAM, but it can't be used in a character*MAXNCNAM statement.

      if (MAXCHR .LT. MAXNCNAM) then
        print *, 'EOS_PutVar warning: MAXNCNAM is larger than ', &
                'dimName array size.'
      endif

#if defined (HDFEOS)
      ! Attach EOS-HDF grid and obtain SD itentifier.  Place this
      ! in 'fid' after saving the HDF-EOS fid in 'hdfeos_fid.'

      hdfeos_fid = fid
      gridId = GDattach (hdfeos_fid, GRID_NAME)
      if (err("PutVar: error in GDattach",rc,-37) .NE. 0)  &
         goto 999
      rct = EHidinfo (hdfeos_fid, hdfFid, fid)
      if (err("PutVar: error in EHidinfo",rc,-37) .NE. 0) goto 999
#endif

! Get SDS id.
        
      sdsIndex = sfn2index (fid, vname) 
      if (err("PutVar: can't find variable",rc,-40).NE.0) then 
        print *, 'Error details: no index for ',TRIM(vname)
        goto 999
      endif
      sdsId = sfselect (fid, sdsIndex)
      if (err("PutVar: can't find variable",rc,-40).NE.0) then
        print *, 'Error details: no id for ',TRIM(vname)
        goto 999
      endif
      
! Dimension error checking

      do i=0,NDIMS_MAX-1

        ! get the dimension ID and infomation about the dimension

        dimid = sfdimid (sdsId, i)
        rct = sfgdinfo (dimid, dimName, dimSize, type, numAttr)
         
        if (rct .NE. 0) then                   ! No dimensions left.
          exit
        endif

        ! look for a "units" attribute.  if none, use a dummy string for units.

        dimUnits = REPEAT(' ',MAXCHR)  ! zero out dimUnits: sfrcatt won't pad 
                                       ! with blanks.
        attrIdx = sffattr (dimid, 'units')
        if (attrIdx .GE. 0) then
          rct = sfgainfo (dimid, attrIdx, attrName, attrType, attrCount)
          if (attrCount .LE. MAXCHR) then
            rct = sfrcatt (dimid, attrIdx, dimUnits)
          endif
        else
          dimUnits='dummy'
        endif

        ! try to identify the dimension based on the name and the units
        ! then do appropriate error checking

        idx = IdentifyDim (dimName, dimUnits)
        if (idx .EQ. 0) then
          if (dimSize .ne. im) then
            rc = -4
            goto 999
          endif
        else if (idx .EQ. 1) then
          if (dimSize .ne. jm) then
            rc = -5
            goto 999
          endif
        else if (idx .EQ. 2) then
          if (kbeg-1 + kount .gt. dimSize) then
            rc = -3
            goto 999
          endif
        else if (idx .EQ. 3) then

          ! for the time dimension, extract the CFIO-specific time information
          ! stored as attributes of the time coordinate variable

          timeid = dimid
          attrIdx = sffattr (timeid, 'begin_date')
          rct = sfrnatt (timeid, attrIdx, begDate)
          if (err("PutVar: error getting begin_date",rc,-44) &
                  .NE. 0) goto 999
          attrIdx = sffattr (timeid, 'begin_time')
          rct = sfrnatt (timeid, attrIdx, begTime)
          if (err("PutVar: error getting begin_time",rc,-44) &
                  .NE. 0) goto 999
          seconds = DiffDate (begDate, begTime, yyyymmdd, hhmmss)
          if (seconds .lt. 0) then
            print *, 'EOS_PutVar: Error code from diffdate.  Problem ', &
                    'with date/time.'
            rc = -7
            goto 999
          endif
          if ( MOD (seconds,60) .eq. 0 ) then
            minutes = seconds / 60
          else
            print *, 'EOS_PutVar: Currently, times must fall on ', &
                    'minute boundaries.'
            rc = -6
            goto 999
          endif
          attrIdx = sffattr (timeid, 'time_increment')
          rct = sfrnatt (timeid, attrIdx, timInc)
          if (err("PutVar: error getting time_increment",rc,-44) &
             .NE.0) goto 999
 
          ! Convert time increment to seconds.

          write (strBuf,203) timinc
203       format (I6)
          read (strBuf,204) hour, min, sec
204       format (3I2)
          incSecs = hour*3600 + min*60 + sec

          if ( MOD (seconds, incSecs) .ne. 0 ) then
            print *, 'CFIO_putvar: Absolute time of ',seconds,' not ', &
                     'possible with an interval of ',timInc
            rc = -2
            goto 999
          else
            timeIndex = seconds/incSecs + 1
          endif
        else
          print *, 'EOS_Putvar: WARNING. Dimension ',TRIM(dimName), &
                  ' is unknown.'
        endif
      enddo
        
! Load starting indicies.

      if ( kbeg .eq. 0 ) then
        corner(1)=0
        corner(2)=0
        corner(3)=timeIndex-1
        stride(1)=1
        stride(2)=1
        stride(3)=1
        edges(1)=im
        edges(2)=jm
        edges(3)=1
        dim_chunk(1) = im
        dim_chunk(2) = jm
        dim_chunk(3) = 1 
      else
        corner(1)=0
        corner(2)=0
        corner(3)=kbeg-1
        corner(4)=timeIndex-1
        stride(1)=1
        stride(2)=1
        stride(3)=1
        stride(4)=1
        edges(1)=im
        edges(2)=jm
        edges(3)=kount
        edges(4)=1
        dim_chunk(1) = im
        dim_chunk(2) = jm
        dim_chunk(3) = 1
        dim_chunk(4) = 1
      endif

! Check variable against valid range.

      attrIdx = sffattr (sdsId, 'vmin')
      rct = sfrnatt (sdsId, attrIdx, low_32)
      if (err("PutVar: error getting vmin",rc,-53) .NE. 0) goto 999
      attrIdx = sffattr (sdsId, 'vmax')
      rct = sfrnatt (sdsId, attrIdx, high_32)
      if (err("PutVar: error getting vmax",rc,-53) .NE. 0) goto 999
      attrIdx = sffattr (sdsId, 'fmissing_value')
      rct = sfrnatt (sdsId, attrIdx, amiss_32)
      if (err("PutVar: error getting FILL",rc,-53) .NE. 0) goto 999

      if (abs(low_32) .NE. amiss_32 .OR. high_32 .NE. amiss_32) then
        do k=1,kount
          do j=1,jm
            do i=1,im
              if (grid(i,j,k) .GT. high_32 .OR. grid(i,j,k) .LT.  &
             low_32) then
                outRange = .TRUE.
                goto 100
              endif
            enddo
          enddo
        enddo
100     continue
      endif
      
! Determine if we are writing single- or double-precision from the "type"
! flag.  Also get the size of the unlimited dimension (time) for later use.

      rct = sfginfo (sdsId, varName, nvDims, dimSizes, type, nvAtts)
      if (err("PutVar: can't get # of times",rc,-44) .NE. 0) &
         goto 999
      numTimes = dimSizes(nvDims)
    
! DO szip compression
      comp_num = 4
      if (comp_num .eq. 5) then
         comp_type   = COMP_CODE_SZIP
         comp_arg(1) = SZ_NN_OPTION_MASK
         comp_arg(2) = pixels_per_block
      end if
      if (comp_num .eq. 4) then
         comp_type   = COMP_CODE_DEFLATE
         comp_arg(1) = DEFLATE_LEVEL
      end if
      if ( do_comp ) then
         if ( do_chunk ) then
            if ( timeIndex .eq. 1 ) then
               rct = sfschnk(sdsId, dim_chunk, comp_type, comp_arg)
               if( rct .ne. 0 ) then
                 print *, 'sfschnk failed for chunk'
               endif
            end if
!            flag = 0
!            maxcache = 2
!            rct = sfscchnk(sdsId, maxcache, flag)
!            if( rct .ne. 0 ) then
!              print *, 'sfscchnk failed for chunk'
!            endif
         else   
            rct = sfscompress(sdsId, comp_type, comp_arg)
            if( rct .ne. 0 ) then
               print *, 'sfscompress failed '
            endif
         end if
      end if

! Write variable in the appropriate precision.

      if (HUGE(dummy) .EQ. HUGE(dummy32)) then        ! -r4
        if (type .EQ. DFNT_FLOAT32) then                     ! 32-bit
          if ( do_chunk ) then
             if ( kbeg .eq. 0 ) then
               origin(1) = 1
               origin(2) = 1
               origin(3) = timeIndex
               rct = sfwchnk(sdsId, origin, grid(:,:,1))
             else   
                do k=1,kount
                  origin(1) = 1
                  origin(2) = 1
                  origin(3) = k
                  origin(4) = timeIndex
                  rct = sfwchnk(sdsId, origin, grid(:,:,k))
                end do
             end if
             if( rct .ne. 0 ) then
               print *, 'sfwchnk failed for chunk'
             endif
          else   
             rct = sfwdata (sdsId, corner, stride, edges, grid)
          end if
          if( rct .ne. 0 ) then
             print *, "return code from sfwdata in CFIO: ", rct
          end if
        else if (type .EQ. DFNT_FLOAT64) then               ! 64-bit
          allocate (grid_64(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid_64(i,j,k) = grid(i,j,k)
              enddo
            enddo
          enddo
          if ( do_chunk ) then
             if ( kbeg .eq. 0 ) then
               origin(1) = 1
               origin(2) = 1
               origin(3) = timeIndex
               rct = sfwchnk(sdsId, origin, grid_64(:,:,1))
             else  
             do k=1,kount
               origin(1) = 1
               origin(2) = 1
               origin(3) = k
               origin(4) = timeIndex
               rct = sfwchnk(sdsId, origin, grid_64(:,:,k))
             end do
             end if
             if( rct .ne. 0 ) then
               print *, 'sfwchnk failed for chunk'
             endif
          else
             rct = sfwdata (sdsId, corner, stride, edges, grid_64)
          end if
          if( rct .ne. 0 ) then
             print *, "return code from sfwdata in CFIO: ", rct
          end if
          deallocate (grid_64)
        else if (type .EQ. DFNT_INT16) then
          attrIdx = sffattr (sdsId, 'packmax')
          rct = sfrnatt (sdsId, attrIdx, high_32)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) &
           goto 999
          attrIdx = sffattr (sdsId, 'packmin')
          rct = sfrnatt (sdsId, attrIdx, low_32)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) &
           goto 999
          attrIdx = sffattr (sdsId, 'scale_factor')
          rct = sfrnatt (sdsId, attrIdx, scale_32)
          if (err("PutVar: error getting scale_factor",rc,-53) &
             .NE. 0) goto 999
          attrIdx = sffattr (sdsId, 'add_offset')
          rct = sfrnatt (sdsId, attrIdx, offset_32)
          if (err("PutVar: error getting add_offset",rc,-53) &
             .NE. 0) goto 999

          allocate (grid_16(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid(i,j,k) .LT. low_32 .OR. grid(i,j,k) .GT. &
               high_32) then
                  grid_16(i,j,k) = PACK_FILL
                  outPRange = .TRUE.
                else
                  grid_16(i,j,k) = (grid(i,j,k) - offset_32)/scale_32
                endif
              enddo
            enddo
          enddo
          if ( do_chunk ) then
             if ( kbeg .eq. 0 ) then
               origin(1) = 1
               origin(2) = 1
               origin(3) = timeIndex
               rct = sfwchnk(sdsId, origin, grid_16(:,:,1))
             else  
             do k=1,kount
               origin(1) = 1
               origin(2) = 1
               origin(3) = k
               origin(4) = timeIndex
               rct = sfwchnk(sdsId, origin, grid_16(:,:,k))
             end do
             end if
             if( rct .ne. 0 ) then
               print *, 'sfwchnk failed for chunk'
             endif
          else
             rct = sfwdata (sdsId, corner, stride, edges, grid_16)
          end if

          deallocate (grid_16)
        else
          rc = -13
          goto 999
        endif
      else if (HUGE(dummy) .EQ. HUGE(dummy64)) then   ! -r8
        if (type .EQ. DFNT_FLOAT32) then                     ! 32-bit
          allocate (grid_32(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                grid_32(i,j,k) = grid(i,j,k)
              enddo
            enddo
          enddo
          if ( do_chunk ) then
             if ( kbeg .eq. 0 ) then
               origin(1) = 1
               origin(2) = 1
               origin(3) = timeIndex
               rct = sfwchnk(sdsId, origin, grid_32(:,:,1))
             else  
             do k=1,kount
               origin(1) = 1
               origin(2) = 1
               origin(3) = k
               origin(4) = timeIndex
               rct = sfwchnk(sdsId, origin, grid_32(:,:,k))
             end do
             end if
             if( rct .ne. 0 ) then
               print *, 'sfwchnk failed for chunk'
             endif
          else
             rct = sfwdata (sdsId, corner, stride, edges, grid_32)
          end if

          if( rct .ne. 0 ) then
             print *, "return code from sfwdata in CFIO: ", rct
          end if
          deallocate (grid_32)
        else if (type .EQ. DFNT_FLOAT64) then                ! 64-bit
          if ( do_chunk ) then
             if ( kbeg .eq. 0 ) then
               origin(1) = 1
               origin(2) = 1
               origin(3) = timeIndex
               rct = sfwchnk(sdsId, origin, grid(:,:,1))
             else   
             do k=1,kount
               origin(1) = 1
               origin(2) = 1
               origin(3) = k
               origin(4) = timeIndex
               rct = sfwchnk(sdsId, origin, grid(:,:,k))
             end do
             end if
             if( rct .ne. 0 ) then
               print *, 'sfwchnk failed for chunk'
             endif
          else
             rct = sfwdata (sdsId, corner, stride, edges, grid)
          end if

          if( rct .ne. 0 ) then
             print *, "return code from sfwdata in CFIO: ", rct
          end if
        else if (type .EQ. DFNT_INT16) then
          attrIdx = sffattr (sdsId, 'packmax')
          rct = sfrnatt (sdsId, attrIdx, high_32)
          if (err("PutVar: error getting packmax",rc,-53) .NE. 0) &
           goto 999
          attrIdx = sffattr (sdsId, 'packmin')
          rct = sfrnatt (sdsId, attrIdx, low_32)
          if (err("PutVar: error getting packmin",rc,-53) .NE. 0) &
           goto 999
          attrIdx = sffattr (sdsId, 'scale_factor')
          rct = sfrnatt (sdsId, attrIdx, scale_32)
          if (err("PutVar: error getting scale_factor",rc,-53) &
             .NE. 0) goto 999
          attrIdx = sffattr (sdsId, 'add_offset')
          rct = sfrnatt (sdsId, attrIdx, offset_32)
          if (err("PutVar: error getting add_offset",rc,-53) &
             .NE. 0) goto 999

          allocate (grid_16(im,jm,kount))
          do k=1,kount
            do j=1,jm
              do i=1,im
                if ( grid(i,j,k) .LT. low_32 .OR. grid(i,j,k) .GT. &
               high_32) then
                  grid_16(i,j,k) = PACK_FILL
                else
                  grid_16(i,j,k) = (grid(i,j,k) - offset_32)/scale_32
                endif
              enddo
            enddo
          enddo
          if ( do_chunk ) then
             if ( kbeg .eq. 0 ) then
               origin(1) = 1
               origin(2) = 1
               origin(3) = timeIndex
               rct = sfwchnk(sdsId, origin, grid_16(:,:,1))
             else
             do k=1,kount
               origin(1) = 1
               origin(2) = 1
               origin(3) = k
               origin(4) = timeIndex
               rct = sfwchnk(sdsId, origin, grid_16(:,:,k))
             end do
             end if
             if( rct .ne. 0 ) then
               print *, 'sfwchnk failed for chunk'
             endif
          else
             rct = sfwdata (sdsId, corner, stride, edges, grid_16)
          end if

          deallocate (grid_16)
        else
          rc = -13
          goto 999
        endif
      else
        rc = -12
        goto 999
      endif
      if (err("PutVar: error writing variable",rc,-45) .NE. 0) &
         goto 999

! Write time to file.

      if (numTimes .GT. timeIndex) then
        allocate ( timeScale(numTimes) )
        allocate ( eosTimeScale(numTimes) )
      else
        allocate ( timeScale(timeIndex) )
        allocate ( eosTimeScale(timeIndex) )
      endif
      
      rct = sfgdscale (timeid, timeScale)
      if (err("PutVar: can't read times",rc,-44) .NE. 0) goto 999
      do i=1,timeIndex-1
        fillTime = (i-1) * incSecs/60
        if ( timeScale(i) .NE. fillTime ) then
           timeScale(i) = fillTime
        endif
        eosTimeScale(i) = timeScale(i)
      enddo
      timeScale(timeIndex) = minutes
      eosTimeScale(timeIndex) = minutes
      rct = sfsdscale (timeid, timeIndex, DFNT_FLOAT64, timeScale)
      if (err("PutVar: can't write times",rc,-44) .NE. 0) goto 999

#if defined (HDFEOS)
      ! This code will write out placeholders for the EOS time.  EOS
      ! requires seconds from a date in 1993 and also requires leap seconds.
      ! A post-processing job will use the EOS Toolkit to add this information.
      
      sdsIndex = sfn2index (fid, "Time")
      eosTimeId = sfselect (fid, sdsIndex)
      if (err("PutVar: can't find EOS time coordinate", &
         rc,-44) .NE. 0) goto 999

      corner(1)=0
      stride(1)=1
      edges(1)=timeIndex
      rct = sfwdata (eosTimeId, corner, stride, edges, eosTimeScale)
      if (err("PutVar: can't write EOS times",rc,-44) .NE. 0) &
         goto 999
#endif

      deallocate (timeScale)
      deallocate (eosTimeScale)

      if (outRange .AND. outRange) then
        rc = -17
      else if (outPRange) then
        rc = -16
      else if (outRange) then
        rc = -15
      else
        rc = 0
      endif

999   continue

#if defined (HDFEOS)
      fid = hdfeos_fid
      rct = GDdetach (gridId)
#endif

      return
      end subroutine EOS_PutVar
#if defined (HDFEOS)

       INTEGER FUNCTION GetSDSid (fid, varName)
       IMPLICIT NONE
       integer       fid
       character*(*) varName

       integer sdid, rc, idx
       integer HDFfid, sd_id
       integer EHidinfo, sfn2index, sfselect

       rc = EHidinfo (fid, HDFfid, sd_id)

       idx = sfn2index (sd_id, varName)

       GetSDSid = sfselect (sd_id, idx)

       return
       end FUNCTION GetSDSid

#endif

#endif

      end module ESMF_CFIOUtilMod

