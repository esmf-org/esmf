!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!!-------------------------------------------------------------------------------------

program ESMF_RegridWeightGenApp

! !USES:
  use ESMF
  use ESMF_IOScripMod
  use ESMF_IOGridspecMod
  use ESMF_IOFileTypeCheckMod
  use ESMF_RegridWeightGenMod
  use ESMF_RegridWeightGenCheckMod

  implicit none

#ifndef ESMF_MPIUNI
  include "mpif.h"
#endif

  integer, parameter :: MAXNAMELEN = 64

  integer            :: rc
  type(ESMF_VM)      :: vm
  integer            :: PetNo, PetCnt
  character(ESMF_MAXPATHLEN) :: srcfile, dstfile, wgtfile
  character(ESMF_MAXPATHLEN) :: srcmeshname, dstmeshname
  character(ESMF_MAXPATHLEN) :: cwd
  character(ESMF_MAXPATHLEN) :: tilePath
  character(len=40)  :: method, flag, lineTypeStr
  character(len=MAXNAMELEN)  :: extrapMethodStr
  character(len=MAXNAMELEN)  :: extrapNumSrcPntsStr
  integer :: extrap_num_src_pnts
  character(len=MAXNAMELEN)  :: extrapDistExponentStr
  real :: extrap_dist_exponent
  type(ESMF_LineType_Flag) :: lineType
  type(ESMF_PoleMethod_Flag) :: pole
   integer            :: poleptrs
  type(ESMF_FileFormat_Flag) :: srcFileType, dstFileType
  type(ESMF_RegridMethod_Flag) :: methodflag
  type(ESMF_ExtrapMethod_Flag) :: extrapMethodFlag
  character(len=ESMF_MAXPATHLEN) :: commandbuf1(4)
  character(len=MAXNAMELEN)  :: commandbuf3(9)
  integer            :: commandbuf2(23)
  integer            :: ind, pos
  logical            :: largeFileFlag
  logical            :: netcdf4FileFlag
  logical            :: weightOnlyFlag
  logical              :: ignoreUnmapped, userAreaFlag, ignoreDegenerate
  type(ESMF_UnmappedAction_Flag) :: unmappedaction
  logical            :: srcMissingValue, dstMissingValue
  logical            :: srcIsRegional, dstIsRegional, typeSetFlag
  logical            :: useSrcCoordVar, useDstCoordVar
  character(len=MAXNAMELEN) :: srcvarname, dstvarname
  character(len=MAXNAMELEN) :: srcCoordNames(2), dstCoordNames(2)
  character(len=256) :: argStr
  logical            :: terminateProg
  !real(ESMF_KIND_R8) :: starttime, endtime
  logical            :: checkFlag, moabFlag
   type(ESMF_LogKind_Flag) :: msgbuf(1)
  type(ESMF_LogKind_Flag) :: logflag
  character(len=ESMF_MAXPATHLEN)  :: argvalue
  integer            :: count, i, length
  type(ESMF_NormType_Flag) :: normType
  logical            :: useSrcCorner, useDstCorner
  logical            :: useTilePathFlag
  integer            :: meshdim
  
  terminateProg = .false.
  
  ! Check if --no_log is given, if so, call ESMF_Initialize() with ESMF_LOGKIND_NONE flag
#ifndef ESMF_MPIUNI
  call MPI_Init(rc)
  if (rc /= MPI_SUCCESS) then
      write(*,*) "ERROR: ESMF_RegridWeightGen initialization error."
      stop 1
  endif
  call MPI_Comm_rank(MPI_COMM_WORLD, PetNo, rc) 
  if (rc /= MPI_SUCCESS) then
      write(*,*) "ERROR: ESMF_RegridWeightGen initialization error."
      call MPI_Finalize(rc)
      stop 1
  endif
#else
  PetNo = 0
#endif

  if (PetNo == 0) then
      logflag = ESMF_LOGKIND_MULTI
      call ESMF_UtilGetArgIndex ('--no_log', argindex=ind)
      if (ind > 0) then
        logflag = ESMF_LOGKIND_NONE 
      end if
      msgbuf(1) = logflag
   endif

#ifndef ESMF_MPIUNI
   ! broadcast to all other PETs
   call MPI_Bcast(msgbuf, 1, MPI_INTEGER, 0, MPI_COMM_WORLD,rc)
   if (PetNo /= 0)  logflag = msgbuf(1)
#endif 
  !------------------------------------------------------------------------
  ! Initialize ESMF
  !
  call ESMF_Initialize (defaultCalKind=ESMF_CALKIND_GREGORIAN, &
      defaultlogfilename="RegridWeightGen.Log", &
                      logkindflag=logflag, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)
  
  !------------------------------------------------------------------------
  ! get global vm information
  !
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(-1)

  ! set up local pet info
  call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

  !------------------------------------------------------------------------
  ! Parse keyword based arguments at Pet 0
  !   then broadcast the results to the rest of the Pets
  !
  if (PetNo == 0) then
    call ESMF_UtilGetArgIndex('--help', argindex=ind)
    if (ind /= -1) then
          call PrintUsage()
      terminateProg=.true.
    endif
    call ESMF_UtilGetArgIndex('--version', argindex=ind)
    if (ind /= -1) then
      call ESMF_UtilVersionPrint (versionFlag=.true.)
      terminateProg=.true.
    endif
    call ESMF_UtilGetArgIndex('-V', argindex=ind)
    if (ind /= -1) then
      call ESMF_UtilVersionPrint (vFlag=.true.)
      terminateProg=.true.
    endif
    if (terminateProg) goto 1110
    call ESMF_UtilGetArgIndex('-s', argindex=ind)
    if (ind == -1) call ESMF_UtilGetArgIndex('--source', argindex=ind, rc=rc)
    if (ind == -1) then
      write(*,*)
      print *, "ERROR: The required argument [-s|--source] is missing."
      print *, "Use the --help argument to see an explanation of usage."
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else
      call ESMF_UtilGetArg(ind+1, argvalue=srcfile)
    endif
    
    call ESMF_UtilGetArgIndex('-d', argindex=ind, rc=rc)
    if (ind == -1) call ESMF_UtilGetArgIndex('--destination', argindex=ind, rc=rc)
    if (ind == -1) then
      write(*,*)
      print *, "ERROR: The required argument [-d|-destination] is missing."
      print *, "Use the --help argument to see an explanation of usage."
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else
      call ESMF_UtilGetArg(ind+1, argvalue=dstfile)
    endif
     
    call ESMF_UtilGetArgIndex('-w', argindex=ind, rc=rc)
    if (ind == -1) call ESMF_UtilGetArgIndex('--weight', argindex=ind, rc=rc)
    if (ind == -1) then
      write(*,*)
      print *, "ERROR: The required argument [-w|--weight] is missing."
      print *, "Use the --help argument to see an explanation of usage."
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else      
      call ESMF_UtilGetArg(ind+1, argvalue=wgtfile)
    endif

    call ESMF_UtilGetArgIndex('-m', argindex=ind, rc=rc)
    if (ind == -1) call ESMF_UtilGetArgIndex('--method', argindex=ind, rc=rc)
    if (ind == -1) then
      !  print *, 'Use default interpolation method: bilinear'
      method = 'bilinear'
    else
      call ESMF_UtilGetArg(ind+1, argvalue=method)
          if ((trim(method) .ne. 'bilinear') .and. &
          (trim(method) .ne. 'conserve') .and. &
          (trim(method) .ne. 'conserve2nd') .and. &
              (trim(method) .ne. 'patch')    .and. &
          (trim(method) .ne. 'nearestdtos')   .and. &
          (trim(method) .ne. 'neareststod')) then
        write(*,*)
        print *, 'ERROR: The interpolation method "', trim(method), '" is not supported'
        print *, '  The supported methods are "bilinear", "patch", "conserve"'
        print *, '  "conserve2nd", "nearestdtos", and "neareststod"'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif    
    endif

   ! Get Line Type
    call ESMF_UtilGetArgIndex('-l', argindex=ind, rc=rc)
    if (ind == -1) call ESMF_UtilGetArgIndex('--line_type', argindex=ind, rc=rc)
    if (ind == -1) then
      !  Use default lineType based on method
      if ((trim(method) .eq. 'conserve') .or. &
          (trim(method) .eq. 'conserve2nd'))then
         lineTypeStr = 'greatcircle'
      else
         lineTypeStr = 'cartesian'
      endif
    else
      call ESMF_UtilGetArg(ind+1, argvalue=lineTypeStr)
      if ((trim(lineTypeStr) .ne. 'cartesian') .and. &
          (trim(lineTypeStr) .ne. 'greatcircle')) then
        write(*,*)
        print *, 'ERROR: The line type "', trim(lineTypeStr), '" is not supported'
        print *, '  The supported line types are "cartesian", and "greatcircle"'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif    
    endif

   ! Get extrap method
    call ESMF_UtilGetArgIndex('--extrap_method', argindex=ind, rc=rc)
    if (ind == -1) then
       extrapMethodStr = 'none'
    else
      call ESMF_UtilGetArg(ind+1, argvalue=extrapMethodStr)
      if ((trim(extrapMethodStr) .ne. 'none') .and. &
           (trim(extrapMethodStr) .ne. 'nearestidavg') .and. &
           (trim(extrapMethodStr) .ne. 'neareststod')) then
        write(*,*)
        print *, 'ERROR: The extrap. method "', trim(extrapMethodStr), '" is not supported'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif    
    endif


   ! Get extrap num src points
    call ESMF_UtilGetArgIndex('--extrap_num_src_pnts', argindex=ind, rc=rc)
    if (ind == -1) then
       extrap_num_src_pnts=8
    else
      call ESMF_UtilGetArg(ind+1, argvalue=extrapNumSrcPntsStr)
      read(extrapNumSrcPntsStr,'(i4)') extrap_num_src_pnts
    endif


   ! Get extrap dist exponent
    call ESMF_UtilGetArgIndex('--extrap_dist_exponent', argindex=ind, rc=rc)
    if (ind == -1) then
       extrapDistExponentStr='2.0'
    else
      call ESMF_UtilGetArg(ind+1, argvalue=extrapDistExponentStr)
    endif


    poleptrs = -1
    call ESMF_UtilGetArgIndex('-p', argindex=ind, rc=rc)
    if (ind == -1) call ESMF_UtilGetArgIndex('--pole', argindex=ind, rc=rc)
    if (ind == -1) then
      if ((trim(method) .eq. 'conserve') .or.    &
          (trim(method) .eq. 'conserve2nd') .or. & 
          (trim(method) .eq. 'nearestdtos') .or. &
          (trim(method) .eq. 'neareststod')) then
        ! print *, 'Use default pole: None'
        pole = ESMF_POLEMETHOD_NONE
        poleptrs = 0
      else
        !print *, 'Use default pole: All'
        pole = ESMF_POLEMETHOD_ALLAVG
      endif
    else
      call ESMF_UtilGetArg(ind+1, argvalue=flag)
      if (trim(flag) .eq. 'none') then
        pole = ESMF_POLEMETHOD_NONE
        poleptrs = 0
      else if (trim(flag) .eq. 'all') then
        pole = ESMF_POLEMETHOD_ALLAVG
      else if (trim(flag) .eq. 'teeth') then
        pole = ESMF_POLEMETHOD_TEETH
        poleptrs = -2
      else 
        read(flag,'(i4)') poleptrs
        pole = ESMF_POLEMETHOD_NPNTAVG
      endif
      if ((method .eq. 'conserve') .and. &
          (pole .ne. ESMF_POLEMETHOD_NONE)) then
        write(*,*)
        print *, 'ERROR: Conserve method only works with no pole.'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      if ((method .eq. 'conserve2nd') .and. &
          (pole .ne. ESMF_POLEMETHOD_NONE)) then
        write(*,*)
        print *, 'ERROR: Conserve2nd method only works with no pole.'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    typeSetFlag = .false.  
    srcFileType = ESMF_FILEFORMAT_UNKNOWN
    dstFileType = ESMF_FILEFORMAT_UNKNOWN
    srcIsRegional = .false.
    dstIsRegional = .false.
    ! deprecated
    call ESMF_UtilGetArgIndex('-t', argindex=ind, rc=rc)
    if (ind /= -1) then
     write(*,*)
     print *, "WARNING: deprecated switch -t will be ignored.  The file type will be detected automatically"
    endif

    call ESMF_UtilGetArgIndex('--src_type', argindex=ind, rc=rc)
    if (ind /= -1) then
     write(*,*)
     print *, "WARNING: deprecated switch -src_type will be ignored.  The file type will be detected automatically"
    endif

    call ESMF_UtilGetArgIndex('--dst_type', argindex=ind, rc=rc)
    if (ind /= -1) then
     write(*,*)
     print *, "WARNING: deprecated switch -dst_type will be ignored.  The file type will be detected automatically"
    endif

    ! Check the srcfile type and dstfile type
    call ESMF_FileTypeCheck(srcfile, srcFileType, varname=srcMeshName, rc=rc)
    if (rc/=ESMF_SUCCESS .or. srcFileType==ESMF_FILEFORMAT_UNKNOWN) then
        write(*,*)
        print *, 'ERROR: Unable to detect the source grid file type.'
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
    ! Check the dstfile type and dstfile type
    call ESMF_FileTypeCheck(dstfile, dstFileType, varname=dstMeshName, rc=rc)
    if (rc/=ESMF_SUCCESS .or. dstFileType==ESMF_FILEFORMAT_UNKNOWN) then
        write(*,*)
        print *, 'ERROR: Unable to detect the destination grid file type.'
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    ! If the src grid type is GRIDSPEC or UGRID, check if --src_missingvalue argument is given
    call ESMF_UtilGetArgIndex('--src_missingvalue', argindex=ind, rc=rc)
    if (ind == -1) then
      srcMissingValue = .false.
    else
      srcMissingValue = .true.
      call ESMF_UtilGetArg(ind+1, argvalue=srcVarName)         
    endif

    ! missing area only supported for GRIDSPEC and UGRID file.  When the grid
    ! file is UGRID, it only support the missing value when doing conservative regrid
    if (srcMissingValue) then
      if (srcFileType /= ESMF_FILEFORMAT_GRIDSPEC .and. &
          srcFileType /= ESMF_FILEFORMAT_UGRID) then
        write(*,*)
        print *, 'ERROR: --src_missingvalue is supported only when the source grid is in'
        print *, '       UGRID or GRIDSPEC format.'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    ! If the dst grid type is GRIDSPEC or UGRID, check if --dst_missingvalue argument is given
    call ESMF_UtilGetArgIndex('--dst_missingvalue', argindex=ind, rc=rc)
    if (ind == -1) then
      dstMissingValue = .false.
    else
      dstMissingValue = .true.
      call ESMF_UtilGetArg(ind+1, argvalue=dstVarName)         
    endif

    ! missing area only supported for GRIDSPEC and UGRID file.  When the grid
    ! file is UGRID, it only support the missing value when doing conservative regrid
    if (dstMissingValue) then
      if (dstFileType /= ESMF_FILEFORMAT_GRIDSPEC .and. &
          dstFileType /= ESMF_FILEFORMAT_UGRID) then
        write(*,*)
        print *, 'ERROR: --dst_missingvalue is supported only when the source grid is in'
        print *, '       UGRID or GRIDSPEC format.'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    ignoreUnmapped=.false.
    call ESMF_UtilGetArgIndex('-i', argindex=ind, rc=rc)
    if (ind == -1) call ESMF_UtilGetArgIndex('--ignore_unmapped', argindex=ind, rc=rc)
    if (ind /= -1) then
      ignoreUnmapped=.true.
    endif

    ignoreDegenerate=.false.
    call ESMF_UtilGetArgIndex('--ignore_degenerate', argindex=ind, rc=rc)
    if (ind /= -1) then
      ignoreDegenerate=.true.
    endif

    call ESMF_UtilGetArgIndex('-r', argindex=ind, rc=rc)
    if (ind /= -1) then
      srcIsRegional = .true.
      dstIsRegional = .true.
      pole = ESMF_POLEMETHOD_NONE
      poleptrs = 0
      ! print *, 'Set pole to None for regional grids.'
    endif

    call ESMF_UtilGetArgIndex('--src_regional', argindex=ind, rc=rc)
    if (ind /= -1) then
      srcIsRegional = .true.
      pole = ESMF_POLEMETHOD_NONE
      poleptrs = 0
      ! print *, 'Set pole to None for regional source grid.'
    endif

    call ESMF_UtilGetArgIndex('--dst_regional', argindex=ind, rc=rc)
    if (ind /= -1) then
      dstIsRegional = .true.
    endif

    ! --64bit_offset for large weight file
    call ESMF_UtilGetArgIndex('--64bit_offset', argindex=ind, rc=rc)
    if (ind /= -1) then
      largeFileFlag = .true.
    else
      largeFileFlag = .false.
    endif
   
    ! --netcdf4 for weight file format
    call ESMF_UtilGetArgIndex('--netcdf4', argindex=ind, rc=rc)
    if (ind /= -1) then
      netcdf4FileFlag = .true.
    else
      netcdf4FileFlag = .false.
    endif
   
    if (largeFileFlag .and. netcdf4FileFlag) then
      write(*,*)
      print *, 'ERROR: Both --netcdf4 and --64bit_offset are specified.'
      print *, '       Only one flag can be given.'
      print *, "Use the --help argument to see an explanation of usage."
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
 
    ! --weight_only for weight file format
    call ESMF_UtilGetArgIndex('--weight_only', argindex=ind, rc=rc)
    if (ind /= -1) then
      weightOnlyFlag = .true.
    else
      weightOnlyFlag = .false.
    endif
   
    ! --user_area - to use user-defined area for the cells
    call ESMF_UtilGetArgIndex('--user_areas', argindex=ind, rc=rc)
    if (ind /= -1) then
      userAreaFlag = .true.
    else
      userAreaFlag = .false.
    endif
   
    ! user area only needed for conservative regridding
    if (userAreaFlag .and. .not. ((method .eq. 'conserve') .or. &
         (method .eq. 'conserve2nd'))) then
       write(*,*)
       print *, 'WARNING: --user_areas is only needed in conservative remapping'
       print *, '       The flag is ignored'
       userAreaFlag = .false.
    endif

    if (userAreaFlag .and. (srcFileType /= ESMF_FILEFORMAT_SCRIP .and. &
        srcFileType /= ESMF_FILEFORMAT_ESMFMESH) .and. &
        (dstFileType /= ESMF_FILEFORMAT_SCRIP .and. &
        dstFileType /= ESMF_FILEFORMAT_ESMFMESH)) then
      write(*,*)
      print *, 'ERROR: --user_areas is supported only when the source or destination'
      print *, '       grid are in SCRIP of ESMF format.'
      print *, "Use the --help argument to see an explanation of usage."
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    ! Norm type
    normType = ESMF_NORMTYPE_DSTAREA ! Default to DSTAREA
    call ESMF_UtilGetArgIndex('--norm_type', argindex=ind, rc=rc)
    if (ind /= -1) then
      call ESMF_UtilGetArg(ind+1, argvalue=flag)
      if (trim(flag) .eq. 'dstarea') then
        normType = ESMF_NORMTYPE_DSTAREA
      else if (trim(flag) .eq. 'fracarea') then
        normType = ESMF_NORMTYPE_FRACAREA
      else 
        write(*,*)
        print *, 'ERROR: Unknown --norm_type: must be either dstarea or fracarea'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    ! --src_coordinates, --dst_coordinates for GRIDSPEC file if there are multiple
    ! coordinate variables
    useSrcCoordVar = .false.
    useDstCoordVar = .false.
    if (srcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
      call ESMF_UtilGetArgIndex('--src_coordinates', argindex=ind, rc=rc)
      if (ind /= -1) then
        call ESMF_UtilGetArg(ind+1, argvalue=argStr)         
        pos = INDEX(argStr, ',')
        if (pos == 0) then
          write(*,*)
          print *, "ERROR: wrong value for --src_coordinates: should be lon and lat "
          print *, "       variable names separated by comma" 
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
        srcCoordNames(1)=argStr(1:pos-1)
        srcCoordNames(2)=argStr(pos+1:)
        pos = INDEX(argStr(pos+1:), ",")
        if (pos /= 0) then
          write(*,*)
          print *, "ERROR: wrong value for --src_coordinates: should be lon and lat "
          print *, "       variable names separated by comma" 
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
        useSrcCoordVar = .true.
      endif
    endif

    if (dstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
      call ESMF_UtilGetArgIndex('--dst_coordinates', argindex=ind, rc=rc)
      if (ind /= -1) then
        call ESMF_UtilGetArg(ind+1, argvalue=argStr)         
        pos = INDEX(argStr, ",")
        if (pos == 0) then
          write(*,*)
          print *, "ERROR: wrong value for --dst_coordinates: should be lon and lat "
          print *, "       variable names separated by comma" 
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
        dstCoordNames(1)=argStr(1:pos-1)
        dstCoordNames(2)=argStr(pos+1:)
        pos = INDEX(argStr(pos+1:), ",")
        if (pos /= 0) then
          write(*,*)
          print *, "ERROR: wrong value for --dst_coordinates: should be lon and lat "
          print *, "       variable names separated by comma" 
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
        useDstCoordVar = .true.
      endif
    endif

    useSrcCorner = .false.
    useDstCorner = .false.
    call ESMF_UtilGetArgIndex('--src_loc', argindex=ind, rc=rc)
    ! the --src_loc is required if the file format is UGRID or ESMFMESH and
    ! the regrid method is not conservative  (no default value to force user to specify
    ! the location)
    if (ind /= -1) then
      call ESMF_UtilGetArg(ind+1, argvalue=argStr)         
      if (trim(argStr) .eq. 'corner') then
         useSrcCorner = .true.
      elseif (trim(argStr) .eq. 'center') then
           useSrcCorner = .false.
      else
          write(*,*)
          print *, 'ERROR: Unknown --src_loc: must be either center or corner'
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    else  ! the argument does not exist
      if ((srcFileType == ESMF_FILEFORMAT_UGRID .or. srcFileType == ESMF_FILEFORMAT_ESMFMESH) &
          .and. (method /= 'conserve') .and. (method /= 'conserve2nd')) then
          write(*,*)
          print *, 'ERROR: --src_loc is required for this source file type and regridding'
        print *, '       method.'
          print *, '       Please specifiy either "center" or "corner"'
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    ! deoes not support corner coordinates for SCRIP and GRIDSPEC files
    if ((srcFileType == ESMF_FILEFORMAT_SCRIP .or. srcFileType == ESMF_FILEFORMAT_GRIDSPEC) &
        .and. useSrcCorner ) then
          write(*,*)
          print *, 'ERROR: cannot use corner coordinates to do regridding for SCRIP or'
        print *, '       GRIDSPEC files.'
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    call ESMF_UtilGetArgIndex('--dst_loc', argindex=ind, rc=rc)
    ! the --dst_loc is required if the file format is UGRID or ESMFMESH and
    ! the regrid method is not conservative  (no default value to force user to specify
    ! the location)
    if (ind /= -1) then
      call ESMF_UtilGetArg(ind+1, argvalue=argStr)         
      if (trim(argStr) .eq. 'corner') then
         useDstCorner = .true.
      elseif (trim(argStr) .eq. 'center') then
           useDstCorner = .false.
      else
          write(*,*)
          print *, 'ERROR: Unknown --dst_loc: must be either center or corner'
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    else  ! the argument does not exist
      if ((dstFileType == ESMF_FILEFORMAT_UGRID .or. dstFileType == ESMF_FILEFORMAT_ESMFMESH) &
          .and. (method /= 'conserve') .and. (method /= 'conserve2nd')) then
          write(*,*)
          print *, 'ERROR: --dst_loc is required for this source file type and regridding'
          print *, '       method.'
          print *, '       Please specifiy either "center" or "corner"'
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    ! does not support corner coordinates for SCRIP and GRIDSPEC files
    if ((dstFileType == ESMF_FILEFORMAT_SCRIP .or. dstFileType == ESMF_FILEFORMAT_GRIDSPEC) &
        .and. useDstCorner) then
          write(*,*)
          print *, 'ERROR: cannot use corner coordinates to do regridding for SCRIP or'
        print *, '       GRIDSPEC files.'
          print *, "Use the --help argument to see an explanation of usage."
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    ! does not support corner coordinates for conservative regridding for any file types
    if ((method == 'conserve' .or. method == 'conserve2nd') .and. &
         (useSrcCorner .or. useDstCorner)) then
          write(*,*)
          print *, 'ERROR: using corner coordinates for conservative regridding is not supported.'
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    ! --tilefile_path to specify alternative tile file path
    call ESMF_UtilGetArgIndex('--tilefile_path', argindex=ind, rc=rc)
    if (ind /= -1) then
      call ESMF_UtilGetArg(ind+1, argvalue=tilePath)         
      length=len_trim(tilePath)
      if (tilePath(length:length) /= '/') tilePath(length+1:length+1)='/'
      useTilePathFlag = .true.
    else
      tilePath=' '
      useTilePathFlag = .false.
    endif

    ! -- if src or dst grid is a 1D UGRID file, there are many restrictions: 
    !    1. if it is dst grid, no conservative regridding, if it is src grid,
    !    only nearest neighbor regridding allowed. 2. only allow regridding on
    !    the corner, 3. only allow weight_only file.
    if (srcFileType == ESMF_FILEFORMAT_UGRID) then
       call ESMF_UGridInq(srcfile, nodeCoordDim = meshdim, rc=rc) 
       if (meshdim == 1) then
         if (method /= 'neareststod' .and. method /= 'nearestdtos') then
            write(*,*)
            print *, 'ERROR: only nearest neighbor regridding is supported for'
            print *, '1D source grid in UGRID format.'
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
         endif
         if (.not. useSrcCorner) then
            write(*,*)
            print *, 'ERROR: Only allow regridding on the corner for 1D source grid.'
            print *, 'Use --src_loc corner to specify it'
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
         endif
       endif
    endif
    if (dstFileType == ESMF_FILEFORMAT_UGRID) then
       call ESMF_UGridInq(dstfile, nodeCoordDim = meshdim, rc=rc) 
       if (meshdim == 1) then
         if (method == 'conserve' .or. method == 'conserve2nd') then
            write(*,*)
            print *, 'ERROR: conservative regridding is not supported for 1D destination grid.'
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
         endif
         if (.not. useDstCorner) then
            write(*,*)
            print *, 'ERROR: Only allow regridding on the corner for 1D destination grid.'
            print *, 'Use --dst_loc corner to specify it'
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
         endif
       endif
    endif

    checkFlag = .false.
    call ESMF_UtilGetArgIndex('--check', argindex=ind, rc=rc)
    if (ind /= -1) checkFlag = .true.

    moabFlag = .false.
    call ESMF_UtilGetArgIndex('--moab', argindex=ind, rc=rc)
    if (ind /= -1) moabFlag = .true.

    if (moabFlag) call ESMF_MeshSetMOAB(.true., rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

1110 continue
    commandbuf2(:)=0
    if (terminateProg) then
      commandbuf2(1)=-9999            
    else
      commandbuf2(1)=srcFileType%fileformat
      commandbuf2(2)=dstFileType%fileformat
      if (method .eq. 'patch') commandbuf2(3)=1
      if (method .eq. 'conserve') commandbuf2(3)=2
      if (method .eq. 'neareststod') commandbuf2(3)=3
      if (method .eq. 'nearestdtos') commandbuf2(3)=4
      if (method .eq. 'conserve2nd') commandbuf2(3)=5
      commandbuf2(4)=poleptrs
      if (ignoreUnmapped) commandbuf2(5) = 1
      if (userAreaFlag)   commandbuf2(6) = 1
      if (srcMissingValue) commandbuf2(7) = 1
      if (dstMissingValue) commandbuf2(8) = 1
      if (srcIsRegional) commandbuf2(9) = 1
      if (dstIsRegional) commandbuf2(10) = 1
      if (useSrcCoordVar) commandbuf2(11) = 1
      if (useDstCoordVar) commandbuf2(12) = 1
      if (largeFileFlag) commandbuf2(13) = 1
      if (netcdf4FileFlag) commandbuf2(14) = 1
      if (checkFlag) commandbuf2(15) = 1 
      commandbuf2(16) = normType%normtype
      if (ignoreDegenerate) commandbuf2(17) = 1
      if (useSrcCorner) commandbuf2(18) = 1
      if (useDstCorner) commandbuf2(19) = 1
      if (trim(lineTypeStr) .eq. 'cartesian') commandbuf2(20) = 1
      if (trim(lineTypeStr) .eq. 'greatcircle') commandbuf2(20) = 2
      if (weightOnlyFlag) commandbuf2(21) = 1
      if (extrapMethodStr .eq. 'none') commandbuf2(22)=1
      if (extrapMethodStr .eq. 'neareststod') commandbuf2(22)=2
      if (extrapMethodStr .eq. 'nearestidavg') commandbuf2(22)=3
      commandbuf2(23)=extrap_num_src_pnts
    endif 


    call ESMF_VMBroadcast(vm, commandbuf2, size (commandbuf2), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

    if (terminateProg) then
      goto 1111
    endif

    ! Group the command line arguments and broadcast to other PETs
    commandbuf1(1)=srcfile
    commandbuf1(2)=dstfile
    commandbuf1(3)=wgtfile
    commandbuf1(4)=tilePath
    commandbuf3(1)=srcMeshName
    commandbuf3(2)=dstMeshName
    commandbuf3(3)=srcVarName
    commandbuf3(4)=dstVarName
    commandbuf3(5)=srcCoordNames(1)
    commandbuf3(6)=srcCoordNames(2)
    commandbuf3(7)=dstCoordNames(1)
    commandbuf3(8)=dstCoordNames(2)
    commandbuf3(9)=extrapDistExponentStr

    ! Broadcast the command line arguments to all the PETs
    call ESMF_VMBroadcast(vm, commandbuf1, len(commandbuf1)*size(commandbuf1), 0, rc=rc)
    call ESMF_VMBroadcast(vm, commandbuf3, len(commandbuf3)*size(commandbuf3), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

  else
    call ESMF_VMBroadcast(vm, commandbuf2, size (commandbuf2), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

    if (commandbuf2(1) == -9999) then
      goto 1111        
    endif

    srcFileType%fileformat = commandbuf2(1)
    dstFileType%fileformat = commandbuf2(2)
    if (commandbuf2(3)==0) then
      method = 'bilinear'
    else if (commandbuf2(3)==1) then
      method = 'patch'
    else if (commandbuf2(3)==2) then
      method = 'conserve'
    else if (commandbuf2(3)==3) then
      method = 'neareststod'
    else if (commandbuf2(3)==4) then
      method = 'nearestdtos'
    else if (commandbuf2(3)==5) then
      method = 'conserve2nd'
    else
      method = 'bilinear'
    endif
    poleptrs = commandbuf2(4)
    if (poleptrs == -1) then 
      pole=ESMF_POLEMETHOD_ALLAVG
    else if (poleptrs == -2) then 
      pole=ESMF_POLEMETHOD_TEETH
    else if (poleptrs ==  0) then
      pole=ESMF_POLEMETHOD_NONE
    else
      pole=ESMF_POLEMETHOD_NPNTAVG 
    endif
    if (commandbuf2(5) == 1) then
      ignoreUnmapped=.true.
    else
      ignoreUnmapped=.false.
    endif

    if (commandbuf2(6) == 1) then
      userAreaFlag=.true.
    else
      userAreaFlag=.false.
    endif

    if (commandbuf2(7) == 1) then
      srcMissingValue=.true.
    else
      srcMissingValue=.false.
    endif

    if (commandbuf2(8) == 1) then
      dstMissingValue=.true.
    else
      dstMissingValue=.false.
    endif
    if (commandbuf2(9)==1) then
      srcIsRegional = .true.
    else
      srcIsRegional = .false.
    endif
    if (commandbuf2(10)==1) then
      dstIsRegional = .true.
    else
      dstIsRegional = .false.
    endif
    if (commandbuf2(11)==1) then
      useSrcCoordVar = .true.
    else
      useSrcCoordVar = .false.
    endif
    if (commandbuf2(12)==1) then
      useDstCoordVar = .true.
    else
      useDstCoordVar = .false.
    endif
    if (commandbuf2(13)==1) then
      largeFileFlag = .true.
    else
      largeFileFlag = .false.
    endif
    if (commandbuf2(14)==1) then
      netcdf4FileFlag = .true.
    else
      netcdf4FileFlag = .false.
    endif
    if (commandbuf2(15)==1) then
      checkFlag = .true.
    else
      checkFlag = .false.
    endif
    normType%normtype=commandbuf2(16)
    if (commandbuf2(17) == 1) then
      ignoreDegenerate=.true.
    else
      ignoreDegenerate=.false.
    endif
    if (commandbuf2(18)==1) then
      useSrcCorner=.true.
    else
      useSrcCorner=.false.
    endif
    if (commandbuf2(19)==1) then
      useDstCorner=.true.
    else
      useDstCorner=.false.
    endif
    if (commandbuf2(21)==1) then
      weightOnlyFlag=.true.
    else
      weightOnlyFlag=.false.
    endif

    if (commandbuf2(22)==0) then
      extrapMethodStr = 'none'
    else if (commandbuf2(22)==1) then
      extrapMethodStr = 'none'
    else if (commandbuf2(22)==2) then
      extrapMethodStr = 'neareststod'
    else if (commandbuf2(22)==3) then
      extrapMethodStr = 'nearestidavg'
    else
      method = 'none'
    endif

    extrap_num_src_pnts=commandbuf2(23)

    call ESMF_VMBroadcast(vm, commandbuf1, len(commandbuf1)*size(commandbuf1), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    call ESMF_VMBroadcast(vm, commandbuf3, len(commandbuf3)*size(commandbuf3), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    srcfile = commandbuf1(1)
    dstfile = commandbuf1(2)
    wgtfile = commandbuf1(3)
    tilePath = commandbuf1(4)
    if (tilePath .eq. ' ') then 
       useTilePathFlag = .false.
    else 
       useTilePathFlag = .true.
    endif
    srcMeshName = commandbuf3(1)
    dstMeshName = commandbuf3(2)
    srcVarName = commandbuf3(3)
    dstVarName = commandbuf3(4)
    srcCoordNames(1) = commandbuf3(5)
    srcCoordNames(2) = commandbuf3(6)
    dstCoordNames(1) = commandbuf3(7)
    dstCoordNames(2) = commandbuf3(8)
    extrapDistExponentStr=commandbuf3(9)
  endif

  if (trim(method) .eq. 'bilinear') then
    methodflag = ESMF_REGRIDMETHOD_BILINEAR
  else if (trim(method) .eq. 'conserve') then
    methodflag = ESMF_REGRIDMETHOD_CONSERVE
  else if (trim(method) .eq. 'conserve2nd') then
    methodflag = ESMF_REGRIDMETHOD_CONSERVE_2ND
  else if (trim(method) .eq. 'patch') then
    methodflag = ESMF_REGRIDMETHOD_PATCH
  else if (trim(method) .eq. 'neareststod') then
    methodflag = ESMF_REGRIDMETHOD_NEAREST_STOD
  else if (trim(method) .eq. 'nearestdtos') then
    methodflag = ESMF_REGRIDMETHOD_NEAREST_DTOS
  endif       

  if (ignoreunmapped) then
    unmappedaction = ESMF_UNMAPPEDACTION_IGNORE
  else
    unmappedaction = ESMF_UNMAPPEDACTION_ERROR
  endif

  ! Set lineType
  if (commandbuf2(20)==1) then
     lineType=ESMF_LINETYPE_CART
  else if (commandbuf2(20)==2) then
     lineType=ESMF_LINETYPE_GREAT_CIRCLE
  endif

  ! Set extrap method
  if (trim(extrapMethodStr) .eq. 'none') then 
     extrapMethodFlag=ESMF_EXTRAPMETHOD_NONE
  else if (trim(extrapMethodStr) .eq. 'neareststod') then
     extrapMethodFlag=ESMF_EXTRAPMETHOD_NEAREST_STOD
  else if (trim(extrapMethodStr) .eq. 'nearestidavg') then
     extrapMethodFlag=ESMF_EXTRAPMETHOD_NEAREST_IDAVG
  else 
     extrapMethodFlag=ESMF_EXTRAPMETHOD_NONE
  endif

  ! Set extrap distance exponent
  read(extrapDistExponentStr,*) extrap_dist_exponent 

#if 0
  write(*,*) "extrapmethod=",extrapMethodflag%extrapmethod
  write(*,*) "extrap_num_src_pnts=",extrap_num_src_pnts
  write(*,*) "extrap_dist_exponent=",extrap_dist_exponent
#endif

  if (useTilePathFlag) then
      call ESMF_RegridWeightGen(srcfile, dstfile, wgtfile, regridmethod=methodflag, &
                            polemethod = pole, regridPoleNPnts = poleptrs, unmappedaction = unmappedaction, &
                            srcFileType = srcFileType, dstFileType = dstFileType, &
                            ignoreDegenerate = ignoreDegenerate, &
                            lineType=lineType, &
                            normType=normType, &
                            extrapMethod=extrapMethodFlag, &
                            extrapNumSrcPnts=extrap_num_src_pnts, &
                            extrapDistExponent=extrap_dist_exponent, &
                            srcRegionalFlag = srcIsRegional, dstRegionalFlag = dstIsRegional, &
                            srcMeshname = srcMeshname, dstMeshname = dstMeshname, &
                            srcMissingvalueFlag = srcMissingValue, srcMissingvalueVar = srcVarName, &
                            dstMissingvalueFlag = dstMissingValue, dstMissingvalueVar = dstVarName, &
                            useSrcCoordFlag = useSrcCoordVar, useDstCoordFlag = useDstCoordVar, &
                            srcCoordinateVars = srcCoordNames, dstCoordinateVars = dstCoordNames, &
                            useUserAreaFlag = userAreaFlag, largefileFlag = largeFileFlag, &
                            netcdf4FileFlag = netcdf4FileFlag,  &
                            weightOnlyFlag  = weightOnlyFlag, &
                            useSrcCornerFlag = useSrcCorner, &
                            useDstCornerFlag = useDstCorner, &
                            tileFilePath = trim(tilePath), &
                            verboseFlag = .true., rc = rc)
  else
      call ESMF_RegridWeightGen(srcfile, dstfile, wgtfile, regridmethod=methodflag, &
                            polemethod = pole, regridPoleNPnts = poleptrs, unmappedaction = unmappedaction, &
                            srcFileType = srcFileType, dstFileType = dstFileType, &
                            ignoreDegenerate = ignoreDegenerate, &
                            lineType=lineType, &
                            normType=normType, &
                            extrapMethod=extrapMethodFlag, &
                            extrapNumSrcPnts=extrap_num_src_pnts, &
                            extrapDistExponent=extrap_dist_exponent, &
                            srcRegionalFlag = srcIsRegional, dstRegionalFlag = dstIsRegional, &
                            srcMeshname = srcMeshname, dstMeshname = dstMeshname, &
                            srcMissingvalueFlag = srcMissingValue, srcMissingvalueVar = srcVarName, &
                            dstMissingvalueFlag = dstMissingValue, dstMissingvalueVar = dstVarName, &
                            useSrcCoordFlag = useSrcCoordVar, useDstCoordFlag = useDstCoordVar, &
                            srcCoordinateVars = srcCoordNames, dstCoordinateVars = dstCoordNames, &
                            useUserAreaFlag = userAreaFlag, largefileFlag = largeFileFlag, &
                            netcdf4FileFlag = netcdf4FileFlag,  &
                            weightOnlyFlag  = weightOnlyFlag, &
                            useSrcCornerFlag = useSrcCorner, &
                            useDstCornerFlag = useDstCorner, &
                            verboseFlag = .true., rc = rc)
  endif

  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

  ! Output success
  if (PetNo==0) then
    write(*,*) "Completed weight generation successfully."
    !write(*,*) "Completed weight generation in ", (endtime-starttime)*1000, "msecs"
    write(*,*) 
  endif

!  write(*,*) "Start of check routine"
!  write(*,*) 

  ! error checking
  if (checkFlag) then
    call ESMF_RegridWeightGenCheck(wgtfile, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
  endif

1111  continue
  call ESMF_Finalize()

contains

  subroutine ErrorMsgAndAbort(localPet)
    integer ::  localPet
  
    if (localPet >= 0) then
      write(*,*) "ERROR: Problem on processor ",localPet,". Please see the PET*.RegridWeightGen.Log files for a traceback."
    else
      write(*,*) "ERROR: Please see the PET*.RegridWeightGen.Log files for a traceback."
    endif
  
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  end subroutine ErrorMsgAndAbort

  subroutine PrintUsage()
    print *, "Usage: ESMF_RegridWeightGen --source|-s src_grid_filename" 
    print *, "                           --destination|-d dst_grid_filename"
    print *, "                      --weight|-w out_weight_file "
    print *, "                      [--method|-m bilinear|patch|neareststod|nearestdtos|conserve|conserve2nd]"
    print *, "                      [--pole|-p all|none|teeth|<N>]"
    print *, "                      [--line_type|-l cartesian|greatcircle]"
    print *, "                      [--norm_type dstarea|fracarea]"
    print *, "                      [--extrap_method none|neareststod|nearestidavg]"
    print *, "                      [--extrap_num_src_pnts <N>]"
    print *, "                      [--extrap_dist_exponent <P>]"
    print *, "                      [--ignore_unmapped|-i]"
    print *, "                      [--ignore_degenerate]"
    print *, "                      [-r]"
    print *, "                      [--src_regional]"
    print *, "                      [--dst_regional]"
    print *, "                      [--64bit_offset]"
    print *, "                      [--netcdf4]"
    print *, "                      [--weight_only]"
    print *, "                      [--src_missingvalue src_var_name]"
    print *, "                      [--dst_missingvalue dst_var_name]"
    print *, "                      [--src_coordinates lon_var_name,lat_var_name]"
    print *, "                      [--dst_coordinates lon_var_name,lat_var_name]"
    print *, "                      [--user_areas]"
    print *, "                      [--src_loc center|corner]"
    print *, "                      [--dst_loc center|corner]"
    print *, "                      [--tilefile_path tile_file_path]"
    print *, "                      [--no_log]"
    print *, "                      [--check]"
    print *, "                      [--help]"
    print *, "                      [--version]"
    print *, "                      [-V]"
    print *, "where"
    print *, "--source or -s - a required argument specifying the source grid file"
    print *, "                 name"
    print *, "--destination or -d - a required argument specifying the destination grid"
    print *, "                      file name"
    print *, "--weight or -w - a required argument specifying the output regridding weight"
    print *, "                 file name"
    print *, "--method or -m - an optional argument specifying which interpolation method is"
    print *, "                 used.  The default method is bilinear."
    print *, "--pole or -p - an optional argument indicating what to do with the pole."
    print *, "                 The default value is all."
    print *, "--line_type or -l - an optional argument indicating the type of path"
    print *, "                     lines (e.g. cell edges) follow on a spherical"
    print *, "                    surface. The default value depends on the regrid"
    print *, "                    method. For non-conservative methods the default is"
    print *, "                    cartesian. For conservative methods the default is greatcircle." 
    print *, "--norm_type - an optional argument indicating the type of normalization to"
    print *, "              do when generating conserative weights. The default value is dstarea."
    print *, "--extrap_method - an optional argument specifying which extrapolation method is"
    print *, "                 used.  The default method is none."
    print *, "--extrap_num_src_pnts - an optional argument specifying how many source points should"
    print *, "                be used when the extrapolation method is nearestidavg. The default is 8."
    print *, "--extrap_dist_exponent - an optional argument specifying the exponent that the distance should"
    print *, "                be raised to when the extrapolation method is nearestidavg. The default is 2.0."
    print *, "--ignore_unmapped or -i - ignore unmapped destination points. If not specified,"
    print *, "                          the default is to stop with an error."
    print *, "--ignore_degenerate - ignore degenerate cells in the input grids. If not specified,"
    print *, "                          the default is to stop with an error."
    print *, "-r         - an optional argument specifying the source and destination grids"
    print *, "             are regional grids.  Without this argument, the grids are assumed"
    print *, "             to be global"
    print *, "--src_regional   - an optional argument specifying the source grid is regional."
    print *, "             Without this argument, the src grids is assumed to be global."
    print *, "--dst_regional   - an optional argument specifying the destination grid is regional"
    print *, "             Without this argument, the dst grids is assumed to be global."
    print *, "--64bit_offset  - an optional argument specifying the output weight file is in"
    print *, "             NetCDF 64-bit offset format.  This option only works with NetCDF library"
    print *, "             version 3.6 and above"
    print *, "--netcdf4  - an optional argument specifying the output weight file is in"
    print *, "             the NetCDF4 format. This option only works with NetCDF library"
    print *, "             version 4.1 and above"
    print *, "--weight_only  - an Optional argument specifying the output weight file only contains"
    print *, "             the weights and the source and destination grid's indices."
    print *, "--src_missingvalue  - an optional argument used when the src file type is GRIDSPEC"
    print *, "             or UGRID. It defines the variable name whose 'missing_value' or"
    print *, "             '_FillValue' attribute will be used to construct the mask for the source"
    print *, "             grid. Without this argument,a GRIDSPEC file or a UGRID file is not masked."
    print *, "--dst_missingvalue  - an optional argument used when the destination file type is"
    print *, "             GRIDSPEC or UGRID. It defines the variable name whose 'missing_value' or"
    print *, "             '_FillValue' attribute will be used to construct the mask for the destination"
    print *, "             grid. Without this argument,a GRIDSPEC file or a UGRID file is not masked."
    print *, "--src_coordinates  - an optional argument used when the source grid type is GRIDSPEC."
    print *, "             It defines the longitude and latitude variable names separated by comma,"
    print *, "             in case there are multiple coordinate variables defined in the file"
    print *, "--dst_coordinates  - an optional argument used when the destination grid type is GRIDSPEC."
    print *, "             It defines the longitude and latitude variable names separated by comma,"
    print *, "             in case there are multiple coordinate variables defined in the file"
    print *, "--user_areas  - an optional argument specifying that the conservation is adjusted to"
    print *, "             hold for the user areas provided in the grid files.  If not specified,"
    print *, "             then the conservation will hold for the ESMF calculated (great circle)"
    print *, "             areas.  Whichever areas the conservation holds for are output to the"
    print *, "             weight file."
    print *, "--src_loc   - an optional argument specifying which location is used to do the regridding"
    print *, "            The location can be either 'center' or 'corner'.  Currently, this argument"
    print *, "            is only required when the source grid file is an unstructured grid defined"
    print *, "            in UGRID or ESMF format and the regridding method is non-conservative. For  "
    print *, "            all other cases, the default location is 'center'."
    print *, "--dst_loc   - an optional argument specifying which location is used to do the regridding"
    print *, "            The location can be either 'center' or 'corner'.  Currently, this argument"
    print *, "            is 'center'.  Currently, this argument will only be used when the"
    print *, "            is only required when the destination grid file is an unstructured grid defined"
    print *, "            in UGRID or ESMF format and the regridding method is non-conservative. For  "
    print *, "            all other cases, the default location is 'center'."
    print *, "--tilefile_path - the alternative file path for the tile files when the grid file type is"
    print *, "            MOSAIC."
    print *, "--no_log    - Turn off the ESMF logs."
    print *, "--check    - Check that the generated weights produce reasonable regridded fields.  This"
    print *, "             is done by calling ESMF_Regrid() on an analytic source field using the weights"
    print *, "             generated by this application.  The mean relative error between the destination"
    print *, "             and analytic field is computed, as well as the relative error between the mass" 
    print *, "             of the source and destination fields in the conservative case."
    print *, "--help     - Print this help message and exit."
    print *, "--version  - Print ESMF version and license information and exit."
    print *, "-V        - Print ESMF version number and exit."
    print *, ""
    print *, "For questions, comments, or feature requests please send email to:"
    print *, "esmf_support@list.woc.noaa.gov"
    print *, ""
    print *, "Visit http://www.earthsystemmodeling.org/ to find out more about the"
    print *, "Earth System Modeling Framework."
    print *, ""
  end subroutine PrintUsage

end program ESMF_RegridWeightGenApp
