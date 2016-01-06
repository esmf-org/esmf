!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!!-------------------------------------------------------------------------------------

program ESMF_RegridApp

! !USES:
  use ESMF
  use ESMF_IOScripMod
  use ESMF_IOGridspecMod
  use ESMF_FileRegridMod

  implicit none

#ifndef ESMF_MPIUNI
  include "mpif.h"
#endif

  integer            :: rc
  type(ESMF_VM)      :: vm
  integer            :: PetNo, PetCnt
  character(ESMF_MAXPATHLEN) :: srcfile, dstfile
  character(ESMF_MAXPATHLEN) :: srcvarname, dstvarname
  character(ESMF_MAXPATHLEN) :: cwd
  character(len=40)  :: method, flag
  type(ESMF_PoleMethod_Flag) :: pole
  integer            :: poleptrs
  type(ESMF_RegridMethod_Flag) :: methodflag
  character(len=ESMF_MAXPATHLEN) :: commandbuf1(4)
  integer            :: commandbuf2(7)
  integer            :: ind, pos
  logical 	     :: ignoreUnmapped, userAreaFlag
  logical            :: ignoreDegenerate
  type(ESMF_UnmappedAction_Flag) :: unmappedaction
  logical            :: srcIsRegional, dstIsRegional, typeSetFlag
  character(len=256) :: argStr
  logical            :: terminateProg
  !real(ESMF_KIND_R8) :: starttime, endtime
  type(ESMF_LogKind_Flag) :: msgbuf(1)
  type(ESMF_LogKind_Flag) :: logflag
  character(len=ESMF_MAXPATHLEN)  :: argvalue
  integer            :: count, i
  
  terminateProg = .false.
  
  ! Check if --no_log is given, if so, call ESMF_Initialize() with ESMF_LOGKIND_NONE flag
#ifndef ESMF_MPIUNI
  call MPI_Init(rc)
  if (rc /= MPI_SUCCESS) then
      write(*,*) "ERROR: ESMF_Regrid initialization error."
      stop 1
  endif
  call MPI_Comm_rank(MPI_COMM_WORLD, PetNo, rc) 
  if (rc /= MPI_SUCCESS) then
      write(*,*) "ERROR: ESMF_Regrid initialization error."
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
	defaultlogfilename="Regrid.Log", &
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
     
    call ESMF_UtilGetArgIndex('--src_var', argindex=ind, rc=rc)
    if (ind == -1) then
      write(*,*)
      print *, "ERROR: The required argument --src_var is missing."
      print *, "Use the --help argument to see an explanation of usage."
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else	
      call ESMF_UtilGetArg(ind+1, argvalue=srcvarname)
    endif

    call ESMF_UtilGetArgIndex('--dst_var', argindex=ind, rc=rc)
    if (ind == -1) then
      write(*,*)
      print *, "ERROR: The required argument --dst_var is missing."
      print *, "Use the --help argument to see an explanation of usage."
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    else	
      call ESMF_UtilGetArg(ind+1, argvalue=dstvarname)
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
	        (trim(method) .ne. 'patch')    .and. &
          (trim(method) .ne. 'nearestdtos')   .and. &
          (trim(method) .ne. 'neareststod')) then
        write(*,*)
        print *, 'ERROR: The interpolation method "', trim(method), '" is not supported'
        print *, '  The supported methods are "bilinear", "patch", and "conserve"'
        print *, "Use the --help argument to see an explanation of usage."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif    
    endif

    poleptrs = -1
    call ESMF_UtilGetArgIndex('-p', argindex=ind, rc=rc)
    if (ind == -1) call ESMF_UtilGetArgIndex('--pole', argindex=ind, rc=rc)
    if (ind == -1) then
      if ((trim(method) .eq. 'conserve') .or.    & 
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

    srcIsRegional = .false.
    dstIsRegional = .false.
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

    ! --user_area - to use user-defined area for the cell
    call ESMF_UtilGetArgIndex('--user_areas', argindex=ind, rc=rc)
    if (ind /= -1) then
      userAreaFlag = .true.
    else
      userAreaFlag = .false.
    endif
   
    ! user area only needed for conservative regridding
    if (userAreaFlag .and. (method .ne. 'conserve')) then
      write(*,*)
      print *, 'WARNING: --user_areas is only needed in conservative remapping'
      print *, '       The flag is ignored'
      userAreaFlag = .false.
    endif

1110 continue 
    commandbuf2(:)=0
    if (terminateProg) then
      commandbuf2(1)=-9999            
    else
      if (method .eq. 'patch') commandbuf2(1)=1
      if (method .eq. 'conserve') commandbuf2(1)=2
      if (method .eq. 'neareststod') commandbuf2(1)=3
      if (method .eq. 'nearestdtos') commandbuf2(1)=4
      commandbuf2(2)=poleptrs
      if (srcIsRegional) commandbuf2(3) = 1
      if (dstIsRegional) commandbuf2(4) = 1
      if (ignoreUnmapped) commandbuf2(5) = 1
      if (userAreaFlag)   commandbuf2(6) = 1
      if (ignoreDegenerate) commandbuf2(7) = 1
    endif 

    call ESMF_VMBroadcast(vm, commandbuf2, size (commandbuf2), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

    if (terminateProg) then
      goto 1111
    endif

    ! Group the command line arguments and broadcast to other PETs
    commandbuf1(1)=srcfile
    commandbuf1(2)=dstfile
    commandbuf1(3)=srcvarname
    commandbuf1(4)=dstvarname

    ! Broadcast the command line arguments to all the PETs
    call ESMF_VMBroadcast(vm, commandbuf1, len (commandbuf1)*size (commandbuf1), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

  else
    call ESMF_VMBroadcast(vm, commandbuf2, size (commandbuf2), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

    if (commandbuf2(1) == -9999) then
      goto 1111	  
    endif

    if (commandbuf2(1)==0) then
      method = 'bilinear'
    else if (commandbuf2(1)==1) then
      method = 'patch'
    else if (commandbuf2(1)==2) then
      method = 'conserve'
    else if (commandbuf2(1)==3) then
      method = 'neareststod'
    else
      method = 'nearestdtos'
    endif
    poleptrs = commandbuf2(2)
    if (poleptrs == -1) then 
      pole=ESMF_POLEMETHOD_ALLAVG
    else if (poleptrs == -2) then 
      pole=ESMF_POLEMETHOD_TEETH
    else if (poleptrs ==  0) then
      pole=ESMF_POLEMETHOD_NONE
    else
      pole=ESMF_POLEMETHOD_NPNTAVG 
    endif
    if (commandbuf2(3)==1) then
      srcIsRegional = .true.
    else
      srcIsRegional = .false.
    endif
    if (commandbuf2(4)==1) then
      dstIsRegional = .true.
    else
      dstIsRegional = .false.
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
      ignoreDegenerate=.true.
    else
      ignoreDegenerate=.false.
    endif

    call ESMF_VMBroadcast(vm, commandbuf1, len (commandbuf1)*size (commandbuf1), 0, rc=rc)
    if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    srcfile = commandbuf1(1)
    dstfile = commandbuf1(2)
    srcvarname = commandbuf1(3)
    dstvarname = commandbuf1(4)
  endif

  if (trim(method) .eq. 'bilinear') then
    methodflag = ESMF_REGRIDMETHOD_BILINEAR
  else if (trim(method) .eq. 'conserve') then
    methodflag = ESMF_REGRIDMETHOD_CONSERVE
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

  call ESMF_FileRegrid(srcfile, dstfile, srcvarname, dstvarname, &
       		            regridmethod=methodflag, &
                            polemethod = pole, regridPoleNPnts = poleptrs, &
			    unmappedaction = unmappedaction, &
			    ignoreDegenerate = ignoreDegenerate, &
                            srcRegionalFlag = srcIsRegional, dstRegionalFlag = dstIsRegional, &
! BOB: Not in interface??   useUserAreaFlag = userAreaFlag, &
                            verboseFlag = .true., rc = rc)

  if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

  ! Output success
  if (PetNo==0) then
    write(*,*) "Completed file regrid successfully."
    !write(*,*) "Completed file regrid in ", (endtime-starttime)*1000, "msecs"
    write(*,*) 
  endif

1111  continue
  call ESMF_Finalize()

contains

  subroutine ErrorMsgAndAbort(localPet)
    integer ::  localPet
  
    if (localPet >= 0) then
      write(*,*) "ERROR: Problem on processor ",localPet,". Please see the PET*.Regrid.Log files for a traceback."
    else
      write(*,*) "ERROR: Please see the PET*.Regrid.Log files for a traceback."
    endif
  
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  end subroutine ErrorMsgAndAbort

  subroutine PrintUsage()
    print *, "Usage: ESMF_Regrid"
    print *, "                      --source|-s src_grid_filename" 
    print *, "                	--destination|-d dst_grid_filename"
    print *, "                      --src_var src_varname"
    print *, "                      --dst_var  dst_varname"
    print *, "                      [--method|-m bilinear|patch|neareststod|nearestdtos|conserve]"
    print *, "                      [--pole|-p all|none|teeth|<N>]"
    print *, "                      [--ignore_unmapped|-i]"
    print *, "                      [--ignore_degenerate]"
    print *, "                      [-r]"
    print *, "                      [--src_regional]"
    print *, "                      [--dst_regional]"
    print *, "                      [--user_areas]"
    print *, "                      [--no_log]"
    print *, "                      [--help]"
    print *, "                      [--version]"
    print *, "                      [-V]"
    print *, "where"
    print *, "--source or -s - a required argument specifying the source grid file"
    print *, "                 name"
    print *, "--destination or -d - a required argument specifying the destination grid"
    print *, "                      file name"
    print *, "--src_var  - a required argument specifying the variable name to be regridded in"
    print *, "                 the source grid file"
    print *, "--dst_var  - a required argument specifying the destination variable name in the "
    print *, "                  destination grid file"
    print *, "--method or -m - an optional argument specifying which interpolation method is"
    print *, "                 used.  The default method is bilinear"
    print *, "--pole or -p - an optional argument indicating what to do with the pole."
    print *, "                 The default value is all"
    print *, "--ignore_unmapped or -i - ignore unmapped destination points. If not specified,"
    print *, "                          the default is to stop with an error."
    print *, "--ignore_degenerate - ignore degenerate cells in the input grids. If not specified,"
    print *, "                          the default is to stop with an error."
    print *, "-r         - an optional argument specifying the source and destination grids"
    print *, "             are regional grids.  Without this argument, the grids are assumed"
    print *, "             to be global. This argument only applies to the GRIDSPEC file"
    print *, "--src_regional   - an optional argument specifying the source grid is regional."
    print *, "             Without this argument, the src grids is assumed to be global. This "
    print *, "             argument only applies to the GRIDSPEC file"
    print *, "--dst_regional   - an optional argument specifying the destination grid is regional"
    print *, "             Without this argument, the dst grids is assumed to be global."
    print *, "             This argument only applies to the GRIDSPEC file"
    print *, "--user_areas  - an optional argument specifying that the conservation is adjusted to"
    print *, "             hold for the user areas provided in the grid files.  If not specified,"
    print *, "             then the conservation will hold for the ESMF calculated (great circle)"
    print *, "             areas."
    print *, "--no_log    - Turn off the ESMF error log."
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

end program ESMF_RegridApp
