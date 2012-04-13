!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id: ESMF_RegridWeightGen.F90,v 1.63 2012/04/13 20:51:31 peggyli Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!!-------------------------------------------------------------------------------------

program ESMF_RegridWeightGen

! !USES:
      use ESMF
      use ESMF_IOScripMod
      use ESMF_IOGridspecMod

      implicit none

      integer            :: rc
      type(ESMF_VM)      :: vm
      integer            :: PetNo, PetCnt
      type(ESMF_Mesh)    :: srcMesh, dstMesh
      type(ESMF_Grid)    :: srcGrid, dstGrid
      type(ESMF_Field)   :: srcField, dstField
      type(ESMF_Field)   :: srcFracField, dstFracField
      type(ESMF_ArraySpec) :: arrayspec
      integer(ESMF_KIND_I4), pointer:: factorIndexList(:,:)
      real(ESMF_KIND_R8), pointer :: factorList(:)
      character(len=256) :: srcfile, dstfile, wgtfile
      character(len=256) :: srcmeshname, dstmeshname
      character(len=40)  :: method, flag
      integer(ESMF_KIND_I4) :: maskvals(1)
      integer            :: index
      type(ESMF_PoleMethod_Flag) :: pole
      integer            :: poleptrs
      integer, pointer   :: srcdims(:), dstdims(:)
      integer            :: srcrank, dstrank
      logical            :: convert3D
      logical            :: isConserve, srcIsSphere, dstIsSphere
      logical            :: addCorners,convertToDual
      type(ESMF_MeshLoc) :: meshloc
      logical            :: srcIsReg, dstIsReg
      type(ESMF_FileFormat_Flag) :: srcFileType, dstFileType
      logical            :: srcIsRegional, dstIsRegional, typeSetFlag
      character(len=256) :: methodStr
      type(ESMF_RegridMethod_Flag) :: methodflag
      real(ESMF_KIND_R8), pointer :: srcArea(:)
      real(ESMF_KIND_R8), pointer :: dstArea(:)
      real(ESMF_KIND_R8), pointer :: dstFrac(:), srcFrac(:)
      character(len=256) :: commandbuf1(5)
      integer            :: commandbuf2(16)
      integer            :: regridScheme
      integer            :: i, bigFac, xpets, ypets, xpart, ypart, xdim, ydim
      logical            :: wasCompacted, largeFileFlag
      integer(ESMF_KIND_I4), pointer:: compactedFactorIndexList(:,:)
      real(ESMF_KIND_R8), pointer :: compactedFactorList(:)
      logical 		 :: ignoreUnmapped, userAreaFlag
      type(ESMF_UnmappedAction_Flag) :: unmappedaction
      logical :: srcMissingValue, dstMissingValue
      character(len=80) :: srcvarname, dstvarname

      !real(ESMF_KIND_R8) :: starttime, endtime
      !------------------------------------------------------------------------
      ! Initialize ESMF
      !
      call ESMF_Initialize (defaultCalKind=ESMF_CALKIND_GREGORIAN, &
			defaultlogfilename="RegridWeightGen.Log", &
                    	logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
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
         call ESMF_UtilGetArgIndex('--help', argindex=index)
         if (index /= -1) then
	   call PrintUsage()
	   call ESMF_Finalize(endflag=ESMF_END_ABORT)
         endif
         call ESMF_UtilGetArgIndex('--version', argindex=index)
         if (index /= -1) then
	   call PrintVersionInfo()
	   call ESMF_Finalize(endflag=ESMF_END_ABORT)
         endif
         call ESMF_UtilGetArgIndex('-s', argindex=index)
         if (index == -1) call ESMF_UtilGetArgIndex('--source', argindex=index, rc=rc)
         if (index == -1) then
           write(*,*)
           print *, 'ERROR: The required argument [-s|--source] is missing.'
           call ESMF_Finalize(endflag=ESMF_END_ABORT)
         else
           call ESMF_UtilGetArg(index+1, argvalue=srcfile)
         endif
      
         call ESMF_UtilGetArgIndex('-d', argindex=index, rc=rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--destination', argindex=index, rc=rc)
         if (index == -1) then
           write(*,*)
           print *, 'ERROR: The required argument [-w|--weight] is missing.'
           call ESMF_Finalize(endflag=ESMF_END_ABORT)
         else
           call ESMF_UtilGetArg(index+1, argvalue=dstfile)
         endif
          
         call ESMF_UtilGetArgIndex('-w', argindex=index, rc=rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--weight', argindex=index, rc=rc)
         if (index == -1) then
           write(*,*)
           print *, 'ERROR: The required argument [-w|--weight] is missing.'
           call ESMF_Finalize(endflag=ESMF_END_ABORT)
         else	
           call ESMF_UtilGetArg(index+1, argvalue=wgtfile)
         endif

         call ESMF_UtilGetArgIndex('-m', argindex=index, rc=rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--method', argindex=index, rc=rc)
         if (index == -1) then
          !  print *, 'Use default interpolation method: bilinear'
           method = 'bilinear'
         else
           call ESMF_UtilGetArg(index+1, argvalue=method)
         endif
    	
         poleptrs = -1
         call ESMF_UtilGetArgIndex('-p', argindex=index, rc=rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--pole', argindex=index, rc=rc)
         if (index == -1) then
	   if (method .eq. 'conserve') then
             ! print *, 'Use default pole: None'
              pole = ESMF_POLEMETHOD_NONE
	      poleptrs = 0
	   else
              !print *, 'Use default pole: All'
              pole = ESMF_POLEMETHOD_ALLAVG
           endif
         else
           call ESMF_UtilGetArg(index+1, argvalue=flag)
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
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif
         endif

         typeSetFlag = .false.  
         srcFileType = ESMF_FILEFORMAT_SCRIP
         dstFileType = ESMF_FILEFORMAT_SCRIP
         srcIsRegional = .false.
         dstIsRegional = .false.
         call ESMF_UtilGetArgIndex('-t', argindex=index, rc=rc)
         if (index /= -1) then
           call ESMF_UtilGetArg(index+1, argvalue=flag)
	   if (trim(flag) .eq. 'ESMF') then
	     srcFileType = ESMF_FILEFORMAT_ESMFMESH
             dstFileType = ESMF_FILEFORMAT_ESMFMESH
             !write(*,*)
             !print *, 'Set src and dst grid file types to ESMF.'
	   else if (trim(flag) .eq. 'UGRID') then
	     srcFileType = ESMF_FILEFORMAT_UGRID
             dstFileType = ESMF_FILEFORMAT_UGRID
             !write(*,*)
             !print *, 'Set src and dst grid file types to UGRID.'
           else if (trim(flag) .eq. 'GRIPSPEC') then
	     srcFileType = ESMF_FILEFORMAT_GRIDSPEC
             dstFileType = ESMF_FILEFORMAT_GRIDSPEC
           else if (trim(flag) .ne. 'SCRIP') then
             write(*,*)
	     print *, 'ERROR: Unknown -t: must be one of ESMF,SCRIP,UGRID or GRIDSPEC.'
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif 
           typeSetFlag = .true.
         endif

         call ESMF_UtilGetArgIndex('--src_type', argindex=index, rc=rc)
         if (index /= -1) then
           call ESMF_UtilGetArg(index+1, argvalue=flag)
	   if (typeSetFlag) then
	     ! check if the type is consistent with -t
             if ((trim(flag) .eq. 'ESMF' .and. srcFileType /= ESMF_FILEFORMAT_ESMFMESH) .or.   &
                (trim(flag) .eq. 'UGRID' .and. srcFileType /= ESMF_FILEFORMAT_UGRID) .or.   &
	        (trim(flag) .eq. 'GRIDSPEC' .and. srcFileType /= ESMF_FILEFORMAT_GRIDSPEC) .or. &
	        (trim(flag) .eq. 'SCRIP' .and. srcFileType /= ESMF_FILEFORMAT_SCRIP)) then
                write(*,*)
	        print *, 'ERROR: Source file type conflict: --src_type and -t.' 
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
	     end if
           endif
           if (trim(flag) .eq. 'ESMF') then
	     srcFileType = ESMF_FILEFORMAT_ESMFMESH
           else if (trim(flag) .eq. 'UGRID') then
	     srcFileType = ESMF_FILEFORMAT_UGRID
           else if (trim(flag) .eq. 'GRIDSPEC') then
	     srcFileType = ESMF_FILEFORMAT_GRIDSPEC
           else if (trim(flag) .ne. 'SCRIP') then
             write(*,*)
	     print *, 'ERROR: Unknown --src_type: must be either ESMF or SCRIP.'
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif
         endif

         call ESMF_UtilGetArgIndex('--dst_type', argindex=index, rc=rc)
         if (index /= -1) then
           call ESMF_UtilGetArg(index+1, argvalue=flag)
	   if (typeSetFlag) then
	     ! check if the type is consistent with -t
             if ((trim(flag) .eq. 'ESMF' .and. dstFileType /= ESMF_FILEFORMAT_ESMFMESH) .or.   &
                (trim(flag) .eq. 'UGRID' .and. dstFileType /= ESMF_FILEFORMAT_UGRID) .or.   &
	        (trim(flag) .eq. 'GRIDSPEC' .and. dstFileType /= ESMF_FILEFORMAT_GRIDSPEC) .or. &
	        (trim(flag) .eq. 'SCRIP' .and. dstFileType /= ESMF_FILEFORMAT_SCRIP)) then
                write(*,*)
	        print *, 'ERROR: Destination file type conflict: --dst_type and -t.' 
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
	     end if
           endif
           if (trim(flag) .eq. 'ESMF') then
	     dstFileType = ESMF_FILEFORMAT_ESMFMESH
           else if (trim(flag) .eq. 'UGRID') then
	     dstFileType = ESMF_FILEFORMAT_UGRID
           else if (trim(flag) .eq. 'GRIDSPEC') then
	     dstFileType = ESMF_FILEFORMAT_GRIDSPEC
           else if (trim(flag) .ne. 'SCRIP') then
             write(*,*)
	     print *, 'ERROR: Unknown --dst_type: must be either ESMF or SCRIP.'
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           end if
         endif

         ! If the src grid type is UGRID, get the dummy variable name in the file
	 if (srcFileType == ESMF_FILEFORMAT_UGRID) then
	    call ESMF_UtilGetArgIndex('--src_meshname', argindex=index, rc=rc)
            if (index == -1) then
	         write(*,*)
                 print *, 'ERROR: The argument --src_meshname is missing.'
                 call ESMF_Finalize(endflag=ESMF_END_ABORT)
            else
                 call ESMF_UtilGetArg(index+1, argvalue=srcMeshName)	   
            endif
         endif

         ! If the dst grid type is UGRID, get the dummy variable name in the file
	 if (dstFileType == ESMF_FILEFORMAT_UGRID) then
	    call ESMF_UtilGetArgIndex('--dst_meshname', argindex=index, rc=rc)
            if (index == -1) then
	         write(*,*)
                 print *, 'ERROR: The argument --dst_meshname is missing.'
                 call ESMF_Finalize(endflag=ESMF_END_ABORT)
            else
                 call ESMF_UtilGetArg(index+1, argvalue=dstMeshName)	   
            endif
         endif

         ! If the src grid type is GRIDSPEC, check if --src_missingvalue argument is given
         call ESMF_UtilGetArgIndex('--src_missingvalue', argindex=index, rc=rc)
         if (index == -1) then
	    srcMissingValue = .false.
         else
	     srcMissingValue = .true.
             call ESMF_UtilGetArg(index+1, argvalue=srcVarName)	   
         endif

         ! If the dst grid type is GRIDSPEC, check if --dst_missingvalue argument is given
	 call ESMF_UtilGetArgIndex('--dst_missingvalue', argindex=index, rc=rc)
         if (index == -1) then
	    dstMissingValue = .false.
         else
	     dstMissingValue = .true.
             call ESMF_UtilGetArg(index+1, argvalue=dstVarName)	   
         endif

         ignoreUnmapped=.false.
         call ESMF_UtilGetArgIndex('-i', argindex=index, rc=rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--ignore_unmapped', argindex=index, rc=rc)
         if (index /= -1) then
            ignoreUnmapped=.true.
         end if

         call ESMF_UtilGetArgIndex('-r', argindex=index, rc=rc)
         if (index /= -1) then
           srcIsRegional = .true.
           dstIsRegional = .true.
           pole = ESMF_POLEMETHOD_NONE
           poleptrs = 0
           ! print *, 'Set pole to None for regional grids.'
         end if

         call ESMF_UtilGetArgIndex('--src_regional', argindex=index, rc=rc)
         if (index /= -1) then
           srcIsRegional = .true.
           pole = ESMF_POLEMETHOD_NONE
           poleptrs = 0
           ! print *, 'Set pole to None for regional source grid.'
         end if

         call ESMF_UtilGetArgIndex('--dst_regional', argindex=index, rc=rc)
         if (index /= -1) then
           dstIsRegional = .true.
         end if

	! --64bit_offset for large weight file
 	call ESMF_UtilGetArgIndex('--64bit_offset', argindex=index, rc=rc)
	if (index /= -1) then
	   largeFileFlag = .true.
	else
	   largeFileFlag = .false.
        end if

        ! --user_area - to use user-defined area for the cells
 	call ESMF_UtilGetArgIndex('--user_area', argindex=index, rc=rc)
	if (index /= -1) then
	   userAreaFlag = .true.
	else
	   userAreaFlag = .false.
        endif

	! user area only applies to SCRIP and ESMF file formats
        if (userAreaFlag .and. (srcFileType /= ESMF_FILEFORMAT_SCRIP .and. &
 	    srcFileType /= ESMF_FILEFORMAT_ESMFMESH) .and. &
	    (dstFileType /= ESMF_FILEFORMAT_SCRIP .and. &
 	    dstFileType /= ESMF_FILEFORMAT_ESMFMESH)) then
             write(*,*)
	     print *, 'ERROR: --user_area is supported only when the source or destination'
	     print *, '       grid are in SCRIP of ESMF format.'
	     call ESMF_Finalize(endflag=ESMF_END_ABORT)
	endif

        ! Should I have only PetNO=0 to open the file and find out the size?
         if (srcFileType == ESMF_FILEFORMAT_SCRIP) then
	   call ESMF_ScripInq(srcfile, grid_rank= srcrank, grid_dims=srcdims, rc=rc)
	   if (rc /= ESMF_SUCCESS) then 
             write(*,*)
	     print *, 'ERROR: Unable to get dimension information from:', srcfile, &
                      'Please check the PET*.RegridWeightGen.Log files for the NetCDF error message.' 
	     call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif
           if (srcrank == 2) then
	     srcIsReg = .true.
           else
             srcIsReg = .false.
           endif
	 elseif (srcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
           allocate(srcdims(2))
	   call ESMF_GridspecInq(srcfile, srcrank, srcdims, rc=rc)
	   if (rc /= ESMF_SUCCESS) then 
             write(*,*)
	     print *, 'ERROR: Unable to get dimension information from:', srcfile, &
                      'Please check the PET*.RegridWeightGen.Log files for the NetCDF error message.' 
	     call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif
	   srcIsReg = .true.
           srcrank = 2
           print *, 'srcdims ', srcdims
         else
	   srcIsReg = .false.
	 endif
     	 if (dstFileType == ESMF_FILEFORMAT_SCRIP) then
	   call ESMF_ScripInq(dstfile, grid_rank=dstrank, grid_dims=dstdims, rc=rc)
           if (rc /= ESMF_SUCCESS) then
             write(*,*)
	     print *, 'ERROR: Unable to get dimension information from:', dstfile, &
                      'Please check the PET*.RegridWeightGen.Log files for the NetCDF error message' 
	     call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif
           if (dstrank == 2) then
	     dstIsReg = .true.
           else
             dstIsReg = .false.
           endif
	 elseif (dstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
	   allocate(dstdims(2))
	   call ESMF_GridspecInq(dstfile, dstrank, dstdims, rc=rc)
	   if (rc /= ESMF_SUCCESS) then 
             write(*,*)
	     print *, 'ERROR: Unable to get dimension information from:', dstfile, &
                      'Please check the PET*.RegridWeightGen.Log files for the NetCDF error message.' 
	     call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif
	   dstrank = 2
	   dstIsReg = .true.
         else
	   dstIsReg = .false.
	 endif
	print *, "Starting weight generation with these inputs: "
	print *, "  Source File: ", trim(srcfile)
	print *, "  Destination File: ", trim(dstfile)
  	print *, "  Weight File: ", trim(wgtfile)
        if (srcFileType == ESMF_FILEFORMAT_SCRIP) then 
            print *, "  Source File is in SCRIP format"
        elseif (srcFileType == ESMF_FILEFORMAT_ESMFMESH) then 
            print *, "  Source File is in ESMF format"
        elseif (srcFileType == ESMF_FILEFORMAT_UGRID) then
            print *, "  Source File is in UGRID format, dummy variable: ", &
		trim(srcMeshName)
	    if (srcMissingValue) then
	      print *, "    Use attribute 'missing_value' of variable '", trim(srcVarName),"' as the mask"
	    endif
	else 
	    print *, "  Source File is in GRIDSPEC foramt"
	    if (srcMissingValue) then
	      print *, "    Use attribute 'missing_value' of variable '", trim(srcVarName),"' as the mask"
            endif
        endif
        if (srcIsRegional) then
	   print *, "  Source Grid is a regional grid"
        else 
	   print *, "  Source Grid is a global grid"
	endif
	if (srcIsReg)   then
	   print *, "  Source Grid is a logically rectangular grid"
        else
	   print *, "  Source Grid is an unstructured grid"
        endif
        if (dstFileType == ESMF_FILEFORMAT_SCRIP) then 
            print *, "  Destination File is in SCRIP format"
        elseif (dstFileType == ESMF_FILEFORMAT_ESMFMESH) then 
            print *, "  Destination File is in ESMF format"
        elseif (srcFileType == ESMF_FILEFORMAT_UGRID) then
            print *, "  Destination File is in UGRID format, dummy variable: ", & 
		trim(dstMeshName)
	    if (dstMissingValue) then
	      print *, "  Use the missing value of ", trim(dstVarName)," as the mask"
            endif	
        else
	    print *, "  Destination File is in GRIDSPEC format"
	    if (dstMissingValue) then
	      print *, "  Use the missing value of ", trim(dstVarName)," as the mask"
            endif	
	endif
        if (dstIsRegional) then
	   print *, "  Destination Grid is a regional grid"
        else 
	   print *, "  Destination Grid is a global grid"
	endif
        if (dstIsReg)   then
	   print *, "  Destination Grid is a logically rectangular grid"
        else
	   print *, "  Destination Grid is an unstructured grid"
        endif
        print *, "  Regrid Method: ", method
        if (pole .eq. ESMF_POLEMETHOD_NONE) then
	       print *, "  Pole option: NONE"
	elseif (pole .eq. ESMF_POLEMETHOD_ALLAVG) then
	       print *, "  Pole option: ALL"
	elseif (pole .eq. ESMF_POLEMETHOD_TEETH) then
	       print *, "  Pole option: TEETH"
	else
	       print *, "  Pole option: ", poleptrs
        endif
        if (ignoreUnmapped) then
	       print *, "  Ignore unmapped destination points"
        endif
	if (largeFileFlag) then
	       print *, "  Output weight file in 64bit offset NetCDF file format"
        endif
	if (userAreaFlag) then
		print *, "  Use user defined cell area for both the source and destination grids"
        endif
        write(*,*)

        ! Group the command line arguments and broadcast to other PETs
        commandbuf1(1)=srcfile
        commandbuf1(2)=dstfile
        commandbuf1(3)=wgtfile
        commandbuf1(4)=srcMeshName
	commandbuf1(5)=dstMeshName

        ! Broadcast the command line arguments to all the PETs
        call ESMF_VMBroadcast(vm, commandbuf1, 256*5, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

        commandbuf2(:)=0
        commandbuf2(1)=srcFileType%fileformat
        if (srcIsReg)   commandbuf2(2)=1
        commandbuf2(3)=dstFileType%fileformat
        if (dstIsReg)   commandbuf2(4)=1
        if (method .eq. 'patch') commandbuf2(5)=1
        if (method .eq. 'conserve') commandbuf2(5)=2
        commandbuf2(6)=poleptrs
        if (srcIsRegional) commandbuf2(7)=1
        if (dstIsRegional) commandbuf2(8)=1
	if (srcFileType == ESMF_FILEFORMAT_SCRIP .or. &
	    srcFileType == ESMF_FILEFORMAT_GRIDSPEC) then 
           commandbuf2(9)=srcrank
           if (srcrank == 1) then
              commandbuf2(10) = srcdims(1)
              commandbuf2(11) = -1
           else 
              commandbuf2(10) = srcdims(1)
              commandbuf2(11) = srcdims(2)
           endif
        endif
        if (dstFileType == ESMF_FILEFORMAT_SCRIP .or. &
	    dstFileType == ESMF_FILEFORMAT_GRIDSPEC) then 
           commandbuf2(12)=dstrank
           if (dstrank == 1) then
              commandbuf2(13) = dstdims(1)
              commandbuf2(14) = -1
           else 
              commandbuf2(13) = dstdims(1)
              commandbuf2(14) = dstdims(2)
           endif
        endif
        if (ignoreUnmapped) commandbuf2(15) = 1
	if (userAreaFlag)   commandbuf2(16) = 1

        call ESMF_VMBroadcast(vm, commandbuf2, 16, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
     else
        call ESMF_VMBroadcast(vm, commandbuf1, 256*5, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        srcfile = commandbuf1(1)
        dstfile = commandbuf1(2)
        wgtfile = commandbuf1(3)
        srcMeshName = commandbuf1(4)
	dstMeshName = commandbuf1(5)

        call ESMF_VMBroadcast(vm, commandbuf2, 16, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	srcFileType%fileformat = commandbuf2(1)
        if (commandbuf2(2)==1) then
           srcIsReg = .true.
        else
           srcIsReg = .false.
        end if
	dstFileType%fileformat = commandbuf2(3)
        if (commandbuf2(4)==1) then
           dstIsReg = .true.
        else
           dstIsReg = .false.
        end if
        if (commandbuf2(5)==0) then
	   method = 'bilinear'
        else if (commandbuf2(5)==1) then
           method = 'patch'
        else
           method = 'conserve'
        end if
        poleptrs = commandbuf2(6)
        if (poleptrs == -1) then 
	   pole=ESMF_POLEMETHOD_ALLAVG
        else if (poleptrs == -2) then 
	   pole=ESMF_POLEMETHOD_TEETH
        else if (poleptrs ==  0) then
           pole=ESMF_POLEMETHOD_NONE
	else
           pole=ESMF_POLEMETHOD_NPNTAVG 
        endif
        if (commandbuf2(7)==1) then
           srcIsRegional = .true.
        else
           srcIsRegional = .false.
        end if
        if (commandbuf2(8)==1) then
           dstIsRegional = .true.
        else
           dstIsRegional = .false.
        end if
        allocate(srcdims(2), dstdims(2))
        if (srcIsReg) then
           srcrank = commandbuf2(9)
           srcdims(1)=commandbuf2(10)
           srcdims(2)=commandbuf2(11)
        endif
        if (dstIsReg) then
           dstrank = commandbuf2(12)
           dstdims(1)=commandbuf2(13)
           dstdims(2)=commandbuf2(14)
        endif

        if (commandbuf2(15) == 1) then
           ignoreUnmapped=.true.
        else
           ignoreUnmapped=.false.
        endif

        if (commandbuf2(16) == 1) then
           userAreaFlag=.true.
        else
           userAreaFlag=.false.
        endif
     endif

     ! Set flag to say if we're conservative
     isConserve=.false.
     if (trim(method) .eq. 'conserve') then
        isConserve=.true.
     endif

     ! Set some varibles for File I/O needed by conservative
     addCorners=.false.
     convertToDual=.true.
     meshloc=ESMF_MESHLOC_NODE
     if (isConserve) then
        addCorners=.true.
        convertToDual=.false.
        meshloc=ESMF_MESHLOC_ELEMENT
     endif

     if (srcIsRegional .and. dstIsRegional) then
        regridScheme = ESMF_REGRID_SCHEME_REGION3D
        srcIsSphere=.false.
        dstIsSphere=.false.
     elseif (srcIsRegional) then
        regridScheme = ESMF_REGRID_SCHEME_REGTOFULL3D
        srcIsSphere=.false.
        dstIsSphere=.true.
     elseif (dstIsRegional) then  
        regridScheme = ESMF_REGRID_SCHEME_FULLTOREG3D
        srcIsSphere=.true.
        dstIsSphere=.false.
     else
        regridScheme = ESMF_REGRID_SCHEME_FULL3D
        srcIsSphere=.true.
        dstIsSphere=.true.
     endif

     ! Set unmapped flag
     unmappedaction=ESMF_UNMAPPEDACTION_ERROR
     if (ignoreUnmapped) then
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE
     endif

     ! Create a decomposition such that each PET will contain at least 2 column and 2 row of data
     ! otherwise, regrid will not work
     if (PetCnt == 1) then
	xpart = 1
	ypart = 1
     else
     	bigFac = 1
     	do i=2, int(sqrt(float(PetCnt)))
	   if ((PetCnt/i)*i == PetCnt) then
	      bigFac = i
           endif
        enddo
	xpets = bigFac
	ypets = PetCnt/xpets
        if (srcIsReg) then
	   if ((srcdims(1) <= srcdims(2) .and. xpets <= ypets) .or. &
	       (srcdims(1) > srcdims(2) .and. xpets > ypets)) then
	       xpart = xpets
	       ypart = ypets
	   else 
	       xpart = ypets
	       ypart = xpets
	   endif
	   xdim = srcdims(1)/xpart
	   ydim = srcdims(2)/ypart
	   do while (xdim <= 1 .and. xpart>1)
	      xpart = xpart-1
   	      xdim = srcdims(1)/xpart
           enddo
	   do while (ydim <= 1 .and. ypart>1) 
	      ypart = ypart-1
   	      ydim = srcdims(2)/ypart
           enddo
        endif
     endif

     !Read in the srcfile and create the corresponding ESMF object (either
     ! ESMF_Grid or ESMF_Mesh

     if (srcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
	if (srcMissingValue) then
	   srcGrid = ESMF_GridCreate(srcfile, srcFileType, (/xpart,ypart/), &
		  addCornerStagger=addCorners, &
		  addMask=.true., varname=trim(srcVarName), isSphere=srcIsSphere, rc=rc)
        else
           srcGrid = ESMF_GridCreate(srcfile, srcFileType, (/xpart,ypart/), & 
		       addCornerStagger=addCorners, &
                       isSphere=srcIsSphere, rc=rc)
	endif
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    	srcField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
     elseif (srcFileType == ESMF_FILEFORMAT_SCRIP) then
	if(srcIsReg) then
           srcGrid = ESMF_GridCreate(srcfile, srcFileType, (/xpart,ypart/), &
			addCornerStagger=addCorners, &
                        isSphere=srcIsSphere, addUserArea =userAreaFlag, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

          ! call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CORNER, filename="srcGrid", isSphere=.false., &
          !       isLatLonDeg=.true., rc=rc)
          ! if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    	   srcField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	else
           srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_SCRIP, convert3D=.true., &
                       convertToDual=convertToDual, addUserArea=userAreaFlag, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
           ! call ESMF_MeshWrite(srcMesh, "srcMesh", rc)
           call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
           srcField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=meshloc,rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	endif
      else
	! if srcfile is not SCRIP, it is always unstructured
	if (srcMissingValue) then
	   srcMesh = ESMF_MeshCreate(srcfile, srcFileType, convert3D=.true., &
                    meshname = trim(srcMeshName), addMask=.true., &
		    varname=trim(srcVarName), rc=rc)
	else
	   srcMesh = ESMF_MeshCreate(srcfile, srcFileType, convert3D=.true., &
                    meshname = trim(srcMeshName), rc=rc)
	endif
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
        srcField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=meshloc,rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
      endif

     !Read in the dstfile and create the corresponding ESMF object (either
     ! ESMF_Grid or ESMF_Mesh)
     if (PetCnt == 1) then
	xpart = 1
	ypart = 1
     else
        if (dstIsReg) then
	   if ((dstdims(1) <= dstdims(2) .and. xpets <= ypets) .or. &
	       (dstdims(1) > dstdims(2) .and. xpets > ypets)) then
	       xpart = xpets
	       ypart = ypets
	   else 
	       xpart = ypets
	       ypart = xpets
	   endif
	   xdim = dstdims(1)/xpart
	   ydim = dstdims(2)/ypart
	   do while (xdim <= 1 .and. xpart>1)
	     xpart = xpart-1
   	     xdim = dstdims(1)/xpart
           enddo
	   do while (ydim <= 1 .and. ypart>1) 
	     ypart = ypart-1
   	     ydim = dstdims(2)/ypart
           enddo
        endif
     endif
     if (dstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
	if (dstMissingValue) then
	   dstGrid = ESMF_GridCreate(dstfile, dstFileType, (/xpart,ypart/), &
		  addCornerStagger=addCorners, &
		  addMask=.true., varname=trim(dstVarName), isSphere=dstIsSphere, rc=rc)
        else
           dstGrid = ESMF_GridCreate(dstfile, dstFileType, (/xpart,ypart/), &
			addCornerStagger=addCorners, &
                        isSphere=dstIsSphere, rc=rc)
	endif
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    	dstField = ESMF_FieldCreate(dstGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
     elseif (dstFileType == ESMF_FILEFORMAT_SCRIP) then
	if(dstIsReg) then
           dstGrid = ESMF_GridCreate(dstfile, dstFileType,(/xpart, ypart/), &
			addCornerStagger=addCorners, &
                        isSphere=dstIsSphere, addUserArea = userAreaFlag, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    	   dstField = ESMF_FieldCreate(dstGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	else
           dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, convert3D=.true., &
                       convertToDual=convertToDual, addUserArea=userAreaFlag, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
           call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
           dstField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=meshloc,rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	endif
      else
	! if dstfile is not SCRIP, it is always unstructured
	if (dstMissingValue) then
 	   dstMesh = ESMF_MeshCreate(dstfile, dstFileType, convert3D=.true., &
                    meshname = trim(dstMeshName), addMask=.true., &
		    varname=trim(dstVarName), rc=rc)
	else
	   dstMesh = ESMF_MeshCreate(dstfile, dstFileType, convert3D=.true., &
                    meshname = trim(dstMeshName), rc=rc)
        endif
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        dstField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=meshloc,rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
      endif

      ! Create Frac Fields if conservative
      if (isConserve) then
         if (srcIsReg) then
            call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         else
            call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            srcFracField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=meshloc,rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         endif

         if (dstIsReg) then
            call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         else
            call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            dstFracField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=meshloc,rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         endif
      endif

      !call ESMF_VMBarrier(vm)
      !call ESMF_VMWtime(starttime, rc=rc)
      maskvals(1) = 0
      if (poleptrs <= 0) poleptrs = 1

      if (trim(method) .eq. 'bilinear') then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedaction=unmappedaction, &
	    factorIndexList=factorIndexList, factorList=factorList, &
            regridmethod = ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod = pole, regridPoleNPnts = poleptrs, &
	    rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            methodStr = "Bilinear remapping"
	    methodflag = ESMF_REGRIDMETHOD_BILINEAR
      else if (trim(method) .eq. 'patch') then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedaction=unmappedaction, &
	    factorIndexList=factorIndexList, factorList=factorList, &
            regridmethod = ESMF_REGRIDMETHOD_PATCH, &
            polemethod = pole, regridPoleNPnts = poleptrs, &
	    rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Patch
	    methodflag = ESMF_REGRIDMETHOD_PATCH
      else if (trim(method) .eq. 'conserve') then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedaction=unmappedaction, &
	    factorIndexList=factorIndexList, factorList=factorList, &
            srcFracField=srcFracField, dstFracField=dstFracField, &
            regridmethod = ESMF_REGRIDMETHOD_CONSERVE, &
            polemethod = pole, regridPoleNPnts = poleptrs, &
	    rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            methodStr = "Conservative remapping"
	    methodflag = ESMF_REGRIDMETHOD_CONSERVE
      else ! nothing recognizable so report error
	     print *, 'ERROR: The -method is not a recognized interpolation method.'
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif

      ! Compute areas if conservative
      ! Area only valid on PET 0 right now, when parallel Array
      ! write works, then make area io parallel
      if (isConserve) then
         if (srcIsReg) then
            call computeAreaGrid(srcGrid, PetNo, srcArea, regridScheme, rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         else
            call computeAreaMesh(srcMesh, vm, petNo, petCnt, srcArea, rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            call ESMF_MeshMergeSplitSrcInd(srcMesh,factorIndexList,rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         endif

         if (dstIsReg) then
            call computeAreaGrid(dstGrid, PetNo, dstArea, regridScheme, rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         else
            call computeAreaMesh(dstMesh, vm, petNo, petCnt, dstArea, rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            call ESMF_MeshMergeSplitDstInd(dstMesh,factorList,factorIndexList,rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         endif
      endif

      ! Compact weight matrix
      ! (only compact if one of the grids is irregular, because that's when the repeated entries occur)
      if ((.not. srcIsReg) .or. (.not. dstIsReg)) then
         call compactMatrix(factorList, factorIndexList, &
                            wasCompacted, &
                            compactedFactorList, compactedFactorIndexList, &
                            rc)
         if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

         ! If the list was compacted get rid of the old lists and 
         ! point to the new lists
         if (wasCompacted) then
            deallocate(factorList)
            factorList=>compactedFactorList
            deallocate(factorIndexList)
            factorIndexList=>compactedFactorIndexList
         endif
      endif

      ! Computer fraction if bilinear
      ! src fraction is always 0
      ! destination fraction depends on the src mask, dst mask, and the weight
      if (method .eq. 'bilinear' .or. method .eq. 'patch') then
	if (dstIsReg) then
	   call computeFracGrid(dstGrid, vm, factorIndexList, dstFrac, rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        else
	   call computeFracMesh(dstMesh, vm, factorIndexList, dstFrac, rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        endif
      else if (method .eq. 'conserve') then
         if (srcIsReg) then
            call gatherFracFieldGrid(srcGrid, srcFracField, petNo, srcFrac, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         else
            call gatherFracFieldMesh(srcMesh, vm, srcFracField, petNo, petCnt, &
                 srcFrac, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         endif

         if (dstIsReg) then
            call gatherFracFieldGrid(dstGrid, dstFracField, petNo, dstFrac, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         else
            call gatherFracFieldMesh(dstMesh, vm, dstFracField, petNo, petCnt, &
                 dstFrac, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         endif
      endif

      !! Write the weight table into a SCRIP format NetCDF file
      if (PetNo == 0) then
         if (isConserve) then
            call ESMF_OutputScripWeightFile(wgtfile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=srcFileType,&
	           dstFileType=dstFileType, method = methodflag, &
                   srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
		   dstFrac=dstFrac, largeFileFlag=largeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcvarname, dstvarname=dstvarname, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         else
            call ESMF_OutputScripWeightFile(wgtfile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=srcFileType,&
	           dstFileType=dstFileType, method = methodflag, dstFrac=dstFrac, &
		   largeFileFlag=largeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcvarname, dstvarname=dstvarname, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	  endif
      else 
	 call ESMF_OutputScripWeightFile(wgtfile, factorList, factorIndexList, rc=rc)
         if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
      endif

      !call ESMF_VMBarrier(vm)
      !call ESMF_VMWtime(endtime, rc=rc)

      ! Get rid of conservative arrays
      if (isConserve) then
         if (PetNo == 0) then
	    deallocate(srcArea)
	    deallocate(dstArea)
         endif
      endif

      ! Output success
      if (PetNo==0) then
         write(*,*) "Completed weight generation successfully."
!         write(*,*) "Completed weight generation in ", (endtime-starttime)*1000, "msecs"
         write(*,*) 
      endif

      call ESMF_Finalize()
      stop

contains


! For now just put into a 1D array on PET 0. When PIO array write is done, then
! do it in parallel
! AREA ONLY VALID ON PET 0
subroutine computeAreaGrid(grid, petNo, area, regridScheme, rc)
  type(ESMF_Grid) :: grid
  integer :: petNo
  real (ESMF_KIND_R8), pointer :: area(:)
  integer :: regridScheme
  integer :: rc

  type(ESMF_Field) :: areaField
  type(ESMF_ArraySpec) ::arrayspec
  integer :: minIndex(2), maxIndex(2), gridDims(2)
  real (ESMF_KIND_R8), pointer :: area2D(:,:)
  integer :: localrc

  ! Setup Arrayspec
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! Create a field on the grid to hold the areas
  areaField=ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, name="area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! compute areas
  call ESMF_FieldRegridGetArea(areaField, rc=localrc)
 if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif


  ! Get size of Grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
	minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
  endif

  ! Grid size
  gridDims(1)=maxIndex(1)-minIndex(1)+1
  gridDims(2)=maxIndex(2)-minIndex(2)+1


     ! Allocate memory for area
     allocate(area2D(gridDims(1),gridDims(2)))

     ! Get area onto PET 0
     call ESMF_FieldGather(areaField, farray=area2D, rootPet=0, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=localrc
        return
     endif
 
     ! Only do this part on PET 0
     if (petNo .eq. 0) then

        ! Allocate memory for area
        allocate(area(gridDims(1)*gridDims(2)))

        ! flatten area
        area=RESHAPE(area2D,(/gridDims(1)*gridDims(2)/))

     endif

     ! deallocate memory for 2D area
     deallocate(area2D)

end subroutine computeAreaGrid


! For now just put into a 1D array on PET 0. When PIO array write is done, then
! do it in parallel
! AREA ONLY VALID ON PET 0
subroutine computeAreaMesh(mesh, vm, petNo, petCnt, area, rc)
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: VM
  integer :: petNo,petCnt
  real (ESMF_KIND_R8), pointer :: area(:)
  integer :: rc
  real (ESMF_KIND_R8), pointer :: localArea(:)
  integer :: localrc
  integer :: localElemCount,i
  integer (ESMF_KIND_I4) :: localCount(1)
  integer (ESMF_KIND_I4),pointer :: globalCount(:),globalDispl(:)
  integer :: totalCount
  logical :: hasSplitElem
 
  ! Find out if elements are split
  call ESMF_MeshGetElemSplit(mesh, hasSplitElem=hasSplitElem, rc=localrc)  
  if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
  endif

  ! Get area depending on split elements
  if (hasSplitElem) then 
     ! Get local size of mesh areas before split
     call ESMF_MeshGetElemSplit(mesh, origElemCount=localElemCount, &
            rc=localrc)  
    if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
    endif

    ! allocate space for areas
    allocate(localArea(localElemCount))

    ! Get local Areas
    call ESMF_MeshGetOrigElemArea(mesh, areaList=localArea, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
     endif
  else 
     ! Get local size of mesh areas
     call ESMF_MeshGet(mesh, numOwnedElements=localElemCount, &
            rc=localrc)  
    if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
    endif

    ! allocate space for areas
    allocate(localArea(localElemCount))

    ! Get local Areas
    call ESMF_MeshGetElemArea(mesh, areaList=localArea, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
     endif
  endif

  ! Allocate List of counts
  allocate(globalCount(petCnt))

  ! Get List of counts
  localCount(1)=localElemCount
  call ESMF_VMGather(vm,localCount,globalCount,count=1,rootPet=0,rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
  endif

  ! Calculate Displacements
  allocate(globalDispl(petCnt))
  if (petNo==0) then
     globalDispl(1)=0
     do i=2,petCnt
        globalDispl(i)=globalDispl(i-1)+globalCount(i-1)
     enddo
  else
    globalDispl=0
  endif


  ! Sum size
  if (petNo==0) then
    totalCount=0
    do i=1,petCnt
       totalCount=totalCount+globalCount(i)
    enddo
  else 
    totalCount=1 ! Because I'm not sure what happens
                 ! if array is not allocated in VM
  endif

  ! Allocate final area list
  allocate(area(totalCount))

  ! Gather all areas
  call ESMF_VMGatherV(vm,sendData=localArea, sendCount=localElemCount,&
         recvData=area,recvCounts=globalCount,recvOffsets=globalDispl,&
         rootPet=0, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
  endif  

  ! Get rid of helper variables
  deallocate(localArea) 
  deallocate(globalCount)
  deallocate(globalDispl)
  if (petNo .ne. 0) deallocate(area)

end subroutine computeAreaMesh

subroutine computeFracGrid(grid, vm, indices, frac, rc)
  type(ESMF_Grid) :: grid
  type(ESMF_VM) :: vm
  integer :: indices(:,:)
  real(ESMF_KIND_R8), pointer :: frac(:)
  integer :: rc

  type (ESMF_DistGrid) :: distgrid
  integer (ESMF_KIND_I4) :: localCount(1), elementCount(1)
  integer (ESMF_KIND_I4),pointer :: globalCount(:),globalDispl(:)
  integer (ESMF_KIND_I4),pointer :: buffer(:),buffer1(:)
  integer :: totalCount
  integer :: i, j, total 
  integer :: petNo,petCnt
  integer :: saved, count

  call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)

  ! Allocate List of counts
  allocate(globalCount(petCnt))

  call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
  if (rc /=ESMF_SUCCESS) then
      return
  endif

  call ESMF_DistGridGet(distgrid, elementCountPTile=elementCount, rc=rc)
  total = size(indices,2)
  ! find unique indices in the destination column: indices(2,:)
  count = 0
  saved = 0
  do i=1,total
    if (indices(2,i) /= saved) then
	count = count+1
        saved = indices(2,i)
    endif
  enddo
  allocate(buffer(count))
  saved = 0
  j=1
  do i=1,total
   if (indices(2,i) /= saved) then
     buffer(j)=indices(2,i)
     j=j+1
     saved = indices(2,i)
   endif
  enddo

  ! Get List of counts
  localCount(1)=count
  call ESMF_VMGather(vm,localCount,globalCount,count=1,rootPet=0,rc=rc)
  if (rc /=ESMF_SUCCESS) then
      return
  endif
 
! Calculate Displacements
  allocate(globalDispl(petCnt))
  if (petNo==0) then
     globalDispl(1)=0
     do i=2,petCnt
        globalDispl(i)=globalDispl(i-1)+globalCount(i-1)
     enddo
  else
    globalDispl=0
  endif

  ! Sum size
  if (petNo==0) then
    totalCount=0
    do i=1,petCnt
       totalCount=totalCount+globalCount(i)
    enddo
  else 
    totalCount=1 ! Because I'm not sure what happens
                 ! if array is not allocated in VM
  endif


  ! Allocate final area list
  allocate(buffer1(totalCount))

  ! Gather all areas
  call ESMF_VMGatherV(vm,sendData=buffer, sendCount=localCount(1),&
         recvData=buffer1,recvCounts=globalCount,recvOffsets=globalDispl,&
         rootPet=0, rc=rc)
  if (rc /=ESMF_SUCCESS) then
      return
  endif  

  if (PetNo==0) then
    allocate(frac(elementCount(1)))
    frac = 0
    do i=1,totalCount
       frac(buffer1(i))=1
    enddo
  endif

  ! Get rid of helper variables
  deallocate(buffer, buffer1) 
  deallocate(globalCount)
  deallocate(globalDispl)

end subroutine computeFracGrid

subroutine computeFracMesh(mesh, vm, indices, frac, rc)
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: vm
  integer :: indices(:,:)
  real(ESMF_KIND_R8), pointer :: frac(:)
  integer :: rc

  type (ESMF_DistGrid) :: distgrid
  integer (ESMF_KIND_I4) :: localCount(1), elementCount(1)
  integer (ESMF_KIND_I4),pointer :: globalCount(:),globalDispl(:)
  integer (ESMF_KIND_I4),pointer :: buffer(:), buffer1(:)
  integer :: totalCount
  integer :: i, j, total 
  integer :: petNo,petCnt
  integer :: count, saved

  call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)

  ! Allocate List of counts
  allocate(globalCount(petCnt))

  call ESMF_MeshGet(mesh, nodalDistgrid=distgrid, rc=rc)
  if (rc /=ESMF_SUCCESS) then
      return
  endif

  call ESMF_DistGridGet(distgrid, elementCountPTile=elementCount, rc=rc)
  total = size(indices,2)
  ! find unique indices in the destination column: indices(2,:)
  count = 0
  saved = 0
  do i=1,total
    if (indices(2,i) /= saved) then
	count = count+1
        saved = indices(2,i)
    endif
  enddo
  allocate(buffer(count))
  saved = 0
  j=1
  do i=1,total
   if (indices(2,i) /= saved) then
     buffer(j)=indices(2,i)
     j=j+1
     saved = indices(2,i)
   endif
  enddo
  
  ! Get List of counts
  localCount(1)=count
  call ESMF_VMGather(vm,localCount,globalCount,count=1,rootPet=0,rc=rc)
  if (rc /=ESMF_SUCCESS) then
      return
  endif
 
! Calculate Displacements
  allocate(globalDispl(petCnt))
  if (petNo==0) then
     globalDispl(1)=0
     do i=2,petCnt
        globalDispl(i)=globalDispl(i-1)+globalCount(i-1)
     enddo
  else
    globalDispl=0
  endif


  ! Sum size
  if (petNo==0) then
    totalCount=0
    do i=1,petCnt
       totalCount=totalCount+globalCount(i)
    enddo
  else 
    totalCount=1 ! Because I'm not sure what happens
                 ! if array is not allocated in VM
  endif


  ! Allocate final area list
  allocate(buffer1(totalCount))

  ! Gather all areas
  call ESMF_VMGatherV(vm,sendData=buffer, sendCount=localCount(1),&
         recvData=buffer1,recvCounts=globalCount,recvOffsets=globalDispl,&
         rootPet=0, rc=rc)
  if (rc /=ESMF_SUCCESS) then
      return
  endif  

  if (PetNo==0) then
    allocate(frac(elementCount(1)))
    frac = 0
    do i=1,totalCount
	frac(buffer1(i))=1
    enddo
  endif

  ! Get rid of helper variables
  deallocate(buffer, buffer1) 
  deallocate(globalCount)
  deallocate(globalDispl)

end subroutine computeFracMesh

subroutine gatherFracFieldGrid(grid, fracField, petNo, frac, rc)
  type(ESMF_Grid) :: grid
  type(ESMF_Field) :: fracField
  integer :: petNo
  real (ESMF_KIND_R8), pointer :: frac(:)
  integer :: rc
  integer :: minIndex(2), maxIndex(2), gridDims(2)
  real (ESMF_KIND_R8), pointer :: frac2D(:,:)
  integer :: localrc


  ! Get size of Grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER, minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
  endif

  ! Grid size
  gridDims(1)=maxIndex(1)-minIndex(1)+1
  gridDims(2)=maxIndex(2)-minIndex(2)+1


     ! Allocate memory for area
     allocate(frac2D(gridDims(1),gridDims(2)))

     ! Get area onto PET 0
     call ESMF_FieldGather(fracField, farray=frac2D, rootPet=0, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=localrc
        return
     endif
 
     ! Only do this part on PET 0
     if (petNo .eq. 0) then

        ! Allocate memory for area
        allocate(frac(gridDims(1)*gridDims(2)))

        ! flatten area
        frac=RESHAPE(frac2D,(/gridDims(1)*gridDims(2)/))

     endif

     ! deallocate memory for 2D area
     deallocate(frac2D)

end subroutine gatherFracFieldGrid


! For now just put into a 1D array on PET 0. When PIO array write is done, then
! do it in parallel
! AREA ONLY VALID ON PET 0
subroutine gatherFracFieldMesh(mesh, vm, fracField, petNo, petCnt, frac, rc)
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: VM
  integer :: petNo,petCnt
  type(ESMF_Field) :: fracField
  real (ESMF_KIND_R8), pointer :: frac(:)
  integer :: rc
  real (ESMF_KIND_R8), pointer :: localFrac(:)
  real (ESMF_KIND_R8), pointer :: mergedFrac(:)
  integer :: localrc
  integer :: localElemCount,i
  integer (ESMF_KIND_I4) :: localCount(1)
  integer (ESMF_KIND_I4),pointer :: globalCount(:),globalDispl(:)
  integer :: totalCount
  logical :: hasSplitElem
  
  ! Get localFrac from field
  call ESMF_FieldGet(fracField, localDE=0, farrayPtr=localFrac,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! Find out if elements are split
  call ESMF_MeshGetElemSplit(mesh, hasSplitElem=hasSplitElem, rc=localrc)  
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! Get merge frac field depending on if split elements
  if (hasSplitElem) then 
     ! Get local size of mesh areas before split
     call ESMF_MeshGetElemSplit(mesh, origElemCount=localElemCount, &
          rc=localrc)  
     if (localrc /=ESMF_SUCCESS) then
        rc=localrc
        return
     endif

     ! allocate space for frac
     allocate(mergedFrac(localElemCount))

     ! Get local Areas
     call ESMF_MeshGetOrigElemFrac(mesh, splitFracList=localFrac, &
          origfracList=mergedFrac, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=localrc
        return
     endif

     ! switch to point to merged areas
     localFrac=>mergedFrac
  else 
     localElemCount=size(localFrac)
     ! localFrac is gotten from the fracField above
  endif


  ! Allocate List of counts
  allocate(globalCount(petCnt))

  ! Get List of counts
  localCount(1)=localElemCount
  call ESMF_VMGather(vm,localCount,globalCount,count=1,rootPet=0,rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! Calculate Displacements
  allocate(globalDispl(petCnt))
  if (petNo==0) then
     globalDispl(1)=0
     do i=2,petCnt
        globalDispl(i)=globalDispl(i-1)+globalCount(i-1)
     enddo
  else
     globalDispl=0
  endif


  ! Sum size
  if (petNo==0) then
     totalCount=0
     do i=1,petCnt
        totalCount=totalCount+globalCount(i)
     enddo
  else 
     totalCount=1 ! Because I'm not sure what happens
     ! if array is not allocated in VM
  endif

  ! Allocate final area list
  allocate(frac(totalCount))

  ! Gather all areas
  call ESMF_VMGatherV(vm,sendData=localFrac, sendCount=localElemCount,&
       recvData=frac,recvCounts=globalCount,recvOffsets=globalDispl,&
       rootPet=0, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! Get rid of helper variables
  if (hasSplitElem) then 
     deallocate(mergedFrac) 
  endif
  deallocate(globalCount)
  deallocate(globalDispl)
  if (petNo .ne. 0) deallocate(frac)

end subroutine gatherFracFieldMesh


! Compact the weight matrix getting rid of duplicate entries with the same row and column
! return the compacted matrix in outFactorList, outFactorIndexList
subroutine compactMatrix(inFactorList, inFactorIndexList, &
                         wasCompacted, &
                         outFactorList, outFactorIndexList, &
                         rc)
    real(ESMF_KIND_R8), intent(inout)               :: inFactorList(:) 
    integer(ESMF_KIND_I4),intent(inout)             :: inFactorIndexList(:,:)
    logical, intent(out)                            :: wasCompacted
    real(ESMF_KIND_R8), pointer                     :: outFactorList(:) 
    integer(ESMF_KIND_I4),pointer                   :: outFactorIndexList(:,:)
    integer, intent(out)                            :: rc
    integer  :: localrc      ! local return code
    integer  :: inListCount, outListCount
    integer  :: i, srcInd, dstInd
    integer  :: outListPos
    real(ESMF_KIND_R8) :: factorSum
    integer  :: beg


    ! Get size of list
    inListCount=size(inFactorIndexList,2)
 
    ! if too small to need compacting (e.g. <2) return
    if (inListCount .lt. 2) then
       wasCompacted=.false.
       return
    endif

   ! Put source indices for each run of destination
   ! indices in sorted order to allow weights with 
   ! the same indices to be merged below. Note
   ! runs with less than 3 entries are not sorted because
   ! they will be handled correctly by the merge code below. 
    beg=1
    dstInd=inFactorIndexList(2,1)
    do i=2,inListCount
       if (inFactorIndexList(2,i) .ne. dstInd) then
          ! Sort [beg,i-1] if there could be repeats 
          if ((i-1)-beg+1 >2) then
             call hsort_array(inFactorIndexList(:,beg:i-1), inFactorList(beg:i-1))
          endif

          ! Reset
          beg=i
          dstInd=inFactorIndexList(2,i)
       endif
    enddo

    ! If long enough sort [beg,inListCount], because it's not handled above 
    if ((inListCount)-beg+1 >2) then
       call hsort_array(inFactorIndexList(:,beg:inListCount), inFactorList(beg:inListCount))
    endif


    ! Loop counting unique entries
    outListCount=1 ! 1 because counting switches below
    srcInd=inFactorIndexList(1,1)
    dstInd=inFactorIndexList(2,1)
    do i=2,inListCount
       if ((srcInd /= inFactorIndexList(1,i)) .or. &
           (dstInd /= inFactorIndexList(2,i))) then
          srcInd=inFactorIndexList(1,i)
          dstInd=inFactorIndexList(2,i)
          outListCount=outListCount+1
       endif
    enddo

    ! if all unique then don't compact
    if (inListCount .eq. outListCount) then
       wasCompacted=.false.
       return
    endif


    ! Allocate new lists
    allocate(outFactorList(outListCount)) 
    allocate(outFactorIndexList(2,outListCount))

    ! Loop counting unique entries
    outListPos=1 
    srcInd=inFactorIndexList(1,1)
    dstInd=inFactorIndexList(2,1)
    factorSum=inFactorList(1)
    do i=2,inListCount
       if ((srcInd /= inFactorIndexList(1,i)) .or. &
           (dstInd /= inFactorIndexList(2,i))) then
          ! Save the old entry
          outFactorIndexList(1,outListPos)=srcInd
          outFactorIndexList(2,outListPos)=dstInd
          outFactorList(outListPos)=factorSum
          
          ! Change to a new entry
          srcInd=inFactorIndexList(1,i)
          dstInd=inFactorIndexList(2,i)
          factorSum=inFactorList(i)

          outListPos=outListPos+1
       else
          factorSum=factorSum+inFactorList(i)
       endif
    enddo

    ! Save the last entry
    outFactorIndexList(1,outListPos)=srcInd
    outFactorIndexList(2,outListPos)=dstInd
    outFactorList(outListPos)=factorSum


    ! Output that the lists were compacted
    wasCompacted=.true.

    ! return success
    rc = ESMF_SUCCESS

end subroutine CompactMatrix

 subroutine hsort_array(ia,ra)
   integer :: ia(:,:)
   real(ESMF_KIND_R8)    :: ra(:)
   integer :: num_a
   integer :: i,ir,j,l
   integer :: tia(size(ia,1))
   real(ESMF_KIND_R8)    :: tra

   ! get size of array
   num_a=size(ia,2)

   ! Leave if list is too small to sort
   if (num_a <2) return

   l=num_a/2+1
   ir=num_a

10 continue
   if (l .gt. 1) then
      l=l-1
      tia(:)=ia(:,l)
      tra=ra(l)
   else
      tia(:)=ia(:,ir)
      tra=ra(ir)
      ia(:,ir)=ia(:,1)
      ra(ir)=ra(1)
      ir=ir-1
      if (ir .eq. 1) then
         ia(:,1)=tia(:)
         ra(1)=tra
         return
      endif
   endif
   i=l
   j=l+l
20 if (j .le. ir) then
      if (j .lt. ir) then
         if (ia(1,j) .lt. ia(1,j+1)) j=j+1
      endif
      if (tia(1) .lt. ia(1,j)) then
         ia(:,i)=ia(:,j)
         ra(i)=ra(j)
         i=j
         j=j+j
      else
         j=ir+1
      endif
      goto 20
   endif
   ia(:,i)=tia(:)
   ra(i)=tra
   goto 10
 end subroutine hsort_array




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
     print *, "Usage: ESMF_RegridWeightGen [--source|-s] src_grid_filename" 
     print *, "                	     [--destination|-d] dst_grid_filename"
     print *, "                      [--weight|-w] out_weight_file "
     print *, "                      [--method|-m] [bilinear|patch|conservative]"
     print *, "                      [--pole|-p] [all|none|<N>]"
     print *, "                      [--ignore_unmapped|-i]"
     print *, "                      --src_type [SCRIP|ESMF|UGRID]" 
     print *, "                      --dst_type [SCRIP|ESMF|UGRID]"
     print *, "                      -t [SCRIP|ESMF|UGRID]"
     print *, "                      --64bit_offset"
     print *, "                      --src_meshname src_mesh_variable"
     print *, "                      --dst_meshname dst_mesh_variable"
     print *, "                      -r"
     print *, "                      --help"
     print *, "                      --version"
     print *, "where"
     print *, "--source or -s - a required argument specifying the source grid file"
     print *, "                 name"
     print *, "--destination or -d - a required argument specifying the destination grid"
     print *, "                      file name"
     print *, "--weight or -w - a required argument specifying the output regridding weight"
     print *, "                 file name"
     print *, "--method or -m - an optional argument specifying which interpolation method is"
     print *, "                 used.  The default method is bilinear"
     print *, "--pole or -p - an optional argument indicating what to do with the pole."
     print *, "                 The default value is all"
     print *, "--ignore_unmapped or -i - ignore unmapped destination points. If not specified,"
     print *, "                          the default is to stop with an error."
     print *, "--src_type - an optional argument specifying the source grid file type."
     print *, "             The value could be one of SCRIP, GRIDSPEC, ESMF, or UGRID."
     print *, "             The ESMF and UGRID types are only available for the unstructured grid."
     print *, "             The default option is SCRIP."
     print *, "--dst_type - an optional argument specifying the destination grid file type."
     print *, "             The value could be one of SCRIP, GRIDSPEC, ESMF, or UGRID."
     print *, "             The ESMF and UGRID types are only available for the unstructured grid."
     print *, "             The default option is SCRIP."
     print *, "-t         - an optional argument specifying the file types for both the source"
     print *, "             and the destination grid files.  The default option is SCRIP."
     print *, "             If both -t and --src_type or --dst_type are given at the same time"
     print *, "             and they disagree with each other, an error message will be"
     print *, "             generated"
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
     print *, "--src_meshname  - required if the source grid type is UGRID. It defines the dummy"
     print *, "             variable name that has all the topology information stored in its"
     print *, "             attributes."
     print *, "--dst_meshname  - required if the destination grid type is UGRID. It defines the"
     print *, "             dummy variable name that has all the topology information stored in its"
     print *, "             attributes."
     print *, "--src_missingvalue  - an optional argument used only when the src file type is GRIDSPEC"
     print *, "             It defines the variable name whose 'missing_value' or '_FillValue' attribute"
     print *, "             will be used to construct the mask for the source grid. Without this argument,"
     print *, "             a GRIDSPEC file is not masked."
     print *, "--dst_missingvalue  - an optional argument used only when the dest file type is GRIDSPEC"
     print *, "             It defines the variable name whose 'missing_value' or '_FillValue' attribute"
     print *, "             will be used to construct the mask for the destination grid. Without this"
     print *, "             argument, a GRIDSPEC file is not masked."
     print *, "--help     - Print this help message and exit."
     print *, "--version  - Print ESMF version and license information and exit."
     print *, ""
     print *, "For questions, comments, or feature requests please send email to:"
     print *, "esmf_support@list.woc.noaa.gov"
     print *, ""
     print *, "Visit http://www.earthsystemmodeling.org/ to find out more about the"
     print *, "Earth System Modeling Framework."
     print *, ""
end subroutine PrintUsage

subroutine PrintVersionInfo()

       print *, "  ESMF_VERSION_STRING:       ", ESMF_VERSION_STRING
       print *, "  ESMF_VERSION_MAJOR:        ", ESMF_VERSION_MAJOR
       print *, "  ESMF_VERSION_MINOR:        ", ESMF_VERSION_MINOR
       print *, "  ESMF_VERSION_REVISION:     ", ESMF_VERSION_REVISION
       print *, "  ESMF_VERSION_PATCHLEVEL:   ", ESMF_VERSION_PATCHLEVEL
       print *, "  ESMF_VERSION_PUBLIC:       ", ESMF_VERSION_PUBLIC
       print *, "  ESMF_VERSION_BETASNAPSHOT: ", ESMF_VERSION_BETASNAPSHOT
       print *, ""
       print *, "Earth System Modeling Framework"
       print *, ""
       print *, "Copyright (c) 2002-2012 University Corporation for Atmospheric Research,"
       print *, "Massachusetts Institute of Technology, Geophysical Fluid Dynamics Laboratory,"
       print *, "University of Michigan, National Centers for Environmental Prediction,"
       print *, "Los Alamos National Laboratory, Argonne National Laboratory,"
       print *, "NASA Goddard Space Flight Center.  All rights reserved."
       print *, ""
       print *, "Permission is hereby granted, free of charge, to any person obtaining a copy"
       print *, 'of this software and associated documentation files (the "Software"), to'
       print *, "deal with the Software without restriction, including without limitation the"
       print *, "rights to use, copy, modify, merge, publish, distribute, sublicense, and/or"
       print *, "sell copies of the Software, and to permit persons to whom the Software is"
       print *, "furnished to do so, subject to the following conditions:"
       print *, "   1. Redistributions of source code must retain the above copyright notice,"
       print *, "      this list of conditions and the following disclaimers."
       print *, "   2. Redistributions in binary form must reproduce the above copyright"
       print *, "      notice, this list of conditions and the following disclaimers in the"
       print *, "      documentation and/or other materials provided with the distribution."
       print *, "   3. Neither the names of the organizations developing this software, nor"
       print *, "      its contributors may be used to endorse or promote products derived"
       print *, "      from this Software without specific prior written permission."
       print *, ""
       print *, 'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR'
       print *, "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
       print *, "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE"
       print *, "CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
       print *, "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING"
       print *, "FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS"
       print *, "WITH THE SOFTWARE."
       print *, ""

end subroutine PrintVersionInfo


end program ESMF_RegridWeightGen
