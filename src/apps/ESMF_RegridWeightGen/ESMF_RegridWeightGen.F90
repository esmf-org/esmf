!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id: ESMF_RegridWeightGen.F90,v 1.35 2011/06/22 15:08:12 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!!-------------------------------------------------------------------------------------

program ESMF_RegridWeightGen

! !USES:
      use ESMF_Mod
      use ESMF_IOScripMod

      implicit none

      integer            :: rc
      type(ESMF_VM)      :: vm
      integer            :: PetNo, PetCnt
      type(ESMF_Mesh)    :: srcMesh, dstMesh
      type(ESMF_Grid)    :: srcGrid, dstGrid
      type(ESMF_Field)   :: srcField, dstField
      type(ESMF_Field)   :: srcFracField, dstFracField
      type(ESMF_ArraySpec) :: arrayspec
      integer(ESMF_KIND_I4), pointer:: indices(:,:)
      real(ESMF_KIND_R8), pointer :: weights(:)
      character(len=256) :: srcfile, dstfile, wgtfile
      character(len=40)  :: method, flag
      integer(ESMF_KIND_I4) :: maskvals(1)
      integer            :: index
      type(ESMF_RegridPole) :: pole
      integer            :: poleptrs
      integer, pointer   :: srcdims(:), dstdims(:)
      integer            :: srcrank, dstrank
      logical            :: convert3D
      logical            :: isConserve, srcIsSphere, dstIsSphere
      logical            :: addCorners,convertToDual
      type(ESMF_MeshLoc) :: meshLoc
      logical            :: srcIsScrip, dstIsScrip, srcIsReg, dstIsReg
      logical            :: srcIsRegional, dstIsRegional, typeSetFlag
      character(len=256) :: methodStr
      real(ESMF_KIND_R8), pointer :: srcArea(:)
      real(ESMF_KIND_R8), pointer :: dstArea(:)
      real(ESMF_KIND_R8), pointer :: dstFrac(:), srcFrac(:)
      character(len=256) :: commandbuf1(3)
      integer            :: commandbuf2(15)
      integer            :: regridScheme
      integer            :: i, bigFac, xpets, ypets, xpart, ypart, xdim, ydim
      logical            :: wasCompacted
      integer(ESMF_KIND_I4), pointer:: compactedIndices(:,:)
      real(ESMF_KIND_R8), pointer :: compactedWeights(:)
      logical :: ignoreUnmapped
      type(ESMF_UnmappedAction) :: unmappedAction

      !real(ESMF_KIND_R8) :: starttime, endtime
      !------------------------------------------------------------------------
      ! Initialize ESMF
      !
      call ESMF_Initialize (defaultCalendar=ESMF_CALKIND_GREGORIAN, &
			defaultlogfilename="RegridWeightGen.Log", &
                    	defaultlogtype=ESMF_LOG_MULTI, rc=rc)
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
              pole = ESMF_REGRIDPOLE_NONE
	      poleptrs = 0
	   else
              !print *, 'Use default pole: All'
              pole = ESMF_REGRIDPOLE_ALLAVG
           endif
         else
           call ESMF_UtilGetArg(index+1, argvalue=flag)
           if (trim(flag) .eq. 'none') then
	     pole = ESMF_REGRIDPOLE_NONE
	     poleptrs = 0
           else if (trim(flag) .eq. 'all') then
             pole = ESMF_REGRIDPOLE_ALLAVG
           else if (trim(flag) .eq. 'teeth') then
             pole = ESMF_REGRIDPOLE_TEETH
             poleptrs = -2
           else 
             read(flag,'(i4)') poleptrs
             pole = ESMF_REGRIDPOLE_NPNTAVG
           endif
	   if ((method .eq. 'conserve') .and. &
	       (pole .ne. ESMF_REGRIDPOLE_NONE)) then
             write(*,*)
	     print *, 'ERROR: Conserve method only works with no pole.'
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif
         endif

         typeSetFlag = .false. 
         srcIsScrip = .true.
         dstIsScrip = .true.
         srcIsRegional = .false.
         dstIsRegional = .false.
         call ESMF_UtilGetArgIndex('-t', argindex=index, rc=rc)
         if (index /= -1) then
           call ESMF_UtilGetArg(index+1, argvalue=flag)
	   if (trim(flag) .eq. 'ESMF') then
             srcIsScrip = .false.
             dstIsScrip = .false.
             write(*,*)
             print *, 'ERROR: Set src and dst grid file types to ESMF.'
           else if (trim(flag) .ne. 'SCRIP') then
             write(*,*)
	     print *, 'ERROR: Unknown -t: must be either ESMF or SCRIP.'
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif 
           typeSetFlag = .true.
         endif

         call ESMF_UtilGetArgIndex('--src_type', argindex=index, rc=rc)
         if (index /= -1) then
           call ESMF_UtilGetArg(index+1, argvalue=flag)
	   if (typeSetFlag) then
	     ! check if the type is consistent with -t
             if ((trim(flag) .eq. 'ESMF' .and. srcIsScrip) .or.   &
	        (trim(flag) .eq. 'SCRIP' .and. .not. srcIsScrip)) then
                write(*,*)
	        print *, 'ERROR: Source file type conflict: --src_type and -t.' 
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
	        srcIsScrip = .false.
	     end if
           endif
           if (trim(flag) .eq. 'ESMF') then
	     srcIsScrip = .false.
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
             if ((trim(flag) .eq. 'ESMF' .and. dstIsScrip) .or.   &
	        (trim(flag) .eq. 'SCRIP' .and. .not. dstIsScrip)) then
                write(*,*)
	        print *, 'ERROR: Destination file type conflict: --dst_type and -t.' 
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
	        dstIsScrip = .false.
	     end if
           endif
           if (trim(flag) .eq. 'ESMF') then
	     dstIsScrip = .false.
           else if (trim(flag) .ne. 'SCRIP') then
             write(*,*)
	     print *, 'ERROR: Unknown --dst_type: must be either ESMF or SCRIP.'
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           endif
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
           pole = ESMF_REGRIDPOLE_NONE
           poleptrs = 0
           ! print *, 'Set pole to None for regional grids.'
         end if

         call ESMF_UtilGetArgIndex('--src_regional', argindex=index, rc=rc)
         if (index /= -1) then
           srcIsRegional = .true.
           pole = ESMF_REGRIDPOLE_NONE
           poleptrs = 0
           ! print *, 'Set pole to None for regional source grid.'
         end if

         call ESMF_UtilGetArgIndex('--dst_regional', argindex=index, rc=rc)
         if (index /= -1) then
           dstIsRegional = .true.
         end if

        ! Should I have only PetNO=0 to open the file and find out the size?
         if (srcIsScrip) then
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
         else
	   srcIsReg = .false.
	 endif
     	 if (dstIsScrip) then
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
         else
	   dstIsReg = .false.
	 endif
	print *, "Starting weight generation with these inputs: "
	print *, "  Source File: ", trim(srcfile)
	print *, "  Destination File: ", trim(dstfile)
  	print *, "  Weight File: ", trim(wgtfile)
        if (srcIsScrip) then 
            print *, "  Source File is in SCRIP format"
        else
            print *, "  Source File is in ESMF format"
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
        if (dstIsScrip) then 
	    print *, "  Destination File is in SCRIP format"
        else
	    print *, "  Destination File is in ESMF format"
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
        if (pole .eq. ESMF_REGRIDPOLE_NONE) then
	       print *, "  Pole option: NONE"
	elseif (pole .eq. ESMF_REGRIDPOLE_ALLAVG) then
	       print *, "  Pole option: ALL"
	elseif (pole .eq. ESMF_REGRIDPOLE_TEETH) then
	       print *, "  Pole option: TEETH"
	else
	       print *, "  Pole option: ", poleptrs
        endif
        if (ignoreUnmapped) then
	       print *, "  Ignore unmapped destination points"
        endif
        write(*,*)

        ! Group the command line arguments and broadcast to other PETs
        commandbuf1(1)=srcfile
        commandbuf1(2)=dstfile
        commandbuf1(3)=wgtfile

        ! Broadcast the command line arguments to all the PETs
        call ESMF_VMBroadcast(vm, commandbuf1, 256*3, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

        commandbuf2(:)=0
        if (srcIsScrip) commandbuf2(1)=1
        if (srcIsReg)   commandbuf2(2)=1
        if (dstIsScrip) commandbuf2(3)=1
        if (dstIsReg)   commandbuf2(4)=1
        if (method .eq. 'patch') commandbuf2(5)=1
        if (method .eq. 'conserve') commandbuf2(5)=2
        commandbuf2(6)=poleptrs
        if (srcIsRegional) commandbuf2(7)=1
        if (dstIsRegional) commandbuf2(8)=1
	if (srcIsScrip) then 
           commandbuf2(9)=srcrank
           if (srcrank == 1) then
              commandbuf2(10) = srcdims(1)
              commandbuf2(11) = -1
           else 
              commandbuf2(10) = srcdims(1)
              commandbuf2(11) = srcdims(2)
           endif
        endif
        if (dstIsScrip) then
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


        call ESMF_VMBroadcast(vm, commandbuf2, 15, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
     else
        call ESMF_VMBroadcast(vm, commandbuf1, 256*3, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        srcfile = commandbuf1(1)
        dstfile = commandbuf1(2)
        wgtfile = commandbuf1(3)

        call ESMF_VMBroadcast(vm, commandbuf2, 15, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        if (commandbuf2(1)==1) then
           srcIsScrip = .true.
        else
           srcIsScrip = .false.
        end if
        if (commandbuf2(2)==1) then
           srcIsReg = .true.
        else
           srcIsReg = .false.
        end if
        if (commandbuf2(3)==1) then
           dstIsScrip = .true.
        else
           dstIsScrip = .false.
        end if
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
	   pole=ESMF_REGRIDPOLE_ALLAVG
        else if (poleptrs == -2) then 
	   pole=ESMF_REGRIDPOLE_TEETH
        else if (poleptrs ==  0) then
           pole=ESMF_REGRIDPOLE_NONE
	else
           pole=ESMF_REGRIDPOLE_NPNTAVG 
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
        if (srcIsScrip) then
           srcrank = commandbuf2(9)
           srcdims(1)=commandbuf2(10)
           srcdims(2)=commandbuf2(11)
        endif
        if (dstIsScrip) then
           dstrank = commandbuf2(12)
           dstdims(1)=commandbuf2(13)
           dstdims(2)=commandbuf2(14)
        endif

        if (commandbuf2(15) == 1) then
           ignoreUnmapped=.true.
        else
           ignoreUnmapped=.false.
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
     meshLoc=ESMF_MESHLOC_NODE
     if (isConserve) then
        addCorners=.true.
        convertToDual=.false.
        meshLoc=ESMF_MESHLOC_ELEMENT
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
     unmappedAction=ESMF_UNMAPPEDACTION_ERROR
     if (ignoreUnmapped) then
        unmappedAction=ESMF_UNMAPPEDACTION_IGNORE
     endif

     ! Create a decomposition such that each PET will contain at least 2 column and 2 row of data
     ! otherwise, regrid will not work
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
	do while (xdim <= 1)
	  xpart = xpart-1
   	  xdim = srcdims(1)/xpart
        enddo
	do while (ydim <= 1) 
	  ypart = ypart-1
   	  ydim = srcdims(2)/ypart
        enddo
     !   print *, 'src grid partition', srcdims, xpart, ypart
     endif
     !Read in the srcfile and create the corresponding ESMF object (either
     ! ESMF_Grid or ESMF_Mesh
     if (srcIsScrip) then
	if(srcIsReg) then
           srcGrid = ESMF_GridCreate(srcfile,(/xpart,ypart/), addCornerStagger=addCorners, &
                       isSphere=srcIsSphere, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    	   srcField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	else
           srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_SCRIP, convert3D=.true., &
                       convertToDual=convertToDual, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
           call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
           srcField=ESMF_FieldCreate(srcMesh,arrayspec,location=meshLoc,rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	endif
      else
	! if srcfile is not SCRIP, it is always unstructured
	srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_ESMFMESH, convert3D=.true., &
                    rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
        srcField=ESMF_FieldCreate(srcMesh,arrayspec,location=meshLoc,rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
      endif

     !Read in the dstfile and create the corresponding ESMF object (either
     ! ESMF_Grid or ESMF_Mesh)
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
	do while (xdim <= 1)
	  xpart = xpart-1
   	  xdim = dstdims(1)/xpart
        enddo
	do while (ydim <= 1)
	  ypart = ypart-1
   	  ydim = dstdims(2)/ypart
        enddo
     !   print *, 'dst grid partition', dstdims, xpart, ypart
     endif
     if (dstIsScrip) then
	if(dstIsReg) then
           dstGrid = ESMF_GridCreate(dstfile,(/xpart, ypart/), addCornerStagger=addCorners, &
                       isSphere=dstIsSphere, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
    	   dstField = ESMF_FieldCreate(dstGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	else
           dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, convert3D=.true., &
                       convertToDual=convertToDual, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
           call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
           dstField=ESMF_FieldCreate(dstMesh,arrayspec,location=meshLoc,rc=rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	endif
      else
	! if dstfile is not SCRIP, it is always unstructured
	dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_ESMFMESH, convert3D=.true., &
                    rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
        if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        dstField=ESMF_FieldCreate(dstMesh,arrayspec,location=meshLoc,rc=rc)
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
            srcFracField=ESMF_FieldCreate(srcMesh,arrayspec,location=meshLoc,rc=rc)
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
            dstFracField=ESMF_FieldCreate(dstMesh,arrayspec,location=meshLoc,rc=rc)
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
	    unmappedDstAction=unmappedAction, &
	    indices=indices, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_BILINEAR, &
            regridPoleType = pole, regridPoleNPnts = poleptrs, &
	    regridScheme = regridScheme, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            methodStr = "Bilinear remapping"
      else if (trim(method) .eq. 'patch') then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedDstAction=unmappedAction, &
	    indices=indices, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_PATCH, &
            regridPoleType = pole, regridPoleNPnts = poleptrs, &
	    regridScheme = regridScheme, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Patch
      else if (trim(method) .eq. 'conserve') then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedDstAction=unmappedAction, &
	    indices=indices, weights=weights, &
            srcFracField=srcFracField, dstFracField=dstFracField, &
            regridMethod = ESMF_REGRID_METHOD_CONSERVE, &
            regridPoleType = pole, regridPoleNPnts = poleptrs, &
	    regridScheme = regridScheme, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            methodStr = "Conservative remapping"
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
            call ESMF_MeshMergeSplitSrcInd(srcMesh,indices,rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         endif

         if (dstIsReg) then
            call computeAreaGrid(dstGrid, PetNo, dstArea, regridScheme, rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         else
            call computeAreaMesh(dstMesh, vm, petNo, petCnt, dstArea, rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
            call ESMF_MeshMergeSplitDstInd(dstMesh,weights,indices,rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
         endif
      endif

      ! Compact weight matrix
      ! (only compact if one of the grids is irregular, because that's when the repeated entries occur)
      if ((.not. srcIsReg) .or. (.not. dstIsReg)) then
         call compactMatrix(weights, indices, &
                            wasCompacted, &
                            compactedWeights, compactedIndices, &
                            rc)
         if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

         ! If the list was compacted get rid of the old lists and 
         ! point to the new lists
         if (wasCompacted) then
            deallocate(weights)
            weights=>compactedWeights
            deallocate(indices)
            indices=>compactedIndices
         endif
      endif

      ! Computer fraction if bilinear
      ! src fraction is always 0
      ! destination fraction depends on the src mask, dst mask, and the weight
      if (method .eq. 'bilinear' .or. method .eq. 'patch') then
	if (dstIsReg) then
	   call computeFracGrid(dstGrid, vm, indices, dstFrac, rc)
           if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
        else
	   call computeFracMesh(dstMesh, vm, indices, dstFrac, rc)
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
            call ESMF_OutputScripWeightFile(wgtfile, weights, indices,  &
	           srcFile=srcfile, dstFile=dstfile, srcIsScrip=srcIsScrip,&
	           dstIsScrip=dstIsScrip, method = methodStr, &
                   srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, dstFrac=dstFrac, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
          else
            call ESMF_OutputScripWeightFile(wgtfile, weights, indices,  &
	           srcFile=srcfile, dstFile=dstfile, srcIsScrip=srcIsScrip,&
	           dstIsScrip=dstIsScrip, method = methodStr, dstFrac=dstFrac, rc=rc)
            if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)
	  endif
      else 
	 call ESMF_OutputScripWeightFile(wgtfile, weights, indices, rc=rc)
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
  call ESMF_FieldRegridGetArea(areaField, regridScheme=regridScheme, rc=localrc)
 if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif


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


    ! Get size of list
    inListCount=size(inFactorIndexList,2)
 
    ! if too small to need compacting (e.g. <2) return
    if (inListCount .lt. 2) then
       wasCompacted=.false.
       return
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
     print *, "                      --src_type [SCRIP|ESMF]" 
     print *, "                      --dst_type [SCRIP|ESMF]"
     print *, "                      -t [SCRIP|ESMF]"
     print *, "                      -r"
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
     print *, "             The ESMF type is only available for the unstructured grid."
     print *, "             The default option is SCRIP."
     print *, "--dst_type - an optional argument specifying the destination grid file type."
     print *, "             The ESMF type is only available for the unstructured grid."
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
end subroutine PrintUsage



end program ESMF_RegridWeightGen
