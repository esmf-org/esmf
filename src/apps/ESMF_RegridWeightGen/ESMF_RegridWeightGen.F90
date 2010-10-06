!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id: ESMF_RegridWeightGen.F90,v 1.8 2010/10/06 17:10:44 peggyli Exp $
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
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_RouteHandle) :: rh1
      integer(ESMF_KIND_I4), pointer:: indicies(:,:)
      real(ESMF_KIND_R8), pointer :: weights(:)
      character(len=256) :: srcfile, dstfile, wgtfile
      character(len=40)  :: method, flag
      integer(ESMF_KIND_I4) :: maskvals(1)
      integer            :: index
      type(ESMF_RegridPole) :: pole
      integer            :: poleptrs
      integer, pointer   :: dims(:)
      logical            :: convert3D
      logical            :: isConserve
      logical            :: addCorners,convertToDual
      type(ESMF_MeshLoc) :: meshLoc
      logical            :: srcIsScrip, dstIsScrip, srcIsReg, dstIsReg, typeSetFlag
      character(len=256) :: methodStr
      real(ESMF_KIND_R8), pointer :: srcArea(:)
      real(ESMF_KIND_R8), pointer :: dstArea(:)
      character(len=256) :: commandbuf1(3)
      integer            :: commandbuf2(6)

      !------------------------------------------------------------------------
      ! Initialize ESMF
      !
      call ESMF_Initialize (defaultCalendar=ESMF_CAL_GREGORIAN, &
			defaultlogfilename="RegridWeightGen.Log", &
                    	defaultlogtype=ESMF_LOG_MULTI, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      !------------------------------------------------------------------------
      ! get global vm information
      !
      call ESMF_VMGetGlobal(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      ! set up local pet info
      call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      !------------------------------------------------------------------------
      ! Parse keyword based arguments at Pet 0
      !   then broadcast the results to the rest of the Pets
      !
      if (PetNo == 0) then
         call ESMF_UtilGetArgIndex('--help',index)
         if (index /= -1) then
	   call PrintUsage()
	   call ESMF_Finalize(terminationflag=ESMF_ABORT)
         endif
         call ESMF_UtilGetArgIndex('-s',index)
         if (index == -1) call ESMF_UtilGetArgIndex('--source',index,rc)
         if (index == -1) then
           print *, 'the required argument [-s|--source] is missing'
           call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else
           call ESMF_UtilGetArg(index+1,srcfile)
         endif
      
         call ESMF_UtilGetArgIndex('-d',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--destination',index,rc)
         if (index == -1) then
           print *, 'the required argument [-w|--weight] is missing'
           call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else
           call ESMF_UtilGetArg(index+1,dstfile)
         endif
          
         call ESMF_UtilGetArgIndex('-w',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--weight',index,rc)
         if (index == -1) then
           print *, 'the required argument [-w|--weight] is missing'
           call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else	
           call ESMF_UtilGetArg(index+1,wgtfile)
         endif

         call ESMF_UtilGetArgIndex('-m',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--method',index,rc)
         if (index == -1) then
           print *, 'use default interpolation method: bilinear'
           method = 'bilinear'
         else
           call ESMF_UtilGetArg(index+1,method)
         endif
    	
         poleptrs = -1
         call ESMF_UtilGetArgIndex('-p',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--pole',index,rc)
         if (index == -1) then
	   if (method .eq. 'conserve') then
              print *, 'use default pole: None'
              pole = ESMF_REGRIDPOLE_NONE
	      poleptrs = 0
	   else
              print *, 'use default pole: All'
              pole = ESMF_REGRIDPOLE_ALLAVG
           endif
         else
           call ESMF_UtilGetArg(index+1,flag)
           if (trim(flag) .eq. 'none') then
	     pole = ESMF_REGRIDPOLE_NONE
	     poleptrs = 0
           else if (trim(flag) .eq. 'all') then
             pole = ESMF_REGRIDPOLE_ALLAVG
           else 
             read(flag,'(i4)') poleptrs
             pole = ESMF_REGRIDPOLE_NPNTAVG
           endif
	   if ((method .eq. 'conserve') .and. &
	       (pole .ne. ESMF_REGRIDPOLE_NONE)) then
	     print *, 'Conserve method only works with no pole'
             call ESMF_Finalize(terminationflag=ESMF_ABORT)
           endif
         endif

         typeSetFlag = .false. 
         srcIsScrip = .true.
         dstIsScrip = .true.
         call ESMF_UtilGetArgIndex('-t',index,rc)
         if (index /= -1) then
           call ESMF_UtilGetArg(index+1,flag)
	   if (trim(flag) .eq. 'ESMF') then
             srcIsScrip = .false.
             dstIsScrip = .false.
             print *, 'Set src and dst grid file types to ESMF'
           else if (trim(flag) .ne. 'SCRIP') then
	     print *, 'Unknown -src_type: must be either ESMF or SCRIP'
             call ESMF_Finalize(terminationflag=ESMF_ABORT)
           endif 
           typeSetFlag = .true.
         endif

         call ESMF_UtilGetArgIndex('-src_type',index,rc)
         if (index /= -1) then
           call ESMF_UtilGetArg(index+1,flag)
	   if (typeSetFlag) then
	     ! check if the type is consistent with -t
             if ((trim(flag) .eq. 'ESMF' .and. srcIsScrip) .or.   &
	        (trim(flag) .eq. 'SCRIP' .and. .not. srcIsScrip)) then
	        print *, 'Source file type conflict: -src_type and -t' 
                call ESMF_Finalize(terminationflag=ESMF_ABORT)
	        srcIsScrip = .false.
	     end if
           endif
           if (trim(flag) .eq. 'ESMF') then
	     srcIsScrip = .false.
           else if (trim(flag) .ne. 'SCRIP') then
	     print *, 'Unknown -src_type: must be either ESMF or SCRIP'
             call ESMF_Finalize(terminationflag=ESMF_ABORT)
           endif
         endif

         call ESMF_UtilGetArgIndex('-dst_type',index,rc)
         if (index /= -1) then
           call ESMF_UtilGetArg(index+1,flag)
	   if (typeSetFlag) then
	     ! check if the type is consistent with -t
             if ((trim(flag) .eq. 'ESMF' .and. dstIsScrip) .or.   &
	        (trim(flag) .eq. 'SCRIP' .and. .not. dstIsScrip)) then
	        print *, 'Destination file type conflict: -dst_type and -t' 
                call ESMF_Finalize(terminationflag=ESMF_ABORT)
	        dstIsScrip = .false.
	     end if
           endif
           if (trim(flag) .eq. 'ESMF') then
	     dstIsScrip = .false.
           else if (trim(flag) .ne. 'SCRIP') then
	     print *, 'Unknown -dst_type: must be either ESMF or SCRIP'
             call ESMF_Finalize(terminationflag=ESMF_ABORT)
           endif
         endif

        ! Should I have only PetNO=0 to open the file and find out the size?
         if (srcIsScrip) then
	   call ESMF_ScripInq(srcfile, grid_dims=dims, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
           ! size(dims) == 1 is a unstructured grid
           ! size(dims) == 2 is a regular grid
           if (size(dims) == 2) then
	     srcIsReg = .true.
           else
             srcIsReg = .false.
           endif
	 endif
     	 if (dstIsScrip) then
	   call ESMF_ScripInq(dstfile, grid_dims=dims, rc=rc)
           if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
           ! size(dims) == 1 is a unstructured grid
           ! size(dims) == 2 is a regular grid
           if (size(dims) == 2) then
	     dstIsReg = .true.
           else
             dstIsReg = .false.
           endif
	 endif
	print *, "Starting weight generation with these inputs: "
	print *, "  Source File: ", trim(srcfile)
	print *, "  Destination File: ", trim(dstfile)
  	print *, "  Weight File: ", trim(wgtfile)
        if (srcIsScrip) print *, "  Source File is in SCRIP format"
        if (dstIsScrip) print *, "  Destination File is in SCRIP format"
        if (srcIsReg)   then
		print *, "  Source Grid is a logically rectangular grid"
        else
	        print *, "  Source Grid is an unstructured grid"
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
	else
	       print *, "  Pole option: ", poleptrs
        endif

        ! Group the command line arguments and broadcast to other PETs
        commandbuf1(1)=srcfile
        commandbuf1(2)=dstfile
        commandbuf1(3)=wgtfile

        ! Broadcast the command line arguments to all the PETs
        call ESMF_VMBroadcast(vm, commandbuf1, 256*3, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

        commandbuf2(:)=0
        if (srcIsScrip) commandbuf2(1)=1
        if (srcIsReg)   commandbuf2(2)=1
        if (dstIsScrip) commandbuf2(3)=1
        if (dstIsReg)   commandbuf2(4)=1
        if (method .eq. 'patch') commandbuf2(5)=1
        if (method .eq. 'conserve') commandbuf2(5)=2
        commandbuf2(6)=poleptrs
        call ESMF_VMBroadcast(vm, commandbuf2, 6, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
     else
        call ESMF_VMBroadcast(vm, commandbuf1, 256*3, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        srcfile = commandbuf1(1)
        dstfile = commandbuf1(2)
        wgtfile = commandbuf1(3)

        call ESMF_VMBroadcast(vm, commandbuf2, 6, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
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
        elseif (poleptrs ==  0) then
           pole=ESMF_REGRIDPOLE_NONE
	else
           pole=ESMF_REGRIDPOLE_NPNTAVG 
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


     !Read in the srcfile and create the corresponding ESMF object (either
     ! ESMF_Grid or ESMF_Mesh
     if (srcIsScrip) then
	if(srcIsReg) then
           srcGrid = ESMF_GridCreate(trim(srcfile),(/PetCnt,1/), addCornerStagger=addCorners, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    	   srcField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	else
           srcMesh = ESMF_MeshCreate(trim(srcfile), ESMF_FILEFORMAT_SCRIP, convert3D=.true., &
                       convertToDual=convertToDual, rc=rc)

	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
           !srcField = CreateFieldFromMesh(srcMesh, rc)
           call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
           srcField=ESMF_FieldCreate(srcMesh,arrayspec,location=meshLoc,rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	endif
      else
	! if srcfile is not SCRIP, it is always unstructured
	srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_ESMFMESH, convert3D=.true., &
                    rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
        srcField=ESMF_FieldCreate(srcMesh,arrayspec,location=meshLoc,rc=rc)
	if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif

     !Read in the dstfile and create the corresponding ESMF object (either
     ! ESMF_Grid or ESMF_Mesh)
     if (dstIsScrip) then
	if(dstIsReg) then
           dstGrid = ESMF_GridCreate(trim(dstfile),(/PetCnt,1/), addCornerStagger=addCorners, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    	   dstField = ESMF_FieldCreate(dstGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	else
           dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, convert3D=.true., &
                       convertToDual=convertToDual, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
           call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
           dstField=ESMF_FieldCreate(dstMesh,arrayspec,location=meshLoc,rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	endif
      else
	! if dstfile is not SCRIP, it is always unstructured
	dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_ESMFMESH, convert3D=.true., &
                    rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        dstField=ESMF_FieldCreate(dstMesh,arrayspec,location=meshLoc,rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif


      maskvals(1) = 0
      if (poleptrs <= 0) poleptrs = 1
      if (trim(method) .eq. 'bilinear') then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_BILINEAR, &
            regridPoleType = pole, regridPoleNPnts = poleptrs, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=rc)
	    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            methodStr = "Bilinear remapping"
      else if (trim(method) .eq. 'patch') then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_PATCH, &
            regridPoleType = pole, regridPoleNPnts = poleptrs, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=rc)
	    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Patch
      else if (trim(method) .eq. 'conserve') then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_CONSERVE, &
            regridPoleType = pole, regridPoleNPnts = poleptrs, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=rc)
	    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            methodStr = "Conservative remapping"
      else ! nothing recognizable so report error
	     print *, 'The -method is not a recognized interpolation method.'
             call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif

      ! Compute areas if conservative
      ! Area only valid on PET 0 right now, when parallel Array
      ! write works, then make area io parallel
      if (isConserve) then
         if (srcIsReg) then
            call computeAreaGrid(srcGrid, PetNo, srcArea, rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else
            call computeAreaMesh(srcMesh, vm, petNo, petCnt, srcArea, rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            call ESMF_MeshMergeSplitSrcInd(srcMesh,indicies,rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
         endif

         if (dstIsReg) then
            call computeAreaGrid(dstGrid, PetNo, dstArea, rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else
            call computeAreaMesh(dstMesh, vm, petNo, petCnt, dstArea, rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            call ESMF_MeshMergeSplitDstInd(dstMesh,weights,indicies,rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
         endif
      endif


      !! Write the weight table into a SCRIP format NetCDF file
      if (PetNo == 0) then
         if (isConserve) then
            call ESMF_OutputScripWeightFile(wgtfile, weights, indicies,  &
	           srcFile=srcfile, dstFile=dstfile, srcIsScrip=srcIsScrip,&
	           dstIsScrip=dstIsScrip, method = methodStr, &
                   srcArea=srcArea, dstArea=dstArea, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else
            call ESMF_OutputScripWeightFile(wgtfile, weights, indicies,  &
	           srcFile=srcfile, dstFile=dstfile, srcIsScrip=srcIsScrip,&
	           dstIsScrip=dstIsScrip, method = methodStr, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
         endif
      else 
	 call ESMF_OutputScripWeightFile(wgtfile, weights, indicies, rc=rc)
         if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif


      ! Get rid of conservative arrays
      if (isConserve) then
         if (PetNo == 0) then
	    deallocate(srcArea)
	    deallocate(dstArea)
         endif
      endif

      ! Output success
      if (PetNo==0) then
         write(*,*)
         write(*,*) "Completed weight generation successfully."
      endif

      call ESMF_Finalize()
      stop

contains


! For now just put into a 1D array on PET 0. When PIO array write is done, then
! do it in parallel
! AREA ONLY VALID ON PET 0
subroutine computeAreaGrid(grid, petNo, area, rc)
  type(ESMF_Grid) :: grid
  integer :: petNo
  integer :: rc
  real (ESMF_KIND_R8), pointer :: area(:)

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
  call ESMF_FieldRegridGetArea(areaField, regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
 if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif


  ! Get size of Grid
  call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
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
  call ESMF_VMGather(vm,localCount,globalCount,count=1,root=0,rc=localrc)
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
         root=0, rc=localrc)
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

subroutine PrintUsage()
     print *, "Usage: ESMF_RegridWeightGen [--source|-s] src_grid_filename" 
     print *, "                	     [--destination|-d] dst_grid_filename"
     print *, "                      [--weight|-w] out_weight_file "
     print *, "                      [--method|-m] [bilinear|patch|conservative]"
     print *, "                      [--pole|-p] [all|none|<N>]"
     print *, "                      --src_type [SCRIP|ESMF]" 
     print *, "                      --dst_type [SCRIP|ESMF]"
     print *, "                      -t [SCRIP|ESMF]"
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

end subroutine PrintUsage

end program ESMF_RegridWeightGen
