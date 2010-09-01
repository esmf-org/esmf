!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id: ESMF_RegridWeightGen.F90,v 1.1 2010/09/01 23:40:26 peggyli Exp $
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

      type CommandArgs
      sequence
        character(len=256) :: srcfile, dstfile,  wgtfile
        logical :: srcIsScrip,dstIsScrip, srcIsReg, dstIsReg
        logical :: conservFlag
        character(len=40) :: method
        integer :: pole
      end type

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
      integer            :: index
      integer            :: pole
      integer, pointer   :: dims(:)
      logical            :: conservFlag, convert3D
      logical            :: srcIsScrip, dstIsScrip, srcIsReg, dstIsReg, typeSetFlag
      type(CommandArgs)  :: mycommand
      character, allocatable :: commandbuf(:)
      integer            :: command_len
      character(len=256) :: methodStr

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
         call ESMF_UtilGetArgIndex('-s',index)
         if (index == -1) call ESMF_UtilGetArgIndex('--source',index,rc)
         if (index == -1) then
           print *, 'the required argument [-s|--source] is missing'
           call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else
           call ESMF_UtilGetArg(index+1,srcfile)
           mycommand%srcfile = srcfile
         endif
      
         call ESMF_UtilGetArgIndex('-d',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--destination',index,rc)
         if (index == -1) then
           print *, 'the required argument [-w|--weight] is missing'
           call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else
           call ESMF_UtilGetArg(index+1,dstfile)
           mycommand%dstfile = dstfile
         endif
          
         call ESMF_UtilGetArgIndex('-w',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--weight',index,rc)
         if (index == -1) then
           print *, 'the required argument [-w|--weight] is missing'
           call ESMF_Finalize(terminationflag=ESMF_ABORT)
         else	
           call ESMF_UtilGetArg(index+1,wgtfile)
           mycommand%wgtfile = wgtfile
         endif

         call ESMF_UtilGetArgIndex('-m',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--method',index,rc)
         if (index == -1) then
           print *, 'use default interpolation method: bilinear'
           method = 'bilinear'
         else
           call ESMF_UtilGetArg(index+1,method)
         endif
         mycommand%method = method
    	
         conservFlag = .false.
         call ESMF_UtilGetArgIndex('-c',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--conservation',index,rc)
         if (index == -1) then
           print *, 'use default conservative flag: off'
         else
           call ESMF_UtilGetArg(index+1,flag)
           if (trim(flag) .eq. 'on' .or. trim(flag) .eq. 'ON') conservFlag = .true.
         endif
         mycommand%conservFlag = conservFlag

         call ESMF_UtilGetArgIndex('-p',index,rc)
         if (index == -1) call ESMF_UtilGetArgIndex('--pole',index,rc)
         if (index == -1) then
           print *, 'use default pole: none'
           pole = 0
         else
           call ESMF_UtilGetArg(index+1,flag)
           if (trim(flag) .eq. 'none') pole = 0
           if (trim(flag) .eq. 'all') then
             pole = -1
           else 
             read(flag,'(i4)') pole
           endif
         endif
         mycommand%pole = pole

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
	   call ESMF_ScripInq(srcfile, grid_dims=dims)
           ! size(dims) == 1 is a unstructured grid
           ! size(dims) == 2 is a regular grid
           if (size(dims) == 2) then
	     srcIsReg = .true.
           else
             srcIsReg = .false.
           endif
	 endif
     	 if (dstIsScrip) then
	   call ESMF_ScripInq(dstfile, grid_dims=dims)
           ! size(dims) == 1 is a unstructured grid
           ! size(dims) == 2 is a regular grid
           if (size(dims) == 2) then
	     dstIsReg = .true.
           else
             dstIsReg = .false.
           endif
	 endif
         mycommand%srcIsScrip = srcIsScrip
         mycommand%dstIsScrip = dstIsScrip
         mycommand%srcIsReg = srcIsReg
         mycommand%dstIsReg = dstIsReg
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
        print *, "Regrid Method: ", method
        if (conservFlag) then
	        print *, "  Conservative is ON"
        else
                print *, "  Conservative is OFF"
        endif

        inquire(iolength=command_len) mycommand
        command_len = command_len * 4
        allocate(commandbuf(command_len))
        commandbuf = transfer(mycommand, mold=commandbuf)

        ! Broadcast the command line arguments to all the PETs
        call ESMF_VMBroadcast(vm, commandbuf, command_len, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
     else
 
        inquire(iolength=command_len) mycommand
        command_len = command_len * 4
        allocate(commandbuf(command_len))

        call ESMF_VMBroadcast(vm, commandbuf, command_len, 0, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        mycommand = transfer(commandbuf, mold=mycommand)
        srcfile = mycommand%srcfile
        dstfile = mycommand%dstfile
        wgtfile = mycommand%wgtfile
        method = mycommand%method
        conservFlag = mycommand%conservFlag
        srcIsScrip = mycommand%srcIsScrip
        srcIsReg  = mycommand%srcIsReg
        dstIsScrip = mycommand%dstIsScrip
        dstIsReg  = mycommand%dstIsReg
        pole = mycommand%pole
     endif

     !Read in the srcfile and create the corresponding ESMF object (either
     ! ESMF_Grid or ESMF_Mesh
     if (srcIsScrip) then
	if(srcIsReg) then
	   srcGrid = ESMF_GridCreate(trim(srcfile),(/PetCnt,1/),rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    	   srcField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	else
           srcMesh = ESMF_MeshCreate(trim(srcfile), ESMF_FILEFORMAT_SCRIP, convert3D=.true., rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
           srcField = CreateFieldFromMesh(srcMesh, rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	endif
      else
	! if srcfile is not SCRIP, it is always unstructured
	srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_ESMFMESH, convert3D=.true., rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        srcField = CreateFieldFromMesh(srcMesh, rc)
	if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif

     !Read in the dstfile and create the corresponding ESMF object (either
     ! ESMF_Grid or ESMF_Mesh)
     if (dstIsScrip) then
	if(dstIsReg) then
	   dstGrid = ESMF_GridCreate(dstfile,(/PetCnt,1/),rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
    	   dstField = ESMF_FieldCreate(dstGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	else
           dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, convert3D=.true., rc=rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
           dstField = CreateFieldFromMesh(dstMesh,rc)
	   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	endif
      else
	! if dstfile is not SCRIP, it is always unstructured
	dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_ESMFMESH, convert3D=.true., rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        dstField = CreateFieldFromMesh(dstMesh, rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif

      if (trim(method) .eq. 'bilinear') then
        if (conservflag) then
        call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_BILINEAR, &
	    regridConserve = ESMF_REGRID_CONSERVE_ON, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=rc)
	    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            methodStr = "Bilinear Regridding with Conservative Correction"

	else 
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_BILINEAR, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=rc)
	    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            methodStr = "Bilinear Regridding"
        endif
      else if (trim(method) .eq. 'patch') then
        if (conservflag) then
        call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_PATCH, &
	    regridConserve = ESMF_REGRID_CONSERVE_ON, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=rc)
	    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            methodStr = "Patch Regridding with Conservative Correction"
	else 
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_PATCH, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=rc)
	    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            methodStr = "Patch Regridding"
        endif
      else ! nothing recognizable so report error
	     print *, 'The -method is not a recognized interpolation method.'
             call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif

      !! Write the weight table into a SCRIP format NetCDF file
      if (PetNo == 0) then
         call ESMF_OutputScripWeightFile(wgtfile, weights, indicies,  &
	      srcFile=srcfile, dstFile=dstfile, srcIsScrip=srcIsScrip,&
	      dstIsScrip=dstIsScrip, method = methodStr, rc=rc)
         if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      else 
	 call ESMF_OutputScripWeightFile(wgtfile, weights, indicies, rc=rc)
         if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif

      call ESMF_Finalize()
      stop

contains

function CreateFieldFromMesh(mesh, rc)

      type(ESMF_Field):: CreateFieldFromMesh
   
      type(ESMF_Mesh)          :: mesh
      integer                  :: rc

! Local Variables
      type(ESMF_Field)       :: field
      type(ESMF_ArraySpec)   :: arraySpec
      type(ESMF_Array)       :: array
      type(ESMF_DistGrid)    :: nodeDG
      integer                :: totalNodes, totalCells, count
      integer, allocatable   :: nodeIds(:)
     
      ! check the MeshGet
      call ESMF_MeshGet(mesh, nodalDistgrid=nodeDG, &
            numOwnedNodes=totalNodes, numOwnedElements=totalCells, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      ! print *, 'PE nodeDG elemDG',PetNo, totalNodes, totalCells
      
      ! get node information from nodeDG
      allocate(nodeIds(totalNodes))
      call ESMF_DistGridGet(nodeDG, 0, seqIndexList=nodeIds, elementCount=count, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      ! *************************************************************      
      ! get vertex data
      ! Create a field using the Mesh and an ESMF Array 
      call ESMF_ArraySpecSet(arraySpec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      array = ESMF_ArrayCreate(arraySpec, nodeDG, computationalEdgeLWidth=(/0/), &
	computationalEdgeUWidth=(/0/), rc=rc)
      if (rc /= ESMF_SUCCESS) return

      ! Create a field using the array and mesh
      field = ESMF_FieldCreate(mesh, array, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      CreateFieldFromMesh = field
      rc = ESMF_SUCCESS
      return

end function CreateFieldFromMesh



end program ESMF_RegridWeightGen
