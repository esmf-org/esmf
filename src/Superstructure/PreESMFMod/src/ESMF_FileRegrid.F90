!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

!==============================================================================
#define ESMF_FILENAME "ESMF_FileRegrid.F90"
!==============================================================================
!
!     ESMF FileRegrid module
module ESMF_FileRegridMod
!
!==============================================================================
!
! This file contains the API wrapper for the ESMF_FileRegrid application
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_VMMod
  use ESMF_LogErrMod
  use ESMF_ArraySpecMod
  use ESMF_ArrayMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_GridUtilMod
  use ESMF_StaggerLocMod
  use ESMF_MeshMod
  use ESMF_FieldMod
  use ESMF_FieldCreateMod
  use ESMF_FieldGetMod
  use ESMF_FieldGatherMod
  use ESMF_FieldSMMMod
  use ESMF_FieldRegridMod
  use ESMF_IOScripMod
  use ESMF_IOGridspecMod
  use ESMF_IO_NCPutGetMod
  use ESMF_IOUGridMod
  use ESMF_RHandleMod
  use ESMF_LocStreamMod

#ifdef ESMF_NETCDF
  use netcdf
#endif
  
  implicit none

#define UNDEFINEDVALUE 0.00000001
integer, parameter :: MAXNAMELEN = 64
integer, parameter:: MAX_VARDIMS = 5

!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:

  public ESMF_FileRegrid

! -------------------------- ESMF-public method -------------------------------
contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FileRegrid"

!BOP
! !IROUTINE: ESMF_FileRegrid - Regrid variables defined in the  grid files
! \label{api:esmf_fileregrid}
! !INTERFACE:
  subroutine ESMF_FileRegrid(srcFile, dstFile, srcVarName, dstVarName, keywordEnforcer, &
    dstCoordVars, regridmethod, polemethod, regridPoleNPnts, &
    unmappedaction, ignoreDegenerate, srcRegionalFlag, dstRegionalFlag, &
    verboseFlag, rc) 

! !ARGUMENTS:

  character(len=*),             intent(in)            :: srcFile
  character(len=*),             intent(in)            :: dstFile
  character(len=*),             intent(in)            :: srcVarName
  character(len=*),             intent(in)            :: dstVarName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  character(len=*),             intent(in),  optional :: dstCoordVars
  type(ESMF_RegridMethod_Flag), intent(in),  optional :: regridmethod
  type(ESMF_PoleMethod_Flag),   intent(in),  optional :: polemethod
  integer,                      intent(in),  optional :: regridPoleNPnts
  type(ESMF_UnmappedAction_Flag),intent(in), optional :: unmappedaction
  logical,                      intent(in),  optional :: ignoreDegenerate
  logical,                      intent(in),  optional :: srcRegionalFlag
  logical,                      intent(in),  optional :: dstRegionalFlag
  logical,                      intent(in),  optional :: verboseFlag
  integer,                      intent(out), optional :: rc

! !DESCRIPTION:
! This subroutine provides the same function as the {\tt ESMF\_Regrid} application
! described in Section~\ref{sec:ESMF_Regrid}.  It takes two grid files in NetCDF format and interpolate
! the variable defined in the source grid file to the destination variable using one of the ESMF supported
! regrid methods -- bilinear~(\ref{sec:interpolation:bilinear}), higher-order patch~(\ref{sec:interpolation:patch}),
! first order conservative~(\ref{sec:interpolation:conserve}) or nearest neighbor methods.  
! The grid files can be in one of the following two formats:
! \begin{itemize}
! \item The GRIDSPEC Tile grid file following the CF metadata convention~(\ref{sec:fileformat:gridspec}) for logically rectangular grids
! \item The proposed CF Unstructured grid (UGRID) format~(\ref{sec:fileformat:ugrid}) for  unstructured grids.
! \end{itemize}
! \smallskip
! The optional arguments allow users to specify various options to control the regrid operation, 
! such as which pole option to use, or whether to use user-specified area in the conservative regridding.
! The acceptable values and the default value for the optional arguments are listed below.
! 
! The arguments are:
!   \begin{description}
!   \item [srcFile]
!     The source grid file name.
!   \item [dstFile]
!     The destination grid file name.
!   \item [srcVarName]
!     The source variable name to be regridded.
!   \item [dstVarName]
!     The destination variable name to be regridded to.
!   \item [{[dstCoordVars]}]
!     The destination coordinate variable names if the dstVarName does not exist in the dstFile
!   \item [{[regridmethod]}]
!     The type of interpolation. Please see Section~\ref{opt:regridmethod} 
!     for a list of valid options. If not specified, defaults to 
!     {\tt ESMF\_REGRIDMETHOD\_BILINEAR}.
!   \item [{[polemethod]}]
!     A flag to indicate which type of artificial pole
!     to construct on the source Grid for regridding. Please see 
!     Section~\ref{const:polemethod} for a list of valid options.
!     The default value varies depending on the regridding method and the grid type and format.  
!   \item [{[regridPoleNPnts]}]
!     If {\tt polemethod} is set to {\tt ESMF\_POLEMETHOD\_NPNTAVG}, this argument is required to 
!     specify how many points should be averaged over at the pole.
!   \item [{[unmappedaction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Please see Section~\ref{const:unmappedaction} for a 
!           list of valid options. If not specified, {\tt unmappedaction} defaults to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!   \item [{[ignoreDegenerate]}]
!           Ignore degenerate cells when checking the input Grids or Meshes for errors. If this is set to true, then the 
!           regridding proceeds, but degenerate cells will be skipped. If set to false, a degenerate cell produces an error. 
!           If not specified, {\tt ignoreDegenerate} defaults to false.
!   \item [{[srcRegionalFlag]}]
!     If .TRUE., the source grid is a regional grid, otherwise,
!     it is a global grid.  The default value is .FALSE.
!   \item [{[dstRegionalFlag]}]
!     If .TRUE., the destination grid is a regional grid, otherwise,
!     it is a global grid.  The default value is .FALSE.
!   \item [{[verboseFlag]}]
!     If .TRUE., it will print summary information about the regrid parameters,
!     default to .FALSE.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOP

      
    type(ESMF_RegridMethod_Flag) :: localRegridMethod
    type(ESMF_PoleMethod_Flag)   :: localPoleMethod
    type(ESMF_FileFormat_Flag)   :: localsrcFileType
    type(ESMF_FileFormat_Flag)   :: localdstFileType
    integer            :: localPoleNPnts
    logical            :: localUserAreaFlag
    logical            :: localVerboseFlag
    integer            :: localrc
    type(ESMF_VM)      :: vm
    integer            :: PetNo, PetCnt
    type(ESMF_RouteHandle) :: routehandle
    type(ESMF_Mesh)    :: srcMesh, dstMesh
    type(ESMF_Grid)    :: srcGrid, dstGrid
    type(ESMF_LocStream) :: srcLocStream, dstLocStream
    type(ESMF_Field)   :: srcField, dstField
    type(ESMF_Array)  :: srcArray, dstArray
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_DistGrid) :: distgrid
    integer(ESMF_KIND_I4) :: maskvals(1)
    integer            :: ind
    integer, allocatable   :: srcdims(:), dstdims(:)
    integer            :: srcRank, dstRank, srccoordim, dstcoordim
    logical            :: isConserve, srcIsSphere, dstIsSphere
    logical            :: addCorners
    type(ESMF_MeshLoc) :: srcmeshloc, dstmeshloc
    logical            :: srcIsReg, dstIsReg
    logical            :: srcIsLocStream, dstIsLocStream
    logical            :: srcIsRegional, dstIsRegional, typeSetFlag
    character(len=256) :: methodStr
    real(ESMF_KIND_R8), pointer :: srcArea(:)
    real(ESMF_KIND_R8), pointer :: dstArea(:)
    real(ESMF_KIND_R8), pointer :: dstFrac(:), srcFrac(:)
    integer            :: regridScheme
    integer            :: i,j, k,l, bigFac, xpets, ypets, xpart, ypart, xdim, ydim
    logical            :: wasCompacted
    integer(ESMF_KIND_I4), pointer:: compactedFactorIndexList(:,:)
    real(ESMF_KIND_R8), pointer :: compactedFactorList(:)
    type(ESMF_UnmappedAction_Flag) :: localUnmappedaction
    character(len=256) :: argStr
    logical            :: useSrcCoordVar, useDstCoordVar
    logical            :: useSrcMask, useDstMask
    logical            :: useSrcCorner, useDstCorner
    integer            :: commandbuf(24)
    real(ESMF_KIND_R8)   :: commandbuf2(2)
    character(len=MAXNAMELEN*2) :: commandbuf1(2)
    character(len=MAXNAMELEN) :: srcLocStr, dstLocStr
    integer              :: srcVarType
    character(len=MAXNAMELEN*2)  :: srcVarStr, dstVarStr
    integer                  :: srcVarRank, dstVarRank, extraRank
    integer                :: srcVarDims(MAX_VARDIMS), dstVarDims(MAX_VARDIMS)
    integer 		   :: srcDimids(MAX_VARDIMS), dstDimids(MAX_VARDIMS)
    logical                :: srcVarExist, dstVarExist
    integer                 :: pos1, pos2
    character(len=MAXNAMELEN) :: srcCoordNames(2), dstCoordNames(2)
    logical                  :: terminateProg
    integer, pointer   :: lbnd(:), ubnd(:), tlbound(:,:), tubound(:,:)
    integer                :: ungridrank
    integer, pointer   :: start(:), count(:)
    real(ESMF_KIND_R8) :: srcMissingVal, dstMissingVal
    real(ESMF_KIND_R8), pointer::  varBuf1D(:), varBuf2D(:,:), varBuf3D(:,:,:), varBuf4D(:,:,:,:)
    real(ESMF_KIND_R8), pointer::  fptr1d(:), fptr2d(:,:), fptr3d(:,:,:), fptr4d(:,:,:,:)
    integer                 :: gridid, varid, start2(2), count2(2), start3(3), count3(3)
    real(ESMF_KIND_R8), pointer :: tmpfptr(:)
    type(ESMF_Array) :: tmparray
    real(ESMF_KIND_R8), pointer :: varbuffer(:)
    integer                 :: totalnodecnt, totalelmtcnt, totalcount
    type(ESMF_Distgrid) :: nodalDG, elementDG
    character(len=20) :: location
    integer                 :: ncStatus
    character(len=256) :: errmsg, attstr
    integer, parameter :: nf90_noerror = 0
    !real(ESMF_KIND_R8) :: starttime, endtime
    logical            :: localIgnoreDegenerate
   
#ifdef ESMF_NETCDF     
    !------------------------------------------------------------------------
    ! get global vm information
    !
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                           ESMF_ERR_PASSTHRU, &
                           ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                           ESMF_ERR_PASSTHRU, &
                           ESMF_CONTEXT, rcToReturn=rc)) return

    ! Default values
    terminateProg = .false.
    useSrcMask = .FALSE.
    useDstMask = .FALSE.
    localRegridMethod = ESMF_REGRIDMETHOD_BILINEAR
    localVerboseFlag = .false.
    srcIsRegional = .false.
    dstIsRegional = .false.
    srcIsLocStream = .false.
    dstIsLocStream = .false.
    localPoleNPnts = 0
    localIgnoreDegenerate = .false.
    
    if (present(regridMethod)) then
      localRegridMethod = regridMethod
    endif
    
    if (present(poleMethod)) then
    	 localPoleMethod = poleMethod
    else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
       localPoleMethod = ESMF_POLEMETHOD_NONE
    else
       localPoleMethod = ESMF_POLEMETHOD_ALLAVG
    endif

    if (localPoleMethod == ESMF_POLEMETHOD_NPNTAVG) then
      if (present(regridPoleNPnts)) then
  	localPoleNPnts = regridPoleNPnts
      else 
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
	  msg ="regridPoleNPnts argument is missing for ESMF_POLEMETHOD_NPNTAVG", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if ((localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) .and. &
	     (localPoleMethod /= ESMF_POLEMETHOD_NONE)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
	  msg ="Conserve method only works with no pole", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    if (present(unmappedaction)) then
      localUnmappedaction = unmappedaction
    else
      localUnmappedaction = ESMF_UNMAPPEDACTION_ERROR
    endif

    if (present(ignoreDegenerate)) then
      localIgnoreDegenerate = ignoreDegenerate
    endif

    if (present(srcRegionalFlag)) then
      srcIsRegional = srcRegionalFlag
    endif

    if (present(dstRegionalFlag)) then
      dstIsRegional = dstRegionalFlag
    endif

!    if (srcIsRegional .or. dstIsRegional) then
    if (srcIsRegional) then
      localPoleMethod = ESMF_POLEMETHOD_NONE
      localPoleNPnts = 0
    endif

    if (present(verboseFlag)) then
      localVerboseFlag = verboseFlag
    endif

!    if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
!       srcLocStr = 'face'
!       dstLocStr = 'face'
!    else
!       srcLocStr = 'node'
!       dstLocStr = 'node'
!    endif

    ! by default, variables are located at the center of the grid
    useSrcCorner = .FALSE.
    useDstCorner = .FALSE.
    if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
       srcmeshloc=ESMF_MESHLOC_ELEMENT
       dstmeshloc=ESMF_MESHLOC_ELEMENT
    else
       srcmeshloc=ESMF_MESHLOC_NODE
       dstmeshloc=ESMF_MESHLOC_NODE
    endif
    ! Find out the grid dimension at the root processor and broadcast to the rest
    if (PetNo == 0) then
      ! find file type
      call checkFileType(trim(srcFile), trim(srcVarName), srcVarExist, &
      		  localsrcFileType, srcVarStr, useSrcMask, srcMissingVal,  &
		  srcVarRank, srcVarDims, srcDimids, locStr=srcLocStr, &
		  vartype=srcVarType,  rc=localrc)
      if (.not. srcVarExist) localrc = ESMF_FAILURE
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
           terminateProg = .TRUE.
           goto 1110
      endif
      if (.not. ((srcVarType == NF90_FLOAT) .or. (srcVarType == NF90_DOUBLE))) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
  	        msg = " The program only supports src variable of type float or double ", &
                ESMF_CONTEXT, rcToReturn=rc)
           terminateProg = .TRUE.
           goto 1110
      endif

      if (trim(srcLocStr) .eq. 'node' .and. (localsrcFileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
          localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
  	        msg = " The source variable has to be located at the center of the grid ", &
                ESMF_CONTEXT, rcToReturn=rc)
           terminateProg = .TRUE.
           goto 1110
      endif
      if (localsrcFileType == ESMF_FILEFORMAT_UGRID .and.  &
          localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE .and. trim(srcLocStr) .eq. 'node') then	      
	   useSrcCorner = .TRUE.
           dstLocStr = 'node'  !in case the dest variable is not defined
      else
           dstLocStr = 'face'
      endif
      call checkFileType(trim(dstFile), trim(dstVarName), dstVarExist, &
      		   localdstFileType, dstVarStr, useDstMask, dstMissingVal, &
		   dstVarRank, dstVarDims, dstDimids, locStr=dstLocStr, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
           terminateProg = .TRUE.
           goto 1110
      endif
    if (dstLocStr .eq. 'node' .and. (localdstFileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
          localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
  	        msg = " The source variable has to be located at the center of the grid ", &
                ESMF_CONTEXT, rcToReturn=rc)
           terminateProg = .TRUE.
           goto 1110
      endif
      if (localdstFileType == ESMF_FILEFORMAT_UGRID .and.  &
          localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE .and. dstLocStr .eq. 'node') then	      
	   useDstCorner = .TRUE.
	   dstmeshloc=ESMF_MESHLOC_NODE
      endif
      if (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
        allocate(srcdims(2))
        ! The coordinates string can be either longitude, latitude, or latitude, longitude.
        ! ESMF_GridSpecInq() will check the units and the returned srccorddim array will always
        ! have longitude first, followed by latitude
	pos1 = index(srcVarStr(1:)," ")
        srcCoordNames(1) = srcVarStr(1:pos1-1)
        pos2 = index(srcVarStr(pos1+1:)," ")
        srcCoordNames(2)=srcVarStr(pos1+1:pos1+pos2-1)
        call ESMF_GridSpecInq(srcFile, srccoordim, srcdims, coord_names=srcCoordNames, rc=localrc)
	if (localVerboseFlag .and. localrc /= ESMF_SUCCESS) then 
          write(*,*)
	        print *, 'ERROR: Unable to get dimension information from:', srcfile
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
           terminateProg = .TRUE.
           goto 1110
        endif
	srcIsReg = .true.
	srcRank = 2
      else
	   srcIsReg = .false.
	   srcRank = 1
      endif

      if (.not. dstVarExist) then
         extraRank = srcVarRank - srcRank
         useDstMask = .false.
	 dstVarRank = dstVarRank + extraRank
         dstVarDims(dstVarRank-extraRank+1:dstVarRank)=srcVarDims(srcRank+1:srcVarRank)
         ! Also check if there is a time dimension in the destination file and
         ! if that matches with the time dimension of the source grid
       
         ! check if the last dimension of the srcVar is named "time", if so, check if
         ! there is a matching time dimension in the dstFile. If there is a time dimension
         ! in the dstFile, the value has to match with the src time dimension. If not exist
         ! create one and create the dstvar as well
         call CreateDstVar(srcFile, dstFile, localdstFileType, srcVarName, &
	   	    dstVarName, dstVarDims, dstVarRank, dstVarStr, &
		    dstLocStr,dstDimids, localrc)
         if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
             terminateProg = .TRUE.
             goto 1110
         endif
      endif
      if (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
        allocate(dstdims(2))
	pos1 = index(dstVarStr(1:)," ")
        dstCoordNames(1) = dstVarStr(1:pos1-1)
        pos2 = index(dstVarStr(pos1+1:)," ")
        dstCoordNames(2)=dstVarStr(pos1+1:pos1+pos2-1)
        call ESMF_GridSpecInq(dstFile, dstcoordim, dstdims, coord_names=dstCoordNames, rc=localrc)
        if (localVerboseFlag .and. localrc /= ESMF_SUCCESS) then 
           write(*,*)
	   print *, 'ERROR: Unable to get dimension information from:', dstfile
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        dstIsReg = .true.
	dstRank = 2
      else
        dstIsReg = .false.
	dstRank = 1
      endif

1110  continue      
      commandbuf(:) = 0
      if (terminateProg) then
        commandbuf(1) = -9999
        call ESMF_VMBroadcast(vm, commandbuf, size(commandbuf), 0, rc=rc)
        if (present(rc)) rc=ESMF_FAILURE
	return
      else
        if (srcIsReg) commandbuf(1) = 1
        if (dstIsReg) commandbuf(2) = 1
        if (srcIsReg) then
          commandbuf(3) = srcdims(1)
     	  commandbuf(4) = srcdims(2)
        endif 
        if (dstIsReg) then 
          commandbuf(5) = dstdims(1)
	  commandbuf(6) = dstdims(2)   
        endif 
        commandbuf(7)=localsrcfiletype%fileformat
        commandbuf(8)=localdstfiletype%fileformat
	if (useSrcMask) then
	   commandbuf(9) = 1
	   commandbuf2(1)=srcMissingVal
        endif
	if (useDstMask) then 
 	   commandbuf(10) = 1
	   commandbuf2(2)=dstMissingVal
        endif
        if (useSrcCorner) then 
           commandbuf(11)=1
        endif
        if (useDstCorner) then 
           commandbuf(12)=1
        endif
        commandbuf(13) = srcVarRank
        do i=1,srcVarRank
          commandbuf(13+i) = srcVarDims(i)
        enddo
        commandbuf(14+srcVarRank) = dstVarRank
        do i=1,dstVarRank
          commandbuf(14+srcVarRank+i) = dstVarDims(i)
        enddo
        commandbuf1(1) = srcVarStr
        commandbuf1(2) = dstVarStr

        call ESMF_VMBroadcast(vm, commandbuf, size(commandbuf), 0, rc=rc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMBroadcast(vm, commandbuf1, len(commandbuf1)*size(commandbuf1), 0, rc=rc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMBroadcast(vm, commandbuf2, size(commandbuf2), 0, rc=rc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    else
      ! Not the Root PET
      allocate(srcdims(2),dstdims(2))
      call ESMF_VMBroadcast(vm, commandbuf, size(commandbuf), 0, rc=rc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      if (commandbuf(1) == -9999) then
        if (present(rc)) rc=ESMF_FAILURE
	return
      endif
      if (commandbuf(1) == 1) then
         srcIsReg = .true.
	 srcRank = 2
      else
         srcIsReg = .false.
	 srcRank = 1
      endif        
      if (commandbuf(2) == 1) then
         dstIsReg = .true.
	 dstRank = 2
      else
         dstIsReg = .false.
	 dstRank = 1
      endif        
      srcdims(1) = commandbuf(3)  
      srcdims(2) = commandbuf(4)  
      dstdims(1) = commandbuf(5)  
      dstdims(2) = commandbuf(6)  
      localsrcfiletype%fileformat = commandbuf(7)
      localdstfiletype%fileformat = commandbuf(8)
      if (commandbuf(9) == 1) then
         useSrcMask = .true.
      endif        
      if (commandbuf(10) == 1) then
         useDstMask = .true.
      endif        
      if (commandbuf(11) == 1) then
         useSrcCorner = .true.
      endif
      if (commandbuf(12) == 1) then
         useDstCorner = .true.
      endif
      srcVarRank = commandbuf(13)
      do i=1, srcVarRank
         srcVarDims(i)=commandbuf(13+i)
      enddo  
      dstVarRank = commandbuf(14+srcVarRank)
      do i=1, dstVarRank
         dstVarDims(i)=commandbuf(14+srcVarRank+i)
      enddo  
      call ESMF_VMBroadcast(vm, commandbuf1, len(commandbuf1)*size(commandbuf1), 0, rc=rc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      srcVarStr = trim(commandbuf1(1))
      dstVarStr = trim(commandbuf1(2))
      
      if (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
         pos1 = index(srcVarStr(1:)," ")
         srcCoordNames(1) = srcVarStr(1:pos1-1)
         pos2 = index(srcVarStr(pos1+1:)," ")
         srcCoordNames(2)=srcVarStr(pos1+1:pos1+pos2-1)
      endif
      if (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
         pos1 = index(dstVarStr(1:)," ")
         dstCoordNames(1) = dstVarStr(1:pos1-1)
         pos2 = index(dstVarStr(pos1+1:)," ")
         dstCoordNames(2)=dstVarStr(pos1+1:pos1+pos2-1)
      endif
      call ESMF_VMBroadcast(vm, commandbuf2, size(commandbuf2), 0, rc=rc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      srcMissingVal=commandbuf2(1)
      dstMissingVal = commandbuf2(2)
    endif

    ! Print the regrid options
    if (localVerboseFlag .and. PetNo == 0) then
  	print *, "Starting weight generation with these inputs: "
	print *, "  Source File: ", trim(srcfile)
        print *, "  Destination File: ", trim(dstfile)
        print *, "  Source variable name: ", trim(srcVarName)
        print *, "  Destination variable name: ", trim(dstVarName)
        if (.not. dstVarExist) then
	   print *, "  Destination variable does not exist in ", trim(dstfile)
        endif
        if (useSrcMask) then
	    print *, "  Souce Grid has a mask, using missingvalue ", srcMissingVal
        endif 
        if (localsrcfiletype == ESMF_FILEFORMAT_UGRID) then
            print *, "  Source File is in UGRID format with mesh name ", trim(srcVarStr)
	else 
            print *, "  Source File is in GRIDSPEC format with coordinate names ", trim(srcVarStr)
        endif
        if (srcIsRegional) then
	      print *, "  Source Grid is a regional grid"
        else 
	      print *, "  Source Grid is a global grid"
	endif
        if (localdstfiletype == ESMF_FILEFORMAT_UGRID) then
           print *, "  Destination File is in UGRID format with mesh name ", trim(dstVarStr)
        else 
           print *, "  Destination File is in GRIDSPEC format with coordinate names ", trim(dstVarStr)
        endif
        if (dstIsRegional) then
	      print *, "  Destination Grid is a regional grid"
        else 
	      print *, "  Destination Grid is a global grid"
	endif
        if (useDstMask) then
	    print *, "  Destination Grid has a mask, using missing value ", dstMissingVal
        endif 
        if (localRegridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
          print *, "  Regrid Method: bilinear"
        else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
          print *, "  Regrid Method: conserve"
        else if (localRegridMethod == ESMF_REGRIDMETHOD_PATCH) then
          print *, "  Regrid Method: patch"
        else if (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_STOD) then
          print *, "  Regrid Method: nearest source to destination"
        else if (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_DTOS) then
          print *, "  Regrid Method: nearest destination to source"
        endif
        if (localPoleMethod .eq. ESMF_POLEMETHOD_NONE) then
	      print *, "  Pole option: NONE"
	else if (localPoleMethod .eq. ESMF_POLEMETHOD_ALLAVG) then
	      print *, "  Pole option: ALL"
	else if (localPoleMethod .eq. ESMF_POLEMETHOD_TEETH) then
	      print *, "  Pole option: TEETH"
	else
	      print *, "  Pole option: ", localPoleNPnts
        endif
        if (localUnmappedaction .eq. ESMF_UNMAPPEDACTION_IGNORE) then
	      print *, "  Ignore unmapped destination points"
        endif
        if (localIgnoreDegenerate) then
              print *, "  Ignore degenerate cells in the input grids"
        endif   
        write(*,*)
    endif 

    ! Set flags according to the regrid method
    if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
      isConserve=.true.
      addCorners=.true.
    else
      isConserve=.false.
      addCorners=.false.
    endif

    if (srcIsRegional .and. dstIsRegional) then
      regridScheme = ESMF_REGRID_SCHEME_REGION3D
      srcIsSphere=.false.
      dstIsSphere=.false.
    else if (srcIsRegional) then
      regridScheme = ESMF_REGRID_SCHEME_REGTOFULL3D
      srcIsSphere=.false.
      dstIsSphere=.true.
    else if (dstIsRegional) then  
      regridScheme = ESMF_REGRID_SCHEME_FULLTOREG3D
      srcIsSphere=.true.
      dstIsSphere=.false.
    else
      regridScheme = ESMF_REGRID_SCHEME_FULL3D
      srcIsSphere=.true.
      dstIsSphere=.true.
    endif

# if 0
      srcIsReg = .false.
      dstIsReg = .false.
#endif

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

    if (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
      srcGrid = ESMF_GridCreate(srcfile, localsrcfiletype, (/xpart,ypart/), &
	        addCornerStagger=addCorners, &
	        addMask=useSrcMask, varname=trim(srcVarName), isSphere=srcIsSphere, &
	        coordNames = srcCoordNames, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      srcField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! if srcfile is not GRIDSPEC, it is UGRID 
      call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      if (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_STOD .or. &
          localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_DTOS) then
        srcIsLocStream = .TRUE.
        if (useSrcMask) then 
          srcLocStream = ESMF_LocStreamCreate(srcfile, & 
       		      fileformat=localSrcFileType, &
		      indexflag=ESMF_INDEX_DELOCAL, & 
		      meshname=trim(srcVarStr), &
		      varname=trim(srcVarName), &
		      centerflag=.not. useSrcCorner, rc=localrc)
        else
          srcLocStream = ESMF_LocStreamCreate(srcfile, & 
       		      fileformat=localSrcFileType, &
		      indexflag=ESMF_INDEX_DELOCAL, & 
		      meshname=trim(srcVarStr), &
		      centerflag=.not. useSrcCorner, rc=localrc)
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        srcField=ESMF_FieldCreate(srcLocStream,arrayspec,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
        if (useSrcMask) then 
          srcMesh = ESMF_MeshCreate(srcfile, localsrcfiletype, &
              meshname = trim(srcVarStr), maskFlag =srcmeshloc, &
	    ! BOB: NEVER DO DUAL WHEN CONSERVE 
              convertToDual= .false., &
  	      varname=trim(srcVarName), rc=localrc)
        else
          srcMesh = ESMF_MeshCreate(srcfile, localsrcfiletype, &
              meshname = trim(srcVarStr), &
	    ! BOB: NEVER DO DUAL WHEN CONSERVE 
              convertToDual= .false., &
  	      rc=localrc)
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        srcField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=srcmeshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else ! Non-conservative, but also not nearest neighbor (this would be things like bilinear, etc.)
        if (useSrcMask) then 
          srcMesh = ESMF_MeshCreate(srcfile, localsrcfiletype, &
              meshname = trim(srcVarStr), maskFlag =srcmeshloc, &
	      convertToDual= .not. useSrcCorner, &
  	      varname=trim(srcVarName), rc=localrc)
        else
          srcMesh = ESMF_MeshCreate(srcfile, localsrcfiletype, &
              meshname = trim(srcVarStr), &
	      convertToDual=.not. useSrcCorner, &
  	      rc=localrc)
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        srcField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=srcmeshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
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

    if (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
      dstGrid = ESMF_GridCreate(dstfile, localdstfiletype, (/xpart,ypart/), &
         addCornerStagger=addCorners, &
         addMask=useDstMask, varname=trim(dstVarName), isSphere=dstIsSphere, &
         coordNames = dstCoordNames, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      dstField = ESMF_FieldCreate(dstGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! dstfile is UGRID
      call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      if (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) then
        dstIsLocStream = .TRUE.
        if (useDstMask) then
           dstLocStream = ESMF_LocStreamCreate(dstfile, & 
       		      fileformat=localDstFileType, &
		      indexflag=ESMF_INDEX_DELOCAL, & 
		      meshname=trim(dstVarStr), &
		      varname=trim(dstVarName), &
		      centerflag=.not. useDstCorner, rc=localrc)
        else 
           dstLocStream = ESMF_LocStreamCreate(dstfile, & 
       		      fileformat=localDstFileType, &
		      indexflag=ESMF_INDEX_DELOCAL, & 
		      meshname=trim(dstVarStr), &
		      centerflag=.not. useDstCorner, rc=localrc)
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        dstField=ESMF_FieldCreate(dstLocStream,arrayspec,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else
        if (useDstMask) then
          dstMesh = ESMF_MeshCreate(dstfile, localdstfiletype, &
            meshname = trim(dstVarStr), maskFlag=dstmeshloc, &
	    ! BOB: NEVER DO DUAL WHEN CONSERVE
	    convertToDual= .false., &
            varname=trim(dstVarName), rc=localrc)
        else  
          dstMesh = ESMF_MeshCreate(dstfile, localdstfiletype, &
            meshname = trim(dstVarStr), &
	    ! BOB: NEVER DO DUAL WHEN CONSERVE
	    convertToDual= .false., &
            rc=localrc)
        endif
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        dstField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=dstmeshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    !call ESMF_VMBarrier(vm)
    !call ESMF_VMWtime(starttime, rc=localrc)
    maskvals(1) = 0
    if (localPoleNPnts <= 0) localPoleNPnts = 1

    if (useSrcMask .and. useDstMask) then
      call ESMF_FieldRegridStore(srcField, dstField, & 
	      srcMaskValues = maskvals, dstMaskValues = maskvals, &
	      unmappedaction=localUnmappedaction, &
	      ignoreDegenerate=localIgnoreDegenerate, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              normType=ESMF_NORMTYPE_FRACAREA, &
	      routehandle = routehandle, &
	      rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if (useSrcMask) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	      srcMaskValues = maskvals, &
	      unmappedaction=localUnmappedaction, &
	      ignoreDegenerate=localIgnoreDegenerate, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              normType=ESMF_NORMTYPE_FRACAREA, &
	      routehandle = routehandle, &
	      rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if (useDstMask) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	      dstMaskValues = maskvals, &
	      unmappedaction=localUnmappedaction, &
	      ignoreDegenerate=localIgnoreDegenerate, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              normType=ESMF_NORMTYPE_FRACAREA, &
	      routehandle = routehandle, &
	      rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else	
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	      unmappedaction=localUnmappedaction, &
	      ignoreDegenerate=localIgnoreDegenerate, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              normType=ESMF_NORMTYPE_FRACAREA, &
	      routehandle = routehandle, &
	      rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    call ESMF_FieldDestroy(srcField)
    call ESMF_FieldDestroy(dstField)

    !!  Read the source variable, construct the field and do the regrid 
    call ESMF_ArraySpecSet(arrayspec, srcVarRank, ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
    ungridrank = srcVarRank - srcRank
    if (ungridrank > 0) then
      allocate(lbnd(ungridrank), ubnd(ungridrank))
      lbnd(:)=1
      do i=1, ungridrank
         ubnd(i)=srcVarDims(i+srcRank)
      enddo
      if (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
           srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
	                  staggerloc=ESMF_STAGGERLOC_CENTER, &
			  ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
			  gridToFieldMap=(/ungridrank+1, ungridrank+2/), rc=localrc)
      else 
          !! UGRID file
          if (srcIsLocStream) then
            srcField = ESMF_FieldCreate(srcLocStream, arrayspec, &
			  ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
			  gridToFieldMap=(/ungridrank+1/), rc=localrc)
          else
            srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
			  meshloc =srcmeshloc, & 
			  ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
			  gridToFieldMap=(/ungridrank+1/), rc=localrc)
	  endif		  
      endif
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    else
      if (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
           srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
	                  staggerloc=ESMF_STAGGERLOC_CENTER, &
			  rc=localrc)
      else 
          !! UGRID file
          if (srcIsLocStream) then
            srcField = ESMF_FieldCreate(srcLocStream, arrayspec, &
			  rc=localrc)
          else			  
            srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
			  meshloc = srcmeshloc, & 
			  rc=localrc)
          endif			        
      endif
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif	  
    
    ! Read the data from the Source Grid File to the srcField 
    ! The code to deal with GRIDSPEC and UGRID are diffferent because the distgrid is 
    ! arbitrary for a Mesh and there is no minIndexPDe and maxIndexPDe
      
    if (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
       call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
       allocate(tlbound(srcRank,petCnt),tubound(srcRank, petCnt))
       call ESMF_ArrayGet(srcArray, distgrid=distgrid, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_DistgridGet(distgrid, minIndexPDe=tlbound, maxIndexPDe=tubound, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       if (srcVarRank == 2) then
          call ESMF_ArrayGet(srcArray, farrayPtr=fptr2d, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
          allocate(start(2), count(2))
          start = tlbound(:,PetNo+1)
          count = tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
          call ESMF_IO_NCPutGetVar(srcFile, srcVarName, fptr2d, start=start, count=count, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          deallocate(start, count)
       else if (srcVarRank == 3) then
           allocate(start(3), count(3))
           start(1:srcRank)=tlbound(:,PetNo+1)
           start(srcRank+1:3)=1
           count(1:srcRank)=tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
           count(srcRank+1:3)=ubnd(:)
           allocate(varBuf3D(count(1), count(2), count(3)))
           call ESMF_IO_NCPutGetVar(srcFile, srcVarName, varBuf3D, start=start, count=count, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           call ESMF_ArrayGet(srcArray, farrayPtr=fptr3d, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           do i=1,count(3)
              fptr3d(i,:,:)=varBuf3D(:,:,i)
           enddo
           deallocate(varBuf3D)  
           deallocate(start, count)
       else if (srcVarRank == 4) then
           allocate(start(4), count(4))
           start(1:srcRank)=tlbound(:,PetNo+1)
           start(srcRank+1:4)=1
           count(1:srcRank)=tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
           count(srcRank+1:4)=ubnd
           allocate(varBuf4D(count(1), count(2), count(3), count(4)))
           call ESMF_IO_NCPutGetVar(srcFile, srcVarName, varBuf4D, start=start, count=count, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           call ESMF_ArrayGet(srcArray, farrayPtr=fptr4d, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i=1,count(3)
              do j=1, count(4)
       	         fptr4d(i,j,:,:) = varBuf4D(:,:,i,j)
              enddo
            enddo
            deallocate(varBuf4D)
            deallocate(start, count)
       endif
       deallocate(tlbound, tubound)
    else !! If the filetype is UGRID
       !! For UGRID, will read the data in parallel into an Array, then redist it using the nodal or 
       !! element distgrid of the mesh
       call ESMF_UGridInq(srcFile, trim(srcVarStr) , nodeCount=totalnodecnt, &
       	    			       elementCount=totalelmtcnt, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      if (srcIsLocStream) then
         call ESMF_LocStreamGet(srcLocStream, distgrid=distgrid, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
         call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
         tmpArray=ESMF_ArrayCreate(distgrid, arrayspec, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       else
         call ESMF_MeshGet(srcMesh, nodalDistgrid=nodalDG, elementDistgrid=elementDG, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
     	    
         call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
         if (srcmeshloc == ESMF_MESHLOC_NODE) then
       	    tmpArray=ESMF_ArrayCreate(nodalDG, arrayspec, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
         else
       	    tmpArray=ESMF_ArrayCreate(elementDG, arrayspec, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
         endif
       endif  
       if (useSrcCorner) then
	    if (PetNo==0)  allocate(varbuffer(totalnodecnt))
	    location='node'
            totalcount = totalnodecnt
       else
	    if (PetNo==0)  allocate(varbuffer(totalelmtcnt))
	    location='face'
	    totalcount = totalelmtcnt
       endif
    
       call ESMF_ArrayGet(tmpArray, farrayPtr=tmpfptr, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

       !! Read in the variable in PET 0 and redistribute it, read one 2D slice at a time to save memory
       if (PetNo==0) then
        ! Open the grid and mosaic files
        ncStatus = nf90_open (path=trim(srcFile), mode=nf90_nowrite, ncid=gridid)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          trim(srcFile), &
          rc)) return
         ncStatus = nf90_inq_varid( gridid, srcVarName, varid)
         errmsg = "variable "//trim(srcVarName)// " in "//trim(srcFile)
         if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
         ncStatus = nf90_get_att(gridid, varid, 'location', attstr)
         if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
	 if (attstr(1:4) .ne. location) then
  	    errmsg = "- variable "//trim(srcVarName)//" is not defined on location "//trim(location)
            call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg=errmsg, ESMF_CONTEXT, rcToReturn=rc) 
            return
         endif
	 if (srcVarRank==1) then
	    ncStatus = nf90_get_var(gridid, varid, varbuffer)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
	    call ESMF_ArrayScatter(srcArray, varbuffer, 0, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
	 else if (srcVarRank==2) then
            call ESMF_ArrayGet(srcArray, farrayPtr=fptr2d, rc=localrc)
            do i=1,ubnd(1)
	       start2(1)=1
	       start2(2)=i
               count2(1)=totalcount
               count2(2)=1
  	       ncStatus = nf90_get_var(gridid, varid, varbuffer, start2, count2)
               if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,&
                  errmsg, &
                  rc)) return
   	      call ESMF_ArrayScatter(tmpArray, varbuffer, 0, rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
	      fptr2d(i,:)=tmpfptr
            enddo
	 else if (srcVarRank==3) then
            call ESMF_ArrayGet(srcArray, farrayPtr=fptr3d, rc=localrc)
            do i=1,ubnd(1)
	      do j=1,ubnd(2)
 	         start3(1)=1
	         start3(2)=i
		 start3(3)=j
                 count3(1)=totalcount
                 count3(2:3)=1
   	         ncStatus = nf90_get_var(gridid, varid, varbuffer, start3, count3)
                 if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
    	         call ESMF_ArrayScatter(tmpArray, varbuffer, 0, rc=localrc)
                 if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
	         fptr3d(i,j,:)=tmpfptr
              enddo
            enddo
         else 
            call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
	        msg = " Undistributed dimension > 2 is not supported", &
                ESMF_CONTEXT, rcToReturn=rc)
	    return
	 endif    
	 deallocate(varbuffer)
       else  !! non-root
        if (srcVarRank == 1) then
           call ESMF_ArrayScatter(srcArray,varbuffer, 0, rc=localrc)		 
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
           !! copy the data to the actual srcField       
  	else if (srcVarRank==2) then
           call ESMF_ArrayGet(srcArray, farrayPtr=fptr2d, rc=localrc)
           do i=1,ubnd(1)
              call ESMF_ArrayScatter(tmpArray,varbuffer, 0, rc=localrc)		 
              if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
   	      fptr2d(i,:)=tmpfptr
           enddo
      	else if (srcVarRank==3) then  
           call ESMF_ArrayGet(srcArray, farrayPtr=fptr3d, rc=localrc)
           do i=1,ubnd(1)
  	     do j=1,ubnd(2)
   	        call ESMF_ArrayScatter(tmpArray, varbuffer, 0, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
  	        fptr3d(i,j,:)=tmpfptr
	     enddo
           enddo
         else 
            call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
	        msg = " Undistributed dimension > 2 is not supported", &
                ESMF_CONTEXT, rcToReturn=rc)
	    return
	 endif
	 call ESMF_ArrayDestroy(tmpArray)
       endif
    endif
    !! Construct the destination field
    !! First check if the undistributed grid dimensions match with the source variable
    if ((dstVarRank - dstRank) /= ungridrank) then
         call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
	      msg = " the undistributed dimensions of the dst variable is different from the source variable", &
              ESMF_CONTEXT, rcToReturn=rc)
	  return
    endif    
    do i=1, ungridrank
       if (ubnd(i) /= dstVarDims(i+dstRank)) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
  	      msg = " the undistributed dimensions of the dst variable is different from the source variable", &
              ESMF_CONTEXT, rcToReturn=rc)
	      return
       endif	      
    enddo 

    call ESMF_ArraySpecSet(arrayspec, dstVarRank, ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if (ungridrank > 0) then
       if (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
            dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
	                  staggerloc=ESMF_STAGGERLOC_CENTER, &
			  ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
			  gridToFieldMap=(/ungridrank+1, ungridrank+2/), rc=localrc)
       else 
           !! UGRID file
	   if (dstIsLocStream) then
             dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
			  ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
			  gridToFieldMap=(/ungridrank+1/), rc=localrc)
           else
             dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
			  meshloc = dstmeshloc, & 
			  ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
			  gridToFieldMap=(/ungridrank+1/), rc=localrc)
           endif			  
       endif
       if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    else
       if (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
           dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
	                  staggerloc=ESMF_STAGGERLOC_CENTER, &
			  rc=localrc)
      else 
           !! UGRID file
	   if (dstIsLocStream) then
             dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
			  rc=localrc)
           else
             dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
			  meshloc = dstmeshloc, rc=localrc)
           endif
      endif
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif

!   if (useDstMask) then
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    	!! initialize the destination field with the _FillValue
	if (dstVarRank == 1) then	
		call ESMF_ArrayGet(dstArray, farrayptr = fptr1d, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
		fptr1d(:)=dstMissingVal
	else if (dstVarRank == 2) then	
		call ESMF_ArrayGet(dstArray, farrayptr = fptr2d, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
		fptr2d(:,:)=dstMissingVal
        else if (dstVarRank == 3) then
		call ESMF_ArrayGet(dstArray, farrayptr = fptr3d, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
		fptr3d(:,:,:)=dstMissingVal
        else if (dstVarRank == 4) then
		call ESMF_ArrayGet(dstArray, farrayptr = fptr4d, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
		fptr4d(:,:,:,:)=dstMissingVal
        endif
!    endif

    !! Call regird   
    call ESMF_FieldRegrid(srcField, dstField, routehandle, zeroregion=ESMF_REGION_SELECT, &
    	 	          rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !!  Write the destination field out to the dest_grid_file
    call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! write out the destination array sequentially
    if (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
      allocate(tlbound(dstRank,petCnt),tubound(dstRank, petCnt))
      call ESMF_ArrayGet(dstArray, distgrid=distgrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_DistgridGet(distgrid, minIndexPDe=tlbound, maxIndexPDe=tubound, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      do i=0, PetCnt
      if (PetNo == i) then
    	  if (dstVarRank == 2) then
            call ESMF_ArrayGet(dstArray, farrayPtr=fptr2d, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
          	  ESMF_CONTEXT, rcToReturn=rc)) return
            allocate(start(2), count(2))
  	    start(1:dstRank) = tlbound(:,PetNo+1)
            count(1:dstRank) = tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
            call ESMF_IO_NCPutGetVar(dstFile, dstVarName, fptr2d, start=start, count=count, &
	         putflag=.TRUE.,rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(start, count)
          else if (dstVarRank == 3) then
            call ESMF_ArrayGet(dstArray, farrayPtr=fptr3d, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
            allocate(start(3), count(3))
            start(1:dstRank)=tlbound(:,PetNo+1)
            start(dstRank+1:3)=1
            count(1:dstRank)=tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
            count(dstRank+1:3)=ubnd
	    allocate(varBuf3D(count(1),count(2),count(3)))
            do j=1,count(3)
	         varBuf3D(:,:,j)=fptr3d(j,:,:)
            enddo
            call ESMF_IO_NCPutGetVar(dstFile, dstVarName, varBuf3D, start=start, count=count, &
	    	 putflag=.TRUE.,rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(varBuf3D)
            deallocate(start, count)
          else if (dstVarRank == 4) then
            call ESMF_ArrayGet(dstArray, farrayPtr=fptr4d, rc=localrc)
            allocate(start(4), count(4))
            start(1:dstRank)=tlbound(:,PetNo+1)
            start(dstRank+1:4)=1
            count(1:dstRank)=tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
            count(dstRank+1:4)=ubnd
	    allocate(varBuf4D(count(1),count(2),count(3),count(4)))
            do k=1,count(3)
  	        do j=1,count(4)
	         varBuf4D(:,:,k,j)=fptr4d(k,j,:,:)
	        enddo
            enddo
            call ESMF_IO_NCPutGetVar(dstFile, dstVarName, varBuf4D, start=start, count=count, putflag=.TRUE.,rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
	    deallocate(varBuf4D)
            deallocate(start, count)
          endif
        endif
        call ESMF_VMBarrier(vm)
      enddo
      deallocate(tlbound, tubound)
    else ! for UGRID
      ! Gather every slice into the root and write out from the root one slice at a time
      call ESMF_UGridInq(dstFile, trim(dstVarStr) , nodeCount=totalnodecnt, &
       	    			       elementCount=totalelmtcnt, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      if (dstIsLocStream) then
         call ESMF_LocStreamGet(dstLocStream, distgrid=distgrid, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
         call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
         tmpArray=ESMF_ArrayCreate(distgrid, arrayspec, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
      else !Mesh code
         call ESMF_MeshGet(dstMesh, nodalDistgrid=nodalDG, elementDistgrid=elementDG, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
     	    
         call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
         if (dstmeshloc == ESMF_MESHLOC_NODE) then
       	    tmpArray=ESMF_ArrayCreate(nodalDG, arrayspec, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
         else
       	    tmpArray=ESMF_ArrayCreate(elementDG, arrayspec, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
         endif      
      endif
      if (useDstCorner) then
	    if (PetNo==0)  allocate(varbuffer(totalnodecnt))
	    location='node'
            totalcount = totalnodecnt
      else
	    if (PetNo==0)  allocate(varbuffer(totalelmtcnt))
	    location='face'
	    totalcount = totalelmtcnt
      endif
      call ESMF_ArrayGet(tmpArray, farrayPtr=tmpfptr, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
      if (PetNo==0) then
        ! Open the grid and mosaic files
        ncStatus = nf90_open (path=trim(dstFile), mode=nf90_write, ncid=gridid)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          trim(dstFile), &
          rc)) return
         ncStatus = nf90_inq_varid( gridid, dstVarName, varid)
         errmsg = "variable "//trim(dstVarName)// " in "//trim(dstFile)
         if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
         ncStatus = nf90_get_att(gridid, varid, 'location', attstr)
         if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
	 if (attstr(1:4) .ne. location) then
  	    errmsg = "- variable "//trim(dstVarName)//" is not defined on location "//trim(location)
            call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg=errmsg, ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
	 if (dstVarRank==1) then
  	    call ESMF_ArrayGather(dstArray, varbuffer, 0, rc=localrc)
	    ncStatus = nf90_put_var(gridid, varid, varbuffer)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
	 else if (dstVarRank==2) then
            call ESMF_ArrayGet(dstArray, farrayPtr=fptr2d, rc=localrc)
            do i=1,ubnd(1)
	       tmpfptr=fptr2d(i,:)
    	       call ESMF_ArrayGather(tmpArray, varbuffer, 0, rc=localrc)
	       start2(1)=1
	       start2(2)=i
               count2(1)=totalcount
               count2(2)=1
  	       ncStatus = nf90_put_var(gridid, varid, varbuffer, start2, count2)
               if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,&
                  errmsg, &
                  rc)) return
            enddo
	 else if (dstVarRank==3) then
            call ESMF_ArrayGet(dstArray, farrayPtr=fptr3d, rc=localrc)
            do i=1,ubnd(1)
	      do j=1,ubnd(2)
 	         tmpfptr=fptr3d(i,j,:)
    	         call ESMF_ArrayGather(tmpArray, varbuffer, 0, rc=localrc)
 	         start3(1)=1
	         start3(2)=i
		 start3(3)=j
                 count3(1)=totalcount
                 count3(2:3)=1
   	         ncStatus = nf90_put_var(gridid, varid, varbuffer, start3, count3)
                 if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
              enddo
            enddo
         else 
            call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
	        msg = " Undistributed dimension > 2 is not supported", &
                ESMF_CONTEXT, rcToReturn=rc)
	    return
	 endif    
	 deallocate(varbuffer)
         ncStatus=nf90_close(gridid)
         if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
       else  !! non-root
            if (dstVarRank == 1) then
                call ESMF_ArrayGather(dstArray,varbuffer, 0, rc=localrc)		 
                !! copy the data to the actual dstField       
  	    else if (dstVarRank==2) then
                call ESMF_ArrayGet(dstArray, farrayPtr=fptr2d, rc=localrc)
                do i=1,ubnd(1)
  	           tmpfptr=fptr2d(i,:)
                   call ESMF_ArrayGather(tmpArray,varbuffer, 0, rc=localrc)		 
		enddo
      	    else if (dstVarRank==3) then  
               call ESMF_ArrayGet(dstArray, farrayPtr=fptr3d, rc=localrc)
               do i=1,ubnd(1)
  	         do j=1,ubnd(2)
  	            tmpfptr=fptr3d(i,j,:)
   	            call ESMF_ArrayGather(tmpArray, varbuffer, 0, rc=localrc)
		 enddo
               enddo
            else 
               call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
	          msg = " Undistributed dimension > 2 is not supported", &
                  ESMF_CONTEXT, rcToReturn=rc)
	       return
	    endif
	    call ESMF_ArrayDestroy(tmpArray)
       endif
    endif  ! UGRID
    if (ungridrank > 0) deallocate(lbnd, ubnd)
    if (allocated(srcdims)) deallocate(srcdims)    
    if (allocated(dstdims)) deallocate(dstdims)    

    !call ESMF_VMBarrier(vm)
    !call ESMF_VMWtime(endtime, rc=localr)

    call ESMF_FieldDestroy(srcField)
    call ESMF_FieldDestroy(dstField)

    if (srcIsLocStream) then
       call ESMF_LocStreamDestroy(srcLocStream)
    elseif (srcIsReg) then
       call ESMF_GridDestroy(srcGrid)
    else
       call ESMF_MeshDestroy(srcMesh)
    endif   
    if (dstIsLocStream) then
       call ESMF_LocStreamDestroy(dstLocStream)
    else if (dstIsReg) then
       call ESMF_GridDestroy(dstGrid)
    else
       call ESMF_MeshDestroy(dstMesh)
    endif   
    rc = ESMF_SUCCESS
    return 
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, & 
      msg="- ESMF_NETCDF not defined when lib was compiled", & 
      ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif
  end subroutine ESMF_FileRegrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CheckFileType"
  subroutine CheckFileType(filename, varname, varexist, filetype, attstr, &
   haveMask, missingval, ndims, dims, dimids, locstr, vartype, coordnames, rc)

! ARGUMENTS:  
    character(len=*),  intent(in) :: filename
    character(len=*),  intent(in) :: varname
    logical, intent(out) :: varexist
    type(ESMF_Fileformat_Flag), intent(out) :: filetype
    character(len=*),  intent(out):: attstr
    logical, intent(out) :: haveMask
    real(ESMF_KIND_R8) :: missingval
    integer, intent(out) :: ndims
    integer, intent(out) :: dims(:)
    integer, intent(out) :: dimids(:)
    character(len=*),  intent(inout), optional :: locstr
    integer, intent(out), optional :: vartype
    character(len=*), intent(in), optional :: coordnames(:)
    integer, intent(out), optional :: rc

    logical :: foundlon, foundlat
    integer :: ncStatus
    integer ::  gridid, varid, tempids(1), varids(2), meshid, len
    character(len=128) :: attvalue, locallocstr, varnames(2)
    integer :: i, nvars, pos
    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0
    
    varexist = .true.
#ifdef ESMF_NETCDF
    rc = ESMF_FAILURE
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
    ncStatus = nf90_inq_varid(gridid, varname, varid)
    if (ncStatus /= nf90_noerror) then
       varexist = .false.
       !print *, varname, ' does not exist in ',trim(filename)
    endif 
    if (varexist) then 
      ! check if mesh attribute is defined, if so, it is a UGRID file
      ncStatus = nf90_inquire_attribute(gridid, varid, "mesh", len=len)
      if (ncStatus == nf90_noerror) then
         filetype = ESMF_FILEFORMAT_UGRID
  	 ncStatus = nf90_get_att(gridid, varid, "mesh", attstr)
         !attstr(len+1)=''
	 errmsg = 'fail to get attribute mesh '//trim(varname)
	 if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,&
              errmsg, &
              rc)) return
	 ! get location attribute
	 ncStatus = nf90_inquire_attribute(gridid, varid, "location", len=len)
         if (ncStatus == nf90_noerror) then
   	    ncStatus = nf90_get_att(gridid, varid, "location", locallocstr)
	    errmsg = 'Fail to get attribute location '//trim(varname)
	    if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,&
              errmsg, &
              rc)) return
	    if (present(locstr)) then
	       locstr = locallocstr(1:4)
            endif      
!	    if (locallocstr .ne. locstr) then
!              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
!  	        msg = " var did not locate at the correct location for the regridding method", &
!                ESMF_CONTEXT, rcToReturn=rc)
!	      return
!	    endif
	 else 
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
  	        msg = " location attribute not defined for the variable", &
                ESMF_CONTEXT, rcToReturn=rc)
	      return
         endif
       else 
         !  otherwise, check if coordinates attribute is defined 
         ncStatus = nf90_inquire_attribute(gridid, varid, "coordinates", len=len)
         if (ncStatus == nf90_noerror) then
           filetype = ESMF_FILEFORMAT_GRIDSPEC 
	   ncStatus = nf90_get_att(gridid, varid, "coordinates", attstr)
           !attstr(len+1)=''
           errmsg = 'fail to get attribute coordinates for '//trim(varname)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
         else
	   call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg=trim(filename)//' is not a GRIDSPEC or a UGRID file', &
                 ESMF_CONTEXT, rcToReturn=rc) 
           return
         endif
       endif
       ! Get missing value attribute
       ncStatus = nf90_get_att(gridid, varid, "_FillValue", missingval)
       if (ncStatus /= nf90_noerror) then
	  ncStatus = nf90_get_att(gridid, varid, "missing_value", missingval)
          if (ncStatus == nf90_noerror) then 
         	 haveMask = .TRUE.
          else
                 haveMask = .FALSE.
          endif
       endif

       ! get the dimension info for the variable
       if (present(vartype)) then
         ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, ndims=ndims, &
	     dimids=dimids)
       else
         ncStatus = nf90_inquire_variable(gridid, varid, ndims=ndims, dimids=dimids)
       endif
       errmsg = 'nf90_inquire_variable failed '//trim(varname)
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       do i=1, ndims
         ncStatus = nf90_inquire_dimension(gridid, dimids(i), len=dims(i))
         errmsg = 'nf90_inquire_dimension failed '//trim(filename)
         if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       enddo
     else !the variable does not exist, -- need to find the file type w/o var
         !Seach for a variable with cf_role or standard_name = 'mesh_topology'
         !find out how many variables are in the file
         ncStatus = nf90_inquire(gridid, nVariables=nvars)
         errmsg = 'nf90_inquire failed '//trim(filename)
         if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
	 fileType = ESMF_FILEFORMAT_GRIDSPEC
	 do i=1,nvars
	    ncStatus=nf90_get_att(gridid, i, 'cf_role', attvalue)
	    if (ncStatus == nf90_noerror) then
               ncStatus = nf90_inquire_attribute(gridid, i, 'cf_role', len=len)
            else
               ncStatus = nf90_get_att (gridid, i, "standard_name", attvalue)
	       if (ncStatus == nf90_noerror) then
                  ncStatus = nf90_inquire_attribute(gridid, i, 'standard_name', len=len)
               endif
            endif 
	    if (ncStatus == nf90_noerror) then
               if (attvalue(len:len) .eq. achar(0)) len = len-1
	       if (attvalue(1:len) .eq. 'mesh_topology') then
	         fileType=ESMF_FILEFORMAT_UGRID
	         meshid=i
		 !find mesh variable name
		 ncStatus = nf90_inquire_variable(gridid, i, name=attstr)
                 errmsg = 'nf90_inquire_variable failed '//trim(filename)
                 if (CDFCheckError (ncStatus, &
                   ESMF_METHOD, &
                   ESMF_SRCLINE,&
                   errmsg, &
                   rc)) return
               endif 
            endif
         enddo
	 ! if UGRID, use the mesh variable to find the coordinates, either face of
	 ! or node depending on the regrid method
         ! use locStr to determine whether the variable should be on the node or on the face
         if (fileType == ESMF_FILEFORMAT_UGRID) then
	    if (present(locStr)) then
	       if (locStr .eq. 'face') then
	          ncStatus=nf90_get_att(gridid, meshid, 'face_coordinates', locallocstr)
		  errmsg = 'face_coordinates attribute does not exist '//trim(attstr)
                  if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
	       else !default non-conservative
	          ncStatus=nf90_get_att(gridid, meshid, 'node_coordinates', locallocstr)
		  errmsg = 'node_coordinates attribute does not exist '//trim(attstr)
                  if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
               endif
            else
               ! signal an error if the variable does not exist and locStr is not present
                call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
     	          msg = " locStr is not present when the variable is not defined", &
                  ESMF_CONTEXT, rcToReturn=rc)
	      return
	    endif
	    
	    ! split the locallocstr to find the coordinate var name
	    pos = INDEX(locallocstr, ' ')
            varnames(1)=locallocstr(1:pos-1)
            varnames(2)=locallocstr(pos+1:)
            ncStatus=nf90_inq_varid(gridid, varnames(1), varids(1))
	    errmsg = trim(varnames(1))//' does not exist '
            if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
            ncStatus=nf90_inq_varid(gridid, varnames(2), varids(2))
	    errmsg = trim(varnames(2))//' does not exist '
            if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
	 else  !GRIDSPEC, find the coordinate variables using units
  	    !Check if the optional coordinate argument exist
	    if (present(coordnames)) then
	      ! check if the coordinate variables exist or not
	      ncStatus=nf90_inq_varid(gridid, coordnames(1), varids(1))
	      errmsg = trim(coordnames(1))//' does not exist '
              if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
	      ncStatus=nf90_inq_varid(gridid, coordnames(2), varids(2))
	      errmsg = trim(coordnames(2))//' does not exist '
              if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
	      ! check the unit attribute and make sure it is a latitude var
 	      ncStatus=nf90_get_att(gridid, varids(1), 'units', attvalue)
	      if (ncStatus == nf90_noerror) then
                if (attvalue(len:len) .eq. achar(0)) len = len-1
  	        if (.not. (attvalue(1:len) .eq. "degrees_north" .or. &
	            attvalue(1:len) .eq. "degree_north" .or. &
	            attvalue(1:len) .eq. "degree_N" .or. &
	            attvalue(1:len) .eq. "degrees_N" .or. &
	            attvalue(1:len) .eq. "degreeN" .or. &
	            attvalue(1:len) .eq. "degreesN"))  then 
                  call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
  	            msg = " not a valid latitude variable - units attribute incorrect", &
                    ESMF_CONTEXT, rcToReturn=rc)
	          return
	        endif
	      else
                  call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
  	            msg = " not a valid latitude variable - units attribute does not exist", &
                    ESMF_CONTEXT, rcToReturn=rc)
	          return
              endif
	      foundlon = .true.
              foundlat = .true.
	      ! check the unit attribute and make sure it is a longitude var
 	      ncStatus=nf90_get_att(gridid, varids(1), 'units', attvalue)
	      if (ncStatus == nf90_noerror) then
                if (attvalue(len:len) .eq. achar(0)) len = len-1
  	        if (.not. (attvalue(1:len) .eq. "degrees_east" .or. &
	            attvalue(1:len) .eq. "degree_east" .or. &
	            attvalue(1:len) .eq. "degree_E" .or. &
	            attvalue(1:len) .eq. "degrees_E" .or. &
	            attvalue(1:len) .eq. "degreeE" .or. &
	            attvalue(1:len) .eq. "degreesE"))  then 
                  call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
  	            msg = " not a valid longitude variable - units attribute incorrect", &
                    ESMF_CONTEXT, rcToReturn=rc)
	          return
	        endif
	      else
                  call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
  	            msg = " not a valid longitude variable - units attribute does not exist", &
                    ESMF_CONTEXT, rcToReturn=rc)
	          return
              endif
	      attstr = trim(coordnames(1))//' '//trim(coordnames(2))
            else  ! did not specify the optional coordnames, find them
	      foundlon = .false.
              foundlat = .false.
	      do i=1,nvars
       	        ncStatus = nf90_inquire_attribute(gridid, i, "units", len=len)
	        if (ncStatus /= nf90_noerror) cycle
	        ncStatus=nf90_get_att(gridid, i, 'units', attvalue)
                if (ncStatus /= nf90_noerror) then
                  print '("NetCDF error: ", A)', trim(nf90_strerror(ncStatus))
                  return
                endif
                if (attvalue(len:len) .eq. achar(0)) len = len-1
                if (len >= 6 .and. (attvalue(1:6) .eq. "degree")) then
  	          if (attvalue(1:len) .eq. "degrees_east" .or. &
	            attvalue(1:len) .eq. "degree_east" .or. &
	            attvalue(1:len) .eq. "degree_E" .or. &
	            attvalue(1:len) .eq. "degrees_E" .or. &
	            attvalue(1:len) .eq. "degreeE" .or. &
	            attvalue(1:len) .eq. "degreesE")  then 
       	            if (foundlon) then
                      call ESMF_LogSetError(ESMF_FAILURE, & 
                        msg="- Duplicate longitude variables defined", & 
                        ESMF_CONTEXT, rcToReturn=rc) 
		      return
	            else
		      ncStatus = nf90_inquire_variable(gridid,i,name=varnames(1))
	              varids(1)=i
	              foundlon = .true.
	            endif
	          else if (attvalue(1:len) .eq. "degrees_north" .or. &
	            attvalue(1:len) .eq. "degree_north" .or. &
	            attvalue(1:len) .eq. "degree_N" .or. &
	            attvalue(1:len) .eq. "degrees_N" .or. &
	            attvalue(1:len) .eq. "degreeN" .or. &
	            attvalue(1:len) .eq. "degreesN")  then 
  	            if (foundlat) then
                      call ESMF_LogSetError(ESMF_FAILURE, & 
                         msg="- Duplicate latitude variables defined", & 
                         ESMF_CONTEXT, rcToReturn=rc) 
		      return
	            else
		      ncStatus = nf90_inquire_variable(gridid,i,name=varnames(2))
	              varids(2)=i
	              foundlat = .true.
	            endif
                  endif
		endif
              enddo ! loop through all the variables
              if (foundlon .and. foundlat) then
 	         attstr = trim(varnames(1))//' '//trim(varnames(2))
	      else
                 call ESMF_LogSetError(ESMF_FAILURE, & 
                    msg="- Did not find the coordinate variables", & 
                    ESMF_CONTEXT, rcToReturn=rc) 
	         return
              endif
            endif !coordname is not present
         endif ! not UGRID
	 ! find the lon, lat dimension
         ncStatus=nf90_inquire_variable(gridid, varids(1), ndims=ndims, dimids=dimids)
         do i=1, ndims
              ncStatus = nf90_inquire_dimension(gridid, dimids(i), len=dims(i))
	      errmsg = 'nf90_inquire_dimension '//trim(filename)
              if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
         enddo
	 if (filetype == ESMF_FILEFORMAT_GRIDSPEC .and. ndims==1) then
	       ! find the dimension of the latitude coordinate
             ncStatus=nf90_inquire_variable(gridid, varids(2), ndims=ndims, dimids=tempids)
	     dimids(2)=tempids(1)
             ncStatus = nf90_inquire_dimension(gridid, dimids(2), len=dims(2))
	      errmsg = 'nf90_inquire_dimension '//trim(filename)
              if (CDFCheckError (ncStatus, &
                     ESMF_METHOD, &
                     ESMF_SRCLINE,&
                     errmsg, &
                     rc)) return
             ndims=2
         endif
     endif ! not varExist 
     rc = ESMF_SUCCESS
     return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

  end subroutine checkFileType

#undef  ESMF_METHOD
#define ESMF_METHOD "CreateDstVar"
!BOPI
! !ROUTINE: CreateDstVar
!
! !INTERFACE:
subroutine CreateDstVar(srcFile, dstFile, fileType, srcVarName, dstVarName, &
	   varDims, varRank, varStr, locStr, varDimids, rc)
    character(len=*), intent(in) :: srcFile
    character(len=*), intent(in) :: dstFile
    type(ESMF_FILEFORMAT_FLAG), intent(in) :: fileType
    character(len=*), intent(in) :: srcVarName
    character(len=*), intent(in) :: dstVarName
    integer, intent(in) :: varDims(:)
    integer, intent(in) :: varRank
    character(len=*), intent(in) :: varStr
    character(len=*), intent(in) :: locStr
    integer, intent(in) :: varDimids(:)
    integer, intent(out) :: rc

    integer:: gridid, gridid1, varid, varid1, dimid
    integer:: extradim, dimval
    integer:: i, j, vartype, ndims, nattrs, rank
    integer, pointer :: dimids(:), srcdimids(:)
    character(len=40) ::dimnames(MAX_VARDIMS), dimname, attname
    integer :: ncStatus
    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    ! First check how many extra dimensions to be created
    if (fileType == ESMF_FILEFORMAT_GRIDSPEC) then
      extradim = varRank - 2
      rank = 2
    else
      extradim = varRank - 1
      rank = 1
    endif
    allocate(dimids(varRank))
    dimids(1:rank)=varDimids(1:rank)
    ! Open the destination file
    ncStatus = nf90_open (path=trim(dstFile), mode=nf90_write, ncid=gridid1)
    if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           trim(dstFile), &
           rc)) return
    
    ncStatus = nf90_open (path=trim(srcFile), mode=nf90_nowrite, ncid=gridid)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        trim(srcFile), &
        rc)) return
    ncStatus = nf90_inq_varid( gridid, srcVarName, varid)
    errmsg = "variable "//trim(srcVarName)// " in "//trim(srcFile)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
    ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, ndims=ndims, &
    	     nAtts=nattrs)
    errmsg = "Variable "//trim(srcVarName)//" in "//trim(srcFile)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
    if (extradim > 0) then
      ! get the dimension id and name for all the extra dimensions
      ! Open the src grid file
      allocate(srcdimids(ndims))
      ncStatus = nf90_inquire_variable(gridid, varid, &
      	       dimids=srcdimids)
      errmsg = "Variable "//trim(srcVarName)//" in "//trim(srcFile)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
      do i=1,extradim
        ncStatus = nf90_inquire_dimension(gridid,srcdimids(ndims-(extradim-i)),&
       		name=dimnames(i))
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg, &
          rc)) return
      enddo
      ! ncStatus = nf90_close(gridid)
      ! check if the last dimension is "time"
      if (dimnames(extradim) .eq. "time") then
        ! the last dimension of the srcVariable is time, check if there is one in the dstFile
        ncStatus = nf90_inq_dimid(gridid1, 'time', dimid)
        if (ncStatus == nf90_noerror) then 
           ncStatus = nf90_inquire_dimension(gridid1, dimid, len=dimval)
           errmsg = 'inquire time dimension in'//trim(dstFile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg, &
             rc)) return
	   if (dimval /= varDims(varRank)) then
	     call ESMF_LogSetError(ESMF_FAILURE, & 
                 msg="- the time dimension values do not match in the src and dst files", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
             return
           endif
           extradim = extradim - 1
	   dimids(varRank) = dimid
        else 
           ! create time dimension and other extra dimensions in the dstFile
           ncStatus = nf90_redef(gridid1)
           ncStatus = nf90_def_dim(gridid1, 'time', varDims(varRank) , dimids(varRank))
	   errmsg = 'define time dimension in '//trim(dstFile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg, &
             rc)) return
	   extradim = extradim - 1
           ncStatus = nf90_enddef(gridid1)
         endif 
       endif ! there is not time dimension in the srcVar
       !if there are more extra dimension, create them
       if (extradim > 0) then
         j = 1
	 do i=1,extradim
           write(dimname, '("extradim", I1)') j
	   ! check if the dimension name already exist, if so and the value
           ! matches, use it, otherwise use another name
           ncStatus = nf90_inq_dimid(gridid1, dimname, dimid)
           do while (ncStatus == nf90_noerror) 
	      j=j+1
	      write(dimname, '("extradim", I1)') j
              ncStatus = nf90_inq_dimid(gridid1, dimname, dimid)
           enddo	      
           ncStatus = nf90_redef(gridid1)
	   errmsg = 'enter redefine mode '//trim(dstFile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
           ncStatus = nf90_def_dim(gridid1, dimname, varDims(rank+i), &
	   	    dimids(rank+i))
	   errmsg = 'define extra dimension in '//trim(dstFile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
         enddo
         ncStatus = nf90_enddef(gridid1)
         errmsg = 'end redefine mode '//trim(dstFile)
         if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       endif
     endif ! End extradim > 0
     ! create the destination variable now
     ncStatus = nf90_redef(gridid1)
     errmsg = 'enter redefine mode '//trim(dstFile)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
     ncStatus = nf90_def_var(gridid1, dstVarName, vartype, dimids, varid1) 	     
     errmsg = 'define destination variable '//trim(dstVarName)
     if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
     ! define the attributes for the variable, copy the attributes from the
     ! the srcVar and create new coordinates attribute
     do i=1,nattrs
       ncStatus = nf90_inq_attname(gridid, varid, i, attname)
       errmsg = 'attribute for '//trim(srcVarName)
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       if ((attname .ne. 'coordinates') .and. (attname .ne. 'mesh')) then
         ncStatus = nf90_copy_att(gridid, varid, attname, gridid1, varid1)
         errmsg = 'adding attribute '//trim(attname)//' to variable '//trim(dstVarName)
         if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       endif
     enddo
     if (fileType == ESMF_FILEFORMAT_GRIDSPEC) then
       ! Add coordinates attribute
       ncStatus = nf90_put_att(gridid1, varid1, 'coordinates', varStr)
       errmsg = 'attribute for '//trim(srcVarName)
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
     else
       ! If the file is a UGRID, need to add mesh attribute too
       ncStatus = nf90_put_att(gridid1, varid1, 'mesh', varStr)
       errmsg = 'define attribute mesh for '//trim(dstVarName)
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       ! Need to know if the variable is on face or node
       ncStatus = nf90_put_att(gridid1, varid1, 'location', locStr)
       errmsg = 'define attribute location for '//trim(dstVarName)
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       ! Need to know if the variable is on face or node
     endif
     ncStatus = nf90_close(gridid)
     ncStatus = nf90_close(gridid1)

    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine CreateDstVar

!------------------------------------------------------------------------------
!
!  check CDF file error code
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CDFCheckError"
function CDFCheckError (ncStatus, module, fileName, lineNo, errmsg, rc)

    logical                       :: CDFCheckError

    integer,          intent(in)  :: ncStatus
    character(len=*), intent(in)  :: module
    character(len=*), intent(in)  :: fileName
    integer,          intent(in)  :: lineNo
    character(len=*), intent(in)  :: errmsg
    integer, intent(out),optional :: rc

    integer, parameter :: nf90_noerror = 0

    CDFCheckError = .FALSE.

#ifdef ESMF_NETCDF
    if ( ncStatus .ne. nf90_noerror) then
        call ESMF_LogWrite (msg="netCDF Status Return Error", logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=lineNo, file=fileName, method=module)
        print '("NetCDF Error: ", A, " : ", A)', &
    		trim(errmsg),trim(nf90_strerror(ncStatus))
        call ESMF_LogFlush()
        if (present(rc)) rc = ESMF_FAILURE
    	CDFCheckError = .TRUE.
    else
       if (present(rc)) rc = ESMF_SUCCESS
       return
    end if
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end function CDFCheckError

!------------------------------------------------------------------------------

end module ESMF_FileRegridMod

