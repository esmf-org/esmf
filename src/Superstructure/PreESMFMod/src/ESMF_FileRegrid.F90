!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
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
  use ESMF_FieldRedistMod
  use ESMF_FieldPrMod
  use ESMF_FieldWrMod
  use ESMF_IOScripMod
  use ESMF_IOGridspecMod
  use ESMF_IO_NCPutGetMod
  use ESMF_IOUGridMod
  use ESMF_IOGridmosaicMod
  use ESMF_RHandleMod
  use ESMF_LocStreamMod
  use ESMF_IOFileTypeCheckMod

#ifdef ESMF_NETCDF
  use netcdf
#endif

  implicit none

#define UNDEFINEDVALUE 0.00000001
integer, parameter :: MAXNAMELEN = 64
integer, parameter:: MAX_VARDIMS = 4

!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:

  public ESMF_FileRegrid
  public CheckVarInfo

! -------------------------- ESMF-public method -------------------------------
contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FileRegrid"

!BOP
! !IROUTINE: ESMF_FileRegrid - Regrid variables defined in the  grid files
! \label{api:esmf_fileregrid}
! !INTERFACE:
  subroutine ESMF_FileRegrid(srcFile, dstFile, srcVarName, dstVarName, keywordEnforcer, &
    dstLoc, srcDataFile, dstDataFile, tileFilePath, &
    dstCoordVars, regridmethod, polemethod, regridPoleNPnts, &
    unmappedaction, ignoreDegenerate, srcRegionalFlag, dstRegionalFlag, &
    verboseFlag, rc)

! !ARGUMENTS:

  character(len=*),             intent(in)            :: srcFile
  character(len=*),             intent(in)            :: dstFile
  character(len=*),             intent(in)            :: srcVarName
  character(len=*),             intent(in)            :: dstVarName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  character(len=*),             intent(in),  optional :: dstLoc
  character(len=*),             intent(in),  optional :: srcDataFile     
  character(len=*),             intent(in),  optional :: dstDataFile     
  character(len=*),             intent(in),  optional :: tileFilePath
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
!     The source variable names to be regridded. If more than one, separate them by comma.
!   \item [dstVarName]
!     The destination variable names to be regridded to. If more than one, separate them by comma.
!   \item [{[dstLoc]}]
!     The destination variable's location, either 'node' or 'face'.  This
!     argument is only used when the destination grid file is UGRID, the regridding method is
!     non-conservative and the destination variable does not exist in the destination grid file.
!     If not specified, default is 'face'.
!   \item [{[srcDataFile]}]
!     The input data file prefix if the srcFile is in GRIDSPEC MOSAIC
!     fileformat.  The tilename and the file extension (.nc) will be added to
!     the prefix.  The tilename is defined in the MOSAIC file using variable "gridtiles".
!   \item [{[dstDataFile]}]
!     The output data file prefix if the dstFile is in GRIDSPEC MOSAIC
!     fileformat.  The tilename and the file extension (.nc) will be added to
!     the prefix.  The tilename is defined in the MOSAIC file using variable "gridtiles".
!   \item [{[tileFilePath]}]
!     The alternative file path for the tile files and mosaic data files when either srcFile or
!     dstFile is a GRIDSPEC MOSAIC grid.  The path can be either relative or absolute.  If it is
!     relative, it is relative to the working directory.  When specified, the gridlocation variable
!     defined in the Mosaic file will be ignored.
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
    character(ESMF_MAXPATHLEN) :: localInputfile, localOutputfile
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
    integer            :: i,j, k,l, i1, ii, bigFac, xpets, ypets, xpart, ypart, xdim, ydim
    type(ESMF_UnmappedAction_Flag) :: localUnmappedaction
    character(len=256) :: argStr
    logical            :: useSrcCoordVar, useDstCoordVar
    logical            :: useSrcMask, useDstMask
    logical            :: useSrcCorner, useDstCorner
    integer, pointer   :: commandbuf(:)
    real(ESMF_KIND_R8) :: commandbuf2(2)
    character(len=MAXNAMELEN*2)   :: commandbuf1(2)
    character(len=MAXNAMELEN)     :: srcLocStr, dstLocStr
    character(len=MAXNAMELEN)     :: srcLocStrSave, dstLocStrSave
    character(len=MAXNAMELEN)     :: srcMeshVar, dstMeshVar
    character(len=MAXNAMELEN), pointer :: srcVarNames(:), dstVarNames(:)
    integer              :: srcVarType, dstVarType
    character(len=MAXNAMELEN*2)  :: srcVarStr, dstVarStr
    integer, pointer       :: srcVarRank(:), dstVarRank(:)
    integer                :: extraRank
    integer, pointer       :: srcVarDims(:,:), dstVarDims(:,:)
    integer                :: srcDimids(MAX_VARDIMS), dstDimids(MAX_VARDIMS)
    logical                :: srcVarExist, dstVarExist
    integer                :: srcVarCount
    integer                :: pos1, pos2
    character(len=MAXNAMELEN) :: srcCoordNames(2), dstCoordNames(2)
    logical                  :: terminateProg
    integer, pointer   :: lbnd(:), ubnd(:), tlbound(:,:), tubound(:,:)
    integer                :: ungridrank
    integer, pointer   :: start(:), count(:)
    integer            :: start1, count1
    real(ESMF_KIND_R8) :: srcMissingVal, dstMissingVal
    real(ESMF_KIND_R8), pointer:: varBuf1D(:), varBuf2D(:,:), varBuf3D(:,:,:), varBuf4D(:,:,:,:)
    real(ESMF_KIND_R8), pointer:: sendbuf(:), recvbuf(:)
    real(ESMF_KIND_R8), pointer::  fptr1d(:), fptr2d(:,:), fptr3d(:,:,:), fptr4d(:,:,:,:)
    integer                 :: gridid, varid, start2(2), count2(2), start3(3), count3(3)
    real(ESMF_KIND_R8), pointer :: tmpfptr(:)
    type(ESMF_Array) :: tmparray
    real(ESMF_KIND_R8), pointer :: varbuffer(:)
    integer                 :: totalnodecnt, totalelmtcnt, totalcount
    integer                 :: total, totalrecv
    type(ESMF_Distgrid) :: nodalDG, elementDG
    character(len=20) :: location
    integer                 :: ncStatus
    character(len=256) :: errmsg, attstr
    integer, parameter :: nf90_noerror = 0
    !real(ESMF_KIND_R8) :: starttime, endtime
    logical            :: localIgnoreDegenerate
    integer            :: dgsize(1)
    integer            :: offset, offset1, localDeCount
    type(ESMF_Mosaic)  :: srcMosaic, dstMosaic
    character(ESMF_MAXPATHLEN) :: srctempname, dsttempname
    type(ESMF_RouteHandle) :: rh
    type(ESMF_Array)   :: arrayPet0
  
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
    localInputfile=' '
    localOutputfile=' '

    if (present(regridMethod)) then
      localRegridMethod = regridMethod
    endif

    if (present(poleMethod)) then
         localPoleMethod = poleMethod
    else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
        localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND ) then
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

    if ((localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
        localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) .and. &
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

    if (srcIsRegional) then
      localPoleMethod = ESMF_POLEMETHOD_NONE
      localPoleNPnts = 0
    endif

    if (present(verboseFlag)) then
      localVerboseFlag = verboseFlag
    endif

    if (present(srcDataFile)) localInputfile = srcDataFile
    if (present(dstDataFile)) localOutputfile = dstDataFile

    if (present(dstLoc)) then
       dstLocStr = trim(dstLoc)
    else
       dstLocStr = 'face'
    endif
    if (dstLocStr .ne. 'node' .and. dstLocStr .ne. 'face') then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
          msg ="dstLoc is not 'node' nor 'face'", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    ! by default, variables are located at the center of the grid
    useSrcCorner = .FALSE.
    useDstCorner = .FALSE.
    if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
        localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
       srcmeshloc=ESMF_MESHLOC_ELEMENT
       dstmeshloc=ESMF_MESHLOC_ELEMENT
    else
       srcmeshloc=ESMF_MESHLOC_NODE
       dstmeshloc=ESMF_MESHLOC_NODE
    endif

    ! Parse SrcVarName and dstVarName, store the variable names in array srcVarNames(:)
    ! and dstVarNames(:), check if the number of variables matches.
      
    ! Two phase, first find out how many source variables, secondly, store the variable
    ! names in an array
    pos1 = index(srcVarName(1:),",")
    start1 = 1
    count1 = 1
    do while (pos1 > 0)
       start1 = start1+pos1
       count1 = count1+1
       pos1 = index(srcVarName(start1:),",")
    end do
    srcVarCount = count1
    allocate(srcVarNames(srcVarCount))      
    pos1 = index(srcVarName(1:),",")
    start1 = 1
    count1 = 1
    do while (pos1 > 0)
       srcVarNames(count1) = srcVarName(start1:start1+pos1-2)
       start1 = start1+pos1
       pos1 = index(srcVarName(start1:),",")
       count1 = count1+1
    end do
    srcVarNames(count1) = trim(srcVarName(start1:))

    ! Check if the destination variable count matches with srcVarCount
    pos1 = index(dstVarName(1:),",")
    start1 = 1
    count1=1
    do while (pos1 > 0)
       start1 = start1+pos1
       count1 = count1+1
       pos1 = index(dstVarName(start1:),",")
    end do
    if (count1 /= srcVarCount) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
              msg = " The source variable count does not match with destination variable count", &
              ESMF_CONTEXT, rcToReturn=rc)
          return
    endif

    allocate(dstVarNames(srcVarCount))      
    pos1 = index(dstVarName(1:),",")
    start1 = 1
    count1=1
    do while (pos1 > 0)
       dstVarNames(count1) = dstVarName(start1:start1+pos1-2)
       start1 = start1+pos1
       pos1 = index(dstVarName(start1:),",")
       count1 = count1+1
    end do
    dstVarNames(count1) = trim(dstVarName(start1:))

    allocate(srcVarRank(srcVarCount), dstVarRank(srcVarCount))
    allocate(srcVarDims(MAX_VARDIMS, srcVarCount), dstVarDims(MAX_VARDIMS, srcVarCount))
    allocate(commandbuf(srcVarCount*(MAX_VARDIMS+1)*2+12))

    ! Find out the grid dimension at the root processor and broadcast to the rest
    if (PetNo == 0) then
      ! find file type
      call ESMF_FileTypeCheck(srcFile, localsrcFileType, varname=srcMeshVar, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) then
           terminateProg = .TRUE.
           goto 1110
      endif
      ! Only ESMF_FILEFORMAT_UGRID, ESMF_FIELFORMAT_GRIDSPEC and
      ! ESMF_FILEFORMAT_MOSAIC are supported, return errors otherwise
      if (localsrcFileType /= ESMF_FILEFORMAT_UGRID .and. localsrcFileType /= ESMF_FILEFORMAT_GRIDSPEC &
          .and. localsrcFileType /= ESMF_FILEFORMAT_MOSAIC) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg = " Source FileType has to be one of UGRID, CFTILE or GRIDSPEC MOSAIC", &
               ESMF_CONTEXT, rcToReturn=rc)
           terminateProg = .TRUE.
           goto 1110
      endif
      if (localsrcFileType == ESMF_FILEFORMAT_UGRID) then
          srcIsReg = .false.
          srcRank = 1
      elseif (localsrcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
          srcIsReg = .true.
          srcRank = 2
      else !Cubed Sphere
          srcIsReg = .false.
          srcRank = 2
      endif

      if (localsrcFileType == ESMF_FILEFORMAT_MOSAIC .and. localInputFile .eq. ' ') then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg = " srcDataFile argument not present when the srcFile is a GRIDSPEC MOSAIC grid", &
               ESMF_CONTEXT, rcToReturn=rc)
           terminateProg = .TRUE.
           goto 1110
      endif

      call ESMF_FileTypeCheck(dstFile, localdstFileType, varname=dstMeshVar, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
      if (localdstFileType /= ESMF_FILEFORMAT_UGRID .and. localdstFileType /= ESMF_FILEFORMAT_GRIDSPEC & 
          .and. localdstFileType /= ESMF_FILEFORMAT_MOSAIC) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg = " Destination FileType has to be one of UGRID, CFTILE or GRIDSPEC MOSAIC", &
               ESMF_CONTEXT, rcToReturn=rc)
           terminateProg = .TRUE.
           goto 1110
      endif
      if (localdstFileType == ESMF_FILEFORMAT_UGRID) then
          dstIsReg = .false.
          dstRank = 1
      elseif (localdstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
          dstIsReg = .true.
          dstRank = 2
      else
          dstIsReg = .false.
          dstRank = 2
      endif

      if (localdstFileType == ESMF_FILEFORMAT_MOSAIC .and. localOutputFile .eq. ' ') then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg = " dstDataFile argument not present when the dstFile is a GRIDSPEC MOSAIC grid", &
               ESMF_CONTEXT, rcToReturn=rc)
           terminateProg = .TRUE.
           goto 1110
      endif

      if (localsrcFileType == ESMF_FILEFORMAT_MOSAIC) then
         call ESMF_GridSpecReadMosaic(srcFile, srcMosaic, tileFilePath = tileFilePath, rc=localrc)
         ! The Mosaic file has to be a cubed sphere grid
         if (srcMosaic%ntiles /= 6) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                  msg = " The srcfile does not have 6 tiles", &
                  ESMF_CONTEXT, rcToReturn=rc)
             terminateProg = .TRUE.
             goto 1110
         endif
         if (srcMosaic%nx /= srcMosaic%ny) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                  msg = " The srcfile tile is not square", &
                  ESMF_CONTEXT, rcToReturn=rc)
             terminateProg = .TRUE.
             goto 1110
         endif
      endif
      if (localdstFileType == ESMF_FILEFORMAT_MOSAIC) then
         call ESMF_GridSpecReadMosaic(dstFile, dstMosaic, tileFilePath = tileFilePath, rc=localrc)
         if (dstMosaic%ntiles /= 6) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                  msg = " The dstfile does not have 6 tiles", &
                  ESMF_CONTEXT, rcToReturn=rc)
             terminateProg = .TRUE.
             goto 1110
         endif
         if (dstMosaic%nx /= dstMosaic%ny) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                  msg = " The dstfile tile is not square", &
                  ESMF_CONTEXT, rcToReturn=rc)
             terminateProg = .TRUE.
             goto 1110
         endif
      endif

      ! Using the first source variable's missing value for all the variables, and to
      ! generate mask as well
    
      if (localsrcFileType /= ESMF_FILEFORMAT_MOSAIC) then
        call CheckVarMask(srcFile, srcVarNames(1), useSrcMask, srcMissingVal, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
           terminateProg = .TRUE.
           goto 1110
        endif
      endif
     
      do i=1,srcVarCount
        if (localsrcFileType == ESMF_FILEFORMAT_MOSAIC) then
          ! totallen = len_trim(srcMosaic%filenames(1))+len_trim(srcMosaic%tileDirectory)
          ! Check the first data tile file, assuming all other data files are consistent
          srctempname = trim(srcMosaic%tileDirectory)//trim(srcDataFile)//"."//trim(srcMosaic%tilenames(1))//".nc"
          call checkVarInfo(trim(srctempname), trim(srcVarNames(i)), srcVarExist, &
                  localsrcFileType, srcMeshVar, srcVarStr, &
                  srcVarRank(i), srcVarDims(:,i), srcDimids, &
                   useSrcMask, srcMissingVal, &
                  vartype=srcVarType,  rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
             terminateProg = .TRUE.
             goto 1110
          endif
          srcLocStr = 'face'
        else
          call checkVarInfo(trim(srcFile), trim(srcVarNames(i)), srcVarExist, &
                  localsrcFileType, srcMeshVar, srcVarStr, &
                  srcVarRank(i), srcVarDims(:,i), srcDimids, &
                   useSrcMask, srcMissingVal, &
                  locStr=srcLocStr, vartype=srcVarType,  rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
             terminateProg = .TRUE.
             goto 1110
          endif
        endif
        if (.not. srcVarExist) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                msg = " source variable "//trim(srcVarNames(i))//" does not exist", &
                ESMF_CONTEXT, rcToReturn=rc)
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

        if (i==1) then
          if (trim(srcLocStr) .eq. 'node' .and. (localsrcFileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
              localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
                      localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                  msg = " The source variable has to be located at the center of the grid ", &
                  ESMF_CONTEXT, rcToReturn=rc)
              terminateProg = .TRUE.
              goto 1110
          endif
          if (localsrcFileType == ESMF_FILEFORMAT_UGRID .and.  &
            (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE .or. &
             localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
             if (trim(srcLocStr) .eq. 'node') then
                useSrcCorner = .TRUE.
             else
                srcmeshloc = ESMF_MESHLOC_ELEMENT
             endif
          endif
          srcLocStrSave = srcLocStr
        else
           if (trim(srcLocStr) .ne. trim(srcLocStrSave)) then
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                msg = " All the source variables have to be on the same stagger location", &
                ESMF_CONTEXT, rcToReturn=rc)
              terminateProg = .TRUE.
              goto 1110
           endif
        endif      

        if (localdstFileType == ESMF_FILEFORMAT_MOSAIC) then
          dsttempname = trim(dstMosaic%tileDirectory)//trim(localOutputFile)//"."//trim(dstMosaic%tilenames(1))//".nc"
          call checkVarInfo(trim(dsttempname), trim(dstVarNames(i)), dstVarExist, &
                   localdstFileType, dstMeshVar, dstVarStr, &
                   dstVarRank(i), dstVarDims(:,i), dstDimids, &
                   useDstMask, dstMissingVal, &
                   vartype=dstVarType,  rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
             terminateProg = .TRUE.
             goto 1110
          endif
          dstLocStr = 'face'
          ! If destVar does not exist, need to find out the var dimension and rank from the mosaic
          !dstVarRank(i)=2
          !dstVarDims(1,i)=dstMosaic%nx
          !dstVarDims(2,i)=dstMosaic%ny
        else
          call checkVarInfo(trim(dstFile), trim(dstVarNames(i)), dstVarExist, &
                   localdstFileType, dstMeshVar, dstVarStr, &
                   dstVarRank(i), dstVarDims(:,i), dstDimids, &
                   useDstMask, dstMissingVal, &
                   vartype=dstVarType, locStr=dstLocStr, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
             terminateProg = .TRUE.
             goto 1110
          endif
        endif
        if (i==1 .and. (.not. dstVarExist .or. .not. useDstMask)) then
          if(useSrcMask) then
            dstMissingVal = srcMissingVal
          else
            dstMissingVal = 0.0
          endif
        endif

        if (dstVarExist .and. (.not. ((dstVarType == NF90_FLOAT) .or. &
           (dstVarType == NF90_DOUBLE)))) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                msg = " The program only supports dst variable of type float or double ", &
                ESMF_CONTEXT, rcToReturn=rc)
           terminateProg = .TRUE.
           goto 1110
        endif

        if (i==1) then
          if (dstLocStr .eq. 'node' .and. (localdstFileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
             localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
             localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                msg = " The destination variable has to be located at the center of the grid ", &
                ESMF_CONTEXT, rcToReturn=rc)
             terminateProg = .TRUE.
             goto 1110
          endif
          if (localdstFileType == ESMF_FILEFORMAT_UGRID .and.  &
             (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE .and. &
             localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
             if (dstLocStr .eq. 'node') then     
                useDstCorner = .TRUE.
                dstmeshloc=ESMF_MESHLOC_NODE
             else
                dstmeshloc=ESMF_MESHLOC_ELEMENT
             endif 
          endif
          dstLocStrSave = dstLocStr
        else
           if (trim(dstLocStr) .ne. trim(dstLocStrSave)) then
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                 msg = " All the destination variables have to be on the same stagger location", &
                 ESMF_CONTEXT, rcToReturn=rc)
              terminateProg = .TRUE.
              goto 1110
           endif
        endif      

        if (i==1 .and. localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
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
        endif

        if (.not. dstVarExist) then
           extraRank = srcVarRank(i) - srcRank
           useDstMask = .false.
           dstVarRank(i) = dstVarRank(i) + extraRank
           dstVarDims(dstVarRank(i)-extraRank+1:dstVarRank(i),i)=srcVarDims(srcRank+1:srcVarRank(i),i)
           ! Also check if there is a time dimension in the destination file and
           ! if that matches with the time dimension of the source grid

           ! check if the last dimension of the srcVar is named "time", if so, check if
           ! there is a matching time dimension in the dstFile. If there is a time dimension
           ! in the dstFile, the value has to match with the src time dimension. If not exist
           ! create one and create the dstvar as well
           ! If the filetype is MOSAIC, need to create the variable in all the tile data files

           if (localsrcfiletype == ESMF_FILEFORMAT_MOSAIC &
             .and. localdstfiletype == ESMF_FILEFORMAT_MOSAIC) then
              do j=1,dstMosaic%ntiles
                   dsttempname = trim(dstMosaic%tileDirectory)//trim(localOutputFile)//&
                        "."//trim(dstMosaic%tilenames(j))//".nc"
                   call CreateDstVar(srctempname, dsttempname, localdstFileType, srcVarNames(i), &
                       dstVarNames(i), dstVarDims(:,i), dstVarRank(i), dstVarStr, &
                       dstLocStr,dstDimids, localrc)       
                   if (ESMF_LogFoundError(localrc, &
                       ESMF_ERR_PASSTHRU, &
                       ESMF_CONTEXT, rcToReturn=rc)) then
                       terminateProg = .TRUE.
                       goto 1110
                   endif
              enddo
           elseif (localsrcfiletype == ESMF_FILEFORMAT_MOSAIC) then
              call CreateDstVar(srctempname, dstFile, localdstFileType, srcVarNames(i), &
                    dstVarNames(i), dstVarDims(:,i), dstVarRank(i), dstVarStr, &
                    dstLocStr,dstDimids, localrc)       
              if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) then
                terminateProg = .TRUE.
                goto 1110
             endif
           elseif (localdstfiletype == ESMF_FILEFORMAT_MOSAIC) then
              do j=1,dstMosaic%ntiles
                   dsttempname = trim(dstMosaic%tileDirectory)//trim(localOutputFile)//&
                        "."//trim(dstMosaic%tilenames(j))//".nc"
                  call CreateDstVar(srcFile, dsttempname, localdstFileType, srcVarNames(i), &
                      dstVarNames(i), dstVarDims(:,i), dstVarRank(i), dstVarStr, &
                      dstLocStr,dstDimids, localrc)       
                  if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) then
                      terminateProg = .TRUE.
                      goto 1110
                  endif
              enddo
           else  
              call CreateDstVar(srcFile, dstFile, localdstFileType, srcVarNames(i), &
                    dstVarNames(i), dstVarDims(:,i), dstVarRank(i), dstVarStr, &
                    dstLocStr,dstDimids, localrc)
              if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) then
                terminateProg = .TRUE.
                goto 1110
             endif
           endif
        endif
        if (i==1 .and. localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
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
              ESMF_CONTEXT, rcToReturn=rc)) then
              terminateProg = .TRUE.
              goto 1110
           endif
        endif
      enddo

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
        endif
        commandbuf2(2)=dstMissingVal
        if (useSrcCorner) then
            commandbuf(11)=1
        endif
        if (useDstCorner) then
            commandbuf(12)=1
        endif
        do i=1,srcVarCount        
          offset = 12+(i-1)*(MAX_VARDIMS+1)*2
          commandbuf(offset+1) = srcVarRank(i)
          do j=1,srcVarRank(i)
            commandbuf(offset+1+j) = srcVarDims(j,i)
          enddo
          offset1 = offset+1+MAX_VARDIMS
          commandbuf(offset1+1) = dstVarRank(i)
          do j=1,dstVarRank(i)
            commandbuf(offset1+1+j) = dstVarDims(j,i)
          enddo
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
      if (localsrcfiletype == ESMF_FILEFORMAT_MOSAIC) srcRank=2
      if (localdstfiletype == ESMF_FILEFORMAT_MOSAIC) dstRank=2
      if (commandbuf(9) == 1) then
         useSrcMask = .true.
      endif
      if (commandbuf(10) == 1) then
         useDstMask = .true.
      endif
      if (commandbuf(11) == 1) then
         useSrcCorner = .true.
      else
         srcmeshloc = ESMF_MESHLOC_ELEMENT
      endif
      if (commandbuf(12) == 1) then
         useDstCorner = .true.
      else
         dstmeshloc = ESMF_MESHLOC_ELEMENT
      endif
      do i=1, srcVarCount
         offset = 12+(i-1)*(MAX_VARDIMS+1)*2
         srcVarRank(i) = commandbuf(offset+1)
         do j=1, srcVarRank(i)
           srcVarDims(j,i)=commandbuf(offset+1+j)
         enddo
         offset1 = offset+1+MAX_VARDIMS
         dstVarRank(i) = commandbuf(offset1+1)
         do j=1, dstVarRank(i)
           dstVarDims(j,i)=commandbuf(offset1+1+j)
         enddo
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
        print *, "  Source variable names: ", trim(srcVarName)
        print *, "  Destination variable names: ", trim(dstVarName)
        if (useSrcMask) then
            print *, "  Souce Grid has a mask, using missingvalue ", srcMissingVal
        endif
        if (localsrcfiletype == ESMF_FILEFORMAT_UGRID) then
            print *, "  Source File is in UGRID format with mesh name ", trim(srcVarStr)
        elseif (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
            print *, "  Source File is in GRIDSPEC format with coordinate names ", trim(srcVarStr)
        else
            print *, "  Source File is in GRIDSPEC MOSAIC format"
        endif
        if (srcIsRegional) then
              print *, "  Source Grid is a regional grid"
        else
              print *, "  Source Grid is a global grid"
        endif
        if (localdstfiletype == ESMF_FILEFORMAT_UGRID) then
           print *, "  Destination File is in UGRID format with mesh name ", trim(dstVarStr)
        elseif (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
           print *, "  Destination File is in GRIDSPEC format with coordinate names ", trim(dstVarStr)
        else
            print *, "  Destination File is in GRIDSPEC MOSAIC format"
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
          print *, "  Regrid Method: conservative"
        else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
          print *, "  Regrid Method: 2nd order conservative"
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
    if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
       localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
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

#if 0
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

    if (localsrcfiletype == ESMF_FILEFORMAT_MOSAIC) then
        srcGrid = ESMF_GridCreateMosaic(srcfile, tileFilePath=TileFilePath, &
                staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
                rc=localrc)
        if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
      srcGrid = ESMF_GridCreate(srcfile, regdecomp=(/xpart,ypart/), &
                addCornerStagger=addCorners, &
                addMask=useSrcMask, varname=trim(srcVarNames(1)), isSphere=srcIsSphere, &
                coordNames = srcCoordNames, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      srcField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, &
                 rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! it is UGRID
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
                      varname=trim(srcVarNames(1)), &
                      centerflag=.not. useSrcCorner, rc=localrc) 
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       else
          srcLocStream = ESMF_LocStreamCreate(srcfile, &
                              fileformat=localSrcFileType, &
                      indexflag=ESMF_INDEX_DELOCAL, &
                      centerflag=.not. useSrcCorner, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       endif
        srcField=ESMF_FieldCreate(srcLocStream,arrayspec,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
             localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
        if (useSrcMask) then
          srcMesh = ESMF_MeshCreate(srcfile, localsrcfiletype, &
              maskFlag =srcmeshloc, &
            ! BOB: NEVER DO DUAL WHEN CONSERVE
              convertToDual= .false., &
              varname=trim(srcVarNames(1)), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
          srcMesh = ESMF_MeshCreate(srcfile, localsrcfiletype, &
            ! BOB: NEVER DO DUAL WHEN CONSERVE
              convertToDual= .false., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        srcField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=srcmeshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else ! Non-conservative, but also not nearest neighbor (this would be things like bilinear, etc.)
        if (useSrcMask) then
          srcMesh = ESMF_MeshCreate(srcfile, localsrcfiletype, &
              maskFlag =srcmeshloc, &
              convertToDual= .not. useSrcCorner, &
              varname=trim(srcVarNames(1)), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
          srcMesh = ESMF_MeshCreate(srcfile, localsrcfiletype, &
              convertToDual=.not. useSrcCorner, &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        srcmeshloc = ESMF_MESHLOC_NODE
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

    if (localdstfiletype == ESMF_FILEFORMAT_MOSAIC) then
        dstGrid = ESMF_GridCreateMosaic(dstfile, tileFilePath=TileFilePath, &
                staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
                rc=localrc)
        if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
      dstGrid = ESMF_GridCreate(dstfile, regdecomp=(/xpart,ypart/), &
         addCornerStagger=addCorners, &
         addMask=useDstMask, varname=trim(dstVarNames(1)), isSphere=dstIsSphere, &
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
      if (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE .and. &
          localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE_2ND) then
        dstIsLocStream = .TRUE.
        if (useDstMask) then
           dstLocStream = ESMF_LocStreamCreate(dstfile, &
                              fileformat=localDstFileType, &
                      indexflag=ESMF_INDEX_DELOCAL, &
                      varname=trim(dstVarNames(1)), &
                      centerflag=.not. useDstCorner, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        else
           dstLocStream = ESMF_LocStreamCreate(dstfile, &
                              fileformat=localDstFileType, &
                      indexflag=ESMF_INDEX_DELOCAL, &
                      centerflag=.not. useDstCorner, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        dstField=ESMF_FieldCreate(dstLocStream,arrayspec,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else  ! conservative
        if (useDstMask) then
          dstMesh = ESMF_MeshCreate(dstfile, localdstfiletype, &
            maskFlag=dstmeshloc, &
            ! BOB: NEVER DO DUAL WHEN CONSERVE
            convertToDual= .false., &
            varname=trim(dstVarNames(1)), rc=localrc)
        else
          dstMesh = ESMF_MeshCreate(dstfile, localdstfiletype, &
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

    ! Create Route handle
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

    !! Open Mosaic file and get the data file path and name
    if (localsrcFileType == ESMF_FILEFORMAT_MOSAIC .and. PetNo /= 0) then
         call ESMF_GridSpecReadMosaic(srcFile, srcMosaic, tileFilePath = tileFilePath, rc=localrc)
    endif
    if (localdstFileType == ESMF_FILEFORMAT_MOSAIC .and. PetNo /= 0) then
         call ESMF_GridSpecReadMosaic(dstFile, dstMosaic, tileFilePath = tileFilePath, rc=localrc)
    endif
    
    !! Regridding all the variables
    do i = 1, srcVarCount
      !!  Read the source variable, construct the field and do the regrid
      call ESMF_ArraySpecSet(arrayspec, srcVarRank(i), ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      ungridrank = srcVarRank(i) - srcRank
      if (ungridrank > 0) then
        allocate(lbnd(ungridrank), ubnd(ungridrank))
        lbnd(:)=1
        do j=1, ungridrank
           ubnd(j)=srcVarDims(j+srcRank,i)
        enddo

        if (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC .or. &
            localsrcfiletype == ESMF_FILEFORMAT_MOSAIC) then
           srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, name=trim(srcVarNames(i)), &
                          ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
!                          gridToFieldMap=(/ungridrank+1,ungridrank+2/), rc=localrc)
                          rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
          !! UGRID file
          if (srcIsLocStream) then
             srcField = ESMF_FieldCreate(srcLocStream, arrayspec, name=trim(srcVarNames(i)), &
                          ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
!                          gridToFieldMap=(/ungridrank+1/), rc=localrc)
                          rc=localrc)
             if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
          else
             srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                          meshloc =srcmeshloc, name=trim(srcVarNames(i)), &
                          ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
!                          gridToFieldMap=(/ungridrank+1/), rc=localrc)
                          rc=localrc)
             if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
          endif                 
        endif
      else ! ungridrank == 0
        if (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC .or. &
            localsrcfiletype == ESMF_FILEFORMAT_MOSAIC) then
           srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, &
                          name=trim(srcVarNames(i)), &
                          rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
          !! UGRID file
          if (srcIsLocStream) then
            srcField = ESMF_FieldCreate(srcLocStream, arrayspec, &
                          name=trim(srcVarNames(i)), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else                  
            srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                          meshloc = srcmeshloc, &
                          name=trim(srcVarNames(i)), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          endif                         
        endif
      endif       

      ! Read the data from the Source Grid File to the srcField
      ! The code to deal with GRIDSPEC and UGRID are diffferent because the distgrid is
      ! arbitrary for a Mesh and there is no minIndexPDe and maxIndexPDe
      ! If it is a Mosaic file, there may be multiple DEs per PET and the data has to be
      ! read in per DE based on tile number
      if (localsrcfiletype == ESMF_FILEFORMAT_MOSAIC) then
         call ReadMosaicField(srcField, localInputFile, srcMosaic, srcVarRank(i), &
              srcVarDims(1:srcVarRank(i),i), rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      elseif (localsrcfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
#if 1
         call ESMF_FieldRead(srcField, srcFile, variableName=srcVarNames(i), &
             rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
#else
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
         if (srcVarRank(i) == 2) then
            call ESMF_ArrayGet(srcArray, farrayPtr=fptr2d, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
            allocate(start(2), count(2))
            start = tlbound(:,PetNo+1)
            count = tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
            call ESMF_IO_NCPutGetVar(srcFile, srcVarNames(i), fptr2d, start=start, count=count, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(start, count)
         else if (srcVarRank(i) == 3) then
            allocate(start(3), count(3))
            start(1:srcRank)=tlbound(:,PetNo+1)
            start(srcRank+1:3)=1
            count(1:srcRank)=tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
            count(srcRank+1:3)=ubnd(:)
!            allocate(varBuf3D(count(1), count(2), count(3)))
            call ESMF_ArrayGet(srcArray, farrayPtr=fptr3d, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_IO_NCPutGetVar(srcFile, srcVarNames(i), fptr3d, start=start, count=count, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
         !  deallocate(varBuf3D)
           deallocate(start, count)
         else if (srcVarRank(i) == 4) then
           allocate(start(4), count(4))
           start(1:srcRank)=tlbound(:,PetNo+1)
           start(srcRank+1:4)=1
           count(1:srcRank)=tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
           count(srcRank+1:4)=ubnd
       !    allocate(varBuf4D(count(1), count(2), count(3), count(4)))
           call ESMF_ArrayGet(srcArray, farrayPtr=fptr4d, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           call ESMF_IO_NCPutGetVar(srcFile, srcVarNames(i), fptr4d, start=start, count=count, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        !    deallocate(varBuf4D)
            deallocate(start, count)
         endif
         deallocate(tlbound, tubound)
#endif
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
           ! For a UGRID file,  the variable has to be located at the center for a conservative regridding
           ! For a non-conservative regridding, the variable can be located at either the center or at the
           ! corner, however, when it is located at the center, a dual mesh is generated to move it to the
           ! corner.  Therefore, the distgrid should be decided by the regrid method and the meshloc

           !if (srcmeshloc == ESMF_MESHLOC_NODE) then
           if (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE .and. &
             localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE_2ND) then
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

         ! Create an array with everything on PET0
         distgrid = ESMF_DistGridCreate((/1/),(/totalcount/), regDecomp=(/1/), &
              rc = rc)
         if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
         arrayPet0 = ESMF_ArrayCreate(distgrid, ESMF_TYPEKIND_R8, rc=localrc)
         if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
         call ESMF_ArrayRedistStore(arrayPet0, tmpArray, routehandle=rh, rc=localrc)
         if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

         !! Read in the variable in PET 0 and redistribute it, read one 2D slice at a time to save memory
         if (PetNo==0) then
           ! Open the grid and mosaic files
           ncStatus = nf90_open (path=trim(srcFile), mode=nf90_nowrite, ncid=gridid)
           if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,&
              trim(srcFile), &
              rc)) return
           ncStatus = nf90_inq_varid( gridid, srcVarNames(i), varid)
           errmsg = "variable "//trim(srcVarNames(i))// " in "//trim(srcFile)
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
              errmsg = "- variable "//trim(srcVarNames(i))//" is not defined on location "//trim(location)
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg=errmsg, ESMF_CONTEXT, rcToReturn=rc)
              return
           endif

           if (srcVarRank(i)==1) then
              ncStatus = nf90_get_var(gridid, varid, varbuffer)
              if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,&
                 errmsg, &
                 rc)) return
              call RedistOnlyFromPet0(arrayPet0,srcArray,rh,buffer=varbuffer,rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
           else if (srcVarRank(i)==2) then
              call ESMF_ArrayGet(srcArray, farrayPtr=fptr2d, rc=localrc)
              do j=1,ubnd(1)
                 start2(1)=1
                 start2(2)=j
                 count2(1)=totalcount
                 count2(2)=1
                 ncStatus = nf90_get_var(gridid, varid, varbuffer, start2, count2)
                 if (CDFCheckError (ncStatus, &
                    ESMF_METHOD, &
                    ESMF_SRCLINE,&
                    errmsg, &
                    rc)) return
                 call RedistOnlyFromPet0(arrayPet0,tmpArray,rh,buffer=varbuffer,rc=localrc)
                 if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
                 fptr2d(:,j)=tmpfptr
              enddo
           else if (srcVarRank(i)==3) then
              call ESMF_ArrayGet(srcArray, farrayPtr=fptr3d, rc=localrc)
              do k=1,ubnd(1)
                do j=1,ubnd(2)
                   start3(1)=1
                   start3(2)=k
                   start3(3)=j
                   count3(1)=totalcount
                   count3(2:3)=1
                   ncStatus = nf90_get_var(gridid, varid, varbuffer, start3, count3)
                   if (CDFCheckError (ncStatus, &
                       ESMF_METHOD, &
                       ESMF_SRCLINE,&
                       errmsg, &
                       rc)) return
                   call RedistOnlyFromPet0(arrayPet0,tmpArray,rh,buffer=varbuffer, rc=localrc)
                   if (ESMF_LogFoundError(localrc, &
                        ESMF_ERR_PASSTHRU, &
                        ESMF_CONTEXT, rcToReturn=rc)) return
                   fptr3d(:,k,j)=tmpfptr
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
           if (srcVarRank(i) == 1) then
              call RedistOnlyFromPet0(arrayPet0, srcArray,rh, rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
              !! copy the data to the actual srcField
           else if (srcVarRank(i)==2) then
              call ESMF_ArrayGet(srcArray, farrayPtr=fptr2d, rc=localrc)
              do j=1,ubnd(1)
                 call RedistOnlyFromPet0(arrayPet0,tmpArray,rh, rc=localrc)
                 if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
                 fptr2d(:,j)=tmpfptr
              enddo
           else if (srcVarRank(i)==3) then
              call ESMF_ArrayGet(srcArray, farrayPtr=fptr3d, rc=localrc)
              do k=1,ubnd(1)
                do j=1,ubnd(2)
                   call RedistOnlyFromPet0(arrayPet0,tmpArray,rh, rc=localrc)
                   if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) return
                   fptr3d(:,k,j)=tmpfptr
                enddo
              enddo
           else
              call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
                  msg = " Undistributed dimension > 2 is not supported", &
                  ESMF_CONTEXT, rcToReturn=rc)
              return
           endif
           call ESMF_ArrayDestroy(tmpArray, rc=localrc)
           if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
         endif
         call ESMF_ArrayRedistRelease(routehandle=rh, rc=localrc)
         if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
         call ESMF_ArrayDestroy(arrayPet0, rc=localrc)
         if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      endif

      !! Construct the destination field
      !! First check if the undistributed grid dimensions match with the source variable
      if ((dstVarRank(i) - dstRank) /= ungridrank) then
         call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
              msg = " the undistributed dime   nsions of the dst variable is different from the source variable", &
              ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
      do j=1, ungridrank
        if (ubnd(j) /= dstVarDims(j+dstRank,i)) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_SYS, &
              msg = " the undistributed dimensions of the dst variable is different from the source variable", &
              ESMF_CONTEXT, rcToReturn=rc)
              return
        endif    
      enddo

    call ESMF_ArraySpecSet(arrayspec, dstVarRank(i), ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if (ungridrank > 0) then
       if (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC .or. &
            localdstfiletype == ESMF_FILEFORMAT_MOSAIC) then
            dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, &
                          name=trim(dstVarNames(i)), &
                          ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
!                          gridToFieldMap=(/ungridrank+1, ungridrank+2/), rc=localrc)
                          rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       else
           !! UGRID file
           if (dstIsLocStream) then
             dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
                          name=trim(dstVarNames(i)), &
                          ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
!                          gridToFieldMap=(/ungridrank+1/), rc=localrc)
                          rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           else
             dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                          meshloc = dstmeshloc, &
                          name=trim(dstVarNames(i)), &
                          ungriddedLBound = lbnd, ungriddedUBound = ubnd, &
!                          gridToFieldMap=(/ungridrank+1/), rc=localrc)
                          rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           endif                        
       endif
    else ! ungridrank == 0
       if (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC .or. &
            localdstfiletype == ESMF_FILEFORMAT_MOSAIC) then
           dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, &
                          name=trim(dstVarNames(i)), &
                          rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       else
           !! UGRID file
           if (dstIsLocStream) then
             dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
                          name=trim(dstVarNames(i)), &
                          rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           else
             dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                          name=trim(dstVarNames(i)), &
                          meshloc = dstmeshloc, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           endif
       endif
    endif

    call ESMF_FieldGet(dstField, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    if (localDeCount > 0) then
      do j=0,localDeCount-1
        !! initialize the destination field with the _FillValue
        if (dstVarRank(i) == 1) then       
                call ESMF_FieldGet(dstField, localDe=j, farrayptr = fptr1d, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
                fptr1d(:)=dstMissingVal
        else if (dstVarRank(i) == 2) then  
                call ESMF_FieldGet(dstField, localDe=j, farrayptr = fptr2d, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
                fptr2d(:,:)=dstMissingVal
        else if (dstVarRank(i) == 3) then
                call ESMF_FieldGet(dstField, localDe=j, farrayptr = fptr3d, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
                fptr3d(:,:,:)=dstMissingVal
        else if (dstVarRank(i) == 4) then
                call ESMF_FieldGet(dstField, localDe=j, farrayptr = fptr4d, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
                fptr4d(:,:,:,:)=dstMissingVal
        endif
      enddo
    endif

    !! Call regrid
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
    ! It seems to crash the queue on yellowstone even if the write is done in sequence from different
    ! processors.  Change the code to send the data to PET0 to write
    if (localdstfiletype == ESMF_FILEFORMAT_MOSAIC) then
      call WriteMosaicField(dstField, localOutputFile, dstMosaic, dstVarRank(i), dstVarDims(1:dstVarRank(i),i), rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (localdstfiletype == ESMF_FILEFORMAT_GRIDSPEC) then
#if 0
      ! Call ESMF_FieldWrite
      ! ESMF_FieldWrite does not work if there is a time dimension in the file.  It has to be written out
      ! one time slice at a time, not the entire variable            
      call ESMF_FieldWrite(dstField, dstFile, variableName=dstVarNames(i), &
         overwrite = .TRUE., rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
#else
      allocate(tlbound(dstRank,petCnt),tubound(dstRank, petCnt))
      call ESMF_ArrayGet(dstArray, distgrid=distgrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_DistgridGet(distgrid, minIndexPDe=tlbound, maxIndexPDe=tubound, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if (dstVarRank(i) == 2) then
          call ESMF_ArrayGet(dstArray, farrayPtr=fptr2d, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
          allocate(start(2), count(2))
          start(1:dstRank) = tlbound(:,PetNo+1)
          count(1:dstRank) = tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
          total = count(1)*count(2)
          allocate(sendbuf(total))
          sendbuf=reshape(fptr2d,(/total/))
      else if (dstVarRank(i) == 3) then
          call ESMF_ArrayGet(dstArray, farrayPtr=fptr3d, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
          allocate(start(3), count(3))
          start(1:dstRank)=tlbound(:,PetNo+1)
          start(dstRank+1:3)=1
          count(1:dstRank)=tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
          count(dstRank+1:3)=ubnd
          total=count(1)*count(2)*count(3)
          allocate(sendbuf(total))
          ii=1
          do i1=1,count(3)
            do j=1,count(2)
              do k=1,count(1)
                sendbuf(ii)=fptr3d(k,j,i1)
                ii=ii+1
              enddo
            enddo
          enddo
      else if (dstVarRank(i) == 4) then
          call ESMF_ArrayGet(dstArray, farrayPtr=fptr4d, rc=localrc)
          allocate(start(4), count(4))
          start(1:dstRank)=tlbound(:,PetNo+1)
          start(dstRank+1:4)=1
          count(1:dstRank)=tubound(:,PetNo+1)-tlbound(:,PetNo+1)+1
          count(dstRank+1:4)=ubnd
          total = count(1)*count(2)*count(3)*count(4)
          allocate(sendbuf(total))
          ii=1
          do i1=1,count(4)
            do j=1,count(3)
              do l=1,count(2)
                do k=1,count(1)
                  sendbuf(ii)=fptr4d(k,l,j,i1)
                  ii=ii+1
                enddo
              enddo
            enddo
          enddo
      endif
      if (PetNo /= 0) then
        call ESMF_VMSend(vm, sendbuf, total, 0, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(sendbuf)
      else
        do i1=0,PetCnt-1
          if (i1 /= 0) then
             ! Find the total count from remote processor
             totalrecv = 1
             do j=1,dstRank
                totalrecv = totalrecv*(tubound(j,i1+1)-tlbound(j,i1+1)+1)
             enddo
             do j=1,ungridrank
                totalrecv = totalrecv*ubnd(j)
             enddo
             allocate(recvbuf(totalrecv))
             call ESMF_VMRecv(vm, recvbuf, totalrecv, i1, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
          else
             recvbuf => sendbuf
          endif
          if (dstVarRank(i) == 2) then
            start(1:dstRank) = tlbound(:,i1+1)
            count(1:dstRank) = tubound(:,i1+1)-tlbound(:,i1+1)+1
            allocate(varBuf2D(count(1),count(2)))
            varBuf2D=reshape(recvbuf, (/count(1), count(2)/))
            call ESMF_IO_NCPutGetVar(dstFile, dstVarNames(i), varBuf2D, &
                 start=start, count=count, putflag=.TRUE.,rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(varBuf2D)
            !print *, 'put out variable from ', start, count
          else if (dstVarRank(i) == 3) then
            start(1:dstRank)=tlbound(:,i1+1)
            start(dstRank+1:3)=1
            count(1:dstRank)=tubound(:,i1+1)-tlbound(:,i1+1)+1
            count(dstRank+1:3)=ubnd
            allocate(varBuf3D(count(1),count(2), count(3)))
            varBuf3D = reshape(recvbuf, (/count(1), count(2), count(3)/))
            call ESMF_IO_NCPutGetVar(dstFile, dstVarNames(i), varBuf3D, &
                 start=start, count=count, putflag=.TRUE.,rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(varBuf3D)
          else if (dstVarRank(i) == 4) then
            start(1:dstRank)=tlbound(:,i1+1)
            start(dstRank+1:4)=1
            count(1:dstRank)=tubound(:,i1+1)-tlbound(:,i1+1)+1
            count(dstRank+1:4)=ubnd
            allocate(varBuf4D(count(1),count(2), count(3), count(4)))
            varBuf4D = reshape(recvbuf, (/count(1), count(2), count(3), count(4)/))
            call ESMF_IO_NCPutGetVar(dstFile, dstVarNames(i), varBuf4D, &
                 start=start, count=count, putflag=.TRUE.,rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(varBuf4D)
          endif
          deallocate(recvbuf)
        enddo
      endif
      deallocate(start, count)
      deallocate(tlbound, tubound)
#endif
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
         ncStatus = nf90_inq_varid( gridid, dstVarNames(i), varid)
         errmsg = "variable "//trim(dstVarNames(i))// " in "//trim(dstFile)
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
            errmsg = "- variable "//trim(dstVarNames(i))//" is not defined on location "//trim(location)
            call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg=errmsg, ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
         if (dstVarRank(i)==1) then
            call ESMF_ArrayGather(dstArray, varbuffer, 0, rc=localrc)
            ncStatus = nf90_put_var(gridid, varid, varbuffer)
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
         else if (dstVarRank(i)==2) then
            call ESMF_ArrayGet(dstArray, farrayPtr=fptr2d, rc=localrc)
            do k=1,ubnd(1)
               tmpfptr=fptr2d(:,k)
               call ESMF_ArrayGather(tmpArray, varbuffer, 0, rc=localrc)
               start2(1)=1
               start2(2)=k
               count2(1)=totalcount
               count2(2)=1
               ncStatus = nf90_put_var(gridid, varid, varbuffer, start2, count2)
               if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,&
                  errmsg, &
                  rc)) return
            enddo
         else if (dstVarRank(i)==3) then
            call ESMF_ArrayGet(dstArray, farrayPtr=fptr3d, rc=localrc)
            do k=1,ubnd(1)
              do j=1,ubnd(2)
                 tmpfptr=fptr3d(:,k,j)
                 call ESMF_ArrayGather(tmpArray, varbuffer, 0, rc=localrc)
                 start3(1)=1
                 start3(2)=k
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
            if (dstVarRank(i) == 1) then
                call ESMF_ArrayGather(dstArray,varbuffer, 0, rc=localrc)                
                !! copy the data to the actual dstField
            else if (dstVarRank(i)==2) then
                call ESMF_ArrayGet(dstArray, farrayPtr=fptr2d, rc=localrc)
                do j=1,ubnd(1)
                   tmpfptr=fptr2d(:,j)
                   call ESMF_ArrayGather(tmpArray,varbuffer, 0, rc=localrc)             
                enddo
            else if (dstVarRank(i)==3) then
               call ESMF_ArrayGet(dstArray, farrayPtr=fptr3d, rc=localrc)
               do k=1,ubnd(1)
                 do j=1,ubnd(2)
                    tmpfptr=fptr3d(:,k,j)
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

    enddo !for each variable

    if (srcIsLocStream) then
       call ESMF_LocStreamDestroy(srcLocStream)
    elseif (srcIsReg .or. localsrcfiletype == ESMF_FILEFORMAT_MOSAIC) then
       call ESMF_GridDestroy(srcGrid)
    else
       call ESMF_MeshDestroy(srcMesh)
    endif
    if (dstIsLocStream) then
       call ESMF_LocStreamDestroy(dstLocStream)
    else if (dstIsReg .or. localdstfiletype == ESMF_FILEFORMAT_MOSAIC) then
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
#define ESMF_METHOD "CheckVarMask"
  subroutine CheckVarMask(filename, varname, haveMask, missingval, rc)

! ARGUMENTS:
    character(len=*),  intent(in) :: filename
    character(len=*),  intent(in) :: varname
    logical, intent(out)          :: haveMask
    real(ESMF_KIND_R8)            :: missingval
    integer, optional             :: rc

    integer :: ncStatus
    integer :: gridid, varid
    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    rc = ESMF_FAILURE
 
    ! check if varname exist for GRIDSPEC and UGRID
    ! varname could be a list of variables separated by comma, need to check all of 
    ! them
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
    ncStatus = nf90_inq_varid(gridid, varname, varid)
    errmsg = 'Variable does not exist '//trim(varname)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
    ! Get missing value attribute
    ncStatus = nf90_get_att(gridid, varid, "_FillValue", missingval)
    if (ncStatus /= nf90_noerror) then
       ncStatus = nf90_get_att(gridid, varid, "missing_value", missingval)
       if (ncStatus == nf90_noerror) then
                   haveMask = .TRUE.
       else
                   haveMask = .FALSE.
       endif
     else
         haveMask = .TRUE.
     endif
     if (.not. haveMask) missingval = 0.0
     ncStatus = nf90_close(gridid)
     if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,&
              errmsg, &
              rc)) return
     rc = ESMF_SUCCESS
     return
#else
     call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
     return
#endif
  end subroutine CheckVarMask

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "CheckVarInfo"
  subroutine CheckVarInfo(filename, varname, varexist, filetype, meshVar, attstr, &
   ndims, dims, dimids, haveMask, missingval, locstr, vartype, coordnames, rc)

! ARGUMENTS:
    character(len=*),  intent(in) :: filename
    character(len=*),  intent(in) :: varname
    logical, intent(out)          :: varexist
    type(ESMF_Fileformat_Flag), intent(in) :: filetype
    character(len=*), intent(in)  :: meshVar
    character(len=*),  intent(out):: attstr  ! the coordinate variable names
    integer, intent(out) :: ndims
    integer, intent(out) :: dims(:)
    integer, intent(out) :: dimids(:)
    logical, intent(out) :: haveMask
    real(ESMF_KIND_R8) :: missingval
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
 
    ! check if varname exist for GRIDSPEC and UGRID
    ! varname could be a list of variables separated by comma, need to check all of 
    ! them
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
         if (filetype == ESMF_FILEFORMAT_UGRID) then
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
           else
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                msg = " location attribute not defined for the variable", &
                ESMF_CONTEXT, rcToReturn=rc)
              return
           endif
           attstr = MeshVar
         else
           !  GRIDSPEC or MOSAIC fileformat
           ncStatus = nf90_inquire_attribute(gridid, varid, "coordinates", len=len)
           if (ncStatus == nf90_noerror) then
              ncStatus = nf90_get_att(gridid, varid, "coordinates", attstr)
              errmsg = 'fail to get attribute coordinates for '//trim(varname)
              if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,&
                 errmsg, &
                 rc)) return
              if (present(locStr)) then
                locStr = "face"
              endif
           else
             call ESMF_LogSetError(ESMF_FAILURE, &
                 msg=trim(filename)//' is not a GRIDSPEC, MOSAIC or a UGRID file', &
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
         else
            haveMask = .TRUE.
         endif

         if (.not. haveMask) missingval = 0.0

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
      else ! variable does not exist      
         if (fileType == ESMF_FILEFORMAT_UGRID) then
           if (present(locStr)) then
              ncStatus = nf90_inq_varid(gridid, meshVar,meshid)
              errmsg = 'mesh_topology dummy varible does not exist '//trim(filename)
              if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,&
                 errmsg, &
                 rc)) return
              if (locStr .eq. 'face') then
                 ncStatus=nf90_get_att(gridid, meshid, 'face_coordinates', locallocstr) 
                 errmsg = 'face_coordinates attribute does not exist '//trim(filename)
                 if (CDFCheckError (ncStatus, &
                    ESMF_METHOD, &
                    ESMF_SRCLINE,&
                    errmsg, &
                    rc)) return
              else ! node, default for non-conservative
                 ncStatus=nf90_get_att(gridid, meshid, 'node_coordinates', locallocstr)
                 errmsg = 'node_coordinates attribute does not exist '//trim(filename)
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
           attstr = MeshVar
         elseif (fileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
            fileType == ESMF_FILEFORMAT_MOSAIC) then
           !GRIDSPEC, find the coordinate variables using units
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
                if (.not. (attvalue(1:len) .eq. "degrees_east" .or. &
                    attvalue(1:len) .eq. "degree_east" .or. &
                    attvalue(1:len) .eq. "degree_E" .or. &
                    attvalue(1:len) .eq. "degrees_E" .or. &
                    attvalue(1:len) .eq. "degreeE" .or. &
                    attvalue(1:len) .eq. "degreesE"))  then
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
              ncStatus=nf90_get_att(gridid, varids(2), 'units', attvalue)
              if (ncStatus == nf90_noerror) then
                if (attvalue(len:len) .eq. achar(0)) len = len-1
               if (.not. (attvalue(1:len) .eq. "degrees_north" .or. &
                   attvalue(1:len) .eq. "degree_north" .or. &
                   attvalue(1:len) .eq. "degree_N" .or. &
                   attvalue(1:len) .eq. "degrees_N" .or. &
                   attvalue(1:len) .eq. "degreeN" .or. &
                   attvalue(1:len) .eq. "degreesN"))  then
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
              ncStatus = nf90_inquire(gridid, nVariables=nvars)
              errmsg = 'nf90_inquire failed '//trim(filename)
              if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,&
                 errmsg, &
                 rc)) return
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
            if (present(locStr)) then
               locStr = "face"
            endif
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
     ncStatus = nf90_close(gridid)
     if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,&
              errmsg, &
              rc)) return
     rc = ESMF_SUCCESS
     return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

  end subroutine checkVarInfo

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
    if (fileType == ESMF_FILEFORMAT_UGRID) then
      extradim = varRank - 1
      rank = 1
    else
      extradim = varRank - 2
      rank = 2
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
     if (fileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
         fileType == ESMF_FILEFORMAT_MOSAIC ) then
       ! Add coordinates attribute
       ncStatus = nf90_put_att(gridid1, varid1, 'coordinates', varStr)
       errmsg = 'attribute for '//trim(dstVarName)
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

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadMosaicField"
subroutine ReadMosaicField(field, inputfile, mosaic, rank, dims, rc)

   type(ESMF_Field), intent(in)   :: field
   character(*), intent(in)       :: inputfile
   type(ESMF_Mosaic), intent(in)  :: mosaic
   integer, intent(in)            :: rank
   integer,  intent(in)           :: dims(:)
   integer, optional, intent(out) :: rc

   ! -- local variables
    integer :: localrc
    integer :: localDe, localDeCount
    integer :: de, deCount, dimCount, tile, tileCount
    integer, dimension(:), allocatable :: deToTileMap, localDeToDeMap
    integer, dimension(:,:), allocatable :: minIndexPDe, maxIndexPDe
    integer, dimension(:,:), allocatable :: minIndexPTile, maxIndexPTile
    type(ESMF_Grid)     :: grid
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Array) :: array
    type(ESMF_VM) :: vm
    integer       :: PetNo
    type(ESMF_StaggerLoc)         :: staggerloc
    character(len=ESMF_MAXPATHLEN):: fileName
    character(len=MAXNAMELEN):: fieldName
    real(ESMF_KIND_R8), pointer   :: fptr2d(:,:), fptr3d(:,:,:), fptr4d(:,:,:,:)
    integer :: start2(2), count2(2), start3(3), count3(3), start4(4), count4(4)
    integer :: lncid, varId, ncStatus

#ifdef ESMF_NETCDF
    rc = ESMF_FAILURE

     call ESMF_VMGetCurrent(vm, rc=localrc)
     if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
     call ESMF_VMGet(vm, localPet = PetNo, rc=localrc)
     if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
     call ESMF_FieldGet(field, grid=grid, &
        staggerloc=staggerloc, localDeCount=localDeCount, rc=localrc)

     if (localDeCount == 0) then
        rc = ESMF_SUCCESS
        return
     endif
       ! -- get domain decomposition
      call ESMF_GridGet(grid, staggerloc, distgrid=distgrid, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_DistGridGet(distgrid, deCount=deCount, dimCount=dimCount, &
        tileCount=tileCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      allocate(minIndexPDe(dimCount, deCount), maxIndexPDe(dimCount, deCount),  &
        minIndexPTile(dimCount, tileCount), maxIndexPTile(dimCount, tileCount), &
        deToTileMap(deCount), localDeToDeMap(localDeCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_DistGridGet(distgrid, &
        minIndexPDe=minIndexPDe, maxIndexPDe=maxIndexPDe, &
        minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_FieldGet(field, name = fieldName, array=array, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_ArrayGet(array, deToTileMap=deToTileMap, &
        localDeToDeMap=localDeToDeMap, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      do localDe = 0, localDeCount-1
        de   = localDeToDeMap(localDe+1) + 1
        tile = deToTileMap(de)

        filename = trim(mosaic%tileDirectory)//trim(inputfile)//"."//trim(mosaic%tilenames(tile))//".nc"

        ncStatus = nf90_open(path=trim(fileName), mode=NF90_NOWRITE, ncid=lncid)
        if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
             msg="Error opening file "//trim(fileName), &
             ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

         ncStatus = nf90_inq_varid(lncid, trim(fieldName), varId)
         if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
              msg="Error inquiring variable "//trim(fieldName)//" in "//trim(fileName), &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

        if (rank==2) then
             start2(:)=minIndexPDe(:,de)
             count2(:)=maxIndexPDe(:,de)-minIndexPDe(:,de)+1
             call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr2d, &
                      rc=localrc)
!                      exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
             ncStatus = nf90_get_var(lncid, varId, fptr2d, start=start2, count=count2)
             if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                 msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                 ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        elseif (rank==3) then
             start3(1:2)=minIndexPDe(:,de)
             count3(1:2)=maxIndexPDe(:,de)-minIndexPDe(:,de)+1
             start3(3) = 1
             count3(3) = dims(3)
              call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr3d, &
                      rc=localrc)
!                      exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
             ncStatus = nf90_get_var(lncid, varId, fptr3d, start=start3, count=count3)
             if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                 msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                 ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        elseif (rank==4) then
             start4(1:2)=minIndexPDe(:,de)
             count4(1:2)=maxIndexPDe(:,de)-minIndexPDe(:,de)+1
             start4(3:4) = 1
             count4(3:4) = dims(3:4)
              call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr4d, &
                      rc=localrc)
!                      exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
             ncStatus = nf90_get_var(lncid, varId, fptr4d, start=start4, count=count4)
             if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                 msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                 ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
         endif
      enddo
        
      ncStatus = nf90_close(lncid)
      if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
        msg="Error closing NetCDF data set", &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      
      rc=ESMF_SUCCESS
      return
#else
     call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
     return
#endif
   end subroutine ReadMosaicField


#undef  ESMF_METHOD
#define ESMF_METHOD "WriteMosaicField"
subroutine WriteMosaicField(field, inputfile, mosaic, rank, dims, rc)

   type(ESMF_Field), intent(in)   :: field
   character(*), intent(in)       :: inputfile
   type(ESMF_Mosaic), intent(in)  :: mosaic
   integer, intent(in)            :: rank
   integer, intent(in)            :: dims(:)
   integer, optional, intent(out) :: rc

   ! -- local variables
    integer :: localrc
    integer :: localDe, localDeCount
    integer :: de, deCount, dimCount, tile, tileCount
    integer, dimension(:), allocatable :: deToTileMap, localDeToDeMap
    integer, dimension(:,:), allocatable :: minIndexPDe, maxIndexPDe
    integer, dimension(:,:), allocatable :: minIndexPTile, maxIndexPTile
    type(ESMF_Grid)     :: grid
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Array) :: array
    type(ESMF_VM) :: vm
    integer       :: PetNo, PetCnt, localroot
    type(ESMF_StaggerLoc)         :: staggerloc
    character(len=ESMF_MAXPATHLEN):: fileName
    character(len=MAXNAMELEN):: fieldName
    real(ESMF_KIND_R8), pointer   :: fptr2d(:,:), fptr3d(:,:,:), fptr4d(:,:,:,:)
    real(ESMF_KIND_R8), allocatable   :: buff2d(:,:), buff3d(:,:,:), buff4d(:,:,:,:)
    real(ESMF_KIND_R8), allocatable   :: sndrcvbuffer(:)
    integer :: sndcount, rcvcount, xsize, ysize
    integer, allocatable :: tilenos(:), denos(:), tilearray(:)
    integer :: lncid, varId, ncStatus
    integer :: i, j, i1, i2, i3, i4, tiles(2)

#ifdef ESMF_NETCDF
    rc = ESMF_FAILURE

     call ESMF_VMGetCurrent(vm, rc=localrc)
     if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
     call ESMF_VMGet(vm, petCount = PetCnt, localPet = PetNo, rc=localrc)
     if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
     call ESMF_FieldGet(field, grid=grid, &
        staggerloc=staggerloc, localDeCount=localDeCount, rc=localrc)

       ! -- get domain decomposition
      call ESMF_GridGet(grid, staggerloc, distgrid=distgrid, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_DistGridGet(distgrid, deCount=deCount, dimCount=dimCount, &
        tileCount=tileCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      allocate(minIndexPDe(dimCount, deCount), maxIndexPDe(dimCount, deCount),  &
        minIndexPTile(dimCount, tileCount), maxIndexPTile(dimCount, tileCount), &
        deToTileMap(deCount), localDeToDeMap(localDeCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_DistGridGet(distgrid, &
        minIndexPDe=minIndexPDe, maxIndexPDe=maxIndexPDe, &
        minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_FieldGet(field, name = fieldName, array=array, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_ArrayGet(array, deToTileMap=deToTileMap, &
        localDeToDeMap=localDeToDeMap, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      ! only write out to file if iamroot is true
      if (deCount >= 12) then 
      ! More than one PETs per tile, need to gather data to one PET to write
      ! out to file, in this case, each PET's localDeCount is always <= 1
         if (localDeCount > 1) then
            call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- localDeCount should not be more than 1 ", &
                 ESMF_CONTEXT, rcToReturn=rc)
         endif
         if (localDeCount > 0) then
            de   = localDeToDeMap(1) + 1
            tile = deToTileMap(de)
         else
            de = -1
            tile = -1
         endif
         allocate(tilearray(PetCnt*2))
         tiles(1) = de
         tiles(2) = tile
         call ESMF_VMAllGather(vm, tiles, tilearray, 2, rc=localrc)
         if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
         allocate(tilenos(PetCnt), denos(PetCnt))
         do i=1,PetCnt
	    tilenos(i)=tilearray(i*2)
            denos(i) = tilearray(i*2-1)
         enddo
         deallocate(tilearray)
         ! Find the root of my local tile
	 localDe = 0
         if (tile > 0) then
            ! get the fortran pointer from my own field first 
            if (rank==2) then
               call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr2d, &
                    rc=localrc)
               if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
               sndcount = size(fptr2d,1)*size(fptr2d,2)
               allocate(sndrcvbuffer(sndcount))
               sndrcvbuffer=reshape(fptr2d, (/sndcount/))
            elseif (rank==3) then
               call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr3d, &
                    rc=localrc)
               if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
               sndcount = size(fptr3d,1)*size(fptr3d,2)*dims(3)
               allocate(sndrcvbuffer(sndcount))
               sndrcvbuffer=reshape(fptr3d, (/sndcount/))
            elseif (rank==4) then
               call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr4d, &
                    rc=localrc)
               if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
               sndcount = size(fptr4d,1)*size(fptr4d,2)*dims(3)*dims(4)
               allocate(sndrcvbuffer(sndcount))
               sndrcvbuffer=reshape(fptr4d, (/sndcount/)) 
            endif
            ! Find the first PET that owns the same tile
	    do i=1, PetCnt
               if (tilenos(i)==tile) then
                  localroot = i-1
                  exit
               endif
            enddo
	    if (PetNo == localroot) then
              ! gather all the data from the PETs that own the tile before writing it out
              if (rank==2) then
                 allocate(buff2d(dims(1),dims(2)))
              elseif (rank==3) then
                 allocate(buff3d(dims(1),dims(2),dims(3)))
              elseif (rank==4) then
                 allocate(buff4d(dims(1),dims(2),dims(3),dims(4)))
              endif
              do i=PetNo+1, PetCnt
                 if (tilenos(i)==tile) then
                    !allocate array to receive the data
                    !Receive the data from PetNo
                    xsize = maxIndexPDe(1,denos(i))-minIndexPDe(1,denos(i))+1
                    ysize = maxIndexPDe(2,denos(i))-minIndexPDe(2,denos(i))+1
                    if (i /= PetNo+1) then 
                       if (rank==2) then
                          rcvcount = xsize*ysize
                       elseif (rank==3) then
                          rcvcount = xsize*ysize*dims(3)
                       elseif (rank==4) then
                          rcvcount = xsize*ysize*dims(3)*dims(4)
                       endif
                       ! Get the data from other PETs that own the same tile
                       allocate(sndrcvbuffer(rcvcount))
                       call ESMF_VMRecv(vm, sndrcvbuffer, rcvcount, i-1, rc=localrc)
                       if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
                    endif
                    ! Put the block of data into the big array
                    if (rank==2) then
                       j=1
                       do i2=minIndexPDe(2,denos(i)),maxIndexPDe(2,denos(i))
                          do i1=minIndexPDe(1,denos(i)),maxIndexPDe(1,denos(i))
                             buff2d(i1,i2)=sndrcvbuffer(j)
                             j=j+1
                          enddo
                       enddo
                    elseif (rank==3) then
                       j=1
                       do i3 = 1, dims(3)
                          do i2=minIndexPDe(2,denos(i)),maxIndexPDe(2,denos(i))
                             do i1=minIndexPDe(1,denos(i)),maxIndexPDe(1,denos(i))
                                buff3d(i1,i2,i3)=sndrcvbuffer(j)
                                j=j+1
                             enddo
                          enddo
                       enddo
                    elseif (rank==4) then
                       j=1
                       do i4 = 1, dims(4)
                          do i3 = 1, dims(3)
                             do i2=minIndexPDe(2,denos(i)),maxIndexPDe(2,denos(i))
                                do i1=minIndexPDe(1,denos(i)),maxIndexPDe(1,denos(i))
                                   buff4d(i1,i2,i3,i4)=sndrcvbuffer(j)
                                   j=j+1
                                enddo
                             enddo
                          enddo
                       enddo
                    endif
		    deallocate(sndrcvbuffer)
                 endif
              enddo
              ! write it out
              filename = trim(mosaic%tileDirectory)//trim(inputfile)//"."//trim(mosaic%tilenames(tile))//".nc"
              ncStatus = nf90_open(path=trim(fileName), mode=NF90_WRITE, ncid=lncid)
              if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                   msg="Error opening file "//trim(fileName), &
                   ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

              ncStatus = nf90_inq_varid(lncid, trim(fieldName), varId)
              if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                   msg="Error inquiring variable "//trim(fieldName)//" in "//trim(fileName), &
                   ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

              if (rank==2) then 
                 ncStatus = nf90_put_var(lncid, varId, buff2d)
                 if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                      msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
                 deallocate(buff2d)
              elseif (rank==3) then
                 ncStatus = nf90_put_var(lncid, varId, buff3d)
                 if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                      msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
                 deallocate(buff3d)
              elseif (rank==4) then
                 ncStatus = nf90_put_var(lncid, varId, buff4d)
                 if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                      msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
                 deallocate(buff4d)
              endif
              ncStatus = nf90_close(lncid)
              if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                   msg="Error closing NetCDF data set", &
                   ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
            else 
               ! If I am not a rootpet of a tile, just send my data to the rootpet
               ! get the fortran pointer from the field 
               call ESMF_VMSend(vm, sndrcvbuffer, sndcount, localroot, rc=localrc)
               if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
               deallocate(sndrcvbuffer)
            endif
         endif
         deallocate(tilenos, denos)
      else !deCount < 12
         do localDe = 0, localDeCount-1
            de   = localDeToDeMap(localDe+1) + 1
            tile = deToTileMap(de)
            
            filename = trim(mosaic%tileDirectory)//trim(inputfile)//"."//trim(mosaic%tilenames(tile))//".nc"
            ncStatus = nf90_open(path=trim(fileName), mode=NF90_WRITE, ncid=lncid)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                 msg="Error opening file "//trim(fileName), &
                 ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

            ncStatus = nf90_inq_varid(lncid, trim(fieldName), varId)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                 msg="Error inquiring variable "//trim(fieldName)//" in "//trim(fileName), &
                 ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

            if (rank==2) then
               call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr2d, &
                    rc=localrc)
               if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
               ncStatus = nf90_put_var(lncid, varId, fptr2d)
               if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                    msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
            elseif (rank==3) then
               call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr3d, &
                    rc=localrc)
               if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
               ncStatus = nf90_put_var(lncid, varId, fptr3d)
               if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                    msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
            elseif (rank==4) then
               call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fptr4d, &
                    rc=localrc)
               if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
               ncStatus = nf90_put_var(lncid, varId, fptr4d)
               if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                    msg="Error reading "//trim(fieldName)//" in "//trim(fileName), &
                    ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
            endif
            ncStatus = nf90_close(lncid)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
                 msg="Error closing NetCDF data set", &
                 ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
         enddo
      endif
      
      rc=ESMF_SUCCESS
      return
#else
     call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
     return
#endif
   end subroutine WriteMosaicField

!------------------------------------------------------------------------------
!
!  Using ESMF_ArrayRedist to distribute data from Pet #0
!
#undef  ESMF_METHOD
#define ESMF_METHOD "RedistOnlyFromPet0"
subroutine RedistOnlyFromPet0(arrayPet0, array, rh, buffer, rc)

  type(ESMF_Array), intent(inout)    :: arrayPet0
  type(ESMF_Array), intent(inout)    :: array
  type(ESMF_RouteHandle), intent(inout) :: rh
  real(ESMF_KIND_R8),pointer, optional:: buffer(:)
  integer, intent(out),     optional :: rc

  real(ESMF_KIND_R8), pointer :: fptr1d(:)
  integer                     :: localrc

  if (present(rc)) rc=ESMF_SUCCESS
  
   ! Set the value at PET0 only   
   if (present(buffer)) then
      call ESMF_ArrayGet(arrayPet0, localDe=0, farrayPtr=fptr1d, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      fptr1d = buffer
   endif
   call ESMF_ArrayRedist(arrayPet0, array, rh, rc=localrc)
   if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

   return
end subroutine RedistOnlyFromPet0

#if 0
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
#endif

!------------------------------------------------------------------------------

end module ESMF_FileRegridMod

