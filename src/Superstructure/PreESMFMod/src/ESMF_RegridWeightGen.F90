!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!!-------------------------------------------------------------------------------------

!==============================================================================
#define ESMF_FILENAME "ESMF_RegridWeightGen.F90"
!==============================================================================
!
!     ESMF RegridWeightGen module
module ESMF_RegridWeightGenMod
!
!==============================================================================
!
! This file contains the API wrapper for the ESMF_RegridWeightGen application
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
  use ESMF_IOUGridMod
  use ESMF_IOFileTypeCheckMod
  use ESMF_RHandleMod
  use ESMF_LocStreamMod

  implicit none

!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:

  public ESMF_RegridWeightGen

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_RegridWeightGen -- Generic interface

! !INTERFACE:
interface ESMF_RegridWeightGen

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_RegridWeightGenFile
      module procedure ESMF_RegridWeightGenDG
! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_RegridWeightGen} subroutines
!EOPI
end interface

!------------------------------------------------------------------------------
contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!#define DOBENCHMARK

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridWeightGenFile"

!BOP
! !IROUTINE: ESMF_RegridWeightGen - Generate regrid weight file from grid files
! \label{api:esmf_regridweightgenfile}
! !INTERFACE:
  ! Private name; call using ESMF_RegridWeightGen()
  subroutine ESMF_RegridWeightGenFile(srcFile, dstFile, weightFile, keywordEnforcer, &
    regridmethod, polemethod, regridPoleNPnts, lineType, normType, &
    extrapMethod, extrapNumSrcPnts, extrapDistExponent, &
    unmappedaction, ignoreDegenerate, srcFileType, dstFileType, &
    srcRegionalFlag, dstRegionalFlag, srcMeshname, dstMeshname,  &
    srcMissingvalueFlag, srcMissingvalueVar, &
    dstMissingvalueFlag, dstMissingvalueVar, &
    useSrcCoordFlag, srcCoordinateVars, &
    useDstCoordFlag, dstCoordinateVars, &
    useSrcCornerFlag, useDstCornerFlag, &
    useUserAreaFlag, largefileFlag, &
    netcdf4fileFlag, weightOnlyFlag, &
    tileFilePath, &
    verboseFlag, rc)

! !ARGUMENTS:

  character(len=*),             intent(in)            :: srcFile
  character(len=*),             intent(in)            :: dstFile
  character(len=*),             intent(in)            :: weightFile
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  type(ESMF_RegridMethod_Flag), intent(in),  optional :: regridmethod
  type(ESMF_PoleMethod_Flag),   intent(in),  optional :: polemethod
  integer,                      intent(in),  optional :: regridPoleNPnts
  type(ESMF_LineType_Flag),     intent(in),  optional :: lineType
  type(ESMF_NormType_Flag),     intent(in),  optional :: normType
  type(ESMF_ExtrapMethod_Flag),   intent(in),    optional :: extrapMethod
  integer,                        intent(in),    optional :: extrapNumSrcPnts
  real,                           intent(in),    optional :: extrapDistExponent
  type(ESMF_UnmappedAction_Flag),intent(in), optional :: unmappedaction
  logical,                      intent(in),  optional :: ignoreDegenerate
  type(ESMF_FileFormat_Flag),   intent(in),  optional :: srcFileType
  type(ESMF_FileFormat_Flag),   intent(in),  optional :: dstFileType
  logical,                      intent(in),  optional :: srcRegionalFlag
  logical,                      intent(in),  optional :: dstRegionalFlag
  character(len=*),             intent(in),  optional :: srcMeshname
  character(len=*),             intent(in),  optional :: dstMeshname
  logical,                      intent(in),  optional :: srcMissingValueFlag
  character(len=*),             intent(in),  optional :: srcMissingValueVar
  logical,                      intent(in),  optional :: dstMissingValueFlag
  character(len=*),             intent(in),  optional :: dstMissingValueVar
  logical,                      intent(in),  optional :: useSrcCoordFlag
  character(len=*),             intent(in),  optional :: srcCoordinateVars(:)
  logical,                      intent(in),  optional :: useDstCoordFlag
  character(len=*),             intent(in),  optional :: dstCoordinateVars(:)
  logical,                      intent(in),  optional :: useSrcCornerFlag
  logical,                      intent(in),  optional :: useDstCornerFlag
  logical,                      intent(in),  optional :: useUserAreaFlag
  logical,                      intent(in),  optional :: largefileFlag
  logical,                      intent(in),  optional :: netcdf4fileFlag
  logical,                      intent(in),  optional :: weightOnlyFlag
  logical,                      intent(in),  optional :: verboseFlag
  character(len=*),             intent(in),  optional :: tileFilePath
  integer,                      intent(out), optional :: rc

! !DESCRIPTION:
! This subroutine provides the same function as the {\tt ESMF\_RegridWeightGen} application
! described in Section~\ref{sec:ESMF_RegridWeightGen}.  It takes two grid files in NetCDF format and writes out an
! interpolation weight file also in NetCDF format.  The interpolation weights can be generated with the
! bilinear~(\ref{sec:interpolation:bilinear}), higher-order patch~(\ref{sec:interpolation:patch}),
! or first order conservative~(\ref{sec:interpolation:conserve}) methods.  The grid files can be in
! one of the following four formats:
! \begin{itemize}
! \item The SCRIP format~(\ref{sec:fileformat:scrip})
! \item The native ESMF format for an unstructured grid~(\ref{sec:fileformat:esmf})
! \item The CF Convention Single Tile File format~(\ref{sec:fileformat:gridspec})
! \item The proposed CF Unstructured grid (UGRID) format~(\ref{sec:fileformat:ugrid})
! \item The GRIDSPEC Mosaic File format~(\ref{sec:fileformat:mosaic})
! \end{itemize}
! \smallskip
! The weight file is created in SCRIP format~(\ref{sec:weightfileformat}).
! The optional arguments allow users to specify various options to control the regrid operation,
! such as which pole option to use,
! whether to use user-specified area in the conservative regridding, or whether ESMF should generate masks using a given
! variable's missing value.  There are also optional arguments specific to a certain type of the grid file.
! All the optional arguments are similar to the command line arguments for the {\tt ESMF\_RegridWeightGen}
! application~(\ref{sec:regridusage}). The acceptable values and the default value for the optional arguments
! are listed below.
!
! The arguments are:
!   \begin{description}
!   \item [srcFile]
!     The source grid file name.
!   \item [dstFile]
!     The destination grid file name.
!   \item [weightFile]
!     The interpolation weight file name.
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
!   \item [{[lineType]}]
!           This argument controls the path of the line which connects two points on a sphere surface. This in
!           turn controls the path along which distances are calculated and the shape of the edges that make
!           up a cell. Both of these quantities can influence how interpolation weights are calculated.
!           As would be expected, this argument is only applicable when {\tt srcField} and {\tt dstField} are
!           built on grids which lie on the surface of a sphere. Section~\ref{opt:lineType} shows a
!           list of valid options for this argument. If not specified, the default depends on the
!           regrid method. Section~\ref{opt:lineType} has the defaults by line type. Figure~\ref{line_type_support} shows
!           which line types are supported for each regrid method as well as showing the default line type by regrid method.
!     \item [{[normType]}]
!           This argument controls the type of normalization used when generating conservative weights. This option
!           only applies to weights generated with {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE}. Please see
!           Section~\ref{opt:normType} for a
!           list of valid options. If not specified {\tt normType} defaults to {\tt ESMF\_NORMTYPE\_DSTAREA}.
!     \item [{[extrapMethod]}]
!           The type of extrapolation. Please see Section~\ref{opt:extrapmethod}
!           for a list of valid options. If not specified, defaults to
!           {\tt ESMF\_EXTRAPMETHOD\_NONE}.
!     \item [{[extrapNumSrcPnts]}]
!           The number of source points to use for the extrapolation methods that use more than one source point
!           (e.g. {\tt ESMF\_EXTRAPMETHOD\_NEAREST\_IDAVG}). If not specified, defaults to 8.
!     \item [{[extrapDistExponent]}]
!           The exponent to raise the distance to when calculating weights for
!           the {\tt ESMF\_EXTRAPMETHOD\_NEAREST\_IDAVG} extrapolation method. A higher value reduces the influence
!           of more distant points. If not specified, defaults to 2.0.
!     \item [{[unmappedaction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Please see Section~\ref{const:unmappedaction} for a
!           list of valid options. If not specified, {\tt unmappedaction} defaults to {\tt ESMF\_UNMAPPEDACTION\_ERROR}.
!     \item [{[ignoreDegenerate]}]
!           Ignore degenerate cells when checking the input Grids or Meshes for errors. If this is set to true, then the
!           regridding proceeds, but degenerate cells will be skipped. If set to false, a degenerate cell produces an error.
!           If not specified, {\tt ignoreDegenerate} defaults to false.
!   \item [{[srcFileType]}]
!     The file format of the source grid. Please see
!     Section~\ref{const:fileformatflag} for a list of valid options. 
!      If not specifed, the program will determine the file format automatically.
!   \item [{[dstFileType]}]
!     The file format of the destination grid.  Please see Section~\ref{const:fileformatflag} for a list of valid options.
!      If not specifed, the program will determine the file format automatically.
!   \item [{[srcRegionalFlag]}]
!     If .TRUE., the source grid is a regional grid, otherwise,
!     it is a global grid.  The default value is .FALSE.
!   \item [{[dstRegionalFlag]}]
!     If .TRUE., the destination grid is a regional grid, otherwise,
!     it is a global grid.  The default value is .FALSE.
!   \item [{[srcMeshname]}]
!     If the source file is in UGRID format, this argument is required
!     to define the dummy variable name in the grid file that contains the
!     mesh topology info.
!   \item [{[dstMeshname]}]
!     If the destination file is in UGRID format, this argument is required
!     to define the dummy variable name in the grid file that contains the
!     mesh topology info.
!   \item [{[srcMissingValueFlag]}]
!     If .TRUE., the source grid mask will be constructed using the missing
!     values of the variable defined in {\tt srcMissingValueVar}. This flag is
!     only used for the grid defined in  the GRIDSPEC or the UGRID file formats.
!     The default value is .FALSE..
!   \item [{[srcMissingValueVar]}]
!     If {\tt srcMissingValueFlag} is .TRUE., the argument is required to define
!     the variable name whose missing values will be used to construct the grid
!     mask.  It is only used for the grid defined in  the GRIDSPEC or the UGRID
!     file formats.
!   \item [{[dstMissingValueFlag]}]
!     If .TRUE., the destination grid mask will be constructed using the missing
!     values of the variable defined in {\tt dstMissingValueVar}. This flag is
!     only used for the grid defined in  the GRIDSPEC or the UGRID file formats.
!     The default value is .FALSE..
!   \item [{[dstMissingValueVar]}]
!     If {\tt dstMissingValueFlag} is .TRUE., the argument is required to define
!     the variable name whose missing values will be used to construct the grid
!     mask.  It is only used for the grid defined in  the GRIDSPEC or the UGRID
!     file formats.
!   \item [{[useSrcCoordFlag]}]
!     If .TRUE., the coordinate variables defined in {\tt srcCoordinateVars} will
!     be used as the longitude and latitude variables for the source grid.
!     This flag is only used for the GRIDSPEC file format.  The default is .FALSE.
!   \item [{[srcCoordinateVars]}]
!     If {\tt useSrcCoordFlag} is .TRUE., this argument defines the longitude and
 !     latitude variables in the source grid file to be used for the regrid.
!     This argument is only used when the grid file is in GRIDSPEC format.
!     {\tt srcCoordinateVars} should be a array of 2 elements.
!   \item [{[useDstCoordFlag]}]
!     If .TRUE., the coordinate variables defined in {\tt dstCoordinateVars} will
!     be used as the longitude and latitude variables for the destination grid.
!     This flag is only used for the GRIDSPEC file format.  The default is .FALSE.
!   \item [{[dstCoordinateVars]}]
!     If {\tt useDstCoordFlag} is .TRUE., this argument defines the longitude and
!     latitude variables in the destination grid file to be used for the regrid.
!     This argument is only used when the grid file is in GRIDSPEC format.
!     {\tt dstCoordinateVars} should be a array of 2 elements.
!   \item [{[useSrcCornerFlag]}]
!     If {\tt useSrcCornerFlag} is .TRUE., the corner coordinates of the source file
!     will be used for regridding. Otherwise, the center coordinates will be us ed.
!     The default is .FALSE. The corner stagger is not supported for the SCRIP formatted input
!     grid or multi-tile GRIDSPEC MOSAIC input grid.
!   \item [{[useDstCornerFlag]}]
!     If {\tt useDstCornerFlag} is .TRUE., the corner coordinates of the destination file
!     will be used for regridding. Otherwise, the center coordinates will be used.
!     The default is .FALSE. The corner stagger is not supported for the SCRIP formatted input
!     grid or multi-tile GRIDSPEC MOSAIC input grid.
!   \item [{[useUserAreaFlag]}]
!     If .TRUE., the element area values defined in the grid files are used.
!     Only the SCRIP and ESMF format grid files have user specified areas. This flag
!     is only used for conservative regridding. The default is .FALSE..
!   \item [{[largefileFlag]}]
!     If .TRUE., the output weight file is in NetCDF 64bit offset format.
!     The default is .FALSE..
!   \item [{[netcdf4fileFlag]}]
!     If .TRUE., the output weight file is in NetCDF4 file format.
!     The default is .FALSE..
!   \item [{[weightOnlyFlag]}]
!     If .TRUE., the output weight file only contains factorList and factorIndexList.
!     The default is .FALSE..
!   \item [{[verboseFlag]}]
!     If .TRUE., it will print summary information about the regrid parameters,
!     default to .FALSE..
!   \item[{[tileFilePath]}]
!     Optional argument to define the path where the tile files reside. If it
!     is given, it overwrites the path defined in {\tt gridlocation} variable
!     in the mosaic file.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOP

    type(ESMF_RegridMethod_Flag) :: localRegridMethod
    type(ESMF_PoleMethod_Flag)   :: localPoleMethod
    type(ESMF_FileFormat_Flag)   :: localSrcFileType
    type(ESMF_FileFormat_Flag)   :: localDstFileType
    integer            :: localPoleNPnts
    logical            :: localUserAreaFlag
    logical            :: localLargefileFlag
    logical            :: localNetcdf4fileFlag
    logical            :: localWeightOnlyFlag
    logical            :: localVerboseFlag
    integer            :: localrc
    type(ESMF_VM)      :: vm
     integer            :: PetNo, PetCnt
    type(ESMF_Mesh)    :: srcMesh, dstMesh
    type(ESMF_Grid)    :: srcGrid, dstGrid
    type(ESMF_Field)   :: srcField, dstField
    type(ESMF_Field)   :: srcFracField, dstFracField
    integer(ESMF_KIND_I4), pointer:: factorIndexList(:,:)
    real(ESMF_KIND_R8), pointer :: factorList(:)
    integer(ESMF_KIND_I4) :: maskvals(1)
    integer            :: ind
    integer            :: srcdims(2), dstdims(2)
    integer            :: srcrank, dstrank
    logical            :: isConserve, srcIsSphere, dstIsSphere
    logical            :: addCorners
    logical            :: convertSrcToDual,convertDstToDual
    type(ESMF_MeshLoc) :: meshloc
    logical            :: srcIsReg, dstIsReg
    logical            :: srcIsMosaic, dstIsMosaic
    logical            :: srcIsRegional, dstIsRegional, typeSetFlag
    character(len=256) :: methodStr
    real(ESMF_KIND_R8), pointer :: srcArea(:)
    real(ESMF_KIND_R8), pointer :: dstArea(:)
    real(ESMF_KIND_R8), pointer :: dstFrac(:), srcFrac(:)
    integer            :: regridScheme
    integer            :: i, bigFac, xpets, ypets, xpart, ypart, xdim, ydim
    logical            :: wasCompacted
    integer(ESMF_KIND_I4), pointer:: compactedFactorIndexList(:,:)
    real(ESMF_KIND_R8), pointer :: compactedFactorList(:)
    type(ESMF_UnmappedAction_Flag) :: localUnmappedaction
    logical            :: srcMissingValue, dstMissingValue
    character(len=256) :: argStr
    logical            :: useSrcCoordVar, useDstCoordVar
    logical            :: useSrcMask, useDstMask
    logical            :: useSrcCorner, useDstCorner
    integer            :: commandbuf(6)
#ifdef DOBENCHMARK
    real(ESMF_KIND_R8) :: starttime, endtime, totaltime
    real(ESMF_KIND_R8), pointer :: sendbuf(:), recvbuf(:)
    type(ESMF_RouteHandle) :: rhandle
    real(ESMF_KIND_R8), pointer :: fptr1D(:), fptr2D(:,:)
#endif
    type(ESMF_LineType_Flag) :: localLineType
    type(ESMF_NormType_Flag):: localNormType
    logical            :: localIgnoreDegenerate
    logical            :: srcUseLocStream, dstUseLocStream
    type(ESMF_LocStream) :: srcLocStream, dstLocStream

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
    useSrcMask = .TRUE.
    useDstMask = .TRUE.
    localRegridMethod = ESMF_REGRIDMETHOD_BILINEAR
    localSrcFileType = ESMF_FILEFORMAT_UNKNOWN
    localDstFileType = ESMF_FILEFORMAT_UNKNOWN
    localVerboseFlag = .false.
    srcIsRegional = .false.
    dstIsRegional = .false.
    srcMissingValue = .false.
    dstMissingValue = .false.
    localLargeFileFlag = .false.
    localNetcdf4FileFlag = .false.
    localWeightOnlyFlag = .false.
    localUserAreaflag = .false.
    useSrcCoordVar = .false.
    useDstCoordVar = .false.
    useSrcCorner = .false.
    useDstCorner = .false.
    localPoleNPnts = 0
    localIgnoreDegenerate = .false.
    srcUseLocStream = .false.
    dstUseLocStream = .false.
    srcIsMosaic = .false.
    dstIsMosaic = .false.
    srcIsReg = .false.
    dstIsReg = .false.

    if (present(regridMethod)) then
      localRegridMethod = regridMethod
    endif

    if (present(poleMethod)) then
         localPoleMethod = poleMethod
    else if ((localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) .or. &
             (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
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

    if ((localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) .and. &
             (localPoleMethod /= ESMF_POLEMETHOD_NONE)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
          msg ="Conserve method only works with no pole", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    if (present(srcFileType)) then
       localSrcFileType = srcFileType
    else
       call ESMF_FileTypeCheck(srcfile, localSrcFileType, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(dstFileType)) then
       localDstFileType = dstFileType
    else
       call ESMF_FileTypeCheck(dstfile, localDstFileType, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
    endif


    ! Handle optional normType argument
    if (present(normType)) then
       localNormType=normType
    else
       localNormType=ESMF_NORMTYPE_DSTAREA
    endif


    ! Handle optional lineType argument
    if (present(lineType)) then
       localLineType=lineType
    else
       if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
          localLineType=ESMF_LINETYPE_GREAT_CIRCLE
       else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
          localLineType=ESMF_LINETYPE_GREAT_CIRCLE
       else
          localLineType=ESMF_LINETYPE_CART
       endif
    endif

#if 0
    ! If the src grid type is UGRID, get the dummy variable name in the file
    if (localSrcFileType == ESMF_FILEFORMAT_UGRID) then
      if (.not. present(srcMeshname)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
          msg ="srcMeshname is not given", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! If the dst grid type is UGRID, get the dummy variable name in the file
    if (localDstFileType == ESMF_FILEFORMAT_UGRID) then
      if (.not. present(dstMeshname)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
          msg ="dstMeshname is not given", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif
#endif

    ! If the src grid type is UGRID or GRIDSPEC, check if the srcMissingvalueFlag is given
    if (localSrcFileType == ESMF_FILEFORMAT_UGRID .or. &
        localSrcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
      if (present(srcMissingvalueFlag)) then
              srcMissingValue = srcMissingvalueFlag
            else
              srcMissingValue = .false.
      endif
      if (srcMissingValue) then
          if (.not. present(srcMissingvalueVar)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                  msg ="srcMissingvalueVar argument is not given", &
                  ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
      endif
    endif

    ! If the dst grid type is UGRID or GRIDSPEC, check if the dstMissingvalueVar is given
    if (localDstFileType == ESMF_FILEFORMAT_UGRID .or. &
        localDstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
      if (present(dstMissingvalueFlag)) then
          dstMissingValue = dstMissingvalueFlag
      else
          dstMissingValue = .false.
      endif
      if (dstMissingValue) then
          if (.not. present(dstMissingvalueVar)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                msg ="dstMissingvalueVar argument is not given", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
      endif
    endif

    if (srcMissingValue .and. (localSrcFileType == ESMF_FILEFORMAT_SCRIP .or. &
        localSrcFileType == ESMF_FILEFORMAT_ESMFMESH)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
              msg =" missingvalue is only supported for UGRID and GRIDSPEC", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

!      if (srcMissingValue .and. localSrcFileType == ESMF_FILEFORMAT_UGRID .and. &
!          localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) then
!          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
!           msg = " missingvalue is only supported on the mesh elements", &
!            ESMF_CONTEXT, rcToReturn=rc)
!          return
!      endif

    if (dstMissingValue .and. (localDstFileType == ESMF_FILEFORMAT_SCRIP .or. &
        localDstFileType == ESMF_FILEFORMAT_ESMFMESH)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
              msg = " missingvalue is only supported for UGRID and GRIDSPEC", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

!      if (dstMissingValue .and. localDstFileType == ESMF_FILEFORMAT_UGRID .and. &
!          localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) then
!          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
!           msg = " missingvalue is only supported on the mesh elements", &
!           ESMF_CONTEXT, rcToReturn=rc)
!          return
!      endif

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

    if (present(largefileFlag)) then
      localLargeFileFlag = largefileFlag
    endif

    if (present(netcdf4fileFlag)) then
      localNetcdf4FileFlag = netcdf4fileFlag
    endif

    if (present(weightOnlyFlag)) then
      localWeightOnlyFlag = weightOnlyFlag
    endif

    if (present(useUserAreaFlag)) then
       localUserAreaFlag = useUserAreaFlag
    endif

    if (present(useSrcCornerFlag)) then
       useSrcCorner = useSrcCornerFlag
    endif

    if (present(useDstCornerFlag)) then
       useDstCorner = useDstCornerFlag
    endif

    if (present(verboseFlag)) then
      localVerboseFlag = verboseFlag
    endif

    if ((useSrcCorner .and. localSrcFileType == ESMF_FILEFORMAT_MOSAIC) .or. &
       (useDstCorner .and. localDstFileType == ESMF_FILEFORMAT_MOSAIC)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
              msg = " Only Center Stagger is supported for the multi-tile GRIDSPEC MOSAIC grid", &
              ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

#if 0
    if ((localSrcFileType == ESMF_FILEFORMAT_MOSAIC .or. &
        localDstFileType == ESMF_FILEFORMAT_MOSAIC) .and. &
        .not. localWeightOnlyFlag) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
              msg = " If one of the grids is in GRIDSPEC MOSAIC format, the WeightOnlyFlag has to be TRUE", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
#endif

    ! user area only needed for conservative regridding
    if (localUserAreaFlag .and. .not. ((localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) .or. &
         (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND))) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg = " user defined area is only used for the conservative regridding", &
            ESMF_CONTEXT, rcToReturn=rc)
       return
    endif

    if (localUserAreaFlag .and. (localSrcFileType /= ESMF_FILEFORMAT_SCRIP .and. &
              localSrcFileType /= ESMF_FILEFORMAT_ESMFMESH) .and. &
              (localDstFileType /= ESMF_FILEFORMAT_SCRIP .and. &
              localDstFileType /= ESMF_FILEFORMAT_ESMFMESH)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
         msg = "user defined areas is supported only when the source or dest grid is in SCRIP of ESMF format", &
         ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! --src_coordinates, --dst_coordinates for GRIDSPEC file if there are multiple
    ! coordinate variables
    if (localsrcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
      if (present(useSrcCoordFlag)) then
        useSrcCoordVar = useSrcCoordFlag
      else
        useSrcCoordVar = .false.
      endif
      if (useSrcCoordVar) then
        if (.not. present(srcCoordinateVars)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg = "srcCoordinateVars argument is not given.", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      endif
    endif

    if (localdstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
      if (present(useDstCoordFlag)) then
        useDstCoordVar = useDstCoordFlag
      else
        useDstCoordVar = .false.
      endif
      if (useDstCoordVar) then
        if (.not. present(dstCoordinateVars)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg = "dstCoordinateVars argument is not given.", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      endif
    endif

    ! Use LocStream if the source file format is SCRIP and the regridmethod is nearest-neighbor
    if ((localSrcFileType /= ESMF_FILEFORMAT_GRIDSPEC .and. &
         localSrcFileType /= ESMF_FILEFORMAT_MOSAIC ) .and. &
        (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_STOD .or. &
        localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_DTOS)) then
        srcUseLocStream = .TRUE.
    endif
    ! Use LocStream if the dest file format is SCRIP and the regridmethod is non-conservative
    if ((localDstFileType /= ESMF_FILEFORMAT_GRIDSPEC .and. &
         localDstFileType /= ESMF_FILEFORMAT_MOSAIC) .and. &
        (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) .and. &
        (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
        dstUseLocStream = .TRUE.
    endif

    ! Only set useSrcMask to false if srcMissingvalue is not given and the file type is
    ! either GRIDSPEC or UGRID, same for useDstMask
    if ((.not. srcMissingvalue) .and. (localSrcFileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
         localSrcFileType == ESMF_FILEFORMAT_MOSAIC)) &
      useSrcMask = .false.

    if ((.not. dstMissingvalue) .and. (localDstFileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
         localDstFileType == ESMF_FILEFORMAT_MOSAIC)) &
      useDstMask = .false.

    ! Should I have only PetNO=0 to open the file and find out the size?
    if (PetNo == 0) then
      if (localSrcFileType == ESMF_FILEFORMAT_SCRIP) then
        call ESMF_ScripInq(srcfile, grid_rank= srcrank, grid_dims=srcdims, rc=localrc)
        if (localVerboseFlag .and. localrc /= ESMF_SUCCESS) then
          write(*,*)
                print *, 'ERROR: Unable to get dimension information from:', srcfile
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        if (srcrank == 2) then
                srcIsReg = .true.
        else
          srcIsReg = .false.
        endif
      elseif (localSrcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
        if (useSrcCoordVar) then
           call ESMF_GridspecInq(srcfile, srcrank, srcdims, coord_names=srcCoordinateVars, rc=localrc)
        else
           call ESMF_GridspecInq(srcfile, srcrank, srcdims, rc=localrc)
        endif
        if (localVerboseFlag .and. localrc /= ESMF_SUCCESS) then
          write(*,*)
                print *, 'ERROR: Unable to get dimension information from:', srcfile
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
              srcIsReg = .true.
        srcrank = 2
      elseif (localSrcFileType == ESMF_FILEFORMAT_MOSAIC) then
        srcIsMosaic = .true.
      endif
      if (localdstFileType == ESMF_FILEFORMAT_SCRIP) then
        call ESMF_ScripInq(dstfile, grid_rank=dstrank, grid_dims=dstdims, rc=localrc)
        if (localVerboseFlag .and. localrc /= ESMF_SUCCESS) then
             write(*,*)
             print *, 'ERROR: Unable to get dimension information from:', dstfile
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        if (dstrank == 2) then
                dstIsReg = .true.
        else
          dstIsReg = .false.
        endif
      elseif (localDstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
        if (useDstCoordVar) then
            call ESMF_GridspecInq(dstfile, dstrank, dstdims, coord_names=dstCoordinateVars, rc=localrc)
        else
            call ESMF_GridspecInq(dstfile, dstrank, dstdims, rc=localrc)
        endif
        if (localVerboseFlag .and. localrc /= ESMF_SUCCESS) then
           write(*,*)
           print *, 'ERROR: Unable to get dimension information from:', dstfile
        endif
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        dstrank = 2
        dstIsReg = .true.
      elseif (localDstFileType == ESMF_FILEFORMAT_MOSAIC) then
        dstIsMosaic = .true.
      endif
      commandbuf(:) = 0
      if (srcIsReg) commandbuf(1) = 1
      if (dstIsReg) commandbuf(2) = 1
      if (srcIsMosaic) commandbuf(1) = 2
      if (dstIsMosaic) commandbuf(2) = 2
      if (srcIsReg) then
        commandbuf(3) = srcdims(1)
        commandbuf(4) = srcdims(2)
      endif
      if (dstIsReg) then
        commandbuf(5) = dstdims(1)
        commandbuf(6) = dstdims(2)
      endif
      call ESMF_VMBroadcast(vm, commandbuf, 6, 0, rc=rc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Not the Root PET
      call ESMF_VMBroadcast(vm, commandbuf, 6, 0, rc=rc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      if (commandbuf(1) == 1) then
        srcIsReg = .true.
      elseif (commandbuf(1) == 2) then
        srcIsMosaic = .true.
      endif
      if (commandbuf(2) == 1) then
        dstIsReg = .true.
      elseif (commandbuf(2) == 2) then
        dstIsMosaic = .true.
      endif
      srcdims(1) = commandbuf(3)
      srcdims(2) = commandbuf(4)
      dstdims(1) = commandbuf(5)
      dstdims(2) = commandbuf(6)
    endif

    ! Print the regrid options
    if (localVerboseFlag .and. PetNo == 0) then
      print *, "Starting weight generation with these inputs: "
      print *, "  Source File: ", trim(srcfile)
      print *, "  Destination File: ", trim(dstfile)
      print *, "  Weight File: ", trim(weightFile)
      if (localWeightOnlyFlag) then
          print *, "    only output weights in the weight file"
      endif
      if (localSrcFileType == ESMF_FILEFORMAT_SCRIP) then
        print *, "  Source File is in SCRIP format"
      elseif (localSrcFileType == ESMF_FILEFORMAT_ESMFMESH) then
        print *, "  Source File is in ESMF format"
      elseif (localSrcFileType == ESMF_FILEFORMAT_UGRID) then
        print *, "  Source File is in UGRID format"
        if (srcMissingValue) then
           print *, "    Use attribute 'missing_value' of variable '", trim(srcMissingvalueVar),"' as the mask"
        endif
      elseif  (localSrcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
        print *, "  Source File is in CF Grid format"
        if (useSrcCoordVar) then
           print *, "    Use '", trim(srcCoordinateVars(1)), "' and '", trim(srcCoordinateVars(2)), &
                       "' as the coordinate variables"
        endif
        if (srcMissingValue) then
           print *, "    Use the missing values of variable '", trim(srcMissingvalueVar),"' as the mask"
       endif
      else
        print *, "  Source File is in GRIDSPEC MOSAIC format"
      endif
      if (localSrcFileType /= ESMF_FILEFORMAT_MOSAIC) then
        if (srcIsRegional) then
           print *, "  Source Grid is a regional grid"
        else
           print *, "  Source Grid is a global grid"
        endif
      endif
      if (srcIsReg)   then
         print *, "  Source Grid is a logically rectangular grid"
      elseif (.not. srcIsMosaic) then
         print *, "  Source Grid is an unstructured grid"
      endif
      if (useSrcCorner) then
         print *, "  Use the corner coordinates of the source grid to do the regrid"
      else
         print *, "  Use the center coordinates of the source grid to do the regrid"
      endif
      if (localDstFileType == ESMF_FILEFORMAT_SCRIP) then
        print *, "  Destination File is in SCRIP format"
      elseif (localDstFileType == ESMF_FILEFORMAT_ESMFMESH) then
        print *, "  Destination File is in ESMF format"
      elseif (localDstFileType == ESMF_FILEFORMAT_UGRID) then
        print *, "  Destination File is in UGRID format"
        if (dstMissingValue) then
           print *, "    Use the missing value of '", trim(dstMissingvalueVar),"' as the mask"
        endif   
      elseif  (localDstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
        print *, "  Destination File is in CF Grid format"
        if (useDstCoordVar) then
           print *, "    Use '", trim(dstCoordinateVars(1)), "' and '", trim(dstCoordinateVars(2)), &
                       "' as the coordinate variables"
        endif
        if (dstMissingValue) then
           print *, "    Use the missing value of '", trim(dstMissingvalueVar),"' as the mask"
        endif   
      else
        print *, "  Destination File is in GRIDSPEC MOSAIC format"
      endif
      if (localDstFileType /= ESMF_FILEFORMAT_MOSAIC) then
        if (dstIsRegional) then
           print *, "  Destination Grid is a regional grid"
        else
           print *, "  Destination Grid is a global grid"
        endif
      endif
      if (dstIsReg)   then
         print *, "  Destination Grid is a logically rectangular grid"
      elseif (.not. dstIsMosaic) then
         print *, "  Destination Grid is an unstructured grid"
      endif
      if (useDstCorner) then
         print *, "  Use the corner coordinates of the destination grid to do the regrid"
      else
         print *, "  Use the center coordinates of the destination grid to do the regrid"
      endif
      if (localRegridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
        print *, "  Regrid Method: bilinear"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
        print *, "  Regrid Method: conserve"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
        print *, "  Regrid Method: conserve2nd"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_PATCH) then
        print *, "  Regrid Method: patch"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_STOD) then
        print *, "  Regrid Method: nearest source to destination"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_DTOS) then
        print *, "  Regrid Method: nearest destination to source"
      endif
      if (localPoleMethod .eq. ESMF_POLEMETHOD_NONE) then
         print *, "  Pole option: NONE"
      elseif (localPoleMethod .eq. ESMF_POLEMETHOD_ALLAVG) then
         print *, "  Pole option: ALL"
      elseif (localPoleMethod .eq. ESMF_POLEMETHOD_TEETH) then
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
      if (localLargeFileFlag) then
         print *, "  Output weight file in 64bit offset NetCDF file format"
      endif
      if (localNetcdf4FileFlag) then
         print *, "  Output weight file in NetCDF4 file format"
      endif
      if (localUserAreaFlag) then
         print *, "  Use user defined cell area for both the source and destination grids"
      endif
      if (localLineType .eq. ESMF_LINETYPE_CART) then
         print *, "  Line Type: cartesian"
      elseif (localLineType .eq. ESMF_LINETYPE_GREAT_CIRCLE) then
         print *, "  Line Type: greatcircle"
      endif
      if (localNormType .eq. ESMF_NORMTYPE_DSTAREA) then
          print *, "  Norm Type: dstarea"
      elseif (localNormType .eq. ESMF_NORMTYPE_FRACAREA) then
          print *, "  Norm Type: fracarea"
      endif
      if (present(extrapMethod)) then
         if (extrapMethod%extrapmethod .eq. &
            ESMF_EXTRAPMETHOD_NONE%extrapmethod) then
          print *, "  Extrap. Method: none"
         else if (extrapMethod%extrapmethod .eq. &
            ESMF_EXTRAPMETHOD_NEAREST_STOD%extrapmethod) then
          print *, "  Extrap. Method: neareststod"
         else if (extrapMethod%extrapmethod .eq. &
            ESMF_EXTRAPMETHOD_NEAREST_IDAVG%extrapmethod) then
          print *, "  Extrap. Method: nearestidavg"
          if (present(extrapNumSrcPnts)) then
          print '(a,i0)', "   Extrap. Number of Source Points: ",extrapNumSrcPnts
          else
          print '(a,i0)', "   Extrap. Number of Source Points: ",8
          endif
          if (present(extrapDistExponent)) then
          print *, "  Extrap. Dist. Exponent: ",extrapDistExponent
          else
          print *, "  Extrap. Dist. Exponent: ",2.0
         endif
         else
          print *, "  Extrap. Method: unknown"
         endif
      else
          print *, "  Extrap. Method: none"
      endif
      if (present(tileFilePath)) then
          print *, "  Alternative tile file path: ", trim(tileFilePath)
      endif
      write(*,*)
    endif

    ! Set flags according to the regrid method
    convertSrcToDual=.false.
    convertDstToDual=.false.
    if ((localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) .or. &
        (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
      isConserve=.true.
      addCorners=.true.
      meshloc=ESMF_MESHLOC_ELEMENT
    else
      isConserve=.false.
      addCorners=.false.
      if (.not. useSrcCorner) then
         convertSrcToDual=.true.
      endif
      if (.not. useDstCorner) then
         convertDstToDual=.true.
      endif
      meshloc=ESMF_MESHLOC_NODE
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

    if (srcUseLocStream) then
       if (srcMissingValue) then
         srcLocStream = ESMF_LocStreamCreate(srcfile, &
                              fileformat=localSrcFileType, &
                      indexflag=ESMF_INDEX_GLOBAL, &
                      varname=trim(srcMissingvalueVar), &
                      centerflag=.not. useSrcCorner, rc=localrc)
       else             
         srcLocStream = ESMF_LocStreamCreate(srcfile, &
                              fileformat=localSrcFileType, &
                      indexflag=ESMF_INDEX_GLOBAL, &
                      centerflag=.not. useSrcCorner, rc=localrc)
       endif
       if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       srcField = ESMF_FieldCreate(srcLocStream, typekind=ESMF_TYPEKIND_R8, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (localSrcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
       if (useSrcCoordVar) then
           if (srcMissingValue) then
              srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), &
                        addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
                        addMask=.true., varname=trim(srcMissingvalueVar), isSphere=srcIsSphere, &
                        coordNames = srcCoordinateVars, rc=localrc)
           else
             srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), &
                        addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
             isSphere=srcIsSphere, coordNames = srcCoordinateVars,rc=localrc)
           endif
        else
           if (srcMissingValue) then
             srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), &
                        addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
                        addMask=.true., varname=trim(srcMissingvalueVar), isSphere=srcIsSphere, rc=localrc)
           else
              srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), &
                        addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
              isSphere=srcIsSphere, rc=localrc)
           endif
         endif
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
         srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (localSrcFileType == ESMF_FILEFORMAT_SCRIP) then
         if(srcIsReg) then
           srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), &
                            addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
                            isSphere=srcIsSphere, addUserArea =localUserAreaFlag, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
           srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
           srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_SCRIP, &
                     convertToDual=convertSrcToDual, addUserArea=localUserAreaFlag, &
                      rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           srcField=ESMF_FieldCreate(srcMesh,typekind=ESMF_TYPEKIND_R8,meshloc=meshloc,rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
    elseif (localSrcFileType == ESMF_FILEFORMAT_MOSAIC) then
        ! multi-tile Mosaic Cubed Sphere grid
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
    else
        ! if srcfile is not SCRIP, it is always unstructured
        if (srcMissingValue) then
           srcMesh = ESMF_MeshCreate(srcfile, localSrcFileType, &
               maskFlag =meshloc, &
               addUserArea=localUserAreaFlag, &
               convertToDual=convertSrcToDual, &
               varname=trim(srcMissingvalueVar), rc=localrc)
        else
           srcMesh = ESMF_MeshCreate(srcfile, localSrcFileType, &
                addUserArea=localUserAreaFlag, &
               convertToDual=convertSrcToDual, &
               rc=localrc)
        endif
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        srcField=ESMF_FieldCreate(srcMesh,typekind=ESMF_TYPEKIND_R8,meshloc=meshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
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

    if (dstUseLocStream) then
       if (dstMissingValue) then
         dstLocStream = ESMF_LocStreamCreate(dstfile, &
                              fileformat=localDstFileType, &
                      indexflag=ESMF_INDEX_GLOBAL, &
                      varname= trim(dstMissingvalueVar), &
                      centerflag=.not. useDstCorner, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       else
         dstLocStream = ESMF_LocStreamCreate(dstfile, &
                              fileformat=localDstFileType, &
                      indexflag=ESMF_INDEX_GLOBAL, &
                      centerflag=.not. useDstCorner, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
       endif            
       dstField = ESMF_FieldCreate(dstLocStream, typekind=ESMF_TYPEKIND_R8, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (localDstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
       if (useDstCoordVar) then
          if (dstMissingValue) then
             dstGrid = ESMF_GridCreate(dstfile, localDstFileType, (/xpart,ypart/), &
                        addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
                        addMask=.true., varname=trim(dstMissingvalueVar), isSphere=dstIsSphere, &
                        coordNames = dstCoordinateVars, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rcToReturn=rc)) return
          else
             dstGrid = ESMF_GridCreate(dstfile, localDstFileType, (/xpart,ypart/), &
                          addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
                          isSphere=dstIsSphere, coordNames=dstCoordinateVars, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rcToReturn=rc)) return
          endif
      else
          if (dstMissingValue) then
             dstGrid = ESMF_GridCreate(dstfile, localDstFileType, (/xpart,ypart/), &
                        addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
                        addMask=.true., varname=trim(dstMissingvalueVar), isSphere=dstIsSphere, rc=localrc)
             if (ESMF_LogFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rcToReturn=rc)) return
          else
              dstGrid = ESMF_GridCreate(dstfile, localDstFileType, (/xpart,ypart/), &
                              addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
                              isSphere=dstIsSphere, rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rcToReturn=rc)) return
          endif
      endif
      dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (localDstFileType == ESMF_FILEFORMAT_SCRIP) then
        if (dstIsReg) then
           dstGrid = ESMF_GridCreate(dstfile, localDstFileType,(/xpart, ypart/), &
                     addCornerStagger=addCorners, indexflag=ESMF_INDEX_GLOBAL, &
                     isSphere=dstIsSphere, addUserArea = localUserAreaFlag, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
           dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                      staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
            dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, &
                convertToDual=convertDstToDual, addUserArea=localUserAreaFlag, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
            dstField=ESMF_FieldCreate(dstMesh,typekind=ESMF_TYPEKIND_R8,meshloc=meshloc,rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
        endif
    elseif (localDstFileType == ESMF_FILEFORMAT_MOSAIC) then
        ! multi-tile Mosaic Cubed Sphere grid
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
    else
        ! if dstfile is not SCRIP, it is always unstructured
        if (dstMissingValue) then
           dstMesh = ESMF_MeshCreate(dstfile, localDstFileType, &
                      maskFlag=meshloc, &
                      addUserArea=localUserAreaFlag, &
                      convertToDual=convertDstToDual, &
                      varname=trim(dstMissingvalueVar), rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        else
           dstMesh = ESMF_MeshCreate(dstfile, localDstFileType, &
                        addUserArea=localUserAreaFlag, &
                        convertToDual=convertDstToDual, &
                        rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        dstField=ESMF_FieldCreate(dstMesh,typekind=ESMF_TYPEKIND_R8,meshloc=meshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

#ifdef DOBENCHMARK
    call ESMF_VMLogMemInfo('Before ESMF_FieldRegridStore',rc=rc)
#endif

    ! Create Frac Fields if conservative
    if (isConserve) then
      if (srcIsReg .or. srcIsMosaic) then
        srcFracField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      else
        srcFracField=ESMF_FieldCreate(srcMesh,typekind=ESMF_TYPEKIND_R8,meshloc=meshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if (dstIsReg .or. dstIsMosaic) then
        dstFracField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      else
        dstFracField=ESMF_FieldCreate(dstMesh,typekind=ESMF_TYPEKIND_R8,meshloc=meshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

#ifdef DOBENCHMARK
    call ESMF_VMBarrier(vm)
    call ESMF_VMWtime(starttime, rc=localrc)
#endif
    maskvals(1) = 0
    if (localPoleNPnts <= 0) localPoleNPnts = 1

    if (localRegridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
      methodStr = "Bilinear remapping"
    else if (localRegridMethod == ESMF_REGRIDMETHOD_PATCH) then
      methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Patch
    else if (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_STOD) then
      methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Nearest neighbor
    else if (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_DTOS) then
      methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Nearest neighbor
    else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
      methodStr = "Conservative remapping"
    else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
      methodStr = "Conservative remapping" ! SCRIP doesn't recognize second-order conservative
    else ! nothing recognizable so report error
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
        msg="unrecognized RegridMethod", &
        ESMF_CONTEXT, rcToReturn=rc)
    endif

    if (useSrcMask .and. useDstMask) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              srcMaskValues = maskvals, dstMaskValues = maskvals, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              factorIndexList=factorIndexList, factorList=factorList, &
              srcFracField=srcFracField, dstFracField=dstFracField, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              lineType=localLineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if (useSrcMask) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              srcMaskValues = maskvals, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              factorIndexList=factorIndexList, factorList=factorList, &
              srcFracField=srcFracField, dstFracField=dstFracField, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              lineType=localLineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if (useDstMask) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              dstMaskValues = maskvals, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              factorIndexList=factorIndexList, factorList=factorList, &
              srcFracField=srcFracField, dstFracField=dstFracField, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              lineType=locallineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else        
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              factorIndexList=factorIndexList, factorList=factorList, &
              srcFracField=srcFracField, dstFracField=dstFracField, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              lineType=locallineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif
#ifdef DOBENCHMARK
    call ESMF_VMBarrier(vm)
    call ESMF_VMWtime(endtime, rc=localrc)
    call ESMF_VMLogMemInfo('After ESMF_FieldRegridStore',rc=rc)
    ! collect all the timing information at PET0
    allocate(sendbuf(1))
    sendbuf(1)=endtime-starttime
    if (PetNo == 0) then
      allocate(recvbuf(PetCnt))
    endif
    call ESMF_VMGather(vm, sendBuf, recvBuf, 1, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    if (PetNo==0) then
       print *, "Time for ESMF_FieldRegridStore(max/avg): ", MAXVAL(recvbuf), SUM(recvbuf)/PetCnt, "seconds"
    endif
#endif
    !print *, "Time for ESMF_FieldFieldRegridStore: ", (endtime-starttime)*1000.0, "msecs"
    ! print *, PetNo, size(factorList), factorIndexList(1,1), factorIndexList(2,1)
    ! Compute areas if conservative
    ! Area only valid on PET 0 right now, when parallel Array
    ! write works, then make area io parallel
    if (.not. localWeightOnlyFlag) then
    if (isConserve) then
      if (srcIsReg .or. srcIsMosaic) then
        call computeAreaGrid(srcGrid, PetNo, srcArea, regridScheme, localrc)
        if (ESMF_LogFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call computeAreaMesh(srcMesh, vm, petNo, petCnt, srcArea, localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_MeshMergeSplitSrcInd(srcMesh,factorIndexList,localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if (dstIsReg .or. dstIsMosaic) then
        call computeAreaGrid(dstGrid, PetNo, dstArea, regridScheme, localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call computeAreaMesh(dstMesh, vm, petNo, petCnt, dstArea, localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_MeshMergeSplitDstInd(dstMesh,factorList,factorIndexList,localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif
    endif
    ! Compact weight matrix
    ! (only compact if one of the grids is irregular, because that's when the repeated entries occur)
    if (((.not. srcIsReg) .and. (.not. srcIsMosaic)) .or. &
        ((.not. dstIsReg) .and. (.not. dstIsMosaic))) then
      call compactMatrix(factorList, factorIndexList, &
        wasCompacted, &
        compactedFactorList, compactedFactorIndexList, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
    if (.not. localWeightOnlyFlag) then
       if ((localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) .and. &
           (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
        if (dstUseLocStream) then
          call computeFracLocStream(dstLocStream, vm, factorIndexList, dstFrac, localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        elseif (dstIsReg .or. dstIsMosaic) then
          call computeFracGrid(dstGrid, vm, factorIndexList, dstFrac, localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call computeFracMesh(dstMesh, vm, factorIndexList, dstFrac, localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
      else
        if (srcIsReg .or. srcIsMosaic) then
          call gatherFracFieldGrid(srcGrid, srcFracField, petNo, srcFrac, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call gatherFracFieldMesh(srcMesh, vm, srcFracField, petNo, petCnt, &
            srcFrac, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (dstIsReg .or. dstIsMosaic) then
          call gatherFracFieldGrid(dstGrid, dstFracField, petNo, dstFrac, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call gatherFracFieldMesh(dstMesh, vm, dstFracField, petNo, petCnt, &
            dstFrac, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
      endif
    endif

    !! Write the weight table into a SCRIP format NetCDF file
    if (PetNo == 0 .and. .not. localWeightOnlyFlag) then
      if (isConserve) then
          if (useSrcCoordVar .and. useDstCoordVar) then
            call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
                  dstFileType=localDstFileType, method = localRegridMethod, &
                  normType=localNormType, &
                  srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
                  dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
                  srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
                  srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
                  tileFilePath = tileFilePath, &
                  srccoordnames = srcCoordinateVars, dstcoordnames = dstCoordinateVars, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          else if (useSrcCoordVar) then         
            call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
                  dstFileType=localDstFileType, method = localRegridMethod, &
                  normType=localNormType, &
                  srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
                  dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
                  srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
                  srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
                  tileFilePath = tileFilePath, &
                  srccoordnames = srcCoordinateVars, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          elseif (useDstCoordVar) then
            call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
                  dstFileType=localDstFileType, method = localRegridMethod, &
                  normType=localNormType, &
                  srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
                  dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
                  srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
                  srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
                  tileFilePath = tileFilePath, &
                  dstcoordnames = dstCoordinateVars, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          else
            call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
                  dstFileType=localDstFileType, method = localRegridMethod, &
                  normType=localNormType, &
                  srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
                  dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
                  srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
                  tileFilePath = tileFilePath, &
                  srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif
      else
          if (useSrcCoordVar .and. useDstCoordVar) then
            call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
                  dstFileType=localDstFileType, method = localRegridMethod, &
                  normType=localNormType, dstFrac=dstFrac, &
                  largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
                  srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
                  srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
                  useSrcCorner=useSrcCorner, useDstCorner=useDstCorner, &
                  tileFilePath = tileFilePath, &
                  srccoordnames = srcCoordinateVars, dstcoordnames = dstCoordinateVars, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          elseif (useSrcCoordVar) then
            call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
                  dstFileType=localDstFileType, method = localRegridMethod, &
                  normType=localNormType, dstFrac=dstFrac, &
                  largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
                  srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
                  srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
                  useSrcCorner=useSrcCorner, useDstCorner=useDstCorner, &
                  tileFilePath = tileFilePath, &
                  srccoordnames = srcCoordinateVars, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          elseif (useDstCoordVar) then
            call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
                  dstFileType=localDstFileType, method = localRegridMethod, &
                  normType=localNormType, dstFrac=dstFrac, &
                  largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
                  srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
                  srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
                  useSrcCorner=useSrcCorner, useDstCorner=useDstCorner, &
                  tileFilePath = tileFilePath, &
                  dstcoordnames = dstCoordinateVars, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          else
            call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
                  dstFileType=localDstFileType, method = localRegridMethod, &
                  normType=localNormType, dstFrac=dstFrac, &
                  largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
                  srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
                  useSrcCorner=useSrcCorner, useDstCorner=useDstCorner, &
                  tileFilePath = tileFilePath, &
                  srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif
      endif
    elseif (.not. localWeightOnlyFlag) then
      ! Not root PET
      call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! localWeightOnlyFlag == .TRUE. for all PETs
      ! write simple weight file
      call ESMF_OutputSimpleWeightFile(weightFile, factorList, factorIndexList, &
                  title = "ESMF Regrid Weight Generator", &
                  method = localRegridMethod, &
                  largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

#ifdef DOBENCHMARK
    call ESMF_VMLogMemInfo('2nd Before ESMF_FieldRegridStore',rc=rc)
    call ESMF_VMBarrier(vm)
    call ESMF_VMWtime(starttime, rc=localrc)
    if (useSrcMask .and. useDstMask) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              srcMaskValues = maskvals, dstMaskValues = maskvals, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              routehandle=rhandle, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              lineType=locallineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if (useSrcMask) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              srcMaskValues = maskvals, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              routehandle=rhandle, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              lineType=locallineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if (useDstMask) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              dstMaskValues = maskvals, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              routehandle=rhandle, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              lineType=locallineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else        
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              routehandle=rhandle, &
              regridmethod = localRegridMethod, &
              polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
              lineType=locallineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    call ESMF_VMBarrier(vm)
    call ESMF_VMWtime(endtime, rc=localrc)
    call ESMF_VMLogMemInfo('2nd After ESMF_FieldRegridStore',rc=rc)
    ! collect all the timing information at PET0
    sendbuf(1)=endtime-starttime
    call ESMF_VMGather(vm, sendBuf, recvBuf, 1, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    if (PetNo==0) then
       print *, "Time for ESMF_FieldRegridStore with rhandle(max/avg): ", MAXVAL(recvbuf), SUM(recvbuf)/PetCnt, "seconds"
    endif

    ! Now, do 10 runs of FieldRegrid and find average timing (exclude the first one)
    do i=1,10
        if (srcIsReg) then
           call ESMF_FieldGet(srcField, farrayptr=fptr2D, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
           ! assign random value
           fptr2D=i*257.4
        else
           call ESMF_FieldGet(srcField, farrayptr=fptr1D, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
           ! assign random value
           call random_seed(PUT=(/i+PetNo/))
           call random_number(fptr1d(1))
           fptr1d=i*257.4
        endif
        call ESMF_VMWtime(starttime, rc=localrc)
        call ESMF_FieldRegrid(srcField,dstField,rhandle,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMWtime(endtime, rc=localrc)
        if (i==1) then
          totaltime=0
        else
          totaltime=totaltime+endtime-starttime
        endif
     enddo      
    sendbuf(1)=totaltime/9
    call ESMF_VMGather(vm, sendBuf, recvBuf, 1, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    if (PetNo==0) then
       print *, "Time for ESMF_FieldRegrid(max/avg): ", MAXVAL(recvbuf)*1000, SUM(recvbuf)*1000/PetCnt, "mseconds"
    deallocate(sendbuf, recvbuf)
    endif
#endif

    ! Get rid of conservative arrays
    if (.not. localWeightOnlyFlag) then
    if (isConserve) then
      if (PetNo == 0) then
        deallocate(srcArea)
        deallocate(dstArea)
      endif
    endif
    if (PetNo == 0) then
      deallocate(dstFrac)
      if (isConserve) deallocate(srcFrac)
    endif
    endif

    ! clean up
    deallocate(factorList, factorIndexList)
    call ESMF_FieldDestroy(srcField)
    call ESMF_FieldDestroy(dstField)
    if (isConserve) then
       call ESMF_FieldDestroy(srcFracField)
       call ESMF_FieldDestroy(dstFracField)
    endif
    if (srcUseLocStream) then
       call ESMF_LocStreamDestroy(srcLocStream)
    elseif (srcIsReg .or. srcIsMosaic) then
       call ESMF_GridDestroy(srcGrid)
    else
       call ESMF_MeshDestroy(srcMesh)
    endif
    if (dstUseLocStream) then
       call ESMF_LocStreamDestroy(dstLocStream)
    elseif (dstIsReg .or. dstIsMosaic) then
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
  end subroutine ESMF_RegridWeightGenFile


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridWeightGenDG"

!BOP
! !IROUTINE: ESMF_RegridWeightGen - Generate regrid routeHandle and an optional weight file from grid files with user-specified distribution
! \label{api:esmf_regridweightgenDG}
! !INTERFACE:
  ! Private name; call using ESMF_RegridWeightGen()
  subroutine ESMF_RegridWeightGenDG(srcFile, dstFile, regridRouteHandle, &
    keywordEnforcer, srcElementDistgrid, dstElementDistgrid, &
    srcNodalDistgrid, dstNodalDistgrid, &
    weightFile, regridmethod, lineType, normType, &
    extrapMethod, extrapNumSrcPnts, extrapDistExponent, &
    unmappedaction, ignoreDegenerate, useUserAreaFlag, &
    largefileFlag, netcdf4fileFlag, &
    weightOnlyFlag, verboseFlag, rc)

! !ARGUMENTS:

  character(len=*),             intent(in)            :: srcFile
  character(len=*),             intent(in)            :: dstFile
  type(ESMF_RouteHandle),       intent(out)           :: regridRouteHandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  type(ESMF_DistGrid),          intent(in),  optional :: srcElementDistgrid
  type(ESMF_DistGrid),          intent(in),  optional :: dstElementDistgrid
  character(len=*),             intent(in),  optional :: weightFile
  type(ESMF_DistGrid),          intent(in),  optional :: srcNodalDistgrid
  type(ESMF_DistGrid),          intent(in),  optional :: dstNodalDistgrid
  type(ESMF_RegridMethod_Flag), intent(in),  optional :: regridmethod
  type(ESMF_LineType_Flag),     intent(in),  optional :: lineType
  type(ESMF_NormType_Flag),     intent(in),  optional :: normType
  type(ESMF_ExtrapMethod_Flag),   intent(in),    optional :: extrapMethod
  integer,                        intent(in),    optional :: extrapNumSrcPnts
  real,                           intent(in),    optional :: extrapDistExponent
  type(ESMF_UnmappedAction_Flag),intent(in), optional :: unmappedaction
  logical,                      intent(in),  optional :: ignoreDegenerate
  logical,                      intent(in),  optional :: useUserAreaFlag
  logical,                      intent(in),  optional :: largefileFlag
  logical,                      intent(in),  optional :: netcdf4fileFlag
  logical,                      intent(in),  optional :: weightOnlyFlag
  logical,                      intent(in),  optional :: verboseFlag
  integer,                      intent(out), optional :: rc

! !DESCRIPTION:
! This subroutine does online regridding weight generation from files with user specified distribution.
! The main differences between this API and the one in \ref{api:esmf_regridweightgenfile} are listed below:
! \begin{itemize}
! \item The input grids are always represented as {\tt ESMF\_Mesh} whether they are logically rectangular or unstructured.
! \item The input grids will be decomposed using a user-specified distribution instead of a fixed decomposition in the
! other subroutine if {\tt srcElementDistgrid} and {\tt dstElementDistgrid} are specified.
! \item The source and destination grid files have to be in the SCRIP grid file format.
! \item This subroutine has one additional required argument {\tt regridRouteHandle} and four additional optional
! arguments: {\tt srcElementDistgrid}, {\tt dstElementDistgrid}, {\tt srcNodelDistgrid} and {\tt dstNodalDistgrid}.
! These four arguments are of type {\tt ESMF\_DistGrid}, they are used to define the distribution of the source
! and destination grid elements and nodes. The output {\tt regridRouteHandle} allows users to regrid the field
! values later in the application.
! \item The {\tt weightFile} argument is optional. When it is given, a weightfile will be generated as well.
! \end{itemize}
! \smallskip
!
! The arguments are:
!   \begin{description}
!   \item [srcFile]
!     The source grid file name in SCRIP grid file format
!   \item [dstFile]
!     The destination grid file name in SCRIP grid file format
!   \item [regridRouteHandle]
!     The regrid RouteHandle returned by {\tt ESMF\_FieldRegridStore()}
!   \item [srcElementDistgrid]
!     An optional distGrid that specifies the distribution of the source grid's elements. If not
!     specified, a system-defined block decomposition is used.
!   \item [dstElementDistgrid]
!     An optional distGrid that specifies the distribution of the destination grid's elements. If
!     not specified, a system-defined block decomposition is used.
!   \item [weightFile]
!     The interpolation weight file name. If present, an output weight file will be generated.
!   \item [srcNodalDistgrid]
!     An optional distGrid that specifies the distribution of the source grid's nodes
!   \item [dstNodalDistgrid]
!     An optional distGrid that specifies the distribution of the destination grid's nodes
!   \item [{[regridmethod]}]
!     The type of interpolation. Please see Section~\ref{opt:regridmethod}
!     for a list of valid options. If not specified, defaults to
!     {\tt ESMF\_REGRIDMETHOD\_BILINEAR}.
!   \item [{[lineType]}]
!           This argument controls the path of the line which connects two points on a sphere surface. This in
!           turn controls the path along which distances are calculated and the shape of the edges that make
!           up a cell. Both of these quantities can influence how interpolation weights are calculated.
!           As would be expected, this argument is only applicable when {\tt srcField} and {\tt dstField} are
!           built on grids which lie on the surface of a sphere. Section~\ref{opt:lineType} shows a
!           list of valid options for this argument. If not specified, the default depends on the
!           regrid method. Section~\ref{opt:lineType} has the defaults by line type. Figure~\ref{line_type_support} shows
!           which line types are supported for each regrid method as well as showing the default line type by regrid method.
!     \item [{[normType]}]
!           This argument controls the type of normalization used when generating conservative weights. This option
!           only applies to weights generated with {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE}. Please see
!           Section~\ref{opt:normType} for a
!           list of valid options. If not specified {\tt normType} defaults to {\tt ESMF\_NORMTYPE\_DSTAREA}.
!     \item [{[extrapMethod]}]
!           The type of extrapolation. Please see Section~\ref{opt:extrapmethod}
!           for a list of valid options. If not specified, defaults to
!           {\tt ESMF\_EXTRAPMETHOD\_NONE}.
!     \item [{[extrapNumSrcPnts]}]
!           The number of source points to use for the extrapolation methods that use more than one source point
!           (e.g. {\tt ESMF\_EXTRAPMETHOD\_NEAREST\_IDAVG}). If not specified, defaults to 8..
!     \item [{[extrapDistExponent]}]
!           The exponent to raise the distance to when calculating weights for
!           the {\tt ESMF\_EXTRAPMETHOD\_NEAREST\_IDAVG} extrapolation method. A higher value reduces the influence
!           of more distant points. If not specified, defaults to 2.0.
!     \item [{[unmappedaction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Please see Section~\ref{const:unmappedaction} for a
!           list of valid options. If not specified, {\tt unmappedaction} defaults to {\tt ESMF\_UNMAPPEDACTION\_ERROR}.
!     \item [{[ignoreDegenerate]}]
!           Ignore degenerate cells when checking the input Grids or Meshes for errors. If this is set to true, then the
!           regridding proceeds, but degenerate cells will be skipped. If set to false, a degenerate cell produces an error.
!           If not specified, {\tt ignoreDegenerate} defaults to false.
!   \item [{[useUserAreaFlag]}]
!     If .TRUE., the element area values defined in the grid files are used.
!     Only the SCRIP and ESMF format grid files have user specified areas. This flag
!     is only used for conservative regridding. The default is .FALSE.
!   \item [{[largefileFlag]}]
!     If .TRUE., the output weight file is in NetCDF 64bit offset format.
!     The default is .FALSE.
!   \item [{[netcdf4fileFlag]}]
!     If .TRUE., the output weight file is in NetCDF4 file format.
!     The default is .FALSE.
!   \item [{[weightOnlyFlag]}]
!     If .TRUE., the output weight file only contains factorList and factorIndexList.
!     The default is .FALSE.
!   \item [{[verboseFlag]}]
!     If .TRUE., it will print summary information about the regrid parameters,
!     default to .FALSE.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOP

    type(ESMF_RegridMethod_Flag) :: localRegridMethod
    logical            :: localUserAreaFlag
     logical            :: localLargefileFlag
    logical            :: localNetcdf4fileFlag
    logical            :: localWeightOnlyFlag
    logical            :: localVerboseFlag
    integer            :: localrc
    type(ESMF_VM)      :: vm
    integer            :: PetNo, PetCnt
    type(ESMF_Mesh)    :: srcMesh, dstMesh
    type(ESMF_Field)   :: srcField, dstField
    type(ESMF_Field)   :: srcFracField, dstFracField
    type(ESMF_ArraySpec) :: arrayspec
    integer(ESMF_KIND_I4) :: maskvals(1)
    integer(ESMF_KIND_I4), pointer:: factorIndexList(:,:)
    real(ESMF_KIND_R8), pointer :: factorList(:)
    integer            :: ind
    logical            :: convert3D
    logical            :: isConserve
    logical            :: convertToDual
    type(ESMF_MeshLoc) :: meshloc
    character(len=256) :: methodStr
    real(ESMF_KIND_R8), pointer :: srcArea(:)
    real(ESMF_KIND_R8), pointer :: dstArea(:)
    real(ESMF_KIND_R8), pointer :: dstFrac(:), srcFrac(:)
    integer            :: regridScheme
    logical            :: wasCompacted
    integer(ESMF_KIND_I4), pointer:: compactedFactorIndexList(:,:)
    real(ESMF_KIND_R8), pointer :: compactedFactorList(:)
    type(ESMF_UnmappedAction_Flag) :: localUnmappedaction
    character(len=256) :: argStr
    !real(ESMF_KIND_R8) :: starttime, endtime
    type(ESMF_LineType_Flag) :: localLineType
    type(ESMF_NormType_Flag):: localNormType
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
    localRegridMethod = ESMF_REGRIDMETHOD_BILINEAR
    localVerboseFlag = .false.
    localLargeFileFlag = .false.
    localNetcdf4FileFlag = .false.
    localWeightOnlyFlag = .false.
    localUserAreaflag = .false.
    localIgnoreDegenerate = .false.

    if (present(regridMethod)) then
        localRegridMethod = regridMethod
    endif

    if (present(unmappedaction)) then
      localUnmappedaction = unmappedaction
    else
      localUnmappedaction = ESMF_UNMAPPEDACTION_ERROR
    endif

    if (present(ignoreDegenerate)) then
      localIgnoreDegenerate = ignoreDegenerate
    endif

    if (present(largefileFlag)) then
            localLargeFileFlag = largefileFlag
    endif

    if (present(netcdf4fileFlag)) then
            localNetcdf4FileFlag = netcdf4fileFlag
    endif

    if (present(weightOnlyFlag)) then
            localWeightOnlyFlag = weightOnlyFlag
    endif
    if (present(useUserAreaFlag)) then
            localUserAreaFlag = useUserAreaFlag
    endif

    if (present(verboseFlag)) then
      localVerboseFlag = verboseFlag
    endif

    ! Handle optional normType argument
    if (present(normType)) then
       localNormType=normType
    else
       localNormType=ESMF_NORMTYPE_DSTAREA
    endif

    ! Handle optional lineType argument
    if (present(lineType)) then
       localLineType=lineType
    else
       if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
          localLineType=ESMF_LINETYPE_GREAT_CIRCLE
       else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
          localLineType=ESMF_LINETYPE_GREAT_CIRCLE
       else
          localLineType=ESMF_LINETYPE_CART
       endif
    endif

    ! user area only needed for conservative regridding
    if (localUserAreaFlag .and. .not. ((localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) .or. &
         (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND))) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg = " user defined area is only used for the conservative regridding", &
            ESMF_CONTEXT, rcToReturn=rc)
       return
    endif

    ! Print the regrid options
    if (localVerboseFlag .and. PetNo == 0) then
      print *, "Starting weight generation with these inputs: "
      print *, "  Source File: ", trim(srcfile)
      print *, "  Destination File: ", trim(dstfile)
      if (present(weightFile)) then
        print *, "  Weight File: ", trim(weightFile)
        if (localWeightOnlyFlag) then
          print *, "  only output weights in the weight file"
        endif
      endif
      if (localRegridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
        print *, "  Regrid Method: bilinear"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
        print *, "  Regrid Method: conserve"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
        print *, "  Regrid Method: conserve2nd"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_PATCH) then
        print *, "  Regrid Method: patch"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_STOD) then
        print *, "  Regrid Method: nearest source to destination"
      elseif (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_DTOS) then
        print *, "  Regrid Method: nearest destination to source"
            endif
      if (localUnmappedaction .eq. ESMF_UNMAPPEDACTION_IGNORE) then
              print *, "  Ignore unmapped destination points"
      endif
            if (localLargeFileFlag) then
              print *, "  Output weight file in 64bit offset NetCDF file format"
      endif
            if (localNetcdf4FileFlag) then
              print *, "  Output weight file in NetCDF4 file format"
      endif
            if (localUserAreaFlag) then
               print *, "  Use user defined cell area for both the source and destination grids"
      endif
      if (localLineType .eq. ESMF_LINETYPE_CART) then
         print *, "  Line Type: cartesian"
      elseif (localLineType .eq. ESMF_LINETYPE_GREAT_CIRCLE) then
         print *, "  Line Type: greatcircle"
      endif
      if (localNormType .eq. ESMF_NORMTYPE_DSTAREA) then
              print *, "  Norm Type: dstarea"
      elseif (localNormType .eq. ESMF_NORMTYPE_FRACAREA) then
              print *, "  Norm Type: fracarea"
      endif
      if (present(extrapMethod)) then
         if (extrapMethod%extrapmethod .eq. &
            ESMF_EXTRAPMETHOD_NONE%extrapmethod) then
          print *, "  Extrap. Method: none"
         else if (extrapMethod%extrapmethod .eq. &
            ESMF_EXTRAPMETHOD_NEAREST_STOD%extrapmethod) then
          print *, "  Extrap. Method: neareststod"
         else if (extrapMethod%extrapmethod .eq. &
            ESMF_EXTRAPMETHOD_NEAREST_IDAVG%extrapmethod) then
          print *, "  Extrap. Method: nearestidavg"
          if (present(extrapNumSrcPnts)) then
          print '(a,i0)', "   Extrap. Number of Source Points: ",extrapNumSrcPnts
          else
          print '(a,i0)', "   Extrap. Number of Source Points: ",8
          endif
          if (present(extrapDistExponent)) then
          print *, "  Extrap. Dist. Exponent: ",extrapDistExponent
          else
          print *, "  Extrap. Dist. Exponent: ",2.0
         endif
         else
          print *, "  Extrap. Method: unknown"
         endif
      else
          print *, "  Extrap. Method: none"
      endif
      write(*,*)
    endif

    ! Set flags according to the regrid method
    if ((localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) .or. &
        (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
      isConserve=.true.
      convertToDual=.false.
      meshloc=ESMF_MESHLOC_ELEMENT
    else
      isConserve=.false.
      convertToDual=.true.
      meshloc=ESMF_MESHLOC_NODE
    endif

    regridScheme = ESMF_REGRID_SCHEME_FULL3D
    maskvals(1) = 0

    !Read in the srcfile and create the corresponding ESMF_Mesh object
    if (present(srcElementDistgrid) .and. present(srcNodalDistgrid)) then
      srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_SCRIP, &
          convertToDual=convertToDual, addUserArea=localUserAreaFlag, &
          elementDistgrid=srcElementDistgrid, nodalDistgrid=srcNodalDistgrid, &
          rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (present(srcElementDistgrid)) then
      srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_SCRIP, &
        convertToDual=convertToDual, addUserArea=localUserAreaFlag, &
        elementDistgrid=srcElementDistgrid, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (present(srcNodalDistgrid)) then
      srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_SCRIP, &
        convertToDual=convertToDual, addUserArea=localUserAreaFlag, &
        nodalDistgrid=srcNodalDistgrid, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    else
      srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_SCRIP, &
        convertToDual=convertToDual, addUserArea=localUserAreaFlag, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    !call ESMF_MeshWrite(srcMesh, "srcMesh", rc)
    call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    srcField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=meshloc,rc=localrc)
    if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    !Read in the dstfile and create the corresponding ESMF object (either
    ! ESMF_Grid or ESMF_Mesh)
    if (present(dstElementDistgrid) .and. present(dstNodalDistgrid)) then
      dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, &
        convertToDual=convertToDual, addUserArea=localUserAreaFlag, &
        elementDistgrid=dstElementDistgrid, nodalDistgrid=dstNodalDistgrid, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (present(dstElementDistgrid)) then
      dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, &
        convertToDual=convertToDual, addUserArea=localUserAreaFlag, &
        elementDistgrid=dstElementDistgrid, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (present(dstNodalDistgrid)) then
      dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, &
        convertToDual=convertToDual, addUserArea=localUserAreaFlag, &
        nodalDistgrid=dstNodalDistgrid, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    else
      dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, &
        convertToDual=convertToDual, addUserArea=localUserAreaFlag, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    dstField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=meshloc,rc=localrc)
    if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    ! Create Frac Fields if conservative
    if (isConserve) then
      call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      srcFracField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=meshloc,rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      dstFracField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=meshloc,rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    !call ESMF_VMBarrier(vm)
    !call ESMF_VMWtime(starttime, rc=localrc)
    if (localRegridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
      methodStr = "Bilinear remapping"
    else if (localRegridMethod == ESMF_REGRIDMETHOD_PATCH) then
      methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Patch
    else if (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_STOD) then
      methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Nearest neighbor
    else if (localRegridMethod == ESMF_REGRIDMETHOD_NEAREST_DTOS) then
      methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Nearest neighbor
    else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
      methodStr = "Conservative remapping"
    else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
      methodStr = "Conservative remapping" ! SCRIP doesn't recognize 2nd order
    else ! nothing recognizable so report error
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
        msg="unrecognized RegridMethod", &
        ESMF_CONTEXT, rcToReturn=rc)
    endif

    if (present(weightFile)) then
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              srcMaskValues = maskvals, dstMaskValues = maskvals, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              routehandle=regridRouteHandle, &
              factorIndexList=factorIndexList, factorList=factorList, &
              srcFracField=srcFracField, dstFracField=dstFracField, &
              regridmethod = localRegridMethod, &
              polemethod = ESMF_POLEMETHOD_NONE, &
              lineType=locallineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
              srcMaskValues = maskvals, dstMaskValues = maskvals, &
              unmappedaction=localUnmappedaction, &
              ignoreDegenerate=localIgnoreDegenerate, &
              routehandle=regridRouteHandle, &
              srcFracField=srcFracField, dstFracField=dstFracField, &
              regridmethod = localRegridMethod, &
              polemethod = ESMF_POLEMETHOD_NONE, &
              lineType=locallineType, &
              normType=localNormType, &
              extrapMethod=extrapMethod, &
              extrapNumSrcPnts=extrapNumSrcPnts, &
              extrapDistExponent=extrapDistExponent, &
              rc=localrc)
      if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Only compute area, fraction and output weight file is weightFile is present
    if (present(weightFile) .and. .not. localWeightOnlyFlag) then
      ! Compute areas if conservative
      ! Area only valid on PET 0 right now, when parallel Array
      ! write works, then make area io parallel
      if (isConserve) then
         call computeRedistAreaMesh(srcMesh, vm, petNo, petCnt, srcArea, localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_MeshMergeSplitSrcInd(srcMesh,factorIndexList,localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        call computeRedistAreaMesh(dstMesh, vm, petNo, petCnt, dstArea, localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_MeshMergeSplitDstInd(dstMesh,factorList,factorIndexList,localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      call compactMatrix(factorList, factorIndexList, &
        wasCompacted, &
        compactedFactorList, compactedFactorIndexList, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

      ! If the list was compacted get rid of the old lists and
      ! point to the new lists
      if (wasCompacted) then
        deallocate(factorList)
        factorList=>compactedFactorList
        deallocate(factorIndexList)
        factorIndexList=>compactedFactorIndexList
      endif

      ! Computer fraction if bilinear
      ! src fraction is always 0
      ! destination fraction depends on the src mask, dst mask, and the weight
      if ((localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) .and. &
           (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
         call computeFracMesh(dstMesh, vm, factorIndexList, dstFrac, localrc)
         if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call gatherRedistFracFieldMesh(srcMesh, vm, srcFracField, petNo, petCnt, &
          srcFrac, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

        call gatherRedistFracFieldMesh(dstMesh, vm, dstFracField, petNo, petCnt, &
          dstFrac, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      !! Write the weight table into a SCRIP format NetCDF file
      if (PetNo == 0) then
        if (isConserve) then
          call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, method = localRegridMethod, &
                  srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
                  normType=localNormType, &
                  dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
                  srcFile=srcfile, dstFile=dstfile, &
                  normType=localNormType, &
                  method = localRegridMethod, dstFrac=dstFrac, &
                  largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
              endif
      else
        call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList, &
                  normType=localNormType, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      !call ESMF_VMBarrier(vm)
      !call ESMF_VMWtime(endtime, rc=localrc)

      ! Get rid of conservative arrays
      if (isConserve) then
        if (PetNo == 0) then
           deallocate(srcArea)
           deallocate(dstArea)
        endif
      endif
      if (PetNo == 0) then
        if (isConserve) deallocate(srcFrac)
        deallocate(dstFrac)
      endif
    else if (present(weightFile)) then  !localWeightOnlyFlag = .TRUE.
      call compactMatrix(factorList, factorIndexList, &
        wasCompacted, &
        compactedFactorList, compactedFactorIndexList, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

      ! If the list was compacted get rid of the old lists and
      ! point to the new lists
      if (wasCompacted) then
        deallocate(factorList)
        factorList=>compactedFactorList
        deallocate(factorIndexList)
        factorIndexList=>compactedFactorIndexList
      endif

      ! write simple weight file
      call ESMF_OutputSimpleWeightFile(weightFile, factorList, factorIndexList, &
                  title = "ESMF Regrid Weight Generator", &
                  method = localRegridMethod, &
                  largeFileFlag=localLargeFileFlag, &
                  netcdf4FileFlag = localNetcdf4FileFlag, &
                  rc=localrc)
      if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! clean up
    call ESMF_FieldDestroy(srcField)
    call ESMF_FieldDestroy(dstField)
    if (isConserve) then
       call ESMF_FieldDestroy(srcFracField)
       call ESMF_FieldDestroy(dstFracField)
    endif
    ! ESMF_MeshDestory() will destroy the distgrid passed in as input argument
    ! Work Around: use ESMF_MeshFreeMemory() instead
    ! call ESMF_MeshDestroy(srcMesh)
    ! call ESMF_MeshDestroy(dstMesh)
    call ESMF_MeshFreeMemory(srcMesh)
    call ESMF_MeshFreeMemory(dstMesh)
    if (present(weightFile)) then
       deallocate(factorList, factorIndexList)
    endif
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
  end subroutine ESMF_RegridWeightGenDG


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeAreaGrid"

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
  integer :: minIndex(2), maxIndex(2), gridDims(2)
  real (ESMF_KIND_R8), pointer :: area2D(:,:)
  integer :: i, start, ntiles
  integer :: localrc

  ! Create a field on the grid to hold the areas
  areaField=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, name="area", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! compute areas
  call ESMF_FieldRegridGetArea(areaField, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
 if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! Get number of tiles
  call ESMF_GridGet(grid, tileCount = ntiles, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get size of Grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
        minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
  endif

  ! Grid size
  gridDims(1)=maxIndex(1)-minIndex(1)+1
  gridDims(2)=maxIndex(2)-minIndex(2)+1

  ! Allocate memory for area
  allocate(area2D(gridDims(1),gridDims(2)))

  ! Only do this part on PET 0
  if (petNo .eq. 0) then
     ! Allocate memory for area
     allocate(area(gridDims(1)*gridDims(2)*ntiles))
  endif

  ! Get area onto PET 0
  start=1
  do i=1,ntiles
    call ESMF_FieldGather(areaField, farray=area2D, rootPet=0, tile=i, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

     ! copy to 1D array
     if (PetNo == 0) then
       ! flatten area
       area(start:start+gridDims(1)*gridDims(2)-1)=RESHAPE(area2D,(/gridDims(1)*gridDims(2)/))
       start= start+gridDims(1)*gridDims(2)
     endif
  enddo

  ! deallocate memory for 2D area
  deallocate(area2D)

end subroutine computeAreaGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeRedistAreaMesh"

! For now just put into a 1D array on PET 0. When PIO array write is done, then
! do it in parallel
! AREA ONLY VALID ON PET 0
subroutine computeRedistAreaMesh(mesh, vm, petNo, petCnt, area, rc)
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: VM
  integer :: petNo,petCnt
  real (ESMF_KIND_R8), pointer :: area(:)
  integer :: rc
  real (ESMF_KIND_R8), pointer :: localArea(:)
  integer :: localrc
  integer :: localElemCount,i
  integer (ESMF_KIND_I4) :: localCount(1), globalCount(1)
  integer :: totalCount
  logical :: hasSplitElem
  type(ESMF_DistGrid) :: distgrid, justPet0Distgrid
  type(ESMF_Array) :: areaArray, justPet0Array
  type(ESMF_RouteHandle) :: rh

  ! Find out if elements are split
  call ESMF_MeshGetElemSplit(mesh, hasSplitElem=hasSplitElem, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get area depending on split elements
  if (hasSplitElem) then
     ! Get local size of mesh areas before split
     call ESMF_MeshGetElemSplit(mesh, origElemCount=localElemCount, &
            rc=localrc)
     if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

    ! allocate space for areas
    allocate(localArea(localElemCount))

    ! Get local Areas
    call ESMF_MeshGetOrigElemArea(mesh, areaList=localArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  else
     ! Get local size of mesh areas
     call ESMF_MeshGet(mesh, numOwnedElements=localElemCount, &
            rc=localrc)
     if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

    ! allocate space for areas
    allocate(localArea(localElemCount))

    ! Get local Areas
    call ESMF_MeshGetElemArea(mesh, areaList=localArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  ! The element disgrid is for the split elements, thus the following code
  ! doesn't work with split element
  call ESMF_MeshGet(mesh, elementDistgrid=distgrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

  areaArray = ESMF_ArrayCreate(distgrid, localArea, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get total size

  localCount(1)=localElemCount
  globalCount(1)=0
  call ESMF_VMAllReduce(vm,localCount,globalCount,count=1, &
            reduceflag=ESMF_REDUCE_SUM, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  totalCount=globalCount(1)

  ! Create distgrid with everything on PET 0
  justPet0Distgrid = ESMF_DistGridCreate((/1/),(/totalCount/), regDecomp=(/1/),&
                     rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  if (PetNo == 0) then
    ! Allocate final area list
    allocate(area(totalCount))
  else
    allocate(area(0))
  endif

  ! Create array from distgrid
  justPet0Array=ESMF_ArrayCreate(justPet0Distgrid, &
                         farrayPtr=area, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Redist from one to the other
  call ESMF_ArrayRedistStore(srcArray=areaArray, &
                             dstArray=justPet0Array, &
                             routehandle=rh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_ArrayRedist(srcArray=areaArray, &
                        dstArray=justPet0Array, &
                        routehandle=rh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_ArrayRedistRelease(routehandle=rh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get rid of helper variables
  call ESMF_ArrayDestroy(areaArray)
  deallocate(localArea)
  ! call ESMF_ArrayDestroy(justPet0Array)
  ! call ESMF_DistGridDestroy(justPet0Distgrid)

end subroutine computeRedistAreaMesh

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeAreaMesh"

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
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get area depending on split elements
  if (hasSplitElem) then
     ! Get local size of mesh areas before split
     call ESMF_MeshGetElemSplit(mesh, origElemCount=localElemCount, &
            rc=localrc)
     if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

    ! allocate space for areas
    allocate(localArea(localElemCount))

    ! Get local Areas
    call ESMF_MeshGetOrigElemArea(mesh, areaList=localArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  else
     ! Get local size of mesh areas
     call ESMF_MeshGet(mesh, numOwnedElements=localElemCount, &
            rc=localrc)
     if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

    ! allocate space for areas
    allocate(localArea(localElemCount))

    ! Get local Areas
    call ESMF_MeshGetElemArea(mesh, areaList=localArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  ! Allocate List of counts
  allocate(globalCount(petCnt))

  ! Get List of counts
  localCount(1)=localElemCount
  call ESMF_VMGather(vm,localCount,globalCount,count=1,rootPet=0,rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

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
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get rid of helper variables
  deallocate(localArea)
  deallocate(globalCount)
  deallocate(globalDispl)
  if (petNo .ne. 0) deallocate(area)

end subroutine computeAreaMesh

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeFracGrid"

subroutine computeFracGrid(grid, vm, indices, frac, rc)
  type(ESMF_Grid) :: grid
  type(ESMF_VM) :: vm
  integer :: indices(:,:)
  real(ESMF_KIND_R8), pointer :: frac(:)
  integer :: rc

  type (ESMF_DistGrid) :: distgrid
  integer (ESMF_KIND_I4) :: localCount(1)
  integer (ESMF_KIND_I4), pointer :: elementCount(:)
  integer (ESMF_KIND_I4),pointer :: globalCount(:),globalDispl(:)
  integer (ESMF_KIND_I4),pointer :: buffer(:),buffer1(:)
  integer :: totalCount, totalElements
  integer :: i, j, total
  integer :: petNo,petCnt
  integer :: saved, count, ntiles

  call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)

  ! Allocate List of counts
  allocate(globalCount(petCnt))

  call ESMF_GridGet(grid, distgrid=distgrid, tileCount=ntiles, rc=rc)
  if (rc /=ESMF_SUCCESS) then
      return
  endif

  allocate(elementCount(ntiles))
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
    totalElements = 0
    do i=1,ntiles
      totalElements=totalElements+elementCount(i)
    enddo
    allocate(frac(totalElements))
    frac = 0
    do i=1,totalCount
       frac(buffer1(i))=1
    enddo
  endif

  ! Get rid of helper variables
  deallocate(buffer, buffer1)
  deallocate(globalCount)
  deallocate(elementCount)
  deallocate(globalDispl)

end subroutine computeFracGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeFracLocStream"

subroutine computeFracLocStream(locstream, vm, indices, frac, rc)
  type(ESMF_Locstream) :: locstream
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

  call ESMF_LocStreamGet(locstream, distgrid=distgrid, rc=rc)
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


  ! Allocate final frac list
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

end subroutine computeFracLocStream

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeFracMesh"

subroutine computeFracMesh(mesh, vm, indices, frac, rc)
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: vm
  integer :: indices(:,:)
  real(ESMF_KIND_R8), pointer :: frac(:)
  integer :: rc

  integer (ESMF_KIND_I4) :: localCount(1)
  integer (ESMF_KIND_I4),pointer :: globalCount(:),globalDispl(:)
  integer (ESMF_KIND_I4),pointer :: buffer(:), buffer1(:)
  integer :: totalCount, maxIndex
  integer :: i, j, total
  integer :: petNo,petCnt
  integer :: count, saved

  call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)

  ! Allocate List of counts
  allocate(globalCount(petCnt))

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
    maxIndex = maxval(buffer1)
    allocate(frac(maxIndex))
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

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "gatherFracFieldGrid"

subroutine gatherFracFieldGrid(grid, fracField, petNo, frac, rc)
  type(ESMF_Grid) :: grid
  type(ESMF_Field) :: fracField
  integer :: petNo
  real (ESMF_KIND_R8), pointer :: frac(:)
  integer :: rc
  integer :: minIndex(2), maxIndex(2), gridDims(2)
  real (ESMF_KIND_R8), pointer :: frac2D(:,:)
  integer :: i, start, ntiles
  integer :: localrc


  ! Get size of Grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
       minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of tiles
  call ESMF_GridGet(grid, tileCount = ntiles, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Grid size
  gridDims(1)=maxIndex(1)-minIndex(1)+1
  gridDims(2)=maxIndex(2)-minIndex(2)+1

  ! Allocate memory for area
  allocate(frac2D(gridDims(1),gridDims(2)))

  ! Only do this part on PET 0
  if (petNo .eq. 0) then
     ! Allocate memory for area
     allocate(frac(gridDims(1)*gridDims(2)*ntiles))
  endif

  ! Get area onto PET 0
  start = 1
  do i=1,ntiles
     call ESMF_FieldGather(fracField, farray=frac2D, rootPet=0, tile=i, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
     ! copy to 1D array
     if (PetNo == 0) then
       ! flatten area
       frac(start:start+gridDims(1)*gridDims(2)-1)=RESHAPE(frac2D,(/gridDims(1)*gridDims(2)/))
       start= start+gridDims(1)*gridDims(2)
     endif
  enddo
  ! deallocate memory for 2D area
  deallocate(frac2D)

end subroutine gatherFracFieldGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "gatherFracFieldMesh"

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

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "gatherRedistFracFieldMesh"

! For now just put into a 1D array on PET 0. When PIO array write is done, then
! do it in parallel
! Frac ONLY VALID ON PET 0
subroutine gatherRedistFracFieldMesh(mesh, vm, fracField, petNo, petCnt, frac, rc)
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: VM
  integer :: petNo,petCnt
  type(ESMF_Field) :: fracField
  type(ESMF_Array) :: fracArray, justPet0Array
  type(ESMF_DistGrid) :: justPet0DistGrid
  type(ESMF_RouteHandle) :: rh
  real (ESMF_KIND_R8), pointer :: frac(:)
  integer :: rc
  real (ESMF_KIND_R8), pointer :: localFrac(:)
  real (ESMF_KIND_R8), pointer :: mergedFrac(:)
  integer :: localrc
  integer :: localElemCount,i
  integer (ESMF_KIND_I4) :: localCount(1), globalCount(1)
  integer :: totalCount
  logical :: hasSplitElem
  integer, pointer :: seqIndexList(:)



  ! Get localFrac from field
  call ESMF_FieldGet(fracField, localDE=0, farrayPtr=localFrac,  rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Find out if elements are split
  call ESMF_MeshGetElemSplit(mesh, hasSplitElem=hasSplitElem, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get merge frac field depending on if split elements
  if (hasSplitElem) then
     ! Get local size of mesh areas before split
     call ESMF_MeshGetElemSplit(mesh, origElemCount=localElemCount, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


     ! allocate space for frac
     allocate(mergedFrac(localElemCount))

     ! Get local Areas
     call ESMF_MeshGetOrigElemFrac(mesh, splitFracList=localFrac, &
          origfracList=mergedFrac, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


     ! switch to point to merged areas
     localFrac=>mergedFrac
  else
     localElemCount=size(localFrac)
     ! localFrac is gotten from the fracField above
  endif


  ! Get total size
  localCount(1)=localElemCount
  globalCount(1)=0
  call ESMF_VMReduce(vm,localCount,globalCount,count=1, &
            reduceflag=ESMF_REDUCE_SUM, rootPet=0,rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Set total size
  if (petNo==0) then
     totalCount=globalCount(1)
  else
     totalCount=0
  endif

  ! Allocate and fill array to create distgrid
  allocate(seqIndexList(totalCount))
  do i=1,totalCount
     seqIndexList(i)=i
  enddo

  ! Create distgrid with everything on PET 0
  justPet0Distgrid=ESMF_DistGridCreate(seqIndexList, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Free seqIndexList memory
  deallocate(seqIndexList)


  ! Allocate final frac list
  allocate(frac(totalCount))

  ! Create array from distgrid
  justPet0Array=ESMF_ArrayCreate(justPet0Distgrid, &
                         farrayPtr=frac, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get array from fracField
  call ESMF_FieldGet(fracField, array=fracArray, &
                     rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Redist from one to the other
  call ESMF_ArrayRedistStore(srcArray=fracArray, &
                             dstArray=justPet0Array, &
                             routehandle=rh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_ArrayRedist(srcArray=fracArray, &
                        dstArray=justPet0Array, &
                        routehandle=rh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_ArrayRedistRelease(routehandle=rh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Properly redisted fractions should now be in frac(:)


  ! Get rid of helper variables
  if (hasSplitElem) then
     deallocate(mergedFrac)
  endif

  call ESMF_ArrayDestroy(justPet0Array)
  call ESMF_DistGridDestroy(justPet0Distgrid)

end subroutine gatherRedistFracFieldMesh


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "compactMatrix"

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

   ! Sort the dstInd first
   call QSort(inListCount, inFactorIndexList, InFactorList)

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


recursive subroutine QSort(nA, A, B)

! DUMMY ARGUMENTS
integer, intent(in) :: nA
integer(ESMF_KIND_I4), intent(inout) :: A(:,:)
real(ESMF_KIND_R8), intent(inout) :: B(:)

! LOCAL VARIABLES
integer :: left, right
real(ESMF_KIND_R8) :: random, tempR
real(ESMF_KIND_I4) :: tempI(2), pivot
integer :: marker

    if (nA > 1) then

        call random_number(random)
        ! random pivor (not best performance, but avoids worst-case)
        pivot = A(2,int(random*real(nA-1))+1)
        left = 0
        right = nA + 1

        do while (left < right)
            right = right - 1
            do while (A(2,right) > pivot)
                right = right - 1
            end do
            left = left + 1
            do while (A(2,left) < pivot)
                left = left + 1
            end do
            if (left < right) then
                tempI = A(:,left)
                A(:,left) = A(:,right)
                A(:,right) = tempI
                tempR = B(left)
                B(left)=B(right)
                B(right)=tempR
            end if
        end do

        if (left == right) then
            marker = left + 1
        else
            marker = left
        end if

        call QSort(marker-1,A(:,:marker-1),B(:marker-1))
        call QSort(nA-marker+1,A(:,marker:),B(marker:))

    end if

end subroutine QSort

!------------------------------------------------------------------------------

end module ESMF_RegridWeightGenMod

