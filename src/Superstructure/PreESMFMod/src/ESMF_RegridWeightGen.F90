!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id: ESMF_RegridWeightGen.F90,v 1.5 2012/09/11 18:22:06 peggyli Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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

  
  implicit none

!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_RegridWeightGen

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------- ESMF-public method -------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridWeightGen"

!BOPI
! !IROUTINE: ESMF_RegridWeightGen - Compute a regridding operation
!
! !INTERFACE:
subroutine ESMF_RegridWeightGen(srcFile, dstFile, weightFile, regridMethod, &
     	         poleMethod, poleNPnts, ignoreUnmappedFlag, srcFileType, dstFileType, &
                 srcRegionalFlag, dstRegionalFlag, srcMeshname, dstMeshname,  &
		 srcMissingvalueFlag, srcMissingvalueVar, &
		 dstMissingvalueFlag, dstMissingvalueVar, &
		 useSrcCoordFlag, srcCoordinateVars, &
                 useDstCoordFlag, dstCoordinateVars, &
		 useUserAreaFlag,  largefileFlag, verboseFlag, rc)

! !ARGUMENTS:

	character(len=*),  intent(in)         :: srcFile
	character(len=*),  intent(in)         :: dstFile
	character(len=*),  intent(in)         :: weightFile
	type(ESMF_RegridMethod_Flag),  intent(in), optional :: regridMethod
	type(ESMF_PoleMethod_Flag),  intent(in), optional :: poleMethod
	integer,   intent(in), optional       :: poleNPnts
	logical,     intent(in), optional     :: ignoreUnmappedFlag
	type(ESMF_FileFormat_Flag), intent(in), optional :: srcFileType
	type(ESMF_FileFormat_Flag), intent(in), optional :: dstFileType
	logical,     intent(in), optional     :: srcRegionalFlag
	logical,     intent(in), optional     :: dstRegionalFlag
	character(len=*), intent(in), optional :: srcMeshname 
	character(len=*), intent(in), optional :: dstMeshname 
	logical,     intent(in), optional      :: srcMissingValueFlag
	character(len=*), intent(in), optional :: srcMissingvalueVar
	logical,     intent(in), optional      :: dstMissingValueFlag
	character(len=*), intent(in), optional :: dstMissingvalueVar
	logical,     intent(in), optional      :: useSrcCoordFlag
	character(len=*), intent(in), optional :: srcCoordinateVars(:)
	logical, intent(in), optional          :: useDstCoordFlag
	character(len=*), intent(in), optional :: dstCoordinateVars(:)
	logical, intent(in), optional          :: useUserAreaFlag
	logical, intent(in), optional          :: largefileFlag
	logical, intent(in), optional          :: verboseFlag
	integer, intent(out), optional         :: rc

! !DESCRIPTION:
!   \begin{description}
!   \item [srcFile]
!     The source grid file name.
!   \item [dstFile]
!     The destination grid file name.
!   \item [weightFile]
!     The interpolation weight file name.
!   \item [{[regridMethod]}]
!     The value is one of {\tt ESMF\_REGRIDMETHOD\_BILINEAR}, {\tt ESMF\_REGRIDMETHOD\_PATCH}, or
!     {\tt ESMF\_REGRIDMETHOD\_CONSERVE}.  The default value is {\tt ESMF\_REGRIDMETHOD\_BILINEAR}.
!   \item [{[poleMethod]}]
!     A flag to indicate what to do with the pole.  The value is one of {\tt ESMF\_POLEMETHOD\_ALLAVG},
!     {\tt ESMF\_POLEMETHOD\_NONE}, {\tt ESMF\_POLEMETHOD\_NPNTAVG} or {\tt ESMF\_POLEMETHOD\_TEETH}.
!     The default value varies depending on the regridding method and the grid type and foramt.  
!   \item [{[poleNPnts]}]
!     If {\tt poleMethod} is set to {\tt ESMF_POLEMETHOD\_NPNTAVG}, this argument is required to 
!     specify how many points are used to represent the pole.
!   \item [{[ignoreUnmappedFlag]}]
!     If .TRUE., the unmapped destination points will be ignored.  If not 
!     specified, the default is to stop the regrid with an error.
!   \item [{[srcFileType]}]
!     The file format of the source grid.  The value is one of
!     {\tt ESMF\_FILEFORMAT\_SCRIP}, {\tt ESMF\_FILEFORMAT\_ESMFMESH}, {\tt ESMF\_FILEFORMAT\_UGRID},
!     or {\tt ESMF\_FILEFORMAT\_GRIDSPEC}.
!   \item [{[dstFileType]}]
!     The file format of the destination grid.  The value is one of
!     {\tt ESMF\_FILEFORMAT\_SCRIP}, {\tt ESMF\_FILEFORMAT\_ESMFMESH}, {\tt ESMF\_FILEFORMAT\_UGRID},
!     or {\tt ESMF\_FILEFORMAT\_GRIDSPEC}.
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
!   \item [{[srcMissingvalueVar]}]
!     If {\tt srcMissingValueFlag} is .TRUE., the argument is required to define
!     the variable name whose missing values will be used to construct the grid 
!     mask.  It is only used for the grid defined in  the GRIDSPEC or the UGRID 
!     file formats.
!   \item [{[dstMissingValueFlag]}]
!     If .TRUE., the destination grid mask will be constructed using the missing
!     values of the variable defined in {\tt dstMissingValueVar}. This flag is
!     only used for the grid defined in  the GRIDSPEC or the UGRID file formats.
!     The default value is .FALSE..
!   \item [{[dstMissingvalueVar]}]
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
!   \item [{[useUserAreaFlag]}]
!     If .TRUE., the element area values defined in the grid files are used.
!     Only the SCRIP and ESMF format grid files have user specified areas. This flag
!     is only used for conservative regridding. The default is .FALSE. 
!   \item [{[largefileFlag]}]
!     If .TRUE., the output weight file is in NetCDF 64bit offset format. 
!     The default is .FALSE.
!   \item [{[verboseFlag]}]
!     If .TRUE., it will print summary information about the regrid parameters,
!     default to .FALSE.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

      
      type(ESMF_RegridMethod_Flag) :: localRegridMethod
      type(ESMF_PoleMethod_Flag)   :: localPoleMethod
      type(ESMF_FileFormat_Flag)   :: localSrcFileType
      type(ESMF_FileFormat_Flag)   :: localDstFileType
      integer            :: localPoleNPnts
      logical            :: localUserAreaFlag
      logical            :: localLargefileFlag
      logical            :: localVerboseFlag
      integer            :: localrc
      type(ESMF_VM)      :: vm
      integer            :: PetNo, PetCnt
      type(ESMF_Mesh)    :: srcMesh, dstMesh
      type(ESMF_Grid)    :: srcGrid, dstGrid
      type(ESMF_Field)   :: srcField, dstField
      type(ESMF_Field)   :: srcFracField, dstFracField
      type(ESMF_ArraySpec) :: arrayspec
      integer(ESMF_KIND_I4), pointer:: factorIndexList(:,:)
      real(ESMF_KIND_R8), pointer :: factorList(:)
      integer(ESMF_KIND_I4) :: maskvals(1)
      integer            :: ind
      integer, pointer   :: srcdims(:), dstdims(:)
      integer            :: srcrank, dstrank
      logical            :: convert3D
      logical            :: isConserve, srcIsSphere, dstIsSphere
      logical            :: addCorners,convertToDual
      type(ESMF_MeshLoc) :: meshloc
      logical            :: srcIsReg, dstIsReg
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
      logical 		 :: ignoreUnmapped
      type(ESMF_UnmappedAction_Flag) :: unmappedaction
      logical            :: srcMissingValue, dstMissingValue
      character(len=256) :: argStr
      logical            :: useSrcCoordVar, useDstCoordVar
      logical            :: useSrcMask, useDstMask
      !real(ESMF_KIND_R8) :: starttime, endtime
     
      !------------------------------------------------------------------------
      ! get global vm information
      !
      call ESMF_VMGetGlobal(vm, rc=localrc)
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
      localSrcFileType = ESMF_FILEFORMAT_SCRIP
      localDstFileType = ESMF_FILEFORMAT_SCRIP
      localVerboseFlag = .false.
      srcIsRegional = .false.
      dstIsRegional = .false.
      srcMissingValue = .false.
      dstMissingValue = .false.
      ignoreUnmapped= .false.
      localLargeFileFlag = .false.
      localUserAreaflag = .false.
      useSrcCoordVar = .false.
      useDstCoordVar = .false.
      localPoleNPnts = 0

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
        if (present(poleNPnts)) then
  	  localPoleNPnts = poleNPnts
        else 
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
	    msg ="poleNPnts argument is missing for ESMF_POLEMETHOD_NPNTAVG", &
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

      if (present(srcFileType)) then
         localSrcFileType = srcFileType
      endif

      if (present(dstFileType)) then
         localDstFileType = dstFileType
      endif

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

      if (srcMissingValue .and. localSrcFileType == ESMF_FILEFORMAT_UGRID .and. &
          localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
	    msg = " missingvalue is only supported on the mesh elements", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      endif

      if (dstMissingValue .and. (localDstFileType == ESMF_FILEFORMAT_SCRIP .or. &
          localDstFileType == ESMF_FILEFORMAT_ESMFMESH)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
	    msg = " missingvalue is only supported for UGRID and GRIDSPEC", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      endif

      if (dstMissingValue .and. localDstFileType == ESMF_FILEFORMAT_UGRID .and. &
          localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
	    msg = " missingvalue is only supported on the mesh elements", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      endif

      if (present(ignoreUnmappedFlag)) then
         ignoreUnmapped = ignoreUnmappedFlag
      endif

      if (present(srcRegionalFlag)) then
         srcIsRegional = srcRegionalFlag
      endif

      if (present(dstRegionalFlag)) then
         dstIsRegional = dstRegionalFlag
      endif

!      if (srcIsRegional .or. dstIsRegional) then
      if (srcIsRegional) then
         localPoleMethod = ESMF_POLEMETHOD_NONE
         localPoleNPnts = 0
      endif

      if (present(largefileFlag)) then
	   localLargeFileFlag = largefileFlag
      endif

      if (present(useUserAreaFlag)) then
	   localUserAreaFlag = useUserAreaFlag
      endif

      if (present(verboseFlag)) then
           localVerboseFlag = verboseFlag
      endif

      ! user area only needed for conservative regridding
      if (localUserAreaFlag .and. (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE)) then
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
      if (srcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
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

      if (dstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
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


      ! Only set useSrcMask to false if srcMissingvalue is not given and the file type is
      ! either GRIDSPEC or UGRID, same for useDstMask
      if ((.not. srcMissingvalue) .and. (localSrcFileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
	    localSrcFileType == ESMF_FILEFORMAT_UGRID)) useSrcMask = .false.

      if ((.not. dstMissingvalue) .and. (dstFileType == ESMF_FILEFORMAT_GRIDSPEC .or. &
	    dstFileType == ESMF_FILEFORMAT_UGRID)) useDstMask = .false.
 
      ! Should I have only PetNO=0 to open the file and find out the size?
!      if (PetNo == 0) then
      if (localSrcFileType == ESMF_FILEFORMAT_SCRIP) then
	   call ESMF_ScripInq(srcfile, grid_rank= srcrank, grid_dims=srcdims, rc=localrc)
#if 1
	   if (localrc /= ESMF_SUCCESS) then 
             write(*,*)
	     print *, 'ERROR: Unable to get dimension information from:', srcfile, &
                      'Please check the PET*.RegridWeightGen.Log files for the NetCDF error message.' 
           endif
#endif
           if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
           if (srcrank == 2) then
	     srcIsReg = .true.
           else
             srcIsReg = .false.
           endif
      elseif (localSrcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
           allocate(srcdims(2))
	   if (useSrcCoordVar) then
   	      call ESMF_GridspecInq(srcfile, srcrank, srcdims, coord_names=srcCoordinateVars, rc=localrc)
	   else
	      call ESMF_GridspecInq(srcfile, srcrank, srcdims, rc=localrc)
 	   endif
#if 1
	   if (localrc /= ESMF_SUCCESS) then 
             write(*,*)
	     print *, 'ERROR: Unable to get dimension information from:', srcfile, &
                      'Please check the PET*.RegridWeightGen.Log files for the NetCDF error message.' 
           endif
#endif
           if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
	   srcIsReg = .true.
           srcrank = 2
      else
	   srcIsReg = .false.
      endif
      if (dstFileType == ESMF_FILEFORMAT_SCRIP) then
	   call ESMF_ScripInq(dstfile, grid_rank=dstrank, grid_dims=dstdims, rc=localrc)
#if 1
           if (localrc /= ESMF_SUCCESS) then
             write(*,*)
	     print *, 'ERROR: Unable to get dimension information from:', dstfile, &
                      'Please check the PET*.RegridWeightGen.Log files for the NetCDF error message' 
           endif
#endif
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
           if (dstrank == 2) then
	     dstIsReg = .true.
           else
             dstIsReg = .false.
           endif
       elseif (localDstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
	   allocate(dstdims(2))
	   if (useDstCoordVar) then
	      call ESMF_GridspecInq(dstfile, dstrank, dstdims, coord_names=dstCoordinateVars, rc=localrc)
	   else
	      call ESMF_GridspecInq(dstfile, dstrank, dstdims, rc=localrc)
	   endif 
#if 1
	   if (rc /= ESMF_SUCCESS) then 
             write(*,*)
	     print *, 'ERROR: Unable to get dimension information from:', dstfile, &
                      'Please check the PET*.RegridWeightGen.Log files for the NetCDF error message.' 
           endif
#endif	
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
	   dstrank = 2
	   dstIsReg = .true.
      else
	   dstIsReg = .false.
      endif
        
      ! Print the regrid options
      if (localVerboseFlag .and. PetNo == 0) then
  	  print *, "Starting weight generation with these inputs: "
	  print *, "  Source File: ", trim(srcfile)
	  print *, "  Destination File: ", trim(dstfile)
  	  print *, "  Weight File: ", trim(weightFile)
          if (localSrcFileType == ESMF_FILEFORMAT_SCRIP) then 
             print *, "  Source File is in SCRIP format"
          elseif (localSrcFileType == ESMF_FILEFORMAT_ESMFMESH) then 
             print *, "  Source File is in ESMF format"
          elseif (localSrcFileType == ESMF_FILEFORMAT_UGRID) then
             print *, "  Source File is in UGRID format, dummy variable: ", &
		trim(srcMeshName)
	     if (srcMissingValue) then
	        print *, "    Use attribute 'missing_value' of variable '", trim(srcMissingvalueVar),"' as the mask"
	     endif
	  else 
	     print *, "  Source File is in GRIDSPEC foramt"
	     if (useSrcCoordVar) then
	        print *, "    Use '", trim(srcCoordinateVars(1)), "' and '", trim(srcCoordinateVars(2)), &
		         "' as the longitude and latitude variables"
	     endif
	     if (srcMissingValue) then
	        print *, "    Use the missing values of variable '", trim(srcMissingvalueVar),"' as the mask"
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
          if (localDstFileType == ESMF_FILEFORMAT_SCRIP) then 
              print *, "  Destination File is in SCRIP format"
          elseif (localDstFileType == ESMF_FILEFORMAT_ESMFMESH) then 
              print *, "  Destination File is in ESMF format"
          elseif (localDstFileType == ESMF_FILEFORMAT_UGRID) then
              print *, "  Destination File is in UGRID format, dummy variable: ", & 
	  	trim(dstMeshName)
	      if (dstMissingValue) then
	        print *, "    Use the missing value of '", trim(dstMissingvalueVar),"' as the mask"
              endif	
          else
	      print *, "  Destination File is in GRIDSPEC format"	
	      if (useDstCoordVar) then
	        print *, "    Use '", trim(dstCoordinateVars(1)), "' and '", trim(dstCoordinateVars(2)), &
		         "' as the longitude and latitude variables"
	      endif
	      if (dstMissingValue) then
	        print *, "    Use the missing value of '", trim(dstMissingvalueVar),"' as the mask"
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
          if (localRegridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
             print *, "  Regrid Method: bilinear"
          elseif (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
             print *, "  Regrid Method: conserve"
          elseif (localRegridMethod == ESMF_REGRIDMETHOD_PATCH) then
             print *, "  Regrid Method: patch"
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
          if (ignoreUnmapped) then
	     print *, "  Ignore unmapped destination points"
          endif
	  if (localLargeFileFlag) then
	     print *, "  Output weight file in 64bit offset NetCDF file format"
          endif
	  if (localUserAreaFlag) then
	     print *, "  Use user defined cell area for both the source and destination grids"
          endif
          write(*,*)
     endif 

     ! Set flags according to the regrid method
     if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
        isConserve=.true.
        addCorners=.true.
        convertToDual=.false.
        meshloc=ESMF_MESHLOC_ELEMENT
     else
        isConserve=.false.
        addCorners=.false.
        convertToDual=.true.
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

     ! Set unmapped flag
     if (ignoreUnmapped) then
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE
     else
        unmappedaction=ESMF_UNMAPPEDACTION_ERROR
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

     if (localSrcFileType == ESMF_FILEFORMAT_GRIDSPEC) then
	if (useSrcCoordVar) then
 	  if (srcMissingValue) then
	     srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), &
		  addCornerStagger=addCorners, &
		  addMask=.true., varname=trim(srcMissingvalueVar), isSphere=srcIsSphere, &
		  coordNames = srcCoordinateVars, rc=localrc)
          else
             srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), & 
		       addCornerStagger=addCorners, &
                       isSphere=srcIsSphere, coordNames = srcCoordinateVars,rc=localrc)
	  endif
        else 
 	  if (srcMissingValue) then
	     srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), &
		  addCornerStagger=addCorners, &
		  addMask=.true., varname=trim(srcMissingvalueVar), isSphere=srcIsSphere, rc=localrc)
          else
             srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), & 
		       addCornerStagger=addCorners, &
                       isSphere=srcIsSphere, rc=localrc)
	  endif
	endif
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
     elseif (localSrcFileType == ESMF_FILEFORMAT_SCRIP) then
	if(srcIsReg) then
           srcGrid = ESMF_GridCreate(srcfile, localSrcFileType, (/xpart,ypart/), &
			addCornerStagger=addCorners, &
                        isSphere=srcIsSphere, addUserArea =localUserAreaFlag, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

          ! call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CORNER, filename="srcGrid", isSphere=.false., &
          !       isLatLonDeg=.true., rc=localrc)
          ! if (rc /= ESMF_SUCCESS) call ErrorMsgAndAbort(PetNo)

	   call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
    	   srcField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
	else
           srcMesh = ESMF_MeshCreate(srcfile, ESMF_FILEFORMAT_SCRIP, convert3D=.true., &
                       convertToDual=convertToDual, addUserArea=localUserAreaFlag, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
           ! call ESMF_MeshWrite(srcMesh, "srcMesh", rc)
           call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
           srcField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=meshloc,rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
	endif
      else
	! if srcfile is not SCRIP, it is always unstructured
	if (srcMissingValue) then
	   srcMesh = ESMF_MeshCreate(srcfile, localSrcFileType, convert3D=.true., &
                    meshname = trim(srcMeshName), addMask=.true., &
		    varname=trim(srcMissingvalueVar), rc=localrc)
	else
	   srcMesh = ESMF_MeshCreate(srcfile, localSrcFileType, convert3D=.true., &
                    meshname = trim(srcMeshName), rc=localrc)
	endif
        if (ESMF_LogFoundError(localrc, &
                               ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
        srcField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=meshloc,rc=localrc)
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
     if (localDstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
	if (useDstCoordVar) then
 	  if (dstMissingValue) then
	     dstGrid = ESMF_GridCreate(dstfile, localDstFileType, (/xpart,ypart/), &
		  addCornerStagger=addCorners, &
		  addMask=.true., varname=trim(dstMissingvalueVar), isSphere=dstIsSphere, &
		  coordNames = dstCoordinateVars, rc=localrc)
          else
             dstGrid = ESMF_GridCreate(dstfile, localDstFileType, (/xpart,ypart/), &
			addCornerStagger=addCorners, &
                        isSphere=dstIsSphere, coordNames=dstCoordinateVars, rc=localrc)
	  endif
        else
 	  if (dstMissingValue) then
	     dstGrid = ESMF_GridCreate(dstfile, localDstFileType, (/xpart,ypart/), &
		  addCornerStagger=addCorners, &
		  addMask=.true., varname=trim(dstMissingvalueVar), isSphere=dstIsSphere, rc=localrc)
          else
             dstGrid = ESMF_GridCreate(dstfile, localDstFileType, (/xpart,ypart/), &
			addCornerStagger=addCorners, &
                        isSphere=dstIsSphere, rc=localrc)
	  endif
	endif
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
     elseif (localDstFileType == ESMF_FILEFORMAT_SCRIP) then
	if(dstIsReg) then
           dstGrid = ESMF_GridCreate(dstfile, localDstFileType,(/xpart, ypart/), &
			addCornerStagger=addCorners, &
                        isSphere=dstIsSphere, addUserArea = localUserAreaFlag, rc=localrc)
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
           dstMesh = ESMF_MeshCreate(dstfile, ESMF_FILEFORMAT_SCRIP, convert3D=.true., &
                       convertToDual=convertToDual, addUserArea=localUserAreaFlag, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
           call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
           dstField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=meshloc,rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
	endif
      else
	! if dstfile is not SCRIP, it is always unstructured
	if (dstMissingValue) then
 	   dstMesh = ESMF_MeshCreate(dstfile, localDstFileType, convert3D=.true., &
                    meshname = trim(dstMeshName), addMask=.true., &
		    varname=trim(dstMissingvalueVar), rc=localrc)
	else
	   dstMesh = ESMF_MeshCreate(dstfile, localDstFileType, convert3D=.true., &
                    meshname = trim(dstMeshName), rc=localrc)
        endif
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
        dstField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=meshloc,rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! Create Frac Fields if conservative
      if (isConserve) then
         if (srcIsReg) then
            call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
            srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
         else
            call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
            srcFracField=ESMF_FieldCreate(srcMesh,arrayspec,meshloc=meshloc,rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
         endif

         if (dstIsReg) then
            call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
            dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
         else
            call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
            dstFracField=ESMF_FieldCreate(dstMesh,arrayspec,meshloc=meshloc,rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
         endif
      endif

      !call ESMF_VMBarrier(vm)
      !call ESMF_VMWtime(starttime, rc=localrc)
      maskvals(1) = 0
      if (localPoleNPnts <= 0) localPoleNPnts = 1

      if (localRegridMethod == ESMF_REGRIDMETHOD_BILINEAR) then
            methodStr = "Bilinear remapping"
      else if (localRegridMethod == ESMF_REGRIDMETHOD_PATCH) then
            methodStr = "Bilinear remapping" ! SCRIP doesn't recognize Patch
      else if (localRegridMethod == ESMF_REGRIDMETHOD_CONSERVE) then
            methodStr = "Conservative remapping"
     else ! nothing recognizable so report error
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg="unrecognized RegridMethod", &
            ESMF_CONTEXT, rcToReturn=rc)
      endif

      if (useSrcMask .and. useDstMask) then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, dstMaskValues = maskvals, &
	    unmappedaction=unmappedaction, &
	    factorIndexList=factorIndexList, factorList=factorList, &
            srcFracField=srcFracField, dstFracField=dstFracField, &
            regridmethod = localRegridMethod, &
            polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
	    rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
       else if (useSrcMask) then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    srcMaskValues = maskvals, &
	    unmappedaction=unmappedaction, &
	    factorIndexList=factorIndexList, factorList=factorList, &
            srcFracField=srcFracField, dstFracField=dstFracField, &
            regridmethod = localRegridMethod, &
            polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
	    rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
       else if (useDstMask) then
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    dstMaskValues = maskvals, &
	    unmappedaction=unmappedaction, &
	    factorIndexList=factorIndexList, factorList=factorList, &
            srcFracField=srcFracField, dstFracField=dstFracField, &
            regridmethod = localRegridMethod, &
            polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
	    rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
       else	
          call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, & 
	    unmappedaction=unmappedaction, &
	    factorIndexList=factorIndexList, factorList=factorList, &
            srcFracField=srcFracField, dstFracField=dstFracField, &
            regridmethod = localRegridMethod, &
            polemethod = localPoleMethod, regridPoleNPnts = localPoleNPnts, &
	    rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
       end if
      ! print *, PetNo, size(factorList), factorIndexList(1,1), factorIndexList(2,1)
      ! Compute areas if conservative
      ! Area only valid on PET 0 right now, when parallel Array
      ! write works, then make area io parallel
      if (isConserve) then
         if (srcIsReg) then
            call computeAreaGrid(srcGrid, PetNo, srcArea, regridScheme, rc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
          else
            call computeAreaMesh(srcMesh, vm, petNo, petCnt, srcArea, rc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_MeshMergeSplitSrcInd(srcMesh,factorIndexList,rc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
         endif

         if (dstIsReg) then
            call computeAreaGrid(dstGrid, PetNo, dstArea, regridScheme, rc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
          else
            call computeAreaMesh(dstMesh, vm, petNo, petCnt, dstArea, rc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_MeshMergeSplitDstInd(dstMesh,factorList,factorIndexList,rc)
            if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
         endif
      endif

      ! Compact weight matrix
      ! (only compact if one of the grids is irregular, because that's when the repeated entries occur)
      if ((.not. srcIsReg) .or. (.not. dstIsReg)) then
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
      if (localRegridMethod /= ESMF_REGRIDMETHOD_CONSERVE) then
	if (dstIsReg) then
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
         if (srcIsReg) then
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

         if (dstIsReg) then
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

      !! Write the weight table into a SCRIP format NetCDF file
      if (PetNo == 0) then
         if (isConserve) then
	   if (useSrcCoordVar .and. useDstCoordVar) then
              call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
	           dstFileType=localDstFileType, method = localRegridMethod, &
                   srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
		   dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
	 	   srccoordnames = srcCoordinateVars, dstcoordnames = dstCoordinateVars, rc=localrc)
    	   else if (useSrcCoordVar) then	
              call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
	           dstFileType=localDstFileType, method = localRegridMethod, &
                   srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
		   dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
	 	   srccoordnames = srcCoordinateVars, rc=localrc)
	   elseif (useDstCoordVar) then
              call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
	           dstFileType=localDstFileType, method = localRegridMethod, &
                   srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
		   dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
	 	   dstcoordnames = dstCoordinateVars, rc=localrc)
	    else
              call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
	           dstFileType=localDstFileType, method = localRegridMethod, &
                   srcArea=srcArea, dstArea=dstArea, srcFrac=srcFrac, &
		   dstFrac=dstFrac, largeFileFlag=localLargeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, rc=localrc)
	   endif
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
         else
	   if (useSrcCoordVar .and. useDstCoordVar) then
               call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
	           dstFileType=localDstFileType, method = localRegridMethod, dstFrac=dstFrac, &
		   largeFileFlag=localLargeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
 	 	   srccoordnames = srcCoordinateVars, dstcoordnames = dstCoordinateVars, rc=localrc)
	   elseif (useSrcCoordVar) then
               call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
	           dstFileType=localDstFileType, method = localRegridMethod, dstFrac=dstFrac, &
		   largeFileFlag=localLargeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
 	 	   srccoordnames = srcCoordinateVars, rc=localrc)
	   elseif (useDstCoordVar) then
               call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
	           dstFileType=localDstFileType, method = localRegridMethod, dstFrac=dstFrac, &
		   largeFileFlag=localLargeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, &
 	 	   dstcoordnames = dstCoordinateVars, rc=localrc)
	   else
               call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList,  &
	           srcFile=srcfile, dstFile=dstfile, srcFileType=localSrcFileType,&
	           dstFileType=localDstFileType, method = localRegridMethod, dstFrac=dstFrac, &
		   largeFileFlag=localLargeFileFlag, &
		   srcmeshname = srcMeshName, dstmeshname = dstMeshName, &
		   srcMissingValue = srcMissingValue, dstMissingValue=dstMissingValue, &
	           srcvarname = srcMissingvalueVar, dstvarname=dstMissingvalueVar, rc=localrc)
           endif
           if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
	 endif
      else 
	 call ESMF_OutputScripWeightFile(weightFile, factorList, factorIndexList, rc=localrc)
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
      
      rc = ESMF_SUCCESS
      return 
end subroutine ESMF_RegridWeightGen

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
  type(ESMF_ArraySpec) ::arrayspec
  integer :: minIndex(2), maxIndex(2), gridDims(2)
  real (ESMF_KIND_R8), pointer :: area2D(:,:)
  integer :: localrc

  ! Setup Arrayspec
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! Create a field on the grid to hold the areas
  areaField=ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, name="area", rc=localrc)
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

     ! Get area onto PET 0
     call ESMF_FieldGather(areaField, farray=area2D, rootPet=0, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
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

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeFracMesh"

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

!------------------------------------------------------------------------------

end module ESMF_RegridWeightGenMod

