! $Id: ESMF_FieldRegrid.F90,v 1.96 2012/02/06 21:22:44 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FieldRegrid.F90"
!==============================================================================
!
!     ESMF FieldRegrid module
module ESMF_FieldRegridMod
!
!==============================================================================
!
! This file contains the FieldRegrid methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_VMMod
  use ESMF_LogErrMod
  use ESMF_ArrayMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_GridUtilMod
  use ESMF_StaggerLocMod
  use ESMF_MeshMod
  use ESMF_RHandleMod
  use ESMF_GeomBaseMod
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_FieldGetMod
  use ESMF_XGridMod
  use ESMF_XGridGetMod

  
  implicit none
  private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_FieldRegridStore        ! Store a regrid matrix
   public ESMF_FieldRegrid             ! apply a regrid operator
   public ESMF_FieldRegridRelease      ! apply a regrid operator
   public ESMF_FieldRegridGetIwts      ! get integration weights
   public ESMF_FieldRegridGetArea      ! get area
   private checkGrid                   ! small subroutine to check the grid

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!BOPI
! !IROUTINE: ESMF_FieldRegridStore -- Generic interface

! !INTERFACE:
  interface ESMF_FieldRegridStore

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_FieldRegridStoreNX
    module procedure ESMF_FieldRegridStoreX
!EOPI

  end interface


!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_FieldRegrid.F90,v 1.96 2012/02/06 21:22:44 oehmke Exp $'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegrid"

!BOP
! !IROUTINE: ESMF_FieldRegrid - Compute a regridding operation
!
! !INTERFACE:
      subroutine ESMF_FieldRegrid(srcField, dstField, &
                   routehandle, keywordEnforcer, zeroregion, checkflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Field),       intent(in),    optional :: srcField
      type(ESMF_Field),       intent(inout), optional :: dstField
      type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Region_Flag), intent(in),    optional :: zeroregion
      logical,                intent(in),    optional :: checkflag
      integer,                intent(out),   optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Execute the precomputed regrid operation stored in {\tt routehandle} to 
!   interpolate from {\tt srcField} to {\tt dstField}.  See {\tt ESMF\_FieldRegridStore()} on how to 
!   precompute the {\tt routehandle}. 
!  
!   \begin{sloppypar}
!   Both {\tt srcField} and {\tt dstField} must be
!   weakly congruent with the respective Fields used during 
!   {\tt ESMF\_FieldRegridStore()}. Congruent Fields possess matching DistGrids and the shape of the 
!   local array tiles matches between the Fields for every DE. For weakly congruent Fields the sizes 
!   of the undistributed dimensions, that vary faster with memory than the first distributed dimension,
!   are permitted to be different. This means that the same routehandle can be applied to a large class 
!   of similar Fields that differ in the number of elements in the left most undistributed dimensions.
!   You can apply the routehandle between any set of Fields weakly congruent to the original Fields used
!   to create the routehandle without incurring an error. However, if you want                                     
!   the routehandle to be the same interpolation between the grid objects upon which the Fields are build as was calculated
!   with the original {\tt ESMF\_FieldRegridStore()} call, then there
!   are additional constraints on the grid objects. To be the same interpolation, the grid objects upon which the 
!   Fields are build must contain the same coordinates at the stagger locations involved in the regridding as 
!   the original source and destination Fields used in the {\tt ESMF\_FieldRegridStore()} call.  
!   The routehandle represents the interpolation between the grid objects as they were during the {\tt ESMF\_FieldRegridStore()} call.  
!   So if the coordinates at the stagger location in the grid objects change, a new call to {\tt ESMF\_FieldRegridStore()} 
!   is necessary to compute the interpolation between that new set of coordinates.
!   \end{sloppypar}
!
!   The {\tt srcField} and {\tt dstField} arguments are optional in support of
!   the situation where {\tt srcField} and/or {\tt dstField} are not defined on
!   all PETs. The {\tt srcField} and {\tt dstField} must be specified on those
!   PETs that hold source or destination DEs, respectively, but may be omitted
!   on all other PETs. PETs that hold neither source nor destination DEs may
!   omit both arguments.
!
!   It is erroneous to specify the identical Field object for {\tt srcField} and
!   {\tt dstField} arguments.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [{[srcField]}]
!     {\tt ESMF\_Field} with source data.
!   \item [{[dstField]}]
!     {\tt ESMF\_Field} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[zeroregion]}]
!     \begin{sloppypar}
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstField} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstField} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroregion} to 
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination Array that will be updated by the sparse matrix
!     multiplication. See section \ref{const:region} for a complete list of
!     valid settings.
!     \end{sloppypar}
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOP
        integer :: localrc
        type(ESMF_Array)     :: srcArray
        type(ESMF_Array)     :: dstArray

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Now we go through the painful process of extracting the data members
        ! that we need, if present.
        if (present(srcField)) then
          call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (present(dstField)) then
          call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (present(srcField) .and. present(dstField)) then
          call ESMF_ArraySMM(srcArray=srcArray, dstArray=dstArray, &
                 routehandle=routehandle, zeroregion=zeroregion, &
                 checkflag=checkflag, rc=localrc)
		else if (present(srcField) .and. .not. present(dstField)) then
          call ESMF_ArraySMM(srcArray=srcArray, &
                 routehandle=routehandle, zeroregion=zeroregion, &
                 checkflag=checkflag, rc=localrc)
		else if (.not. present(srcField) .and. present(dstField)) then
          call ESMF_ArraySMM(dstArray=dstArray, &
                 routehandle=routehandle, zeroregion=zeroregion, &
                 checkflag=checkflag, rc=localrc)
        else if (.not. present(srcField) .and. .not. present(dstField)) then
          call ESMF_ArraySMM(routehandle=routehandle, zeroregion=zeroregion, &
                 checkflag=checkflag, rc=localrc)
        else
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg="Supplied combination of optional Fields not supported", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

        if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

        ! Once the compilation order is sorted out, 
        ! Should be able to call into Field SMM directly.
        !call ESMF_FieldSMM(srcField=srcField, dstField=dstField, &
        !           routehandle=routehandle, zeroregion=zeroregion, &
        !           checkflag=checkflag, rc=localrc)

        !if (ESMF_LogFoundError(localrc, &
        !                             ESMF_ERR_PASSTHRU, &
        !                             ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegrid
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridRelease"

!BOP
! !IROUTINE: ESMF_FieldRegridRelease - Free resources used by a regridding operation
!
! !INTERFACE:
      subroutine ESMF_FieldRegridRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(out), optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Free resources used by regrid objec
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle]
!           Handle carrying the sparse matrix
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc

        call ESMF_RouteHandleRelease(routehandle=routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridRelease
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridStoreNX"

!BOP
! !IROUTINE: ESMF_FieldRegridStore - Precompute a Field regridding operation and return a RouteHandle and weights
!
! !INTERFACE:
  !   Private name; call using ESMF_FieldRegridStore()
      subroutine ESMF_FieldRegridStoreNX(srcField, dstField, keywordEnforcer, &
                                       srcMaskValues, dstMaskValues, &
                                       regridmethod, &
                                       polemethod, regridPoleNPnts, & 
                                       unmappedaction, &
                                       routehandle, &
                                       factorList, factorIndexList, & 
                                       weights, indices, &  ! DEPRECATED ARGUMENTS
                                       srcFracField, dstFracField, rc)
!      
! !ARGUMENTS:
      type(ESMF_Field),            intent(in)             :: srcField
      type(ESMF_Field),            intent(inout)          :: dstField
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),       intent(in),   optional :: srcMaskValues(:)
      integer(ESMF_KIND_I4),       intent(in),   optional :: dstMaskValues(:)
      type(ESMF_RegridMethod_Flag),intent(in),   optional :: regridmethod
      type(ESMF_PoleMethod_Flag),  intent(in),   optional :: polemethod
      integer,                     intent(in),   optional :: regridPoleNPnts
      type(ESMF_UnmappedAction_Flag),intent(in), optional :: unmappedaction
      type(ESMF_RouteHandle),      intent(inout),optional :: routehandle
      real(ESMF_KIND_R8),          pointer,      optional :: factorList(:)
      integer(ESMF_KIND_I4),       pointer,      optional :: factorIndexList(:,:)
      real(ESMF_KIND_R8),          pointer,      optional :: weights(:)   ! DEPRECATED ARGUMENT
      integer(ESMF_KIND_I4),       pointer,      optional :: indices(:,:) ! DEPRECATED ARGUMENT 
      type(ESMF_Field),            intent(inout),optional :: srcFracField
      type(ESMF_Field),            intent(inout),optional :: dstFracField
      integer,                     intent(out),  optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.2.0rp1] Added arguments {\tt factorList} and {\tt factorIndexList}.
!                 Started to deprecate arguments {\tt weights} and {\tt indices}.
!                 This corrects an inconsistency of this interface with all 
!                 other ESMF methods that take these same arguments.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!       \begin{sloppypar}
!       Creates a sparse matrix operation (stored in {\tt routehandle}) that 
!       contains the calculations and communications necessary to interpolate
!       from {\tt srcField} to {\tt dstField}. The routehandle can then be 
!       used in the call {\tt ESMF\_FieldRegrid()} to interpolate between the
!       Fields. The user may also get the interpolation matrix in sparse 
!       matrix form via the optional arguments {\tt factorList} and {\tt factorIndexList}. 
!       \end{sloppypar}
!       
!       The routehandle generated by this call is based just on the 
!       coordinates in the Grids contained in the Fields.  If those
!       coordinates don't change the routehandle can
!       be used repeatedly to interpolate from the source Field to the 
!       destination Field.  This is true even if the data in the Fields 
!       changes. The routehandle may also be used to interpolate between any
!       source and destination Field which are created on the same stagger 
!       location and Grid as the original Fields.        
!
!       When it's no longer needed the routehandle should be destroyed by 
!       using {\tt ESMF\_FieldRegridRelease()} to free the memory it's using. 
!       Note {\tt ESMF\_FieldRegridStore()} assumes the coordinates used in 
!       the Grids upon which the Fields are built are in degrees.  
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           Source Field.
!     \item [{[srcMaskValues]}]
!           List of values that indicate a source point should be masked out. 
!           If not specified, no masking will occur. 
!     \item [dstField]
!           Destination Field.
!     \item [{[dstMaskValues]}]
!           List of values that indicate a destination point should be masked out. 
!           If not specified, no masking will occur.
!     \item [{[unmappedaction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Options are 
!           {\tt ESMF\_UNMAPPEDACTION\_ERROR} or 
!           {\tt ESMF\_UNMAPPEDACTION\_IGNORE}. If not specified, defaults 
!           to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!     \item [{[routehandle]}]
!           The communication handle that implements the regrid operation and that can be used later in 
!           the {\tt ESMF\_FieldRegrid()} call. The {\tt routehandle} is optional so that if the 
!           user doesn't need it, then they can indicate that by not requesting it. 
!           The time to compute the {\tt routehandle} can be a significant fraction of the time 
!           taken by this method, so if it's not needed then not requesting it is worthwhile.  
!     \item [{[factorList]}] 
!           The list of coefficients for a sparse matrix which interpolates from {\tt srcField} to 
!           {\tt dstField}. The array coming out of this variable is in the appropriate format to be used
!           in other ESMF sparse matrix multiply calls, for example {\tt ESMF\_FieldSMMStore()}. 
!           The {\tt factorList} array is allocated by the method and the user is responsible for 
!           deallocating it. 
!     \item [{[factorIndexList]}] 
!           The indices for a sparse matrix which interpolates from {\tt srcField} to 
!           {\tt dstField}. This argument is a 2D array containing pairs of source and destination
!           sequence indices corresponding to the coefficients in the {\tt factorList} argument. 
!           The first dimension of {\tt factorIndexList} is of size 2. {\tt factorIndexList(1,:)} specifes 
!           the sequence index of the source element in the {\tt srcField}. {\tt factorIndexList(2,:)} specifes 
!           the sequence index of the destination element in the {\tt dstField}. The second dimension of 
!           {\tt factorIndexList} steps through the list of pairs, i.e. {\tt size(factorIndexList,2)==size(factorList)}.
!           The array coming out of this variable is in the appropriate format to be used
!           in other ESMF sparse matrix multiply calls, for example {\tt ESMF\_FieldSMMStore()}. 
!           The {\tt factorIndexList} array is allocated by the method and the user is responsible for deallocating it. 
!     \item [{[weights]}] 
!           \apiDeprecatedArgWithReplacement{factorList}
!     \item [{[indices]}] 
!           \apiDeprecatedArgWithReplacement{factorIndexList}
!     \item [{[srcFracField]}] 
!           The fraction of each source cell participating in the regridding. Only 
!           valid when regridmethod is {\tt ESMF\_REGRIDMETHOD\_CONSERVE}.
!           This Field needs to be created on the same location (e.g staggerloc) 
!           as the srcField.
!     \item [{[dstFracField]}] 
!           The fraction of each destination cell participating in the regridding. Only 
!           valid when regridmethod is {\tt ESMF\_REGRIDMETHOD\_CONSERVE}.
!           This Field needs to be created on the same location (e.g staggerloc) 
!           as the dstField.
!     \item [{[regridmethod]}]
!           The type of interpolation. Please see Section~\ref{opt:regridmethod} 
!           for a list of valid options. If not specified, defaults to 
!           {\tt ESMF\_REGRIDMETHOD\_BILINEAR}.
!     \item [{[polemethod]}]
!           Which type of artificial pole
!           to construct on the source Grid for regridding. Please see 
!           Section~\ref{const:polemethod} for a list of
!           valid options. If not specified, defaults to {\tt ESMF\_POLEMETHOD\_ALLAVG}. 
!     \item [{[regridPoleNPnts]}]
!           If {\tt polemethod} is {\tt ESMF\_POLEMETHOD\_NPNTAVG}.
!           This parameter indicates how many points should be averaged
!           over. Must be specified if {\tt polemethod} is 
!           {\tt ESMF\_POLEMETHOD\_NPNTAVG}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc
        integer              :: lregridScheme
        type(ESMF_RegridMethod_Flag) :: lregridmethod
        type(ESMF_GeomType_Flag)  :: srcgeomtype
        type(ESMF_GeomType_Flag)  :: dstgeomtype
        type(ESMF_Grid)      :: srcGrid
        type(ESMF_Grid)      :: dstGrid
        type(ESMF_Array)     :: srcArray
        type(ESMF_Array)     :: dstArray
        type(ESMF_Array)     :: fracArray
        type(ESMF_VM)        :: vm
        type(ESMF_Mesh)      :: srcMesh
        type(ESMF_Mesh)      :: dstMesh
        type(ESMF_MeshLoc)   :: srcMeshloc,dstMeshloc,fracMeshloc
        type(ESMF_StaggerLoc) :: srcStaggerLoc,dstStaggerLoc
        type(ESMF_StaggerLoc) :: srcStaggerLocG2M,dstStaggerLocG2M
        type(ESMF_StaggerLoc) :: fracStaggerLoc
        integer              :: gridDimCount
        type(ESMF_PoleMethod_Flag):: localpolemethod
        integer              :: localRegridPoleNPnts
        logical              :: srcIsLatLonDeg, dstIsLatLonDeg
        integer              :: srcIsSphere, dstIsSphere
        type(ESMF_RegridConserve) :: regridConserveG2M
        real(ESMF_KIND_R8), pointer :: fracFptr(:)
        integer(ESMF_KIND_I4),       pointer :: tmp_indices(:,:)
        real(ESMF_KIND_R8),          pointer :: tmp_weights(:)


        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Warnings for deprecated arguments
        if (present(indices)) then
           call ESMF_LogWrite("The use of argument 'indices' in call " // &
                "ESMF_FieldRegridStore() is DEPRECATED! Use argumemt 'factorIndexList' " // &
                "instead.", ESMF_LOGMSG_WARNING, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (present(weights)) then
           call ESMF_LogWrite("The use of argument 'weights' in call " // &
                "ESMF_FieldRegridStore() is DEPRECATED! Use argumemt 'factorList' " // &
                "instead.", ESMF_LOGMSG_WARNING, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif


        ! global vm for now
        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! Now we go through the painful process of extracting the data members
        ! that we need.
        call ESMF_FieldGet(srcField, geomtype=srcgeomtype, array=srcArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        if (srcgeomtype .eq. ESMF_GEOMTYPE_XGRID) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- RegridStore on XGrid is not supported in this overloaded method", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
                

        call ESMF_FieldGet(dstField, geomtype=dstgeomtype, array=dstArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        if (dstgeomtype .eq. ESMF_GEOMTYPE_XGRID) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- RegridStore on XGrid is not supported in this overloaded method", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif


        ! Set this for now just to not have to remove it everywhere
        ! TODO get rid of it. 
        lregridScheme = ESMF_REGRID_SCHEME_NATIVE


        ! Handle optional method argument
        if (present(regridmethod)) then
           lregridmethod=regridmethod
        else     
           lregridmethod=ESMF_REGRIDMETHOD_BILINEAR
        endif


       ! Handle pole method
        if (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) then
           if (present(polemethod)) then
              if (polemethod .ne. ESMF_POLEMETHOD_NONE) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                      msg="- Only ESMF_POLEMETHOD_NONE polemethod supported for ESMF_REGRIDMETHOD_CONSERVE", & 
                      ESMF_CONTEXT, rcToReturn=rc) 
                 return
              endif
           else    
              localpolemethod=ESMF_POLEMETHOD_NONE
           endif
        else 
           if (present(polemethod)) then
              localpolemethod=polemethod
           else    
              localpolemethod=ESMF_POLEMETHOD_ALLAVG
           endif
        endif
        
        if (localpolemethod .eq. ESMF_POLEMETHOD_NPNTAVG) then
           if (.not. present(regridPoleNPnts)) then
                       call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- RegridPoleNPnts must be specified if polemethod is ESMF_POLEMETHOD_NPNTAVG", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
            return
           else 
             if (regridPoleNPnts < 1) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- RegridPoleNPnts must be >=1 ", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
            return
            endif
           endif
        endif

        ! Set subject to the defaults error checked above
        if (present(regridPoleNPnts)) then
           localRegridPoleNPnts=regridPoleNPnts
        else     
           localRegridPoleNPnts=1
        endif

        ! Set interpretation of grid based on regridScheme
        if (lregridScheme .eq. ESMF_REGRID_SCHEME_FULL3D) then
           srcIsSphere = 1
           srcIsLatLonDeg=.true.
           dstIsSphere = 1
           dstIsLatLonDeg=.true.
        else if (lregridScheme .eq. ESMF_REGRID_SCHEME_FULLTOREG3D) then
           srcIsSphere = 1
           srcIsLatLonDeg=.true.
           dstIsSphere = 0
           dstIsLatLonDeg=.true.
        else if (lregridScheme .eq. ESMF_REGRID_SCHEME_REGTOFULL3D) then
           srcIsSphere = 0
           srcIsLatLonDeg=.true.
           dstIsSphere = 1
           dstIsLatLonDeg=.true.
        else if (lregridScheme .eq. ESMF_REGRID_SCHEME_REGION3D) then
           srcIsSphere = 0
           srcIsLatLonDeg=.true.
           dstIsSphere = 0
           dstIsLatLonDeg=.true.
        else if (lregridScheme .eq. ESMF_REGRID_SCHEME_DCON3D) then
           srcIsSphere = 0
           srcIsLatLonDeg=.true.
           dstIsSphere = 0
           dstIsLatLonDeg=.true.
        else if (lregridScheme .eq. ESMF_REGRID_SCHEME_DCON3DWPOLE) then
           srcIsSphere = 0
           srcIsLatLonDeg=.true.
           dstIsSphere = 0
           dstIsLatLonDeg=.true.
        else  
           srcIsSphere = 0
           srcIsLatLonDeg=.false.
           dstIsSphere = 0
           dstIsLatLonDeg=.false.
        endif

        ! Set conserve flag for GridToMesh
        if (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) then
           regridConserveG2M=ESMF_REGRID_CONSERVE_ON
        else
           regridConserveG2M=ESMF_REGRID_CONSERVE_OFF
        endif


        ! If grids, then convert to a mesh to do the regridding
        if (srcgeomtype .eq. ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(srcField, grid=srcGrid, &
                 staggerloc=srcStaggerloc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

	  ! if we're doing conservative then do some checking and
          ! change staggerloc
          if (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) then
            ! Only Center stagger is supported right now until we figure out what the
            ! control volume for the others should be
	    if (srcStaggerloc .ne. ESMF_STAGGERLOC_CENTER) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- can't currently do conservative regrid on a stagger other then center", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif

            ! Create the mesh from corner stagger to better represent the
            ! control volumes
            call ESMF_GridGet(grid=srcGrid, &
                   dimCount=gridDimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            if (gridDimCount .eq. 2) then
               srcStaggerlocG2M=ESMF_STAGGERLOC_CORNER
            else if (gridDimCount .eq. 3) then
               srcStaggerlocG2M=ESMF_STAGGERLOC_CORNER_VFACE
            else
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- can currently only do conservative regridding on 2D or 3D grids", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif
          else 
            ! If we're not conservative, then use the staggerloc that the field is built upon
	    srcStaggerlocG2M=srcStaggerloc
          endif

          ! check grid
          call checkGrid(srcGrid,srcStaggerlocG2M,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

          ! Convert Grid to Mesh
          srcMesh = ESMF_GridToMesh(srcGrid, srcStaggerLocG2M, srcIsSphere, srcIsLatLonDeg, &
                      maskValues=srcMaskValues, regridConserve=regridConserveG2M, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        else
          call ESMF_FieldGet(srcField, mesh=srcMesh, meshloc=srcMeshloc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          ! Mesh needs to be built on elements for conservative, and nodes for the others
          if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE)) then
              if (srcMeshloc .ne. ESMF_MESHLOC_ELEMENT) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- can currently only do conservative regridding on a mesh built on elements", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
                 return	  
              endif
          else
              if (srcMeshloc .ne. ESMF_MESHLOC_NODE) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- S can currently only do bilinear or patch regridding on a mesh built on nodes", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
                 return	  
              endif
          endif	  
        endif

        if (dstgeomtype .eq. ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(dstField, grid=dstGrid, &
                 staggerloc=dstStaggerloc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

	  ! if we're doing conservative then do some checking and
          ! change staggerloc
          if (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) then
            ! Only Center stagger is supported right now until we figure out what the
            ! control volume for the others should be
	    if (dstStaggerloc .ne. ESMF_STAGGERLOC_CENTER) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- can't currently do conservative regrid on a stagger other then center", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif

            ! Create the mesh from corner stagger to better represent the
            ! control volumes 
            call ESMF_GridGet(grid=dstGrid, &
                   dimCount=gridDimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            if (gridDimCount .eq. 2) then
               dstStaggerlocG2M=ESMF_STAGGERLOC_CORNER
            else if (gridDimCount .eq. 3) then
               dstStaggerlocG2M=ESMF_STAGGERLOC_CORNER_VFACE
            else
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- can currently only do conservative regridding on 2D or 3D grids", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif

          else 
            ! If we're not conservative, then use the staggerloc that the field is built upon
	    dstStaggerlocG2M=dstStaggerloc
          endif

          ! check grid
          call checkGrid(dstGrid,dstStaggerlocG2M,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

          ! Convert Grid to Mesh
          dstMesh = ESMF_GridToMesh(dstGrid, dstStaggerLocG2M, dstIsSphere, dstIsLatLonDeg, &
                      maskValues=dstMaskValues, regridConserve=regridConserveG2M, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_FieldGet(dstField, mesh=dstMesh, meshloc=dstMeshloc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          ! Mesh needs to be built on elements for conservative, and nodes for the others
          if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE)) then
              if (dstMeshloc .ne. ESMF_MESHLOC_ELEMENT) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- can currently only do conservative regridding on a mesh built on elements", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
                return	  
              endif
          else
              if (dstMeshloc .ne. ESMF_MESHLOC_NODE) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- can currently only do bilinear or patch regridding on a mesh built on nodes", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
                return	  
              endif
          endif	  
        endif

        ! At this point, we have the meshes, so we are ready to call
        ! the 'mesh only' interface of the regrid.

        ! call into the Regrid mesh interface
        if (present(weights) .or. present(factorList) .or. &
             present(indices) .or. present(factorIndexList)) then
           call ESMF_RegridStore(srcMesh, srcArray, dstMesh, dstArray, &
                lregridmethod, &
                localpolemethod, localRegridPoleNPnts, &
                lregridScheme, &
                unmappedaction, routehandle, &
                tmp_indices, tmp_weights, localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

           ! attach sparse matrix to appropriate output variable
           if (present(weights)) weights=>tmp_weights
           if (present(factorList)) factorList=>tmp_weights
           if (present(indices)) indices=>tmp_indices
           if (present(factorIndexList)) factorIndexList=>tmp_indices

           ! deallocate if not being passed out
           if (.not. (present(weights) .or. present(factorList))) deallocate(tmp_weights)
           if (.not. (present(indices) .or. present(factorIndexList))) deallocate(tmp_indices)
        else
           call ESMF_RegridStore(srcMesh, srcArray, dstMesh, dstArray, &
                lregridmethod, &
                localpolemethod, localRegridPoleNPnts, &
                lregridScheme, &
                unmappedaction, routehandle, &
                rc=localrc)
           if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif


        ! Get Fraction info
        if (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) then
           if (present(srcFracField)) then
              if (srcgeomtype .eq. ESMF_GEOMTYPE_GRID) then
                 call ESMF_FieldGet(srcFracField, array=fracArray, staggerloc=fracStaggerloc, &
                      rc=localrc)
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return

                 ! Make sure the staggerlocs match
                 if (srcStaggerloc .ne. fracStaggerloc) then
                    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                         msg="- srcFracField staggerloc must match srcField staggerloc", &
                         ESMF_CONTEXT, rcToReturn=rc)
                    return
                 endif

                 ! Get Frac info
                 call ESMF_RegridGetFrac(srcGrid, mesh=srcMesh, array=fracArray, &
                      staggerloc=srcStaggerLocG2M, rc=localrc)
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return
              else if (srcgeomtype .eq. ESMF_GEOMTYPE_MESH) then
                 call ESMF_FieldGet(srcFracField, meshloc=fracMeshloc, &
                      rc=localrc)
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return

                 ! Make sure the locs match
                 if (srcMeshLoc .ne. fracMeshLoc) then
                    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                         msg="- srcFracField staggerloc must match srcField staggerloc", &
                         ESMF_CONTEXT, rcToReturn=rc)
                    return
                 endif

                 ! get frac pointer
                 call ESMF_FieldGet(srcFracField, localDE=0, farrayPtr=fracFptr,  rc=localrc)
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return


                 ! Get Frac info
                 call ESMF_MeshGetElemFrac(srcMesh, fracList=fracFptr, rc=localrc)     
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return
              endif
           endif

           if (present(dstFracField)) then
              if (dstgeomtype .eq. ESMF_GEOMTYPE_GRID) then
                 call ESMF_FieldGet(dstFracField, array=fracArray, staggerloc=fracStaggerloc, &
                      rc=localrc)
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return

                 ! Make sure the staggerlocs match
                 if (dstStaggerloc .ne. fracStaggerloc) then
                    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                         msg="- dstFracField Field staggerloc must match dstField staggerloc", &
                         ESMF_CONTEXT, rcToReturn=rc)
                    return
                 endif

                 call ESMF_RegridGetFrac(dstGrid, mesh=dstMesh, array=fracArray, &
                      staggerloc=dstStaggerLocG2M, rc=localrc)
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return
              else if (dstgeomtype .eq. ESMF_GEOMTYPE_MESH) then
                 call ESMF_FieldGet(dstFracField, meshloc=fracMeshloc, &
                      rc=localrc)
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return

                 ! Make sure the locs match
                 if (dstMeshLoc .ne. fracMeshLoc) then
                    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                         msg="- srcFracField staggerloc must match srcField staggerloc", &
                         ESMF_CONTEXT, rcToReturn=rc)
                    return
                 endif

                 ! get frac pointer
                 call ESMF_FieldGet(dstFracField, localDE=0, farrayPtr=fracFptr,  rc=localrc)
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return

                 ! Get Frac info
                 call ESMF_MeshGetElemFrac(dstMesh, fracList=fracFptr, rc=localrc)     
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                      ESMF_CONTEXT, rcToReturn=rc)) return
              endif
           endif
        endif
     
        ! destroy Meshes, if they were created here
        if (srcgeomtype .ne. ESMF_GEOMTYPE_MESH) then
        call ESMF_MeshDestroy(srcMesh,rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (dstgeomtype .ne. ESMF_GEOMTYPE_MESH) then
        call ESMF_MeshDestroy(dstMesh,rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridStoreNX

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridStoreX"

!BOP
! !IROUTINE: ESMF_FieldRegridStore - Precompute a Field regridding operation and return a RouteHandle using XGrid
!
! !INTERFACE:
  !   Private name; call using ESMF_FieldRegridStore()
      subroutine ESMF_FieldRegridStoreX(xgrid, srcField, dstField, &
                                       keywordEnforcer, routehandle, rc)
!      
! !ARGUMENTS:
      type(ESMF_XGrid),       intent(in)              :: xgrid
      type(ESMF_Field),       intent(in)              :: srcField
      type(ESMF_Field),       intent(inout)           :: dstField
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_RouteHandle), intent(inout), optional :: routehandle
      integer,                intent(out),   optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!       \begin{sloppypar}
!       Creates a sparse matrix operation (stored in {\tt routehandle}) that contains the calculations and 
!       communications necessary to interpolate from {\tt srcField} to {\tt dstField}. The routehandle can then be used in the call
!       {\tt ESMF\_FieldRegrid()} to interpolate between the Fields. Informaton such as
!       index mapping and weights are obtained from the XGrid by matching the Field Grids in the XGrid. It's important the Grids in the {\tt srcField} and {\tt dstField} donot match, i.e.
!       they are different in either tological or geometric characteristic.
!       \end{sloppypar}
!       
!       The routehandle generated by this call is subsequently computed based on these information.
!       If those information don't change the routehandle can be used repeatedly to interpolate from the source Field to the destination Field. 
!       This is true even if the data in the Fields changes. The routehandle may also be used to interpolate between any source and 
!       destination Field which are created on the same stagger location and Grid as the original Fields.        
!
!       When it's no longer needed the routehandle should be destroyed by using {\tt ESMF\_FieldRegridRelease()} to free the memory it's using. 
!       Note {\tt ESMF\_FieldRegridStore()} assumes the coordinates used in the Grids upon which the Fields are built are
!   in degrees.  
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!           Exchange Grid.
!     \item [srcField]
!           Source Field.
!     \item [dstField]
!           Destination Field.
!     \item [{[routehandle]}]
!           The handle that implements the regrid and that can be used in later 
!           {\tt ESMF\_FieldRegrid}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc, i

        type(ESMF_GeomType_Flag)  :: geomtype
        type(ESMF_XGrid)     :: srcXGrid, dstXGrid        

        integer :: srcIdx, dstIdx, ngrid_a, ngrid_b
        type(ESMF_XGridSide_Flag) :: srcSide, dstSide
        type(ESMF_Grid), allocatable :: gridA(:), gridB(:)
        type(ESMF_Grid)      :: srcGrid
        type(ESMF_Grid)      :: dstGrid
        type(ESMF_XGridSpec) :: sparseMat
        logical :: found, match
        type(ESMF_Array)     :: srcArray
        type(ESMF_Array)     :: dstArray
    
        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! look for the correct Grid to use
        ! first Get necessary information from XGrid and Fields
        call ESMF_XGridGet(xgrid, ngridA=ngrid_a, ngridB=ngrid_b, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(gridA(ngrid_a), gridB(ngrid_b))

        call ESMF_XGridGet(xgrid, sideA=gridA, sideB=gridB, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(srcField, geomtype=geomtype, &
               rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! locate the Grid or XGrid contained in srcField
        if(geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(srcField, grid=srcGrid, &
                   rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            found = .false.
            do i = 1, ngrid_a
                if(ESMF_GridMatch(srcGrid, gridA(i))>=ESMF_GRIDMATCH_EXACT) then
                    srcIdx = i
                    srcSide = ESMF_XGRIDSIDE_A
                    found = .true.
                    exit
                endif
            enddo 
            do i = 1, ngrid_b
                if(ESMF_GridMatch(srcGrid, gridB(i))>=ESMF_GRIDMATCH_EXACT) then
                    if(found) then
                        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                           msg="- duplication of Grid found in XGrid", &
                           ESMF_CONTEXT, rcToReturn=rc) 
                        return
                    endif
                    srcIdx = i
                    srcSide = ESMF_XGRIDSIDE_B
                    found = .true.
                    exit
                endif
            enddo 

            if(.not. found) then
                call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                   msg="- cannot Locate src Field Grid in XGrid", &
                   ESMF_CONTEXT, rcToReturn=rc) 
                return
            endif

        else
            if(geomtype == ESMF_GEOMTYPE_XGRID) then
                call ESMF_FieldGet(srcField, xgrid=srcXGrid, &
                       rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
                
                match = ESMF_XGridMatch(xgrid, srcXGrid, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return

                if(match) then
                    srcSide = ESMF_XGRIDSIDE_BALANCED
                    srcIdx = 1
                else
                    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                           msg="- XGrid in srcField doesn't match the input XGrid", &
                           ESMF_CONTEXT, rcToReturn=rc) 
                    return
                endif
            else
                call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                       msg="- src Field is not built on Grid or XGrid", &
                       ESMF_CONTEXT, rcToReturn=rc) 
                return
            endif
        endif

        ! locate the Grid or XGrid contained in dstField
        call ESMF_FieldGet(dstField, geomtype=geomtype, &
               rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if(geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(dstField, grid=dstGrid, &
                   rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            found = .false.
            do i = 1, ngrid_a
                if(ESMF_GridMatch(dstGrid, gridA(i))>=ESMF_GRIDMATCH_EXACT) then
                    dstIdx = i
                    dstSide = ESMF_XGRIDSIDE_A
                    found = .true.
                    exit
                endif
            enddo 
            do i = 1, ngrid_b
                if(ESMF_GridMatch(dstGrid, gridB(i))>=ESMF_GRIDMATCH_EXACT) then
                    if(found) then
                        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                           msg="- duplication of Grid found in XGrid", &
                           ESMF_CONTEXT, rcToReturn=rc) 
                        return
                    endif
                    dstIdx = i
                    dstSide = ESMF_XGRIDSIDE_B
                    found = .true.
                    exit
                endif
            enddo 

            if(.not. found) then
                call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                   msg="- cannot Locate dst Field Grid in XGrid", &
                   ESMF_CONTEXT, rcToReturn=rc) 
                return
            endif

        else
            if(geomtype == ESMF_GEOMTYPE_XGRID) then
                call ESMF_FieldGet(dstField, xgrid=dstXGrid, &
                       rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return

                match = ESMF_XGridMatch(xgrid, dstXGrid, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return

                if(match) then
                    dstSide = ESMF_XGRIDSIDE_BALANCED
                    dstIdx = 1
                else
                    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                           msg="- XGrid in dstField doesn't match the input XGrid", &
                           ESMF_CONTEXT, rcToReturn=rc) 
                    return
                endif
            else
                call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                       msg="- src Field is not built on Grid or XGrid", &
                       ESMF_CONTEXT, rcToReturn=rc) 
                return
            endif
        endif

        ! src and dst Fields should not be on the same side
        if ( srcSide == dstSide ) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
               msg="- src and dst Fields should not be on same side of the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif

        ! retrieve the correct sparseMat structure
        call ESMF_XGridGet(xgrid, sparseMat, srcSide, srcIdx, &
            dstSide, dstIdx, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! Now we go through the painful process of extracting the data members
        ! that we need. Need to resolve compilation order.
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySMMStore(srcArray, dstArray, routehandle, &
            sparseMat%factorList, sparseMat%factorIndexList, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        !call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
        !    sparseMat%factorList, sparseMat%factorIndexList, &
        !    localrc)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        !  ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridStoreX


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridGetArea"

!BOPI
! !IROUTINE: ESMF_FieldRegridGetArea - Get the integration area
!
! !INTERFACE:
  !   Private name; call using ESMF_FieldRegridGetArea()
      subroutine ESMF_FieldRegridGetArea(areaField, MaskValues, rc)
!
! !RETURN VALUE:
!      
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)                 :: areaField
      integer(ESMF_KIND_I4), intent(in), optional     :: MaskValues(:)
      integer, intent(out), optional                  :: rc 
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item [Field]
!           The Field.
!     \item [{[regridScheme]}]
!           Whether to convert to spherical coordinates 
!           ({\tt ESMF\_REGRID\_SCHEME\_FULL3D}), 
!           or to leave in native coordinates 
!           ({\tt ESMF\_REGRID\_SCHEME\_NATIVE}). 
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
        integer :: localrc
        integer              :: lregridScheme
        type(ESMF_GeomType_Flag)  :: geomtype

        type(ESMF_Grid)      :: Grid
        type(ESMF_Array)     :: Array
        type(ESMF_Mesh)      :: Mesh
        type(ESMF_StaggerLoc) :: staggerLoc, staggerLocG2M
	type(ESMF_MeshLoc)   :: meshloc
        real(ESMF_KIND_R8), pointer :: areaFptr(:)
        integer :: gridDimCount, localDECount
	type(ESMF_TypeKind_Flag) :: typekind
        logical              :: isLatLonDeg
        integer              :: isSphere

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
        if (present(rc)) rc = ESMF_RC_NOT_IMPL


  !  check Field and areaField to make sure they are from the same grid

        ! Now we go through the painful process of extracting the data members
        ! that we need.
        call ESMF_FieldGet(areaField, typekind=typekind, geomtype=geomtype, array=Array, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

	! Check typekind
        if (typekind .ne. ESMF_TYPEKIND_R8) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- Area calculation is only supported for Fields of typekind=ESMF_TYPEKIND_R8", & 
              ESMF_CONTEXT, rcToReturn=rc) 
           return
        endif


        ! TODO: Get rid of this
        lregridScheme = ESMF_REGRID_SCHEME_NATIVE
          

        ! Set interpretation of grid based on regridScheme
        if (lregridScheme .eq. ESMF_REGRID_SCHEME_FULL3D) then
           isSphere = 1
           isLatLonDeg=.true.
        else if (lregridScheme .eq. ESMF_REGRID_SCHEME_REGION3D) then
           isSphere = 0
           isLatLonDeg=.true.
        else  
           isSphere = 0
           isLatLonDeg=.false.
        endif

        ! If grids, then convert to a mesh to do the regridding
        if (geomtype .eq. ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(areaField, grid=Grid, &
                 staggerloc=staggerLoc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          ! Only Center stagger is supported right now until we figure out what the
          ! control volume for the others should be
	  if (staggerloc .ne. ESMF_STAGGERLOC_CENTER) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- can't currently calculate area on a stagger other then center", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
          endif

            ! Create the mesh from corner stagger to better represent the
            ! control volumes 
            call ESMF_GridGet(grid=Grid, &
                   dimCount=gridDimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            if (gridDimCount .eq. 2) then
               staggerlocG2M=ESMF_STAGGERLOC_CORNER
            else if (gridDimCount .eq. 3) then
               staggerlocG2M=ESMF_STAGGERLOC_CORNER_VFACE
            else
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- can currently only do conservative regridding on 2D or 3D grids", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
            endif


          ! check grid
          call checkGrid(Grid, staggerlocG2M, rc=localrc)

          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

          ! Convert Grid to Mesh
          Mesh = ESMF_GridToMesh(Grid, staggerlocG2M, isSphere, isLatLonDeg, &
                      maskValues=MaskValues, regridConserve=ESMF_REGRID_CONSERVE_OFF, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

	  ! call into the Regrid GetareaField interface
          call ESMF_RegridGetArea(Grid, Mesh, Array, staggerlocG2M, lregridScheme, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_FieldGet(areaField, mesh=Mesh, meshloc=meshloc, &
                 localDECount=localDECount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

	  if (meshloc .ne. ESMF_MESHLOC_ELEMENT) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- can't currently calculate area on a mesh location other than elements", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
          endif

	  ! Don't need to do anything if there are no DEs
	  if (localDECount < 1) then
              if(present(rc)) rc = ESMF_SUCCESS
              return
          endif

          ! Get pointer to field data
	  ! Right now Mesh will only have one DE per PET
          call ESMF_FieldGet(areaField, localDE=0,  farrayPtr=areaFptr, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          ! Get Area
	  call ESMF_MeshGetElemArea(mesh, areaList=areaFptr, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        endif


        ! destroy Mesh, if they were created here
        if (geomtype .ne. ESMF_GEOMTYPE_MESH) then
        call ESMF_MeshDestroy(Mesh,rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridGetArea

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridGetIwts"

!BOPI
! !IROUTINE: ESMF_FieldRegridGetIwts - Get the integration weights
!
! !INTERFACE:
  !   Private name; call using ESMF_FieldRegridGetIwts()
      subroutine ESMF_FieldRegridGetIwts(Field, Iwts, MaskValues, regridScheme, rc)
!
! !RETURN VALUE:
!      
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)                    :: Field
      type(ESMF_Field), intent(inout)                 :: Iwts
      integer(ESMF_KIND_I4), intent(in), optional     :: MaskValues(:)
      integer, intent(in), optional                   :: regridScheme
      integer, intent(out), optional                  :: rc 
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item [Field]
!           The Field.
!     \item [{[regridScheme]}]
!           Whether to convert to spherical coordinates 
!           ({\tt ESMF\_REGRID\_SCHEME\_FULL3D}), 
!           or to leave in native coordinates 
!           ({\tt ESMF\_REGRID\_SCHEME\_NATIVE}). 
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
        integer :: localrc
        integer              :: isSphere
        integer              :: lregridScheme
        type(ESMF_GeomType_Flag)  :: geomtype

        type(ESMF_Grid)      :: Grid
        type(ESMF_Array)     :: Array
        type(ESMF_VM)        :: vm
        type(ESMF_Mesh)      :: Mesh
        type(ESMF_StaggerLoc) :: staggerLoc

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! global vm for now
        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

  !  check Field and Iwts to make sure they are from the same grid

        ! Now we go through the painful process of extracting the data members
        ! that we need.
        call ESMF_FieldGet(Iwts, geomtype=geomtype, array=Array, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! Will eventually determine scheme either as a parameter or from properties
        ! of the source grid
        if (present(regridScheme)) then
          lregridScheme = regridScheme
        else
          lregridScheme = ESMF_REGRID_SCHEME_NATIVE
        endif

        ! If grids, then convert to a mesh to do the regridding
        if (geomtype .eq. ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(Iwts, grid=Grid, &
                 staggerloc=staggerLoc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          ! check grid
          call checkGrid(Grid,staggerloc,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

          ! Convert Grid to Mesh
          Mesh = ESMF_GridToMesh(Grid, staggerLoc, isSphere, &
                      maskValues=MaskValues, regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        else
          call ESMF_FieldGet(Iwts, mesh=Mesh, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! call into the Regrid GetIwts interface
        call ESMF_RegridGetIwts(Grid, Mesh, Array, staggerLoc, lregridScheme, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

        ! destroy Mesh, if they were created here
        if (geomtype .ne. ESMF_GEOMTYPE_MESH) then
        call ESMF_MeshDestroy(Mesh,rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridGetIwts

!------------------------------------------------------------------------------

    ! Small subroutine to make sure that Grid doesn't
    ! contain some of the properties that aren't currently
    ! allowed in regridding
    subroutine checkGrid(grid,staggerloc,rc)
        type (ESMF_Grid) :: grid
        type(ESMF_StaggerLoc) :: staggerloc
        integer, intent(out), optional :: rc
        type(ESMF_GridDecompType) :: decompType
        integer :: localDECount, lDE, ec(ESMF_MAXDIM)
        integer :: localrc, i, dimCount

        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Make sure Grid isn't arbitrarily distributed
        call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

       ! Error if decompType is ARBITRARY
       if (decompType .eq. ESMF_GRID_ARBITRARY) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- can't currently regrid an arbitrarily distributed Grid", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
       endif        

       ! Make sure Grid doesn't contain width 1 DEs
       call ESMF_GridGet(grid,localDECount=localDECount, dimCount=dimCount, &
              rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
       
       ! loop through checking DEs
       do lDE=0,localDECount-1
           
           ! Get bounds of DE
           call ESMF_GridGet(grid,staggerloc=staggerloc, localDE=lDE, &
                  exclusivecount=ec,rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

           ! loop and make sure they aren't too small in any dimension
           do i=1,dimCount
              if (ec(i) .lt. 2) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
              msg="- can't currently regrid a grid that contains a DE of width less than 2", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
              return
              endif
           enddo
       enddo

       if(present(rc)) rc = ESMF_SUCCESS
   end subroutine checkGrid

!------------------------------------------------------------------------------

end module ESMF_FieldRegridMod
