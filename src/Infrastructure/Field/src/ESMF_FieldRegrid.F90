! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
  use ESMF_DynamicMaskMod
  use ESMF_ArrayMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_GridUtilMod
  use ESMF_StaggerLocMod
  use ESMF_MeshMod
  use ESMF_RHandleMod
  use ESMF_GeomBaseMod
  use ESMF_XGridGeomBaseMod
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_FieldCreateMod
  use ESMF_FieldGetMod
  use ESMF_FieldSMMMod
  use ESMF_XGridMod
  use ESMF_XGridGetMod
  use ESMF_PointListMod
  use ESMF_LocStreamMod
  use ESMF_IOScripMod
  use ESMF_TraceMod
  
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
   private checkGridLite               ! same as checkGrid but less restrictive 
                                       !  due to anticipated conversion to pointlist

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
    '$Id$'


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
  subroutine ESMF_FieldRegrid(srcField, dstField, routehandle, keywordEnforcer, &
    zeroregion, termorderflag, checkflag, dynamicMask, rc)
!
! !ARGUMENTS:
      type(ESMF_Field),               intent(in),    optional :: srcField
      type(ESMF_Field),               intent(inout), optional :: dstField
      type(ESMF_RouteHandle),         intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Region_Flag),         intent(in),    optional :: zeroregion
      type(ESMF_TermOrder_Flag),      intent(in),    optional :: termorderflag
      logical,                        intent(in),    optional :: checkflag
      type(ESMF_DynamicMask), target, intent(in),    optional :: dynamicMask
      integer,                        intent(out),   optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt termorderflag}.
!              The new argument gives the user control over the order in which
!              the src terms are summed up.
! \item[7.1.0r] Added argument {\tt dynamicMask}.
!              The new argument supports the dynamic masking feature.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Execute the precomputed regrid operation stored in {\tt routehandle} to 
!   interpolate from {\tt srcField} to {\tt dstField}.  See {\tt ESMF\_FieldRegridStore()} on how to 
!   precompute the {\tt routehandle}. 
!  
!   \begin{sloppypar}
!   Both {\tt srcField} and {\tt dstField} must match the respective Fields
!   used during {\tt ESMF\_FieldRegridStore()} in {\em type}, {\em kind}, and 
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
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
!   \item [{[termorderflag]}]
!     Specifies the order of the source side terms in all of the destination
!     sums. The {\tt termorderflag} only affects the order of terms during 
!     the execution of the RouteHandle. See the \ref{RH:bfb} section for an
!     in-depth discussion of {\em all} bit-for-bit reproducibility
!     aspects related to route-based communication methods.
!     See \ref{const:termorderflag} for a full list of options.
!     The default setting depends on whether the {\tt dynamicMask} argument
!     is present or not. With {\tt dynamicMask} argument present, the default
!     of {\tt termorderflag} is {\tt ESMF\_TERMORDER\_SRCSEQ}. This ensures
!     that {\tt all} source terms are present on the destination side, and 
!     the interpolation can be calculated as a single sum. When 
!     {\tt dynamicMask} is absent, the default of {\tt termorderflag} is
!     {\tt ESMF\_TERMORDER\_FREE}, allowing maximum flexibility and partial 
!     sums for optimum performance.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[dynamicMask]}]
!     Object holding dynamic masking information.
!     See section \ref{RH:DynMask} for a discussion of dynamic masking.
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
                 termorderflag=termorderflag, checkflag=checkflag, &
                 dynamicMask=dynamicMask, rc=localrc)
        else if (present(srcField) .and. .not. present(dstField)) then
          call ESMF_ArraySMM(srcArray=srcArray, &
                 routehandle=routehandle, zeroregion=zeroregion, &
                 termorderflag=termorderflag, checkflag=checkflag, &
                 dynamicMask=dynamicMask, rc=localrc)
        else if (.not. present(srcField) .and. present(dstField)) then
          call ESMF_ArraySMM(dstArray=dstArray, &
                 routehandle=routehandle, zeroregion=zeroregion, &
                 termorderflag=termorderflag, checkflag=checkflag, &
                 dynamicMask=dynamicMask, rc=localrc)
        else if (.not. present(srcField) .and. .not. present(dstField)) then
          call ESMF_ArraySMM(routehandle=routehandle, zeroregion=zeroregion, &
                 termorderflag=termorderflag, checkflag=checkflag, &
                 dynamicMask=dynamicMask, rc=localrc)
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
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridRelease"
!BOP
! !IROUTINE: ESMF_FieldRegridRelease - Free resources used by a regridding operation
!
! !INTERFACE:
      subroutine ESMF_FieldRegridRelease(routehandle, keywordEnforcer, &
        noGarbage, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,                intent(in),  optional :: noGarbage
      integer,                intent(out), optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!     Free resources used by regrid objec
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle]
!           Handle carrying the sparse matrix
!     \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc

        call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridRelease
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridStoreNX"

!BOP
! !IROUTINE: ESMF_FieldRegridStore - Precompute a Field regridding operation and return a RouteHandle and weights
! \label{api:esmf_fieldregridstorenx}
! !INTERFACE:
  !   Private name; call using ESMF_FieldRegridStore()
      subroutine ESMF_FieldRegridStoreNX(srcField, dstField, keywordEnforcer, &
                    srcMaskValues, dstMaskValues, &
                    regridmethod, &
                    polemethod, regridPoleNPnts, & 
                    lineType, &
                    normType, &
                    extrapMethod, &
                    extrapNumSrcPnts, &
                    extrapDistExponent, &
                    extrapNumLevels, &
                    extrapNumInputLevels, &
                    unmappedaction, ignoreDegenerate, &
                    srcTermProcessing, & 
                    pipeLineDepth, &
                    routehandle, &
                    factorList, factorIndexList, & 
                    weights, indices, &  ! DEPRECATED ARGUMENTS
                    srcFracField, dstFracField, &
                    dstStatusField, &
                    unmappedDstList, &
                    rc)
!      
! !ARGUMENTS:
      type(ESMF_Field),               intent(in)              :: srcField
      type(ESMF_Field),               intent(inout)           :: dstField
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),          intent(in),    optional :: srcMaskValues(:)
      integer(ESMF_KIND_I4),         intent(in),    optional :: dstMaskValues(:)
      type(ESMF_RegridMethod_Flag),   intent(in),    optional :: regridmethod
      type(ESMF_PoleMethod_Flag),     intent(in),    optional :: polemethod
      integer,                        intent(in),    optional :: regridPoleNPnts
      type(ESMF_LineType_Flag),       intent(in),    optional :: lineType
      type(ESMF_NormType_Flag),       intent(in),    optional :: normType
      type(ESMF_ExtrapMethod_Flag),   intent(in),    optional :: extrapMethod
      integer,                        intent(in),    optional :: extrapNumSrcPnts
      real(ESMF_KIND_R4),             intent(in),    optional :: extrapDistExponent
      integer,                        intent(in),    optional :: extrapNumLevels
      integer,                        intent(in),    optional :: extrapNumInputLevels
      type(ESMF_UnmappedAction_Flag), intent(in),    optional :: unmappedaction
      logical,                        intent(in),    optional :: ignoreDegenerate
      integer,                        intent(inout), optional :: srcTermProcessing
      integer,                        intent(inout), optional :: pipeLineDepth
      type(ESMF_RouteHandle),         intent(inout), optional :: routehandle
      real(ESMF_KIND_R8),             pointer,       optional :: factorList(:)
      integer(ESMF_KIND_I4),          pointer,       optional :: factorIndexList(:,:)
      real(ESMF_KIND_R8),    pointer, optional :: weights(:)   ! DEPRECATED ARG
      integer(ESMF_KIND_I4), pointer, optional :: indices(:,:) ! DEPRECATED ARG
      type(ESMF_Field),               intent(inout), optional :: srcFracField
      type(ESMF_Field),               intent(inout), optional :: dstFracField
      type(ESMF_Field),               intent(inout), optional :: dstStatusField
      integer(ESMF_KIND_I4),          pointer,       optional :: unmappedDstList(:)
      integer,                        intent(out),   optional :: rc 
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
! \item[6.1.0] Added arguments {\tt ignoreDegenerate}, {\tt srcTermProcessing},
!              {\tt pipelineDepth}, and {\tt unmappedDstList}.
!              The argument {\tt ignoreDegenerate} allows the user to skip degenerate
!              cells in the regridding instead of stopping with an error.
!              The two arguments {\tt srcTermProcessing} and {\tt pipelineDepth}
!              provide access to the tuning parameters affecting the sparse matrix
!              execution. The argument {\tt unmappedDstList} allows the user to
!              get a list of the destination items which the regridding couldn't
!              map to a source.
! \item[6.3.0r] Added argument {\tt lineType}. This argument allows the user to
!               control the path of the line between two points on a sphere surface.
!               This allows the user to use their preferred line path for the calculation
!               of distances and the shape of cells during regrid weight calculation on
!               a sphere.
! \item[6.3.0rp1] Added argument {\tt normType}. This argument allows the user to
!               control the type of normalization done during conservative weight generation.
! \item[7.1.0r] Added argument {\tt dstStatusField}. This argument allows the user to
!              receive information about what happened to each location in the destination
!              Field during regridding.
!
!              Added arguments {\tt extrapMethod}, {\tt extrapNumSrcPnts}, and
!              {\tt extrapDistExponent}. These three new extrapolation arguments allow the 
!              user to extrapolate destination points not mapped by the regrid method. 
!              {\tt extrapMethod} allows the user to choose the extrapolation method.
!              {\tt extrapNumSrcPnts} and {\tt extrapDistExponent} are parameters that
!              allow the user to tune the behavior of the {\tt ESMF\_EXTRAPMETHOD\_NEAREST\_IDAVG} 
!              method.
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
!       coordinates in the Grids or Meshes contained in the Fields.  If those
!       coordinates don't change the routehandle can
!       be used repeatedly to interpolate from the source Field to the 
!       destination Field.  This is true even if the data in the Fields 
!       changes. The routehandle may also be used to interpolate between any
!       source and destination Field which are created on the same location 
!       in the same Grid, LocStream, or Mesh as the original Fields.        
!
!       When it's no longer needed the routehandle should be destroyed by 
!       using {\tt ESMF\_FieldRegridRelease()} to free the memory it's using. 
!
!       Note, as a side effect, that this call may change the data in {\tt dstField}. If
!       this is undesirable, then an easy work around is to create a second temporary field
!       with the same structure as {\tt dstField} and pass that in instead. 
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           Source Field.
!     \item [dstField]
!           Destination Field. The data in this Field may be overwritten by this call. 
!     \item [{[srcMaskValues]}]
!           Mask information can be set in the Grid (see~\ref{sec:usage:items}) or Mesh (see~\ref{sec:mesh:mask}) 
!           upon which the {\tt srcField} is built. The {\tt srcMaskValues} argument specifies the values in that 
!           mask information which indicate a source point should be masked out. In other words, a locati on is masked if and only if the
!           value for that location in the mask information matches one of the values listed in {\tt srcMaskValues}.  
!           If {\tt srcMaskValues} is not specified, no masking will occur. 
!     \item [{[dstMaskValues]}]
!           Mask information can be set in the Grid (see~\ref{sec:usage:items}) or Mesh (see~\ref{sec:mesh:mask}) 
!           upon which the {\tt dstField} is built. The {\tt dstMaskValues} argument specifies the values in that 
!           mask information which indicate a destination point should be masked out. In other words, a location is masked if and only if the
!           value for that location in the mask information matches one of the values listed in {\tt dstMaskValues}.  
!           If {\tt dstMaskValues} is not specified, no masking will occur. 
!     \item [{[regridmethod]}]
!           The type of interpolation. Please see Section~\ref{opt:regridmethod} 
!           for a list of valid options. If not specified, defaults to 
!           {\tt ESMF\_REGRIDMETHOD\_BILINEAR}.
!     \item [{[polemethod]}]
!           Specifies the type of pole
!           to construct on the source Grid during regridding. Please see 
!           Section~\ref{const:polemethod} for a list of
!           valid options. If not specified, defaults to {\tt ESMF\_POLEMETHOD\_ALLAVG} for non-conservative regrid methods, 
!           and {\tt ESMF\_POLEMETHOD\_NONE} for conservative methods.
!     \item [{[regridPoleNPnts]}]
!           If {\tt polemethod} is {\tt ESMF\_POLEMETHOD\_NPNTAVG},
!           then this parameter indicates the number of points over which to average.
!           If {\tt polemethod} is not {ESMF\_POLEMETHOD\_NPNTAVG} and {\tt regridPoleNPnts} is specified, 
!           then it will be ignored.  
!           This subroutine will return an error if {\tt polemethod} is {ESMF\_POLEMETHOD\_NPNTAVG} and 
!           {\tt regridPoleNPnts} is not specified. 
!     \item [{[lineType]}]
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
!           only applies to weights generated with {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE} or  {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE\_2ND}
!           Please see  Section~\ref{opt:normType} for a 
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
!     \item [{[extrapNumLevels]}] 
!           The number of levels to output for the extrapolation methods that fill levels
!           (e.g. {\tt ESMF\_EXTRAPMETHOD\_CREEP}). When a method is used that requires this, then an error will be returned, if it 
!           is not specified.
!     \item [{[extrapNumInputLevels]}] 
!           The number of levels to use as input for the extrapolation methods that use levels
!           (e.g. {\tt ESMF\_EXTRAPMETHOD\_CREEP}). If not specified, defaults to 1.
!     \item [{[unmappedaction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Please see Section~\ref{const:unmappedaction} for a 
!           list of valid options. If not specified, {\tt unmappedaction} defaults to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!     \item [{[ignoreDegenerate]}]
!           Ignore degenerate cells when checking the input Grids or Meshes for errors. If this is set to true, then the 
!           regridding proceeds, but degenerate cells will be skipped. If set to false, a degenerate cell produces an error. 
!           If not specified, {\tt ignoreDegenerate} defaults to false.
!     \item [{[srcTermProcessing]}]
!           The {\tt srcTermProcessing} parameter controls how many source terms,
!           located on the same PET and summing into the same destination element,
!           are summed into partial sums on the source PET before being transferred
!           to the destination PET. A value of 0 indicates that the entire arithmetic
!           is done on the destination PET; source elements are neither multiplied 
!           by their factors nor added into partial sums before being sent off by the
!           source PET. A value of 1 indicates that source elements are multiplied
!           by their factors on the source side before being sent to the destination
!           PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
!           of terms in the partial sums on the source side.
!
!     Note that partial sums may lead to bit-for-bit differences in the results.
!     See section \ref{RH:bfb} for an in-depth discussion of {\em all}
!     bit-for-bit reproducibility aspects related to route-based communication
!     methods.
!
!     \begin{sloppypar}
!     The {\tt ESMF\_FieldRegridStore()} method implements an auto-tuning scheme
!     for the {\tt srcTermProcessing} parameter. The intent on the 
!     {\tt srcTermProcessing} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt srcTermProcessing} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt srcTermProcessing} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
!     {\tt srcTermProcessing} argument is omitted.
!     \end{sloppypar}
!     
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a sparse matrix exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_FieldRegridStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the 
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
  !     {\tt pipelineDepth} argument is omitted.
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
!           The first dimension of {\tt factorIndexList} is of size 2. {\tt factorIndexList(1,:)} specifies 
!           the sequence index of the source element in the {\tt srcField}. {\tt factorIndexList(2,:)} specifies 
!           the sequence index of the destination element in the {\tt dstField}. The se cond dimension of 
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
!           valid when regridmethod is {\tt ESMF\_REGRIDMETHOD\_CONSERVE} or  {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE\_2ND}.
!           This Field needs to be created on the same location (e.g staggerloc) 
!           as the srcField.
!     \item [{[dstFracField]}] 
!           The fraction of each destination cell participating in the regridding. Only 
!           valid when regridmethod is {\tt ESMF\_REGRIDMETHOD\_CONSERVE} or  {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE\_2ND}.
!           This Field needs to be created on the same location (e.g staggerloc) 
!           as the dstField. It is important to note that the current implementation
!           of conservative regridding doesn't normalize the interpolation weights by the destination fraction. This   means that for a destination
!           grid which only partially overlaps the source grid the destination field which is output from the 
!           regrid operation should be divided by the corresponding destination fraction to yield the 
!           true interpolated values for cells which are only partially covered by the  source grid. 
!     \item [{[dstStatusField]}] 
!           An ESMF Field which outputs a regrid status value for each destination location.
!           Section~\ref{opt:regridstatus} indicates the meaning of each value. The Field needs to 
!           be built on the same grid-location (e.g. staggerloc) in the same Grid, Mesh, or LocStream as the {\tt dstField} argument. 
!           The Field also needs to be of typekind {\tt ESMF\_TYPEKIND\_I4}.  This option currently doesn't work with 
!           the {\tt ESMF\_REGRIDMETHOD\_NEAREST\_DTOS} regrid method.
!     \item [{[unmappedDstList]}] 
!           The list of the sequence indices for locations in {\tt dstField} which couldn't be mapped the {\tt srcField}. 
!           The list on each PET only contains the unmapped locations for the piece of the {\tt dstField} on that PET. 
!           If a destination point is masked, it won't be put in this list. This option currently doesn't work with 
!           the {\tt ESMF\_REGRIDMETHOD\_NEAREST\_DTOS} regrid method.
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
        type(ESMF_Mesh)      :: srcMesh, srcMeshDual
        type(ESMF_Mesh)      :: dstMesh, dstMeshDual
        type(ESMF_Mesh)      :: tempMesh
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
        logical :: localIgnoreDegenerate
        type(ESMF_LineType_Flag):: localLineType
        type(ESMF_NormType_Flag):: localNormType
        type(ESMF_ExtrapMethod_Flag):: localExtrapMethod
        logical :: srcDual, dstDual
        logical :: src_pl_used, dst_pl_used
        type(ESMF_PointList) :: dstPointList, srcPointList
        type(ESMF_LocStream) :: dstLocStream, srcLocStream
        logical :: hasStatusArray
        type(ESMF_Array) :: statusArray
        type(ESMF_TypeKind_Flag) :: typekind
        integer :: tileCount
        integer :: localExtrapNumSrcPnts
        real(ESMF_KIND_R8) :: localExtrapDistExponent
        integer :: localExtrapNumLevels
        integer :: localExtrapNumInputLevels


!        real(ESMF_KIND_R8) :: beg_time, end_time
!        call ESMF_VMWtime(beg_time)

        ! ESMF_METHOD_ENTER(localrc)

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
                "ESMF_FieldRegridStore() is DEPRECATED! Use argumemt 'factorList' " //  &
                "instead.", ESMF_LOGMSG_WARNING, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! process status field argument
        hasStatusArray=.false.
        if (present(dstStatusField)) then
           call ESMF_FieldGet(dstStatusField, array=statusArray, rc=localrc) 
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
 
           hasStatusArray=.true.
        endif

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
              msg="- RegridStore on XGrid is not supported in this overloaded method",  & 
              ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif


        ! Error check dstStatusField
        if (present(dstStatusField)) then
           call ESMF_FieldGet(dstStatusField, typekind=typekind, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           if (typekind .ne. ESMF_TYPEKIND_I4) then
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                   msg=" dstStatusField must have typekind = ESMF_TYPEKIND_I4.",  & 
                   ESMF_CONTEXT, rcToReturn=rc) 
              return
           endif
        endif


        ! Init variables
        srcDual=.false.
        dstDual=.false.
        src_pl_used=.false.
        dst_pl_used=.false.

        ! Set this for now just to not have to remove it everywhere
        ! TODO get rid of it. 
        lregridScheme = ESMF_REGRID_SCHEME_NATIVE


        ! Handle optional method argument
        if (present(regridmethod)) then
           lregridmethod=regridmethod
        else     
           lregridmethod=ESMF_REGRIDMETHOD_BILINEAR
        endif

        ! Handle optional extrap method argument
        if (present(extrapMethod)) then
           localExtrapMethod=extrapMethod
        else     
           localExtrapMethod=ESMF_EXTRAPMETHOD_NONE
        endif


        ! Handle optional extrapDistExponent
        if (present(extrapDistExponent)) then
           localExtrapDistExponent=REAL(extrapDistExponent,ESMF_KIND_R8)
        else     
           localExtrapDistExponent=2.0_ESMF_KIND_R8
        endif

        ! Handle optional extrapNumInputLevels
        if (present(extrapNumLevels)) then
           localExtrapNumLevels=extrapNumLevels
        else     
           if (localExtrapMethod==ESMF_EXTRAPMETHOD_CREEP) then 
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                   msg=" If extrapMethod is ESMF_EXTRAPMETHOD_CREEP, then extrapNumLevels must be specified.", & 
                   ESMF_CONTEXT, rcToReturn=rc) 
              return
           endif
        endif

        ! Handle optional extrapNumInputLevels
        if (present(extrapNumInputLevels)) then
           localExtrapNumInputLevels=extrapNumInputLevels
        else     
           localExtrapNumInputLevels=1
        endif

        ! TODO: If lineType is present then do error checking here

        ! Handle optional lineType argument
        if (present(lineType)) then
           localLineType=lineType
        else     
           localLineType=ESMF_LINETYPE_CART
        endif

         ! Handle optional normType argument
        if (present(normType)) then
           localNormType=normType
        else     
           localNormType=ESMF_NORMTYPE_DSTAREA
        endif


       ! Handle pole method
        if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
             (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
           if (present(polemethod)) then
              if (polemethod .ne. ESMF_POLEMETHOD_NONE) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                   msg="- Only ESMF_POLEMETHOD_NONE polemethod supported for conservative regrid methods", & 
                   ESMF_CONTEXT, rcToReturn=rc) 
                 return
              else
                 localpolemethod = polemethod
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

        ! Handle default for extrapNumSrcPnts
        if (present(extrapNumSrcPnts)) then
           localExtrapNumSrcPnts=extrapNumSrcPnts
        else 
           localExtrapNumSrcPnts=8
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

        ! Set subject to the defaults error checked above
        localIgnoreDegenerate=.false.
        if (present(ignoreDegenerate)) then
           localIgnoreDegenerate=ignoreDegenerate
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
        if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
             (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
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
        if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
             (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
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
                   dimCount=gridDimCount, tileCount=tileCount, rc=localrc)
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


          if (lregridmethod .eq. ESMF_REGRIDMETHOD_NEAREST_STOD .or. &
              lregridmethod .eq. ESMF_REGRIDMETHOD_NEAREST_DTOS) then

            ! check grid
            call checkGridLite(srcGrid,srcStaggerlocG2M,rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            srcPointList=ESMF_PointListCreate(srcGrid,srcStaggerlocG2M, &
                                              maskValues=srcMaskValues, &
                                              rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            src_pl_used=.true.

  else if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
       (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then

            ! check grid
            call checkGrid(srcGrid,srcStaggerlocG2M,rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            ! Convert Grid to Mesh
            if (tileCount .eq. 1) then
               srcMesh = ESMF_GridToMesh(srcGrid, srcStaggerLocG2M, srcIsSphere, srcIsLatLonDeg, &
                    maskValues=srcMaskValues, regridConserve=regridConserveG2M, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            else 
               srcMesh = ESMF_GridToMeshCell(srcGrid, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

               ! Turn on masking
               if (present(srcMaskValues)) then
                  call ESMF_MeshTurnOnCellMask(srcMesh, maskValues=srcMaskValues, rc=localrc);
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                       ESMF_CONTEXT, rcToReturn=rc)) return
               endif
            endif
          else 

            ! check grid
            call checkGrid(srcGrid,srcStaggerlocG2M,rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            ! ESMF_REGION_ENTER("gridToMesh", localrc)
            ! Convert Grid to Mesh
            srcMesh = ESMF_GridToMesh(srcGrid, srcStaggerLocG2M, srcIsSphere, srcIsLatLonDeg, &
                        maskValues=srcMaskValues, regridConserve=regridConserveG2M, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            ! ESMF_REGION_EXIT("gridToMesh", localrc)
          endif

        else if (srcgeomtype .eq. ESMF_GEOMTYPE_MESH) then

          call ESMF_FieldGet(srcField, mesh=tempMesh, meshloc=srcMeshloc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          ! Mesh needs to be built on elements for conservative, and nodes for the others
          if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
               (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
             if (srcMeshloc .ne. ESMF_MESHLOC_ELEMENT) then
                call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                  msg="- can currently only do conservative regridding on a mesh built on elements", & 
                  ESMF_CONTEXT, rcToReturn=rc) 
                return  
             endif

             ! Turn on masking
             if (present(srcMaskValues)) then
                call ESMF_MeshTurnOnCellMask(tempMesh, maskValues=srcMaskValues, rc=localrc);
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
             endif
             srcMesh=tempMesh


          else if (lregridmethod .eq. ESMF_REGRIDMETHOD_BILINEAR .or. &
                   lregridmethod .eq. ESMF_REGRIDMETHOD_PATCH) then
               
             if (srcMeshloc .ne. ESMF_MESHLOC_NODE) then
                if (srcMeshloc .eq. ESMF_MESHLOC_ELEMENT) then
                   ! Create a dual of the Mesh
                   srcMeshDual=ESMF_MeshCreateDual(tempMesh, rc=localrc)
                   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                     ESMF_CONTEXT, rcToReturn=rc)) return
                   
                   ! Use the dual as the srcMesh
                   tempMesh=srcMeshDual
                   
                   ! Record that we created the dual
                   srcDual=.true.
                else
                   call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                    msg="- S can currently only do non-conservative  on a mesh built on nodes or elements", & 
                    ESMF_CONTEXT, rcToReturn=rc) 
                   return  
                endif
             endif

             ! Turn on masking
             if (present(srcMaskValues)) then
                call ESMF_MeshTurnOnNodeMask(tempMesh, maskValues=srcMaskValues, rc=localrc);
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
             endif
             srcMesh=tempMesh
          else

             if (srcMeshloc .ne. ESMF_MESHLOC_NODE) then
               if (srcMeshloc .ne. ESMF_MESHLOC_ELEMENT) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- D can currently only do non-conservative  on a mesh built on nodes or elements", &
                 ESMF_CONTEXT, rcToReturn=rc)
                 return
               endif
             endif

             srcPointList=ESMF_PointListCreate(tempMesh, srcMeshloc, &
                                               maskValues=srcMaskValues, &
                                               rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
             src_pl_used=.true.


          endif
        else if (srcgeomtype .eq. ESMF_GEOMTYPE_LOCSTREAM) then

          if (lregridmethod .eq. ESMF_REGRIDMETHOD_BILINEAR .or. &
              lregridmethod .eq. ESMF_REGRIDMETHOD_PATCH    .or. &
              lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE .or. &
              lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
               msg="- only nearest neighbor regridding allowed when using location stream as source", & 
               ESMF_CONTEXT, rcToReturn=rc) 
            return  
          endif

          !extract locstream from srcField, then pass into pointlistcreate
          call ESMF_FieldGet(srcField, locStream=srcLocStream, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return


          srcPointList=ESMF_PointListCreate(srcLocStream, &
                                            maskValues=srcMaskValues, &
                                            rc=localrc)

          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          src_pl_used=.true.


        else
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg="source GEOMTYPE not supported, must be GRID,MESH or LOCSTREAM", &
            ESMF_CONTEXT, rcToReturn=rc)
          return

        endif

        if (dstgeomtype .eq. ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(dstField, grid=dstGrid, &
                 staggerloc=dstStaggerloc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          ! if we're doing conservative then do some checking and
          ! change staggerloc
          if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
               (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
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
                   dimCount=gridDimCount, tileCount=tileCount, &
                   rc=localrc)
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

            ! check grid
            call checkGrid(dstGrid,dstStaggerlocG2M,rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

          
            ! Convert Grid to Mesh
            if (tileCount .eq. 1) then
               dstMesh = ESMF_GridToMesh(dstGrid, dstStaggerLocG2M, dstIsSphere, dstIsLatLonDeg, &
                    maskValues=dstMaskValues, regridConserve=regridConserveG2M, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            else 
               dstMesh = ESMF_GridToMeshCell(dstGrid, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

               ! Turn on masking
               if (present(dstMaskValues)) then
                  call ESMF_MeshTurnOnCellMask(dstMesh, maskValues=dstMaskValues, rc=localrc);
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                       ESMF_CONTEXT, rcToReturn=rc)) return
               endif
            endif
          else 
            ! If we're not conservative, then use the staggerloc that the field is built upon
            dstStaggerlocG2M=dstStaggerloc

            ! check grid
            call checkGridLite(dstGrid,dstStaggerlocG2M,rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            dstPointList=ESMF_PointListCreate(dstGrid,dstStaggerlocG2M, &
                                              maskValues=dstMaskValues, &
                                              rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            dst_pl_used=.true.
          endif

          ! If we're doing creep fill, then also need Mesh
          if (localExtrapMethod .eq. ESMF_EXTRAPMETHOD_CREEP) then
               dstMesh = ESMF_GridToMesh(dstGrid, dstStaggerLocG2M, dstIsSphere, dstIsLatLonDeg, &
                    maskValues=dstMaskValues, regridConserve=regridConserveG2M, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
          endif

        else if (dstgeomtype .eq. ESMF_GEOMTYPE_MESH) then

          call ESMF_FieldGet(dstField, mesh=tempMesh, meshloc=dstMeshloc, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          ! Mesh needs to be built on elements for conservative, and nodes for the others
          if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
               (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
              if (dstMeshloc .ne. ESMF_MESHLOC_ELEMENT) then
                 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                 msg="- can currently only do conservative regridding on a mesh built on elements", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
                return 
              endif

              ! Turn on masking
              if (present(dstMaskValues)) then
                call ESMF_MeshTurnOnCellMask(tempMesh, maskValues=dstMaskValues, rc=localrc);
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              endif
              dstMesh=tempMesh

          else
             if (dstMeshloc .ne. ESMF_MESHLOC_NODE) then
                if (dstMeshloc .ne. ESMF_MESHLOC_ELEMENT) then
                   call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                   msg="- D can currently only do non-conservative  on a mesh built on nodes or elements", & 
                   ESMF_CONTEXT, rcToReturn=rc) 
                   return  
                endif
             endif

             dstPointList=ESMF_PointListCreate(tempMesh, dstMeshloc, &
                                               maskValues=dstMaskValues, &
                                               rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
             dst_pl_used=.true.


             ! Generate Mesh for creep fill extrapolation 
             if (localExtrapMethod .eq. ESMF_EXTRAPMETHOD_CREEP) then
                if (dstMeshloc .ne. ESMF_MESHLOC_NODE) then
                   if (dstMeshloc .eq. ESMF_MESHLOC_ELEMENT) then
                      ! Create a dual of the Mesh
                      dstMeshDual=ESMF_MeshCreateDual(tempMesh, rc=localrc)
                      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                           ESMF_CONTEXT, rcToReturn=rc)) return
                   
                      ! Use the dual as the srcMesh
                      tempMesh=dstMeshDual
                   
                      ! Record that we created the dual
                      dstDual=.true.
                   else
                      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                           msg="- D can currently only do non-conservative  on a mesh built on nodes or elements", & 
                           ESMF_CONTEXT, rcToReturn=rc) 
                      return  
                   endif
                endif

                ! Turn on masking
                if (present(dstMaskValues)) then
                   call ESMF_MeshTurnOnNodeMask(tempMesh, maskValues=dstMaskValues, rc=localrc);
                   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                        ESMF_CONTEXT, rcToReturn=rc)) return
                endif
                dstMesh=tempMesh               
             endif
          endif
          
        else if (dstgeomtype .eq. ESMF_GEOMTYPE_LOCSTREAM) then

           if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
                (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                   msg="- conservative regridding not allowed with location stream as destination", & 
                   ESMF_CONTEXT, rcToReturn=rc) 
              return  
           endif

          !extract locstream from dstField, then pass into pointlistcreate
          call ESMF_FieldGet(dstField, locStream=dstLocStream, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          dstPointList=ESMF_PointListCreate(dstLocStream, &
                                            maskValues=dstMaskValues, &
                                            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          dst_pl_used=.true.

          ! Can't do creep fill on locstream
          if (localExtrapMethod .eq. ESMF_EXTRAPMETHOD_CREEP) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                  msg=" - Creep fill extrapolation is not allowed when destination is a location stream", & 
                  ESMF_CONTEXT, rcToReturn=rc) 
             return  
          endif
        else
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg="destination GEOMTYPE not supported, must be GRID,MESH or LOCSTREAM", &
            ESMF_CONTEXT, rcToReturn=rc)
          return

        endif

        ! At this point, we have the meshes or pointlists, so we are ready to call
        ! the interface of the regrid.

        ! call into the Regrid mesh interface
        if (present(weights) .or. present(factorList) .or. &
            present(indices) .or. present(factorIndexList)) then

            call ESMF_RegridStore(srcMesh, srcArray, srcPointList, src_pl_used, &
                                  dstMesh, dstArray, dstPointList, dst_pl_used, &
                                  lregridmethod, &
                                  localLineType, &
                                  localNormType, &
                                  localpolemethod, localRegridPoleNPnts, &
                                  lregridScheme, &
                                  hasStatusArray, statusArray, &
                                  localExtrapMethod, &
                                  localExtrapNumSrcPnts, &
                                  localExtrapDistExponent, &
                                  localExtrapNumLevels, &
                                  localExtrapNumInputLevels, &
                                  unmappedaction, &
                                  localIgnoreDegenerate, &
                                  srcTermProcessing, &
                                  pipeLineDepth, &
                                  routehandle, &
                                  tmp_indices, tmp_weights, &
                                  unmappedDstList, &
                                  localrc)

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

            call ESMF_RegridStore(srcMesh, srcArray, srcPointList, src_pl_used, &
                                  dstMesh, dstArray, dstPointList, dst_pl_used, &
                                  lregridmethod, &
                                  localLineType, &
                                  localNormType, &
                                  localpolemethod, localRegridPoleNPnts, &
                                  lregridScheme, &
                                  hasStatusArray, statusArray, &
                                  localExtrapMethod, &
                                  localExtrapNumSrcPnts, &
                                  localExtrapDistExponent, &
                                  localExtrapNumLevels, &
                                  localExtrapNumInputLevels, &
                                  unmappedaction, &
                                  localIgnoreDegenerate, &
                                  srcTermProcessing, &
                                  pipeLineDepth, &
                                  routehandle, &
                                  unmappedDstList=unmappedDstList, &
                                  rc=localrc)

           if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (dst_pl_used) then
          call ESMF_PointListDestroy(dstPointList,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if (src_pl_used) then
          call ESMF_PointListDestroy(srcPointList,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif


        ! Get Fraction info
        if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
             (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
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

        ! Clean up Meshes
        if (srcgeomtype .eq. ESMF_GEOMTYPE_GRID) then
          if (.not. src_pl_used) then
            call ESMF_MeshDestroy(srcMesh,rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          endif
           
        else if (srcgeomtype .eq. ESMF_GEOMTYPE_MESH) then
           ! Otherwise reset masking
           if (lregridmethod .ne. ESMF_REGRIDMETHOD_NEAREST_STOD .and. &
               lregridmethod .ne. ESMF_REGRIDMETHOD_NEAREST_DTOS) then

           if (present(srcMaskValues)) then
              if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
                   (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
                 call ESMF_MeshTurnOffCellMask(srcMesh, rc=localrc);
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
              else
                 call ESMF_MeshTurnOffNodeMask(srcMesh, rc=localrc);
                 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
              endif
           endif

           endif

           ! Get rid of dual mesh
           if (srcDual) then
              call ESMF_MeshDestroy(srcMeshDual,rc=localrc)
              if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           endif
        endif

        if (dstgeomtype .eq. ESMF_GEOMTYPE_GRID) then
           if (.not. dst_pl_used) then
             call ESMF_MeshDestroy(dstMesh,rc=localrc)
             if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
           else 
              ! If we're doing creep fill, then also made mesh
              if (localExtrapMethod .eq. ESMF_EXTRAPMETHOD_CREEP) then
                 call ESMF_MeshDestroy(dstMesh,rc=localrc)
                 if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) return
              endif
           endif           
        else if (dstgeomtype .eq. ESMF_GEOMTYPE_MESH) then
           if (.not. dst_pl_used) then

              ! Otherwise reset masking
              if (present(dstMaskValues)) then
                 if ((lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) .or. &
                      (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
                    call ESMF_MeshTurnOffCellMask(dstMesh, rc=localrc);
                    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
                 else
                    call ESMF_MeshTurnOffNodeMask(dstMesh, rc=localrc);
                    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
                 endif
              endif
           else
              ! If we're doing creep fill and made a dual, then also destroy
              if ((localExtrapMethod .eq. ESMF_EXTRAPMETHOD_CREEP) .and. &
                   dstDual) then
                 
                 call ESMF_MeshDestroy(dstMesh,rc=localrc)
                 if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) return
              endif
           endif
        endif

        if(present(rc)) rc = ESMF_SUCCESS

        ! ESMF_METHOD_EXIT(localrc)

!        call ESMF_VMWtime(end_time)
!        print*,'regrid store time= ',end_time-beg_time

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
                    keywordEnforcer, regridmethod, routehandle, &
                    srcFracField, dstFracField, &
                    srcMergeFracField, dstMergeFracField, rc)
!      
! !ARGUMENTS:
      type(ESMF_XGrid),       intent(in)              :: xgrid
      type(ESMF_Field),       intent(in)              :: srcField
      type(ESMF_Field),       intent(inout)           :: dstField
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_RegridMethod_Flag),   intent(in),    optional :: regridmethod
      type(ESMF_RouteHandle), intent(inout), optional :: routehandle
      type(ESMF_Field),       intent(inout), optional :: srcFracField
      type(ESMF_Field),       intent(inout), optional :: dstFracField
      type(ESMF_Field),       intent(inout), optional :: srcMergeFracField
      type(ESMF_Field),       intent(inout), optional :: dstMergeFracField
      integer,                intent(out),   optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.3.0] Added arguments {\tt srcFracField}, {\tt dstFracField}, {\tt srcMergeFracField}, and {\tt dstMergeFracField}.
! These fraction Fields allow a user to calculate correct flux regridded through {\tt ESMF\_XGrid}.
! \item[7.1.0r] Added argument {\tt regridmethod}. This new argument allows the user to choose the regrid method
!               to apply when computing the routehandle. 
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!       \begin{sloppypar}
!       Creates a sparse matrix operation (stored in {\tt routehandle}) that contains the calculations and 
!       communications necessary to interpolate from {\tt srcField} to {\tt dstField}. 
!       The routehandle can then be used in the call
!       {\tt ESMF\_FieldRegrid()} to interpolate between the {\tt ESMF\_Field}s. Information such as
!       index mapping and weights are obtained from the XGrid by matching the Field Grids or Meshes in the XGrid. 
!       It's erroneous to have matching Grid or Mesh objects in the {\tt srcField} and {\tt dstField}. 
!       They must be different in either topological or geometric characteristics. For {\tt ESMF\_Field}s 
!       built on identical {\tt ESMF\_Grid} or {\tt ESMF\_Mesh} on
!       different VM, user can use {\tt ESMF\_FieldRedistStore()} and {\tt ESMF\_FieldRedist()} 
!       methods to communicate data directly without interpolation.
!       \end{sloppypar}
!       
!       The routehandle generated by this call is subsequently computed based on these information.
!       If those information don't change the routehandle can be used repeatedly to interpolate 
!       from the source Field to the destination Field. 
!       This is true even if the data in the Fields changes. The routehandle may also be used to 
!       interpolate between any source and 
!       destination Field which are created on the same stagger location and Grid
!       or on the same mesh location and Mesh as the original Fields.        
!
!       When it's no longer needed the routehandle should be destroyed by using 
!       {\tt ESMF\_FieldRegridRelease()} to free the memory it's using. 
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
!           Destination Field. The data in this Field may be overwritten by this call. 
!     \item [{[regridmethod]}]
!           The type of interpolation. For this method only 
!           {\tt ESMF\_REGRIDMETHOD\_CONSERVE} and {\tt ESMF\_REGRIDMETHOD\_CONSERVE\_2ND} are
!           supported. If not specified, defaults to {\tt ESMF\_REGRIDMETHOD\_CONSERVE}.
!     \item [{[routehandle]}]
!           The handle that implements the regrid and that can be used in later 
!           {\tt ESMF\_FieldRegrid}.
!     \item [{[srcFracField]}] 
!           The fraction of each source cell participating in the regridding returned from this call. 
!           This Field needs to be created on the same Grid and location (e.g staggerloc) 
!           as the srcField.
!     \item [{[dstFracField]}] 
!           The fraction of each destination cell participating in the regridding returned from this call. 
!           This Field needs to be created on the same Grid and location (e.g staggerloc) 
!           as the dstField.
!     \item [{[srcMergeFracField]}] 
!           The fraction of each source cell as a result of Grid merge returned from this call.
!           This Field needs to be created on the same Grid and location (e.g staggerloc) 
!           as the srcField.
!     \item [{[dstMergeFracField]}] 
!           The fraction of each destination cell as a result of Grid merge returned from this call.
!           This Field needs to be created on the same Grid and location (e.g staggerloc) 
!           as the dstField.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc, i

        type(ESMF_GeomType_Flag)  :: geomtype, srcgeomtype, dstgeomtype
        type(ESMF_XGrid)     :: srcXGrid, dstXGrid        
        type(ESMF_Mesh)      :: srcMesh, dstMesh

        integer :: srcIdx, dstIdx, ngrid_a, ngrid_b
        integer :: sideAGC, sideAMC, sideBGC, sideBMC
        type(ESMF_XGridSide_Flag) :: srcSide, dstSide
        type(ESMF_XGridGeomBase), allocatable :: gridA(:), gridB(:)
        type(ESMF_Grid)      :: srcGrid
        type(ESMF_Grid)      :: dstGrid
        type(ESMF_XGridSpec) :: sparseMat
        logical :: found, match
        type(ESMF_Array)     :: srcFracArray
        type(ESMF_Array)     :: dstFracArray
        type(ESMF_STAGGERLOC):: interpFieldStaggerloc, fracFieldStaggerloc
        type(ESMF_MESHLOC)   :: interpFieldMeshloc, fracFieldMeshloc
        integer              :: srcTermProcessingVal
        type(ESMF_RegridMethod_Flag) :: lregridmethod
        type(ESMF_Mesh)      :: superMesh
        type(ESMF_Field)     :: tmpSrcField, tmpDstField
        type(ESMF_Typekind_Flag) :: fieldTypeKind

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Set optional method argument
        if (present(regridmethod)) then
           lregridmethod=regridmethod
        else     
           lregridmethod=ESMF_REGRIDMETHOD_CONSERVE
        endif

        ! Only conservative methods supported for now
        if ((lregridmethod .ne. ESMF_REGRIDMETHOD_CONSERVE) .and. &
             (lregridmethod .ne. ESMF_REGRIDMETHOD_CONSERVE_2ND)) then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                msg="- Only conservative regrid methods supported through XGrid", & 
                ESMF_CONTEXT, rcToReturn=rc) 
           return
        endif

        ! look for the correct Grid to use
        ! first Get necessary information from XGrid and Fields
        call ESMF_XGridGet(xgrid, &
            sideAGridCount=sideAGC, sideAMeshCount=sideAMC, &
            sideBGridCount=sideBGC, sideBMeshCount=sideBMC, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ngrid_a = sideAGC + sideAMC
        ngrid_b = sideBGC + sideBMC
        allocate(gridA(ngrid_a), gridB(ngrid_b))

        call ESMF_XGridGet(xgrid, gridA, gridB, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(srcField, geomtype=geomtype, &
               rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        srcgeomtype = geomtype

        ! locate the Grid or XGrid contained in srcField
        if(geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(srcField, grid=srcGrid, &
                   rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            found = .false.
            do i = 1, ngrid_a
                !if(ESMF_GridMatch(srcGrid, gridA(i)%gbcp%grid, &
                !     globalflag=.true.) >=ESMF_GRIDMATCH_EXACT) then
                if(srcGrid == gridA(i)%gbcp%grid) then
                    srcIdx = i
                    srcSide = ESMF_XGRIDSIDE_A
                    found = .true.
                    exit
                endif
            enddo 
            do i = 1, ngrid_b
                !if(ESMF_GridMatch(srcGrid, gridB(i)%gbcp%grid, &
                !     globalflag=.true.) >=ESMF_GRIDMATCH_EXACT) then
                if(srcGrid == gridB(i)%gbcp%grid) then
                    if(found) then
                      ! TODO: maybe we should attach standard attibute
                      ! to differentiate src and dst side for regridding
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

        else if(geomtype == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldGet(srcField, mesh=srcMesh, &
                   rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            found = .false.
            do i = 1, ngrid_a
                !if(ESMF_MeshMatch(srcMesh, gridA(i)%gbcp%mesh)) then
                if(srcMesh == gridA(i)%gbcp%mesh) then
                    srcIdx = i
                    srcSide = ESMF_XGRIDSIDE_A
                    found = .true.
                    exit
                endif
            enddo 
            do i = 1, ngrid_b
                !if(ESMF_MeshMatch(srcMesh, gridB(i)%gbcp%mesh)) then
                if(srcMesh == gridB(i)%gbcp%mesh) then
                    if(found) then
                      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                        msg="- duplication of Mesh found in XGrid", &
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
                   msg="- cannot Locate src Field Mesh in XGrid", &
                   ESMF_CONTEXT, rcToReturn=rc) 
                return
            endif
                
        else if(geomtype == ESMF_GEOMTYPE_XGRID) then
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
              msg="- src Field is not built on Grid, Mesh, or XGrid", &
              ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif

        ! locate the Grid or XGrid contained in dstField
        call ESMF_FieldGet(dstField, geomtype=geomtype, &
               rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        dstgeomtype = geomtype

        if(geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(dstField, grid=dstGrid, &
                   rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            found = .false.
            do i = 1, ngrid_a
                !if(ESMF_GridMatch(dstGrid, gridA(i)%gbcp%grid, &
                !   globalflag=.true.) >=ESMF_GRIDMATCH_EXACT) then
                if(dstGrid == gridA(i)%gbcp%grid) then
                    dstIdx = i
                    dstSide = ESMF_XGRIDSIDE_A
                    found = .true.
                    exit
                endif
            enddo 
            do i = 1, ngrid_b
                !if(ESMF_GridMatch(dstGrid, gridB(i)%gbcp%grid, &
                !   globalflag=.true.) >=ESMF_GRIDMATCH_EXACT) then
                if(dstGrid == gridB(i)%gbcp%grid) then
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

        else if(geomtype == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldGet(dstField, mesh=dstMesh, &
                   rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

            found = .false.
            do i = 1, ngrid_a
                !if(ESMF_MeshMatch(dstMesh, gridA(i)%gbcp%mesh)) then
                if(dstMesh == gridA(i)%gbcp%mesh) then
                    dstIdx = i
                    dstSide = ESMF_XGRIDSIDE_A
                    found = .true.
                    exit
                endif
            enddo 
            do i = 1, ngrid_b
                !if(ESMF_MeshMatch(dstMesh, gridB(i)%gbcp%mesh)) then
                if(dstMesh == gridB(i)%gbcp%mesh) then
                    if(found) then
                      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
                        msg="- duplication of Mesh found in XGrid", &
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
                   msg="- cannot Locate dst Field Mesh in XGrid", &
                   ESMF_CONTEXT, rcToReturn=rc) 
                return
            endif

        else if(geomtype == ESMF_GEOMTYPE_XGRID) then
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

        ! src and dst Fields should not be on the same side
        if ( srcSide == dstSide ) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
               msg="- src and dst Fields should not be on same side of the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif

        ! retrieve regridding fraction Fields on demand
        if(present(srcFracField)) then
          if(srcgeomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(srcFracField, staggerloc=fracFieldStaggerloc, &
                 array=srcFracArray, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(srcField, staggerloc=interpFieldStaggerloc, &
                 rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
      
            ! Make sure the staggerlocs match
            if (interpFieldStaggerloc .ne. fracFieldStaggerloc) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- fracField Field staggerloc must match interpField staggerloc", &
                 ESMF_CONTEXT, rcToReturn=rc)
               return
            endif
          else if(srcgeomtype == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldGet(srcFracField, meshloc=fracFieldMeshloc, &
                 array=srcFracArray, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(srcField, meshloc=interpFieldMeshloc, &
                 rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
      
            ! Make sure the staggerlocs match
            if (interpFieldMeshloc .ne. fracFieldMeshloc) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- fracField Field Meshloc must match interpField Meshloc", &
                 ESMF_CONTEXT, rcToReturn=rc)
               return
            endif
          else
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="- src Field has unrecognized geom type", &
              ESMF_CONTEXT, rcToReturn=rc)
            return
          endif

          call ESMF_XGridGet(xgrid, srcSide, srcIdx, &
              dstSide, dstIdx, srcFracArray=srcFracArray, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(dstFracField)) then
          if(dstgeomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(dstFracField, staggerloc=fracFieldStaggerloc, &
                 array=dstFracArray, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(dstField, staggerloc=interpFieldStaggerloc, &
                 rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
      
            ! Make sure the staggerlocs match
            if (interpFieldStaggerloc .ne. fracFieldStaggerloc) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- fracField Field staggerloc must match interpField staggerloc", &
                 ESMF_CONTEXT, rcToReturn=rc)
               return
            endif
          else if(dstgeomtype == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldGet(dstFracField, meshloc=fracFieldMeshloc, &
                 array=dstFracArray, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(dstField, meshloc=interpFieldMeshloc, &
                 rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
      
            ! Make sure the staggerlocs match
            if (interpFieldMeshloc .ne. fracFieldMeshloc) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- fracField Field Meshloc must match interpField Meshloc", &
                 ESMF_CONTEXT, rcToReturn=rc)
               return
            endif
          else
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="- dst Field has unrecognized geom type", &
              ESMF_CONTEXT, rcToReturn=rc)
            return
          endif

          call ESMF_XGridGet(xgrid, srcSide, srcIdx, &
              dstSide, dstIdx, dstFracArray=dstFracArray, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! retrieve regridding fraction2 Fields on demand
        if(present(srcMergeFracField)) then
          if(srcgeomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(srcMergeFracField, staggerloc=fracFieldStaggerloc, &
                 array=srcFracArray, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(srcField, staggerloc=interpFieldStaggerloc, &
                 rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
      
            ! Make sure the staggerlocs match
            if (interpFieldStaggerloc .ne. fracFieldStaggerloc) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- fracField Field staggerloc must match interpField staggerloc", &
                 ESMF_CONTEXT, rcToReturn=rc)
               return
            endif
          else if(srcgeomtype == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldGet(srcFracField, meshloc=fracFieldMeshloc, &
                 array=srcFracArray, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(srcField, meshloc=interpFieldMeshloc, &
                 rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
      
            ! Make sure the staggerlocs match
            if (interpFieldMeshloc .ne. fracFieldMeshloc) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- fracField Field Meshloc must match interpField Meshloc", &
                 ESMF_CONTEXT, rcToReturn=rc)
               return
            endif
          else
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="- src Field has unrecognized geom type", &
              ESMF_CONTEXT, rcToReturn=rc)
            return
          endif

          call ESMF_XGridGet(xgrid, srcSide, srcIdx, &
              dstSide, dstIdx, srcFrac2Array=srcFracArray, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(dstMergeFracField)) then
          if(dstgeomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(dstMergeFracField, staggerloc=fracFieldStaggerloc, &
                 array=dstFracArray, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(dstField, staggerloc=interpFieldStaggerloc, &
                 rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
      
            ! Make sure the staggerlocs match
            if (interpFieldStaggerloc .ne. fracFieldStaggerloc) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- fracField Field staggerloc must match interpField staggerloc", &
                 ESMF_CONTEXT, rcToReturn=rc)
               return
            endif
          else if(dstgeomtype == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldGet(dstFracField, meshloc=fracFieldMeshloc, &
                 array=dstFracArray, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(dstField, meshloc=interpFieldMeshloc, &
                 rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return
      
            ! Make sure the staggerlocs match
            if (interpFieldMeshloc .ne. fracFieldMeshloc) then
               call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                 msg="- fracField Field Meshloc must match interpField Meshloc", &
                 ESMF_CONTEXT, rcToReturn=rc)
               return
            endif
          else
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="- dst Field has unrecognized geom type", &
              ESMF_CONTEXT, rcToReturn=rc)
            return
          endif

          call ESMF_XGridGet(xgrid, srcSide, srcIdx, &
              dstSide, dstIdx, dstFrac2Array=dstFracArray, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! Create routehandle based on regrid method
        if (lregridmethod .eq. ESMF_REGRIDMETHOD_CONSERVE) then

           ! retrieve the correct sparseMat structure
           call ESMF_XGridGet(xgrid, srcSide, srcIdx, &
                dstSide, dstIdx, sparseMat=sparseMat, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           
           ! call FieldSMMStore
           srcTermProcessingVal = 0
           call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                sparseMat%factorList, sparseMat%factorIndexList, &
                srcTermProcessing=srcTermProcessingVal, &
                rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        else

           ! Set temporary field for source
           if (srcSide == ESMF_XGRIDSIDE_BALANCED) then

              ! Get Field typekind
              call ESMF_FieldGet(srcField, typekind=fieldTypeKind, &
                   rc=localrc) 
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return

              ! Get Super Mesh
              call ESMF_XGridGet(xgrid, mesh=superMesh, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return

              ! Create temporary field
              tmpSrcField=ESMF_FieldCreate(superMesh, &
                   typekind=fieldTypeKind, &
                   meshloc=ESMF_MESHLOC_ELEMENT, &
                   rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
           else 
              tmpSrcField=srcField
           endif

           ! Set temporary field for dst
           if (dstSide == ESMF_XGRIDSIDE_BALANCED) then

              ! Get Field typekind
              call ESMF_FieldGet(dstField, typekind=fieldTypeKind, &
                   rc=localrc) 
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return

              ! Get Super Mesh
              call ESMF_XGridGet(xgrid, mesh=superMesh, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return

              ! Create temporary field
              tmpDstField=ESMF_FieldCreate(superMesh, &
                   typekind=fieldTypeKind, &
                   meshloc=ESMF_MESHLOC_ELEMENT, &
                   rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
           else 
              tmpDstField=dstField
           endif

           ! Generate routehandle other that 1st order conserve
           srcTermProcessingVal = 1 ! multiply on src, but all adding on dst
           call ESMF_FieldRegridStoreNX(&
                srcField=tmpSrcField, &
                dstField=tmpDstField, &
! ??            srcMaskValues, dstMaskValues, &
                regridmethod=lregridmethod, &
                srcTermProcessing=srcTermProcessingVal, &
                routehandle=routehandle, &
                srcFracField=srcFracField, &
                dstFracField=dstFracField, &
                rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

           ! Get rid of temporary source Field if necessary
           if (srcSide == ESMF_XGRIDSIDE_BALANCED) then

              call ESMF_FieldDestroy(tmpSrcField, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
           endif

           ! Get rid of temporary destination Field if necessary
           if (dstSide == ESMF_XGRIDSIDE_BALANCED) then

              call ESMF_FieldDestroy(tmpDstField, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
           endif

        endif


        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridStoreX


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridGetArea"

!BOP
! !IROUTINE: ESMF_FieldRegridGetArea - Get the area of the cells used for conservative interpolation
!
! !INTERFACE:
      subroutine ESMF_FieldRegridGetArea(areaField, rc)
!
! !RETURN VALUE:
!      
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)                 :: areaField
      integer, intent(out), optional                  :: rc 

!
! !DESCRIPTION:
!     This subroutine gets the area of the cells used for conservative interpolation for the grid object 
!     associated with {\tt areaField} and puts them into {\tt areaField}. If created on a 2D Grid, it must 
!     be built on the {\tt ESMF\_STAGGERLOC\_CENTER} stagger location. 
!     If created on a 3D Grid, it must be built on the {\tt ESMF\_STAGGERLOC\_CENTER\_VCENTER} stagger 
!     location. If created on a Mesh, it must be built on the {\tt ESMF\_MESHLOC\_ELEMENT} mesh location. 
!
!     If the user has set the area in the Grid or Mesh under {\tt areaField}, then that's the area that's
!     returned in the units that the user set it in. If the user hasn't set the area, then the area is 
!     calculated and returned. If the Grid or Mesh is on the surface of a sphere, then the calculated area is in
!     units of square radians. If the Grid or Mesh is 
!     Cartesian, then the calculated area is in square units of whatever unit the coordinates are in. 
!
!     The arguments are:
!     \begin{description}
!     \item [areaField]
!           The Field to put the area values in. 
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc
        integer              :: lregridScheme
        type(ESMF_GeomType_Flag)  :: geomtype

        type(ESMF_Grid)      :: Grid
        type(ESMF_Array)     :: Array
        type(ESMF_Mesh)      :: Mesh
        type(ESMF_StaggerLoc) :: staggerLoc, staggerLocG2M
        type(ESMF_MeshLoc)   :: meshloc
        real(ESMF_KIND_R8), pointer :: areaFptr(:)
        integer :: gridDimCount, localDECount, tileCount
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
            call ESMF_GridGet(grid=Grid, tileCount=tileCount, &
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
          if (tileCount .eq. 1) then
             Mesh = ESMF_GridToMesh(Grid, staggerlocG2M, isSphere, isLatLonDeg, &
                  regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
          else 
             Mesh = ESMF_GridToMeshCell(Grid, rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
          endif

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

    ! Same as checkGrid, but less restrictive due to anticipated conversion to a pointlist
    subroutine checkGridLite(grid,staggerloc,rc)
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

       if(present(rc)) rc = ESMF_SUCCESS
   end subroutine checkGridLite

!------------------------------------------------------------------------------

end module ESMF_FieldRegridMod
