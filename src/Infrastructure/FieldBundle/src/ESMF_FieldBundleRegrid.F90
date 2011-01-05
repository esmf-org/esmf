! $Id: ESMF_FieldBundleRegrid.F90,v 1.15 2011/01/05 20:05:43 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_FieldBundleRegrid.F90"
!
!   ESMF FieldBundle Communications Regrid module
module ESMF_FieldBundleRegridMod
!
!==============================================================================
!
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldBundleRegridMod - FieldBundleRegrid routines for FieldBundle objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldBundleRegrid} subroutine.
!
!EOPI
!------------------------------------------------------------------------------
! !USES:
    use ESMF_UtilTypesMod
    use ESMF_InitMacrosMod
    use ESMF_LogErrMod
    use ESMF_VMMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod
    use ESMF_FieldRegridMod
    use ESMF_FieldBundleMod
    use ESMF_FieldBundleSMMMod
    use ESMF_RHandleMod
    use ESMF_RegridMod
    use ESMF_GeomBaseMod
    use ESMF_GridMod
    use ESMF_StaggerLocMod    
    use ESMF_FieldSMMMod

    implicit none
    private
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!  <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
    public ESMF_FieldBundleRegridStore
    public ESMF_FieldBundleRegrid
    public ESMF_FieldBundleRegridRelease
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id: ESMF_FieldBundleRegrid.F90,v 1.15 2011/01/05 20:05:43 svasquez Exp $'

!------------------------------------------------------------------------------
contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegrid()"
!BOP
! !IROUTINE: ESMF_FieldBundleRegrid - Execute a FieldBundle regrid operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRegrid(srcFieldBundle, dstFieldBundle, &
         routehandle, zeroflag, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(in),   optional  :: srcFieldBundle
        type(ESMF_FieldBundle), intent(inout),optional  :: dstFieldBundle
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_RegionFlag),  intent(in),   optional  :: zeroflag
        logical,                intent(in),   optional  :: checkflag
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed FieldBundle regrid from {\tt srcFieldBundle} to
!   {\tt dstFieldBundle}. Both {\tt srcFieldBundle} and {\tt dstFieldBundle} must be
!   congruent and typekind conform with the respective FieldBundles used during 
!   {\tt ESMF\_FieldBundleRegridStore()}. Congruent FieldBundles possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the FieldBundles for every DE. For weakly congruent Fields the sizes of the 
!   undistributed dimensions, that vary faster with memory than the first distributed 
!   dimension, are permitted to be different. This means that the same {\tt routehandle} 
!   can be applied to a large class of similar Fields that differ in the number of 
!   elements in the left most undistributed dimensions. 
!   \end{sloppypar}
!
!   It is erroneous to specify the identical FieldBundle object for {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} arguments.
!
!   See {\tt ESMF\_FieldBundleRegridStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [{[srcFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with source data.
!   \item [{[dstFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[zeroflag]}]
!     \begin{sloppypar}
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstFieldBundle} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstFieldBundle} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroflag} to 
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination FieldBundle that will be updated by the sparse matrix
!     multiplication. See section \ref{opt:regionflag} for a complete list of
!     valid settings.
!     \end{sloppypar}
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input FieldBundle pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code
        
        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional FieldBundle args
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        call ESMF_FieldBundleSMM(srcFieldBundle=srcFieldBundle, &
          dstFieldBundle=dstFieldBundle, routehandle=routehandle, &
          zeroflag=zeroflag, checkflag=checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRegrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegridRelease()"
!BOP
! !IROUTINE: ESMF_FieldBundleRegridRelease - Release resources associated with a FieldBundle regrid operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRegridRelease(routehandle, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with a FieldBundle regrid operation. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

        ! Call into the RouteHandle code
        call ESMF_RouteHandleRelease(routehandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRegridRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegridStore()"
!BOP
! !IROUTINE: ESMF_FieldBundleRegridStore - Precompute a FieldBundle regrid operation
!
! !INTERFACE:
    subroutine ESMF_FieldBundleRegridStore(srcFieldBundle, srcMaskValues, &
                                       dstFieldBundle, dstMaskValues, &
                                       unmappedDstAction,              &
                                       regridMethod, &
                                       regridPoleType, regridPoleNPnts, &
                                       regridScheme, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)             :: srcFieldBundle
    integer(ESMF_KIND_I4), intent(in), optional       :: srcMaskValues(:)
    type(ESMF_FieldBundle), intent(inout)             :: dstFieldBundle
    integer(ESMF_KIND_I4), intent(in), optional       :: dstMaskValues(:)
    type(ESMF_UnmappedAction), intent(in), optional   :: unmappedDstAction
    type(ESMF_RegridMethod), intent(in), optional     :: regridMethod
      type(ESMF_RegridPole), intent(in), optional     :: regridPoleType
      integer, intent(in),optional                    :: regridPoleNPnts
    integer, intent(in), optional                     :: regridScheme
    type(ESMF_RouteHandle), intent(inout)             :: routehandle
    integer,                intent(out),     optional :: rc
!
! !DESCRIPTION:
!   Store a FieldBundle regrid operation over the data in {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} pair. 
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleRegrid()} on any FieldBundle pairs that are weakly congruent
!   and typekind conform to the FieldBundle pair used here.
!   Congruency for FieldBundles is
!   given by the congruency of its constituents.
!   Congruent Fields possess matching DistGrids, and the shape of the local
!   array tiles matches between the Fields for every DE. For weakly congruent
!   Fields the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Fields that differ in the number of elements in the left most
!   undistributed dimensions.
!  
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [srcFieldbundle]
!     Source {\tt ESMF\_FieldBundle} containing data to be regridded.
!  \item [{[srcMaskValues]}]
!     List of values that indicate a source point should be masked out. 
!     If not specified, no masking will occur. 
!   \item [dstFieldbundle]
!     Destination {\tt ESMF\_FieldBundle}.
!  \item [{[dstMaskValues]}]
!     List of values that indicate a destination point should be masked out. 
!     If not specified, no masking will occur.
!  \item [{[unmappedDstAction]}]
!    Specifies what should happen if there are destination points that
!    can't be mapped to a source cell. Options are 
!    {\tt ESMF\_UNMAPPEDACTION\_ERROR} or 
!    {\tt ESMF\_UNMAPPEDACTION\_IGNORE}. If not specified, defaults 
!    to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!   \item [{[regridMethod]}]
!     The type of interpolation. Please see Section~\ref{opt:regridmethod} for a list of
!     valid options. If not specified, defaults to {\tt ESMF\_REGRID\_METHOD\_BILINEAR}.
!   \item [{[regridPoleType]}]
!    Which type of artificial pole
!    to construct on the source Grid for regridding. Only valid when {\tt regridScheme} is set to 
!    {\tt ESMF\_REGRID\_SCHEME\_FULL3D}.  Please see Section~\ref{opt:regridpole} for a list of
!    valid options. If not specified, defaults to {\tt ESMF\_REGRIDPOLE\_ALLAVG}. 
!   \item [{[regridPoleNPnts]}]
!    If {\tt regridPoleType} is {\tt ESMF\_REGRIDPOLE\_NPNTAVG}.
!    This parameter indicates how many points should be averaged
!    over. Must be specified if {\tt regridPoleType} is 
!    {\tt ESMF\_REGRIDPOLE\_NPNTAVG}.
!   \item [{[regridScheme]}]
!     Whether to convert to spherical coordinates (ESMF\_REGRID\_SCHEME\_FULL3D), 
!     or to leave in native coordinates (ESMF\_REGRID\_SCHEME\_NATIVE). 
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        ! internal local variables 
        integer                                       :: localrc, i, localDeCount, fieldCount
        integer                                       :: rraShift, vectorLengthShift 
        type(ESMF_Field)                              :: srcField, dstField
        type(ESMF_RouteHandle)                        :: rh
        logical         :: havePrev, matchesPrev, isGridPair
        type(ESMF_Grid) :: prevSrcGrid, prevDstGrid
        type(ESMF_StaggerLoc) :: prevSrcStaggerLoc, prevDstStaggerLoc
        type(ESMF_Grid) :: currSrcGrid, currDstGrid
        type(ESMF_StaggerLoc) :: currSrcStaggerLoc, currDstStaggerLoc
        integer(ESMF_KIND_I4), pointer :: tmp_indicies(:,:)
        integer(ESMF_KIND_I4), pointer :: prev_indicies(:,:)
        real(ESMF_KIND_R8), pointer :: prev_weights(:)
        type(ESMF_GeomType) :: srcGeomtype        
        type(ESMF_GeomType) :: dstGeomtype        
        integer :: j

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        if(srcFieldBundle%btypep%field_count .ne. dstFieldBundle%btypep%field_count) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               "src and dst FieldBundle must have same number of Fields", &
                ESMF_CONTEXT, rc)
            return
        endif 

        ! init some variables for optimization
        havePrev=.false.
        matchesPrev=.false.

        fieldCount = srcFieldBundle%btypep%field_count

        routehandle = ESMF_RouteHandleCreate(rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_RouteHandlePrepXXE(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! 6) loop over all Fields in FieldBundles, call FieldRegridStore and append rh
        rraShift = 0          ! reset
        vectorLengthShift = 0 ! reset
        do i=1, fieldCount
      
          ! obtain srcField
          call ESMF_FieldBundleGet(srcFieldBundle, fieldIndex=i, field=srcField, &
            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          
          ! obtain dstField
          call ESMF_FieldBundleGet(dstFieldBundle, fieldIndex=i, field=dstField, &
            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          
          ! If these are both Grids, then check for optimization
          call ESMF_FieldGet(srcField, geomType=srcGeomType, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_FieldGet(dstField, geomType=dstGeomType, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

          isGridPair=.false.
          if ((srcGeomType==ESMF_GEOMTYPE_GRID) .and. (dstGeomType==ESMF_GEOMTYPE_GRID)) then
             ! Mark that this is a pair of grids and therefore optimizable
             isGridPair=.true.

             ! Get Grids and staggerlocs
             call ESMF_FieldGet(srcField, grid=currSrcGrid, staggerloc=currSrcStaggerloc, rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

             call ESMF_FieldGet(dstField, grid=currDstGrid, staggerloc=currDstStaggerloc, rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

             ! see if grids match prev grids
             matchesPrev=.false.
             if (havePrev) then
                if ((currSrcStaggerLoc==prevSrcStaggerLoc) .and.  &
                    (currDstStaggerLoc==prevDstStaggerLoc)) then 

                   if (ESMF_GridMatch(currSrcGrid, prevSrcGrid) .and. &
                       ESMF_GridMatch(currDstGrid, prevDstGrid)) then
                      matchesPrev=.true.
                   endif
                endif
             endif
          endif

          ! precompute regrid operation based on grids, etc.
          if (isGridPair) then
             if (matchesPrev) then
                call ESMF_FieldSMMStore(srcField=srcField, dstField=dstField, & 
                        routehandle=rh, &
                        factorList=prev_weights, factorIndexList=prev_indicies, &
                        rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                     ESMF_CONTEXT, rcToReturn=rc)) return
             else ! If it doesn't match make a new previous
                ! if we have them, get rid of old matrix
                if (havePrev) then
                   deallocate(prev_indicies)
                   deallocate(prev_weights)
                endif

                ! Get routeHandle as well as matrix info
                call ESMF_FieldRegridStore(srcField=srcField, srcMaskValues=srcMaskValues, &
                     dstField=dstField, dstMaskValues=dstMaskValues, &
                     unmappedDstAction=unmappedDstAction, &
                     routehandle=rh, &
                     indicies=tmp_indicies, weights=prev_weights, &
                     regridMethod=regridMethod, &
                     regridPoleType=regridPoleType, regridPoleNPnts=regridPoleNPnts, &
                     regridScheme=regridScheme, &
                     rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                     ESMF_CONTEXT, rcToReturn=rc)) return

                ! Swap order
                ! TODO: fix order in regrid
                allocate(prev_indicies(2,size(tmp_indicies,1)))
                do j=1,size(tmp_indicies,1)
                   prev_indicies(1,j)=tmp_indicies(j,1)
                   prev_indicies(2,j)=tmp_indicies(j,2)
                enddo
                deallocate(tmp_indicies)

                ! Mark as prev and record info about prev
                havePrev=.true.
                prevSrcStaggerLoc=currSrcStaggerLoc
                prevDstStaggerLoc=currDstStaggerLoc
                prevSrcGrid=currSrcGrid 
                prevDstGrid=currDstGrid 
             endif
          else ! If not a grid pair no optimization at this point         
             call ESMF_FieldRegridStore(srcField=srcField, srcMaskValues=srcMaskValues, &
                  dstField=dstField, dstMaskValues=dstMaskValues, &
                  unmappedDstAction=unmappedDstAction, &
                  routehandle=rh, regridMethod=regridMethod, &
                  regridPoleType=regridPoleType, regridPoleNPnts=regridPoleNPnts, &
                  regridScheme=regridScheme, &
                  rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
           endif

          ! append rh to routehandle and clear rh
          call ESMF_RouteHandleAppendClear(routehandle, appendRoutehandle=rh, &
            rraShift=rraShift, vectorLengthShift=vectorLengthShift, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
          ! adjust rraShift and vectorLengthShift
          call ESMF_FieldGet(srcField, localDeCount=localDeCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          rraShift = rraShift + localDeCount
          call ESMF_FieldGet(dstField, localDeCount=localDeCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          rraShift = rraShift + localDeCount
          vectorLengthShift = vectorLengthShift + 1
        enddo

        ! if we have them, get rid of old matrix
        if (havePrev) then
           deallocate(prev_indicies)
           deallocate(prev_weights)
        endif

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRegridStore
!------------------------------------------------------------------------------ 

end module
