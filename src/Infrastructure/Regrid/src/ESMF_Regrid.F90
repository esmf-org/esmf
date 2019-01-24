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
!
#define ESMF_FILENAME "ESMF_Regrid.F90"
!
!     ESMF  Regrid Module
      module ESMF_RegridMod
!
!==============================================================================
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_RegridMod - Regridding and interpolation
!
! !DESCRIPTION:
!
! The code in this file interfaces most of the Regrid class methods.  Regrid 
! is responsible for any regridding and interpolation required for ESMF 
! applications.
! Regridding includes any process that transforms a field from one ESMF
! igrid to another, including:
! \begin{itemize}
! \item bilinear or patch-recovery interpolation
! \end{itemize}
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_GridMod
      use ESMF_StaggerLocMod
      use ESMF_RHandleMod
      use ESMF_UtilTypesMod
      use ESMF_BaseMod          ! ESMF base class
      use ESMF_LogErrMod
      use ESMF_ArrayMod
      use ESMF_F90InterfaceMod
      use ESMF_MeshMod
      use ESMF_PointListMod

       implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

      ! temporarily store the weights while F90 arrays are alloc'ed
      type ESMF_TempWeights 
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
        type(ESMF_Pointer) :: this
      end type

      type ESMF_TempUDL 
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
        type(ESMF_Pointer) :: this
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!
!------------------------------------------------------------------------------


!
! !PUBLIC MEMBER FUNCTIONS:
!

    ! These are wrapper routines which call RegridStore to do the
    !  actual work.  Since all our routines are data centric methods
    !  and we are not exposing an externally visible "regrid" object, 
    !  these routines must exist to be consistent with the other interfaces.  
    ! 
    public ESMF_RegridStore
    public ESMF_RegridGetIwts
    public ESMF_RegridGetArea
    public ESMF_RegridGetFrac


! -------------------------- ESMF-public method -------------------------------
!BOPI


!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
          '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!==============================================================================

      contains

function my_xor(a, b)
    logical                                       :: my_xor
    logical,                intent(in)         :: a
    logical,                intent(in)         :: b

    if (a .and. b) then
      my_xor = .false.
      return
    endif

    if (.not.(a .or. b)) then
      my_xor = .false.
      return
    endif

    my_xor = .true.

end function my_xor


!==============================================================================
!
! This section includes the Regrid Create, Run, and Destroy methods.
! 
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridStore"
!BOPI
! !IROUTINE: ESMF_RegridStore - Precomputes Regrid data

! !INTERFACE:
      subroutine ESMF_RegridStore(srcMesh, srcArray, srcPointList, src_pl_used, &
                 dstMesh, dstArray, dstPointList, dst_pl_used, &
                 regridmethod, &
                 lineType, &
                 normType, &
                  polemethod, regridPoleNPnts, &
                 regridScheme, &
                 hasStatusArray, &
                 statusArray, &
                 extrapMethod, &
                 extrapNumSrcPnts, &
                 extrapDistExponent, &
                 extrapNumLevels, &
                 extrapNumInputLevels, &
                 unmappedaction, &
                 ignoreDegenerate, &
                 srcTermProcessing, &
                 pipelineDepth, &
                 routehandle, &
                 indices, weights, &
                 unmappedDstList, &
                 rc)
!
! !ARGUMENTS:
      type(ESMF_Mesh), intent(inout)         :: srcMesh
      type(ESMF_Array), intent(inout)        :: srcArray
      type(ESMF_PointList), intent(inout)    :: srcPointList
      logical, intent(in)                    :: src_pl_used
      type(ESMF_Mesh), intent(inout)         :: dstMesh
      type(ESMF_Array), intent(inout)        :: dstArray
      type(ESMF_PointList), intent(inout)    :: dstPointList
      logical, intent(in)                    :: dst_pl_used
      type(ESMF_RegridMethod_Flag), intent(in)    :: regridmethod
      type(ESMF_LineType_Flag), intent(in)    :: lineType
      type(ESMF_NormType_Flag), intent(in)    :: normType
      type(ESMF_PoleMethod_Flag), intent(in)      :: polemethod
      integer, intent(in)                    :: regridPoleNPnts
      integer, intent(in)                    :: regridScheme
      type(ESMF_ExtrapMethod_Flag),   intent(in) :: extrapMethod
      integer, intent(in)                    :: extrapNumSrcPnts
      real(ESMF_KIND_R8)                     :: extrapDistExponent
      integer, intent(in)                    :: extrapNumLevels
      integer, intent(in)                    :: extrapNumInputLevels
      type(ESMF_UnmappedAction_Flag), intent(in), optional :: unmappedaction
      logical, intent(in)                              :: ignoreDegenerate
      integer,                       intent(inout), optional :: srcTermProcessing
      integer,                       intent(inout), optional :: pipelineDepth
      type(ESMF_RouteHandle),  intent(inout), optional :: routehandle
      integer(ESMF_KIND_I4), pointer, optional         :: indices(:,:)
      real(ESMF_KIND_R8), pointer, optional            :: weights(:)
      integer(ESMF_KIND_I4),       pointer, optional   :: unmappedDstList(:)
      logical                     :: hasStatusArray
      type(ESMF_Array)            :: statusArray
      integer,                  intent(  out), optional :: rc
!
! !DESCRIPTION:
 !     The arguments are:
!     \begin{description}
 !     \item[srcGrid]
 !          The source grid.
!     \item[srcArray]
!          The source grid array.
!     \item[dstGrid]
!          The destination grid.
!     \item[dstArray]
!          The destination array.
!     \item[regridmethod]
!          The interpolation method to use.
!     \item[regridScheme]
!          Whether to use 3d or native coordinates
 !     \item [{[regridConserve]}]
!           Specifies whether to implement the mass conservation 
!           correction or not.  Options are 
!           {\tt ESMF\_REGRID_CONSERVE\_OFF} or 
!           {\tt ESMF\_REGRID_CONSERVE\_ON}. If not specified, defaults 
!           to {\tt ESMF\_REGRID_CONSERVE\_OFF}. 
!     \item [{[unmappedaction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Options are 
!           {\tt ESMF\_UNMAPPEDACTION\_ERROR} or 
!           {\tt ESMF\_UNMAPPEDACTION\_IGNORE}. If not specified, defaults 
!           to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!     \item[routeHandle]
!          Handle to store the resulting sparseMatrix
!     \item [{[unmappedDstList]}] 
!           The list of the sequence indices for locations in {\tt dstField} which couldn't be mapped the {\tt srcField}. 
!           The list on each PET only contains the unmapped locations for the piece of the {\tt dstField} on that PET. 
!           If a destination point is masked, it won't be put in this list. 
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       integer :: has_rh, has_iw, nentries
       type(ESMF_TempWeights) :: tweights
       integer :: has_udl, num_udl
       type(ESMF_TempUDL) :: tudl
       type(ESMF_RegridConserve) :: localregridConserve
       type(ESMF_UnmappedAction_Flag) :: localunmappedaction
       logical :: isMemFreed
       integer :: localIgnoreDegenerate
       integer :: src_pl_used_int, dst_pl_used_int
       integer ::  has_statusArrayInt



       ! Logic to determine if valid optional args are passed.  

        ! First thing to check is that indices <=> weights
        if (my_xor(present(indices), present(weights))) then
         localrc = ESMF_RC_ARG_BAD
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
       endif

       ! Next, we require that the user request at least something
       if (.not.(present(routehandle) .or. present(indices))) then
         localrc = ESMF_RC_ARG_BAD
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
       endif

       ! **************************************************
        ! Tests passed, so proceed

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       has_rh = 0
        has_iw = 0
       has_udl=0
       if (present(routehandle)) has_rh = 1
       if (present(indices)) has_iw = 1
       if (present(unmappedDstList)) has_udl = 1

       if (present(unmappedaction)) then
          localunmappedaction=unmappedaction
       else
          localunmappedaction=ESMF_UNMAPPEDACTION_ERROR
       endif

       if (ignoreDegenerate) then
          localIgnoreDegenerate=1
       else
          localIgnoreDegenerate=0
       endif

       if (.not. src_pl_used) then
         ! Make sure the srcMesh has its internal bits in place
         call ESMF_MeshGet(srcMesh, isMemFreed=isMemFreed, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

          if (isMemFreed)  then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                   msg="- source Mesh has had its coordinate and connectivity info freed", & 
                   ESMF_CONTEXT, rcToReturn=rc) 
             return 
         endif
       endif


       if (.not. dst_pl_used) then
         ! Make sure the dstMesh has its internal bits in place
         call ESMF_MeshGet(dstMesh, isMemFreed=isMemFreed, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

          if (isMemFreed)  then
              call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                   msg="- dest Mesh has had its coordinate and connectivity info freed", & 
                   ESMF_CONTEXT, rcToReturn=rc) 
             return 
         endif
       endif

       ! Make used ints
       src_pl_used_int=0
       if (src_pl_used) then
          src_pl_used_int=1
       endif

       dst_pl_used_int=0
       if (dst_pl_used) then
          dst_pl_used_int=1
       endif

       ! Get statusArray if present and set appropriate flag
       has_statusArrayInt=0
       if (hasStatusArray) then
          has_statusArrayInt=1
       endif


        ! Call through to the C++ object that does the work
        call c_ESMC_regrid_create(srcMesh%this, srcArray, srcPointList, src_pl_used_int, &
                   dstMesh%this, dstArray, dstPointList, dst_pl_used_int, &
                   regridmethod,  &
                   lineType, &
                   normType, &
                   polemethod, regridPoleNPnts, &    
                   regridScheme, &
                   extrapMethod, &
                   extrapNumSrcPnts, &
                   extrapDistExponent, &
                   extrapNumLevels, &
                   extrapNumInputLevels, &
                   localunmappedaction%unmappedaction, &
                   localIgnoreDegenerate, &
                   srcTermProcessing, pipelineDepth, &
                   routehandle, has_rh, has_iw, &
                   nentries, tweights, &
                   has_udl, num_udl, tudl, &
                   has_statusArrayInt, statusArray, &
                   localrc)

       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
         
#ifdef C_SIDE_REGRID_FREE_MESH
! enabling this freature currently breaks several tests
       ! Mark Meshes as CMemFreed
       call ESMF_MeshSetIsCMeshFreed(srcMesh, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_MeshSetIsCMeshFreed(dstMesh, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
#endif
       ! Now we must allocate the F90 pointers and copy weights
       if (present(indices)) then
         allocate(indices(2,nentries))
         allocate(weights(nentries))

         if (nentries > 0)  then
            call c_ESMC_Copy_TempWeights(tweights, indices(1,1), weights(1))
         endif
       endif

       ! If unmappedDstList is present then we must allocate the F90 pointers and copy 
       if (present(unmappedDstList)) then
         allocate(unmappedDstList(num_udl))
         call c_ESMC_Copy_TempUDL(num_udl, tudl, unmappedDstList(1))
       endif

       ! Mark route handle created
      if (present(routeHandle)) then 
        call ESMF_RouteHandleSetInitCreated(routeHandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      rc = ESMF_SUCCESS
       end subroutine ESMF_RegridStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridGetIwts"
!BOPI
! !IROUTINE: ESMF_RegridGetIwts - Gets the integration weights

! !INTERFACE:
      subroutine ESMF_RegridGetIwts(Grid, Mesh, Array, staggerLoc, &
                 regridScheme, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout)         :: Grid
      type(ESMF_Mesh), intent(inout)         :: Mesh
      type(ESMF_Array), intent(inout)        :: Array
      type(ESMF_StaggerLoc), intent(in)      :: staggerLoc
      integer, intent(in)                    :: regridScheme
      integer, intent(out), optional         :: rc
!
! !DESCRIPTION:
!     The arguments are:
!     \begin{description}
!     \item[Mesh]
!          The mesh.
!     \item[Array]
!          The grid array.
!     \item[regridScheme]
!          Whether to use 3d or native coordinates
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       logical :: isMemFreed

       ! Logic to determine if valid optional args are passed.  

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Make sure the srcMesh has its internal bits in place
       call ESMF_MeshGet(Mesh, isMemFreed=isMemFreed, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (isMemFreed)  then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- Mesh has had its coordinate and connectivity info freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif

       ! Call through to the C++ object that does the work
       call c_ESMC_regrid_getiwts(Grid, Mesh, Array, staggerLoc, &
                                  regridScheme, localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      rc = ESMF_SUCCESS

      end subroutine ESMF_RegridGetIwts



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridGetArea"
!BOPI
! !IROUTINE: ESMF_RegridGetArea - Gets the area of grid cells

! !INTERFACE:
      subroutine ESMF_RegridGetArea(Grid, Mesh, Array, staggerLoc, &
                 regridScheme, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout)         :: Grid
      type(ESMF_Mesh), intent(inout)         :: Mesh
      type(ESMF_Array), intent(inout)        :: Array
      type(ESMF_StaggerLoc), intent(in)      :: staggerLoc
      integer, intent(in)                    :: regridScheme
      integer, intent(out), optional         :: rc
!
! !DESCRIPTION:
!     The arguments are:
!     \begin{description}
!     \item[Mesh]
!          The mesh.
!     \item[Array]
!          The grid array.
!     \item[regridScheme]
!          Whether to use 3d or native coordinates
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       logical :: isMemFreed

       ! Logic to determine if valid optional args are passed.  

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Make sure the srcMesh has its internal bits in place
       call ESMF_MeshGet(Mesh, isMemFreed=isMemFreed, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (isMemFreed)  then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- Mesh has had its coordinate and connectivity info freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif


       ! Call through to the C++ object that does the work
       call c_ESMC_regrid_getarea(Grid, Mesh, Array, staggerLoc, &
                                  regridScheme, localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


      rc = ESMF_SUCCESS

      end subroutine ESMF_RegridGetArea


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridGetFrac"
!BOPI
! !IROUTINE: ESMF_RegridGetArea - Gets the frac of grid cells after a regrid from a Mesh

! !INTERFACE:
      subroutine ESMF_RegridGetFrac(Grid, Mesh, Array, staggerLoc, &
                 rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout)         :: Grid
      type(ESMF_Mesh), intent(inout)         :: Mesh
      type(ESMF_Array), intent(inout)        :: Array
      type(ESMF_StaggerLoc), intent(in)      :: staggerLoc
      integer, intent(out), optional         :: rc
!
! !DESCRIPTION:
!     The arguments are:
!     \begin{description}
!     \item[Mesh]
!          The mesh.
!     \item[Array]
!          The grid array.
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       logical :: isMemFreed

       ! Logic to determine if valid optional args are passed.  

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Make sure the srcMesh has its internal bits in place
       call ESMF_MeshGet(Mesh, isMemFreed=isMemFreed, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (isMemFreed)  then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- Mesh has had its coordinate and connectivity info freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif

       ! Call through to the C++ object that does the work
       call c_ESMC_regrid_getfrac(Grid, Mesh, Array, staggerLoc, &
                                  localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      rc = ESMF_SUCCESS

      end subroutine ESMF_RegridGetFrac




   end module ESMF_RegridMod
