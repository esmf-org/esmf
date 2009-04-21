! $Id: ESMF_FieldRegrid.F90,v 1.17 2009/04/21 22:51:44 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
  use ESMF_GridMod
  use ESMF_GridUtilMod
  use ESMF_StaggerLocMod
  use ESMF_MeshMod
  use ESMF_RHandleMod
  use ESMF_GeomBaseMod
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_FieldGetMod

  
  implicit none

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_FieldRegridStore        ! Store a regrid matrix
   public ESMF_FieldRegridRun          ! apply a regrid operator
   public ESMF_FieldRegrid             ! apply a regrid operator
   public ESMF_FieldRegridRelease      ! apply a regrid operator

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_FieldRegrid -- Generic interface

! !INTERFACE:
  interface ESMF_FieldRegrid

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_FieldRegridRun
!EOPI

  end interface


! -------------------------- ESMF-public method -------------------------------


!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_FieldRegrid.F90,v 1.17 2009/04/21 22:51:44 oehmke Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridRun"

!BOP
! !IROUTINE: ESMF_FieldRegrid - Apply the regrid operator
!
! !INTERFACE:
  !   Private name; call using ESMF_FieldRegrid()
      subroutine ESMF_FieldRegridRun(srcField, dstField, &
                      routeHandle, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)     :: srcField
      type(ESMF_Field), intent(inout)     :: dstField
      type(ESMF_RouteHandle), intent(inout)  :: routeHandle
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!       Applys the regrid operation.
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           Source Field.
!     \item [dstField]
!           Destination Field.
!     \item [routeHandle]
!           The regrid operator.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc
        type(ESMF_Array)     :: srcArray
        type(ESMF_Array)     :: dstArray

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Now we go through the painful process of extracting the data members
        ! that we need.
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySMM(srcArray=srcArray, dstArray=dstArray, &
                   routehandle=routeHandle, rc=localrc)

        if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridRun
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridRelease"

!BOP
! !IROUTINE: ESMF_FieldRegridRelease - Free resources used by regrid object
!
! !INTERFACE:
      subroutine ESMF_FieldRegridRelease(routeHandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout)  :: routeHandle
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Free resources used by regrid objec
!
!     The arguments are:
!     \begin{description}
!     \item [routeHandle]
!           Handle carrying the sparse matrix
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc

        call ESMF_RouteHandleRelease(routehandle=routeHandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridRelease
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridStore"

!BOP
! !IROUTINE: ESMF_FieldRegridStore - Store regrid and return RouteHandle and weights
!
! !INTERFACE:
  !   Private name; call using ESMF_FieldRegridStore()
      subroutine ESMF_FieldRegridStore(srcField, dstField, dstMaskValues, &
                   routeHandle, indicies, weights, regridMethod, &
                   regridScheme, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)                 :: srcField
      type(ESMF_Field), intent(inout)                 :: dstField
      integer(ESMF_KIND_I4), intent(inout), optional  :: dstMaskValues(:)
      type(ESMF_RouteHandle), intent(inout), optional :: routeHandle
      integer(ESMF_KIND_I4), pointer, optional        :: indicies(:,:)
      real(ESMF_KIND_R8), pointer, optional           :: weights(:)
      type(ESMF_RegridMethod), intent(in)             :: regridMethod
      integer, intent(in), optional                   :: regridScheme
      integer, intent(out), optional                  :: rc 
!
! !DESCRIPTION:
!       Creates a sparse matrix (stored in routehandle) that regrids
!       from src to dst Field.
!
!     The arguments are:
!     \begin{description}
!     \item [srcField]
!           Source Field.
!     \item [dstField]
!           Destination Field.
!     \item [{[dstMaskValues]}]
!           List of values that indicate a destination point should be masked out. 
!           If not specified, no masking will occur. 
!     \item [{[routeHandle]}]
!           The handle that implements the regrid and that can be used in later 
!           {\tt ESMF\_FieldRegrid}.
!     \item [{[indices]}] 
!           The indices for the sparse matrix.
!     \item [{[weights]}] 
!           The weights for the sparse matrix.
!     \item [{[regridMethod]}]
!           The type of regrid. Options are 
!           {\tt ESMF\_REGRID\_METHOD\_BILINEAR} or 
!           {\tt ESMF\_REGRID\_METHOD\_PATCH}. If not specified, defaults 
!           to {\tt ESMF\_REGRID\_METHOD\_BILINEAR}.
!     \item [{[regridScheme]}]
!           Whether to convert to spherical coordinates 
!           ({\tt ESMF\_REGRID\_SCHEME\_FULL3D}), 
!           or to leave in native coordinates 
!           ({\tt ESMF\_REGRID\_SCHEME\_NATIVE}). 
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc
        integer              :: lregridScheme
        integer              :: isSphere
        type(ESMF_GeomType)  :: srcgeomtype
        type(ESMF_GeomType)  :: dstgeomtype

        type(ESMF_Grid)      :: srcGrid
        type(ESMF_Grid)      :: dstGrid
        type(ESMF_Array)     :: srcArray
        type(ESMF_Array)     :: dstArray
        type(ESMF_VM)        :: vm
        type(ESMF_Mesh)      :: srcMesh
        type(ESMF_Mesh)      :: dstMesh
        type(ESMF_StaggerLoc) :: staggerLoc

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
        if (present(rc)) rc = ESMF_RC_NOT_IMPL


        ! global vm for now
        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! Now we go through the painful process of extracting the data members
        ! that we need.
        call ESMF_FieldGet(srcField, geomtype=srcgeomtype, array=srcArray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(dstField, geomtype=dstgeomtype, array=dstArray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! Determine the stagger as function of method (may change in future)
        if (regridMethod .eq. ESMF_REGRID_METHOD_BILINEAR) then
            staggerLoc = ESMF_STAGGERLOC_CENTER
          elseif (regridMethod .eq. ESMF_REGRID_METHOD_PATCH) then
            staggerLoc = ESMF_STAGGERLOC_CENTER
          else
            staggerLoc = ESMF_STAGGERLOC_CENTER
        endif

        ! Will eventually determine scheme either as a parameter or from properties
        ! of the source grid
        if (present(regridScheme)) then
          lregridScheme = regridScheme
        else
          lregridScheme = ESMF_REGRID_SCHEME_NATIVE
        endif


        ! If grids, then convert to a mesh to do the regridding
        if (srcgeomtype .eq. ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(srcField, grid=srcGrid, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          isSphere = 0
          if (lregridScheme .eq. ESMF_REGRID_SCHEME_FULL3D) isSphere = 1

          srcMesh = ESMF_GridToMesh(srcGrid, staggerLoc%staggerloc, isSphere, &
                      rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        else
          call ESMF_FieldGet(srcField, mesh=srcMesh, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (dstgeomtype .eq. ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(dstField, grid=dstGrid, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

          isSphere = 0
          if (lregridScheme .eq. ESMF_REGRID_SCHEME_FULL3D) isSphere = 1

          dstMesh = ESMF_GridToMesh(dstGrid, staggerLoc%staggerloc, isSphere, &
                      dstMaskValues, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_FieldGet(dstField, mesh=dstMesh, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! At this point, we have the meshes, so we are ready to call
        ! the 'mesh only' interface of the regrid.


        ! call into the Regrid mesh interface
        call ESMF_RegridStore(srcMesh, srcArray, dstMesh, dstArray, &
              regridMethod, lregridScheme, routeHandle, &
              indicies, weights, localrc)

        if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return


        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridStore

!------------------------------------------------------------------------------

end module ESMF_FieldRegridMod
