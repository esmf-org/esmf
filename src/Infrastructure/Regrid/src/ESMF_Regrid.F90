! $Id: ESMF_Regrid.F90,v 1.165 2011/06/30 19:22:20 w6ws Exp $
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
      use ESMF_VMMod
      use ESMF_RHandleMod
      use ESMF_UtilTypesMod
      use ESMF_BaseMod          ! ESMF base class
      use ESMF_LogErrMod
      use ESMF_ArrayMod
      use ESMF_F90InterfaceMod
      use ESMF_MeshMod


      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

      ! temporarily store the weights while F90 arrays are alloc'ed
      type ESMF_TempWeights 
      sequence
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
    public operator (==)


! -------------------------- ESMF-public method -------------------------------
!BOPI


!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
         '$Id: ESMF_Regrid.F90,v 1.165 2011/06/30 19:22:20 w6ws Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridMethodEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF RegridMethod.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.ne.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridMethodNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF RegridMethod.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------


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
#define ESMF_METHOD "ESMF_RegridMethodEqual"
!BOPI
! !IROUTINE: ESMF_RegridMethodEqual - Equality of RegridMethods
!
! !INTERFACE:
      function ESMF_RegridMethodEqual(RegridMethod1, RegridMethod2)

! !RETURN VALUE:
      logical :: ESMF_RegridMethodEqual

! !ARGUMENTS:

      type (ESMF_RegridMethod_Flag), intent(in) :: &
         RegridMethod1,      &! Two igrid statuses to compare for
         RegridMethod2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF RegridMethod statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[RegridMethod1, RegridMethod2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_RegridMethodEqual = (RegridMethod1%regridmethod == &
                              RegridMethod2%regridmethod)

      end function ESMF_RegridMethodEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridMethodNotEqual"
!BOPI
! !IROUTINE: ESMF_RegridMethodNotEqual - Non-equality of RegridMethods
!
! !INTERFACE:
      function ESMF_RegridMethodNotEqual(RegridMethod1, RegridMethod2)

! !RETURN VALUE:
      logical :: ESMF_RegridMethodNotEqual

! !ARGUMENTS:

      type (ESMF_RegridMethod_Flag), intent(in) :: &
         RegridMethod1,      &! Two RegridMethod Statuses to compare for
         RegridMethod2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF RegridMethod statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[RegridMethod1, RegridMethod2]
!          Two statuses of RegridMethods to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_RegridMethodNotEqual = (RegridMethod1%regridmethod /= &
                                 RegridMethod2%regridmethod)

      end function ESMF_RegridMethodNotEqual


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridStore"
!BOPI
! !IROUTINE: ESMF_RegridStore - Precomputes Regrid data

! !INTERFACE:
      subroutine ESMF_RegridStore(srcMesh, srcArray, &
                 dstMesh, dstArray, &
                 regridmethod, &
                 polemethod, regridPoleNPnts, &
                 regridScheme, &
                 unmappedaction, routehandle, &
                 indices, weights, &
                 rc)
!
! !ARGUMENTS:
      type(ESMF_Mesh), intent(inout)         :: srcMesh
      type(ESMF_Array), intent(inout)        :: srcArray
      type(ESMF_Mesh), intent(inout)         :: dstMesh
      type(ESMF_Array), intent(inout)        :: dstArray
      type(ESMF_RegridMethod_Flag), intent(in)    :: regridmethod
      type(ESMF_PoleMethod_Flag), intent(in)      :: polemethod
      integer, intent(in)                    :: regridPoleNPnts
      integer, intent(in)                    :: regridScheme
      type(ESMF_UnmappedAction), intent(in), optional :: unmappedaction
      type(ESMF_RouteHandle),  intent(inout), optional :: routehandle
      integer(ESMF_KIND_I4), pointer, optional         :: indices(:,:)
      real(ESMF_KIND_R8), pointer, optional            :: weights(:)
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
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       type(ESMF_VM)        :: vm
       integer :: has_rh, has_iw, nentries
       type(ESMF_TempWeights) :: tweights
       type(ESMF_RegridConserve) :: localregridConserve
       type(ESMF_UnmappedAction) :: localunmappedaction
       logical :: isMemFreed

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

       ! global vm for now
       call ESMF_VMGetGlobal(vm, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       has_rh = 0
       has_iw = 0
       if (present(routehandle)) has_rh = 1
       if (present(indices)) has_iw = 1

       if (present(unmappedaction)) then
          localunmappedaction=unmappedaction
       else
          localunmappedaction=ESMF_UNMAPPEDACTION_ERROR
       endif


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

       ! Make sure the dstMesh has its internal bits in place
       call ESMF_MeshGet(dstMesh, isMemFreed=isMemFreed, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (isMemFreed)  then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- destination Mesh has had its coordinate and connectivity info freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif

       ! Call through to the C++ object that does the work
       call c_ESMC_regrid_create(vm, srcMesh%this, srcArray, &
                   dstMesh%this, dstArray, &
                   regridmethod,  &
                   polemethod, regridPoleNPnts, &    
                   regridScheme, localunmappedaction%unmappedaction, &
                   routehandle, has_rh, has_iw, &
                   nentries, tweights, &
                   localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Now we must allocate the F90 pointers and copy weights
       if (present(indices)) then
         allocate(indices(2,nentries))
         allocate(weights(nentries))

         call c_ESMC_Copy_TempWeights(tweights, indices(1,1), weights(1))

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
       type(ESMF_VM)        :: vm
       logical :: isMemFreed

       ! Logic to determine if valid optional args are passed.  

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! global vm for now
       call ESMF_VMGetGlobal(vm, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

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
       call c_ESMC_regrid_getiwts(vm, Grid, Mesh, Array, staggerLoc, &
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
       type(ESMF_VM)        :: vm
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
       type(ESMF_VM)        :: vm
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
