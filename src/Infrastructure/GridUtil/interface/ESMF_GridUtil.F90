! $Id: ESMF_GridUtil.F90,v 1.13.2.3 2010/03/12 04:35:15 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_GridUtil.F90"
!==============================================================================
!
! ESMF GridUtil
module ESMF_GridUtilMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the Mesh class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_MeshMod
!

!   F90 API wrapper of C++ implemenation of Mesh
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_VMMod
  use ESMF_DELayoutMod
  use ESMF_DistGridMod
  use ESMF_RHandleMod
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  use ESMF_GridMod
  use ESMF_StaggerLocMod
  use ESMF_ArrayMod
  use ESMF_MeshMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_MeshIO
  public ESMF_GridToMesh

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_GridUtil.F90,v 1.13.2.3 2010/03/12 04:35:15 oehmke Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

      contains
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshIO()"
!BOPI
! !IROUTINE: ESMF_MeshIO -- Use mesh to write arrays, since it can
!
! !INTERFACE:
    subroutine ESMF_MeshIO(vm, grid, staggerLoc, filename, &
                         array1, array2, array3, array4, array5, array6, &
                         spherical, rc)
!
!
! !ARGUMENTS:
    type(ESMF_VM), intent(inout)                  :: vm
    type(ESMF_Grid), intent(inout)                :: grid
    type(ESMF_StaggerLoc), intent(in)             :: staggerLoc
    character(len = *), intent(in)                :: filename 
    type(ESMF_Array), intent(inout), optional     :: array1
    type(ESMF_Array), intent(inout), optional     :: array2
    type(ESMF_Array), intent(inout), optional     :: array3
    type(ESMF_Array), intent(inout), optional     :: array4
    type(ESMF_Array), intent(inout), optional     :: array5
    type(ESMF_Array), intent(inout), optional     :: array6
    integer,                intent(in), optional  :: spherical
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Write the fields out for purview.  This function overcomes certain
!   difficulties in passing strings with an optional number of arguments.
!
!   \begin{description}
!   \item [vm]
!         Virutal machine
!   \item[staggerLoc] 
!         stagger of field
!   \item[filename]
!         File (stub) to write results to
!   \item [{[array1-6]}]
!         Arrays to write as data
!   \item [{[spherical]}]
!         If == 1, then grid will be transformed to a 3d spherical manifold
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: minidx
    integer                 :: lspherical

    ! initialize return code; assume routine not implemented
    localrc = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    lspherical = 0
    if (present(spherical)) lspherical = spherical

    minidx = 10

    if (.not. present(array6)) minidx = 6
    if (.not. present(array5)) minidx = 5
    if (.not. present(array4)) minidx = 4
    if (.not. present(array3)) minidx = 3
    if (.not. present(array2)) minidx = 2
    if (.not. present(array1)) minidx = 1

    select case (minidx)
      case (1)
        return 
      case (2)
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 1, filename, localrc, &
             array1, array1, array1, array1, array1, array1, lspherical)
      case (3) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 2, filename, localrc, &
             array1, array2, array1, array1, array1, array1, lspherical)
      case (4) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 3, filename, localrc, &
             array1, array2, array3, array1, array1, array1, lspherical)
      case (5) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 4, filename, localrc, &
             array1, array2, array3, array4, array1, array1, lspherical)
      case (6) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 5, filename, localrc, &
             array1, array2, array3, array4, array5, array1, lspherical)
      case (10) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 6, filename, localrc, &
             array1, array2, array3, array4, array5, array6, lspherical)
      case default
        localrc = ESMF_RC_NOT_IMPL
    end select

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_MeshIO
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridToMesh()"
!BOPI
! !IROUTINE: ESMF_GridToMesh -- return a mesh with same topo as mesh
!
! !INTERFACE:
   function ESMF_GridToMesh(grid, staggerLoc, isSphere, maskValues, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)                               :: ESMF_GridToMesh
!
! !ARGUMENTS:
    type(ESMF_Grid), intent(in)                :: grid
    type(ESMF_StaggerLoc),  intent(in)            :: staggerLoc
    integer,                intent(in)            :: isSphere
    integer(ESMF_KIND_I4), optional               :: maskValues(:)
    integer, intent(out) , optional               :: rc
!
! !DESCRIPTION:
!   Create a mesh object with the same topology as the grid.
!
!   \begin{description}
!   \item [grid]
!         The grid to copy.
!   \item [staggerLoc]
!         Stagger location on grid to build.
!   \item [isSphhere]
!         1 = a spherical grid, build 3d mesh
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc 
    type(ESMF_Pointer) :: theMesh
    type(ESMF_InterfaceInt) :: maskValuesArg
    type(ESMF_IndexFlag) :: indexflag

    localrc = ESMF_SUCCESS

    ! Error check Grid
    call ESMF_GridGet(grid, indexflag=indexflag, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	  ESMF_CONTEXT, rcToReturn=rc)) return

    ! Make sure indexflag is ESMF_INDEX_GLOBAL
    if (.not. (indexflag .eq. ESMF_INDEX_GLOBAL)) then
       if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
" - Currently the Grid must be created with indexflag=ESMF_INDEX_GLOBAL to use this functionality", &
       ESMF_CONTEXT, rc)) return
    endif 

    ! convert mask values 
    maskValuesArg = ESMF_InterfaceIntCreate(maskValues, rc=localrc)
    	if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	  ESMF_CONTEXT, rcToReturn=rc)) return
 
    call c_esmc_gridtomesh(grid, staggerLoc%staggerloc, isSphere, theMesh, maskValuesArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	    ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterfaceIntDestroy(maskValuesArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	    ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_GridToMesh = ESMF_MeshCreate(theMesh)

    if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_GridToMesh

end module ESMF_GridUtilMod


