! $Id: ESMF_GridUtil.F90,v 1.1 2008/02/29 22:14:51 dneckels Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
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

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_GridUtil.F90,v 1.1 2008/02/29 22:14:51 dneckels Exp $'

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
                         array1, array2, array3, array4, array5, array6, rc)
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
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: minidx

    ! initialize return code; assume routine not implemented
    localrc = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

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
             array1, array1, array1, array1, array1, array1)
      case (3) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 2, filename, localrc, &
             array1, array2, array1, array1, array1, array1)
      case (4) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 3, filename, localrc, &
             array1, array2, array3, array1, array1, array1)
      case (5) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 4, filename, localrc, &
             array1, array2, array3, array4, array1, array1)
      case (6) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 5, filename, localrc, &
             array1, array2, array3, array4, array5, array1)
      case (10) 
        call c_ESMC_MeshIO(vm, grid, staggerLoc, 6, filename, localrc, &
             array1, array2, array3, array4, array5, array6)
      case default
        localrc = ESMF_RC_NOT_IMPL
    end select

  end subroutine ESMF_MeshIO
!------------------------------------------------------------------------------

end module ESMF_GridUtilMod
