! $Id: ESMF_Regrid.F90,v 1.121 2008/02/25 21:33:50 dneckels Exp $
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
!
#define ESMF_FILENAME "ESMF_Regrid.F90"
!
!     ESMF  Regrid Module
      module ESMF_RegridMod
!
!==============================================================================
!
! ***THIS CODE IS CURRENTLY NON-FUNCTIONAL WHILE WE BRING IN A NEW,
! ***MORE GENERAL REGRIDDING ENGINE.  It REMAINS HERE FOR THE TIME BEING
! ***BECAUSE WE ANTICIPATE REUSING PARTS OF THE INTERFACE.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
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
      use ESMF_FieldMod
      use ESMF_GridMod
      use ESMF_StaggerLocMod
      use ESMF_VmMod
      use ESMF_RHandleMod
      use ESMF_UtilTypesMod
      use ESMF_BaseMod          ! ESMF base class
      use ESMF_LogErrMod
      use ESMF_ArrayMod
      use ESMF_F90InterfaceMod


      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    ! These are wrapper routines which call RegridCreate to do the
    !  actual work.  Since all our routines are data centric methods
    !  and we are not exposing an externally visible "regrid" object, 
    !  these routines must exist to be consistent with the other interfaces.  
    ! 
    public ESMF_RegridCreate

!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
         '$Id: ESMF_Regrid.F90,v 1.121 2008/02/25 21:33:50 dneckels Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Regrid Create, Run, and Destroy methods.
! 
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridCreate"
!BOPI
! !IROUTINE: ESMF_RegridCreate - Precomputes Regrid data

! !INTERFACE:
      subroutine ESMF_RegridCreate(srcField, dstField, routeHandle, rc)
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)        :: srcField
      type(ESMF_Field), intent(inout)        :: dstField
      type(ESMF_RouteHandle),  intent(inout) :: routehandle
      integer,                  intent(  out), optional :: rc
!
! !DESCRIPTION:
!EOPI
     integer :: localrc
     type(ESMF_Grid)      :: srcGrid
     type(ESMF_Grid)      :: dstGrid
     type(ESMF_Array)     :: srcArray
     type(ESMF_Array)     :: dstArray
     type(ESMF_VM)        :: vm

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL


       ! global vm for now
       call ESMF_VMGetGlobal(vm, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Now we go through the painful process of extracting the data members
       ! that we need.
       call ESMF_FieldGet(srcField, grid=srcGrid, array=srcArray, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       call ESMF_FieldGet(dstField, grid=dstGrid, array=dstArray, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Call through to the C++ object that does the work
       call c_ESMC_regrid_create(vm, srcGrid, srcArray, ESMF_STAGGERLOC_CENTER,  &
                   dstGrid, dstArray, ESMF_STAGGERLOC_CENTER, routehandle, localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Mark route handle created
       call ESMF_RouteHandleSetInitCreated(routeHandle, rc)

      end subroutine ESMF_RegridCreate

   end module ESMF_RegridMod
