! $Id: ESMF_Regrid.F90,v 1.123 2008/04/05 03:38:52 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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
      type ESMF_RegridMethod
      sequence
!  private
         integer :: regridmethod
      end type


      type(ESMF_RegridMethod), parameter :: &
           ESMF_REGRID_METHOD_BILINEAR    = ESMF_RegridMethod(0), &
           ESMF_REGRID_METHOD_PATCH       = ESMF_RegridMethod(1), &
           ESMF_REGRID_METHOD_CONSERV1    = ESMF_RegridMethod(2)

      integer, parameter :: ESMF_REGRID_SCHEME_FULL3D = 0, &
                            ESMF_REGRID_SCHEME_NATIVE = 1                            

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!
!------------------------------------------------------------------------------

       public ESMF_RegridMethod,  ESMF_REGRID_METHOD_BILINEAR, &
                                  ESMF_REGRID_METHOD_PATCH, &
                                  ESMF_REGRID_METHOD_CONSERV1

       public ESMF_REGRID_SCHEME_FULL3D, &
              ESMF_REGRID_SCHEME_NATIVE
!
! !PUBLIC MEMBER FUNCTIONS:
!

    ! These are wrapper routines which call RegridStore to do the
    !  actual work.  Since all our routines are data centric methods
    !  and we are not exposing an externally visible "regrid" object, 
    !  these routines must exist to be consistent with the other interfaces.  
    ! 
    public ESMF_RegridStore

!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
         '$Id: ESMF_Regrid.F90,v 1.123 2008/04/05 03:38:52 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (.eq.)

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

!==============================================================================
!
! This section includes the Regrid Create, Run, and Destroy methods.
! 
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridStore"
!BOPI
! !IROUTINE: ESMF_RegridStore - Precomputes Regrid data

! !INTERFACE:
      subroutine ESMF_RegridStore(srcGrid, srcArray, &
                 dstGrid, dstArray, &
                 regridMethod, regridScheme, &
                 routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout)         :: srcGrid
      type(ESMF_Array), intent(inout)        :: srcArray
      type(ESMF_Grid), intent(inout)         :: dstGrid
      type(ESMF_Array), intent(inout)        :: dstArray
      type(ESMF_RegridMethod), intent(in)    :: regridMethod
      integer, intent(in)                    :: regridScheme
      type(ESMF_RouteHandle),  intent(inout) :: routehandle
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
!     \item[regridMethod]
!          The interpolation method to use.
!     \item[regridScheme]
!          Whether to use 3d or native coordinates
!     \item[routeHandle]
!          Handle to store the resulting sparseMatrix
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       type(ESMF_StaggerLoc) :: staggerLoc
       type(ESMF_VM)        :: vm

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! For the moment only support bilinear
       if (regridMethod .ne. ESMF_REGRID_METHOD_BILINEAR) then
         localrc = ESMF_RC_NOT_IMPL
       else
         localrc = ESMF_SUCCESS
       endif

       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


       ! global vm for now
       call ESMF_VMGetGlobal(vm, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! For the moment only support bilinear
       if (regridMethod .ne. ESMF_REGRID_METHOD_BILINEAR) then
         localrc = ESMF_RC_NOT_IMPL
       else
         localrc = ESMF_SUCCESS
       endif

       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! global vm for now
       call ESMF_VMGetGlobal(vm, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Choose the stagger.  Perhaps eventually this can be more configurable,
       ! but for now, conserve = node, bilinear = center
       if (regridMethod .eq. ESMF_REGRID_METHOD_BILINEAR) then
         staggerLoc = ESMF_STAGGERLOC_CENTER
       elseif (regridMethod .eq. ESMF_REGRID_METHOD_PATCH) then
         staggerLoc = ESMF_STAGGERLOC_CENTER
       else
         staggerLoc = ESMF_STAGGERLOC_CENTER
       endif

       ! Call through to the C++ object that does the work
       call c_ESMC_regrid_create(vm, srcGrid, srcArray, staggerLoc,  &
                   dstGrid, dstArray, staggerLoc%staggerloc, &
                   regridMethod, regridScheme, &
                   routehandle, localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Mark route handle created
       call ESMF_RouteHandleSetInitCreated(routeHandle, rc)

      end subroutine ESMF_RegridStore

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

      type (ESMF_RegridMethod), intent(in) :: &
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

      type (ESMF_RegridMethod), intent(in) :: &
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



   end module ESMF_RegridMod
