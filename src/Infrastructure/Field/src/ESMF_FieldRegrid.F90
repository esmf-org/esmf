! $Id: ESMF_FieldRegrid.F90,v 1.8 2008/05/16 22:14:22 dneckels Exp $
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
  use ESMF_RHandleMod
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
   public ESMF_FieldRegridRelease      ! apply a regrid operator

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_FieldRegridStore -- Generic interface

! !INTERFACE:
  interface ESMF_FieldRegridStore

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_FieldRegridStoreRH
    module procedure ESMF_FieldRegridStoreRHIW
    module procedure ESMF_FieldRegridStoreIW
!EOPI

  end interface


!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_FieldRegrid.F90,v 1.8 2008/05/16 22:14:22 dneckels Exp $'

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
#define ESMF_METHOD "ESMF_FieldRegridStoreRH"

!BOP
! !IROUTINE: ESMF_FieldRegridStoreRH - Store the regrid in routeHandle
!
! !INTERFACE:
      subroutine ESMF_FieldRegridStoreRH(srcField, dstField, routeHandle,&
                      regridMethod, regridScheme, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)     :: srcField
      type(ESMF_Field), intent(inout)     :: dstField
      type(ESMF_RouteHandle), intent(inout) :: routeHandle
      type(ESMF_RegridMethod), intent(in) :: regridMethod
      integer, intent(in), optional       :: regridScheme
      integer, intent(out), optional :: rc 
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
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc
        integer              :: lregridScheme

        type(ESMF_Grid)      :: srcGrid
        type(ESMF_Grid)      :: dstGrid
        type(ESMF_Array)     :: srcArray
        type(ESMF_Array)     :: dstArray
        type(ESMF_VM)        :: vm

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
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

        ! Will eventually determine scheme either as a parameter or from properties
        ! of the source grid
        if (present(regridScheme)) then
          lregridScheme = regridScheme
        else
          lregridScheme = ESMF_REGRID_SCHEME_NATIVE
        endif

        
        call ESMF_RegridStore(srcGrid, srcArray, dstGrid, dstArray, &
                 regridMethod, regridScheme, routeHandle, localrc)

        if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return


        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridStoreRH

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridStoreRHIW"

!BOP
! !IROUTINE: ESMF_FieldRegridStoreRHIW - Store the regrid and
!            return routehandle, weights
!
! !INTERFACE:
      subroutine ESMF_FieldRegridStoreRHIW(srcField, dstField, routeHandle,&
                      indicies, weights, &
                      regridMethod, regridScheme, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)     :: srcField
      type(ESMF_Field), intent(inout)     :: dstField
      type(ESMF_RouteHandle), intent(inout) :: routeHandle
      integer(ESMF_KIND_I4), pointer, intent(out) :: indicies(:,:)
      real(ESMF_KIND_R4), pointer, intent(out) :: weights(:)
      type(ESMF_RegridMethod), intent(in) :: regridMethod
      integer, intent(in), optional       :: regridScheme
      integer, intent(out), optional :: rc 
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
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc
        integer              :: lregridScheme

        type(ESMF_Grid)      :: srcGrid
        type(ESMF_Grid)      :: dstGrid
        type(ESMF_Array)     :: srcArray
        type(ESMF_Array)     :: dstArray
        type(ESMF_VM)        :: vm

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
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

        ! Will eventually determine scheme either as a parameter or from properties
        ! of the source grid
        if (present(regridScheme)) then
          lregridScheme = regridScheme
        else
          lregridScheme = ESMF_REGRID_SCHEME_NATIVE
        endif

        
        call ESMF_RegridStore(srcGrid, srcArray, dstGrid, dstArray, &
                 regridMethod, regridScheme, routeHandle, &
                 indicies, weights, localrc)

        if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return


        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridStoreRHIW

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridStoreIW"

!BOP
! !IROUTINE: ESMF_FieldRegridStoreIW - Store the regrid and
!            return weights
!
! !INTERFACE:
      subroutine ESMF_FieldRegridStoreIW(srcField, dstField, &
                      indicies, weights, &
                      regridMethod, regridScheme, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout)     :: srcField
      type(ESMF_Field), intent(inout)     :: dstField
      integer(ESMF_KIND_I4), pointer, intent(out) :: indicies(:,:)
      real(ESMF_KIND_R4), pointer, intent(out) :: weights(:)
      type(ESMF_RegridMethod), intent(in) :: regridMethod
      integer, intent(in), optional       :: regridScheme
      integer, intent(out), optional :: rc 
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
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        integer :: localrc
        integer              :: lregridScheme

        type(ESMF_Grid)      :: srcGrid
        type(ESMF_Grid)      :: dstGrid
        type(ESMF_Array)     :: srcArray
        type(ESMF_Array)     :: dstArray
        type(ESMF_VM)        :: vm

        ! Initialize return code; assume failure until success is certain
        localrc = ESMF_SUCCESS
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

        ! Will eventually determine scheme either as a parameter or from properties
        ! of the source grid
        if (present(regridScheme)) then
          lregridScheme = regridScheme
        else
          lregridScheme = ESMF_REGRID_SCHEME_NATIVE
        endif

        
        call ESMF_RegridStore(srcGrid, srcArray, dstGrid, dstArray, &
                 regridMethod, regridScheme, &
                 indicies, weights, localrc)

        if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return


        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRegridStoreIW

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRegridRun"

!BOP
! !IROUTINE: ESMF_FieldRegridRun - Apply the regrid operator
!
! !INTERFACE:
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

end module ESMF_FieldRegridMod
