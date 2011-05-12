! $Id: ESMF_StateItem.F90,v 1.2 2011/05/12 04:56:30 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateItem.F90"
!
!     ESMF StateItem module
      module ESMF_StateItemMod
!
!==============================================================================
!
! This file contains the State class definitions. 
!  Other files in this directory contain the variou State class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayBundleMod
      use ESMF_FieldMod
      use ESMF_FieldGetMod
      use ESMF_FieldBundleMod
      use ESMF_RHandleMod
      use ESMF_InitMacrosMod
      use ESMF_ContainerMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_StateItemType
!     !   Each entry in the list of states is either simply a name placeholder
!     !   or an actual data item - FieldBundle, Field, Array, or State. 
!
      type ESMF_StateItemType
      sequence
      !private
         integer :: ot
      end type

      ! keep these numbers distinct from the 30 or so esmf object types.
      type(ESMF_StateItemType), parameter :: &
                ESMF_STATEITEM_FIELD        = ESMF_StateItemType(101), &
                ESMF_STATEITEM_FIELDBUNDLE  = ESMF_StateItemType(102), &
                ESMF_STATEITEM_ARRAY        = ESMF_StateItemType(103), &
                ESMF_STATEITEM_ARRAYBUNDLE  = ESMF_StateItemType(104), &
                ESMF_STATEITEM_ROUTEHANDLE  = ESMF_StateItemType(105), &
                ESMF_STATEITEM_STATE        = ESMF_StateItemType(106), &
                ESMF_STATEITEM_NAME         = ESMF_StateItemType(107), &
                ESMF_STATEITEM_INDIRECT     = ESMF_StateItemType(108), &
                ESMF_STATEITEM_UNKNOWN      = ESMF_StateItemType(109), &
                ESMF_STATEITEM_NOTFOUND     = ESMF_StateItemType(110)

!------------------------------------------------------------------------------
!     ! ESMF_NeededFlag
!     !   For an Export State if all data which can potentially be created is
!     !   not needed, this flag can be used to mark data which does not need
!     !   to be created by the Component.
!
      type ESMF_NeededFlag
      sequence
      !private
         integer :: needed
      end type

      type(ESMF_NeededFlag), parameter :: &
                ESMF_NEEDED = ESMF_NeededFlag(1), &
                ESMF_NOTNEEDED = ESMF_NeededFlag(2)

!------------------------------------------------------------------------------
!     ! ESMF_ReadyFlag
!
      type ESMF_ReadyFlag
      sequence
      !private
         integer :: ready
      end type

      type(ESMF_ReadyFlag), parameter :: &
                ESMF_READYTOWRITE = ESMF_ReadyFlag(1), &
                ESMF_READYTOREAD = ESMF_ReadyFlag(2), &
                ESMF_NOTREADY = ESMF_ReadyFlag(3)


!------------------------------------------------------------------------------
!     ! ESMF_ReqForRestartFlag
!
      type ESMF_ReqForRestartFlag
      sequence
      !private
         integer :: required4restart
      end type

      type(ESMF_ReqForRestartFlag), parameter :: &
                ESMF_REQUIRED_FOR_RESTART = ESMF_ReqForRestartFlag(1), &
                ESMF_NOTREQUIRED_FOR_RESTART = ESMF_ReqForRestartFlag(2)


!------------------------------------------------------------------------------
!     ! ESMF_ValidFlag
!
      type ESMF_ValidFlag
      sequence
      !private
         integer :: valid
      end type

      type(ESMF_ValidFlag), parameter :: &
                ESMF_VALID = ESMF_ValidFlag(1), &
                ESMF_INVALID= ESMF_ValidFlag(2), &
                ESMF_VALIDITYUNKNOWN = ESMF_ValidFlag(3)


!------------------------------------------------------------------------------
!     ! ESMF_DataHolder
!
!     ! Make a single data type for FieldBundles, Fields, and Arrays.
!     !  The ObjectType is one level up, because this structure is not
!     !  allocated until it is actually needed.  This is a private type.

!     ! state has to be different because it's a forward reference.

      type ESMF_DataHolder
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
          type(ESMF_Field)        :: fp 
          type(ESMF_FieldBundle)  :: fbp
          type(ESMF_Array)        :: ap
          type(ESMF_ArrayBundle)  :: abp
          type(ESMF_RouteHandle)  :: rp
          type(ESMF_StateClass), pointer  :: spp
          ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_StateItem
!
!     ! Description of next Data item in list, or simply a name
!     !  which holds the place for an optional Data item.

      type ESMF_StateItem
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
        type(ESMF_DataHolder) :: datap
        type(ESMF_StateItemType) :: otype
        type(ESMF_NeededFlag) :: needed
        type(ESMF_ReadyFlag) :: ready
        type(ESMF_ValidFlag) :: valid
        type(ESMF_ReqForRestartFlag) :: reqrestart

        ! VMId is currently needed for FieldBundles and their indirect Fields.
        type(ESMF_VMId)      :: FldBundleVMId

        logical :: proxyFlag
        integer :: indirect_index
        character(len=ESMF_MAXSTR) :: namep
        logical :: removedflag
         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_StateType
!     !   Enumerated value for storing Import or Export State type.
!
      type ESMF_StateType
      sequence
      !private
         integer :: state
      end type

      type(ESMF_StateType), parameter :: &
                ESMF_STATE_IMPORT   = ESMF_StateType(1), &
                ESMF_STATE_EXPORT   = ESMF_StateType(2), &
                ESMF_STATE_UNSPECIFIED = ESMF_StateType(3), &
                ESMF_STATE_INVALID  = ESMF_StateType(4)

!------------------------------------------------------------------------------
!     ! ESMF_StateClass
!
!     ! Internal State data type.

      type ESMF_StateClass
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
        type(ESMF_Base) :: base
        type(ESMF_MethodTable) :: methodTable
        type(ESMF_StateType) :: st
        type(ESMF_NeededFlag) :: needed_default
        type(ESMF_ReadyFlag) :: ready_default
        type(ESMF_ValidFlag) :: stvalid_default
        type(ESMF_ReqForRestartFlag) :: reqrestart_default
        integer :: alloccount
        integer :: datacount
        type(ESMF_MapPtr) :: nameMap
        type(ESMF_StateItem), pointer :: datalist(:)
        type(ESMF_Container):: stateContainer
        logical :: reconcileneededflag
         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

      public ESMF_StateItemType, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
        ESMF_STATEITEM_ARRAY, ESMF_STATEITEM_ARRAYBUNDLE, &
        ESMF_STATEITEM_ROUTEHANDLE, ESMF_STATEITEM_STATE, ESMF_STATEITEM_NAME, &
        ESMF_STATEITEM_NOTFOUND
      public ESMF_StateType, ESMF_STATE_IMPORT, ESMF_STATE_EXPORT, &
                                   ESMF_STATE_UNSPECIFIED
      public ESMF_NeededFlag, ESMF_NEEDED, &
                                   ESMF_NOTNEEDED
      public ESMF_ReadyFlag,  ESMF_READYTOWRITE, &
                                   ESMF_READYTOREAD, &
                                   ESMF_NOTREADY
      public ESMF_ReqForRestartFlag,  ESMF_REQUIRED_FOR_RESTART, &
                                   ESMF_NOTREQUIRED_FOR_RESTART
      public ESMF_ValidFlag,  ESMF_VALID, &
                                   ESMF_INVALID, &
                                   ESMF_VALIDITYUNKNOWN

      ! only public for other files in the state class (should be friend)
      public ESMF_StateClass, ESMF_StateItem, ESMF_DataHolder
      public ESMF_STATEITEM_INDIRECT, ESMF_STATEITEM_UNKNOWN
      public ESMF_STATE_INVALID

  
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-internal methods:
  public ESMF_StateItemGet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateItemGet()"
!BOPI
! !IROUTINE: ESMF_StateItemGet - Query a StateItem

! !INTERFACE:
  subroutine ESMF_StateItemGet(stateItem, name, rc)
!
! !ARGUMENTS:
    type(ESMF_StateItem), intent(in)            :: stateItem
    character(len=*),     intent(out), optional :: name
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Query a StateItem for information.
!
!   The arguments are:
!   \begin{description}
!   \item[stateItem]
!     {\tt ESMF\_StateItem} queried.
!   \item [{[name]}]
!     Name of the contained State item.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    select case (stateItem%otype%ot)
    case (ESMF_STATEITEM_FIELD%ot)
      call ESMF_FieldGet(stateItem%datap%fp, name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        return
    case (ESMF_STATEITEM_FIELDBUNDLE%ot)
      call ESMF_FieldBundleGet(stateItem%datap%fbp, name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        return
    case (ESMF_STATEITEM_ARRAY%ot)
      call ESMF_ArrayGet(stateItem%datap%ap, name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        return
    case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
      call ESMF_ArrayBundleGet(stateItem%datap%abp, name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        return
    case (ESMF_STATEITEM_ROUTEHANDLE%ot)
      call ESMF_RouteHandleGet(stateItem%datap%rp, name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) &
        return
    case (ESMF_STATEITEM_STATE%ot)
      if (present(name)) then
        call c_ESMC_GetName(stateItem%datap%spp%base, name, localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          return
      endif
    case default
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="- unsupported StateItemType", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail out
    end select
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_StateItemGet
!------------------------------------------------------------------------------


end module ESMF_StateItemMod


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_stateitemcast"
subroutine f_esmf_stateitemcast(stateitemOut, stateitemIn, rc)

  use ESMF_UtilTypesMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_StateItemMod

  implicit none

  type(ESMF_StateItem),intent(inout) :: stateItemOut
  type(ESMF_StateItem),intent(inout) :: stateItemIn
  integer, intent(out)           :: rc              

  integer :: localrc

  localrc = ESMF_RC_NOT_IMPL

  ! simple assignment
  stateItemOut = stateItemIn

  ! return successfully
  rc = ESMF_SUCCESS

end subroutine f_esmf_stateitemcast

