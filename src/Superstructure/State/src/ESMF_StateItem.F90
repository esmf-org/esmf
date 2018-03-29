! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
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
      use ESMF_IOUtilMod
      use ESMF_ContainerMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_StateItem_Flag
!     !   Each entry in the list of states is either simply a name placeholder
!     !   or an actual data item - FieldBundle, Field, Array, or State. 
!
      type ESMF_StateItem_Flag
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      !private
         integer :: ot
      end type

      ! keep these numbers distinct from the 30 or so esmf object types.
      type(ESMF_StateItem_Flag), parameter :: &
                ESMF_STATEITEM_FIELD        = ESMF_StateItem_Flag(101), &
                ESMF_STATEITEM_FIELDBUNDLE  = ESMF_StateItem_Flag(102), &
                ESMF_STATEITEM_ARRAY        = ESMF_StateItem_Flag(103), &
                ESMF_STATEITEM_ARRAYBUNDLE  = ESMF_StateItem_Flag(104), &
                ESMF_STATEITEM_ROUTEHANDLE  = ESMF_StateItem_Flag(105), &
                ESMF_STATEITEM_STATE        = ESMF_StateItem_Flag(106), &
#if 0
                ESMF_STATEITEM_NAME         = ESMF_StateItem_Flag(107), &
                ESMF_STATEITEM_INDIRECT     = ESMF_StateItem_Flag(108), &
#endif
                ESMF_STATEITEM_UNKNOWN      = ESMF_StateItem_Flag(109), &
                ESMF_STATEITEM_NOTFOUND     = ESMF_StateItem_Flag(110)

#if 0
!------------------------------------------------------------------------------
!     ! ESMF_NeededFlag
!     !   For an Export State if all data which can potentially be created is
!     !   not needed, this flag can be used to mark data which does not need
!     !   to be created by the Component.
!
      type ESMF_NeededFlag
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
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
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
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
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
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
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      !private
         integer :: valid
      end type

      type(ESMF_ValidFlag), parameter :: &
                ESMF_VALID = ESMF_ValidFlag(1), &
                ESMF_INVALID= ESMF_ValidFlag(2), &
                ESMF_VALIDITYUNKNOWN = ESMF_ValidFlag(3)
#endif

!------------------------------------------------------------------------------
!     ! ESMF_DataHolder
!
!     ! Make a single data type for FieldBundles, Fields, and Arrays.
!     !  The ObjectType is one level up, because this structure is not
!     !  allocated until it is actually needed.  This is a private type.

!     ! state has to be different because it's a forward reference.

      type ESMF_DataHolder
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
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
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
#endif
      !private
        type(ESMF_DataHolder) :: datap
        type(ESMF_StateItem_Flag) :: otype
#if 0
        type(ESMF_NeededFlag) :: needed 
        type(ESMF_ReadyFlag) :: ready
        type(ESMF_ValidFlag) :: valid
        type(ESMF_ReqForRestartFlag) :: reqrestart
#endif
        ! VMId is currently needed for FieldBundles and their indirect Fields.          
        type(ESMF_VMId)      :: FldBundleVMId
        logical :: proxyFlag
        integer :: indirect_index
        character(len=ESMF_MAXSTR) :: namep
        logical :: removedflag
        ESMF_INIT_DECLARE
      end type
      
!------------------------------------------------------------------------------
!     ! ESMF_StateItemWrap
!
!     ! Extra level of indirection, allowing use of ESMF_Container

      type ESMF_StateItemWrap
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
#endif
      !private
        type(ESMF_StateItem), pointer  :: si
      end type

!------------------------------------------------------------------------------
!     ! ESMF_StateIntent_Flag
!     !   Enumerated value for storing Import or Export State type.
!
      type ESMF_StateIntent_Flag
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      !private
         integer :: state
      end type

      type(ESMF_StateIntent_Flag), parameter :: &
                ESMF_STATEINTENT_IMPORT   = ESMF_StateIntent_Flag(1), &
                ESMF_STATEINTENT_EXPORT   = ESMF_StateIntent_Flag(2), &
                ESMF_STATEINTENT_UNSPECIFIED = ESMF_StateIntent_Flag(3), &
                ESMF_STATEINTENT_INVALID  = ESMF_StateIntent_Flag(4)

!------------------------------------------------------------------------------
!     ! ESMF_StateClass
!
!     ! Internal State data type.

      type ESMF_StateClass
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
#endif
      !private
        type(ESMF_Base) :: base
        type(ESMF_MethodTable) :: methodTable
        type(ESMF_StateIntent_Flag) :: st
        type(ESMF_StateItemWrap), pointer :: zapList(:)
#if 0
        type(ESMF_NeededFlag) :: needed_default
        type(ESMF_ReadyFlag) :: ready_default
        type(ESMF_ValidFlag) :: stvalid_default
        type(ESMF_ReqForRestartFlag) :: reqrestart_default
#endif
        integer :: datacount
        type(ESMF_MapPtr) :: nameMap
#if 0
        type(ESMF_StateItem), pointer :: datalist(:)
#endif
        type(ESMF_Container):: stateContainer
        integer :: alloccount
        logical :: reconcileneededflag
         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

      public ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
        ESMF_STATEITEM_ARRAY, ESMF_STATEITEM_ARRAYBUNDLE, &
        ESMF_STATEITEM_ROUTEHANDLE, ESMF_STATEITEM_STATE, &
#if 0
        ESMF_STATEITEM_NAME, &
#endif
        ESMF_STATEITEM_NOTFOUND
      public ESMF_StateItemConstruct
      public ESMF_StateIntent_Flag, ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_EXPORT, &
                                   ESMF_STATEINTENT_UNSPECIFIED
#if 0
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
#endif

      ! only public for other files in the state class (should be friend)
      public ESMF_StateClass, ESMF_StateItem, ESMF_StateItemWrap, ESMF_DataHolder
#if 0
      public ESMF_STATEITEM_INDIRECT
#endif
      public ESMF_STATEITEM_UNKNOWN, ESMF_STATEINTENT_INVALID

  
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-internal methods:
  public ESMF_StateItemGet
  public ESMF_StateItemPrint

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateItemConstruct()"
!BOPI
! !IROUTINE: ESMF_StateItemConstruct - Create a new State Item

! !INTERFACE:
  function ESMF_StateItemConstruct (name, itemtype, keywordEnforcer, &
      proxyflag, rc) result (sip)
!
! !RETURN VALUE:
    type(ESMF_StateItem),     pointer    :: sip
!
! !ARGUMENTS:
    character(*),             intent(in) :: name
    type(ESMF_StateItem_Flag), intent(in) :: itemtype
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                  intent(in),  optional :: proxyflag
    integer,                  intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Pointer-valued function to create a StateItem.
!   \item[name]
!     Item name
!   \item[itemtype]
!     State item type code
!   \item[{[proxyflag]}]
!     Set proxy flag
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! local vars
    integer :: memstat

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    allocate (sip, stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, msg="creating StateItem", &
             ESMF_CONTEXT, rcToReturn=rc)) return

! print *, ESMF_METHOD, ': creating sip with name = ', trim (name)
    sip%namep = name
    sip%otype = itemtype
    sip%datap%spp => null ()

    if (present (proxyflag)) then
      sip%proxyFlag = proxyflag
    else
      sip%proxyFlag = .false.
    end if

    ESMF_INIT_SET_CREATED(sip)

    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_StateItemConstruct

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
        call ESMF_GetName(stateItem%datap%spp%base, name, localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) &
          return
      endif
    case default
      call ESMF_LogSetError(rcToCheck=ESMF_RC_INTNRL_BAD, &
        msg="- unsupported StateItemType", &
        ESMF_CONTEXT, rcToReturn=rc)
      name = '(unknown)'
      return  ! bail out
    end select
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_StateItemGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateItemPrint ()"
!BOPI
! !IROUTINE: ESMF_StateItemPrint - Print a StateItem

! !INTERFACE:
  subroutine ESMF_StateItemPrint (stateItem, header, prefixstr,  &
      longflag, debugflag, unit, rc)
!
! !ARGUMENTS:
    type(ESMF_StateItem), intent(in), target    :: stateItem
    character(*),         intent(in)            :: header
    character(*),         intent(in)            :: prefixstr
    logical,              intent(in)            :: longflag
    logical,              intent(in)            :: debugflag
    integer,              intent(in),  optional :: unit
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Print a StateItem.
!
!   The arguments are:
!   \begin{description}
!   \item[stateItem]
!     {\tt ESMF\_StateItem} queried.
!   \item[header]
!     Title line
!   \item[prefixstr]
!     Leading characters for output string (for indentation levels)
!   \item[longflag]
!     Print additional information such as proxyflag
!   \item[longflag]
!     Print additional information such as VMId
!   \item[unit]
!     Fortran unit number
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    type(ESMF_VMId) :: vmid
    type(ESMF_Array),           pointer :: arrayp
    type(ESMF_ArrayBundle),     pointer :: abundlep
    type(ESMF_FieldType),       pointer :: fieldp
    type(ESMF_FieldBundleType), pointer :: fbundlep
    type(ESMF_RouteHandle),     pointer :: rhandlep
    type(ESMF_StateClass),      pointer :: statep

    integer                     :: localrc      ! local return code
    integer                     :: localunit
    character(2*ESMF_MAXSTR)    :: outbuf

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    localunit = ESMF_UtilIOStdout
    if (present (unit)) then
      localunit = unit
    end if

    write (localunit, *) prefixstr, header, 'name: ', trim (stateItem%namep)

    outbuf = prefixstr // "          type:"

    select case (stateItem%otype%ot)
    case (ESMF_STATEITEM_FIELDBUNDLE%ot)
        outbuf = trim (outbuf) // " FieldBundle"
    case (ESMF_STATEITEM_FIELD%ot)
        outbuf = trim (outbuf) // " Field"
    case (ESMF_STATEITEM_ARRAY%ot)
        outbuf = trim (outbuf) // " Array"
    case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
        outbuf = trim (outbuf) // " ArrayBundle"
    case (ESMF_STATEITEM_ROUTEHANDLE%ot)
        outbuf = trim (outbuf) // " Route handle"
    case (ESMF_STATEITEM_STATE%ot)
        outbuf = trim (outbuf) // " State"
#if 0
    case (ESMF_STATEITEM_NAME%ot)
        outbuf = trim (outbuf) // " Placeholder name"
    case (ESMF_STATEITEM_INDIRECT%ot)
        outbuf = trim (outbuf) // " Indirect Field inside a FieldBundle"
#endif
    case (ESMF_STATEITEM_UNKNOWN%ot)
        outbuf = trim (outbuf) // " Unknown"
    case (ESMF_STATEITEM_NOTFOUND%ot)
        outbuf = trim (outbuf) // " Not found"
    case default
        outbuf = trim (outbuf) // " (bad type value)"
    end select

    if (longflag) then
      outbuf = trim (outbuf) //  &
            ", proxy flag: " // merge ("yes", "no ", stateItem%proxyFlag)

    end if

    write (localunit,*) trim(outbuf)

    if (debugflag) then

      select case (stateItem%otype%ot)
      case (ESMF_STATEITEM_FIELDBUNDLE%ot)
        fbundlep => stateItem%datap%fbp%this
        call ESMF_BaseGetVMId (fbundlep%base, vmid, rc=localrc)

      case (ESMF_STATEITEM_FIELD%ot)
        fieldp => stateItem%datap%fp%ftypep

        call ESMF_BaseGetVMId (fieldp%base, vmid, rc=localrc)
        call c_ESMC_BasePrint(fieldp, 1, "debug", ESMF_FALSE, "", ESMF_FALSE, localrc)

      case (ESMF_STATEITEM_ARRAY%ot)
        arrayp => stateItem%datap%ap
        call c_ESMC_GetVMId (arrayp, vmid, localrc)

      case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
        abundlep => stateItem%datap%abp
        call c_ESMC_GetVMId (abundlep, vmid, localrc)

      case (ESMF_STATEITEM_ROUTEHANDLE%ot)
        rhandlep => stateItem%datap%rp
        call c_ESMC_GetVMId (rhandlep, vmid, localrc)

      case (ESMF_STATEITEM_STATE%ot)
        statep => stateItem%datap%spp
        call ESMF_BaseGetVMId (statep%base, vmid, rc=localrc)

      end select
      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_UtilIOUnitFlush (localunit, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      call c_esmc_vmidprint (vmid, localrc)
      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    end if

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_StateItemPrint
!------------------------------------------------------------------------------
    

end module ESMF_StateItemMod


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_stateitemwrapcast"
subroutine f_esmf_stateitemwrapcast(stateItemWrapOut, stateItemWrapIn, rc)

  use ESMF_UtilTypesMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_StateItemMod

  implicit none

  type(ESMF_StateItemWrap),intent(inout) :: stateItemWrapOut
  type(ESMF_StateItemWrap),intent(inout) :: stateItemWrapIn
  integer, intent(out)           :: rc              

  integer :: localrc

  localrc = ESMF_RC_NOT_IMPL

  ! simple assignment
  stateItemWrapOut = stateItemWrapIn

  ! return successfully
  rc = ESMF_SUCCESS

end subroutine f_esmf_stateitemwrapcast

