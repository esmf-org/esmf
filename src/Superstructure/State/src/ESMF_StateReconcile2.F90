! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateReconcile2.F90"
!
! ESMF StateReconcile module
module ESMF_StateReconcile2Mod
!
!==============================================================================
!
! This file contains the State Reconcile class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"

! TODO: Ensure buffer offsets are aligned between items until the various
! type-specific methods are totally reliable.  (Array in particular.)
#define ALIGN_FIX 1

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateReconcileMod - Data exchange within a component
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran function and subroutine 
!  interfaces to ensure that {\tt ESMF\_State} data is consistent across
!  all PETs.  The intended use is by components that have subcomponents 
!  which run on subsets of the couplers PET list.
!  Objects that have been created on only a subset of the PETs cannot be
!  identified to methods like Regrid or Redistribution since they have no 
!  valid handles to identify them.  The code here communicates the missing
!  object information to other PETs in the current VM.
!
!
! !USES:
  use ESMF_BaseMod
  use ESMF_InitMacrosMod
  use ESMF_IOUtilMod
  use ESMF_LogErrMod
  use ESMF_StateMod
  use ESMF_StateContainerMod
  use ESMF_StateItemMod
  use ESMF_StateTypesMod
  use ESMF_VMMod
  use ESMF_UtilTypesMod

  use ESMF_ArrayMod
  use ESMF_ArrayBundleMod
  use ESMF_FieldMod
  use ESMF_FieldGetMod
  use ESMF_FieldBundleMod
  use ESMF_RHandleMod

  implicit none
  private

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

  public :: ESMF_StateReconcile  ! make State consistent for concurrent apps

  ! These are only public for unit testing.  They are not intended
  ! to be called by ESMF users.
  ! public :: ESMF_ReconcileDeserialize, ESMF_ReconcileSerialize
  ! public :: ESMF_ReconcileSendItems

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
  '$Id$'
!==============================================================================

! !PRIVATE TYPES:
!------------------------------------------------------------------------------

! ! ESMF_NeedsBuffer
  type ESMF_NeedsBuffer
    logical, pointer :: needs(:) => null ()
  end type

! ! ESMF_ReconcileIDInfo
!
! ! ID/VMId pair, plus other global PET info

  type ESMF_ReconcileIDInfo
    integer,         pointer :: id(:) => null ()
    type(ESMF_VMId), pointer :: vmid(:) => null ()
    logical,         pointer :: needed(:) => null ()
    character,       pointer :: item_buffer(:) => null ()
  end type

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!==============================================================================
!
! Misc
!
!==============================================================================

  logical, parameter :: trace=.false.
  logical, parameter :: debug=.false.

contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReconcile2"
!BOP
! !IROUTINE: ESMF_StateReconcile -- Reconcile State data across all PETs in a VM
!
! !INTERFACE:
  subroutine ESMF_StateReconcile (state, vm, attreconflag, rc)
!
! !ARGUMENTS:
    type(ESMF_State),            intent(inout)         :: state
    type(ESMF_VM),               intent(in),  optional :: vm
    type(ESMF_AttReconcileFlag), intent(in),  optional :: attreconflag
    integer,                     intent(out), optional :: rc                   
!
!
! !DESCRIPTION:
!     Must be called for any {\tt ESMF\_State} which contains ESMF objects
!     that have not been created on all the {\tt PET}s of the currently
!     running {\tt ESMF\_Component}.  
!     For example, if a coupler is operating on data
!     which was created by another component that ran on only a subset
!     of the couplers {\tt PET}s, the coupler must make this call first
!     before operating on any data inside that {\tt ESMF\_State}.
!     After calling {\tt ESMF\_StateReconcile} all {\tt PET}s will have
!     a common view of all objects contained in this {\tt ESMF\_State}.
!     The option to reconcile the metadata associated with the objects
!     contained in this {\tt ESMF\_State} also exists.  The default behavior
!     for this capability is to {\it not} reconcile metadata unless told
!     otherwise.
!
!     This call is collective across the specified VM.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to reconcile.
!     \item[{[vm]}]
!       {\tt ESMF\_VM} for this {\tt ESMF\_Component}.  By default, it is set to the current vm.
!     \item[{[attreconflag]}]
!       Flag to tell if Attribute reconciliation is to be done as well as data reconciliation.
!       This flag is documented in section \ref{const:attreconcile}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
    integer :: localrc
    type(ESMF_VM) :: localvm
    type(ESMF_AttReconcileFlag) :: lattreconflag

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    if (present (vm)) then
      localvm = vm
    else
      call ESMF_VMGetCurrent(vm=localvm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    ! Each PET broadcasts the object ID lists and compares them to what
    ! they get back.   Missing objects are sent so they can be recreated
    ! on the PETs without those objects as "proxy" objects.  Eventually
    ! we might want to hash the ID lists so we can send a single number
    ! (or short list of numbers) instead of having to build and send the
    ! list each time.
     
    ! Set the optional ESMF_AttReconcileFlag
    lattreconflag = ESMF_ATTRECONCILE_OFF
    if(present(attreconflag)) then
      lattreconflag = attreconflag
    endif

    call ESMF_StateReconcile_driver (state, vm=localvm, &
        attreconflag=lattreconflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_StateReconcile

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReconcile_driver"
!BOPI
! !IROUTINE: ESMF_StateReconcile_driver
!
! !INTERFACE:
    subroutine ESMF_StateReconcile_driver (state, vm, attreconflag, rc)
!
! !ARGUMENTS:
      type (ESMF_State), intent(inout) :: state
      type (ESMF_VM),    intent(in)    :: vm
      type(ESMF_AttReconcileFlag), intent(in)  :: attreconflag
      integer,           intent(out)   :: rc
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to collect information from.
!     \item[vm]
!       The current {\tt ESMF\_VM} (virtual machine).  All PETs in this
!       {\tt ESMF\_VM} will exchange information about objects which might
!       only be known to one or more PETs, and ensure all PETs in this VM
!       have a consistent view of the object list in this {\tt ESMF\_State}.
!     \item[{[attreconflag]}]
!       Flag to tell if Attribute reconciliation is to be done as well as data reconciliation
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
    integer :: localrc
    integer :: memstat
    integer :: mypet, npets

    integer, pointer :: nitems_buf(:)
    type (ESMF_StateItemWrap), pointer :: siwrap(:)

    integer,         pointer :: ids_send(:), itemtypes_send(:)
    type(ESMF_VMId), pointer :: vmids_send(:)

    type(ESMF_ReconcileIDInfo), allocatable :: id_info(:)

    logical, pointer :: recvd_needs_matrix(:,:)

    type(ESMF_CharPtr), allocatable :: items_recv(:)
    character, pointer :: buffer_recv(:)

    integer :: i

    logical, parameter :: debug = .false.
    logical, parameter :: meminfo = .false.
    logical, parameter :: trace = .false.

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (debug) then
      do, i=0, npets-1
        if (i == mypet) then
          call ESMF_StatePrint (state)
          call ESMF_UtilIOUnitFlush (6)
        end if
        call ESMF_VMBarrier (vm)
      end do
    end if

    ! 0.) Interchange item counts between PETs.  Set up counts/displacements
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 0 - Initialize item counts and siwrappers')
    end if
    siwrap     => null ()
    nitems_buf => null ()
    call ESMF_ReconcileInitialize (state, vm,  &
        siwrap=siwrap, nitems_all=nitems_buf, rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! 1.) Each PET constructs its send arrays containing local Id
    ! and VMId info for each object contained in the State.
    ! Note that element zero is reserved for the State itself.
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 1 - Build send arrays')
    end if
    itemtypes_send => null ()
    ids_send   => null ()
    vmids_send => null ()      
    call ESMF_ReconcileGetStateIDInfo (state, siwrap,  &
        itemtype=itemtypes_send,  &
          id=  ids_send,  &
        vmid=vmids_send,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (meminfo) call ESMF_VMLogMemInfo ('after Step 1')


    ! 2.) All PETs send their items Ids and VMIds to all the other PETs,
    ! then create local directories of which PETs have which ids/VMIds.
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 2 - Exchange Ids/VMIds')
    end if

    allocate (id_info(0:npets-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    call ESMF_ReconcileExchgIDInfo (vm,  &
        nitems_buf=nitems_buf,  &
        id=ids_send,  &
        vmid=vmids_send,  &
        id_info=id_info, &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (meminfo) call ESMF_VMLogMemInfo ('after Step 2')


! At this point, each PET knows what items can be found on all of
! the other PETs.  The id_info array has global PET info in it.

    ! 3.) Construct needs list.  Receiving PETs compare IDs and VMIds
    ! in their send ID/VMId array with what was received from the
    ! currently-being-processed sending PET.  Note that multiple PETs
    ! can 'offer' an item.

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 3 - Compare and create needs arrays')
    end if

    call ESMF_ReconcileCompareNeeds (vm,  &
          id=  ids_send,  &
        vmid=vmids_send,  &
        id_info=id_info,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (meminfo) call ESMF_VMLogMemInfo ('after Step 3')


    ! 4.) Communicate needs back to the offering PETs.
    ! Send to each offering PET a buffer containing 'needed' array
    ! specifying which items are needed.  The array is the same size as,
    ! and corresponds to, the ID and VMId arrays that were previously
    ! offered.

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 4 - Exchange needs')
    end if

    recvd_needs_matrix => null ()
    call ESMF_ReconcileExchgNeeds (vm,  &
        id_info=id_info,  &
        recv_needs=recvd_needs_matrix,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (meminfo) call ESMF_VMLogMemInfo ('after Step 4')


    ! 5.) Serialized needed objects

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 5 - Serialize needs', ask=.false.)
    end if
    call ESMF_ReconcileSerialize (state, vm, siwrap, &
        needs_list=recvd_needs_matrix, &
        attreconflag=attreconflag,  &
        id_info=id_info,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    deallocate (recvd_needs_matrix, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (meminfo) call ESMF_VMLogMemInfo ('after Step 5')


    ! 6.) Send/receive serialized objects to whoever needed them

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 6 - Exchange serialized objects')
    end if

    allocate (items_recv(0:npets-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    buffer_recv => null ()
    call ESMF_ReconcileExchgItems (vm,  &
        id_info=id_info,  &
        recv_items=items_recv,  &  ! %cptr aliased to portions of buffer_recv
        recv_buffer=buffer_recv,  &
        rc=localrc)
    if (debug)  &
        localrc = ESMF_ReconcileAllRC (vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (meminfo) call ESMF_VMLogMemInfo ('after Step 6')


    ! 7.) Deserialize received objects and create proxies (recurse on
    !     nested States as needed)

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 7 - Deserialize needs')
    end if

    do, i=0, npets-1
      if (debug) then
        write (*, '(a,i0,a,i0,a,l1)')  &
            '   PET ', mypet, ': Deserializing from PET ', i,  &
            ', associated (items_recv(i)%cptr) =', associated (items_recv(i)%cptr)
      end if
      if (associated (items_recv(i)%cptr)) then
        if (debug) then
          print *, '    items_recv(', lbound (items_recv(i)%cptr),  &
              ':', ubound (items_recv(i)%cptr), ')'
        end if
        call ESMF_ReconcileDeserialize (state, vm,  &
            obj_buffer=items_recv(i)%cptr,  &
            vm_ids=id_info(i)%vmid,  &
            attreconflag=attreconflag, rc=localrc)
      else
        localrc = ESMF_SUCCESS
      end if
      if (debug)  &
          localrc = ESMF_ReconcileAllRC (vm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end do
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 7 - Complete')
    end if
    if (meminfo) call ESMF_VMLogMemInfo ('after Step 7')

! Clean up

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': At clean up.', ask=.false.)
    end if
    call ESMF_VMBarrier (vm)

    if (associated (buffer_recv)) then
      deallocate (buffer_recv, stat=memstat)
      if (ESMF_LogFoundDeallocError (memstat, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (associated (ids_send)) then
      deallocate (ids_send, itemtypes_send, vmids_send, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    do, i=0, ubound (id_info, 1)
      if (associated (id_info(i)%vmid)) then
        call ESMF_VMIdDestroy (id_info(i)%vmid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
      end if
      if (associated (id_info(i)%id)) then
        deallocate (id_info(i)%id, id_info(i)%vmid, id_info(i)%needed,  &
            stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
      end if
      if (associated (id_info(i)%item_buffer)) then
        deallocate (id_info(i)%item_buffer,  &
            stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
      end if
    end do

    deallocate (id_info, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (associated (siwrap)) then
      deallocate (siwrap, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    deallocate (nitems_buf, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_ReconcileZappedProxies (state, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! 8.) Attributes on the State itself

    if (attreconflag == ESMF_ATTRECONCILE_ON) then
      if (trace) then
        call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
            ': *** Step 8 - Exchange Base Attributes', ask=.false.)
      end if

      call ESMF_VMBarrier (vm)
      call ESMF_ReconcileExchgAttributes (state, vm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    state%statep%reconcileneededflag = .false.

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD // ': Complete!')
      call ESMF_VMBarrier (vm)
    end if
    rc = ESMF_SUCCESS

  end subroutine ESMF_StateReconcile_driver

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileCompareNeeds"
!BOPI
! !IROUTINE: ESMF_ReconcileCompareNeeds
!
! !INTERFACE:
  subroutine ESMF_ReconcileCompareNeeds (vm, id, vmid, id_info, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),     intent(in)   :: vm
    integer,           intent(in)   :: id(0:)
    type(ESMF_VMId),   intent(in)   :: vmid(0:)
    type(ESMF_ReconcileIDInfo), intent(inout) :: id_info(0:)
    integer,           intent(out)  :: rc
!
! !DESCRIPTION:
!
!  Calculates which PETs have items that this PET needs.  When a given item is
!  offered by multiple PETs, a heuristic is used to determine which PET will
!  provide it in order to try to avoid 'hot spotting' the offering PET.
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[id]
!     The object ids of this PETs State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.
!   \item[vmid]
!     The object VMIds of this PETs State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.  Note that since VMId is a deep object class, the vmid array
!     has aliases to existing VMId objects, rather than copies of them.
!   \item[id_info]
!     Array of arrays of global VMId info.  Upon input, the array has a size
!     of numPets, and each element points to Id/VMId arrays.  Returns 'needed'
!     flag for each desired object.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: mypet, npets
    integer :: i, j, k
    logical :: needed
    character(ESMF_MAXSTR) :: msgstring

    type NeedsList_t
      integer          :: id
      type(ESMF_VMId)  :: vmid
      logical, pointer :: offerers(:) => null ()
      integer, pointer :: position(:) => null ()
      type(NeedsList_t), pointer :: next => null ()
    end type

    type(NeedsList_t), pointer :: needs_list

    logical, parameter :: debug = .false.

    ! Sanity checks

    call ESMF_VMGet (vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (size (id) /= size (vmid)) then
      if (ESMF_LogFoundError (ESMF_RC_INTNRL_INCONS, msg='size (id) /= size (vmid)', &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (size (id_info) /= npets) then
      if (ESMF_LogFoundError (ESMF_RC_INTNRL_INCONS, msg='size (id_info) /= npets', &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (debug) then
      print *, '  PET ', mypet, ': id/vmid sizes =', size (id), size (vmid)
    end if

! Check other PETs contents to see if there are objects this PET needs

! When 'needed' ID/VMId pairs are found, create a list of 'offering' PETs who can
! provide it.

    needs_list => null ()

! call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
!     ': computing id_info%needed')
    do, i=0, npets-1
      id_info(i)%needed = .false.
      if (i == mypet) cycle

      do, j = 1, ubound (id_info(i)%id, 1)
        needed = .true.
! print *, '  PET', mypet, ': setting needed to .true.', j, k
        do, k = 1, ubound (id, 1)
          if (id(k) == id_info(i)%id(j)) then
            if (ESMF_VMIdCompare (vmid(k), id_info(i)%vmid(j))) then
! print *, '  PET', mypet, ': setting needed to .false.', j, k
              needed = .false.
              exit
            end if
          end if
        end do

        if (needed) then
! print *, '  PET', mypet, ': calling insert, associated =', associated (needs_list)
          call needs_list_insert (needs_list, pet_1=i,  &
                id_1=id_info(i)%id(j),  &
              vmid_1=id_info(i)%vmid(j),  &
              position=j, rc_1=localrc)
          if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
        end if
      end do
    end do

    if (debug) call needs_list_print (needs_list)

    ! Go through the list of needed IDs/VMIds and select an offerer for each.

    call needs_list_select (needs_list, id_info)

    call needs_list_deallocate (needs_list, rc_1=localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (associated (needs_list)) then
      deallocate (needs_list, stat=memstat)
      if (ESMF_LogFoundDeallocError (memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if


    if (debug) then
      do, j=0, npets-1
        if (j == myPet) then
          do, i=0, ubound (id_info, 1)
            write (msgstring,'(2a,i0,a,i0,a)') ESMF_METHOD,  &
                ': pet', j, ': id_info%needed(',i,') ='
            write (6,*) msgstring, id_info(i)%needed
            call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
          end do
        end if
        call ESMF_VMBarrier (vm)
      end do
    end if

    rc = localrc

  contains

    recursive subroutine needs_list_deallocate (needs_list_1, rc_1)
      type(NeedsList_t),  pointer :: needs_list_1  ! intent(inout)
      integer :: rc_1

      integer :: localrc_1
      integer :: memstat_1

      if (.not. associated (needs_list_1)) then
        rc_1 = ESMF_SUCCESS
        return
      end if

      deallocate (  &
          needs_list_1%offerers,  &
          needs_list_1%position,  &
          stat=memstat_1)
      if (ESMF_LogFoundDeallocError (memstat_1, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc_1)) return

      if (associated (needs_list_1%next)) then
! print *, 'pet', mypet, ': needs_list_deallocate: recursing'
        call needs_list_deallocate (needs_list_1%next, rc_1=localrc_1)
        if (ESMF_LogFoundError (localrc_1, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc_1)) return
        deallocate (needs_list_1%next,  &
            stat=memstat_1)
        if (ESMF_LogFoundDeallocError (memstat_1, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc_1)) return
      end if

      rc_1 = ESMF_SUCCESS
        
    end subroutine needs_list_deallocate

    subroutine needs_list_insert (needs_list_1, pet_1,  &
        id_1, vmid_1, position, rc_1)
      type(NeedsList_t),  pointer :: needs_list_1  ! intent(inout)
      integer,         intent(in) :: pet_1
      integer,         intent(in) :: id_1
      type(ESMF_VMId), intent(in) :: vmid_1
      integer,         intent(in) :: position
      integer,         intent(out):: rc_1

      type(NeedsList_t), pointer :: needslist_p
      integer :: memstat_1

    ! Called when a Id/VMId is offered by some remote PET, and is needed
    ! by the local PET.
    !
    ! If the Id/VMId is not in the needs list, create a new needs_list
    ! entry.   If it is present, add that this PET is also offering it.

      rc_1 = ESMF_SUCCESS

      if (.not. associated (needs_list_1)) then
! print *, 'pet', mypet, ': needs_list_insert: creating needs_list_1'
        allocate (needs_list_1, stat=memstat_1)
        if (ESMF_LogFoundAllocError (memstat_1, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc_1)) return
        allocate (  &
            needs_list_1%offerers(0:npets-1),  &
            needs_list_1%position(0:npets-1),  &
            stat=memstat_1)
        if (ESMF_LogFoundAllocError (memstat_1, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc_1)) return
        needs_list_1%offerers = .false.
        needs_list_1%position = 0
        needs_list_1%id = id_1
        needs_list_1%vmid = vmid_1

        needs_list_1%offerers(pet_1) = .true.
        needs_list_1%position(pet_1) = position
        return
      end if
         
      needslist_p => needs_list_1
      do
        if (id_1 == needslist_p%id .and.  &
            ESMF_VMIdCompare (vmid_1, needslist_p%vmid)) then
! print *, 'pet', mypet, ': needs_list_insert: marking match and returing'
          needslist_p%offerers(pet_1) = .true.
          needslist_p%position(pet_1) = position
          return
        end if

        if (.not. associated (needslist_p%next)) exit
! print *, 'pet', mypet, ': needs_list_insert: advancing to next entry'
        needslist_p => needslist_p%next
      end do

      ! At the end of the list, but no matches found.  So add new entry.

! print *, 'pet', mypet, ': needs_list_insert: creating needslist_p entry'
      allocate (needslist_p%next, stat=memstat_1)
      if (ESMF_LogFoundAllocError (memstat_1, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc_1)) return
      needslist_p => needslist_p%next
      allocate (  &
          needslist_p%offerers(0:npets-1),  &
          needslist_p%position(0:npets-1),  &
          stat=memstat_1)
      if (ESMF_LogFoundAllocError (memstat_1, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc_1)) return
      needslist_p%offerers = .false.
      needslist_p%position = 0
      needslist_p%id = id_1
      needslist_p%vmid = vmid_1

      needslist_p%offerers(pet_1) = .true.
      needslist_p%position(pet_1) = position

    end subroutine needs_list_insert

    subroutine needs_list_print (needs_list_1)
      type(NeedsList_t),  pointer :: needs_list_1  ! intent(in)

      type(NeedsList_t), pointer :: needs_list_next
      integer :: i

      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
      call ESMF_VMBarrier (vm)
      do, i=0, npets-1
        if (i == mypet) then
          if (associated (needs_list_1)) then
            needs_list_next => needs_list_1
            do
              print *, 'PET', mypet, ': offerers =', needs_list_next%offerers,  &
                  ', position =', needs_list_next%position
              if (.not. associated (needs_list_next%next)) exit
              needs_list_next => needs_list_next%next
            end do
          else
            print *, 'PET', mypet, ': Needs list empty'
          end if
          call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
        end if
        call ESMF_VMBarrier (vm)
      end do

    end subroutine needs_list_print

    subroutine needs_list_select (needs_list_1, id_info_1)
      type(needsList_t),          pointer       :: needs_list_1  ! intent(in)
      type(ESMF_ReconcileIDInfo), intent(inout) :: id_info_1(0:)

      ! For each needed Id/VMId pair, select an offering PET and set it in
      ! the id_info_array.

      type(needsList_t), pointer :: needslist_p
      integer :: i, idx
      integer :: offer_first, offer_last
      logical :: found_first
      real :: rand_nos(0:npets-1)

      needslist_p => needs_list_1

#if 1
      ! Try to load distribute by starting at a point in the offerer list
      ! bounded by the first and last offering PETs, and using a hash based
      ! on PETs position in a pseudo-random number table.
      call random_number (rand_nos)

      do
        if (.not. associated (needslist_p)) exit
        ! Find first and last offering PETs
        offer_first = 0
        offer_last = npets-1
        found_first = .false.
        do, i=0, npets-1
          if (needslist_p%offerers(i)) then
            if (.not. found_first) then
              offer_first = i
              found_first = .true.
            end if
            offer_last = i
          end if
        end do

        ! Use a hash to select a starting index between the bounds
        idx = rand_nos(myPet) * (offer_last-offer_first) + offer_first
! print *, 'pet', mypet, ': offer_first, offer_last, starting idx =', offer_first, offer_last, idx
        do, i=0, npets-1
          if (needslist_p%offerers(idx)) then
! print *, 'pet', mypet, ': needs_list_select: setting position', idx, ' to true'
            id_info_1(idx)%needed(needslist_p%position(idx)) = .true.
            exit
          end if
          idx = mod (idx+1, npets)
        end do
        needslist_p => needslist_p%next
      end do

#else
      ! Simply select the first offering PET.
      do
        if (.not. associated (needslist_p)) exit
        do, i=0, npets-1
          if (needslist_p%offerers(i)) then
! print *, 'pet', mypet, ': needs_list_select: setting position', i, ' to true'
            id_info_1(i)%needed(needslist_p%position(i)) = .true.
            exit
          end if
        end do
        needslist_p => needslist_p%next
      end do
#endif
      
    end subroutine needs_list_select

  end subroutine ESMF_ReconcileCompareNeeds
  
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileDeserialize"
!BOPI
! !IROUTINE: ESMF_ReconcileDeserialize
!
! !INTERFACE:
  subroutine ESMF_ReconcileDeserialize (state, vm, obj_buffer, vm_ids,  &
      attreconflag, rc)
!
! !ARGUMENTS:
    type (ESMF_State), intent(inout):: state                                   
    type (ESMF_VM),    intent(in)   :: vm                                   
    character,         pointer      :: obj_buffer(:) ! intent(in)          
    type(ESMF_VMId),   pointer      :: vm_ids(:)     ! intent(in)          
    type(ESMF_AttReconcileFlag),intent(in)   :: attreconflag
    integer,           intent(out)  :: rc
!
! !DESCRIPTION:
!   Builds proxy items for each of the items in the buffer.
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[state]
!     {\tt ESMF\_State} to add proxy objects to.
!   \item[obj_buffer]
!     Buffer of serialized State objects (intent(in))
!   \item[vm_ids]
!     VMIds to be associated with proxy objects (intent(in))
!   \item[attreconflag]
!     Flag to indicate attribute reconciliation.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc

    type(ESMF_FieldBundle) :: fieldbundle
    type(ESMF_Field) :: field
    type(ESMF_Array) :: array
    type(ESMF_ArrayBundle) :: arraybundle
    type(ESMF_State) :: substate

    integer :: buffer_offset
    integer :: needs_count

    integer :: i
    integer :: stateitem_type
    character(ESMF_MAXSTR) :: errstring

    integer :: mypet

    logical, parameter :: debug = .false.
    logical, parameter :: trace = .false.

    ! Sanity checks
    call ESMF_VMGet (vm, localPet=mypet, rc=localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (trace) then
      print *, '    pet', mypet,  &
          ': *** Step 0 - sanity checks'
    end if

    needs_count = transfer (  &
        source=obj_buffer(0:ESMF_SIZEOF_DEFINT-1),  &
        mold  =needs_count)
    if (debug) then
      print *, ESMF_METHOD, ': PET', mypet, ', needs_count =', needs_count
    end if
    buffer_offset = ESMF_SIZEOF_DEFINT
#if defined (ALIGN_FIX)
    buffer_offset = ((buffer_offset+7)/8)*8
#endif

    if (needs_count /= ubound (vm_ids, 1)) then
      print *, ESMF_METHOD, ': pet', mypet,  &
          ':: WARNING - size mismatch between needs_count and vm_ids', needs_count, ubound (vm_ids, 1)
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, msg='needs_count /= ubound (vm_ids, 1)', &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    ! Deserialize
    if (trace) then
      print *, '    pet', mypet,  &
          ': *** Step 1 - main deserialization loop'
    end if
    do, i=1, needs_count

      ! Item type
#if !defined (__G95__)
      stateitem_type = transfer (  &
          source=obj_buffer(buffer_offset:buffer_offset+ESMF_SIZEOF_DEFINT-1), &
          mold  = stateitem_type)
#else
      ! g95 snapshots prior to April 4, 2010 have a bug in TRANSFER.
      ! The following works around it.
      stateitem_type = ESMF_Reconcile_g95_getint (  &
          obj_buffer(buffer_offset:buffer_offset+ESMF_SIZEOF_DEFINT-1))
#endif
      buffer_offset = buffer_offset+ESMF_SIZEOF_DEFINT
#if defined (ALIGN_FIX)
      buffer_offset = ((buffer_offset+7)/8)*8
#endif

      if (debug) then
        print *, ESMF_METHOD,  &
            ': stateitem_type =', stateitem_type, ', offset =', buffer_offset
      end if

      ! Item itself
      select case (stateitem_type)
        case (ESMF_STATEITEM_FIELDBUNDLE%ot)
          if (debug) then
            print *, "deserializing FieldBundle, offset =", buffer_offset
          end if
          fieldbundle = ESMF_FieldBundleDeserialize(obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call c_ESMC_SetVMId(fieldbundle%this, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, fieldbundle, &
              addflag=.true., proxyflag=.true.,  &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_FIELD%ot)
          if (debug) then
            print *, "deserializing Field, offset =", buffer_offset
          end if
          field = ESMF_FieldDeserialize(obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

!!DEBUG "created field, ready to set id and add to local state"
          call c_ESMC_SetVMId(field%ftypep, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, field,      &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_ARRAY%ot)
          if (debug) then
            print *, "    PET", mypet,  &
                ": deserializing Array, offset =", buffer_offset
          end if
          call c_ESMC_ArrayDeserialize(array, obj_buffer, buffer_offset, &
              attreconflag, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Set init code
          call ESMF_ArraySetInitCreated(array, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call c_ESMC_SetVMId(array, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, array,      &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
          if (debug) then
            print *, "deserializing ArrayBundle, offset =", buffer_offset
          end if
          call c_ESMC_ArrayBundleDeserialize(arraybundle, obj_buffer, &
              buffer_offset, attreconflag, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Set init code
          call ESMF_ArrayBundleSetInitCreated(arraybundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call c_ESMC_SetVMId(arraybundle, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, arraybundle, &
              addflag=.true., proxyflag=.true.,  &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_STATE%ot)
          if (debug) then
            print *, "deserializing nested State, offset =", buffer_offset
          end if
          substate = ESMF_StateDeserialize(vm, obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call c_ESMC_SetVMId(substate%statep, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, substate,   &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_UNKNOWN%ot)
          write (errstring, '(a,i0)') 'can''t deserialize unknown type: ', stateitem_type
          if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, msg=errstring, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case default
          write (errstring, '(a,i0)') 'can''t deserialize unsupported type: ', stateitem_type
          if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, msg=errstring, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
      end select

#if defined (ALIGN_FIX)
      buffer_offset = ((buffer_offset+7)/8)*8
#endif

      if (debug) then
        print *, '   buffer offset after item loop =', buffer_offset
      end if

    end do

    if (trace) then
      print *, '    pet', mypet,  &
          ': *** Deserialization complete'
    end if

    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileDeserialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchgAttributes"
!BOPI
! !IROUTINE: ESMF_ReconcileExchgAttributes
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchgAttributes (state, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_State),  intent(inout):: state
    type(ESMF_VM),     intent(in)   :: vm
    integer,           intent(out)  :: rc
!
! !DESCRIPTION:
!
!  Exchange attributes on the base of the State itself.
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[state]
!     {\tt ESMF\_State} to add proxy objects to.
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI
    integer :: localrc
    integer :: memstat

    type(ESMF_Base), pointer :: base
    type(ESMF_Base) :: base_temp
    character, allocatable :: buffer(:), buffer_recv(:)
    integer,   allocatable :: recv_sizes(:), recv_offsets(:)
    integer :: buffer_size(1)

    integer :: i, pass
    integer :: mypet, npets
    integer :: offset
    type(ESMF_InquireFlag) :: inqflag

    logical, parameter :: debug = .false.

    rc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    base => state%statep%base

    ! Serialize the Base attributes
    do, pass = 1, 2
      select case (pass)
      case (1)
      ! Pass 1 finds the required buffer length to serialize any attributes.
        allocate (buffer(4), stat=memstat)  ! Dummy to avoid null pointer derefs
        if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
        inqflag = ESMF_INQUIREONLY

      case (2)
        ! Pass 2 allocates the buffer and performs the actual serialization.
        deallocate (buffer, stat=memstat)
        if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        allocate (buffer(0:offset-1), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
        inqflag = ESMF_NOINQUIRE
      end select

      offset = 0
      call ESMF_BaseSerialize (base, buffer, offset,  &
          ESMF_ATTRECONCILE_ON, inqflag,  &
          rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return

    end do

    ! Exchange serialized buffer sizes
    allocate (recv_sizes(0:npets-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    buffer_size(1) = offset
    call ESMF_VMAllGather (vm,  &
        sendData=buffer_size, recvData=recv_sizes,  &
        count=1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (debug) then
      print *, ESMF_METHOD,  &
          ':  PET', mypet, ': Base sizes   recved are:', recv_sizes
    end if

    ! Exchange serialized buffers

    allocate (  &
        buffer_recv(0:sum (recv_sizes)-1),  &
        recv_offsets(0:npets-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    recv_offsets(0) = 0
    do, i=1, npets-1
      recv_offsets(i) = recv_offsets(i-1)+recv_sizes(i-1)
    end do

    if (debug) then
      print *, ESMF_METHOD,  &
          ':  PET', mypet, ': Base offsets recved are:', recv_offsets
    end if

    call ESMF_VMAllGatherV (vm,  &
        sendData=buffer(:buffer_size(1)-1), sendCount=buffer_size(1),  &
        recvData=buffer_recv, recvCounts=recv_sizes, recvOffsets=recv_offsets,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Update local Base

    do, i=0, npets-1
      if (i /= mypet) then
        base_temp = ESMF_BaseDeserialize (buffer_recv, offset=recv_offsets(i),  &
            attreconflag=ESMF_ATTRECONCILE_ON, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_BaseSetInitCreated(base_temp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call c_ESMC_AttributeCopy(base_temp, base, &
          ESMF_COPY_VALUE, ESMF_ATTTREE_OFF, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      end if
    end do

    ! Reset the change flags in the Attribute hierarchy
    call c_ESMC_AttributeUpdateReset(base, localrc);
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileExchgAttributes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchgIDInfo"
!BOPI
! !IROUTINE: ESMF_ReconcileExchgIDInfo
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchgIDInfo (vm,  &
      nitems_buf, id, vmid, id_info, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),          intent(in)  :: vm
    integer,                intent(in)  :: nitems_buf(0:)
    integer,                intent(in)  :: id(0:)
    type(ESMF_VMId),        intent(in)  :: vmid(0:)
    type(ESMF_ReconcileIDInfo), intent(inout) :: id_info(0:)
    integer,                intent(out) :: rc
!
! !DESCRIPTION:
!
!  Dense AlltoAll of all Ids and VMIds from every PET to every PET.
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[nitems_buf]
!     Number of items on each PET.
!   \item[id]
!     The object ids of this PETs State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.
!   \item[vmid]
!     The object VMIds of this PETs State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.  Note that since VMId is a deep object class, the vmid array
!     has aliases to existing VMId objects, rather than copies of them.
!   \item[id_info]
!     Array of arrays of global VMId info.  Array has a size of numPets, and each
!     element points to Id/VMId arrays.  Also returns which objects are not
!     present on the current PET.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: mypet, npets
    integer :: send_pet
    integer, allocatable :: counts_buf_send(:), counts_buf_recv(:)
    integer, allocatable :: displs_buf_send(:), displs_buf_recv(:)
    integer :: i, ipos
    integer :: memstat

    integer,         allocatable ::   id_recv(:)

    logical, parameter :: debug = .false.

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Sanity checks

    if (size (id) /= size (vmid)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (size (id_info) /= npets) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (size (nitems_buf) /= npets) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    ! Broadcast each Id to all the other PETs.  Since the number of items per
    ! PET can vary, use AllToAllV.

    do, i=0, npets-1
      allocate (  &
          id_info(i)%  id  (0:nitems_buf(i)), &
          id_info(i)%vmid  (0:nitems_buf(i)), &
          id_info(i)%needed(  nitems_buf(i)), &
          stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      call ESMF_VMIdCreate (id_info(i)%vmid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      id_info(i)%needed = .false.
    end do

    ! First, compute counts and displacements for AllToAllV calls.  Note that
    ! sending displacements are always zero, since each PET is broadcasting

    allocate (counts_buf_send(0:npets-1), displs_buf_send(0:npets-1),  &
              counts_buf_recv(0:npets-1), displs_buf_recv(0:npets-1),  &
              stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Add 1 to take the State itself (element 0) into account
    counts_buf_send = nitems_buf(mypet) + 1
    counts_buf_recv = nitems_buf + 1

    displs_buf_send    = 0 ! Always zero, since local PET is broadcasting
    displs_buf_recv(0) = 0
    do, i=1, npets-1
      displs_buf_recv(i) = displs_buf_recv(i-1) + counts_buf_recv(i-1)
    end do

    if (debug) then
      do, i=0, npets-1
        if (i == mypet) then
          write (6,*) ESMF_METHOD, ': pet', mypet, ': counts_buf_send =', counts_buf_send
          write (6,*) ESMF_METHOD, ': pet', mypet, ': displs_buf_send =', displs_buf_send
          write (6,*) ESMF_METHOD, ': pet', mypet, ': counts_buf_recv =', counts_buf_recv
          write (6,*) ESMF_METHOD, ': pet', mypet, ': displs_buf_recv =', displs_buf_recv
          call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
        end if
        call ESMF_VMBarrier (vm)
      end do
    end if

    ! Exchange Ids

    allocate (id_recv(0:sum (counts_buf_recv+1)-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (debug) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ':   Exchanging Ids   (using ESMF_VMAllGatherV)')
    end if
    call ESMF_VMAllGatherV (vm,  &
        sendData=id     , sendCount =size (id),  &
        recvData=id_recv, recvCounts=counts_buf_recv, recvOffsets=displs_buf_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ipos = 0
    do, i=0, npets-1
      id_info(i)%id = id_recv(ipos:ipos+counts_buf_recv(i)-1)
      ipos = ipos + counts_buf_recv(i)
    end do

!    if (debug) then
!      do, j=0, npets-1
!	if (j == myPet) then
!	  do, i=0, ubound (id_info, 1)
!	    write (6,*) 'pet', j, ': id_info%id     =', id_info(i)%id
!	    call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
!	  end do
!	end if
!	call ESMF_VMBarrier (vm)
!      end do
!    end if

    ! Exchange VMIds

#if 0
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ':   Exchanging VMIds (using ESMF_VMAllGatherVMId)')
    end if

    allocate (vmid_recv(0:sum (counts_buf_recv+1)-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    call ESMF_VMIdCreate (vmid_recv, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMAllGatherV (vm,  &
        sendData=vmid, sendCount=size (vmid),  &
        recvData=vmid_recv, recvCounts=counts_buf_recv, recvOffsets=displs_buf_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ipos = 0
    do, i=0, npets-1
      call ESMF_VMIdCopy (  &
          dest  =id_info(i)%vmid,  &
          source=vmid_recv(ipos:ipos+counts_buf_recv(i)-1),  &
          rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      ipos = ipos + counts_buf_recv(i)
    end do
#else
! VMBcastVMId version
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ':   Exchanging VMIds (using ESMF_VMBcastVMId)')
    end if
    if (debug) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ':     VMIdCopying...')
    end if
    call ESMF_VMIdCopy (  &
        dest=id_info(mypet)%vmid,  &
        source=vmid,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, send_pet=0, npets-1
      if (debug) then
        call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
            ':     broadcasting VMId, using rootPet ' // iToS (send_pet),  &
            ask=.false.)
      end if
      call ESMF_VMBcastVMId (vm,  &
          bcstData=id_info(send_pet)%vmid,  &
          count=size (id_info(send_pet)%vmid),  &
          rootPet=send_pet, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end do
#endif

    rc = localrc

  end subroutine ESMF_ReconcileExchgIDInfo
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchgItems"
!BOPI
! !IROUTINE: ESMF_ReconcileExchgItems
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchgItems (vm, id_info, recv_items, recv_buffer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)  :: vm
    type(ESMF_ReconcileIDInfo), intent(in)  :: id_info(0:)
    type(ESMF_CharPtr),         intent(out) :: recv_items(0:)
    character,                  pointer     :: recv_buffer(:) ! intent(out)
    integer,                    intent(out) :: rc
!
! !DESCRIPTION:
!
!  Performs alltoallv communications of serialized data from offering PETs
!  to PETs requesting items.
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[id_info]
!     Array of arrays of global VMId info.
!   \item[recv_items]
!     Array of arrays of serialized item data.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: mypet, npets
    integer :: i
    integer :: itemcount, itemcount_global, itemcount_local
    integer :: offset_pos

    integer,   allocatable :: counts_recv(:),  counts_send(:)
    integer,   allocatable :: offsets_recv(:), offsets_send(:)
    character, allocatable :: buffer_send(:)

    character, pointer :: cptr_tmp(:)

    logical, parameter :: debug = .false.

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (size (id_info) /= npets) then
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="size (id_info) /= npets", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (size (recv_items) /= npets) then
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="size (recv_items) /= npets", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

!   Set up send counts, offsets, and buffer.

    allocate (  &
        counts_send (0:npets-1),  &
        offsets_send(0:npets-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=0, npets-1
      if (associated (id_info(i)%item_buffer)) then
        counts_send(i) = size (id_info(i)%item_buffer)
      else
        counts_send(i) = 0
      end if
    end do

    itemcount_local = counts_send(mypet)
    itemcount_global = sum (counts_send)

    allocate (  &
        buffer_send(0:max (0,itemcount_global-1)),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    offset_pos = 0
    do, i=0, npets-1
      itemcount = counts_send(i)
      offsets_send(i) = offset_pos
      if (associated (id_info(i)%item_buffer)) then
        buffer_send(offset_pos:offset_pos+itemcount-1) = id_info(i)%item_buffer
      end if
      offset_pos = offset_pos + itemcount
    end do

!   Set up recv counts, offsets, and buffer.  Since there will be a different
!   buffer size from each remote PET, an AllToAll communication is necessary
!   for PETs to exchange the buffer sizes they are sending to each other.

    allocate (  &
        counts_recv(0:npets-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMAllToAll (vm,  &
        sendData=counts_send, sendCount=1,  &
        recvData=counts_recv, recvCount=1,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
    if (debug) then
      print *, ESMF_METHOD, ': PET', mypet, ': serialized buffer sizes',  &
          ': counts_send =', counts_send,  &
          ', counts_recv =', counts_recv
    end if

    allocate (  &
        offsets_recv(0:npets-1),  &
        recv_buffer(0:max (0, sum (counts_recv)-1)),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    offset_pos = 0
    do, i=0, npets-1
      itemcount = counts_recv(i)
      offsets_recv(i) = offset_pos
      offset_pos = offset_pos + itemcount
    end do

    ! AlltoAllV

    call ESMF_VMAllToAllV (vm,  &
        sendData=buffer_send, sendCounts=counts_send, sendOffsets=offsets_send,  &
        recvData=recv_buffer, recvCounts=counts_recv, recvOffsets=offsets_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    deallocate (buffer_send, counts_send, offsets_send,  &
        stat=memstat)
    if (ESMF_LogFoundDeallocError (memstat, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Copy recv buffers into recv_items

    do, i=0, npets-1
      itemcount = counts_recv(i)
      if (itemcount > 0) then
        offset_pos = offsets_recv(i)
#if 0
      ! Fortran 2003 version
        recv_items(i)%cptr(0:) => recv_buffer(offset_pos:offset_pos+itemcount-1)
#else
      ! Fortran 90/95 version
        cptr_tmp => recv_buffer(offset_pos:offset_pos+itemcount-1)
      ! cptr_tmp is 1-based.  Convert to 0-based.
        call ptr_assoc_zero (cptr_tmp, itemcount, recv_items(i)%cptr)
!       print *, 'associated cptr(', lbound (recv_items(i)%cptr,1), ':', ubound (recv_items(i)%cptr,1), ')'
#endif
      else
        recv_items(i)%cptr => null ()
      end if
    end do

    rc = localrc

  contains

    subroutine ptr_assoc_zero (cbuffer, itemcount, cptr)
      integer,   intent(in)   :: itemcount
      character, intent(in), target :: cbuffer(0:itemcount-1)
      character, pointer      :: cptr(:)

      cptr => cbuffer

    end subroutine

  end subroutine ESMF_ReconcileExchgItems

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchgNeeds"
!BOPI
! !IROUTINE: ESMF_ReconcileExchgNeeds
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchgNeeds (vm, id_info, recv_needs, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)  :: vm
    type(ESMF_ReconcileIDInfo), intent(in)  :: id_info(0:)
    logical,                    pointer     :: recv_needs(:,:) ! intent(out)
    integer,                    intent(out) :: rc
!
! !DESCRIPTION:
!
!  Performs alltoallv communications from needy PETs to PETs which offer
!  items they need.
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[vm]
!     The current {\tt ESMF\_VM} (virtual machine).
!   \item[id_info]
!     Array of arrays of global VMId info.  The 'needed' flags indicate
!     which items are needed from which offering PETs.
!   \item[recv_needs]
!     Array of needy PETs and their needs.  If a flag is set, the PET
!     needs the item.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: mypet, npets
    integer :: i
    integer :: itemcount, itemcount_global, itemcount_local
    integer :: offset_pos

    integer, allocatable :: counts_recv(:),  counts_send(:)
    integer, allocatable :: offsets_recv(:), offsets_send(:)
    logical, allocatable :: buffer_recv(:),  buffer_send(:)

    character(ESMF_MAXSTR) :: msgstring

    logical, parameter :: debug = .false.

    localrc = ESMF_RC_NOT_IMPL

    if (associated (recv_needs)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (size (id_info) /= npets) then
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="size (id_info) /= npets", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

!   Set up send counts, offsets, and buffer.  Note that each remote PET
!   can have differing numbers of items to offer.

    allocate (  &
        counts_send (0:npets-1),  &
        offsets_send(0:npets-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=0, npets-1
      counts_send(i) = size (id_info(i)%needed)
    end do

    itemcount_local = counts_send(mypet)
    itemcount_global = sum (counts_send)

    allocate (  &
        buffer_send(0:itemcount_global-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    offset_pos = 0
    do, i=0, npets-1
      itemcount = counts_send(i)
      offsets_send(i) = offset_pos
      buffer_send(offset_pos:offset_pos+itemcount-1) = id_info(i)%needed
      offset_pos = offset_pos + itemcount
    end do

!   Each remote PET should return a buffer that is the same
!   size as the number of items on the local PET.  So the recv_needs
!   buffer can be a simple rectangular matrix of which PETs need
!   which of my items.

    allocate (  &
        counts_recv (0:npets-1),  &
        offsets_recv(0:npets-1),  &
        buffer_recv(0:itemcount_local*npets-1),  &
        recv_needs(itemcount_local,0:npets-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    counts_recv = itemcount_local
    offsets_recv = itemcount_local * (/ (i,i=0, npets-1) /)
    buffer_recv = .false.

    ! AlltoAllV

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': calling VMAllToAll')
    end if
    call ESMF_VMAllToAllV (vm,  &
        sendData=buffer_send, sendCounts=counts_send, sendOffsets=offsets_send, &
        recvData=buffer_recv, recvCounts=counts_recv, recvOffsets=offsets_recv, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Copy recv buffers into recv_needs

    do, i=0, npets-1
      itemcount = counts_recv(i)
      offset_pos = offsets_recv(i)
      recv_needs(:,i) = buffer_recv(offset_pos:offset_pos+itemcount-1)
    end do

    if (debug) then
      do, i=0, npets-1
        write (msgstring,'(a,i0,a,i0,a)')  &
            '  PET ', mypet, ': needs that PET ', i, ' requested are:'
        write (6,*) trim (msgstring), recv_needs(:,i)
        call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
      end do
    end if

    rc = localrc

  end subroutine ESMF_ReconcileExchgNeeds

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileGetStateIDInfo"
!BOPI
! !IROUTINE: ESMF_ReconcileGetStateIDInfo
!
! !INTERFACE:
  subroutine ESMF_ReconcileGetStateIDInfo (state, siwrap,  &
      itemtype, id, vmid, rc)
!
! !ARGUMENTS:
    type (ESMF_State), intent(in)  :: state
    type(ESMF_StateItemWrap), pointer :: siwrap(:)! intent(in)
    integer,           pointer     :: itemtype(:) ! intent(out)
    integer,           pointer     :: id(:)       ! intent(out)
    type(ESMF_VMId),   pointer     :: vmid(:)     ! intent(out)
    integer,           intent(out) :: rc
!
! !DESCRIPTION:
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[state]
!     {\tt ESMF\_State} to collect information from.
!   \item[siwrap]
!     Pointers to the items in the State
!   \item[itemtype]
!     The object ids of the State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.
!   \item[id]
!     The object ids of the State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.
!   \item[vmid]
!     The object VMIds of the State itself (in element 0) and the items
!     contained within it.  It does not return the IDs of nested State
!     items.  Note that since VMId is a deep object class, the vmid array
!     has aliases to existing VMId objects, rather than copies of them.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    type(ESMF_Array),           pointer :: arrayp
    type(ESMF_ArrayBundle),     pointer :: abundlep
    type(ESMF_FieldType),       pointer :: fieldp
    type(ESMF_FieldBundleType), pointer :: fbundlep
    type(ESMF_RouteHandle),     pointer :: rhandlep
    type(ESMF_StateClass),      pointer :: statep

    integer :: localrc
    integer :: i
    integer :: memstat
    integer :: nitems

    localrc = ESMF_RC_NOT_IMPL

    if (associated (itemtype) .or. associated (id) .or. associated (vmid)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD,  &
          ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    if (associated (siwrap)) then
      nitems = size (siwrap)
    else
      nitems = 0
    end if

    allocate (  &
        itemtype(0:nitems),  &
              id(0:nitems),  &
            vmid(0:nitems),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

! Element 0s are for the State itself

    itemtype(0) = ESMF_STATEITEM_STATE%ot
    statep => state%statep

    call c_ESMC_GetID(statep, id(0), localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call c_ESMC_GetVMId(statep, vmid(0), localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

! Loop over each item

    do, i=1, nitems

      itemtype(i) = siwrap(i)%si%otype%ot

      select case (itemtype(i))
      case (ESMF_STATEITEM_ARRAY%ot)
        arrayp => siwrap(i)%si%datap%ap

        call c_ESMC_GetID(arrayp, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(arrayp, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return


      case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
        abundlep => siwrap(i)%si%datap%abp

        call c_ESMC_GetID(abundlep, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(abundlep, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return


      case (ESMF_STATEITEM_FIELD%ot)
        fieldp => siwrap(i)%si%datap%fp%ftypep

        call c_ESMC_GetID(fieldp, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(fieldp, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return


      case (ESMF_STATEITEM_FIELDBUNDLE%ot)
        fbundlep => siwrap(i)%si%datap%fbp%this

        call c_ESMC_GetID(fbundlep, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(fbundlep, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return


      case (ESMF_STATEITEM_ROUTEHANDLE%ot)
        rhandlep => siwrap(i)%si%datap%rp

        call c_ESMC_GetID(rhandlep, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(rhandlep, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return


      case (ESMF_STATEITEM_STATE%ot)
        statep => siwrap(i)%si%datap%spp

        call c_ESMC_GetID(statep, id(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

        call c_ESMC_GetVMId(statep, vmid(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return


      case default
        if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
            msg="Unknown State item type", &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

      end select

    end do

    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileGetStateIDInfo
  
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileInitialize"
!BOPI
! !IROUTINE: ESMF_ReconcileInitialize
!
! !INTERFACE:
  subroutine ESMF_ReconcileInitialize (state, vm,  &
      siwrap, nitems_all, rc)
!
! !ARGUMENTS:
    type (ESMF_State), intent(inout)   :: state
    type (ESMF_VM),    intent(in)      :: vm
    type (ESMF_StateItemWrap), pointer :: siwrap(:)     ! intent(out)
    integer,                   pointer :: nitems_all(:) ! intent(out)
    integer,           intent(out)     :: rc
!
! !DESCRIPTION:
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[state]
!     {\tt ESMF\_State} to collect information from.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat
    integer :: nitems_local(1)
    integer :: mypet, npets

    localrc = ESMF_RC_NOT_IMPL

    if (associated (siwrap) .or. associated (nitems_all)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Brute force removal of all existing proxies from the State
    ! to handle the re-reconcile case.  If State items were removed
    ! between reconciles, there should be no proxies for them.
    !
    ! TODO: Consider maintaining a flag in the state.  Perform
    ! a communication step to see if any removals have taken place.
    ! Conditionally zap the proxies depending on whether it is actually
    ! needed.
    call ESMF_ReconcileZapProxies (state, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Obtain local PET item list
    siwrap => null ()
    call ESMF_ContainerGet (state%statep%stateContainer,  &
        itemList=siwrap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (associated (siwrap)) then
      nitems_local(1) = size (siwrap)
    else
      nitems_local(1) = 0
    end if

    ! All PETs send their item counts to all the other PETs for recv array sizing.
    allocate (nitems_all(0:npets-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMAllGather (vm,  &
        sendData=nitems_local, recvData=nitems_all,  &
        count=1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

  end subroutine ESMF_ReconcileInitialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileSerialize"
!BOPI
! !IROUTINE: ESMF_ReconcileSerialize
!
! !INTERFACE:
  subroutine ESMF_ReconcileSerialize (state, vm, siwrap,  &
      needs_list, attreconflag,  &
      id_info, rc)
!
! !ARGUMENTS:
    type (ESMF_State),          intent(in)  :: state
    type (ESMF_VM),             intent(in)  :: vm
    type (ESMF_StateItemWrap),  intent(in)  :: siwrap(:)
    logical,                    intent(in)  :: needs_list(:,0:)
    type(ESMF_AttReconcileFlag),intent(in)  :: attreconflag
    type(ESMF_ReconcileIDInfo), intent(inout) :: id_info(0:)
    integer,                    intent(out) :: rc
!
! !DESCRIPTION:
!
!   The arguments are:                                                     
!   \begin{description}                                                    
!   \item[state]
!     {\tt ESMF\_State} to collect information from.
!   \item[siwrap]
!     State items in the state.
!   \item[needs\_list]
!     List of State items that need to be sent to other PETs
!   \item[attreconflag]
!     Flag to indicate attribute reconciliation.
!   \item[id\_info]
!     IDInfo array containing buffers of serialized State objects (intent(out))
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat

    type PetNeeds_t
      logical :: needed = .false.
      character(1), pointer :: obj_buffer(:) => null ()
      integer :: buffer_size = 0 ! Actual space used in obj_buffer.  May be
                                 ! smaller than size(obj_buffer)
    end type

    type(PetNeeds_t), allocatable :: pet_needs(:)

    character(1), pointer :: obj_buffer(:)
    type(ESMF_StateItem), pointer :: stateitem
    type(ESMF_InquireFlag) :: inqflag
    type(ESMF_State) :: wrapper

    integer :: buffer_offset
    integer :: needs_count
    integer :: item, nitems
    integer :: lbufsize
    integer :: pass

    integer :: i
    integer :: mypet, npets, pet

    logical, parameter :: debug=.false.
    logical, parameter :: trace=.false.

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

! Sanity check: siwrap and needs list must be the same size.

    if (ubound (siwrap, 1) /= ubound (needs_list, 1)) then
      print *, ESMF_METHOD,  &
          ': error - siwrap ubound =', ubound (siwrap, 1),  &
          '/= needs_list =', ubound (needs_list, 1)
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="ubound (siwrap) /= ubound (needs_list)", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if
    nitems = size (siwrap)

    ! Find the union of all the needs for this PET.
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 1 - Find union of needs')
    end if
    allocate (pet_needs(nitems),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=1, nitems
      pet_needs(i)%needed = any (needs_list(i,:))
    end do
    if (debug) then
      print *, '    PET', mypet,  &
          ': needed_items array: ', pet_needs%needed
    end if

  ! Serialize all needed objects
    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 2 - Serialize all needed objects')
    end if
  item_loop:  &
    do, item = 1, nitems

  ! Make two passes through each needed item.  The first time to calculate
  ! the size of the buffer, and the second time to perform the actual
  ! serialization.

      if (.not. pet_needs(item)%needed) cycle item_loop

    pass_loop:  &
      do, pass = 1, 2
        select case (pass)
        ! Pass 1 finds the required buffer length to serialize the item.
        case (1)
          ! Allocate a very small buffer to avoid possible null pointer
          ! references in the serialization routines.
          inqflag = ESMF_INQUIREONLY
          allocate (obj_buffer(0:ESMF_SIZEOF_DEFINT-1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        ! Pass 2 performs the actual serialization.
        case (2)
          inqflag = ESMF_NOINQUIRE
          deallocate (obj_buffer, stat=memstat)
          if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          if (debug) then
            print *, ESMF_METHOD, ': allocating obj_buffer bounds = (0:', buffer_offset-1, ')'
          end if

          allocate (obj_buffer(0:buffer_offset-1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        end select

        lbufsize = size (obj_buffer)

        stateitem => siwrap(item)%si

        ! serialize item type
        if (inqflag == ESMF_NOINQUIRE) then
          obj_buffer(0:ESMF_SIZEOF_DEFINT-1) = transfer ( &
              source=stateitem%otype%ot,  &
              mold  =obj_buffer)
        end if
        buffer_offset = ESMF_SIZEOF_DEFINT
#if defined (ALIGN_FIX)
        buffer_offset = ((buffer_offset+7)/8)*8
#endif

        ! serialize item itself
        select case (stateitem%otype%ot)

          case (ESMF_STATEITEM_FIELDBUNDLE%ot)
            if (debug) then
              print *, '    PET', mypet,  &
                  ': serializing FieldBundle, pass =', pass, ', offset =', buffer_offset
            end if
            call ESMF_FieldBundleSerialize(stateitem%datap%fbp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_FIELD%ot)
            if (debug) then
              print *, '    PET', mypet,  &
                  ': serializing Field, pass =', pass, ', offset =', buffer_offset
            end if
            call ESMF_FieldSerialize(stateitem%datap%fp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_ARRAY%ot)
            if (debug) then
              print *, '    PET', mypet,  &
                  ': serialized Array, pass =', pass, ', offset =', buffer_offset
            end if
            call c_ESMC_ArraySerialize(stateitem%datap%ap,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag, inqflag,  &
                localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
            if (debug) then
              print *, '    PET', mypet,  &
                  ': serializing ArrayBundle, pass =', pass, ', offset =', buffer_offset
            end if
            call c_ESMC_ArrayBundleSerialize(stateitem%datap%abp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag, inqflag,  &
                localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_STATE%ot)
            if (debug) then
              print *, '    PET', mypet,  &
                  ': serializing subState, pass =', pass, ', offset =', buffer_offset
            end if
            wrapper%statep => stateitem%datap%spp
            ESMF_INIT_SET_CREATED(wrapper)
            call ESMF_StateSerialize(wrapper,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case (ESMF_STATEITEM_ROUTEHANDLE%ot)
            if (debug) then
              print *, '    PET', mypet,  &
                  ': ignoring RouteHandle, pass =', pass
            end if
          ! Do nothing for RouteHandles.  There is no need to reconcile them.


          case (ESMF_STATEITEM_UNKNOWN%ot)
            if (debug) then
              print *, ESMF_METHOD, ': serializing unknown: ', trim (stateitem%namep)
            end if
            call c_ESMC_StringSerialize(stateitem%namep,  &
                obj_buffer, lbufsize, buffer_offset,  &
                inqflag,  &
                localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT,  &
                rcToReturn=rc)) return

          case default
            localrc = ESMF_RC_INTNRL_INCONS
            if (debug) then
              print *, '    PET', mypet,  &
                  ': serialization error in default case.  Returning ESMF_RC_INTNRL_INCONS'
            end if

        end select

        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return

#if defined (ALIGN_FIX)
        buffer_offset = ((buffer_offset+7)/8)*8
#endif

        if (debug) then
          print *, '    PET', mypet,  &
              ': item serialized, pass =', pass, ', new offset =', buffer_offset
        end if

      end do pass_loop

      pet_needs(item)%obj_buffer => obj_buffer
      pet_needs(item)%buffer_size = buffer_offset
      obj_buffer => null ()

    end do item_loop

! For each PET, create a buffer containing its serialized needs.

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 3 - Create per-PET serialized buffers')
    end if
    do, pet=0, npets-1
      needs_count = count (needs_list(:,pet))
      if (debug) then
        print *, '    PET', mypet,  &
            ': needs_count =', needs_count, ', for PET', pet
      end if
      if (needs_count == 0) then
        id_info(pet)%item_buffer => null ()
        cycle
      end if

      buffer_offset = ESMF_SIZEOF_DEFINT ! space for needs_count
#if defined (ALIGN_FIX)
      buffer_offset = ((buffer_offset+7)/8)*8
#endif
      do, item=1, nitems
        if (needs_list(item, pet))  &
          buffer_offset = buffer_offset + pet_needs(item)%buffer_size
      end do

      if (debug) then
        print *, '    PET', mypet,  &
            ': computed buffer_offset =', buffer_offset, ', for PET', pet
      end if

      allocate (id_info(pet)%item_buffer(0:buffer_offset-1),  &
          stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      obj_buffer => id_info(pet)%item_buffer

      obj_buffer(0:ESMF_SIZEOF_DEFINT-1) = transfer (  &
          source=needs_count,  &
          mold  =obj_buffer(0:ESMF_SIZEOF_DEFINT-1))

      buffer_offset = ESMF_SIZEOF_DEFINT ! space for needs_count
#if defined (ALIGN_FIX)
      buffer_offset = ((buffer_offset+7)/8)*8
#endif
      do, item=1, nitems
        if (.not. needs_list(item, pet)) cycle
        lbufsize = pet_needs(item)%buffer_size
        if (lbufsize == 0) cycle

        if (debug) then
          print *, '    PET', mypet,  &
              ': packing at buffer_offset =', buffer_offset, ', for PET', pet,  &
              ', item =', item
        end if
        obj_buffer(buffer_offset:buffer_offset+lbufsize-1) =  &
            pet_needs(item)%obj_buffer(:lbufsize-1)
        buffer_offset = buffer_offset + lbufsize
      end do ! items

    end do ! pets

    if (trace) then
      call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
          ': *** Step 4 - Deallocate memory')
    end if

    if (allocated (pet_needs)) then
      do, i=1, nitems
        if (associated (pet_needs(i)%obj_buffer)) then
          deallocate (pet_needs(i)%obj_buffer, stat=memstat)
          if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return
        end if
      end do

      deallocate (pet_needs, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    rc = ESMF_SUCCESS

  end subroutine ESMF_ReconcileSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileZapProxies"
!BOPI
! !IROUTINE: ESMF_ReconcileZapProxies -- Zap proxies from State
!
! !INTERFACE:
    subroutine ESMF_ReconcileZapProxies(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to clear proxies out of.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc, i
      type(ESMF_StateClass),    pointer :: stypep
      type(ESMF_StateItemWrap), pointer :: itemList(:)
      character(len=ESMF_MAXSTR) :: thisname

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      stypep => state%statep

      itemList => null ()
      call ESMF_ContainerGet(container=stypep%stateContainer, itemList=itemList, &
          rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      if (associated(itemList)) then
        do i=1, size(itemList)
          if (itemList(i)%si%proxyFlag) then
            call ESMF_StateItemGet(itemList(i)%si, name=thisname, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_StateRemove (state, itemNameList=(/thisname/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            itemList(i)%si => null ()
          end if
        end do
      endif

      stypep%zapList => itemList ! hang on for ESMF_ReconcileZappedProxies()

      if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_ReconcileZapProxies

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileZappedProxies"
!BOPI
! !IROUTINE: ESMF_ReconcileZappedProxies -- Conditionally restore zapped proxies
!
! !INTERFACE:
  subroutine ESMF_ReconcileZappedProxies(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout)         :: state
      integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State}
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc, i, k
    integer :: memstat
    type(ESMF_StateClass),    pointer :: stypep
    type(ESMF_StateItemWrap), pointer :: itemList(:)
    type(ESMF_StateItemWrap), pointer :: zapList(:)
    character(len=ESMF_MAXSTR)  :: thisname
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Field)            :: tempField

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    stypep => state%statep
    zapList => stypep%zapList
    
    itemList => null ()
    call ESMF_ContainerGet(container=stypep%stateContainer, itemList=itemList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

!print *, "ESMF_ReconcileZappedProxies() looking"

    if (associated(itemList).and.associated(zapList)) then
      do i=1, size(itemList)
        if (itemList(i)%si%proxyFlag .and. &
            itemList(i)%si%otype==ESMF_STATEITEM_FIELD) then
          call ESMF_StateItemGet(itemList(i)%si, name=thisname, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          
          do k=1, size(zapList)
            if (associated (zapList(k)%si)) then
              if (zapList(k)%si%otype==ESMF_STATEITEM_FIELD) then
                call ESMF_FieldGet(zapList(k)%si%datap%fp, name=name, rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) &
                    return
!print *, "ESMF_ReconcileZappedProxies() checking: ", trim(name)
                if (name == thisname) then
!print *, "ESMF_ReconcileZappedProxies() found: ", trim(name)
                ! Bend pointers and copy contents to result in the desired
                ! behavior for re-reconcile. From a user perspective of
                ! Reconcile() proxies should persist when a State is 
                ! re-reconciled, and the same proxies are needed. Basically
                ! a user should be able to hang on to a proxy.
                  tempField%ftypep => itemList(i)%si%datap%fp%ftypep
                  zapList(k)%si%datap%fp%ftypep = itemList(i)%si%datap%fp%ftypep
                  itemList(i)%si%datap%fp%ftypep => zapList(k)%si%datap%fp%ftypep
                  zapList(k)%si%datap%fp%ftypep => tempField%ftypep
                end if
              end if
            end if
          end do ! k
        end if
      end do ! i
    endif

    if (associated (itemList)) then
      deallocate(itemList, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    if (associated (zaplist)) then
      deallocate(zaplist, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_ReconcileZappedProxies

!------------------------------------------------------------------------------
#if defined (__G95__)
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_Reconcileg95_getint"
    function ESMF_Reconcile_g95_getint (bytes) result (int)
      character, intent(in) :: bytes(:)
      integer :: int

      ! Workaround routine for g95 TRANSFER bug.

      int = transfer (bytes, int)

    end function ESMF_Reconcile_g95_getint
#endif

!------------------------------------------------------------------------------
! Debugging and support procedures
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileAllRC"
  function ESMF_ReconcileAllRC (vm, rc) result (rc_return)
    type(ESMF_VM), intent(in) :: vm
    integer,       intent(in) :: rc
    integer                   :: rc_return

    integer :: rc_send(1)
    integer, allocatable :: rc_all(:)
    integer :: mypet, npets

    call ESMF_VMGet(vm, localpet=mypet, petCount=npets)
    allocate (rc_all(npets))

    rc_send = rc
    call ESMF_VMGather (vm,  &
        sendData=rc_send, recvData=rc_all, count=1,  &
        rootPet=0)
    call ESMF_VMBroadcast (vm,  &
        bcstData=rc_all, count=npets,  &
        rootPet=0)

    rc_return = rc
    if (any (rc_all /= ESMF_SUCCESS)) then
      rc_return = merge (ESMF_FAILURE, rc, rc == ESMF_SUCCESS)
    end if


  end function ESMF_ReconcileAllRC

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileDebugPrint"
  subroutine ESMF_ReconcileDebugPrint (text, multitext, ask, rc)
    use ESMF_IOUtilMod
    character(*), intent(in),  optional :: text
    character(*), intent(in),  optional :: multitext
    logical,      intent(in),  optional :: ask
    integer,      intent(out), optional :: rc

    type(ESMF_VM) :: vm
    integer :: localrc
    integer :: mypet, npets
    character(16) :: answer
    character(10) :: time
    logical :: localask

    call ESMF_VMGetCurrent(vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    localask = .false.
    if (present (ask)) then
      localask = ask
    end if

    if (present (text)) then
      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
      call ESMF_VMBarrier (vm)
      if (mypet == 0) then
        call date_and_time (time=time)
        write (ESMF_UtilIOStdout,*)  &
          time(1:2), ':', time(3:4), ':', time(5:), ': ', text
        call ESMF_UtilIOUnitflush (ESMF_UtilIOStdout)
      end if
      call ESMF_VMBarrier (vm)
    end if

    if (present (multitext)) then
      write (ESMF_UtilIOStdout,*) multitext
      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
      call ESMF_VMBarrier (vm)
    end if

    if (localask) then
      if (mypet == 0) then
        write (ESMF_UtilIOStdout,'(a)') 'Proceed?'
        call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout)
        read (ESMF_UtilIOStdin,'(a)') answer
      end if
      call ESMF_VMBarrier (vm)
    end if

  end subroutine ESMF_ReconcileDebugPrint

  pure function iTos_len (i)
    integer, intent(in) :: i
    integer :: iTos_len

    character(16) :: string

    write (string,'(i16)') i
    iTos_len = len_trim (adjustl (string))

  end function iTos_len

  function iTos (i)
    integer, intent(in) :: i
    character(iTos_len (i)) :: iTos

    character(16) :: string

    write (string,'(i16)') i
    iTos = adjustl (string)

  end function iTos

end module ESMF_StateReconcile2Mod
