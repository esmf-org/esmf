! $Id: ESMF_StateReconcile2.F90,v 1.5 2012/03/22 20:20:10 w6ws Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research, 
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

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateReconcileMod - Data exchange within a component
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran function and subroutine 
!  interfaces to ensure that {\tt ESMF\_State} data is consistent across
!  all PETs.  The intended use is by components that have subcomponents 
!  which run on subsets of the coupler's PET list.
!  Objects that have been created on only a subset of the PETs cannot be
!  identified to methods like Regrid or Redistribution since they have no 
!  valid handles to identify them.  The code here communicates the missing
!  object information to other PETs in the current VM.
!
!
! !USES:
  use ESMF_BaseMod
  use ESMF_InitMacrosMod
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
  use ESMF_FieldBundleMod
  use ESMF_RHandleMod

  implicit none
  private

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

  public :: ESMF_StateReconcile2 ! make State consistent for concurrent apps

  ! These are only public for unit testing.  They are not intended
  ! to be called by ESMF users.
  public :: ESMF_ReconcileDeserialize, ESMF_ReconcileSerialize
  public :: ESMF_ReconcileSendItems

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
  '$Id: ESMF_StateReconcile2.F90,v 1.5 2012/03/22 20:20:10 w6ws Exp $'
!==============================================================================

! !PRIVATE TYPES:
!------------------------------------------------------------------------------
! ! ESMF_ItemBuffer
  type ESMF_ItemBuffer
    character, pointer :: item_buffer(:) => null ()
  end type

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

contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReconcile2"
!BOP
! !IROUTINE: ESMF_StateReconcile -- Reconcile State data across all PETs in a VM
!
! !INTERFACE:
  subroutine ESMF_StateReconcile2(state, vm, attreconflag, rc)
!
! !ARGUMENTS:
    type(ESMF_State),		 intent(inout)         :: state
    type(ESMF_VM),		 intent(in),  optional :: vm
    type(ESMF_AttReconcileFlag), intent(in),  optional :: attreconflag
    integer,			 intent(out), optional :: rc		   
!
!
! !DESCRIPTION:
!     Must be called for any {\tt ESMF\_State} which contains ESMF objects
!     that have not been created on all the {\tt PET}s of the currently
!     running {\tt ESMF\_Component}.  
!     For example, if a coupler is operating on data
!     which was created by another component that ran on only a subset
!     of the coupler's {\tt PET}s, the coupler must make this call first
!     before operating on any data inside that {\tt ESMF\_State}.
!     After calling {\tt ESMF\_StateReconcile} all {\tt PET}s will have
!     a common view of all objects contained in this {\tt ESMF\_State}.
!     The option to reconcile the metadata associated with the objects
!     contained in this {\tt ESMF\_State} also exists.  The default behavior
!     for this capability is to {\it not} reconcile metadata unless told
!     otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to reconcile.
!     \item[{[vm]}]
!       {\tt ESMF\_VM} for this {\tt ESMF\_Component}.  By default, it set to the current vm.
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

! do, i=0, npets-1
!   if (i == mypet) then
!     call ESMF_StatePrint (state)
!   end if
!   call ESMF_VMBarrier (vm)
! end do

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

  end subroutine ESMF_StateReconcile2

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateReconcile_driver"
!BOPI
! !IROUTINE: ESMF_StateReconcile_driver
!
! !INTERFACE:
    recursive subroutine ESMF_StateReconcile_driver (state, vm, attreconflag, rc)
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
    integer :: mypet, npets, send_pet
    logical :: i_send, i_recv

    integer :: nitems       ! # of items contained within the local State
    integer, pointer :: nitems_buf(:)
    type (ESMF_StateItemWrap), pointer :: siwrap(:)

    integer,         pointer :: ids_send(:), itemtypes_send(:)
    type(ESMF_VMId), pointer :: vmids_send(:)
    integer,     allocatable :: ids_recv(:), itemtypes_recv(:)
    type(ESMF_VMId), pointer :: vmids_recv(:)

    type(ESMF_ReconcileIDInfo), pointer :: id_info(:)

    logical, pointer :: recvd_needs_matrix(:,:)

    type(ESMF_Itembuffer), pointer :: items_recv(:)

    integer :: i, j

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! 0.) Interchange item counts between PETs.  Set up counts/displacements
call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': *** Step 0: Initialize item counts and siwrappers')
    siwrap     => null ()
    nitems_buf => null ()
    call ESMF_ReconcileInitialize (state, vm,  &
        siwrap=siwrap, nitems_all=nitems_buf, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

print *, ESMF_METHOD, ': siwrap(', lbound (siwrap,1), ',', ubound (siwrap,1), ')'
print *, ESMF_METHOD, ': nitems_buf(', lbound (nitems_buf,1), ',', ubound (nitems_buf,1), ')'

    ! 1.) Each PET constructs its send arrays containing local Id
    ! and VMId info for each object contained in the State
    ! Note that element zero is reserved for the State itself.
call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': *** Step 1 - Build send arrays')
    itemtypes_send => null ()
    ids_send   => null ()
    vmids_send => null ()      
    call ESMF_ReconcileGetStateIDInfo (state, siwrap,  &
	itemtype=itemtypes_send,  &
	  id=  ids_send,  &
	vmid=vmids_send,  &
	rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! 2.) All PETs send their items Ids and VMIds to all the other PETs,
    ! then create local directories of which PETs have which ids/VMIds.
call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': *** Step 2 - Exchange Ids/VMIds')

    id_info => null ()
    call ESMF_ReconcileExchangeIDInfo (vm,  &
        nitems_buf=nitems_buf,  &
	  id=  ids_send,  &
	vmid=vmids_send,  &
        id_info=id_info, &
	rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return


! At this point, each PET knows what items can be found on all of
! the other PETs.  The id_info array has global PET info in it.

    ! 3.) Construct needs list.  Receiving PETs compare IDs and VMIds
    ! in their send ID/VMId array with what was received from the
    ! currently-being-processed sending PET.  Note that multiple PETs
    ! can 'offer' an item.

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': *** Step 3 - Compare and create needs arrays')

    call ESMF_ReconcileCompareNeeds (vm,  &
	  id=  ids_send,  &
	vmid=vmids_send,  &
        id_info=id_info,  &
	rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return


    ! 4.) Communicate needs back to the offering PETs

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': *** Step 4 - Exchange needs')

    ! Send to each offering PET a buffer containing 'needed' array
    ! specifying which items are needed.  The array is the same size as,
    ! and corresponds to, the ID and VMId arrays that were previously
    ! offered.

    recvd_needs_matrix => null ()
    call ESMF_ReconcileExchangeNeeds (vm,  &
        id_info=id_info,  &
        recv_needs=recvd_needs_matrix,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return


    ! 5.) Serialized needed objects

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': *** Step 5 - Serialize needs')
    do, i=0, npets-1
write (6,*) '  PET', mypet, ': needs that PET', i, ' requested are:', recvd_needs_matrix(:,i)
flush (6)
      id_info(i)%item_buffer => null ()
      call ESMF_ReconcileSerialize (state, siwrap, &
	  needs_list=recvd_needs_matrix(:,i), attreconflag=attreconflag,  &
	  obj_buffer=id_info(i)%item_buffer, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
	  ESMF_CONTEXT,  &
	  rcToReturn=rc)) return
    end do


    ! 6.) Send/receive serialized objects to whoever needed them

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': *** Step 6 - Exchange serialized objects')

    items_recv => null ()
    call ESMF_ReconcileSendItems (vm,  &
        id_info=id_info,  &
        recv_items=items_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return


    ! 7.) Deserialize received objects and create proxies (recurse on
    !     nested States as needed

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': *** Step 7 - Deserialize needs')

! !!! DO NOT REMOVE THIS BARRIER !!!
! All serialization and communications must be complete before
! deserialization can begin.

    call ESMF_VMBarrier (vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
	ESMF_CONTEXT,  &
	rcToReturn=rc)) return

    do, i=0, npets-1
      if (associated (items_recv(i)%item_buffer)) then
	call ESMF_ReconcileDeserialize (state, vm,  &
            obj_buffer=items_recv(i)%item_buffer,  &
            vm_ids=id_info(i)%vmid,  &
            attreconflag=attreconflag, rc=localrc)
	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
      end if
    end do

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': Deallocating recvd_needs_matrix')
      deallocate (recvd_needs_matrix, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return

#if !defined (ALIAS_VMID)
call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': Destroying vmids_recv')
    call ESMF_VMIdDestroy (vmids_recv, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
#endif

    deallocate (ids_send, itemtypes_send, vmids_send, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=0, ubound (id_info, 1)
      deallocate (id_info(i)%id, id_info(i)%vmid, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
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

    state%statep%reconcileneededflag = .false.

call ESMF_ReconcileDebugPrint (ESMF_METHOD // ': at the end without crashing!')
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
    integer :: mypet, npets
    integer :: i, j, k
    logical :: needed

    type NeedsList_t
      integer          :: id
      type(ESMF_VMId)  :: vmid
      logical, pointer :: offerers(:) => null ()
      integer, pointer :: position(:) => null ()
      type(NeedsList_t), pointer :: next => null ()
    end type

    type(NeedsList_t), pointer :: needs_list

! Check other PETs contents to see if there are objects this PET needs

    call ESMF_VMGet (vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (npets /= size (id_info)) then
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, msg='size (id_info) /= npets', &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    ! TODO: Sanity check

! When 'needed' ID/VMId pairs are found, create a list of 'offering' PETs who can
! provide it.

    needs_list => null ()

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': computing id_info%needed')
    do, i=0, npets-1
      id_info(i)%needed = .false.
      if (i == mypet) cycle
      do, j = 1, ubound (id, 1)
        do, k = 1, ubound (id_info(i)%id, 1)
          needed = id(j) /= id_info(i)%id(k)
          needed = needed .or. ESMF_VMIdCompare (vmid(j), id_info(i)%vmid(k))
          if (needed) then
            call needs_list_insert (needs_list, pet_1=i,  &
                id_1=id_info(i)%id(k), vmid_1=id_info(i)%vmid(k),  &
                position=k)
          end if
!          id_info(i)%needed(k) = needed
        end do
      end do
    end do

    ! Go through the list of needed IDs/VMIds and select an offerer for each.

    call needs_list_select (needs_list, id_info)

    ! TODO: clean up
    ! deallocate needs_list

do, j=0, npets-1
  if (j == myPet) then
    do, i=0, ubound (id_info, 1)
      write (6,*) ESMF_METHOD, ': pet', j, ': id_info%needed =', id_info(i)%needed
      flush (6)
    end do
  end if
  call ESMF_VMBarrier (vm)
end do

    rc = localrc

  contains

    subroutine needs_list_insert (needs_list_1, pet_1,  &
        id_1, vmid_1, position)
      type(NeedsList_t),  pointer :: needs_list_1  ! intent(inout)
      integer,         intent(in) :: pet_1
      integer,         intent(in) :: id_1
      type(ESMF_VMId), intent(in) :: vmid_1
      integer,         intent(in) :: position

      type(NeedsList_t), pointer :: needslist_p
      logical :: needs
      integer :: memstat

    ! Called when a Id/VMId is offered by some remote PET, and is needed
    ! by the local PET.
    !
    ! If the Id/VMId is not in the needs list, create a new needs_list
    ! entry.   If it is present, add that this PET is also offering it.

      needslist_p => needs_list_1
      do
        if (.not. associated (needslist_p)) then
          ! At end of list, so create a new entry
          allocate (needslist_p, stat=memstat)
          allocate (  &
              needslist_p%offerers(0:npets-1),  &
              needslist_p%position(0:npets-1),  &
              stat=memstat)
          needslist_p%offerers = .false.
          needslist_p%position = 0
          needslist_p%offerers(pet_1) = .true.
          needslist_p%position(pet_1) = position
          needslist_p%id = id_1
          call ESMF_VMIdCreate (needslist_p%vmid)
          needslist_p%vmid = vmid_1
          return
        else
          if (id_1 == needslist_p%id) then
            if (ESMF_VMIdCompare (vmid_1, needslist_p%vmid)) then
              needslist_p%offerers(pet_1) = .true.
              needslist_p%position(pet_1) = position
              return
            end if
          end if
        end if
        needslist_p => needslist_p%next
      end do

    end subroutine needs_list_insert

    subroutine needs_list_select (needs_list_1, id_info_1)
      type(needsList_t),          pointer       :: needs_list_1  ! intent(in)
      type(ESMF_ReconcileIDInfo), intent(inout) :: id_info_1(0:)

      ! For each needed Id/VMId pair, select an offering PET and set it in
      ! the id_info_array.

      ! TODO: Initially, simply select the first offering PET.  Eventually
      ! try to load balance by looking for nearest offering neighbor, etc.

      type(needsList_t), pointer :: needslist_p
      integer :: i

      needslist_p => needs_list_1
      do
        if (.not. associated (needslist_p)) exit
        do, i=0, npets-1
          if (needslist_p%offerers(i)) then
            id_info_1(i)%needed(needslist_p%position(i)) = .true.
            exit
          end if
        end do
        needslist_p => needslist_p%next
      end do
      
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
    integer :: memstat

    type(ESMF_Base) :: base
    type(ESMF_FieldBundle) :: fieldbundle
    type(ESMF_Field) :: field
    type(ESMF_Array) :: array
    type(ESMF_ArrayBundle) :: arraybundle
    type(ESMF_State) :: substate

    integer :: buffer_offset
    integer :: needs_count
    integer, allocatable :: offsets(:)

    integer :: i
    integer :: stateitem_type

    needs_count = transfer (obj_buffer(0:ESMF_SIZEOF_DEFINT-1), needs_count)
!    allocate (offsets(0:needs_count))
!    offsets = transfer (obj_buffer(0:(needs_count+1)*ESMF_SIZEOF_DEFINT-1), offsets)
!    buffer_offset = (needs_count+1)*ESMF_SIZEOF_DEFINT
!    print *, '   offsets =', offsets, ', initial buffer offset =', buffer_offset
    print *, '  needs_count =', needs_count
    buffer_offset = ESMF_SIZEOF_DEFINT

    if (needs_count /= size (vm_ids)) then
      print *, ESMF_METHOD, ': WARNING - size mismatch between needs_count and vm_ids'
    end if

    if (attreconflag == ESMF_ATTRECONCILE_ON) then
      ! TODO: For the State itself
    end if

    do, i=1, needs_count

      ! Item type
      stateitem_type =  &
          transfer (obj_buffer(buffer_offset:buffer_offset+ESMF_SIZEOF_DEFINT-1), stateitem_type)
      buffer_offset = buffer_offset+ESMF_SIZEOF_DEFINT

      ! Item itself
      select case (stateitem_type)
        case (ESMF_STATEITEM_FIELDBUNDLE%ot)
print *, "deserializing fieldbundle"
          fieldbundle = ESMF_FieldBundleDeserialize(obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="nested fieldbundle deserialize", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call c_ESMC_SetVMId(fieldbundle%this, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, msg="nested fieldbundle SetVMId call", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, fieldbundle, &
              addflag=.true., proxyflag=.true.,  &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="nested fieldbundle add to local state", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_FIELD%ot)
print *, "deserializing field"
          field = ESMF_FieldDeserialize(obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Field deserialize", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

!!DEBUG "created field, ready to set id and add to local state"
          call c_ESMC_SetVMId(field%ftypep, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Field SetVMId call", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, field,      &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Field add to local state", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_ARRAY%ot)
print *, "deserializing array"
          call c_ESMC_ArrayDeserialize(array, obj_buffer, buffer_offset, &
              attreconflag, localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Array deserialize", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Set init code
          call ESMF_ArraySetInitCreated(array, rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="Array SetInit call", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call c_ESMC_SetVMId(array, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Array SetVMId call", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, array,      &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Array add to local state", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
print *, "deserializing arraybundle"
          call c_ESMC_ArrayBundleDeserialize(arraybundle, obj_buffer, &
              buffer_offset, attreconflag, localrc)
          if (ESMF_LogFoundError(localrc, msg="nested arraybundle deserialize", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          ! Set init code
          call ESMF_ArrayBundleSetInitCreated(arraybundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="arraybundle SetInit call", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call c_ESMC_SetVMId(arraybundle, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, msg="nested arraybundle SetVMId call", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, arraybundle, &
              addflag=.true., proxyflag=.true.,  &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="nested arraybundle add to local state", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

! TODO: Routehandle...

        case (ESMF_STATEITEM_STATE%ot)
print *, "deserializing substate"
          substate = ESMF_StateDeserialize(vm, obj_buffer, buffer_offset, &
              attreconflag=attreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Substate deserialize", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call c_ESMC_SetVMId(substate%statep, vm_ids(i), localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Substate SetVMId call", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

          call ESMF_StateAdd(state, substate,   &
              addflag=.true., proxyflag=.true., &
              rc=localrc)
          if (ESMF_LogFoundError(localrc, msg="nested Substate add to local state", &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

        case (ESMF_STATEITEM_UNKNOWN%ot)
print *, "deserializing unknown type"
          localrc = ESMF_RC_INTNRL_INCONS

        case default
          localrc = ESMF_RC_INTNRL_INCONS
print *, "deserialization error in default case.  Returning ESMF_RC_INTNRL_INCONS"
      end select

!      buffer_offset = buffer_offset + offsets(i)
    end do

  end subroutine ESMF_ReconcileDeserialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchangeIDInfo"
!BOPI
! !IROUTINE: ESMF_ReconcileExchangeIDInfo
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchangeIDInfo (vm,  &
      nitems_buf, id, vmid, id_info, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),          intent(in)  :: vm
    integer,                intent(in)  :: nitems_buf(0:)
    integer,                intent(in)  :: id(0:)
    type(ESMF_VMId),        intent(in)  :: vmid(0:)
    type(ESMF_ReconcileIDInfo), pointer :: id_info(:) ! intent(out)
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
    integer :: recv_count, recv_offset
    integer :: i, j, k, ipos
    integer :: memstat
    logical :: needed

    integer, allocatable :: id_recv(:)
    type(ESMF_VMId), allocatable :: vmid_recv(:)

    localrc = ESMF_RC_NOT_IMPL

    ! Sanity checks

    if (size (id) /= size (vmid)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    if (associated (id_info)) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    call ESMF_VMGet(vm, localPet=mypet, petCount=npets, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    if (size (nitems_buf) /= npets) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

    ! Broadcast each Id to all the other PETs.  Since the number of items per
    ! PET can vary, use AllToAllV.

    allocate (id_info(0:npets-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=0, npets-1
      allocate (  &
          id_info(i)%  id  (0:nitems_buf(i)), &
          id_info(i)%vmid  (0:nitems_buf(i)), &
          id_info(i)%needed(  nitems_buf(i)), &
          stat=memstat)
      if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
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
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Add 1 to take the State itself (element 0) into account
    counts_buf_send = nitems_buf(mypet) + 1
    counts_buf_recv = nitems_buf + 1

    displs_buf_send    = 0 ! Always zero, since we are broadcasting
    displs_buf_recv(0) = 0
    do, i=1, npets-1
      displs_buf_recv(i) = displs_buf_recv(i-1) + counts_buf_recv(i-1)
    end do

! do, i=0, npets-1
!   if (i == mypet) then
!     write (6,*) ESMF_METHOD, ': pet', mypet, ': counts_buf_recv =', counts_buf_recv
!     write (6,*) ESMF_METHOD, ': pet', mypet, ': displs_buf_recv =', displs_buf_recv
!     flush (6)
!   end if
!   call ESMF_VMBarrier (vm)
! end do

    !  - Send all IDs to all PETs

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': AllToAllVing Ids on PET ' // iTos (mypet))

    allocate (id_recv(0:sum (counts_buf_recv+1)-1),  &
        stat=memstat)
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMAllToAllV (vm,  &
       id     , counts_buf_send, displs_buf_send,  &
       id_recv, counts_buf_recv, displs_buf_recv,  &
       rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ipos = 0
    do, i=0, npets-1
      id_info(i)%id = id_recv(ipos:ipos+counts_buf_recv(i)-1)
      ipos = ipos + counts_buf_recv(i)
    end do

    ! Interchange VMIds

    !  - Send all VMIDs to all PETs

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': AllToAllVing VMIds on PET ' // iTos (mypet))
    allocate (vmid_recv(0:sum (counts_buf_recv+1)-1),  &
              stat=memstat)
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMIdCreate (vmid_recv, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

#if 0
! VMBcastVMId version
call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': AllToAllVVMIds broadcast (using VMBcastVMId)')
    do, send_pet=0, npets-1
      recv_offset = displs_buf_recv(send_pet)
      recv_count  = counts_buf_recv(send_pet)
      if (mypet == send_pet) then
#define ALIAS_VMID
#if defined (ALIAS_VMID)
! use alias to sending vmids.  Not sure this will work for passes
! past PET 0.
        vmid_recv = vmid
#else
! can not use simple assignment here because we need a deep copy
        call ESMF_VMIdCopy (  &  ! deep copy
            dest=vmid_recv(recv_offset:recv_offset+recv_count-1),  &
            source=vmid,  &
            rc=localrc)
	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
#endif
      end if

print *, 'pet', mypet, ': recv_offset:', recv_offset, ', recv_count =', recv_count, ', ubound (vmid_recv) =', ubound (vmid_recv, 1)
      call ESMF_VMBcastVMId (vm,  &
	bcstData=vmid_recv(recv_offset:recv_offset+recv_count-1),  &
	count=recv_count, rootPet=send_pet, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end do
#else
! AllToAllVVMId version
call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': AllToAllVVMIds broadcast (using VMAllToAllVVMId)')
    call ESMF_VMAllToAllVVMId (vm,  &
       vmid     , counts_buf_send, displs_buf_send,  &
       vmid_recv, counts_buf_recv, displs_buf_recv,  &
       rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return
#endif

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': copying VMIds into id_info array')
    ipos = 0
    do, i=0, npets-1
      call ESMF_VMIdCopy (dest=id_info(i)%vmid, &
          source=vmid_recv(ipos:ipos+counts_buf_recv(i)-1),  &
          rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
      ipos = ipos + counts_buf_recv(i)
    end do
      
do, j=0, npets-1
  if (j == myPet) then
    do, i=0, ubound (id_info, 1)
      write (6,*) 'pet', j, ': id_info%id     =', id_info(i)%id
      flush (6)
    end do
  end if
  call ESMF_VMBarrier (vm)
end do

    rc = localrc

  end subroutine ESMF_ReconcileExchangeIDInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileExchangeNeeds"
!BOPI
! !IROUTINE: ESMF_ReconcileExchangeNeeds
!
! !INTERFACE:
  subroutine ESMF_ReconcileExchangeNeeds (vm, id_info, recv_needs, rc)
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
    offsets_recv = offsets_send
    buffer_recv = .false.

    ! AlltoAllV

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

    rc = localrc

  end subroutine ESMF_ReconcileExchangeNeeds

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
    integer,           pointer     :: id(:)	  ! intent(out)
    type(ESMF_VMId),   pointer     :: vmid(:)	  ! intent(out)
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
    integer :: array_sizes
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

    call ESMF_VMIdCreate (vmid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

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
    if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    call ESMF_VMAllGather (vm,  &
        sendData=nitems_local, recvData=nitems_all,  &
        count=1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

write (6,*) ' PET', mypet, ': nitems_all =', nitems_all
flush (6)
call ESMF_VMBarrier (vm)

call ESMF_ReconcileDebugPrint (ESMF_METHOD //  &
    ': complete')

  end subroutine ESMF_ReconcileInitialize
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileSendItems"
!BOPI
! !IROUTINE: ESMF_ReconcileSendItems
!
! !INTERFACE:
  subroutine ESMF_ReconcileSendItems (vm, id_info, recv_items, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)  :: vm
    type(ESMF_ReconcileIDInfo), intent(in)  :: id_info(0:)
    type(ESMF_ItemBuffer),      pointer     :: recv_items(:) ! intent(out)
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
    character, allocatable :: buffer_recv(:),  buffer_send(:)

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
        buffer_send(0:itemcount_global-1),  &
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
print *, ESMF_METHOD, ': PET', mypet, ': serialized buffer sizes',  &
   ': counts_send =', counts_send,  &
   ', counts_recv =', counts_recv

    allocate (  &
        offsets_recv(0:npets-1),  &
        buffer_recv(0:sum (counts_recv)-1),  &
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
        recvData=buffer_recv, recvCounts=counts_recv, recvOffsets=offsets_recv,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    ! Copy recv buffers into recv_items

    allocate (recv_items(0:npets-1), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT,  &
        rcToReturn=rc)) return

    do, i=0, npets-1
      itemcount = counts_recv(i)
      if (itemcount > 0) then
	allocate (  &
            recv_items(i)%item_buffer(0:itemcount-1),  &
            stat=memstat)
	if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
	offset_pos = offsets_recv(i)
	recv_items(i)%item_buffer = buffer_recv(offset_pos:offset_pos+itemcount-1)
      else
        recv_items(i)%item_buffer => null ()
      end if
    end do

    rc = localrc

  end subroutine ESMF_ReconcileSendItems

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileSerialize"
!BOPI
! !IROUTINE: ESMF_ReconcileSerialize
!
! !INTERFACE:
  subroutine ESMF_ReconcileSerialize (state, siwrap,  &
      needs_list, attreconflag,  &
      obj_buffer, rc)
!
! !ARGUMENTS:
    type (ESMF_State),          intent(in)  :: state
    type (ESMF_StateItemWrap),  intent(in)  :: siwrap(:)
    logical,                    intent(in)  :: needs_list(:)
    type(ESMF_AttReconcileFlag),intent(in)  :: attreconflag
    character,                  pointer     :: obj_buffer(:) ! intent(out)
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
!   \item[needs_list]
!     List of State items that need to be sent to other PETs
!   \item[attreconflag]
!     Flag to indicate attribute reconciliation.
!   \item[buffer]
!     Buffer of serialized State objects (intent(out))
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI

    integer :: localrc
    integer :: memstat

    type(ESMF_StateItem), pointer :: stateitem
    type(ESMF_InquireFlag) :: inqflag
    integer :: pass
    type(ESMF_State) :: wrapper

    integer :: buffer_offset
    integer :: needs_count
    integer, allocatable :: offsets(:)
    integer :: lbufsize

    integer :: i

    localrc = ESMF_RC_NOT_IMPL

! Sanity check: siwrap and needs list must be the same size.

    if (ubound (siwrap, 1) /= ubound (needs_list, 1)) then
print *, ESMF_METHOD, ': error - siwrap ubound =', ubound (siwrap, 1),  &
    '/= needs_list =', ubound (needs_list, 1)
      if (ESMF_LogFoundError(ESMF_RC_INTNRL_INCONS, &
          msg="ubound (siwrap) /= ubound (needs_list)", &
          ESMF_CONTEXT,  &
          rcToReturn=rc)) return
    end if

! Make two passes through the State objects.  The first time to calculate
! the size of the buffer, and the second time to perform the actual
! serialization.

    needs_count = count (needs_list)
    if (needs_count == 0) then
      obj_buffer => null ()
      rc = ESMF_SUCCESS
      return
    end if

!    allocate (offsets(0:needs_count), stat=memstat)
!    if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
!	ESMF_CONTEXT,  &
!	rcToReturn=rc)) return
!    offsets = 0
!    offsets(0) = needs_count

  pass_loop:  &
    do, pass = 1, 2
      select case (pass)
      ! Pass 1 finds the required buffer length to serialize each of the
      ! needed items.
      case (1)
        ! Allocate a very small buffer to avoid possible null pointer
        ! references in the serialization routines.
        allocate (obj_buffer(0:ESMF_SIZEOF_DEFINT-1), stat=memstat)
	if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
        ! leave room 
!        buffer_offset = (needs_count+1)*ESMF_SIZEOF_DEFINT
        buffer_offset = ESMF_SIZEOF_DEFINT
        inqflag = ESMF_INQUIREONLY

      ! Pass 2 performs the actual serialization of the items.  It also
      ! prepends them with the # of items and an array of starting offsets
      ! of each item.
      case (2)
        deallocate (obj_buffer, stat=memstat)
	if (ESMF_LogFoundDeallocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
print *, ESMF_METHOD, ': obj_buffer bounds = (0:', buffer_offset, ')'

        allocate (obj_buffer(0:buffer_offset-1), stat=memstat)
	if (ESMF_LogFoundAllocError(memstat, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) return
!        obj_buffer(0:(needs_count+1)*ESMF_SIZEOF_DEFINT-1) =  &
!            transfer (offsets, obj_buffer(0:(needs_count+1)*ESMF_SIZEOF_DEFINT-1))
!        buffer_offset = (needs_count+1)*ESMF_SIZEOF_DEFINT

        obj_buffer(0:ESMF_SIZEOF_DEFINT-1) = transfer (needs_count, obj_buffer(0:ESMF_SIZEOF_DEFINT-1))
        buffer_offset = ESMF_SIZEOF_DEFINT
        inqflag = ESMF_NOINQUIRE
      end select

      ! TODO: Serialize siwrap(0) when attribute reconcile is turned on

      lbufsize = size (obj_buffer)

      do, i=1, size (needs_list)

        if (.not. needs_list(i)) cycle

        stateitem => siwrap(i)%si

        ! Item type
        if (inqflag == ESMF_NOINQUIRE) then
	  obj_buffer(buffer_offset:buffer_offset+ESMF_SIZEOF_DEFINT-1) =  &
	      transfer (stateitem%otype%ot, obj_buffer(1:4))
        end if
        buffer_offset = buffer_offset + ESMF_SIZEOF_DEFINT

        ! Item itself
        select case (stateitem%otype%ot)
          case (ESMF_STATEITEM_FIELDBUNDLE%ot)
            call ESMF_FieldBundleSerialize(stateitem%datap%fbp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
	    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT,  &
        	rcToReturn=rc)) return

print *, 'serialized fieldbundle, pass =', pass

          case (ESMF_STATEITEM_FIELD%ot)
            call ESMF_FieldSerialize(stateitem%datap%fp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
	    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT,  &
        	rcToReturn=rc)) return

print *, 'serialized field, pass =', pass

          case (ESMF_STATEITEM_ARRAY%ot)
            call c_ESMC_ArraySerialize(stateitem%datap%ap,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag, inqflag,  &
                localrc)
	    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT,  &
        	rcToReturn=rc)) return

print *, 'serialized array, pass =', pass

          case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
            call c_ESMC_ArrayBundleSerialize(stateitem%datap%abp,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag, inqflag,  &
                localrc)
	    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT,  &
        	rcToReturn=rc)) return

! TODO: Routehandle...

print *, 'serialized arraybundle, pass =', pass

          case (ESMF_STATEITEM_STATE%ot)
            wrapper%statep => stateitem%datap%spp
            ESMF_INIT_SET_CREATED(wrapper)
            call ESMF_StateSerialize(wrapper,  &
                obj_buffer, lbufsize, buffer_offset,  &
                attreconflag=attreconflag, inquireflag=inqflag,  &
                rc=localrc)
	    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT,  &
        	rcToReturn=rc)) return

print *, 'serialized substate, pass =', pass

          case (ESMF_STATEITEM_UNKNOWN%ot)
            call c_ESMC_StringSerialize(stateitem%namep,  &
                obj_buffer, lbufsize, buffer_offset,  &
                inqflag,  &
                localrc)
	    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        	ESMF_CONTEXT,  &
        	rcToReturn=rc)) return

print *, "serialized unknown type, name=", trim(stateitem%namep)

          case default
            localrc = ESMF_RC_INTNRL_INCONS
print *, "serialization error in default case.  Returning ESMF_RC_INTNRL_INCONS"

          end select

	  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT,  &
              rcToReturn=rc)) return

      end do

    end do pass_loop

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
      integer :: memstat
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

            call ESMF_StateRemove (state, itemName=thisname, rc=localrc)
	    if (ESMF_LogFoundError(localrc, &
	        ESMF_ERR_PASSTHRU, &
	        ESMF_CONTEXT, rcToReturn=rc)) return
	  end if
	end do
	deallocate(itemList, stat=memstat)
	if (ESMF_LogFoundDeallocError(memstat, &
	    ESMF_ERR_PASSTHRU, &
	    ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_ReconcileZapProxies


!------------------------------------------------------------------------------
! Debugging and support procedures
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ReconcileDebugPrint"
  subroutine ESMF_ReconcileDebugPrint (text, multitext, ask, rc)
    use ESMF_IOUtilMod
    character(*), intent(in),  optional :: text
    character(*), intent(in),  optional :: multitext
    logical,      intent(in),  optional :: ask
    integer,      intent(out), optional :: rc

    type(ESMF_VM) :: vm
    integer :: iostat
    integer :: localrc
    integer :: memstat
    integer :: mypet, npets
    character(16) :: answer
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
      flush (ESMF_UtilIOStdout)
      call ESMF_VMBarrier (vm)
      if (mypet == 0) then
	write (ESMF_UtilIOStdout,*) text
	flush (ESMF_UtilIOStderr)
      end if
      call ESMF_VMBarrier (vm)
    end if

    if (present (multitext)) then
      write (ESMF_UtilIOStdout,*) multitext
      flush (ESMF_UtilIOStdout)
      call ESMF_VMBarrier (vm)
    end if

    if (localask) then
      if (mypet == 0) then
	write (ESMF_UtilIOStdout,'(a)', advance='no') 'Proceed?'
	flush (ESMF_UtilIOStdout)
	read (ESMF_UtilIOStdin,'(a)') answer
      end if
      call ESMF_VMBarrier (vm)
    end if

  end subroutine ESMF_ReconcileDebugPrint

  elemental function iTos_len (i)
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
