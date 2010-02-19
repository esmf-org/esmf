! $Id: ESMF_StateReconcile.F90,v 1.70.2.2 2010/02/19 04:44:33 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateReconcile.F90"
!
!     ESMF StateReconcile module
      module ESMF_StateReconcileMod
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
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayBundleMod
      use ESMF_FieldMod
      use ESMF_FieldBundleMod
      use ESMF_StateTypesMod
      use ESMF_StateMod
      use ESMF_InitMacrosMod
      implicit none

#if defined FIXED_BUFFER
      integer,parameter :: BUFSIZE = 102400   ! 100 k element buffer
#endif

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private
!------------------------------------------------------------------------------
!     ! ESMF_StateItemInfo
!
!     ! Simple descriptor block for objects in a State

      type ESMF_StateItemInfo
      sequence
      private
        type(ESMF_StateItemInfo), dimension(:), pointer :: childList
        type(ESMF_StateItemInfo), dimension(:), pointer :: attrList
        ! TODO: these need to be integrated in a better fashion.
        integer(ESMF_KIND_I4) :: mycount, theircount
        integer(ESMF_KIND_I4), pointer, dimension(:) :: idsend, idrecv
        type(ESMF_VMId), pointer, dimension(:) :: vmidsend, vmidrecv
        integer(ESMF_KIND_I4), pointer, dimension(:) :: objsend, objrecv
        character, pointer, dimension(:,:) :: blindsend, blindrecv
        ! TODO: longer term, build a linked list or tree of objects.
        !type(ESMF_StateItemInfo), pointer :: originalObject
        !integer :: blockType   ! new obj, dup, or end marker
        !integer :: objType     ! ESMF object type
        !integer :: objID       ! must be unique! (get from base class)
        !integer :: childCount
      end type

      integer, parameter :: ESMF_BT_NEWOBJ = 1, &
                            ESMF_BT_DUPOBJ = 2, &
                            ESMF_BT_ENDMARKER = 3

      ! obj types from base class, same with IDs

!TODO: This does not work as expected on systems which parse out comments
! before doing substitution.  (it replaces DEBUG with nothing.)
! The alternative is:  #define DEBUG  if (.false.) then
! but i am not sure if that is recognized at compile time and avoided
! or if it introduces a bunch of run-time tests.  (hopefully the former.)
! For now, just comment out the whole mess.
!#if 0
!#define DEBUG  print *, mypet, 
!#else
!#define DEBUG !!
!#define DEBUG if (.false.) then
!#endif



!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateReconcile          ! make consistent for concurrent apps

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateReconcile.F90,v 1.70.2.2 2010/02/19 04:44:33 theurich Exp $'

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
#define ESMF_METHOD "ESMF_StateReconcile"
!BOP
! !IROUTINE: ESMF_StateReconcile -- Reconcile State data across all PETs in a VM
!
! !INTERFACE:
      subroutine ESMF_StateReconcile(state, vm, attreconflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag        
      integer, intent(out), optional :: rc               
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
!     \item[vm]
!       {\tt ESMF\_VM} for this {\tt ESMF\_Component}.
!     \item[{[attreconflag]}]
!       Flag to tell if Attribute reconciliation is to be done as well as data reconciliation
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!     NOTE:  The options for {\tt attreconflag} include:
!            \begin{enumerate}
!              \item ESMF\_ATTRECONCILE\_ON will allow reconciliation of metadata (Attributes)
!              \item ESMF\_ATTRECONCILE\_OFF is the default behavior, this option turns off
!                                            the metadata reconciliation
!            \end{enumerate}
!EOP
    integer :: localrc
    type(ESMF_StateItemInfo), dimension(:), pointer :: stateinfo
    type(ESMF_AttReconcileFlag) :: lattreconflag

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
      
      
    ! First remove all empty nested States from State
    ! Doing this leads to much lower (factor petCount) complexity of the 
    ! current ProxyCreate() code.
    call ESMF_StateZapEmptyNests(state, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return


    ! This turns off the fast option on Regrid; it is working now for
    !  exclusive components, but if there is any reason we should turn
    !  it back off, here is how to do it.
    !!domainOption = 0

    ! Each PET broadcasts the object ID lists and compares them to what
    ! they get back.   Missing objects are sent so they can be recreated
    ! on the PETs without those objects.  Eventually we might want to
    ! hash the ID lists so we can send a single number (or short list of
    ! numbers) instead of having to build and send the list each time.
     
    ! Set the optional ESMF_AttReconcileFlag
    if(present(attreconflag)) then
      lattreconflag = attreconflag
    else
      lattreconflag = ESMF_ATTRECONCILE_OFF
    endif

    ! This recursively descends the state objects and collects information
    ! about each one.
    nullify(stateinfo)
    call ESMF_StateInfoBuild(state, stateinfo, vm, lattreconflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return
    
    ! This one sends missing objects from the PETs which contain them
    ! to the PETs which do not.
    call ESMF_StateProxyCreate(state, stateinfo, vm, lattreconflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return
    
    ! This frees resources which were allocated during the building of
    ! the information blocks during the InfoBuild call.
    call ESMF_StateInfoDrop(stateinfo, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return

    ! Reset the change flags in the Attribute hierarchy
    if (lattreconflag%value == ESMF_ATTRECONCILE_ON%value) then
      call c_ESMC_AttributeUpdateReset(state%statep%base, localrc);
      if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS

 
    end subroutine ESMF_StateReconcile


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateInfoBuild"
!BOPI
! !IROUTINE: ESMF_StateInfoBuild -- Collect information for contained objects
!
! !INTERFACE:
      subroutine ESMF_StateInfoBuild(state, stateInfoList, vm, attreconflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(in) :: state
      type(ESMF_StateItemInfo), dimension(:), pointer :: stateInfoList
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_AttReconcileFlag), intent(in) :: attreconflag        
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to collect information from.
!     \item[stateInfoList]
!       Array of info blocks, one for each object in the {\tt ESMF\_State}.
!       This routine allocates and/or extends the array of blocks, so it
!       can come in as a NULL pointer and will return allocated and filled
!       with data.
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
!
!EOPI
    integer :: i, localrc, attreconstart
    type(ESMF_StateItem), pointer :: stateitem
    type(ESMF_StateItemInfo), pointer :: si
    type(ESMF_State) :: wrapper
    character, pointer, dimension(:) :: bptr
    integer :: offset, mypet
    type(ESMF_VMId) :: VMdummyID

    type(ESMF_InquireFlag) :: inqflag
    integer :: lbufsize, maxbufsize
    integer :: pass

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL


    ! get total num pets.  this is not needed by the code, just the debug
    ! messages below.
    call ESMF_VMGet(vm, localPet=mypet, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              "Accessing the VM", &
                              ESMF_CONTEXT, rc)) return

    ! Get the VM ID of the state to use as a dummy in the code below
    call c_ESMC_GetVMId(state%statep, VMdummyID, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              "Getting VM ID", &
                              ESMF_CONTEXT, rc)) return

    ! make some initial space
    ! TODO: the current code only uses the first entry and hangs everything
    ! onto it.  eventually, have an info block for each distinct object in
    ! the state. 
    allocate(stateInfoList(4), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for ID list", &
                                   ESMF_CONTEXT, rc)) return
    allocate(stateInfoList(1)%childList(4), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for child ID list", &
                                   ESMF_CONTEXT, rc)) return


    ! shortname for use in the code below
    si => stateInfoList(1)
    
    ! (+1) to allow top level State space to reconcile Attributes
    if (attreconflag%value == ESMF_ATTRECONCILE_ON%value) then
      si%mycount = state%statep%datacount + 1
    else
      si%mycount = state%statep%datacount
    endif
    if (si%mycount .gt. 0) then
        allocate(si%idsend(si%mycount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        allocate(si%vmidsend(si%mycount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local VM ID list", &
                                       ESMF_CONTEXT, rc)) return
        allocate(si%objsend(si%mycount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Allocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
    endif

#if defined (FIXED_BUFFER)
    lbufsize = BUFSIZE
    inqflag = ESMF_NOINQUIRE
    maxbufsize = BUFSIZE
    if (si%mycount > 0) then
      allocate(si%blindsend(lbufsize, si%mycount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, &
                           "Allocating buffer for local buf list", &
                               ESMF_CONTEXT, rc)) return
    end if
#else
    do, pass=1,2

      if (pass == 1) then
        ! inquire to find the size of the largest serialization buffer
        ! needed.
        inqflag = ESMF_INQUIREONLY
        maxbufsize = 0
        lbufsize = 1
        allocate (si%blindsend(1,si%mycount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Allocating buffer for local buf inquiry", &
                                     ESMF_CONTEXT, rc)) return
      else
        ! Allocate the buffer, and do the serialization for real
        deallocate (si%blindsend)
        inqflag = ESMF_NOINQUIRE
!DEBUG        print *, 'ESMF_StateInfoBuild: leading buffer dimension =', maxbufsize
        lbufsize = maxbufsize

        if (si%mycount > 0) then
          allocate(si%blindsend(lbufsize, si%mycount), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, &
                               "Allocating buffer for local buf list", &
                                   ESMF_CONTEXT, rc)) return
        end if
      end if
#endif

    ! (+1) to allow top level State space to reconcile Attributes
    if (attreconflag%value == ESMF_ATTRECONCILE_ON%value) then
      si%mycount = state%statep%datacount + 1
      offset = 0
      call c_ESMC_GetID(state%statep%base, si%idsend(1), localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                             "Top level get ID", &
                             ESMF_CONTEXT, rc)) return

      call c_ESMC_GetVMId(state%statep%base, si%vmidsend(1), localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                             "Top level get VM ID", &
                             ESMF_CONTEXT, rc)) return

      si%objsend(1) = ESMF_ID_BASE%objectID
      bptr => si%blindsend(:,1)
      call c_ESMC_BaseSerialize(state%statep%base, bptr(1), lbufsize, offset, &
        attreconflag, inqflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                             "Top level Base serialize", &
                             ESMF_CONTEXT, rc)) return

      maxbufsize = offset
      attreconstart = 2
    else
      si%mycount = state%statep%datacount
      attreconstart = 1
    endif

    !  start from 2, top level State is number 1
    do i=attreconstart, si%mycount
        ! i-1 because we are starting from 2, all else should be i
        if (attreconflag%value == ESMF_ATTRECONCILE_ON%value) then
          stateitem => state%statep%datalist(i-1)
        else
          stateitem => state%statep%datalist(i)
        endif
        offset = 0
        select case (stateitem%otype%ot)
           case (ESMF_STATEITEM_FIELDBUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%fbp%btypep, si%idsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Fieldbundle get ID", &
                             ESMF_CONTEXT, rc)) return

             call c_ESMC_GetVMId(stateitem%datap%fbp%btypep, si%vmidsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested fieldbundle get VM ID", &
                             ESMF_CONTEXT, rc)) return

             si%objsend(i) = ESMF_ID_FIELDBUNDLE%objectID
             bptr => si%blindsend(:,i)
             call ESMF_FieldBundleSerialize(stateitem%datap%fbp, bptr, lbufsize, &
                                       offset, attreconflag=attreconflag, &
                                       inquireflag=inqflag, rc=localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested fieldbundle serialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "serialized bundle, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_FIELD%ot)
             call c_ESMC_GetID(stateitem%datap%fp%ftypep, si%idsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Field get ID", &
                             ESMF_CONTEXT, rc)) return

             call c_ESMC_GetVMId(stateitem%datap%fp%ftypep, si%vmidsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Field get VM ID", &
                             ESMF_CONTEXT, rc)) return

             si%objsend(i) = ESMF_ID_FIELD%objectID
             bptr => si%blindsend(:,i)
             call ESMF_FieldSerialize(stateitem%datap%fp, bptr, lbufsize, &
                                       offset, attreconflag=attreconflag, &
                                       inquireflag=inqflag, rc=localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Field serialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "serialized field, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_ARRAY%ot)
             call c_ESMC_GetID(stateitem%datap%ap, si%idsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Array get ID", &
                             ESMF_CONTEXT, rc)) return

             call c_ESMC_GetVMId(stateitem%datap%ap, si%vmidsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Array get VM ID", &
                             ESMF_CONTEXT, rc)) return

             si%objsend(i) = ESMF_ID_ARRAY%objectID
             bptr => si%blindsend(:,i)
             call c_ESMC_ArraySerialize(stateitem%datap%ap, bptr(1), &
                                       lbufsize, offset, attreconflag, &
                                       inqflag, localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Array serialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "serialized array, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
             call c_ESMC_GetID(stateitem%datap%abp, si%idsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Arraybundle get ID", &
                             ESMF_CONTEXT, rc)) return

             call c_ESMC_GetVMId(stateitem%datap%abp, si%vmidsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Arraybundle get VM ID", &
                             ESMF_CONTEXT, rc)) return

             si%objsend(i) = ESMF_ID_ARRAYBUNDLE%objectID
             bptr => si%blindsend(:,i)
             call c_ESMC_ArrayBundleSerialize(stateitem%datap%abp, bptr(1), &
                                       lbufsize, offset, attreconflag, &
                                       inqflag, localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested Arraybundle serialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "serialized arraybundle, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_STATE%ot)
             call c_ESMC_GetID(stateitem%datap%spp, si%idsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested State get ID", &
                             ESMF_CONTEXT, rc)) return

             call c_ESMC_GetVMId(stateitem%datap%spp, si%vmidsend(i), localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested State get VM ID", &
                             ESMF_CONTEXT, rc)) return

             si%objsend(i) = ESMF_ID_STATE%objectID
             bptr => si%blindsend(:,i)
             wrapper%statep => stateitem%datap%spp
             ESMF_INIT_SET_CREATED(wrapper)
             call ESMF_StateSerialize(wrapper, bptr, lbufsize, offset, &
              attreconflag=attreconflag, inquireflag=inqflag, rc=localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested State serialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "serialized substate, obj=", si%objsend(i), " id=", si%idsend(i)

           case (ESMF_STATEITEM_NAME%ot)
             si%idsend(i) = -1
             ! TODO: decide what this should be.
             si%vmidsend(i) = VMdummyID
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), lbufsize, offset, &
               inqflag, localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested string serialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "serialized placeholder, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS

           case (ESMF_STATEITEM_INDIRECT%ot)
             si%idsend(i) = -2
             si%vmidsend(i) = VMdummyID
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), lbufsize, offset, &
               inqflag, localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested string serialize (indirect)", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "serialized field-in-bundle, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS

           case (ESMF_STATEITEM_UNKNOWN%ot)
             si%idsend(i) = -3
             si%vmidsend(i) = VMdummyID
             si%objsend(i) = ESMF_STATEITEM_NAME%ot
             bptr => si%blindsend(:,i)
             call c_ESMC_StringSerialize(stateitem%namep, bptr(1), lbufsize, offset, &
               inqflag, localrc)
             if (ESMF_LogMsgFoundError(localrc, &
                             "nested unknown type serialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "serialized unknown type, name=", trim(stateitem%namep)
             localrc = ESMF_SUCCESS
        end select

        if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) then
        
!!DEBUG "error -- i, offset, lbufsize = ", i, offset, lbufsize

           ! TODO: this is a bit too late; if offset has moved past the end
           ! of the buffer, we've already written over memory that is not ours.
           ! but better late than never??
           if (offset > lbufsize) then
               call ESMF_LogMsgSetError(ESMF_RC_INTNRL_INCONS, &
                         "Too many objects in State for Reconcile to handle", &
                                        ESMF_CONTEXT, rc)
           endif
        ! either way, return here.
        return
        endif

!!DEBUG        print *, "ESMF_StateInfoBuild: i, offset, lbufsize = ", i, offset, lbufsize
        maxbufsize = max (maxbufsize, offset)
      end do
#if !defined (FIXED_BUFFER)
    end do ! pass
#endif
 
    if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_StateInfoBuild

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateInfoDrop"
!BOPI
! !IROUTINE: ESMF_StateInfoDrop -- Discard information for contained objects
!
! !INTERFACE:
      subroutine ESMF_StateInfoDrop(stateInfoList, rc)
!
! !ARGUMENTS:
      type(ESMF_StateItemInfo), dimension(:), pointer :: stateInfoList
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!
!     Called after the communication is complete, this routine frees
!     any space allocated during the traversal of the state list and
!     communication of the object information.
!
!     The arguments are:
!     \begin{description}
!     \item[stateInfoList]
!       Array of info blocks, one for each object in the {\tt ESMF\_State}.
!       Allocated by previous code, this routine traverses the blocks
!       and frees all allocated space.  If no errors, this pointer 
!       returns nullified.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc
    type(ESMF_StateItemInfo), pointer :: si

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! shortname for use in the code below
    si => stateInfoList(1)

    ! The receive buffers are allocated and freed during the sending of
    ! objects.  What remains allocated are the send buffers which still
    ! need to be freed.  TODO: this needs to be better integrated with the
    ! object tree code below.
    if (si%mycount .gt. 0) then
        deallocate(si%idsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local ID list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%vmidsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local VM ID list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%objsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local obj list", &
                                       ESMF_CONTEXT, rc)) return
        deallocate(si%blindsend, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                 "Deallocating buffer for local buf list", &
                                       ESMF_CONTEXT, rc)) return
    endif

    ! The tree of objects - currently we are only using the first entry
    ! and creating blocks all attached to it.
    deallocate(stateInfoList(1)%childList, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                   "Deallocating buffer for child ID list", &
                                   ESMF_CONTEXT, rc)) return
    deallocate(stateInfoList, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                  "DeaAllocating buffer for state ID list", &
                                   ESMF_CONTEXT, rc)) return
    nullify(stateInfoList)

    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_StateInfoDrop

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateProxyCreate"
!BOPI
! !IROUTINE: ESMF_StateProxyCreate -- Create missing objects
!
! !INTERFACE:
      subroutine ESMF_StateProxyCreate(state, stateInfoList, vm, attreconflag, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      type(ESMF_StateItemInfo), dimension(:), pointer :: stateInfoList
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_AttReconcileFlag), intent(in) :: attreconflag        
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!
!     Traverse the state object and build blocks for all subobjects.
!     Communicate with all other PETs in this {\tt ESMF\_VM} and determine
!     if any objects are unknown to other PETs.  If so, communicate enough
!     information to build proxy objects, which contain no local data, but
!     contain information about the size and distribution of the data on
!     other PETs.  Also communicate attribute information so the remote
!     object can be queried on any PET and return consistent informaion.
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       {\tt ESMF\_State} to add info into.
!     \item[stateInfoList]
!       Array of info blocks, one for each object in the {\tt ESMF\_State}.
!       This was built during the call to {\tt ESMF\_StateInfoBuild()}, and
!       is released by a call to {\tt ESMF\_StateInfoDrop()}.
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
!
!EOPI
    integer :: pets, mypet, i, j, k, l, m, localrc, attreconstart
    integer(ESMF_KIND_I4) :: objcount, bsbufsize
    integer(ESMF_KIND_I4) :: comm_ints(2)
    type(ESMF_State) :: substate
    type(ESMF_Base) :: base
    type(ESMF_FieldBundle) :: bundle
    type(ESMF_Field) :: field
    type(ESMF_Array) :: array
    type(ESMF_ArrayBundle) :: arraybundle
    character(len=ESMF_MAXSTR) :: thisname
    character, pointer, dimension(:) :: bptr
    logical :: ihave
    type(ESMF_VMId) :: temp_vmid
    type(ESMF_StateItemInfo), pointer :: si
    integer :: offset, myOrigCount
    logical :: i_send, i_recv, llow

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! shorthand for the code below
    si => stateInfoList(1)

    ! get total num pets.
    call ESMF_VMGet(vm, localPet=mypet, petCount=pets, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                   ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rc)) return

    myOrigCount = si%mycount  ! my original count on entering this routine

    do, j = 0, pets-1
!!DEBUG "Outer loop, j = ", j
       ! each PET takes turns broadcasting to all

       i_send = mypet == j
       i_recv = mypet /= j

       ! First, broadcast object count and blindsend buffer size.
       comm_ints(1) = myOrigCount
       comm_ints(2) = size (si%blindsend, dim=1)
       call ESMF_VMBroadcast (vm, comm_ints, count=size (comm_ints), &
                      root=j, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rc)) return

       objcount  = comm_ints(1)
       if (i_recv)  &
           si%theircount = objcount
       bsbufsize = comm_ints(2)
       
       if (objcount > 0) then

           ! Broadcast the local object IDs
           if (i_send) then
               call ESMF_VMBroadcast (vm, si%idsend, count=myOrigCount, root=j, rc=localrc)
           else
               allocate(si%idrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Allocating buffer for local ID list", &
                              ESMF_CONTEXT, rc)) return
               call ESMF_VMBroadcast (vm, si%idrecv, count=si%theircount, root=j, rc=localrc)
           end if
           if (ESMF_LogMsgFoundError(localrc, &
                          ESMF_ERR_PASSTHRU, &
                          ESMF_CONTEXT, rc)) return
!!DEBUG "completed broadcast of id list"

           ! Broadcast the object types
           if (i_send) then
               call ESMF_VMBroadcast (vm, si%objsend, count=myOrigCount, root=j, rc=localrc)
           else
               allocate(si%objrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Allocating buffer for local obj list", &
                              ESMF_CONTEXT, rc)) return
               call ESMF_VMBroadcast (vm, si%objrecv, count=si%theircount, root=j, rc=localrc)
           end if
           if (ESMF_LogMsgFoundError(localrc, &
                          ESMF_ERR_PASSTHRU, &
                          ESMF_CONTEXT, rc)) return
!!DEBUG "completed broadcast of object type list"

           ! Broadcast VMIds
#define NEWVMID
#ifdef NEWVMID
           if (i_send) then
               call ESMF_VMBcastVMId(vm, si%vmidsend, count=myOrigCount, &
                   root=j, rc=localrc)
           else
	       allocate(si%vmidrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Allocating buffer for local VM ID list", &
                              ESMF_CONTEXT, rc)) return
               do, k=1, si%theircount
                   call ESMF_VMIdCreate (si%vmidrecv(k))
               enddo                              
               call ESMF_VMBcastVMId(vm, si%vmidrecv, count=size (si%vmidrecv), &
                   root=j, rc=localrc)
           end if
           if (ESMF_LogMsgFoundError(localrc, &
                          ESMF_ERR_PASSTHRU, &
                          ESMF_CONTEXT, rc)) return
#else
           if (i_send) then
               do, i=0, pets-1
                   if (j == i) cycle
                   do, k=1, myOrigCount
                       call ESMF_VMSendVMId (vm, si%vmidsend(k), i, rc=localrc)
                       if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
                   end do
               end do
           else
               allocate(si%vmidrecv(si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Allocating buffer for local VM ID list", &
                              ESMF_CONTEXT, rc)) return
               do, k=1, si%theircount
                   call ESMF_VMIdCreate (si%vmidrecv(k))
                   call ESMF_VMRecvVMId (vm, si%vmidrecv(k), j, rc=localrc)
                   if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
               end do
           end if
#endif

           ! Broadcast serialized object buffers
           if (i_send) then
               call ESMF_VMBroadcast (vm, si%blindsend, bsbufsize*myOrigCount, root=j, rc=localrc)
	   else
               allocate(si%blindrecv(bsbufsize, si%theircount), stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Allocating buffer for local buf list", &
                              ESMF_CONTEXT, rc)) return
               call ESMF_VMBroadcast (vm, si%blindrecv, size (si%blindrecv), root=j, rc=localrc)
           end if
           if (ESMF_LogMsgFoundError(localrc, &
                          ESMF_ERR_PASSTHRU, &
                          ESMF_CONTEXT, rc)) return

       end if ! objcount > 0

       if (i_recv) then

           ! at this point we have all their objects here in our address
           ! space.  we just need to sort thru the lists and figure out
           ! if there are any which are missing from our list.

#if defined (DEBUG)
           do k=1, si%mycount
!!DEBUG  " num, send id and obj id", k, si%idsend(k), si%objsend(k)
           enddo
           do k=1, si%theircount
!!DEBUG  " num, recv id and obj id", k, si%idrecv(k), si%objrecv(k)
           enddo
#endif

        ! unpack the top level State Attributes first
        if (attreconflag%value == ESMF_ATTRECONCILE_ON%value) then
          if (si%objrecv(1) == ESMF_ID_BASE%objectID) then
            bptr => si%blindrecv(:,1)
            offset = 0
            call c_ESMC_BaseDeserialize(base, bptr, offset, &
              attreconflag, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rc)) return

            call ESMF_BaseSetInitCreated(base, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rc)) return

            call c_ESMC_AttributeCopy(base, state%statep%base, &
              ESMF_ATTCOPY_VALUE, ESMF_ATTTREE_OFF, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rc)) return

          endif
          attreconstart = 2
        else
          attreconstart = 1
        endif

           !!! TODO: 
           !!!   make a combined object id list here, so only one copy of
           !!!   the missing object is sent.

           ! k from 2 because the top level State was number 1
           do k=attreconstart, si%theircount
!!DEBUG " checking remote id for object ", k, "value is ", si%idrecv(k)
             ihave = .false.
             
#define IHAVEOLD
#ifdef IHAVEOLD
             do l=1, si%mycount
!!DEBUG " checking local id for object ", l, "value is ", si%idsend(l)
                ! cannot just print a vmid, have to call real print routine
                !print *, "vm compare says ", ESMF_VMIdCompare(si%vmidrecv(k), si%vmidsend(l)) 
                if ((si%idrecv(k) .eq. si%idsend(l)) &
                .and. & 
     (ESMF_VMIdCompare(si%vmidrecv(k), si%vmidsend(l)) .eq. ESMF_TRUE) ) then
                          ihave = .true.
!!DEBUG "  objects match, no need to create proxy"
                     exit
                 endif
             enddo
#else
             ! Only search back a fixed number of times (20), otherwise simply
             ! add object. This prevents the following loop from becoming more
             ! and more expensive.
             
             !gjt -> turning this fixed size search on does not really help
             ! for the test code I was using. I believe that the complexity of
             ! the StateAdd() calls is also O(si%mycount)! Consequently this
             ! search here doesn't really matter much (from a complexity/
             ! scaling standpoint). It just comes in as an extra bit of factor!
             
             llow = max(1, si%mycount - 20)
             do l=si%mycount, llow, -1
               if ((si%idrecv(k) .eq. si%idsend(l)) .and. & 
     (ESMF_VMIdCompare(si%vmidrecv(k), si%vmidsend(l)) .eq. ESMF_TRUE) ) then
                 ihave = .true.
                 exit
               endif
             enddo
#endif
             
!!DEBUG "  end of match loop for remote object ", k, "ihave flag is ", ihave
             if (.not. ihave) then
             
!!DEBUG " need to create local proxy object"
                offset = 0  
                select case (si%objrecv(k))
                   case (ESMF_ID_FIELDBUNDLE%objectID)
!!DEBUG "need to create proxy bundle, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    bundle = ESMF_FieldBundleDeserialize(bptr, offset, &
                      attreconflag=attreconflag, rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Fieldbundle deserialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "created Fieldbundle, ready to set id and add to local state"
                    call c_ESMC_SetVMId(bundle%btypep, si%vmidrecv(k), localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Fieldbundle SetVMId call", &
                             ESMF_CONTEXT, rc)) return

                    call ESMF_StateAdd(state, bundle, proxyflag=.true., &
                      rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Fieldbundle add to local state", &
                             ESMF_CONTEXT, rc)) return
!!DEBUG "Fieldbundle added to state"

                   case (ESMF_ID_FIELD%objectID)
!!DEBUG "need to create proxy field, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    field = ESMF_FieldDeserialize(bptr, offset, &
                      attreconflag=attreconflag, rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Field deserialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "created field, ready to set id and add to local state"
                    call c_ESMC_SetVMId(field%ftypep, si%vmidrecv(k), localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Field SetVMId call", &
                             ESMF_CONTEXT, rc)) return

                    call ESMF_StateAdd(state, field, proxyflag=.true., &
                      rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Field add to local state", &
                             ESMF_CONTEXT, rc)) return
!!DEBUG "field added to state"

                   case (ESMF_ID_ARRAY%objectID)
!!DEBUG "need to create proxy array, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    call c_ESMC_ArrayDeserialize(array, bptr, offset, &
                      attreconflag, localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Array deserialize", &
                             ESMF_CONTEXT, rc)) return

                    ! Set init code
                    call ESMF_ArraySetInitCreated(array, rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "Array SetInit call", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "created array, ready to set id and add to local state"
                    call c_ESMC_SetVMId(array, si%vmidrecv(k), localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Array SetVMId call", &
                             ESMF_CONTEXT, rc)) return

                    call ESMF_StateAdd(state, array, proxyflag=.true., &
                      rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Array add to local state", &
                             ESMF_CONTEXT, rc)) return
!!DEBUG "array added to state"

                   case (ESMF_ID_ARRAYBUNDLE%objectID)
!!DEBUG "need to create proxy arraybundle, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    call c_ESMC_ArrayBundleDeserialize(arraybundle, bptr, &
                      offset, attreconflag, localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Arraybundle deserialize", &
                             ESMF_CONTEXT, rc)) return

                    ! Set init code
                    call ESMF_ArrayBundleSetInitCreated(arraybundle, rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "Arraybundle SetInit call", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "created arraybundle, ready to set id and add to local state"
                    call c_ESMC_SetVMId(arraybundle, si%vmidrecv(k), localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Arraybundle SetVMId call", &
                             ESMF_CONTEXT, rc)) return

                    call ESMF_StateAdd(state, arraybundle, &
                      proxyflag=.true., rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Arraybundle add to local state", &
                             ESMF_CONTEXT, rc)) return
!!DEBUG "arraybundle added to state"

                   case (ESMF_ID_STATE%objectID)
!!DEBUG "need to create proxy substate, remote id=", si%idrecv(k)
                    bptr => si%blindrecv(:,k)
                    substate = ESMF_StateDeserialize(vm, bptr, offset, &
                      attreconflag=attreconflag, rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Substate deserialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "created substate, ready to set id and add to local state"
                    call c_ESMC_SetVMId(substate%statep, si%vmidrecv(k), localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Substate SetVMId call", &
                             ESMF_CONTEXT, rc)) return

                    call ESMF_StateAdd(state, substate, proxyflag=.true., &
                      rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Substate add to local state", &
                             ESMF_CONTEXT, rc)) return
!!DEBUG "substate added to state"

                   case (ESMF_STATEITEM_NAME%ot)
!!DEBUG "need to create proxy placeholder name, remote id=", si%idrecv(k)
                     call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Stateitem deserialize", &
                             ESMF_CONTEXT, rc)) return

!!DEBUG "created string, ready to add to local state"
                    call ESMF_StateAdd(state, thisname, rc=localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested Stateitem add to local state", &
                             ESMF_CONTEXT, rc)) return
!!DEBUG "placeholder added to state"
         
                  case (ESMF_STATEITEM_INDIRECT%ot)
                     !print *, "field inside a bundle"
                    call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested String deserialize", &
                             ESMF_CONTEXT, rc)) return

                     ! do nothing here
            
                  case (ESMF_STATEITEM_UNKNOWN%ot)
                    print *, "WARNING: unknown type"
                    call c_ESMC_StringDeserialize(thisname, &
                                                   bptr(1), offset, localrc)
                    if (ESMF_LogMsgFoundError(localrc, &
                             "nested 'unknown' deserialize", &
                             ESMF_CONTEXT, rc)) return

                     ! do nothing here

                   case default
                    print *, "WARNING: unexpected type"
                end select
             endif
           enddo

!!DEBUG "end of proxy create section"

           if (si%theircount .gt. 0) then
!!DEBUG "the remote pet had sent us objects; remove temp space now"
               deallocate(si%idrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local ID list", &
                               ESMF_CONTEXT, rc)) return
               do k = 1, si%theircount
                   call ESMF_VMIdDestroy(si%vmidrecv(k))
               enddo
               deallocate(si%vmidrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local VM ID list", &
                               ESMF_CONTEXT, rc)) return
               deallocate(si%objrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local obj list", &
                               ESMF_CONTEXT, rc)) return
               deallocate(si%blindrecv, stat=localrc)
               if (ESMF_LogMsgFoundAllocError(localrc, &
                              "Deallocating buffer for local buf list", &
                               ESMF_CONTEXT, rc)) return
           endif
!!DEBUG "done deleting local space"
           
           ! TODO:
           ! and now, i have a different local object list.   brute force
           ! rebuild my send list.   Once this code is stable, revisit this
           ! and make a way to add the new object into my "known object"
           ! list without reserializing all objects.

!!DEBUG "drop old list and rebuild new one for our local objects"
           call ESMF_StateInfoDrop(stateInfoList, rc=localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

           call ESMF_StateInfoBuild(state, stateInfoList, vm, attreconflag, localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

!!DEBUG "reset pointer to state list"
           si => stateInfoList(1)

       endif   ! receiver
!!DEBUG "bottom of loop"
    enddo ! source PET number

!!DEBUG "end of state proxy create"
    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_StateProxyCreate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateZapEmptyNests"
!BOPI
! !IROUTINE: ESMF_StateZapEmptyNests -- Zap empty nested States from State
!
! !INTERFACE:
    subroutine ESMF_StateZapEmptyNests(state, rc)
!
! !ARGUMENTS:
      type(ESMF_State), intent(inout) :: state
      integer, intent(out), optional :: rc               

      integer :: localrc, i, iwrt, oldcount
      type (ESMF_StateClass), pointer :: stypep
      logical :: emptyNest

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      stypep => state%statep
      
      iwrt = 1 ! initialize
      do i=1, stypep%datacount
        emptyNest = .false. ! reset
        if (stypep%datalist(i)%otype%ot == ESMF_STATEITEM_STATE%ot) then
          ! found nested State
          if (stypep%datalist(i)%datap%spp%datacount == 0) then
            ! nested State is empty
            emptyNest = .true.
            call c_ESMC_AttributeLinkRemove(stypep%base, stypep%datalist(i)%datap%spp%base, &
              localrc)
           if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
          endif
!print *, "gjt: zap empty nest in State"
        endif
        if (.not. emptyNest) then
          if (iwrt < i) then
            stypep%datalist(iwrt) = stypep%datalist(i)
          endif
          iwrt = iwrt + 1
        endif
      enddo
!oldcount = stypep%datacount
      stypep%datacount = iwrt-1
!print *, "gjt: reduced objects in State from", oldcount, " to ", stypep%datacount
      
      if (present(rc)) rc = ESMF_SUCCESS
    end subroutine ESMF_StateZapEmptyNests

end module ESMF_StateReconcileMod


