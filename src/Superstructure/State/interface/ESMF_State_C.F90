! $Id: ESMF_State_C.F90,v 1.24.2.1 2010/02/05 20:04:52 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_State_C.F90"
!==============================================================================
!
! F77 interfaces for C++ layer calling into F90 implementation layer.
!
!==============================================================================
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_State_C.F90,v 1.24.2.1 2010/02/05 20:04:52 svasquez Exp $'
!==============================================================================

!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
! 
! The code in this file implements the interface code between C++ and F90
!  for the {\tt State} entry points.  When the user calls an
!  {\tt ESMC_State}XXX method, that code calls these functions, which
!  in turn call the F90 module code.  C++ cannot call directly into an
!  F90 module because the module routine names are altered in a similar
!  fashion as C++ name mangling.
! 
!EOP
!------------------------------------------------------------------------------
   subroutine f_esmf_statecreate(state, stateName, rc)

       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       implicit none

       type(ESMF_State) :: state
      character(len=*), intent(in) :: stateName
      integer, intent(out) :: rc


       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       state = ESMF_StateCreate(statename=stateName, rc=rc)
               
    
   end subroutine f_esmf_statecreate

!------------------------------------------------------------------------------
   subroutine f_esmf_stateaddarray(state, array, rc)

       use ESMF_UtilTypesMod
      !use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_ArrayCreateMod
       implicit none

       type(ESMF_State) :: state        !inout
       type(ESMF_Array) :: array        !in
      integer, intent(out) :: rc        !out

       ! local variable
       type(ESMF_Array) :: farray

       ! Must first create a proper ESMF_Array that contains the 
       ! required "isInit" class member.
       
       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       ! Copy the this pointer a new ESMF_Array object
       call ESMF_ArrayCopyThis(array, farray, rc)

       !  set the valid init code of the new object
       call ESMF_ArraySetInitCreated(farray, rc)

       call ESMF_StateAdd(state=state, array=farray, rc=rc)


   end subroutine f_esmf_stateaddarray

!------------------------------------------------------------------------------
   subroutine f_esmf_stateaddfield(state, field, rc)

       use ESMF_UtilTypesMod
      !use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_FieldMod
       implicit none

       type(ESMF_State) :: state        !inout
       type(ESMF_Field) :: field        !in
      integer, intent(out) :: rc        !out

       ! field is directly usable - it is a deep class implemented in Fortran
       call ESMF_StateAdd(state=state, field=field, rc=rc)

   end subroutine f_esmf_stateaddfield

!------------------------------------------------------------------------------
   subroutine f_esmf_stategetarray(state, arrayName, array, rc)

       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_ArrayCreateMod
       implicit none

       type(ESMF_State) :: state        !in
       character(len=*) :: arrayName    !in
       type(ESMF_Array) :: array        !out
      integer, intent(out) :: rc        !out

       ! local variable
       type(ESMF_Array) :: farray

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       call ESMF_StateGet(state=state, itemName=arrayName, &
                               array=farray, rc=rc)

       ! the array object returned to the C interface must consist only of the
       ! this pointer. It must not contain the isInit member.
       call ESMF_ArrayCopyThis(farray, array, rc)


   end subroutine f_esmf_stategetarray

!------------------------------------------------------------------------------

   subroutine f_esmf_stategetfield(state, fieldName, field, rc)

       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_FieldMod
       implicit none

       type(ESMF_State) :: state        !in
       character(len=*) :: fieldName    !in
       type(ESMF_Field) :: field        !out
      integer, intent(out) :: rc        !out

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       call ESMF_StateGet(state=state, itemName=fieldName, &
                               field=field, rc=rc)

   end subroutine f_esmf_stategetfield

!------------------------------------------------------------------------------


   subroutine f_esmf_stateprint(state, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       implicit none

       type(ESMF_State) :: state
       integer :: rc

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       call ESMF_StatePrint(state,rc=rc)

   end subroutine f_esmf_stateprint

!------------------------------------------------------------------------------

   subroutine f_esmf_statedestroy(state, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       implicit none

       type(ESMF_State) :: state
       integer :: rc              

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       call ESMF_StateDestroy(state, rc)
    
   end subroutine f_esmf_statedestroy

!------------------------------------------------------------------------------

 !  type(ESMF_Array) function arrayCtoF90(carray)
 !  subroutine arrayCtoF90(carray, farray, rc)
 !     use ESMF_UtilTypesMod
 !     use ESMF_BaseMod    ! ESMF base class
 !     use ESMF_ArrayCreateMod
 !     implicit none

  ! Very important: the pointers passed from C and used as references for
  ! arrayInArg and arrayOutArg are simple pointers to pointers from the C side.
  ! This means that there is no memory for what the F90 INITMACROS are using
  ! at that location! In order to deal with this C<->F90 difference local
  ! F90 variables are necessary to work on the F90 side and this glue code will
  ! copy the "this" member in the derived type which is the part that actually
  ! needs to be passed between C and F90.

 !     type(ESMF_Array) :: carray 
 !     type(ESMF_Array) :: farray 
      
       !  local variable
       !type(ESMF_Array) :: farray

 !     farray%this = carray%this

       !  set the valid init code
 !     ESMF_INIT_SET_CREATED(farray)


 !     rc = ESMF_SUCCESS

 !  end subroutine arrayCtoF90

!------------------------------------------------------------------------------
   subroutine f_esmf_stateadddata(statep, name, func, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       implicit none

       type(ESMF_State), pointer :: statep
       character(*)              :: name
       integer                   :: func
       integer, intent(out)      :: rc              

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       !call ESMF_StateAddData(statep, rc)
    
   end subroutine f_esmf_stateadddata

   subroutine f_esmf_stategetdata(statep, name, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       implicit none

       type(ESMF_State), pointer :: statep      
       character(*)              :: name
       integer, intent(out)      :: rc     

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       !call ESMF_StateGetData(statep, rc)

   end subroutine f_esmf_stategetdata

   subroutine f_esmf_stateget(statep, name, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       implicit none

       type(ESMF_State), pointer :: statep      
       character(*)              :: name
       integer, intent(out)      :: rc     

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       !call ESMF_StateGet(statep, rc)

   end subroutine f_esmf_stateget


   ! TODO: add rest of state entry points

!------------------------------------------------------------------------------

   subroutine f_esmf_stategetnumitems(state, itemCount, rc)

       use ESMF_UtilTypesMod
       use ESMF_StateTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_ArrayCreateMod
       implicit none

       type(ESMF_State), intent(in) :: state        !in
       integer, intent(out)         :: itemCount    !out
       integer, intent(out)         :: rc           !out

       ! local variable

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

!       call ESMF_StateGetInfo(state=state, itemCount=itemCount, rc=rc)
       call ESMF_StateGet(state=state, itemCount=itemCount, rc=rc)

   end subroutine f_esmf_stategetnumitems


!------------------------------------------------------------------------------

   subroutine f_esmf_stategetitemnames(state, numItems, itemNameList, &
                                       itemTypeList, rc)

       use ESMF_UtilTypesMod
       use ESMF_StateTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_ArrayCreateMod
       implicit none

       type(ESMF_State), intent(in)              :: state                  !in
       integer, intent(in)                       :: numItems               !in
       character(len=*), intent(inout)           :: itemNameList(numItems) !out
       type(ESMF_StateItemType), intent(inout)   :: itemTypeList(numItems) !out
       integer, intent(out)                      :: rc                     !out

       ! local variable
       integer                    :: itemCount
       character(len=ESMF_MAXSTR) :: localNameList(numItems)
       type(ESMF_StateItemType)   :: localTypeList(numItems)

       integer                    :: i

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

!       call ESMF_StateGetInfo(state=state, itemCount=itemCount, &
!                              itemNameList=localNameList, &
!                              stateitemtypeList=localTypeList, rc=rc)
       call ESMF_StateGet(state=state, itemCount=itemCount, &
                          itemNameList=localNameList, &
                          stateitemtypeList=localTypeList, rc=rc)

       do i = 1, itemCount
          itemTypeList(i) = localTypeList(i)
          itemNameList(i) = localNameList(i)
       enddo

   end subroutine f_esmf_stategetitemnames

!------------------------------------------------------------------------------

  subroutine f_esmf_statecollectgarbage(state, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_statecollectgarbage"
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_StateTypesMod
    use ESMF_StateMod
    
    type(ESMF_State)     :: state
    integer, intent(out) :: rc
  
    integer :: localrc
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
  
    !print *, "collecting State garbage"

    ! destruct internal data allocations
    call ESMF_StateDestruct(state%statep, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rc)) return

    ! deallocate actual StateClass allocation      
    if (associated(state%statep)) then
      deallocate(state%statep, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Deallocating State", &
        ESMF_CONTEXT, &
        rc)) return
    endif
    nullify(state%statep)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_statecollectgarbage
