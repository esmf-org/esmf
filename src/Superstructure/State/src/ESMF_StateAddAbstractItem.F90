! This file is technically not valid Fortran and should be rewritten
! as C.  It will still be used to "cheat" the enforcement of
! interfaces, but as C it will at least be portable.  Intel Fortran
! just happens to accept this interface despite most arguments not
! being C interoperable.
#define ESMF_METHOD "ESMF_StateAddAbstractItem"
subroutine ESMF_StateAddAbstractItem(state, itemname, item, &
     relaxedflag, rc) bind(c, name="ESMF_StateAddAbstractItem")
   use ESMF_UtilTypesMod
   use ESMF_StateTypesMod
   use ESMF_AbstractStateItemMod
   use ESMF_StateItemMod
   use ESMF_StateInternalsMod
   use ESMF_InitMacrosMod
   use ESMF_StateVaMod
   use ESMF_LogErrMod
   implicit none
   type(ESMF_State),  intent(inout) :: state
   character(*), intent(in) :: itemname
   type(ESMF_AbstractItemWrapper), target, intent(in) :: item
   logical, optional, intent(in) :: relaxedflag
   type(integer), intent(out), optional :: rc

#  include "ESMF.h"



      type(ESMF_AbstractStateItem) :: temp_list(1)
      character(ESMF_MAXSTR) :: lvalue1, lvalue2
      character(ESMF_MAXSTR) :: lobject, lname

      integer :: localrc

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
!      ESMF_INIT_CHECK_DEEP(ESMF_AbstractStateItemGetInit,item,rc)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_StateValidate(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      temp_list(1)%name = itemname
      temp_list(1)%wrapper => item
      call ESMF_StateClsAdd (state%statep, temp_list, &
        addflag=.true., relaxedflag=relaxedflag, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      if (present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_StateAddAbstractItem
#undef ESMF_METHOD

#define ESMF_METHOD "ESMF_StateGetAbstractItem"
subroutine ESMF_StateGetAbstractItem(state, itemname, item, rc) bind(c, name="ESMF_StateGetAbstractItem")
   use ESMF_UtilTypesMod
   use ESMF_StateTypesMod
   use ESMF_AbstractStateItemMod
   use ESMF_StateItemMod
   use ESMF_StateInternalsMod
   use ESMF_InitMacrosMod
   use ESMF_StateVaMod
   use ESMF_LogErrMod
   implicit none
   type(ESMF_State),  intent(inout) :: state
   character(len=*), intent(in) :: itemname
   type(ESMF_AbstractItemWrapper), pointer, intent(out) :: item
   type(integer), intent(out), optional :: rc

   type(ESMF_StateItem), pointer :: dataitem
   logical :: exists
   integer :: localrc
   character(len=len(itemName)+ESMF_MAXSTR) :: errmsg

   localrc = ESMF_RC_NOT_IMPL

   ! check input variables
   ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)
   call ESMF_StateValidate(state, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Assume failure until we know we will succeed 
      if (present(rc)) rc=ESMF_RC_NOT_IMPL
      ! TODO: do we need an empty (or invalid) item to mark failure?

      exists = ESMF_StateClassFindData(state%statep,   &
                 dataname=itemName, expected=.true., &
                 dataitem=dataitem,  &
                 rc=localrc)
      if (.not. exists) then
          write(errmsg, *) "no AbstractItem found named: ", trim(itemName)
          if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, msg=errmsg, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if (dataitem%otype .ne. ESMF_STATEITEM_ABSTRACTITEM) then
          write(errmsg, *) trim(itemName), " found but not type AbstarctItem"
          if (ESMF_LogFoundError(ESMF_RC_ARG_INCOMP, msg=errmsg, &
                ESMF_CONTEXT, rcToReturn=rc)) return 
      endif

!!$#if !defined (stateversion)
      item = dataitem%datap%asip%wrapper
!!$#else
!!$      item%statep => dataitem%datap%mcomponent
!!$      ! validate created state
!!$      ESMF_INIT_SET_CREATED(nestedState)
!!$#endif
      if (present(rc)) rc = ESMF_SUCCESS
end subroutine ESMF_StateGetAbstractItem
#undef ESMF_METHOD
