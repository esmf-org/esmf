module ESMF_AbstractStateItemMod
   use ESMF_UtilTypesMod
   implicit none
   private

   public :: ESMF_AbstractItem

   public :: ESMF_AbstractStateItem
   public :: ESMF_AbstractItemWrapper

   public :: ESMF_AbstractStateItemGetInit
   public :: ESMF_AbstractStateItemGet

   type, abstract :: ESMF_AbstractItem
   end type ESMF_AbstractItem


   type :: FakeAbstractItem
      sequence
      integer :: placeholder(2)
   end type FakeAbstractItem
   ! This wrapper is intended to just to be the the same size as user
   ! wrappers containing their own pointers to objects of types that
   ! extend AbstractStateItem.
   type :: ESMF_AbstractItemWrapper
      sequence
      type(FakeAbstractItem), pointer :: item
   end type ESMF_AbstractItemWrapper

   type :: ESMF_AbstractStateItem
      sequence
      character(ESMF_MAXSTR) :: name
      type(ESMF_AbstractItemWrapper) :: wrapper
   end type ESMF_AbstractStateItem



#include "ESMF.h"

contains

   subroutine ESMF_AbstractStateItemGet(item, name, keywordEnforcer, rc)
      type(ESMF_AbstractStateItem), intent(in) :: item
      character(len=ESMF_MAXSTR), intent(out) :: name
      type(ESMF_KeywordEnforcer), optional :: keywordEnforcer
      integer, optional, intent(out) :: rc

      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      name = item%name
      ! Return successfully
      if (present(rc)) rc = ESMF_SUCCESS
      
   end subroutine ESMF_AbstractStateItemGet

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AbstractStateItemWrapperInit"
!BOPI
! !IROUTINE: ESMF_AbstractStateItemGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_AbstractStateItemGetInit(si) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_AbstractStateItemGetInit   
!
! !ARGUMENTS:
      type(ESMF_AbstractStateItem), intent(in), optional :: si
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [si]
!           StateItem object.
!     \end{description}
!
!EOPI

!!$    if (present(si)) then
!!$      ESMF_AbstractStateItemGetInit = ESMF_INIT_GET(si)
!!$    else
!!$      ESMF_AbstractStateItemGetInit = ESMF_INIT_DEFINED
!!$    endif
      ESMF_AbstractStateItemGetInit = ESMF_INIT_DEFINED

   end function ESMF_AbstractStateItemGetInit
!------------------------------------------------------------------------------

end module ESMF_AbstractStateItemMod
