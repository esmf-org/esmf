! $Id$
!
! Example code which contains the include info required by the user
! model

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User negotitation info required by the user model
!
!
!\begin{verbatim}

    module user_neg_info

    ! ESMF Framework module
    use ESMF

    implicit none

!------------------------------------------------------------------------------
! ! ESMF_GridCompUserNegInfo accInfo types

  integer, parameter ::&
    ESMF_COMP_NO_ACC=0, ESMF_COMP_CAN_ACC=1, ESMF_COMP_MUST_ACC=2

!------------------------------------------------------------------------------
! ! ESMF_GridCompUserNegInfo state types

  integer, parameter ::&
    ESMF_COMP_USER_NEG_NOTPRESENT=0,&
    ESMF_COMP_USER_NEG_INIT=1, ESMF_COMP_USER_NEG_FINALIZE=2,&
    ESMF_COMP_USER_NEG_INPROGRESS=3

!------------------------------------------------------------------------------
! ! ESMF_GridCompUserNegInfo petlist info types

  integer, parameter ::&
    ESMF_COMP_USER_NEG_PETLIST_INFO_ENUMERATE=0,&
    ESMF_COMP_USER_NEG_PETLIST_INFO_TUPLE=1
!------------------------------------------------------------------------------

  public  ESMF_COMP_NO_ACC, ESMF_COMP_CAN_ACC, ESMF_COMP_MUST_ACC
  public  ESMF_COMP_USER_NEG_NOTPRESENT,&
          ESMF_COMP_USER_NEG_INIT, ESMF_COMP_USER_NEG_FINALIZE,&
          ESMF_COMP_USER_NEG_INPROGRESS

  public  ESMF_COMP_USER_NEG_PETLIST_INFO_ENUMERATE,&
          ESMF_COMP_USER_NEG_PETLIST_INFO_TUPLE

    end module user_neg_info
    
!\end{verbatim}
