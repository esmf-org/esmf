! $Id: ESMF_State_C.F90,v 1.9 2008/02/29 18:25:24 rosalind Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable 
!   arrays, or ...
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
!      '$Id: ESMF_State_C.F90,v 1.9 2008/02/29 18:25:24 rosalind Exp $'
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
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod

       type(ESMF_State) :: state
       type(ESMF_Array) :: array
      integer, intent(out) :: rc

       print*,'In f_esmf_stateaddarray, before call to ESMF_StateAddArray'

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       call ESMF_StateAddArray(state=state, array=array, rc=rc)

       print*,'In f_esmf_stateaddarray, after  call to ESMF_StateAddArray'

   end subroutine f_esmf_stateaddarray

!------------------------------------------------------------------------------
   subroutine f_esmf_statedestroy(state, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod

       type(ESMF_State) :: state
       integer :: rc              

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       call ESMF_StateDestroy(state, rc)
    
   end subroutine f_esmf_statedestroy

   subroutine f_esmf_stateadddata(statep, name, func, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod

       character(*) :: name
       integer :: func
       integer :: rc              

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       !call ESMF_StateAddData(statep, rc)
    
   end subroutine f_esmf_stateadddata

   subroutine f_esmf_stategetdata(statep, name, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod

       type(ESMF_State), pointer :: statep      
       character(*) :: name
       integer :: rc     

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       !call ESMF_StateGetData(statep, rc)

   end subroutine f_esmf_stategetdata

   subroutine f_esmf_stateget(statep, name, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod

       type(ESMF_State), pointer :: statep      
       character(*) :: name
       integer :: rc     

       ! Initialize return code; assume routine not implemented
       rc = ESMF_RC_NOT_IMPL

       !call ESMF_StateGet(statep, rc)

   end subroutine f_esmf_stateget


   ! TODO: add rest of state entry points

