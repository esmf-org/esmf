!  $Id: ESMF_Comp_C.F90,v 1.4 2003/06/26 23:06:43 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
!      '$Id: ESMF_Comp_C.F90,v 1.4 2003/06/26 23:06:43 nscollins Exp $'
!==============================================================================

!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
! 
! The code in this file implements the interface code between C++ and F90
!  for the {\tt Component} entry points.  When the user calls an
!  {\tt ESMC_Comp}XXX method, that code calls these functions, which
!  in turn call the F90 module code.  C++ cannot call directly into an
!  F90 module because the module routine names are altered in a similar
!  fashion as C++ name mangling.
! 
!EOP
!------------------------------------------------------------------------------
   function f_esmf_appcompcreate(name, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_AppCompMod

       type(ESMF_AppComp) :: f_esmf_appcompcreate
       character(*) :: name
       integer :: rc              

       f_esmf_appcompcreate = ESMF_AppCompCreate(name, rc=rc)
    
   end function f_esmf_appcompcreate

   subroutine f_esmf_appcompdestroy(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_AppCompMod

       type(ESMF_AppComp) :: comp
       integer :: rc              

       call ESMF_AppCompDestroy(comp, rc)
    
   end subroutine f_esmf_appcompdestroy

   function f_esmf_gridcompcreate(name, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: f_esmf_gridcompcreate
       character(*) :: name
       integer :: rc              

       f_esmf_gridcompcreate = ESMF_GridCompCreate(name, rc=rc)
    
   end function f_esmf_gridcompcreate

   subroutine f_esmf_gridcompdestroy(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp
       integer :: rc              

       call ESMF_GridCompDestroy(comp, rc)
    
   end subroutine f_esmf_gridcompdestroy

   subroutine f_esmf_gridcompinitialize(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       integer :: rc     

       call ESMF_GridCompInitialize(comp, rc=rc)

   end subroutine f_esmf_gridcompinitialize

   subroutine f_esmf_gridcomprun(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       integer :: rc     

       call ESMF_GridCompRun(comp, rc=rc)

   end subroutine f_esmf_gridcomprun

   subroutine f_esmf_gridcompfinalize(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       integer :: rc     

       call ESMF_GridCompFinalize(comp, rc=rc)

   end subroutine f_esmf_gridcompfinalize

 

   function f_esmf_cplcompcreate(name, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: f_esmf_cplcompcreate
       character(*) :: name
       integer :: rc              

       f_esmf_cplcompcreate = ESMF_CplCompCreate(name, rc=rc)
    
   end function f_esmf_cplcompcreate

   subroutine f_esmf_cplcompdestroy(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp
       integer :: rc              

       call ESMF_CplCompDestroy(comp, rc)
    
   end subroutine f_esmf_cplcompdestroy

   subroutine f_esmf_cplcompinitialize(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: stat
       integer :: rc     

       call ESMF_CplCompInitialize(comp, stat, rc=rc)

   end subroutine f_esmf_cplcompinitialize

   subroutine f_esmf_cplcomprun(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: stat
       integer :: rc     

       call ESMF_CplCompRun(comp, stat, rc=rc)

   end subroutine f_esmf_cplcomprun

   subroutine f_esmf_cplcompfinalize(comp, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: stat
       integer :: rc     

       call ESMF_CplCompFinalize(comp, stat, rc=rc)

   end subroutine f_esmf_cplcompfinalize

 

