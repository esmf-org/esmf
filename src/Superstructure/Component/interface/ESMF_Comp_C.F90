!  $Id: ESMF_Comp_C.F90,v 1.1 2003/02/20 16:00:23 nscollins Exp $
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
!      '$Id: ESMF_Comp_C.F90,v 1.1 2003/02/20 16:00:23 nscollins Exp $'
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
   function f_esmf_compcreate(name, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod

       character(*) :: name
       integer :: rc              

       !f_esmf_compcreate = ESMF_CompCreate(name, rc)
       f_esmf_compcreate = 0
    
   end function f_esmf_compcreate

   subroutine f_esmf_compdestroy(compp, name, func, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod

       character(*) :: name
       integer :: func
       integer :: rc              

       !call ESMF_CompDestroy(compp, rc)
    
   end subroutine f_esmf_compdestroy

   subroutine f_esmf_compinit(compp, name, func, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod

       character(*) :: name
       integer :: func
       integer :: rc              

       !call ESMF_CompInit(compp, rc)
    
   end subroutine f_esmf_compinit

   subroutine f_esmf_comprun(compp, name, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod

       type(ESMF_Comp), pointer :: compp      
       character(*) :: name
       integer :: rc     

       !call ESMF_CompRun(compp, rc)

   end subroutine f_esmf_comprun

   subroutine f_esmf_compfinalize(compp, name, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod

       type(ESMF_Comp), pointer :: compp      
       character(*) :: name
       integer :: rc     

       !call ESMF_CompFinalize(compp, rc)

   end subroutine f_esmf_compfinalize

