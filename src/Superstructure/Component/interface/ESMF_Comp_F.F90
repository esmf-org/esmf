!  $Id: ESMF_Comp_F.F90,v 1.1 2003/02/14 22:44:21 nscollins Exp $
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
!      '$Id: ESMF_Comp_F.F90,v 1.1 2003/02/14 22:44:21 nscollins Exp $'
!==============================================================================

   subroutine f_esmf_compsetroutine(compp, name, func, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod

       character(*) :: name
       integer :: func
       integer :: rc              

       ! call ESMF_CompXXX()
    
   end subroutine f_esmf_compsetroutine

   subroutine f_esmf_compcallroutine(compp, name, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod

       type(ESMF_Comp), pointer :: compp      
       character(*) :: name
       integer :: rc     

       call ESMF_CompDestroy(compp, rc)

   end subroutine f_esmf_compcallroutine

