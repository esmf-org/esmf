!  $Id: ESMF_Alloc_F.F90,v 1.3 2002/12/07 00:08:55 nscollins Exp $
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
!      '$Id: ESMF_Alloc_F.F90,v 1.3 2002/12/07 00:08:55 nscollins Exp $'
!==============================================================================
   subroutine f_esmf_allocate2dr4(f90ptr, ni, nj, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_AllocMod   ! ESMF allocation routines
     real*4, pointer :: f90ptr(:,:)
     integer, intent(in) :: ni, nj
     integer, intent(out) :: rc              

     !call ESMF_Allocate(f90ptr, ni, nj, rc)
    
   end subroutine f_esmf_allocate2dr4

   subroutine f_esmf_deallocate2dr4(f90ptr, rc)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_AllocMod   ! ESMF allocation routines
     real*4, pointer :: f90ptr(:,:)
     integer, intent(out), optional :: rc     

     !call ESMF_Deallocate(f90ptr, rc)

   end subroutine f_esmf_deallocate2dr4

  end
