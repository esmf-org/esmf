!  $Id: ESMF_Bundle_C.F90,v 1.3 2004/10/04 20:07:14 nscollins Exp $
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
!      '$Id: ESMF_Bundle_C.F90,v 1.3 2004/10/04 20:07:14 nscollins Exp $'
!==============================================================================
   subroutine f_esmf_bundlecreate(bundlep, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_BundleMod
     type(ESMF_Bundle), pointer :: bundlep
     type(ESMF_Bundle), target :: thebundle
     integer, intent(out) :: rc              
     integer :: localrc

     thebundle = ESMF_BundleCreate(rc=localrc)
    
     bundlep => thebundle
     rc = localrc
   end subroutine f_esmf_bundlecreate

   subroutine f_esmf_bundledestroy(bundlep, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_BundleMod
     type(ESMF_Bundle), pointer :: bundlep      
     integer, intent(out), optional :: rc     

     call ESMF_BundleDestroy(bundlep, rc)


   end subroutine f_esmf_bundledestroy

