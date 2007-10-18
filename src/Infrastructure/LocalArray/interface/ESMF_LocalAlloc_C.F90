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
!      '$Id: ESMF_LocalAlloc_C.F90,v 1.4.4.3 2007/10/18 02:42:57 cdeluca Exp $'
!==============================================================================
   subroutine f_esmf_localarrayf90allocate(array, rank, type, kind, counts, &
                                           lbounds, ubounds, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_LocalArrayMod
     type(ESMF_LocalArray) :: array
     integer :: rank
     type(ESMF_DataType) :: type
     type(ESMF_DataKind) :: kind
     integer :: counts(rank)
     integer :: lbounds(rank)
     integer :: ubounds(rank)
     integer, intent(out), optional :: rc     

     ! Beware - these args are not in the same order
     call ESMF_LocalArrConstrF90Ptr(array, counts, rank, type, kind, &
                                                        lbounds, ubounds, rc)
    
   end subroutine f_esmf_localarrayf90allocate

   subroutine f_esmf_localarrayf90deallocate(array, rank, type, kind, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_LocalArrayMod
     type(ESMF_LocalArray) :: array
     integer :: rank
     type(ESMF_DataType) :: type
     type(ESMF_DataKind) :: kind
     integer, intent(out), optional :: rc     

     call ESMF_LocalArrayF90Deallocate(array, rank, type, kind, rc)
    
   end subroutine f_esmf_localarrayf90deallocate


