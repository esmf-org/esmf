!  $Id: ESMF_Alloc_C.F90,v 1.4 2007/02/16 05:27:45 rosalind Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Alloc_C.F90"
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable 
!   s, or ...
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
!      '$Id: ESMF_Alloc_C.F90,v 1.4 2007/02/16 05:27:45 rosalind Exp $'
!==============================================================================
   subroutine f_esmf_arrayf90allocate(array, rank, type, kind, counts, &
                                      lbounds, ubounds, hwidth, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_InternArrayMod
       use ESMF_InternArrayCreateMod
     type(ESMF_InternArray) :: array
     integer :: rank
     type(ESMF_DataType) :: type
     type(ESMF_TypeKind) :: kind
     integer :: counts(rank)
     integer :: lbounds(rank)
     integer :: ubounds(rank)
     integer :: hwidth
     integer, intent(out) :: rc     

     ! Beware - these args are not in the same order
     call ESMF_InternArrayConstructF90Ptr(array, counts, hwidth, rank, type, kind, &
                                    lbounds, ubounds, rc)
    
   end subroutine f_esmf_arrayf90allocate

   subroutine f_esmf_arrayf90deallocate(array, rank, type, kind, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_InternArrayMod
       use ESMF_InternArrayCreateMod
     type(ESMF_InternArray) :: array
     integer :: rank
     type(ESMF_DataType) :: type
     type(ESMF_TypeKind) :: kind
     integer, intent(out) :: rc     

     call ESMF_InternArrayF90Deallocate(array, rank, type, kind, rc)
    
   end subroutine f_esmf_arrayf90deallocate


