! $Id: ESMF_ArraySpec_C.F90,v 1.1 2008/02/29 17:44:02 rosalind Exp $
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
! F77 interface files for C layer calling into F90 implementation layer.
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
!      '$Id: ESMF_ArraySpec_C.F90,v 1.1 2008/02/29 17:44:02 rosalind Exp $'
!==============================================================================

!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
!
! The code in this file implements the interface code between C and F90
!  for the {\tt ArraySpec} entry points.  When the user calls an
!  {\tt ESMC_ArraySpec}XXX method, that code calls these functions, which
!  in turn call the F90 module code.  C cannot call directly into an
!  F90 module because the module routine names are altered in a similar
!  fashion as C++ name mangling.
!
!EOP
!------------------------------------------------------------------------------

  subroutine f_esmf_arrayspecset(arrayspec, rank, typekind, rc)

  use ESMF_ArraySpecMod
  use ESMF_UtilTypesMod


  type(ESMF_ArraySpec) :: arrayspec
  integer :: rank
  type(ESMF_TypeKind) :: typekind
  integer :: rc

  call ESMF_ArraySpecSet(arrayspec, rank, typekind, rc)

  return
  end

!---------------------------------------------------------------

  subroutine f_esmf_arrayspecget(arrayspec, rank, typekind, rc)

  use ESMF_ArraySpecMod
  use ESMF_UtilTypesMod


  type(ESMF_ArraySpec) :: arrayspec
  integer :: rank
  type(ESMF_TypeKind) :: typekind
  integer :: rc

  call ESMF_ArraySpecGet(arrayspec, rank, typekind, rc)

  return
  end
