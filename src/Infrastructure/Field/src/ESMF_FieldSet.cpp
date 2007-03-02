! $Id: ESMF_FieldSet.cpp,v 1.11 2007/03/02 22:58:21 theurich Exp $
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
^define ESMF_FILENAME "ESMF_FieldSet.F90"
!
!     ESMF FieldSet module
      module ESMF_FieldSetMod
!
!==============================================================================
!
! This file contains the Field class methods which are automatically
!  generated from macros to handle the type/kind/rank overloading.
!  See ESMF_Field.F90 for non-macroized functions and subroutines.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below.  they are created by the files which
!   define various macros. >
^include "ESMF.h"
#include "ESMF_TypeKindRankMacros.hcppF90"
#include "ESMF_FieldSetMacros.h"

!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod
      use ESMF_InternArrayMod
      use ESMF_InternArrayCreateMod
      use ESMF_FieldMod
      use ESMF_InitMacrosMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_FieldSetDataPointer
 
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldSet.cpp,v 1.11 2007/03/02 22:58:21 theurich Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOPI
! !IROUTINE: ESMF_FieldSetDataPointer -- Set a Fortran pointer to the data contents

! !INTERFACE:
     interface ESMF_FieldSetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
TypeKindRankInterfaceMacro(FieldSetDataPointer)

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_FieldSetDataPointer} subroutines.   
!  
!EOPI 
end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      ! < declarations of subroutines for each T/K/R >
TypeKindRankDeclarationMacro(FieldSetDataPointer)


        end module ESMF_FieldSetMod
