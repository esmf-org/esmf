! $Id: ESMF_FieldSet.cpp,v 1.2 2004/06/07 05:21:07 nscollins Exp $
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
#include "ESMF_FieldSetMacros.h"
^include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayMod
      use ESMF_ArrayCreateMod
      use ESMF_FieldMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_FieldSetDataPointer
 
!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldSet.cpp,v 1.2 2004/06/07 05:21:07 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_FieldSetDataPointer -- Set a Fortran pointer to the data contents

! !INTERFACE:
     interface ESMF_FieldSetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
InterfaceMacro(FieldSetDataPointer)

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_FieldSetDataPointer} subroutines.   
!  
!EOP 
end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      ! < declarations of subroutines for each T/K/R >
DeclarationMacro(FieldSetDataPointer)


        end module ESMF_FieldSetMod
