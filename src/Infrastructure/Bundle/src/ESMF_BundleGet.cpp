! $Id: ESMF_BundleGet.cpp,v 1.5 2004/06/08 09:27:15 nscollins Exp $
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
^define ESMF_FILENAME "ESMF_BundleGet.F90"
!
!     ESMF BundleGet module
      module ESMF_BundleGetMod
!
!==============================================================================
!
! This file contains the Bundle class methods which are automatically
!  generated from macros to handle the type/kind/rank overloading.
!  See ESMF_Bundle.F90 for non-macroized functions and subroutines.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below.  they are created by the files which
!   define various macros. >
#include "ESMF_BundleGetMacros.h"
^include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_BundleGetDataPointer
 
!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_BundleGet.cpp,v 1.5 2004/06/08 09:27:15 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_BundleGetDataPointer -- Get a Fortran pointer to the data contents

! !INTERFACE:
     interface ESMF_BundleGetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
InterfaceMacro(BundleGetDataPointer)

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_BundleGetDataPointer} subroutines.   
!  
!EOP 
end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      
      ! < declarations of subroutines for each T/K/R >
DeclarationMacro(BundleGetDataPointer)


        end module ESMF_BundleGetMod

