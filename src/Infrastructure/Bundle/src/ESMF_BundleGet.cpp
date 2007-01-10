! $Id: ESMF_BundleGet.cpp,v 1.10 2007/01/10 00:16:55 oehmke Exp $
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
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod
      use ESMF_InternArrayMod
      use ESMF_InternArrayGetMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      use ESMF_InitMacrosMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_BundleGetDataPointer
 
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_BundleGet.cpp,v 1.10 2007/01/10 00:16:55 oehmke Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOPI
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
!EOPI
end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      
      ! < declarations of subroutines for each T/K/R >
DeclarationMacro(BundleGetDataPointer)


        end module ESMF_BundleGetMod

