! $Id: ESMF_StateGet.cpp,v 1.9 2007/01/04 23:19:17 oehmke Exp $
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
^define ESMF_FILENAME "ESMF_StateGet.F90"
!
!     ESMF StateGet module
      module ESMF_StateGetMod
!
!==============================================================================
!
! This file contains the State class methods which are automatically
!  generated from macros to handle the type/kind/rank overloading.
!  See ESMF_State.F90 for non-macroized entry points.
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below.  they are created by the files which
!   define various macros. >
#include "ESMF_StateMacros.h"
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
      use ESMF_StateMod
      use ESMF_InitMacrosMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateGetDataPointer
 
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateGet.cpp,v 1.9 2007/01/04 23:19:17 oehmke Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOPI
! !IROUTINE: ESMF_StateGetDataPointer -- Get an F90 pointer to the data contents

! !INTERFACE:
     interface ESMF_StateGetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
InterfaceMacro(StateGetDataPointer)

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateGetDataPointer} subroutines.   
!  
!EOPI
end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      ! < declarations of subroutines for each T/K/R >
DeclarationMacro(StateGetDataPointer)


        end module ESMF_StateGetMod

