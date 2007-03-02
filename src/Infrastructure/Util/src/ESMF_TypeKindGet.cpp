! $Id: ESMF_TypeKindGet.cpp,v 1.4 2007/03/02 22:58:21 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2006, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
^define ESMF_FILENAME "ESMF_TypeKindGet.F90"

!     ESMF TypeKindGet module
      module ESMF_TypeKindGetMod

!==============================================================================
!
! This file contains wordsize functions that are automatically
!  generated from macros to handle the type/kind overloading.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below.  they are created by the files which
! define various macros. >
#include "ESMF_TypeKindMacros.hcppF90"
#include "ESMF_TypeKindGetMacros.h"
^include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
     use ESMF_UtilTypesMod
     use ESMF_LogErrMod

     implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC FUNCTION:

      public ESMF_TypeKindGet

!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_TypeKindGet"
!BOP
! !IROUTINE: ESMF_TypeKindGet -- Generic interface to return the correct
!                                ESMF_TypeKind parameter of a scalar.
!
! !INTERFACE:


    interface ESMF_TypeKindGet
!EOP

      ! < interfaces for each TK >
TypeKindInterfaceMacro(ESMF_TypeKindGet)

    end interface ESMF_TypeKindGet

    contains

!==============================================================================
!------------------------------------------------------------------------------

TypeKindDeclarationMacro(ESMF_TypeKindGet)

!! < end of automatically generated functions >
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!^undef  ESMF_METHOD

    end module ESMF_TypeKindGetMod
