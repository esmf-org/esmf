! $Id: ESMF_LocalArrayGet.cpp,v 1.4 2007/03/02 22:37:20 theurich Exp $
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
^define ESMF_FILENAME "ESMF_LocalArrayGet.F90"
!
!     ESMF LocalArrayCreate module
      module ESMF_LocalArrayGetMod
!
!==============================================================================
!
! This file contains the LocalArray class definition and all LocalArray
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below.  they are created by the files which
!   define various macros. >
^include "ESMF.h"
#include "ESMF_TypeKindRankMacros.hcppF90"
#include "ESMF_LocalArrayMacros.h"
#include "ESMF_LocalAllocMacros.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_LocalArrayGetMod - Manage data uniformly between F90 and C++
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_LocalArray} class and 
!  associated functions and subroutines.  
!
! C and C++ arrays are simple pointers to memory.
! Fortran arrays contain shape and stride definitions and are strongly
! typed.  To enable interoperability between the languages the C++ code
! must be able to obtain this information from the Fortran description
! (which is called the "dope vector" in Fortran), either through a priori
! knowledge or through query.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_ArraySpecMod
      use ESMF_LocalArrayCreateMod
      implicit none


!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_LocalArrayGet.cpp,v 1.4 2007/03/02 22:37:20 theurich Exp $'

!------------------------------------------------------------------------------
!     ! Internal wrapper structures for passing f90 pointers to C++ and
!     ! guaranteeing they are passed by reference on all compilers and all
!     ! platforms.  These are never seen outside this module.
!
      ! < these expand into defined type declarations >
AllTypesMacro(LocalArrayType)


!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_LocalArrayGetData

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_LocalArrayGetData -- Get an F90 pointer to the data contents

! !INTERFACE:
     interface ESMF_LocalArrayGetData

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
InterfaceMacro(LocalArrayGetData)

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_LocalArrayGetData} functions.   
!  
!EOP
end interface

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
DeclarationMacro(LocalArrayGetData)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

end module ESMF_LocalArrayGetMod
