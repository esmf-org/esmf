! $Id: ESMF_StateGet_F90.cpp,v 1.1 2004/02/05 21:53:55 nscollins Exp $
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
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayBaseMod
      use ESMF_ArrayExpandMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      use ESMF_StateMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateGetDataPointer
 
!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateGet_F90.cpp,v 1.1 2004/02/05 21:53:55 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_StateGetDataPointer -- Get an F90 pointer to the data contents

! !INTERFACE:
     interface ESMF_StateGetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
StateInterfaceMacro(StateGetDataPointer)

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_StateGetDataPointer} subroutines.   
!  
!EOP 
end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual subroutine bodies after expansion >

StateGetDataPointerMacro(integer, I2, 1, COL1, LEN1, RNG1, LOC1)

StateGetDataPointerMacro(integer, I4, 1, COL1, LEN1, RNG1, LOC1)

StateGetDataPointerMacro(integer, I8, 1, COL1, LEN1, RNG1, LOC1)

StateGetDataPointerMacro(integer, I2, 2, COL2, LEN2, RNG1, LOC2)

StateGetDataPointerMacro(integer, I4, 2, COL2, LEN2, RNG2, LOC2)

StateGetDataPointerMacro(integer, I8, 2, COL2, LEN2, RNG2, LOC2)

StateGetDataPointerMacro(integer, I2, 3, COL3, LEN3, RNG3, LOC3)

StateGetDataPointerMacro(integer, I4, 3, COL3, LEN3, RNG3, LOC3)

StateGetDataPointerMacro(integer, I8, 3, COL3, LEN3, RNG3, LOC3)

StateGetDataPointerMacro(integer, I2, 4, COL4, LEN4, RNG4, LOC4)

StateGetDataPointerMacro(integer, I4, 4, COL4, LEN4, RNG4, LOC4)

StateGetDataPointerMacro(integer, I8, 4, COL4, LEN4, RNG4, LOC4)

StateGetDataPointerMacro(integer, I2, 5, COL5, LEN5, RNG5, LOC5)

StateGetDataPointerMacro(integer, I4, 5, COL5, LEN5, RNG5, LOC5)

StateGetDataPointerMacro(integer, I8, 5, COL5, LEN5, RNG5, LOC5)

StateGetDataPointerMacro(real, R4, 1, COL1, LEN1, RNG1, LOC1)

StateGetDataPointerMacro(real, R8, 1, COL1, LEN1, RNG1, LOC1)

StateGetDataPointerMacro(real, R4, 2, COL2, LEN2, RNG2, LOC2)

StateGetDataPointerMacro(real, R8, 2, COL2, LEN2, RNG2, LOC2)

StateGetDataPointerMacro(real, R4, 3, COL3, LEN3, RNG3, LOC3)

StateGetDataPointerMacro(real, R8, 3, COL3, LEN3, RNG3, LOC3)

StateGetDataPointerMacro(real, R4, 4, COL4, LEN4, RNG4, LOC4)

StateGetDataPointerMacro(real, R8, 4, COL4, LEN4, RNG4, LOC4)

StateGetDataPointerMacro(real, R4, 5, COL5, LEN5, RNG5, LOC5)

StateGetDataPointerMacro(real, R8, 5, COL5, LEN5, RNG5, LOC5)


        end module ESMF_StateGetMod

