! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_State.F90"
!
!     ESMF State module
module ESMF_StateMod
!
!==============================================================================
!
! This file contains the State class definition and all State
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateMod - Data exchange between components
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran function and subroutine 
!  interfaces to the {\tt State} class and associated data structures.
!
!
! !USES:
      use ESMF_StateAPIMod
      use ESMF_StateRemRepMod

      implicit none
      
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_State               ! implemented in ESMF_StateTypesMod

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public operator(==)
      public operator(/=)

      public ESMF_StateCreate, ESMF_StateDestroy
      public ESMF_StateIsCreated
      
      public ESMF_StateDestruct    ! for ESMF garbage collection

      public ESMF_StateAdd, ESMF_StateAddReplace
      public ESMF_StateGet
      public ESMF_StateIsReconcileNeeded
      public ESMF_StateRemove
      public ESMF_StateReplace


      public ESMF_StateWriteRestart
      public ESMF_StateReadRestart

      public ESMF_StateRead
      public ESMF_StateWrite
      public ESMF_StatePrint

      public ESMF_StateSerialize, ESMF_StateDeserialize

      public ESMF_StateClassFindData
      
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!------------------------------------------------------------------------------

end module ESMF_StateMod
