! $Id: ESMF_FieldComm.F90,v 1.99 2008/04/30 17:27:41 feiliu Exp $
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
!
#define ESMF_FILENAME "ESMF_FieldComm.F90"
!
!     ESMF Field Communications module
      module ESMF_FieldCommMod
!
!==============================================================================
!
! This file contains the Field communication methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldCommMod - Communication routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Field} class
! communication routines, including Regridding, Redistribution, Halo, Gather,
! Scatter, and others.
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_VMMod
      use ESMF_DELayoutMod    ! ESMF layout class
      use ESMF_LocalArrayMod
      use ESMF_RHandleMod
      use ESMF_FieldMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private

!  <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldComm.F90,v 1.99 2008/04/30 17:27:41 feiliu Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!
!==============================================================================
!
!      contains
!
!==============================================================================
!------------------------------------------------------------------------------

      end module ESMF_FieldCommMod
