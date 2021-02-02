! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_IO_NetCDF.F90"
!==============================================================================
!
!     ESMF I/O NetCDF Module
      module ESMF_IO_NetCDFMod
!     
!==============================================================================
!     
! This file contains the I/O NetCDF class definition and all class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_IO_NetCDFMod
!     
! !DESCRIPTION:
! Part of I/O NetCDF Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMCI\_IO\_NetCDF} implementation.
!     
! See {\tt ../include/ESMCI\_IO\_NetCDF.h} for complete description.  TODO ??
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_InitMacrosMod

      ! associated derived types
      ! use ESMF_???Mod TODO

      ! type definition for this module
      ! use ESMF_IOTypeMod  TODO ??

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------

      type ESMF_IO_NetCDF
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      private
        type(ESMF_Pointer) :: this
        ESMF_INIT_DECLARE
      end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_IO_NetCDF
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!EOPI

! !PRIVATE MEMBER FUNCTIONS:


      end module ESMF_IO_NetCDFMod

