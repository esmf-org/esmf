! $Id: ESMF_Fraction.F90,v 1.4 2003/04/23 21:39:34 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Fraction Module
!
! (all lines below between the !BOP and !EOP markers will be included in
!  the automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_TimeMgr.inc>

!------------------------------------------------------------------------------
! module definition

      module ESMF_FractionMod
!===============================================================================
!BOPI
!
! !MODULE: ESMF_FractionMod
!
! !DESCRIPTION:
!
!
!
!
!
!
! !USES:
!------------------------------------------------------------------------------

!
! !PUBLIC TYPES:
      implicit none

      type ESMF_Fraction
      sequence
      private
        integer :: n    ! fractional numerator
        integer :: d    ! fractional denominator
      end type
!
! !PUBLIC MEMBER FUNCTIONS:
!
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
!     Part of Time Manager F90 API wrapper of C++ implemenation

!------------------------------------------------------------------------------

!     contains

!------------------------------------------------------------------------------

      ! wrappers to C++ fraction routines

!EOPI
!===============================================================================
      end module ESMF_FractionMod
