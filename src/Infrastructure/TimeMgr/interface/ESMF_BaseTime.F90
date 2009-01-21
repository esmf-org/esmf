! $Id: ESMF_BaseTime.F90,v 1.21.2.3 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_BaseTime.F90"
!==============================================================================
!
!     ESMF BaseTime Module
      module ESMF_BaseTimeMod
!
!==============================================================================
!
! This file contains the BaseTime class definition and all BaseTime class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_TimeMgr.inc"
!
!===============================================================================
!BOPI
! !MODULE: ESMF_BaseTimeMod - Base ESMF time definition 
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implemenation.
!
! This module serves only as the common Time definition inherited
! by {\tt ESMF\_TimeInterval} and {\tt ESMF\_Time}.
!
! See {\tt ../include/ESMC\_BaseTime.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod    ! ESMF Base class
      use ESMF_InitMacrosMod
      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_BaseTime
!
!     ! Base class type to match C++ BaseTime class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_BaseTime
      sequence                         ! for C++ interoperability
      private
#ifndef ESMF_NO_INITIALIZERS)
        integer(ESMF_KIND_I8) :: s    = 0  ! whole seconds
        integer(ESMF_KIND_I4) :: sN   = 0  ! fractional seconds, numerator
        integer(ESMF_KIND_I4) :: sD   = 0  ! fractional seconds, denominator
        integer               :: pad1 = 0  ! to match halem C++ <vtbl> long[4]*
        integer               :: pad2 = 0  ! to match halem C++ <vtbl> long[6]*
#else
        integer(ESMF_KIND_I8) :: s     ! whole seconds
        integer(ESMF_KIND_I4) :: sN    ! fractional seconds, numerator
        integer(ESMF_KIND_I4) :: sD    ! fractional seconds, denominator
        integer               :: pad1  ! to match halem C++ <vtbl> long[4]*
        integer               :: pad2  ! to match halem C++ <vtbl> long[6]*
#endif
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_BaseTime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! None exposed at Fortran API layer; inherited through
! ESMF_TimeInterval and ESMF_Time
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_BaseTime.F90,v 1.21.2.3 2009/01/21 21:25:23 cdeluca Exp $'

!------------------------------------------------------------------------------

      end module ESMF_BaseTimeMod
