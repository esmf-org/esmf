! $Id: ESMF_BaseTime.F90,v 1.2 2003/04/10 22:23:51 eschwab Exp $
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
#include <conf.h>   ! TODO: remove when padding in type ESMF_BaseTime removed
#include <ESMF_TimeMgr.inc>
!
!===============================================================================
!BOP
! !MODULE: ESMF_BaseTimeMod - Base ESMF time definition 
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! This module serves only as the common Time definition inherited
! by {\tt ESMF\_TimeInterval} and {\tt ESMF\_Time}
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod    ! ESMF Base class
      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_BaseTime
!
!     ! Base class to match C++ BaseTime class in size and sequence

      type ESMF_BaseTime
      sequence                        ! for C++ interoperability
      private
        integer(ESMF_IKIND_I8) :: S   ! whole seconds
        integer                :: Sn  ! fractional seconds, numerator
        integer                :: Sd  ! fractional seconds, denominator
! TODO:
#ifdef PARCH_alpha
        integer                :: pad1  ! to match halem C++ <vtbl> long[4]*
        integer                :: pad2  ! to match halem C++ <vtbl> long[6]*
#endif
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_BaseTime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! None exposed at F90 API layer; inherited through
! ESMF_TimeInterval and ESMF_Time
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_BaseTime.F90,v 1.2 2003/04/10 22:23:51 eschwab Exp $'

!------------------------------------------------------------------------------

      end module ESMF_BaseTimeMod
