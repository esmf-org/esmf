! $Id: ESMF_Types.F90,v 1.2 2002/10/28 18:16:13 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Types Module
!
! (all lines below between the !BOP and !EOP markers will be included in
!  the automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_TimeMgr.h>

!------------------------------------------------------------------------------
! module definition

      module ESMF_TypesMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_TypesMod
!
! !USES:
!
!------------------------------------------------------------------------------

! !PUBLIC TYPES:
        implicit none

        ! define 64 and 32 bit integer types
        integer, parameter :: int64 = selected_int_kind(18)
        integer, parameter :: int32 = selected_int_kind(9)
!
! !PUBLIC MEMBER FUNCTIONS:
!
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
!        Part of Time Manager F90 API wrapper of C++ implemenation
!        Define platform-independent 64-bit and 32-bit integer types
!
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!
!EOP
!===============================================================================

	end module ESMF_TypesMod
