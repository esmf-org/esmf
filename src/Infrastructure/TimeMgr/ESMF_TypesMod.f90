! $Id: ESMF_TypesMod.f90,v 1.1 2002/08/18 23:22:49 eschwab Exp $
      module ESMF_TypesMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_TypesMod
!
! !USES:
!
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
