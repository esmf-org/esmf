! $Id: ESMF_FractionMod.f90,v 1.1 2002/08/18 23:22:49 eschwab Exp $
      module ESMF_FractionMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_FractionMod
!
! !USES:
		use ESMF_TypesMod
!
! !PUBLIC TYPES:
        implicit none

		type ESMF_Fraction
			private
			sequence
                integer(int32) :: n    ! fractional numerator
                integer(int32) :: d    ! fractional denominator
		end type
!
! !PUBLIC MEMBER FUNCTIONS:
!
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
!        Part of Time Manager F90 API wrapper of C++ implemenation
!
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!
!EOP
!===============================================================================

	!contains

	! wrappers to C++ fraction routines

	end module ESMF_FractionMod
