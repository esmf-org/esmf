! $Id: ESMF_TimeMod.f90,v 1.1 2002/08/18 23:22:49 eschwab Exp $
      module ESMF_TimeMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_TimeMod
!
! !USES:
        use ESMF_TypesMod
!
! !PUBLIC TYPES:
        implicit none

        type ESMF_Time
            private
            sequence
                integer(int64) :: S        ! whole seconds
                integer(int32) :: Sn    ! fractional seconds, numerator
                integer(int32) :: Sd    ! fractional seconds, denominator
        end type
!
! !PUBLIC MEMBER FUNCTIONS:
!        None exposed at F90 API layer; inherited through ESMF\_TimeInterval
!        and ESMF\_TimeInstant
!
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
!        Part of Time Manager F90 API wrapper of C++ implemenation
!
!        This module serves only as the common Time definition inherited
!        by ESMF\_TimeInterval and ESMF\_TimeInstant
!
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!
!EOP
!===============================================================================

    end module ESMF_TimeMod
