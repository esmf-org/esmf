! $Id: ESMF_TODMod.f,v 1.1 2002/11/15 21:35:37 jwolfe Exp $
	module ESMF_TODMod
!===============================================================================
!BOP
! !MODULE: ESMF_TODMod
! 
! !USES:
!
! !PUBLIC TYPES:
	implicit none

	type ESMF_TOD 
          private
          sequence
          integer(8) type
          integer(8) sec
          integer(8) msec
	end type ESMF_TOD 
!
! !DESCRIPTION:
! Describes and contains a time of day.
!EOP
!===============================================================================

	end module
