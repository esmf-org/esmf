! $Id: ESMF_TimeMgmtMod.f90,v 1.2 2002/11/26 18:25:35 jwolfe Exp $
      module ESMF_TimeMgmtMod
!===============================================================================
!BOP
! !MODULE: ESMF_TimeMgmtMod
!
! !USES:
!jw      use ESMF_BasicUtilMod
      use ESMF_ErrorMod
      use ESMF_TimeMod
      use ESMF_DateMod
      use ESMF_TimeMgrMod
      use ESMF_AlarmMod
!
! !DESCRIPTION:
! Contains all of the time Management utilities.
!EOP
!===============================================================================

      implicit none

      integer, parameter :: ESMF_SUCCESS=0

      integer, parameter :: ESMF_ERR_RETURN=0,
     &                        ESMF_ERR_EXIT=1,
     &                        ESMF_ERR_USER_DEFINED=2
      integer(8), parameter :: ESMF_NULL = 0

      end module ESMF_TimeMgmtMod













