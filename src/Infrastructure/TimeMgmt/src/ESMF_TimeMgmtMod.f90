! $Id: ESMF_TimeMgmtMod.f90,v 1.3 2002/12/11 16:06:36 nscollins Exp $
      module ESMF_TimeMgmtMod
!===============================================================================
!BOP
! !MODULE: ESMF_TimeMgmtMod
!
! !USES:
!jw      use ESMF_BasicUtilMod
!nsc   use ESMF_ErrorMod
!nsc   use ESMF_TimeMod
!nsc   use ESMF_DateMod
!nsc   use ESMF_TimeMgrMod
!nsc   use ESMF_AlarmMod
!
! !DESCRIPTION:
! Contains all of the time Management utilities.
!EOP
!===============================================================================

      implicit none

      integer, parameter :: ESMF_SUCCESS=0

      integer, parameter :: ESMF_ERR_RETURN=0, &
                              ESMF_ERR_EXIT=1, &
                              ESMF_ERR_USER_DEFINED=2
      integer(8), parameter :: ESMF_NULL = 0

      end module ESMF_TimeMgmtMod













