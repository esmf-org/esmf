! $Id: ESMF_BundleDataMapEx.F90,v 1.1 2004/06/03 12:50:05 nscollins Exp $
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
      program ESMF_BundleDataMapEx

!------------------------------------------------------------------------------
!EXAMPLE	String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_BundleDataMapEx - BundleDataMap manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of BundleDataMap set and get usage
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! local variables 
      type(ESMF_BundleDataMap) :: bundleDM
      type(ESMF_BundleInterleave) :: il


      ! return code
      integer:: rc
!EOC

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOP
!\subsubsection{Setting BundleDataMap Defaults}

! This example shows how to set the default values in 
! an {\tt ESMF\_BundleDataMap}.
!EOP

!BOC
      call ESMF_BundleDataMapSetDefault(bundleDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Default values for BundleDataMap = "
      call ESMF_BundleDataMapPrint(bundleDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOP
!\subsubsection{Setting BundleDataMap Values}

! This example shows how to set values in an {\tt ESMF\_BundleDataMap}.
!EOP

!BOC
      il = ESMF_BIL_BYITEM
      call ESMF_BundleDataMapSet(bundleDM, bundleInterleave=il, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "BundleDataMap after setting interleave = "
      call ESMF_BundleDataMapPrint(bundleDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOP
!\subsubsection{Getting BundleDataMap Values}

! This example shows how to query an {\tt ESMF\_BundleDataMap}.
!EOP

!BOC
      call ESMF_BundleDataMapGet(bundleDM, bundleInterleave = il, rc=rc)
      if (il .eq. ESMF_BIL_BYITEM) then
        print *, "Interleaved by individual data items"
      else if (il .eq. ESMF_BIL_BYFIELD) then
        print *, "Interleaved by fields"
      endif
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_BundleDataMapEx.F90"
      else
        print *, "FAIL: ESMF_BundleDataMapEx.F90"
      end if

!BOC
      end program ESMF_BundleDataMapEx
!EOC
