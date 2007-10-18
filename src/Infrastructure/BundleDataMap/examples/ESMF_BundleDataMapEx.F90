! $Id: ESMF_BundleDataMapEx.F90,v 1.4.4.3 2007/10/18 02:42:28 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
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
      type(ESMF_InterleaveFlag) :: il


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
      il = ESMF_INTERLEAVE_BY_ITEM
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
      if (il .eq. ESMF_INTERLEAVE_BY_ITEM) then
        print *, "Interleaved by individual data items"
      else if (il .eq. ESMF_INTERLEAVE_BY_BLOCK) then
        print *, "Interleaved by fields"
      endif
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
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
