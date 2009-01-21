! $Id: ESMF_InternArrayDataMapEx.F90,v 1.6.2.3 2009/01/21 21:25:21 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_ArrayDataMapEx

!------------------------------------------------------------------------------
!ESMF_not_working_EXAMPLE	String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_ArrayDataMapEx - Array DataMap manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of Array DataMap set and get usage
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! local variables 
      type(ESMF_ArrayDataMap) :: arrayDM
      integer :: drank
      integer :: dlist(ESMF_MAXDIM), dcounts(ESMF_MAXDIM)


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
!\subsubsection{Setting Array DataMap Defaults and Invalidation}

! This example shows how to set the default values in 
! an {\tt ESMF\_ArrayDataMap}, and how to intentionally
! mark an {\tt ESMF\_ArrayDataMap} invalid.
!EOP

!BOC
      ! Set up a default data map for a Array with 2D data,
      ! and a 1-for-1 mapping with the IGrid.
      call ESMF_ArrayDataMapSetDefault(arrayDM, 2, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Default values for ArrayDataMap = "
      call ESMF_ArrayDataMapPrint(arrayDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


!BOC
      call ESMF_ArrayDataMapSetInvalid(arrayDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Invalid ArrayDataMap = "
      call ESMF_ArrayDataMapPrint(arrayDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOP
!\subsubsection{Setting Array DataMap Values}

! This example shows how to set values in an {\tt ESMF\_ArrayDataMap}.
!EOP

!BOC
      dlist(1:3) = (/ 1, 2, 0 /)
      dcounts(1) = 4
      call ESMF_ArrayDataMapSet(arrayDM, dataRank=3, dataIndexList=dlist, &
                                counts=dcounts, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "ArrayDataMap after set = "
      call ESMF_ArrayDataMapPrint(arrayDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOP
!\subsubsection{Getting Array DataMap Values}

! This example shows how to query an {\tt ESMF\_ArrayDataMap}.
!EOP

!BOC
      call ESMF_ArrayDataMapGet(arrayDM, drank, dlist, dcounts, rc=rc)
      print *, "Returned values from Array DataMap:"
      print *, "rank =", drank
      print *, "correspondance to igrid indices = ", dlist
      print *, "counts for non-igrid dimensions =", dcounts
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_ArrayDataMapEx.F90"
      else
        print *, "FAIL: ESMF_ArrayDataMapEx.F90"
      end if

!BOC
      end program ESMF_ArrayDataMapEx
!EOC
