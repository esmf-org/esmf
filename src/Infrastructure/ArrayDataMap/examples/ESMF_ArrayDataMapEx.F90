! $Id: ESMF_ArrayDataMapEx.F90,v 1.1 2004/06/04 12:09:36 nscollins Exp $
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
      program ESMF_ArrayDataMapEx

!------------------------------------------------------------------------------
!EXAMPLE	String used by test script to count examples.
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
      type(ESMF_IndexOrder) :: indexOrder


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
      ! and a 1-for-1 mapping with the Grid.
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
      call ESMF_ArrayDataMapSet(arrayDM, rc=rc)
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
      call ESMF_ArrayDataMapGet(arrayDM, rc=rc)
      ! print *, "Returned values from Array DataMap:"
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc)
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
