! $Id: ESMF_FieldDataMapEx.F90,v 1.10 2007/06/23 04:00:27 cdeluca Exp $
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
      program ESMF_FieldDataMapEx

!------------------------------------------------------------------------------
!EXAMPLE	String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_FieldDataMapEx - Field DataMap manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of Field DataMap set and get usage
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! local variables 
      type(ESMF_FieldDataMap) :: fieldDM
      type(ESMF_RelLoc) :: relativeLocation
      integer :: dataRank, dataIndexList(ESMF_MAXDIM)
      ! integer :: counts(ESMF_MAXDIM)
      ! type(ESMF_IndexOrder) :: indexOrder



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
!\subsubsection{Setting Field DataMap Defaults and Invalidation}

! This example shows how to set the default values in 
! an {\tt ESMF\_FieldDataMap}, and how to intentionally
! mark an {\tt ESMF\_FieldDataMap} invalid.
!EOP

!BOC
      ! Set up a default data map for a Field with 2D data,
      ! and a 1-for-1 mapping with the IGrid.
      call ESMF_FieldDataMapSetDefault(fieldDM, 2, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Default values for FieldDataMap = "
      call ESMF_FieldDataMapPrint(fieldDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE


!BOC
      relativeLocation = ESMF_CELL_NECORNER
      call ESMF_FieldDataMapSetDefault(fieldDM, ESMF_INDEX_IJK, &
                                       horzRelloc=relativeLocation, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "FieldDataMap after set = "
      call ESMF_FieldDataMapPrint(fieldDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_FieldDataMapSetInvalid(fieldDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Invalid FieldDataMap = "
      call ESMF_FieldDataMapPrint(fieldDM, rc=rc)
!EOC


!BOP
!\subsubsection{Setting Field DataMap Values}

! This example shows how to set values in an {\tt ESMF\_FieldDataMap}.
!EOP

!BOC
      relativeLocation = ESMF_CELL_CENTER
      call ESMF_FieldDataMapSet(fieldDM, dataRank=2, &
                                horzRelloc=relativeLocation, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "FieldDataMap after set = "
      call ESMF_FieldDataMapPrint(fieldDM, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOP
!\subsubsection{Getting Field DataMap Values}

! This example shows how to query an {\tt ESMF\_FieldDataMap}.
!EOP

!BOC
      call ESMF_FieldDataMapGet(fieldDM, dataRank, dataIndexList, &
                                horzRelloc=relativeLocation, rc=rc)
      print *, "Returned values from Field DataMap:"
      print *, "data rank: ", dataRank
      print *, "mapping of igrid to data indices: ", dataIndexList
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_FieldDataMapEx.F90"
      else
        print *, "FAIL: ESMF_FieldDataMapEx.F90"
      end if

!BOC
      end program ESMF_FieldDataMapEx
!EOC
