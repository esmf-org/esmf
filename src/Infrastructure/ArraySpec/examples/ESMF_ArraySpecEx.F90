! $Id: ESMF_ArraySpecEx.F90,v 1.21.2.1 2012/01/06 20:42:25 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_ArraySpecEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE	String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_ArraySpecEx - ArraySpec manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of ArraySpec set and get usage
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF
      implicit none

      ! local variables 
      type(ESMF_ArraySpec) :: arrayDS
      integer :: myrank
      type(ESMF_TypeKind_Flag) :: mytypekind


      ! return code
      integer:: rc
!EOC

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(defaultlogfilename="ArraySpecEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOP
!\subsubsection{Set ArraySpec values}

! This example shows how to set values in an {\tt ESMF\_ArraySpec}.
!EOP

!BOC
      call ESMF_ArraySpecSet(arrayDS, rank=2, &
                             typekind=ESMF_TYPEKIND_R8, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOP
!\subsubsection{Get ArraySpec values}

! This example shows how to query an {\tt ESMF\_ArraySpec}.
!EOP

!BOC
      call ESMF_ArraySpecGet(arrayDS, rank=myrank, &
        typekind=mytypekind, rc=rc)
      print *, "Returned values from ArraySpec:"
      print *, "rank =", myrank
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_ArraySpecEx.F90"
      else
        print *, "FAIL: ESMF_ArraySpecEx.F90"
      end if

!BOC
      end program ESMF_ArraySpecEx
!EOC
