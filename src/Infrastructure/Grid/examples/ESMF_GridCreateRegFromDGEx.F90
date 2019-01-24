! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_GridCreateEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"

!BOE
! \subsubsection{Create a 2D Grid with regular distribution from a DistGrid}~\label{sec:usage:ex:adv:reg}
!
! This example illustrates the creation of a single tile 2D Grid
! with a regular distribution from a DistGrid.  The size of the Grid is
! gridSize(1) by gridSize(2) elements. It only contains data
! in the center stagger location (i.e. Arakawa A-Grid).
!EOE


!  !PROGRAM: ESMF_GridCreateEx - Examples of Grid creation.
!
!  !DESCRIPTION: 
!
! This program shows examples of Grid creation


!BOC
      ! Use ESMF framework module
      use ESMF
      use ESMF_TestMod
      implicit none

      ! Local variables  
      integer:: rc, finalrc, result
      type(ESMF_VM):: vm
      type(ESMF_DistGrid) :: distgrid2D
      type(ESMF_Grid) :: grid2D
!EOC         
      character(ESMF_MAXSTR) :: testname
      character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_GridCreateRegFromDGEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, defaultlogfilename="GridCreateRegFromDGEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! First construct a single tile distgrid with regular distribution of the
! appropriate size.
!EOE
!BOC

      distgrid2D = ESMF_DistGridCreate(minIndex=(/1,1/),      &
                          maxIndex=(/20,30/), rc=rc)  
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!    Create a Grid using the distgrid. 
!EOE

!BOC 
     Grid2D=ESMF_GridCreate(name="Simple 2D Regular", &
               distgrid=distgrid2D, rc=rc)
!EOC  

     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Set the one stagger location as center. 
!EOE

!BOC
   call ESMF_GridAddCoord(Grid2D,  &
          staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ! Get rid of memory
   call ESMF_GridDestroy(Grid2D, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   ! Get rid of memory
   call ESMF_DistgridDestroy(distgrid2D, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_GridCreateRegFromDGEx.F90"
  else
    print *, "FAIL: ESMF_GridCreateRegFromDGEx.F90"
  endif
  
end program
