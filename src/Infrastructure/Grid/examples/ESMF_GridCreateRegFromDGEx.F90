
! $Id: ESMF_GridCreateRegFromDGEx.F90,v 1.6.2.3 2009/01/21 21:25:21 cdeluca Exp $
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

program ESMF_GridCreateEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Create 2D Grid with Regular Distribution from a DistGrid}~\label{sec:usage:ex:adv:reg}
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
      use ESMF_Mod
      implicit none

      ! Local variables  
      integer:: rc, finalrc
      type(ESMF_VM):: vm
      type(ESMF_DistGrid) :: distgrid2D
      type(ESMF_Grid) :: grid2D
!EOC         

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)

!BOE
! First construct a single tile distgrid with regular distribution of the
! appropriate size.
!EOE
!BOC

      distgrid2D = ESMF_DistGridCreate(minIndex=(/1,1/),      &
                          maxIndex=(/20,30/), rc=rc)  
!EOC

!BOE
!    Create a Grid using the distgrid. 
!EOE

!BOC 
     Grid2D=ESMF_GridCreate(name="Simple 2D Regular", &
               distgrid=distgrid2D, rc=rc)
!EOC  

!BOE
! Set the one stagger location as center. 
!EOE

!BOC
   call ESMF_GridAddCoord(Grid2D,  &
          staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
!EOC


10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_GridCreateRegFromDGEx.F90"
  else
    print *, "FAIL: ESMF_GridCreateRegFromDGEx.F90"
  endif
  
end program
