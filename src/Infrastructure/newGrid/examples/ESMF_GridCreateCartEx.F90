! $Id: ESMF_GridCreateCartEx.F90,v 1.2 2007/05/14 17:40:28 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_GridCreateEx

!==============================================================================
!EX_NOTWORKING_AMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: 2D Cartesian Grid Creation and Access}~\label{sec:example1}
!
! This example illustrates the creation of a simple 2D Cartesian Grid.  
! The size of the Grid is gridSize(1) by gridSize(2) elements. 
!  It only contains data in the center stagger location (i.e. Its an Arakawa A-Grid).
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
      type(ESMF_StaggerLoc) :: staggerLocs(1)
      type(ESMF_ArraySpec) ::  arrayspec2D
      real(ESMF_KIND_R8), pointer :: coordsX(:,:),coordsY(:,:)
      integer :: gridSize(2)
!EOC         

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)

!BOE
! First construct a single patch Cartesian distgrid of the appropriate size.
!EOE
!BOC

      distgrid2D = ESMF_DistGridCreate( minCorner=(/1,1/),      &
                          maxCorner=(/gridSize(1),gridSize(2)/),     &
                           rc=rc)  
!EOC

!BOE
! Next use array spec. to specify the dimension of the Grid and
! the type and kind of the coordinate data. 
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec2D, type=ESMF_DATA_REAL,         &
         kind=ESMF_R8, rank=2)
!EOC

!BOE
! Set the one stagger location as center. 
!EOE
!BOC
     staggerLocs(1)=ESMF_STAGGERLOC_2D_CENTER
!EOC

!BOE
!    Create a Grid using the distgrid, arrayspec, and the stagger array. 
!EOE

!BOC 
      Grid2D=ESMF_GridCreate(name="Simple 2D Cartesian",arrayspec=arrayspec2D, &
               distgrid=distgrid2D, staggerLocs, rc=rc)
!EOC  


!BOE
!    Get the f90 pointer to the coordinate data and the array bounds of this processor's piece,
!    then set the coordinates.
!EOE

!BOC 
     call ESMF_GridLocalTileGetData(Grid2D,ESMF_STAGGERLOC_2D_CENTER,comp=1,&
            coordsX,rc=rc)
     call ESMF_GridLocalTileGetData(Grid2D,ESMF_STAGGERLOC_2D_CENTER,comp=2,&
            coordsY,rc=rc)

     call ESMF_GridLocalTileGet(Grid2D,ESMF_STAGGERLOC_2D_CENTER, &
            exclusiveLBound=lbnd, exclusiveUBound=ubnd,rc=rc)

     do j=lbnd(2),ubnd(2)
     do i=lbnd(1),ubnd(1)
         coordsX(i,j) = CalcXCoord(i,j)
         coordsY(i,j) = CalcYCoord(i,j)
     enddo
     enddo

!EOC  


10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_GridEx.F90"
  else
    print *, "FAIL: ESMF_GridEx.F90"
  endif
  
end program
