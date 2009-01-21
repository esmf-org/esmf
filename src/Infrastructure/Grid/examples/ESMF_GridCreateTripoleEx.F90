! $Id: ESMF_GridCreateTripoleEx.F90,v 1.1.2.2 2009/01/21 21:25:21 cdeluca Exp $
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
!ESMF_EX_NOTWORKING_AMPLE        String used by test script to count examples.
!==============================================================================

!  !PROGRAM: ESMF_GridCreateEx - Examples of Grid creation.
!
!  !DESCRIPTION: 
!
! This program shows examples of Grid creation

!BOE
! \subsubsection{Example: 2D Tripole Grid Creation From Arrays}\label{sec:usage:ex:adv:tripole}
!
! This example illustrates the creation of a 2D tripole Grid from coordinate data
! read in on a single processor and then distributed to the rest of the processors. 
! The Grid contains just the center stagger location. The size of the Grid is gridSize(1) by 
! gridSize(2).
!EOE


!BOC
      ! Use ESMF framework module
      use ESMF_Mod
      implicit none

      ! Local variables  
      integer:: rc, finalrc
      integer:: myPet, npets, rootPet
      type(ESMF_VM):: vm
      type(ESMF_Config) :: config
      type(ESMF_DistGrid) :: distgrid
      type(ESMF_Array) :: gridCoordArrays(2,1)
      type(ESMF_Array) :: gridCntrCoordArrayLat,gridCntrCoordArrayLon
      type(ESMF_StaggerLoc) :: staggerLocs(1)
      type(ESMF_ArraySpec) ::  arrayspec
      real(ESMF_KIND_R8), allocatable :: globalGridCntrCoordLat(:,:)
      real(ESMF_KIND_R8), allocatable :: globalGridCntrCoordLon(:,:)
      integer :: gridSize(2), gridRank
      integer, allocatable ::  connectionList(:,:)
!EOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet=myPet, petCount=npets, rc=rc)

      rootPet = zero


!BOE
! Allocate a fortran array to hold sphere coodinates, then read them in. This 
! all takes place on one processor. Later the data will be distributed across the processors. 
!EOE

!BOC
      gridRank=2  ! 2D grid
      call read2Dgriddata(gridSize)
      allocate( globalGridCntrCoordLat(gridSize(1),gridSize(2)))
      allocate( globalGridCntrCoordLon(gridSize(1),gridSize(2)))
      call read2Dgrid(globalGridCntrCoordLat)
      call read2Dgrid(globalGridCntrCoordLon)
!EOC
      
!BOE    
! Construct a single patch tripole domain. 
! Specifiy that the first dimension is periodic: \\
!
! \begin{itemize}
! \item Setting patchIndexA=patchIndexB indicates that the connection 
!      is within the patch.
! \item The position vector is set to span the width of the patch's 
!      first dimension.
! \item The repetitionVector indicates that the connection repeats along
!           the dimension. This takes care of both sides of the connection.
! \end{itemize}
!EOE

!BOC

      allocate( connectionList(2*gridRank,3) )
      call ESMF_ConnectionElementConstruct(                          &
                 connectionElement=connectionList(:,1),     &
                 patchIndexA=1, patchIndexB=1,              &
                 positionVector = (/gridSize(1),0/),        &
                 repetitionVector= (/1,0/), rc=rc)
!EOC

!BOE    
! Specifiy the northern bipolar fold: \\
!
!  The position and orientation vectors indicate that each element 
!  along the top edge of the patch is attached to the corresponding
!  element across the fold. 
!EOE

!BOC
       call ESMF_ConnectionElementConstruct(
                  connectionElement = connectionList(:,2), &
                  patchIndexA = 1, patchIndexB = 1, &
                  positionVector = (/gridSize(1)+1, 2*gridSize(2)/), &
                  orientationVector = (/-1, -2/), &
                  rc=rc)
!EOC

!BOE    
! Specifiy the south pole: \\
!
!  The position and orientation vectors indicate that each element along
!   the bottom edge of the patch is attached to the element directly across the pole. 
!EOE

!BOC
       call ESMF_ConnectionElementConstruct( 
                 connectionElement = connectionList(:,3), &
                 patchIndexA = 1, patchIndexB = 1, &
                 positionVector = (/gridSize(1)/2, 0/), &
                 orientationVector = (/1, -2/), &
                 repetitionVector = (/1, 0/), &
                 rc=rc)
!EOC

!BOE    
!  Construct distgrid from connection information.
!EOE

!BOC
      distgrid = ESMF_DistGridCreate( minCorner=(/1,1/),  &
                          maxCorner=(/gridSize(1),gridSize(2)/),    &
                          connectionList=connectionList, rc=rc)  

      deallocate( connectionList )
!EOC

!BOE
!  Create an array into which to put the spherical coordinates.

!EOE
!BOC
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8,  &
                 rank=gridRank)

      gridCntrCoordArrayLat = ESMF_ArrayCreate(arrayspec=arrayspec,  &
                                                 distgrid=distgrid, rc=rc)
      gridCntrCoordArrayLon = ESMF_ArrayCreate(arrayspec=arrayspec, &
                                                 distgrid=distgrid, rc=rc)
!EOC

!BOE
!  Scatter the fortran array according to DistGrid into the esmf Array.
!EOE

!BOC
      call ESMF_ArrayScatter(gridCntrCoordArrayLat, globalGridCntrCoordLat, &
                rootPet=rootPet, vm=vm, rc=rc)    

      call ESMF_ArrayScatter(gridCntrCoordArrayLon, globalGridCntrCoordLon, &
                rootPet=rootPet, vm=vm, rc=rc)    
!EOC

!BOE
! Load Stagger location and corresponding coordinate arrays into array of ESMF Arrays.
!EOE

!BOC
     staggerlocs(1)=ESMF_STAGGERLOC_CENTER
     gridCoordArrays(1,1)=gridCntrCoordArrayLon     
     gridCoordArrays(2,1)=gridCntrCoordArrayLat     
!EOC

!BOE
!    Create a Grid from the coordinate array. 
!EOE

!BOC 
     tripoleGrid = ESMF_GridCreate(arrays=gridCoordArrays, &
                               staggerLocs=staggerlocs,rc=rc)
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
