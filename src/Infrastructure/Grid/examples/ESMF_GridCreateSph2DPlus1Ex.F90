! $Id: ESMF_GridCreateSph2DPlus1Ex.F90,v 1.1.2.2 2009/01/21 21:25:21 cdeluca Exp $
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
! \subsubsection{Example: 2D+1 Spherical Grid Creation}
!
! This example  illustrates the creation of a 2D spherical Grid 
! with a 3rd undistributed dimension.  The Grid contains both the center stagger location and a corner
! (i.e. Arakawa B-Grid). The size of the 2D horizontal Grid is gridSize(1) by gridSize(2). The number of
! vertical levels is gridSizeVert.
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
      type(ESMF_Array) :: gridCoordArrays(3,2)
      type(ESMF_Array) :: gridCntrCoordArrayLat,gridCntrCoordArrayLon
      type(ESMF_Array) :: gridNECnrCoordArrayLat,gridNECnrCoordArrayLon
      type(ESMF_Array) :: cntArrayData1
      type(ESMF_StaggerLoc) :: staggerLocs(2), neCenterStaggerLoc
      type(ESMF_ArraySpec) ::  arrayspec1D,arrayspec2D,arrayspec3D
      integer :: gridSize(2), gridVert_size
      integer, allocatable ::  connectionList(:,:)
!EOC         

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)

!BOE    
! Construct a single patch spherical domain without connection across
! the poles.
!EOE

!BOC

      allocate( connectionList(2*2,2) )
      call ESMF_ConnectionElementConstruct(                          &
                          connectionElement=connectionList(:,1),     &
                          patchIndexA=1, patchIndexB=1,              &
                          positionVector = (/gridSize(1),0/),        &
                          repetitionVector= (/1,0/), rc=rc)

      distgrid = ESMF_DistGridCreate( minCorner=(/1,1/),             &
                          maxCorner=(/gridSize(1),gridSize(2)/),     &
                          connectionList=connectionList, rc=rc)  

      deallocate( connectionList )

!EOC

!BOE
!  Create arrays into which to put the 2D spherical coordinates. Create one array for each stagger location. 
!EOE
!BOC
      call ESMF_ArraySpecSet(arrayspec2D, type=ESMF_DATA_REAL,         &
                 kind=ESMF_R8, rank=2)

      gridCntrCoordArrayLon = ESMF_ArrayCreate(arrayspec=arrayspec2D, &
                                                   distgrid=distgrid, rc=rc)
      gridCntrCoordArrayLat = ESMF_ArrayCreate(arrayspec=arrayspec2D, &
                                                  distgrid=distgrid, rc=rc)
      gridNECnrCoordArrayLon = ESMF_ArrayCreate(arrayspec=arrayspec2D,&
                                                      distgrid=distgrid, rc=rc)
      gridNECnrCoordArrayLat = ESMF_ArrayCreate(arrayspec=arrayspec2D, &
                                                     distgrid=distgrid, rc=rc)
!EOC

!BOE
!  Create array into which to put the vertical coordinate.
! Note that this array creation subroutine is currently not implemented. What it would do is
! create an undistributed array, but make a copy on each DE. 
!EOE
!BOC
      call ESMF_ArraySpecSet(arrayspec1D, type=ESMF_DATA_REAL,         &
                 kind=ESMF_R8, rank=1)

      gridCoordArrayVert = ESMF_ArrayCreate(arrayspec=arrayspec1D, &
                                             distgrid=distgrid, dimmap=(/0,0/), &
                                             lbound=(/1/), ubound=(/gridSizeVert/), &
                                             rc=rc) 
!EOC

!BOE
! Now that the arrays are created we could set the coordinates in them.
! (The following are user subroutines.)
!EOE
!BOC
!  call SetHorzCoords(gridCntrCoordArrayLon,gridCntrCoordArrayLat, &
!           gridNECnrCoordArrayLon,gridNECnrCoordArrayLat)
!  call SetVertCoords(gridrCoordArrayVert)

!EOC

!BOE
!  Create stagger location corresponding to the center of the north east corner
!  edge. (i.e. the same vertical level as the 3D center, but in the northeast corner).
!EOE
!BOC

      call ESMF_StaggerLocSet(neCenterStaggerLoc,where=(/1,1,0/),rc=rc)

!EOC

!BOE
! Load Stagger location and corresponding coordinate arrays into array of ESMF Arrays.
!
!EOE

!BOC
      ! Setup center stagger.
     staggerlocs(1)=ESMF_STAGGERLOC_CENTER
     gridCoordArrays(1,1)=gridCntrCoordArrayLon     
     gridCoordArrays(2,1)=gridCntrCoordArrayLat     
     gridCoordArrays(3,1)=gridCoordArrayVert     

      ! Setup corner stagger.
     staggerlocs(2)=neCenterStaggerLoc
     gridCoordArrays(1,2)=gridNECnrCoordArrayLon     
     gridCoordArrays(2,2)=gridNECnrCoordArrayLat     
     gridCoordArrays(3,2)=gridCoordArrayVert     

!EOC

!BOE
!    Create a Grid from the coordinate arrays. 
!EOE

!BOC 
     sphericalGrid = ESMF_GridCreate(arrays=gridCoordArrays, &
                                  staggerLocs=staggerlocs,rc=rc)
!EOC  

!BOE
!  Create a 3D array cooresponding to the center stagger into which to put data.
!EOE
!BOC
      call ESMF_ArraySpecSet(arrayspec3D, type=ESMF_DATA_REAL,         &
                 kind=ESMF_R8, rank=3)

      cntrArrayData1 = ESMF_ArrayCreate(arrayspec=arrayspec3D,  &
                                        distgrid=distgrid, &
                                        lbounds=(/1/),ubounds=(/gridSizeVert/), &
                                        rc=rc)
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
