!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#undef LOCAL_NOT_IMPL 
program ESMF_LocStreamEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"


!  !PROGRAM: ESMF_LocStreamEx - LocStream examples.
!
!  !DESCRIPTION: 
!

      ! Use ESMF framework module
      use ESMF
      use ESMF_TestMod
      implicit none

      ! Parameters
      integer, parameter :: ESMF_Coord1=1, ESMF_Coord2=2, ESMF_Coord3=3
      ! Local variables  
      integer:: rc, finalrc
      type(ESMF_VM):: vm

      real(ESMF_KIND_R8), pointer :: lat(:), lon(:), temperature(:)
      type(ESMF_Field)            :: field_temperature
      type(ESMF_LocStream) :: locstream, newlocstream
      type(ESMF_Field) :: srcField,dstField
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Grid) :: grid
      integer :: localPet, petCount, numLocations, numLocationsOnThisPet
      integer :: i
      integer,parameter :: GridLatSize=20
      integer,parameter :: GridLonSize=20
      integer :: i1,i2
      real(ESMF_KIND_R8), pointer :: farrayPtrLonC(:,:)
      real(ESMF_KIND_R8), pointer :: farrayPtrLatC(:,:)
      real(ESMF_KIND_R8), pointer :: farrayPtr1D(:)
      real(ESMF_KIND_R8), pointer :: latArray(:)
      real(ESMF_KIND_R8), pointer :: lonArray(:)
      real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
      real(ESMF_KIND_R8) :: DEG2RAD,theta,phi
      real(ESMF_KIND_R8) :: x,y,z,expected
      integer :: clbnd(2),cubnd(2), result

      character(ESMF_MAXSTR) :: testname
      character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_LocStreamEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


      ! initialize ESMF
      finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm,  defaultlogfilename="LocStreamEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

#if 1
!BOE
!\subsubsection{Create a LocStream with user allocated memory}
!
! The following is an example of creating a LocStream object.
! After creation, key data is added, and a Field is created to hold data
! (temperature) at each location. 
!
!EOE

!BOC

   !-------------------------------------------------------------------
   ! Get parallel information. Here petCount is the total number of 
   ! running PETs, and localPet is the number of this particular PET.
   !-------------------------------------------------------------------
   call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC

   !-------------------------------------------------------------------
   ! Allocate and set example location information. Locations on a PET
   ! are wrapped around sphere. Each PET occupies a different latitude
   ! ranging from +50.0 to -50.0.
   !-------------------------------------------------------------------
   numLocations = 20
   allocate(lon(numLocations))
   allocate(lat(numLocations))

   do i=1,numLocations
      lon(i)=360.0*i/numLocations
      lat(i)=100*REAL(localPet,ESMF_KIND_R8)/REAL(petCount,ESMF_KIND_R8)-50.0
   enddo

   !-------------------------------------------------------------------
   ! Allocate and set example Field data
   !-------------------------------------------------------------------
   allocate(temperature(numLocations))

   do i=1,numLocations
      temperature(i)= 300 - abs(lat(i))
   enddo

   !-------------------------------------------------------------------
   ! Create the LocStream:  Allocate space for the LocStream object, 
   ! define the number and distribution of the locations. 
   !-------------------------------------------------------------------
   locstream=ESMF_LocStreamCreate(name="Temperature Measurements",   &
                                  localCount=numLocations, &
                                  coordSys=ESMF_COORDSYS_SPH_DEG,   &
                                  rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Add key data, referencing a user data pointer. By changing the 
   ! datacopyflag to ESMF_DATACOPY_VALUE an internally allocated copy of the 
   ! user data may also be set.  
   !-------------------------------------------------------------------
   call ESMF_LocStreamAddKey(locstream,              &
                             keyName="ESMF:Lat",     &
                             farray=lat,             &
                             datacopyflag=ESMF_DATACOPY_REFERENCE, &
                             keyUnits="Degrees",     &
                             keyLongName="Latitude", rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   call ESMF_LocStreamAddKey(locstream,              &
                             keyName="ESMF:Lon",     &
                             farray=lon,             &
                             datacopyflag=ESMF_DATACOPY_REFERENCE, &
                             keyUnits="Degrees",     &
                             keyLongName="Longitude", rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Create a Field on the Location Stream. In this case the 
   ! Field is created from a user array, but any of the other
   ! Field create methods (e.g. from ArraySpec) would also apply.
   !-------------------------------------------------------------------       
   field_temperature=ESMF_FieldCreate(locstream,   &
                                   temperature, &
                                   name="temperature", &
                                   rc=rc)


!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_FieldDestroy(field_temperature, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_LocStreamDestroy(locstream, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   deallocate(lon)
   deallocate(lat)
   deallocate(temperature)


!BOE
!\subsubsection{Create a LocStream with internally allocated memory}
!
! The following is an example of creating a LocStream object.
! After creation, key data is internally allocated,
! the pointer is retrieved, and the data is set.
! A Field is also created on the LocStream to hold data
! (temperature) at each location. 
!
!EOE

!BOC

   !-------------------------------------------------------------------
   ! Get parallel information. Here petCount is the total number of 
   ! running PETs, and localPet is the number of this particular PET.
   !-------------------------------------------------------------------
   call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!BOC
   numLocations = 20

   !-------------------------------------------------------------------
   ! Create the LocStream:  Allocate space for the LocStream object, 
   ! define the number and distribution of the locations. 
   !-------------------------------------------------------------------
   locstream=ESMF_LocStreamCreate(name="Temperature Measurements",   &
                                  localCount=numLocations, &
                                  coordSys=ESMF_COORDSYS_SPH_DEG,   &
                                  rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


   !-------------------------------------------------------------------
   ! Add key data (internally allocating memory).
   !-------------------------------------------------------------------
   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="ESMF:Lat",           &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="Degrees",           &
                             keyLongName="Latitude", rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="ESMF:Lon",           &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="Degrees",           &
                             keyLongName="Longitude", rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Get key data. 
   !-------------------------------------------------------------------
   call ESMF_LocStreamGetKey(locstream,                    &
                             keyName="ESMF:Lat",           &
                             farray=lat,                   &
                             rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   call ESMF_LocStreamGetKey(locstream,                    &
                             keyName="ESMF:Lon",           &
                             farray=lon,                   &
                             rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Set example location information. Locations on a PET are wrapped 
   ! around sphere. Each PET occupies a different latitude ranging 
   ! from +50.0 to -50.0.
   !-------------------------------------------------------------------
   do i=1,numLocations
      lon(i)=360.0*i/numLocations
      lat(i)=100*REAL(localPet,ESMF_KIND_R8)/REAL(petCount,ESMF_KIND_R8)-50.0
   enddo


   !-------------------------------------------------------------------
   ! Allocate and set example Field data
   !-------------------------------------------------------------------
   allocate(temperature(numLocations))
   do i=1,numLocations
      temperature(i)= 300 - abs(lat(i))
   enddo

   !-------------------------------------------------------------------
   ! Create a Field on the Location Stream. In this case the 
   ! Field is created from a user array, but any of the other
   ! Field create methods (e.g. from ArraySpec) would also apply.
   !-------------------------------------------------------------------    
   field_temperature=ESMF_FieldCreate(locstream,   &
                                 temperature, &
                                 name="temperature", &
                                 rc=rc)


!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_FieldDestroy(field_temperature, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_LocStreamDestroy(locstream, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   deallocate(temperature)




!BOE
!\subsubsection{Create a LocStream with a distribution based on a Grid}
!
! The following is an example of using the LocStream create from background
! Grid capability. Using this capability, the newly created LocStream 
! is a copy of the old LocStream, but with a new distribution. The new LocStream 
! is distributed such that if the coordinates of a location in the LocStream lie 
! within a Grid cell, then that location is put on the same PET as the Grid cell. 
!
!EOE

!BOC

   !-------------------------------------------------------------------
   ! Get parallel information. Here petCount is the total number of 
   ! running PETs, and localPet is the number of this particular PET.
   !-------------------------------------------------------------------
   call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!BOC
   !-------------------------------------------------------------------
   ! Create the LocStream:  Allocate space for the LocStream object, 
   ! define the number and distribution of the locations. 
   !-------------------------------------------------------------------
   numLocations = 20
   locstream=ESMF_LocStreamCreate(name="Temperature Measurements",   &
                                  localCount=numLocations, &
                                  coordSys=ESMF_COORDSYS_SPH_DEG,   &
                                  rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Add key data (internally allocating memory).
   !-------------------------------------------------------------------
   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="ESMF:Lon",           &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="Degrees",           &
                             keyLongName="Longitude", rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="ESMF:Lat",           &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="Degrees",           &
                             keyLongName="Latitude", rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


   !-------------------------------------------------------------------
   ! Get Fortran arrays which hold the key data, so that it can be set. 
   !-------------------------------------------------------------------
   call ESMF_LocStreamGetKey(locstream,                    &
                             keyName="ESMF:Lon",           &
                             farray=lon,                   &
                             rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   call ESMF_LocStreamGetKey(locstream,                    &
                             keyName="ESMF:Lat",           &
                             farray=lat,                   &
                             rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Set example location information. Locations on a PET are wrapped 
   ! around sphere. Each PET occupies a different latitude ranging 
   ! from +50.0 to -50.0.
   !-------------------------------------------------------------------
   do i=1,numLocations
      lon(i)=360.0*i/numLocations
      lat(i)=100*REAL(localPet,ESMF_KIND_R8)/REAL(petCount,ESMF_KIND_R8)-50.0
   enddo

   !-------------------------------------------------------------------
   ! Create a Grid to use as the background. The Grid is 
   ! GridLonSize by GridLatSize with the default distribution 
   ! (The first dimension split across the PETs). The coordinate range
   ! is  0 to 360 in longitude and -90 to 90 in latitude. Note that we 
   ! use indexflag=ESMF_INDEX_GLOBAL for the Grid creation. At this time 
   ! this is required for a Grid to be usable as a background Grid.
   ! Note that here the points are treated as cartesian.
   !-------------------------------------------------------------------
   grid=ESMF_GridCreateNoPeriDim(maxIndex=(/GridLonSize,GridLatSize/), &
                                 coordSys=ESMF_COORDSYS_SPH_DEG,       &
                                 indexflag=ESMF_INDEX_GLOBAL,          &
                                 rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Allocate the corner stagger location in which to put the coordinates. 
   ! (The corner stagger must be used for the Grid to be usable as a
   !  background Grid.)
   !-------------------------------------------------------------------
   call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Get access to the Fortran array pointers that hold the Grid 
   ! coordinate information and then set the coordinates to be uniformly 
   ! distributed around the globe. 
   !-------------------------------------------------------------------
   call ESMF_GridGetCoord(grid,                                  &
                          staggerLoc=ESMF_STAGGERLOC_CORNER,     &
                          coordDim=1, computationalLBound=clbnd, &
                          computationalUBound=cubnd,             & 
                          farrayPtr=farrayPtrLonC, rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


   call ESMF_GridGetCoord(grid,                                  &
                         staggerLoc=ESMF_STAGGERLOC_CORNER,      &
                          coordDim=2, farrayPtr=farrayPtrLatC, rc=rc)

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   do i1=clbnd(1),cubnd(1)
   do i2=clbnd(2),cubnd(2)
      ! Set Grid longitude coordinates as 0 to 360
      farrayPtrLonC(i1,i2) = REAL(i1-1)*360.0/REAL(GridLonSize)

      ! Set Grid latitude coordinates as -90 to 90
      farrayPtrLatC(i1,i2) = -90. + REAL(i2-1)*180.0/REAL(GridLatSize) + &
                                      0.5*180.0/REAL(GridLatSize)
   enddo
   enddo


   !-------------------------------------------------------------------
   ! Create newLocstream on the background Grid using the 
   ! "Lon" and "Lat" keys as the coordinates for the entries in 
   ! locstream. The entries in newLocstream with coordinates (lon,lat)
   ! are on the same PET as the piece of grid which contains (lon,lat). 
   !-------------------------------------------------------------------
   newLocstream=ESMF_LocStreamCreate(locstream, &
                  background=grid, rc=rc)


   !-------------------------------------------------------------------
   ! A Field can now be created on newLocstream and 
   ! ESMF_FieldRedist() can be used to move data between Fields built 
   ! on locstream and Fields built on newLocstream.
   !-------------------------------------------------------------------
!EOC


!BOE
!\subsubsection{Regridding from a Grid to a LocStream}
!
! The following is an example of how a LocStream object can be used in regridding.
!
!EOE

!BOC
   !-------------------------------------------------------------------
   ! Create a global Grid to use as the regridding source. The Grid is 
   ! GridLonSize by GridLatSize with the default distribution 
   ! (The first dimension split across the PETs). The coordinate range
   ! is  0 to 360 in longitude and -90 to 90 in latitude. Note that we 
   ! use indexflag=ESMF_INDEX_GLOBAL for the Grid creation to calculate
   ! coordinates across PETs.
   !-------------------------------------------------------------------
   grid=ESMF_GridCreate1PeriDim(maxIndex=(/GridLonSize,GridLatSize/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG,       &
                                indexflag=ESMF_INDEX_GLOBAL,          &
                                rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
   !-------------------------------------------------------------------
   ! Allocate the center stagger location in which to put the coordinates. 
   !-------------------------------------------------------------------
   call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
   !-------------------------------------------------------------------
   ! Get access to the Fortran array pointers that hold the Grid 
   ! coordinate information.
   !------------------------------------------------------------------- 
   ! Longitudes 
   call ESMF_GridGetCoord(grid,                                  &
                          staggerLoc=ESMF_STAGGERLOC_CENTER,     &
                          coordDim=1, computationalLBound=clbnd, &
                          computationalUBound=cubnd,             &
                          farrayPtr=farrayPtrLonC, rc=rc)
!EOC
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
   ! Latitudes
   call ESMF_GridGetCoord(grid,                                  &
                          staggerLoc=ESMF_STAGGERLOC_CENTER,     &
                          coordDim=2, computationalLBound=clbnd, &
                          computationalUBound=cubnd,             &
                          farrayPtr=farrayPtrLatC, rc=rc)
!EOC
     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Create a source Field to hold the data to be regridded to the 
   ! destination
   !-------------------------------------------------------------------
   srcField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               name="source", rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   !-------------------------------------------------------------------
   ! Get access to the Fortran array pointers that hold the Field data.
   !-------------------------------------------------------------------
   call ESMF_FieldGet(srcField, 0, farrayPtr, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   DEG2RAD = 3.14159265/180.0
!BOC
   !-------------------------------------------------------------------
   ! Set the Grid coordinates to be uniformly distributed around the globe. 
   !-------------------------------------------------------------------
   do i1=clbnd(1),cubnd(1)
   do i2=clbnd(2),cubnd(2)
      ! Set Grid longitude coordinates as 0 to 360
      farrayPtrLonC(i1,i2) = REAL(i1-1)*360.0/REAL(GridLonSize)

      ! Set Grid latitude coordinates as -90 to 90
      farrayPtrLatC(i1,i2) = -90. + REAL(i2-1)*180.0/REAL(GridLatSize) + &
                                       0.5*180.0/REAL(GridLatSize)
!EOC
      ! Arbitrarily set the source data to be a function of the x,y,z coordinates
      ! (something relatively smooth, that varies everywhere)
      theta = DEG2RAD*(farrayPtrLonC(i1,i2))
      phi = DEG2RAD*(90.-farrayPtrLatC(i1,i2))
      x = cos(theta)*sin(phi)
      y = sin(theta)*sin(phi)
      z = cos(phi)

      
      farrayPtr(i1,i2) = x+y+z+15.0
!BOC
   enddo
   enddo

   !-------------------------------------------------------------------
   ! Set the number of points the destination LocStream will have
   ! depending on the PET. 
   !-------------------------------------------------------------------
   if (petCount .eq. 1) then
     numLocationsOnThisPet=7
   else
     if (localpet .eq. 0) then
       numLocationsOnThisPet=2
     else if (localpet .eq. 1) then
       numLocationsOnThisPet=2
     else if (localpet .eq. 2) then
       numLocationsOnThisPet=2
     else if (localpet .eq. 3) then
       numLocationsOnThisPet=1
     endif
   endif

   !-------------------------------------------------------------------
   ! Create the LocStream:  Allocate space for the LocStream object,
   ! define the number of locations on this PET. 
   !-------------------------------------------------------------------
   locstream=ESMF_LocStreamCreate(name="Test Data",                 &
                                  localCount=numLocationsOnThisPet, &
                                  coordSys=ESMF_COORDSYS_SPH_DEG,   &
                                  rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
   !-------------------------------------------------------------------
   ! Add key data to LocStream(internally allocating memory).
   !-------------------------------------------------------------------
   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="ESMF:Lat",           &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="degrees",           &
                             keyLongName="Latitude", rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="ESMF:Lon",           &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="degrees",           &
                             keyLongName="Longitude", rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
   !-------------------------------------------------------------------
   ! Get access to the Fortran array pointers that hold the key data.
   !-------------------------------------------------------------------
   ! Longitudes
   call ESMF_LocStreamGetKey(locstream,           &
                             keyName="ESMF:Lon",  &
                             farray=lonArray,     &
                             rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
   ! Latitudes
   call ESMF_LocStreamGetKey(locstream,           &
                             keyName="ESMF:Lat",  &
                             farray=latArray,     &
                             rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

   !-------------------------------------------------------------------
   ! Set coordinates in key arrays depending on the PET.
   ! For this example use an arbitrary set of points around globe.  
   !-------------------------------------------------------------------
   if (petCount .eq. 1) then
     latArray = (/-87.75, -56.25, -26.5, 0.0, 26.5, 56.25, 87.75 /)
     lonArray = (/51.4, 102.8, 154.2, 205.6, 257.0, 308.4, 359.8 /)
   else
     if (localpet .eq. 0) then
       latArray = (/ -87.75, -56.25 /)
       lonArray = (/ 51.4, 102.8 /)
     else if (localpet .eq.1) then
       latArray = (/ -26.5, 0.0 /)
       lonArray = (/ 154.2, 205.6 /)
     else if (localpet .eq.2) then
       latArray = (/ 26.5, 56.25 /)
       lonArray = (/ 257.0, 308.4 /)
     else if (localpet .eq.3) then
       latArray = (/ 87.75 /)
       lonArray = (/ 359.8 /)
     endif
   endif

   !-------------------------------------------------------------------
   ! Create the destination Field on the LocStream to hold the 
   ! result of the regridding. 
   !-------------------------------------------------------------------
   dstField = ESMF_FieldCreate(locstream, typekind=ESMF_TYPEKIND_R8, &
                               name="dest", rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   !-------------------------------------------------------------------
   ! Clear the destination Field.
   !-------------------------------------------------------------------
   call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   farrayPtr1D=0.0
!BOC

   !-------------------------------------------------------------------
   ! Calculate the RouteHandle that represents the regridding from 
   ! the source to destination Field using the Bilinear regridding method.
   !-------------------------------------------------------------------
   call ESMF_FieldRegridStore( srcField=srcField,                       &
                               dstField=dstField,                       &
                               routeHandle=routeHandle,                 &
                               regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                               rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


   !-------------------------------------------------------------------
   ! Regrid from srcField to dstField
   !-------------------------------------------------------------------
   ! Can loop here regridding from srcField to dstField as src data changes
   ! do i=1,...

        ! (Put data into srcField)

        !-------------------------------------------------------------------
        ! Use the RouteHandle to regrid data from srcField to dstField.
        !-------------------------------------------------------------------
        call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=rc)

        ! (Can now use the data in dstField)

   ! enddo

!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
   !-------------------------------------------------------------------
   ! Now that we are done, release the RouteHandle freeing its memory. 
   !-------------------------------------------------------------------
   call ESMF_FieldRegridRelease(routeHandle, rc=rc)
!EOC
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   !-------------------------------------------------------------------
   ! loop through nodes and make sure interpolated values are reasonable
   !-------------------------------------------------------------------
   do i1=1,numLocationsOnThisPet
     ! get the x,y,z coordinates
     theta = DEG2RAD*lonArray(i1)
     phi = DEG2RAD*(90.-latArray(i1))
     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)

     ! determine validation data
     expected = x+y+z+15.0

     ! if error is too big report an error
     if ( abs( farrayPtr1D(i1)-(expected) )/expected > 0.01) then
       print*,'ERROR: larger than expected difference, expected ',expected, &
              '  got ',farrayPtr1D(i1),'  diff= ',abs(farrayPtr1D(i1)-expected), &
              '  rel diff= ',abs(farrayPtr1D(i1)-expected)/expected
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
     endif
   enddo

   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_FieldDestroy(srcField, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_FieldDestroy(dstField, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_LocStreamDestroy(locstream, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_LocStreamDestroy(newLocstream, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_GridDestroy(grid, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#endif

   !-------------------------------------------------------------------
   ! Shut down and end.
   !-------------------------------------------------------------------
10 continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_LocStreamEx.F90"
  else
    print *, "FAIL: ESMF_LocStreamEx.F90"
  endif
  
end program
