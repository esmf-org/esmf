!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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


!  !PROGRAM: ESMF_LocStreamEx - LocStream examples.
!
!  !DESCRIPTION: 
!

      ! Use ESMF framework module
      use ESMF_Mod
      implicit none

      ! Parameters
      integer, parameter :: ESMF_Coord1=1, ESMF_Coord2=2, ESMF_Coord3=3
      ! Local variables  
      integer:: rc, finalrc
      type(ESMF_VM):: vm

      real(ESMF_KIND_R8), pointer :: lat(:), lon(:), temperature(:)
      type(ESMF_Field)            :: field_temperature
      type(ESMF_Grid) :: grid 
      type(ESMF_LocStream) :: locstream, newlocstream
      integer :: localPet, petCount
      integer,parameter :: numLocationsOnThisPet=20
      integer,parameter :: GridLatSize=20
      integer,parameter :: GridLonSize=20
      integer :: i,i1,i2
      real(ESMF_KIND_R8), pointer :: fptrLonC(:,:)
      real(ESMF_KIND_R8), pointer :: fptrLatC(:,:)
      integer :: clbnd(2),cubnd(2)

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm,  rc=rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

#if 1
!BOE
!\subsubsection{Creating a LocStream Employing User Allocated Memory}
!
! The following is an example of creating a LocStream object.
! After creation, key data is added, and a Field is created to hold data
! (temperature) at each location. 
!
!EOE


!BOC
   !-------------------------------------------------------------------
   ! Allocate and set example location information
   !-------------------------------------------------------------------
   allocate(lon(numLocationsOnThisPet))
   allocate(lat(numLocationsOnThisPet))

   do i=1,numLocationsOnThisPet
      lon(i)=360.0/numLocationsOnThisPet
      lat(i)=0.0
   enddo

   !-------------------------------------------------------------------
   ! Allocate and set example Field data
   !-------------------------------------------------------------------
   allocate(temperature(numLocationsOnThisPet))

   do i=1,numLocationsOnThisPet
      temperature(i)=90.0
   enddo


   !-------------------------------------------------------------------
   ! Create the LocStream:  Allocate space for the LocStream object, 
   ! define the number and distribution of the locations. 
   !-------------------------------------------------------------------
   locstream=ESMF_LocStreamCreate(name="Equatorial Measurements",   &
                                  localCount=numLocationsOnThisPet, &
                                  rc=rc)

   !-------------------------------------------------------------------
   ! Add key data, referencing a user data pointer. By changing the 
   ! copyFlag to ESMF_DATA_COPY an internally allocated copy of the 
   ! user data may also be set.  
   !-------------------------------------------------------------------
   call ESMF_LocStreamAddKey(locstream,              &
                             keyName="Lat",          &
                             farray=lat,             &
                             copyFlag=ESMF_DATA_REF, &
                             keyUnits="Degrees",     &
                             keyLongName="Latitude", rc=rc)

   call ESMF_LocStreamAddKey(locstream,              &
                             keyName="Lon",          &
                             farray=lon,             &
                             copyFlag=ESMF_DATA_REF, &
                             keyUnits="Degrees",     &
                             keyLongName="Longitude", rc=rc)

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
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_FieldDestroy(field_temperature, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   call ESMF_LocStreamDestroy(locstream, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   deallocate(lon)
   deallocate(lat)
   deallocate(temperature)

!BOE
!\subsubsection{Creating a LocStream Employing Internally Allocated Memory}
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
   ! Allocate and set example Field data
   !-------------------------------------------------------------------
   allocate(temperature(numLocationsOnThisPet))

   do i=1,numLocationsOnThisPet
      temperature(i)=80.0
   enddo


   !-------------------------------------------------------------------
   ! Create the LocStream:  Allocate space for the LocStream object, 
   ! define the number and distribution of the locations. 
   !-------------------------------------------------------------------
   locstream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                  localCount=numLocationsOnThisPet, &
                                  rc=rc)

   !-------------------------------------------------------------------
   ! Add key data (internally allocating memory).
   !-------------------------------------------------------------------
   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="Lat",                &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="Degrees",           &
                             keyLongName="Latitude", rc=rc)

   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="Lon",                &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="Degrees",           &
                             keyLongName="Longitude", rc=rc)


   !-------------------------------------------------------------------
   ! Get key data. 
   !-------------------------------------------------------------------
   call ESMF_LocStreamGetKey(locstream,                    &
                             localDE=0,                    &
                             keyName="Lat",                &
                             farray=lat,                   &
                             rc=rc)

   call ESMF_LocStreamGetKey(locstream,                    &
                             localDE=0,                    &
                             keyName="Lon",                &
                             farray=lon,                   &
                             rc=rc)

   !-------------------------------------------------------------------
   ! Set key data. 
   !-------------------------------------------------------------------
   do i=1,numLocationsOnThisPet
      lon(i)=360.0/numLocationsOnThisPet
      lat(i)=0.0
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
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_FieldDestroy(field_temperature, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   call ESMF_LocStreamDestroy(locstream, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   deallocate(temperature)



!BOE
!\subsubsection{Creating a LocStream from a Background Grid}
!
! The following is an example of creating a LocStream object from another LocStream object 
! using a background Grid. The new LocStream contains the data present in the old LocStream, 
! but is redistributed so that entries with a given set of coordinates are on the same PET 
! as the piece of the background Grid which contains those coordinates. 
!
!EOE


!BOC
   !-------------------------------------------------------------------
   ! Create the LocStream:  Allocate space for the LocStream object, 
   ! define the number and distribution of the locations. 
   !-------------------------------------------------------------------
   locstream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                  localCount=numLocationsOnThisPet, &
                                  rc=rc)
   !-------------------------------------------------------------------
   ! Add key data (internally allocating memory).
   !-------------------------------------------------------------------
   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="Lon",                &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="Degrees",           &
                             keyLongName="Longitude", rc=rc)

   call ESMF_LocStreamAddKey(locstream,                    &
                             keyName="Lat",                &
                             KeyTypeKind=ESMF_TYPEKIND_R8, &
                             keyUnits="Degrees",           &
                             keyLongName="Latitude", rc=rc)


   !-------------------------------------------------------------------
   ! Get Fortran arrays which hold the key data, so that it can be set. 
   ! Using localDE=0, because the locstream was created with 1 DE per PET. 
   !-------------------------------------------------------------------
   call ESMF_LocStreamGetKey(locstream,                    &
                             localDE=0,                    &
                             keyName="Lon",                &
                             farray=lon,                   &
                             rc=rc)

   call ESMF_LocStreamGetKey(locstream,                    &
                             localDE=0,                    &
                             keyName="Lat",                &
                             farray=lat,                   &
                             rc=rc)

   !-------------------------------------------------------------------
   ! Set the longitude and latitude coordinates of the points in the 
   ! LocStream. Each PET contains points scattered around the equator. 
   !-------------------------------------------------------------------
   do i=1,numLocationsOnThisPet
      lon(i)=0.5+REAL(i-1)*360.0/numLocationsOnThisPet
      lat(i)=0.0
   enddo

   !-------------------------------------------------------------------
   ! Create a Grid to use as the background. The Grid is 
   ! GridLonSize by GridLatSize with the default distribution 
   ! (The first dimension split across the PETs). The coordinate range
   ! is  0 to 360 in longitude and -90 to 90 in latitude. Note that we 
   ! use indexflag=ESMF_INDEX_GLOBAL for the Grid creation. At this time 
   ! this is required for a Grid to be usable as a background Grid.
   !-------------------------------------------------------------------
   grid=ESMF_GridCreateShapeTile(maxIndex=(/GridLonSize,GridLatSize/),          &
                                 indexflag=ESMF_INDEX_GLOBAL, &
                                 rc=rc)

   !-------------------------------------------------------------------
   ! Allocate the corner stagger location in which to put the coordinates. 
   ! (The corner stagger must be used for the Grid to be usable as a
   !  background Grid.)
   !-------------------------------------------------------------------
   call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)


   !-------------------------------------------------------------------
   ! Get access to the Fortran array pointers that hold the Grid 
   ! coordinate information and then set the coordinates to be uniformly 
   ! distributed around the globe. 
   !-------------------------------------------------------------------
   call ESMF_GridGetCoord(grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER,   &
                          coordDim=1,                                           &
                          computationalLBound=clbnd, computationalUBound=cubnd, & 
                          fptr=fptrLonC, rc=rc)

   call ESMF_GridGetCoord(grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
                          coordDim=2,                                         &
                          fptr=fptrLatC, rc=rc)

   do i1=clbnd(1),cubnd(1)
   do i2=clbnd(2),cubnd(2)
      ! Set Grid longitude coordinates as 0 to 360
      fptrLonC(i1,i2) = REAL(i1-1)*360.0/REAL(GridLonSize)

      ! Set Grid latitude coordinates as -90 to 90
      fptrLatC(i1,i2) = -90. + REAL(i2-1)*180.0/REAL(GridLatSize) + &
                                      0.5*180.0/REAL(GridLatSize)
   enddo
   enddo


   !-------------------------------------------------------------------
   ! Create newLocstream on the background Grid using the 
   ! "Lon" and "Lat" keys as the coordinates for the entries in 
   ! locstream. The entries in newLocstream with coordinates (lon,lat)
   ! are on the same PET as the piece of grid which contains (lon,lat). 
   !-------------------------------------------------------------------
   newLocstream=ESMF_LocStreamCreate(locstream, coordKeyNames="Lon:Lat", &
                  background=grid, rc=rc)

   !-------------------------------------------------------------------
   ! A Field can now be created on newLocstream and 
   ! ESMF_FieldRedist() can be used to move data between Fields built 
   ! on locstream and Fields built on newLocstream.
   !-------------------------------------------------------------------


!EOC
   !-------------------------------------------------------------------
   ! Clean up to prepare for the next example.
   !-------------------------------------------------------------------
   call ESMF_LocStreamDestroy(locstream, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   call ESMF_LocStreamDestroy(newLocstream, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   call ESMF_GridDestroy(grid, rc=rc)
   if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

#endif

   !-------------------------------------------------------------------
   ! Shut down and end.
   !-------------------------------------------------------------------
10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_LocStreamEx.F90"
  else
    print *, "FAIL: ESMF_LocStreamEx.F90"
  endif
  
end program
