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
    
      type(ESMF_LocStream) :: locstream
      integer :: localPet, petCount
      integer,parameter :: numLocationsOnThisPet=20
      integer :: i

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm,  rc=rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

#if 1
!BOE
!\subsubsection{Creating A LocStream Employing User Allocated Memory}
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
!\subsubsection{Creating A LocStream Employing Internally Allocated Memory}
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
