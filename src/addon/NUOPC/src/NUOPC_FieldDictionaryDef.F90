! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2015, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_FieldDictionaryDef.F90"
!==============================================================================

module NUOPC_FieldDictionaryDef

  use ESMF

  implicit none
  
  private
  
  type NUOPC_FieldDictionaryEntryS
    character(ESMF_MAXSTR)          :: standardName
    character(ESMF_MAXSTR)          :: canonicalUnits
    character(ESMF_MAXSTR), pointer :: connectedOptions(:)
  end type
  
  type NUOPC_FieldDictionaryEntry
    type(NUOPC_FieldDictionaryEntryS), pointer :: wrap
  end type

  ! public types
  public NUOPC_FieldDictionaryEntryS, NUOPC_FieldDictionaryEntry

  ! public module interfaces
  public NUOPC_FieldDictionaryAddEntryI
  public NUOPC_FieldDictionaryGetEntryI
  public NUOPC_FieldDictionaryHasEntryI
  public NUOPC_FieldDictionaryDefinition

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryAddEntryI - Add an entry to the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
    standardName, canonicalUnits, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    character(*),                     intent(in)            :: canonicalUnits
    integer,                          intent(out), optional :: rc
! !DESCRIPTION:
!   Add an entry to the NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    integer                           :: stat, i, count
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! allocate fdEntry
    allocate(fdEntry%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating fdEntry", &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! set values inside of fdEntry
    fdEntry%wrap%standardName     = standardName
    fdEntry%wrap%canonicalUnits   = canonicalUnits
    allocate(fdEntry%wrap%connectedOptions(2), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating fdEntry member", &
      line=__LINE__, file=FILENAME)) return  ! bail out
    fdEntry%wrap%connectedOptions(1)     = "false" ! default
    fdEntry%wrap%connectedOptions(2)     = "true"
    
    ! add fdEntry to the FieldDictionary
    call ESMF_ContainerAddUDT(fieldDictionary, &
      trim(fdEntry%wrap%standardName), fdEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryGetEntryI - Get information about a NUOPC Field dictionary entry
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryGetEntryI(fieldDictionary, &
    standardName, canonicalUnits, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    character(*),                     intent(out), optional :: canonicalUnits
    integer,                          intent(out), optional :: rc
! !DESCRIPTION:
!   Query an entry in the NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ContainerGetUDT(fieldDictionary, trim(StandardName), &
      fdEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    if (present(canonicalUnits)) &
      canonicalUnits = trim(fdEntry%wrap%canonicalUnits)

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryHasEntryI - Check whether the NUOPC Field dictionary has a specific entry
! !INTERFACE:
  function NUOPC_FieldDictionaryHasEntryI(fieldDictionary, standardName, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldDictionaryHasEntryI
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    integer,                          intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if the NUOPC Field dictionary has an entry with the
!   specified StandardName, {\tt .false.} otherwise.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Logical)            :: isPres
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call c_ESMC_ContainerGetIsPresent(fieldDictionary, trim(standardName), &
      isPres, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    NUOPC_FieldDictionaryHasEntryI = isPres
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryDefinition - Create the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryDefinition(fieldDictionary, rc)
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)          :: fieldDictionary
    integer,              intent(out),  optional :: rc
! !DESCRIPTION:
!   Create NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

!BOLT l l
! "{\bf StandardName}"
! "{\bf CanonicalUnits}"
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "air_pressure_at_sea_level", &
      canonicalUnits    = "Pa", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "magnitude_of_surface_downward_stress", &
      canonicalUnits    = "Pa", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "precipitation_flux", &
      canonicalUnits    = "kg m-2 s-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_height_above_sea_level", &
      canonicalUnits    = "m", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_salinity", &
      canonicalUnits    = "1e-3", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_temperature", &
      canonicalUnits    = "K", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_eastward_sea_water_velocity", &
      canonicalUnits    = "m s-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_eastward_stress", &
      canonicalUnits    = "Pa", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_heat_flux_in_air", &
      canonicalUnits    = "W m-2", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_water_flux", &
      canonicalUnits    = "kg m-2 s-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_northward_stress", &
      canonicalUnits    = "Pa", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_net_downward_shortwave_flux", &
      canonicalUnits    = "W m-2", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_net_downward_longwave_flux", &
      canonicalUnits    = "W m-2", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_northward_sea_water_velocity", &
      canonicalUnits    = "m s-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!EOLT
  end subroutine
  !-----------------------------------------------------------------------------

end module
