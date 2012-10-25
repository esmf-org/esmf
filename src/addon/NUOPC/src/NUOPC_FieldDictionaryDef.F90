! $Id: NUOPC_FieldDictionaryDef.F90,v 1.9 2012/10/25 19:48:22 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_FieldDictionaryDef.F90"

module NUOPC_FieldDictionaryDef

  use ESMF

  implicit none
  
  private
  
  type NUOPC_FieldDictionaryEntryS
    sequence
    character(ESMF_MAXSTR)          :: standardName
    character(ESMF_MAXSTR)          :: canonicalUnits
    character(ESMF_MAXSTR), pointer :: connectedOptions(:)
    character(ESMF_MAXSTR)          :: defaultLongName
    character(ESMF_MAXSTR)          :: defaultShortName
  end type
  
  type NUOPC_FieldDictionaryEntry
    sequence
    type(NUOPC_FieldDictionaryEntryS), pointer :: wrap
  end type

  ! public types
  public NUOPC_FieldDictionaryEntryS, NUOPC_FieldDictionaryEntry

  ! public module interfaces
  public NUOPC_FieldDictionaryAddEntryI
  public NUOPC_FieldDictionaryGetEntryI
  public NUOPC_FieldDictionaryDefinition

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryAddEntryI - Add an entry to the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
    standardName, canonicalUnits, defaultLongName, defaultShortName, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    character(*),                     intent(in)            :: canonicalUnits
    character(*),                     intent(in),  optional :: defaultLongName
    character(*),                     intent(in),  optional :: defaultShortName
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
    if (present(defaultLongName)) then
      fdEntry%wrap%defaultLongName  = defaultLongName
    else
      fdEntry%wrap%defaultLongName  = ""
    endif
    if (present(defaultShortName)) then
      fdEntry%wrap%defaultShortName = defaultShortName
    else
      fdEntry%wrap%defaultShortName = ""
    endif
    
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
    standardName, canonicalUnits, defaultLongName, defaultShortName, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    character(*),                     intent(out), optional :: canonicalUnits
    character(*),                     intent(out), optional :: defaultLongName
    character(*),                     intent(out), optional :: defaultShortName
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
    if (present(defaultLongName)) &
      defaultLongName = trim(fdEntry%wrap%defaultLongName)
    if (present(defaultShortName)) &
      defaultShortName = trim(fdEntry%wrap%defaultShortName)

  end subroutine
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


!BOLT l l l l
! "{\bf StandardName}"
! "{\bf Units}"
! "{\bf LongName}"
! "{\bf ShortName} \\"
! ""
! "(canonical)"
! "(default)"
! "(default)"
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "air_pressure_at_sea_level", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Air Pressure at Sea Level", &
      defaultShortName  = "pmsl", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "air_sea_temperature_difference", &
      canonicalUnits    = "K", &
      defaultLongName   = "Air Sea Temperature Difference", &
      defaultShortName  = "astd", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "bottom_depth", &
      canonicalUnits    = "m", &
      defaultLongName   = "Bottom depth", &
      defaultShortName  = "bdpt", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "bottom_friction_coefficient", &
      canonicalUnits    = "1", &
      defaultLongName   = "Bottom Friction Coefficient", &
      defaultShortName  = "bfrc", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "eastward_10m_wind", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Eastward 10m Wind", &
      defaultShortName  = "wndu", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "eastward_northward_wave_radiation_stress", &
      canonicalUnits    = "N m-1", &
      defaultLongName   = "Eastward Northward Wave Radiation Stress", &
      defaultShortName  = "wsuv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "eastward_stokes_drift_current", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Eastward Stokes Drift Current", &
      defaultShortName  = "sdcu", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "eastward_wave_bottom_current", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Eastward Wave Bottom Current", &
      defaultShortName  = "wbcu", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "eastward_wave_radiation_stress", &
      canonicalUnits    = "N m-1", &
      defaultLongName   = "Eastward Wave Radiation Stress", &
      defaultShortName  = "wsuu", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "eastward_wave_radiation_stress_gradient", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Eastward Wave Radiation Stress Gradient", &
      defaultShortName  = "wsgu", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "isotropic_longwave_radiance_in_air", &
      canonicalUnits    = "W m-2 sr-1", &
      defaultLongName   = "Isotropic Longwave Radiance in Air", &
      defaultShortName  = "rilw", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "isotropic_shortwave_radiance_in_air", &
      canonicalUnits    = "W m-2 sr-1", &
      defaultLongName   = "Isotropic Shortwave Radiance in Air", &
      defaultShortName  = "risw", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "magnitude_of_surface_downward_stress", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Magnitude of Surface Downward Stress", &
      defaultShortName  = "taum", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "northward_10m_wind", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Northward 10m Wind", &
      defaultShortName  = "wndv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "northward_stokes_drift_current", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Northward Stokes Drift Current", &
      defaultShortName  = "sdcv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "northward_wave_bottom_current", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Northward Wave Bottom Current", &
      defaultShortName  = "wbcv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "northward_wave_radiation_stress", &
      canonicalUnits    = "N m-1", &
      defaultLongName   = "Northward Wave Radiation Stress", &
      defaultShortName  = "wsvv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "northward_wave_radiation_stress_gradient", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Northward Wave Radiation Stress Gradient", &
      defaultShortName  = "wsgv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "precipitation_amount", &
      canonicalUnits    = "kg m-2", &
      defaultLongName   = "Precipitation Amount", &
      defaultShortName  = "prcp", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_height_above_sea_level", &
      canonicalUnits    = "m", &
      defaultLongName   = "Sea Surface Height Above Sea Level", &
      defaultShortName  = "ssh", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_salinity", &
      canonicalUnits    = "1e-3", &
      defaultLongName   = "Sea Surface Salinity", &
      defaultShortName  = "sss", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_temperature", &
      canonicalUnits    = "K", &
      defaultLongName   = "Sea Surface Temperature", &
      defaultShortName  = "sst", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_air_pressure", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Surface Air Pressure", &
      defaultShortName  = "psfc", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_eastward_sea_water_velocity", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Surface Eastward Sea Water Velocity", &
      defaultShortName  = "sscu", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_eastward_wave_induced_stress", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Surface Eastward Wave Induced Stress", &
      defaultShortName  = "wvsu", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_eastward_stress", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Surface Downward Eastward Stress", &
      defaultShortName  = "tauu", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_heat_flux", &
      canonicalUnits    = "W m-2", &
      defaultLongName   = "Surface Downward Heat Flux", &
      defaultShortName  = "hfns", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_latent_heat_flux", &
      canonicalUnits    = "W m-2", &
      defaultLongName   = "Surface Downward Latent Heat Flux", &
      defaultShortName  = "hfls", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_moisture_flux", &
      canonicalUnits    = "kg m-2 s-1", &
      defaultLongName   = "Surface Downward Moisture Flux", &
      defaultShortName  = "mfns", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_northward_stress", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Surface Downward Northward Stress", &
      defaultShortName  = "tauv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_sensible_heat_flux", &
      canonicalUnits    = "W m-2", &
      defaultLongName   = "Surface Downward Sensible Heat Flux", &
      defaultShortName  = "hfss", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_x_stress", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Surface Downward X Stress", &
      defaultShortName  = "taux", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_y_stress", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Surface Downward Y Stress", &
      defaultShortName  = "tauy", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_northward_sea_water_velocity", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Surface Northward Sea Water Velocity", &
      defaultShortName  = "sscv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_northward_wave_induced_stress", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Surface Northward Wave Induced Stress", &
      defaultShortName  = "wvsv", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_roughness_length", &
      canonicalUnits    = "m", &
      defaultLongName   = "Surface Roughness Length", &
      defaultShortName  = "srl", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_total_wave_induced_stress", &
      canonicalUnits    = "Pa", &
      defaultLongName   = "Surface Total Wave Induced Stress", &
      defaultShortName  = "wvst", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "wave_bottom_current_radian_frequency", &
      canonicalUnits    = "rad s-1", &
      defaultLongName   = "Wave Bottom Current Radian Frequency", &
      defaultShortName  = "wbcf", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "wave_induced_charnock_parameter", &
      canonicalUnits    = "1", &
      defaultLongName   = "Wave Induced Charnock Parameter", &
      defaultShortName  = "chnk", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "x_10m_wind", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "X 10m Wind", &
      defaultShortName  = "wndx", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "y_10m_wind", &
      canonicalUnits    = "m s-1", &
      defaultLongName   = "Y 10m Wind", &
      defaultShortName  = "wndy", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!EOLT
  end subroutine
  !-----------------------------------------------------------------------------

end module
