! $Id: NUOPC_FieldDictionaryDef.F90,v 1.2 2011/05/13 17:50:06 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_FieldDictionaryDef.F90"

module NUOPC_FieldDictionaryDef

  use ESMF_Mod

  implicit none
  
  private
  
  type NUOPC_FieldDictionaryEntryS
    sequence
    character(ESMF_MAXSTR)          :: standardName
    character(ESMF_MAXSTR), pointer :: unitOptions(:)
    character(ESMF_MAXSTR), pointer :: connectedOptions(:)
    character(ESMF_MAXSTR)          :: defaultLongName
    character(ESMF_MAXSTR)          :: defaultShortName
  end type
  
  type NUOPC_FieldDictionaryEntry
    sequence
    type(NUOPC_FieldDictionaryEntryS), pointer :: wrap
  end type

  ! public module interfaces
  public NUOPC_FieldDictionaryDefinition

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryAddEntry - Add an entry to the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryAddEntry(fieldDictionary, &
    standardName, unitOptions, defaultLongName, defaultShortName, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    character(*),                     intent(in)            :: unitOptions(:)
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
    count = size(unitOptions)
    allocate(fdEntry%wrap%unitOptions(count), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating stdAttrNameList", &
      line=__LINE__, file=FILENAME)) return  ! bail out
    do i=1, count
      fdEntry%wrap%unitOptions(i) = unitOptions(i)
    enddo
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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
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

!BOT l l l l
! "{\bf StandardName}"
! "{\bf Units}"
! "{\bf LongName (default)}"
! "{\bf ShortName (default)}"
!BOTL
    call NUOPC_FieldDictionaryAddEntry(fieldDictionary, &
      standardName      = "air_pressure_at_sea_level", &
      unitOptions       = (/"Pa"/), &
      defaultLongName   = "Air Pressure at Sea Level", &
      defaultShortName  = "pmsl", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntry(fieldDictionary, &
      standardName      = "isotropic_shortwave_radiance_in_air", &
      unitOptions       = (/"W m-2 sr-1"/), &
      defaultLongName   = "Isotropic Shortwave Radiance in Air", &
      defaultShortName  = "risw", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntry(fieldDictionary, &
      standardName      = "sea_surface_temperature", &
      unitOptions       = (/"K"/), &
      defaultLongName   = "Sea Surface Temperature", &
      defaultShortName  = "sst", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!EOT
  end subroutine
  !-----------------------------------------------------------------------------

end module
