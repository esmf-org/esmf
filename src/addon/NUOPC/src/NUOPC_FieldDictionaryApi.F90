! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_FieldDictionaryApi.F90"
!==============================================================================

module NUOPC_FieldDictionaryApi

  use ESMF
  use NUOPC_FieldDictionaryDef
  use NUOPC_FreeFormatDef

  implicit none
  
  private

  ! public types
  public NUOPC_FieldDictionaryEntryS, NUOPC_FieldDictionaryEntry

  ! public module variables
  logical, save :: NUOPC_FieldDictionaryIsSetup = .false.
  public NUOPC_FieldDictionaryIsSetup
  logical, save :: NUOPC_FieldDictionaryAutoAdd = .false.  
  public NUOPC_FieldDictionaryAutoAdd
  type(ESMF_Container), save  :: NUOPC_FieldDictionary
  public NUOPC_FieldDictionary
  
  ! public module interface for the NUOPC API
  public NUOPC_FieldDictionaryAddEntry
  public NUOPC_FieldDictionaryEgest
  public NUOPC_FieldDictionaryGetEntry
  public NUOPC_FieldDictionaryHasEntry
  public NUOPC_FieldDictionaryMatchSyno
  public NUOPC_FieldDictionarySetSyno
  public NUOPC_FieldDictionarySetup
  public NUOPC_FieldDictionarySetAutoAdd

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryAddEntry - Add an entry to the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryAddEntry(standardName, canonicalUnits, rc)
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName
    character(*),                 intent(in)            :: canonicalUnits
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Add an entry to the NUOPC Field dictionary. If necessary the dictionary is
!   first set up.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_FieldDictionaryAddEntryI(NUOPC_FieldDictionary, &
      standardName = standardName, canonicalUnits = canonicalUnits, &
      rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryEgest - Egest NUOPC Field dictionary into FreeFormat
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryEgest(freeFormat, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat), intent(out)           :: freeFormat
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Egest the contents of the NUOPC Field dictionary into a FreeFormat object.
!   It is the caller's responsibility to destroy the created {\tt freeFormat}
!   object.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call NUOPC_FieldDictionaryEgestI(NUOPC_FieldDictionary, freeFormat, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryGetEntry - Get information about a NUOPC Field dictionary entry
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryGetEntry(standardName, canonicalUnits, rc)
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName
    character(*),                 intent(out), optional :: canonicalUnits
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Return the canonical units that the NUOPC Field dictionary associates with
!   the {\tt standardName}.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_FieldDictionaryGetEntryI(NUOPC_FieldDictionary, &
      standardName = standardName, canonicalUnits = canonicalUnits, &
      rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryHasEntry - Check whether the NUOPC Field dictionary has a specific entry
! !INTERFACE:
  function NUOPC_FieldDictionaryHasEntry(standardName, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldDictionaryHasEntry
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the NUOPC Field dictionary has an entry with the
!   specified {\tt standardName}, {\tt .false.} otherwise.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    NUOPC_FieldDictionaryHasEntry = &
      NUOPC_FieldDictionaryHasEntryI(NUOPC_FieldDictionary, &
      standardName = standardName, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryMatchSyno - Check whether the NUOPC Field dictionary considers the standard names synonyms
! !INTERFACE:
  function NUOPC_FieldDictionaryMatchSyno(standardName1, standardName2, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldDictionaryMatchSyno
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName1
    character(*),                 intent(in)            :: standardName2
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the NUOPC Field dictionary considers
!   {\tt standardName1} and {\tt standardName2} synonyms, {\tt .false.} 
!   otherwise. An entry with standard name of {\tt standardName1} must
!   exist in the field dictionary, or else an error will be returned. 
!   However, {\tt standardName2} need not correspond to an existing entry.
!   If {\tt standardName2} does not correspond to an existing entry in the 
!   field dictionary, the value of {.false.} will be returned.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    NUOPC_FieldDictionaryMatchSyno = &
      NUOPC_FieldDictionaryMatchSynoI(NUOPC_FieldDictionary, &
      standardName1 = standardName1, standardName2 = standardName2, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionarySetSyno - Set synonyms in the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionarySetSyno(standardNames, rc)
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardNames(:)
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Set all of the elements of the {\tt standardNames} argument to be considered
!   synonyms by the field dictionary. Every element in {\tt standardNames} must
!   correspond to the standard name of already existing entries in the field 
!   dictionary, or else an error will be returned.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_FieldDictionarySetSynoI(NUOPC_FieldDictionary, &
      standardNames = standardNames, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionarySetup - Setup the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionarySetup(rc)
! !ARGUMENTS:
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Setup the NUOPC Field dictionary.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    if (.not.NUOPC_FieldDictionaryIsSetup) then
    
      NUOPC_FieldDictionary = ESMF_ContainerCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    
      call ESMF_ContainerGarbageOn(NUOPC_FieldDictionary, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

      call NUOPC_FieldDictionaryDefinition(NUOPC_FieldDictionary, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      
      NUOPC_FieldDictionaryIsSetup = .true.
      
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionarySetAutoAdd - Turn on/off AutoAdd
! !INTERFACE:
  subroutine NUOPC_FieldDictionarySetAutoAdd(setting, rc)
! !ARGUMENTS:
    logical,      intent(in)              :: setting
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Turn on/off AutoAdd in the NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    NUOPC_FieldDictionaryAutoAdd = setting

  end subroutine
  !-----------------------------------------------------------------------------

end module
