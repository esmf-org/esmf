! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
  use NUOPC_FreeFormatDef

  implicit none
  
  private

  ! public types
  public NUOPC_FieldDictionaryEntryS, NUOPC_FieldDictionaryEntry

  type NUOPC_FieldDictionaryEntryS
    character(ESMF_MAXSTR)          :: standardName
    character(ESMF_MAXSTR)          :: canonicalUnits
    character(ESMF_MAXSTR), pointer :: connectedOptions(:)
    character(ESMF_MAXSTR), pointer :: synonyms(:)
  end type
  
  type NUOPC_FieldDictionaryEntry
    type(NUOPC_FieldDictionaryEntryS), pointer :: wrap
  end type

  ! public module interfaces
  public NUOPC_FieldDictionaryAddEntryI
  public NUOPC_FieldDictionaryCreateI
  public NUOPC_FieldDictionaryDestroyI
  public NUOPC_FieldDictionaryEgestI
  public NUOPC_FieldDictionaryGetEntryI
  public NUOPC_FieldDictionaryHasEntryI
  public NUOPC_FieldDictionaryIngestI
  public NUOPC_FieldDictionaryMatchSynoI
  public NUOPC_FieldDictionarySetSynoI
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
    integer                           :: stat
    integer                           :: localrc
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! allocate fdEntry
    allocate(fdEntry%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating fdEntry", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! set values inside of fdEntry
    fdEntry%wrap%standardName     = standardName
    fdEntry%wrap%canonicalUnits   = canonicalUnits
    allocate(fdEntry%wrap%connectedOptions(2), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating fdEntry member", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    fdEntry%wrap%connectedOptions(1)     = "false" ! default
    fdEntry%wrap%connectedOptions(2)     = "true"
    allocate(fdEntry%wrap%synonyms(0), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating synonyms member", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! add fdEntry to the FieldDictionary
    call ESMF_ContainerAddUDT(fieldDictionary, &
      trim(fdEntry%wrap%standardName), fdEntry, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryCreateI - Create an empty NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryCreateI(fieldDictionary, rc)
! !ARGUMENTS:
    type(ESMF_Container), intent(out)           :: fieldDictionary
    integer,              intent(out), optional :: rc
! !DESCRIPTION:
!   Create an empty container to host a NUOPC Field dictionary. Garbage feature
!   will always be on for this container.
!
!EOPI
  !-----------------------------------------------------------------------------
    integer :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    fieldDictionary = ESMF_ContainerCreate(rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    call ESMF_ContainerGarbageOn(fieldDictionary, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryDestroyI - Destroy a NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryDestroyI(fieldDictionary, rc)
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)         :: fieldDictionary
    integer,              intent(out), optional :: rc
! !DESCRIPTION:
!   Erase the content of a NUOPC Field dictionary and free up the memory
!   associated with it.
!EOPI
  !-----------------------------------------------------------------------------
    integer :: localrc
    integer :: garbageCount, item
    type(NUOPC_FieldDictionaryEntry) :: fdEntry

    if (present(rc)) rc = ESMF_SUCCESS

    ! clear NUOPC Field dictionary content (move to garbage)
    call ESMF_ContainerClear(fieldDictionary, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

    ! retrieve number of dictionary items in garbage
    call ESMF_ContainerGarbageGet(fieldDictionary, &
      garbageCount=garbageCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

    ! loop over garbage items to deallocate them
    do item = 1, garbageCount
      call ESMF_ContainerGarbageGetUDT(fieldDictionary, &
        item, fdEntry, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
      deallocate(fdEntry % wrap, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, &
        msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)) &
        return  ! bail out
    end do

    ! destroy original NUOPC Field dictionary container
    call ESMF_ContainerDestroy(fieldDictionary, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryEgestI - Egest NUOPC Field dictionary into FreeFormat
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryEgestI(fieldDictionary, freeFormat, iofmt, rc)
! !ARGUMENTS:
    type(ESMF_Container),   intent(inout)         :: fieldDictionary
    type(NUOPC_FreeFormat), intent(out)           :: freeFormat
    type(ESMF_IOFmt_Flag),  intent(in),  optional :: iofmt
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Egest the contents of the NUOPC Field dictionary into a FreeFormat object
!   in the format specified by {\tt iofmt}.
!   It is the caller's responsibility to destroy the created {\tt freeFormat}
!   object.
!EOPI
  !-----------------------------------------------------------------------------
    integer :: localrc
    logical :: isIOFmtYAML

    if (present(rc)) rc = ESMF_SUCCESS

    isIOFmtYAML = .false.
    if (present(iofmt)) isIOFmtYAML = (iofmt == ESMF_IOFMT_YAML)

    if (isIOFmtYAML) then
      call NUOPC_FieldDictionaryEgestYAMLI(fieldDictionary, freeFormat, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
    else
      call NUOPC_FieldDictionaryEgestDefaultI(fieldDictionary, freeFormat, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
    end if

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryEgestDefaultI - Egest NUOPC Field dictionary into default FreeFormat
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryEgestDefaultI(fieldDictionary, freeFormat, rc)
! !ARGUMENTS:
    type(ESMF_Container),   intent(inout)         :: fieldDictionary
    type(NUOPC_FreeFormat), intent(out)           :: freeFormat
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Egest the contents of the NUOPC Field dictionary into a default FreeFormat object.
!   It is the caller's responsibility to destroy the created {\tt freeFormat}
!   object.
!EOPI
  !-----------------------------------------------------------------------------
    type(NUOPC_FieldDictionaryEntry)                :: fdEntry
    integer                                         :: localrc
    integer                                         :: stat, i, k, count, len
    character(len=NUOPC_FreeFormatLen)              :: tempString
    character(len=10)                               :: lenString
    character(len=*), parameter :: sepString  = &
      "----------------------------------------------------------------"
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ContainerGet(fieldDictionary, itemCount=count, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! create free format object with estimated capacity
    freeFormat = NUOPC_FreeFormatCreate(capacity=4*count, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    do i=1, count
      call ESMF_ContainerGetUDTByIndex(fieldDictionary, i, fdEntry, &
        ESMF_ITEMORDER_ABC, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! determine approoriate character length for output
      len = max(48,len_trim(fdEntry%wrap%standardName), &
        len_trim(fdEntry%wrap%canonicalUnits))
      do k=1, size(fdEntry%wrap%synonyms)
        len = max(len,len_trim(fdEntry%wrap%synonyms(k)))
      enddo
      write(lenString,"(I3)") len
      ! standardName
      write(tempString, "('standardName:   ',a"//trim(lenString)//")") &
        trim(fdEntry%wrap%standardName)
      call NUOPC_FreeFormatAdd(freeFormat, (/adjustl(tempString)/), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! canonicalUnits
      write(tempString, "('canonicalUnits: ',a"//trim(lenString)//")") &
        trim(fdEntry%wrap%canonicalUnits)
      call NUOPC_FreeFormatAdd(freeFormat, (/adjustl(tempString)/), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      ! synonyms
      do k=1, size(fdEntry%wrap%synonyms)
        write(tempString, "('synonym:        ',a"//trim(lenString)//")") &
          trim(fdEntry%wrap%synonyms(k))
        call NUOPC_FreeFormatAdd(freeFormat, (/adjustl(tempString)/), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) &
          return  ! bail out
      enddo
      call NUOPC_FreeFormatAdd(freeFormat, (/adjustl(sepString)/), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
    enddo

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryEgestYAMLI - Egest NUOPC Field dictionary into FreeFormat as YAML
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryEgestYAMLI(fieldDictionary, freeFormat, rc)
! !ARGUMENTS:
    type(ESMF_Container),   intent(inout)         :: fieldDictionary
    type(NUOPC_FreeFormat), intent(out)           :: freeFormat
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Egest the contents of the NUOPC Field dictionary into a FreeFormat object
!   expressed using the YAML Ain't Markup Language (YAML).
!   An empty FreeFormat object is returned in case of empty NUOPC Field dictionary.
!   It is the caller's responsibility to destroy the created {\tt freeFormat}
!   object.
!EOPI
  !-----------------------------------------------------------------------------
    character(len=*), parameter :: intro(6) = &
      (/&
      "field_dictionary:                                                   ", &
      "  version_number: 0.0.0                                             ", &
      "  institution: National ESPC, CSC & MCL Working Groups              ", &
      "  source: automatically generated by the NUOPC Layer                ", &
      "  description: Community-based dictionary for shared coupling fields", &
      "  entries:                                                          "  &
      /)

    integer :: localrc
    integer :: aliasCount, capacity, item, itemCount, localItemCount, k
    logical :: isNamePres, isAliasPres
    type(ESMF_Container)               :: localFieldDictionary
    type(NUOPC_FieldDictionaryEntry)   :: fdEntry, lfdEntry
    character(len=NUOPC_FreeFormatLen) :: tempString

    ! The NUOPC Field dictionary lists synonyms as standard names as well.
    ! However, synonyms should be properly listed only as aliases in the
    ! NUOPC Field dictionary YAML file to avoid duplicates and reduce size.
    ! This is accomplished here by copying the NUOPC Field dictionary to a
    ! local Field dictionary, then removing those standard name entries that
    ! were added as synonyms in the original ESMF_ITEMORDER_ADDORDER.

    if (present(rc)) rc = ESMF_SUCCESS

    ! create local Field dictionary
    call NUOPC_FieldDictionaryCreateI(localFieldDictionary, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! retrieve number of item in NUOPC Field dictionary
    call ESMF_ContainerGet(fieldDictionary, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! copy NUOPC Field dictionary to local Field dictionary
    do item = 1, itemCount
      call ESMF_ContainerGetUDTByIndex(fieldDictionary, item, fdEntry, &
        ESMF_ITEMORDER_ADDORDER, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_FieldDictionaryAddEntryI(localFieldDictionary, &
        trim(fdEntry%wrap%standardName), &
        trim(fdEntry%wrap%canonicalUnits), &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
    end do

    ! now remove synonyms
    do item = 1, itemCount
      call ESMF_ContainerGetUDTByIndex(fieldDictionary, item, fdEntry, &
        ESMF_ITEMORDER_ADDORDER, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      isNamePres = NUOPC_FieldDictionaryHasEntryI(localFieldDictionary, &
        trim(fdEntry%wrap%standardName), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      if (isNamePres) then
        do k = 1, size(fdEntry%wrap%synonyms)
          isAliasPres = NUOPC_FieldDictionaryHasEntryI(localFieldDictionary, &
            trim(fdEntry%wrap%synonyms(k)), rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) &
            return  ! bail out
          if (isAliasPres) then
            call ESMF_ContainerRemove(localFieldDictionary, &
              itemNameList=(/trim(fdEntry%wrap%synonyms(k))/), rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) &
              return
          end if
        end do
      end if
    end do

    ! compute capacity of FreeFormat object

    ! (a) retrieve number of item in local Field dictionary
    call ESMF_ContainerGet(localFieldDictionary, itemCount=localItemCount, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! (b) count total number of aliases
    aliasCount = 0
    do item = 1, localItemCount
      call ESMF_ContainerGetUDTByIndex(localFieldDictionary, item, fdEntry, &
        ESMF_ITEMORDER_ADDORDER, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      aliasCount = aliasCount + size(fdEntry%wrap%synonyms)
    end do

    capacity = 0
    if (localItemCount > 0) capacity = size(intro) + 2*(localItemCount + aliasCount)

    ! create free format object with estimated capacity
    freeFormat = NUOPC_FreeFormatCreate(capacity=capacity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! fill FreeFormat object

    ! add intro
    if (capacity > 0) then
      do item = 1, size(intro)
        call NUOPC_FreeFormatAdd(freeFormat, (/intro(item)/), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) &
          return  ! bail out
      end do
    end if

    ! add standard names first
    do item = 1, localItemCount
      call ESMF_ContainerGetUDTByIndex(localFieldDictionary, item, fdEntry, &
        ESMF_ITEMORDER_ABC, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      tempString = ""
      write(tempString, "(4x,'- standard_name: ',a)") &
        trim(fdEntry%wrap%standardName)
      call NUOPC_FreeFormatAdd(freeFormat, (/tempString/), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      write(tempString, "(6x,'canonical_units: ',a)") &
        trim(fdEntry%wrap%canonicalUnits)
      call NUOPC_FreeFormatAdd(freeFormat, (/tempString/), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
    end do

    ! now add aliases
    do item = 1, localItemCount
      call ESMF_ContainerGetUDTByIndex(localFieldDictionary, item, lfdEntry, &
        ESMF_ITEMORDER_ABC, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out

      call ESMF_ContainerGetUDT(fieldDictionary, &
        trim(lfdEntry%wrap%standardName), fdEntry, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out

      do k = 1, size(fdEntry%wrap%synonyms)
        tempString = ""
        write(tempString, "(4x,'- alias: ',a)") &
          trim(fdEntry%wrap%synonyms(k))
        call NUOPC_FreeFormatAdd(freeFormat, (/tempString/), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) &
          return  ! bail out
        tempString = ""
        write(tempString, "(4x,'  standard_name: ',a)") &
          trim(fdEntry%wrap%standardName)
        call NUOPC_FreeFormatAdd(freeFormat, (/tempString/), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) &
          return  ! bail out
      enddo
    enddo

    ! delete local Field dictionary
    call NUOPC_FieldDictionaryDestroyI(localFieldDictionary, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) &
      return  ! bail out

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
    integer                           :: localrc
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ContainerGetUDT(fieldDictionary, trim(standardName), &
      fdEntry, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
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
    integer                       :: localrc
    type(ESMF_Logical)            :: isPres
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    NUOPC_FieldDictionaryHasEntryI = .false.

    call c_ESMC_ContainerGetIsPresent(fieldDictionary, trim(standardName), &
      isPres, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
    NUOPC_FieldDictionaryHasEntryI = isPres
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryIngestI - Ingest FreeFormat into NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryIngestI(fieldDictionary, freeFormat, rc)
! !ARGUMENTS:
    type(ESMF_Container),   intent(inout)         :: fieldDictionary
    type(NUOPC_FreeFormat), intent(in)            :: freeFormat
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Ingest the contents of a FreeFormat object into the NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    integer                                         :: localrc
    integer                                         :: stat, i, k, nameCount
    character(len=NUOPC_FreeFormatLen)              :: keyString, valueString
    character(len=NUOPC_FreeFormatLen)              :: canonicalUnits
    character(len=NUOPC_FreeFormatLen), allocatable :: standardNames(:)
    character(len=NUOPC_FreeFormatLen), allocatable :: tmpList(:)

    logical                     :: isPres
    integer,          parameter :: bufferSize = 10
    character(len=*), parameter :: sepString  = &
      "----------------------------------------------------------------"

    if (present(rc)) rc = ESMF_SUCCESS

    ! allocate buffer to hold list of standard names and synonyms
    allocate(standardNames(bufferSize), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating internal workspace", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return

    nameCount      = 0
    standardNames  = ""
    canonicalUnits = ""

    ! process lines from FreeFormat object
    freeform_input: do i = 1, freeFormat % count

      if (trim(freeFormat % stringList(i)) == sepString) then

        if (nameCount == 0) then
          call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
            msg="Invalid FreeFormat object: missing standardName", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)
          exit freeform_input
        end if
        if (len_trim(canonicalUnits) == 0) then
          call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
            msg="Invalid FreeFormat object: missing canonicalUnits", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)
          exit freeform_input
        end if

        ! add standard name entries
        do k = 1, nameCount
          isPres = NUOPC_FieldDictionaryHasEntryI(fieldDictionary, &
            standardNames(k), rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) exit freeform_input
          if (.not.isPres) then
            call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
              standardNames(k), canonicalUnits, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) exit freeform_input
          end if
        end do

        ! add synonyms
        if (nameCount > 1) then
          call NUOPC_FieldDictionarySetSynoI(fieldDictionary, &
            standardNames(1:nameCount), rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) exit freeform_input
        end if

        ! reset buffers and counter
        nameCount      = 0
        standardNames  = ""
        canonicalUnits = ""

      else

        call getKeyValue(freeFormat % stringList(i), &
          keyString, valueString, localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, &
          msg="Error reading from FreeFormat object", &
          line=__LINE__, file=FILENAME, &
          rcToReturn=rc)) exit freeform_input

        select case (trim(keyString))
          case ("canonicalUnits")
            if (len_trim(canonicalUnits) > 0) then
              call ESMF_LogSetError(ESMF_RC_DUP_NAME, &
                msg="Invalid FreeFormat object: canonicalUnits", &
                line=__LINE__, &
                file=FILENAME, &
                rcToReturn=rc)
              exit freeform_input
            end if
            canonicalUnits = valueString
          case ("standardName","synonym")
            nameCount = nameCount + 1
            if (nameCount > size(standardNames)) then
              allocate(tmpList(size(standardNames)), stat=stat)
              if (ESMF_LogFoundAllocError(stat, &
                msg="allocating internal workspace", &
                line=__LINE__, file=FILENAME, &
                rcToReturn=rc)) exit freeform_input
              tmpList = standardNames
              deallocate(standardNames, stat=stat)
              if (ESMF_LogFoundAllocError(stat, &
                msg="deallocating internal workspace", &
                line=__LINE__, file=FILENAME, &
                rcToReturn=rc)) exit freeform_input
              allocate(standardNames(size(tmpList)+bufferSize), stat=stat)
              if (ESMF_LogFoundAllocError(stat, &
                msg="allocating internal workspace", &
                line=__LINE__, file=FILENAME, &
                rcToReturn=rc)) exit freeform_input
              standardNames = ""
              standardNames(1:size(tmpList)) = tmpList
              deallocate(tmpList, stat=stat)
              if (ESMF_LogFoundDeallocError(stat, &
                msg="deallocating internal workspace", &
                line=__LINE__, file=FILENAME, &
                rcToReturn=rc)) exit freeform_input
            end if
            standardNames(nameCount) = valueString
          case default
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
              msg="Invalid FreeFormat keyword: " // trim(keyString), &
              line=__LINE__, &
              file=FILENAME, &
              rcToReturn=rc)
            exit freeform_input
        end select
      end if
    end do freeform_input

    if (allocated(tmpList)) then
      deallocate(tmpList, stat=stat)
      isPres = ESMF_LogFoundDeallocError(stat, &
        msg="deallocating internal workspace", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)
     end if

    if (allocated(standardNames)) then
      deallocate(standardNames, stat=stat)
      isPres = ESMF_LogFoundDeallocError(stat, &
        msg="deallocating internal workspace", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)
     end if

  contains

    subroutine getKeyValue(string, keyString, valueString, rc)
      character(len=*), intent(in)    :: string
      character(len=*), intent(inout) :: keyString
      character(len=*), intent(inout) :: valueString
      integer,          intent(out)   :: rc

      integer :: ic, lenString

      rc = ESMF_SUCCESS

      keyString   = ""
      valueString = ""

      ic = index(string, ":")
      if (ic > 2) then
        keyString = adjustl(string(1:ic-1))
      else
        call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
          msg="key not found", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      end if

      lenString = len_trim(string)
      if (ic < lenString) then
        valueString = adjustl(string(ic+1:lenString))
      else
        call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
          msg="value not found", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      end if

    end subroutine getKeyValue

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryMatchSynoI - Check whether the NUOPC Field dictionary considers the standard names synonyms
! !INTERFACE:
  function NUOPC_FieldDictionaryMatchSynoI(fieldDictionary, standardName1, &
    standardName2, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldDictionaryMatchSynoI
! !ARGUMENTS:
    type(ESMF_Container),         intent(inout)         :: fieldDictionary
    character(*),                 intent(in)            :: standardName1
    character(*),                 intent(in)            :: standardName2
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the NUOPC Field dictionary considers
!   {\tt standardName1} and {\tt standardName2} synonyms, {\tt .false.} 
!   otherwise. Also, if {\tt standardName1} and/or {\tt standardName2} do not 
!   correspond to an existing dictionary entry, {.false.} will be returned.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    integer                           :: localrc
    integer                           :: i
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    
    if (present(rc)) rc = ESMF_SUCCESS

    NUOPC_FieldDictionaryMatchSynoI = &
      NUOPC_FieldDictionaryHasEntryI(fieldDictionary, standardName1, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    if (.not.NUOPC_FieldDictionaryMatchSynoI) return  ! early exit

    NUOPC_FieldDictionaryMatchSynoI = .true. ! re-initialize
    if (trim(standardName2) == trim(standardName1)) return  ! early exit
    
    NUOPC_FieldDictionaryMatchSynoI = .false. ! re-initialize
    call ESMF_ContainerGetUDT(fieldDictionary, trim(standardName1), &
      fdEntry, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
    do i=1, size(fdEntry%wrap%synonyms)
      if (trim(standardName2) == trim(fdEntry%wrap%synonyms(i))) then
        NUOPC_FieldDictionaryMatchSynoI = .true.
        exit  ! bail out on finding synonym
      endif
    enddo
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionarySetSynoI - Set synonyms in the NUOPC Field dictionary entry
! !INTERFACE:
  subroutine NUOPC_FieldDictionarySetSynoI(fieldDictionary, &
    standardNames, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardNames(:)
    integer,                          intent(out), optional :: rc
! !DESCRIPTION:
!   Set all of the elements of the {\tt standardNames} argument to be considered
!   synonyms by the field dictionary. Every element in {\tt standardNames} must
!   correspond to the standard name of already existing entries in the field 
!   dictionary, or else an error will be returned.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    integer                           :: localrc
    integer                           :: i, k, j, stat
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    character(ESMF_MAXSTR), pointer   :: synonyms(:)
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! first loop to ensure all of the provided standard names are valid entries
    do i=1, size(standardNames)
      call ESMF_ContainerGetUDT(fieldDictionary, trim(standardNames(i)), &
        fdEntry, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    enddo
    
    ! second loop to set the synonyms
    do i=1, size(standardNames)
    
      call ESMF_ContainerGetUDT(fieldDictionary, trim(standardNames(i)), &
        fdEntry, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
      j = size(fdEntry%wrap%synonyms)
      allocate(synonyms(j+size(standardNames)-1), stat=stat)
      if (ESMF_LogFoundAllocError(stat, msg="allocating synonyms array", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      
      do k=1, j
        synonyms(k) = fdEntry%wrap%synonyms(k)  ! fill in the existing entries
      enddo
      
      do k=1, size(standardNames)
        if (k==i) cycle
        j = j+1
        synonyms(j) = trim(standardNames(k))
      enddo
      
      deallocate(fdEntry%wrap%synonyms)
      fdEntry%wrap%synonyms => synonyms

    enddo

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
    integer                 :: localrc
    type(NUOPC_FreeFormat)  :: freeFormatFD
    
    if (present(rc)) rc = ESMF_SUCCESS

!   The following definition of the default FD is pulled into the NUOPC RefDoc
!   via ProTex. Modify with care.
!
!BOLTFD l l
! ":{\bf StandardName}"
! ":{\bf CanonicalUnits}"
    freeFormatFD = NUOPC_FreeFormatCreate(stringList=(/ &
!BOLTCFD
      "standardName:                          air_pressure_at_sea_level", &
      "canonicalUnits:                                               Pa", &
      "----------------------------------------------------------------", &
      "standardName:               magnitude_of_surface_downward_stress", &
      "canonicalUnits:                                               Pa", &
      "----------------------------------------------------------------", &
      "standardName:                                 precipitation_flux", &
      "canonicalUnits:                                       kg m-2 s-1", &
      "----------------------------------------------------------------", &
      "standardName:                 sea_surface_height_above_sea_level", &
      "canonicalUnits:                                                m", &
      "----------------------------------------------------------------", &
      "standardName:                               sea_surface_salinity", &
      "canonicalUnits:                                             1e-3", &
      "----------------------------------------------------------------", &
      "standardName:                            sea_surface_temperature", &
      "canonicalUnits:                                                K", &
      "----------------------------------------------------------------", &
      "standardName:                   surface_downward_eastward_stress", &
      "canonicalUnits:                                               Pa", &
      "----------------------------------------------------------------", &
      "standardName:                  surface_downward_heat_flux_in_air", &
      "canonicalUnits:                                            W m-2", &
      "----------------------------------------------------------------", &
      "standardName:                  surface_downward_northward_stress", &
      "canonicalUnits:                                               Pa", &
      "----------------------------------------------------------------", &
      "standardName:                        surface_downward_water_flux", &
      "canonicalUnits:                                       kg m-2 s-1", &
      "----------------------------------------------------------------", &
      "standardName:                surface_eastward_sea_water_velocity", &
      "canonicalUnits:                                            m s-1", &
      "----------------------------------------------------------------", &
      "standardName:                 surface_net_downward_longwave_flux", &
      "canonicalUnits:                                            W m-2", &
      "----------------------------------------------------------------", &
      "standardName:                surface_net_downward_shortwave_flux", &
      "canonicalUnits:                                            W m-2", &
      "----------------------------------------------------------------", &
      "standardName:               surface_northward_sea_water_velocity", &
      "canonicalUnits:                                            m s-1", &
      "----------------------------------------------------------------"/), &
!EOLTCFD
!EOLTFD
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    call NUOPC_FieldDictionaryIngestI(fieldDictionary, freeFormatFD, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

end module
