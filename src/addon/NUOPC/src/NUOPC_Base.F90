! $Id$
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
#define FILENAME "src/addon/NUOPC/src/NUOPC_Base.F90"
!==============================================================================

#define PROFILE_off

module NUOPC_Base

  !-----------------------------------------------------------------------------
  ! Generic code collection
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC_FieldDictionaryApi
  use NUOPC_Auxiliary

  implicit none
  
  private
  
  ! public
  public NUOPC_PhaseMapStringLength       ! parameter
  integer, parameter :: NUOPC_PhaseMapStringLength = 160

  ! public FieldDictionary API
  public NUOPC_FieldDictionary            ! variable
  public NUOPC_FieldDictionaryAddEntry    ! defined in NUOPC_FieldDictionaryApi
  public NUOPC_FieldDictionaryEgest       ! defined in NUOPC_FieldDictionaryApi
  public NUOPC_FieldDictionaryGetEntry    ! defined in NUOPC_FieldDictionaryApi
  public NUOPC_FieldDictionaryHasEntry    ! defined in NUOPC_FieldDictionaryApi
  public NUOPC_FieldDictionaryMatchSyno   ! defined in NUOPC_FieldDictionaryApi
  public NUOPC_FieldDictionarySetSyno     ! defined in NUOPC_FieldDictionaryApi
  public NUOPC_FieldDictionarySetup       ! defined in NUOPC_FieldDictionaryApi
  public NUOPC_FieldDictionarySetAutoAdd  ! defined in NUOPC_FieldDictionaryApi

  ! public Utility API
  public NUOPC_AddNestedState             ! method
  public NUOPC_AddNamespace               ! method
  public NUOPC_AdjustClock                ! method
  public NUOPC_Advertise                  ! method
  public NUOPC_CheckSetClock              ! method
  public NUOPC_GetAttribute               ! method
  public NUOPC_GetStateMemberLists        ! method
  public NUOPC_InitAttributes             ! method
  public NUOPC_IsAtTime                   ! method
  public NUOPC_IsConnected                ! method
  public NUOPC_IsUpdated                  ! method
  public NUOPC_NoOp                       ! method
  public NUOPC_Realize                    ! method
  public NUOPC_Reconcile                  ! method
  public NUOPC_SetAttribute               ! method
  public NUOPC_UpdateTimestamp            ! method

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface NUOPC_Advertise
    module procedure NUOPC_AdvertiseField
    module procedure NUOPC_AdvertiseFields
  end interface
  
  interface NUOPC_GetAttribute
    module procedure NUOPC_GetAttributeFieldVal
    module procedure NUOPC_GetAttributeFieldTK
    module procedure NUOPC_GetAttributeState
  end interface
  
  interface NUOPC_InitAttributes
    module procedure NUOPC_InitAttributesField
    module procedure NUOPC_InitAttributesState
  end interface

  interface NUOPC_IsAtTime
    module procedure NUOPC_IsAtTimeField
    module procedure NUOPC_IsAtTimeState
  end interface

  interface NUOPC_IsConnected
    module procedure NUOPC_IsConnectedField
    module procedure NUOPC_IsConnectedState
  end interface
  
  interface NUOPC_IsUpdated
    module procedure NUOPC_IsUpdatedField
    module procedure NUOPC_IsUpdatedState
  end interface

  interface NUOPC_Realize
    module procedure NUOPC_RealizeCompleteG
    module procedure NUOPC_RealizeCompleteLS
    module procedure NUOPC_RealizeCompleteM
    module procedure NUOPC_RealizeField
  end interface
  
  interface NUOPC_SetAttribute
    module procedure NUOPC_SetAttributeField
    module procedure NUOPC_SetAttributeState
  end interface

  interface NUOPC_UpdateTimestamp
    module procedure NUOPC_UpdateFieldList
    module procedure NUOPC_UpdateAcrossFieldLists
    module procedure NUOPC_FieldBundleUpdateTime
    module procedure NUOPC_StateUpdateTimestamp
    module procedure NUOPC_StateSetTimestamp
  end interface
  
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_AddNamespace - Add a nested state with Namespace to a State
! !INTERFACE:
  subroutine NUOPC_AddNamespace(state, Namespace, nestedStateName, &
    nestedState, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(len=*), intent(in)            :: Namespace
    character(len=*), intent(in),  optional :: nestedStateName
    type(ESMF_State), intent(out), optional :: nestedState
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Add a Namespace to {\tt state}. Namespaces are implemented via nested 
!   states. This creates a nested state inside of {\tt state}. The nested state
!   is returned as {\tt nestedState}. If provided, {\tt nestedStateName} will 
!   be used to name the newly created nested state. The default name of the 
!   nested state is equal to {\tt Namespace}.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object to which the Namespace is added.
!   \item[Namespace]
!     The Namespace string.
!   \item[{[nestedStateName]}]
!     Name of the nested state. Defaults to {\tt Namespace}.
!   \item[{[nestedState]}]
!     Optional return of the newly created nested state.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_State)        :: nestedS
    character(len=80)       :: nestedSName
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(nestedStateName)) then
      nestedSName = trim(nestedStateName)
    else
      nestedSName = trim(Namespace)
    endif
    
    nestedS = ESMF_StateCreate(name=nestedSName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_InitAttributes(nestedS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_SetAttribute(nestedS, name="Namespace", &
      value=trim(Namespace), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    call ESMF_StateAdd(state, (/nestedS/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (present(nestedState)) &
      nestedState = nestedS
    
  end subroutine
  !---------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_AddNestedState - Add a nested state to a state with NUOPC attributes
! !INTERFACE:
  subroutine NUOPC_AddNestedState(state, Namespace, CplSet, nestedStateName, &
    nestedState, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(len=*), intent(in),  optional :: Namespace
    character(len=*), intent(in),  optional :: CplSet
    character(len=*), intent(in),  optional :: nestedStateName
    type(ESMF_State), intent(out), optional :: nestedState
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Create a nested state inside of {\tt state}. The arguments {\tt Namespace}
!   and {tt\ CplSet} are used to set NUOPC attributes on the newly created
!   state. The nested state is returned as {\tt nestedState}. If provided,
!   {\tt nestedStateName} will be used to name the newly created nested state.
!   The default name of the nested state is equal to
!   {\tt Namespace}\_{\tt CplSet}, {\tt Namespace}, or {\tt CplSet} if the
!   arguments are provided.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object to which the namespace is added.
!   \item[Namespace]
!     Optional The Namespace string. Defaults to "\_\_UNSPECIFIED\_\_".
!   \item[CplSet]
!     Optional The CplSet string. Defaults to "\_\_UNSPECIFIED\_\_".
!   \item[{[nestedStateName]}]
!     Name of the nested state. Defaults to {\tt Namespace}\_{\tt CplSet},
!     {\tt Namespace}, or {\tt CplSet} if arguments are provided.
!   \item[{[nestedState]}]
!     Optional return of the newly created nested state.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_State)        :: nestedS
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(nestedStateName)) then
      nestedS = ESMF_StateCreate(name=trim(nestedStateName), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    elseif (present(Namespace)) then
      if (present(CplSet)) then
        nestedS = ESMF_StateCreate(name=trim(Namespace)//"_"//trim(CplSet), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      else
        nestedS = ESMF_StateCreate(name=trim(Namespace), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif
    elseif (present(CplSet)) then
      nestedS = ESMF_StateCreate(name=trim(CplSet), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    else
      nestedS = ESMF_StateCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    
    call NUOPC_InitAttributes(nestedS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (present(Namespace)) then
      call NUOPC_SetAttribute(nestedS, name="Namespace", &
        value=trim(Namespace), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    else
      call NUOPC_SetAttribute(nestedS, name="Namespace", &
        value="__UNSPECIFIED__", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

    if (present(CplSet)) then
      call NUOPC_SetAttribute(nestedS, name="CplSet", &
        value=trim(CplSet), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    else
      call NUOPC_SetAttribute(nestedS, name="CplSet", &
        value="__UNSPECIFIED__", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

    call ESMF_StateAdd(state, (/nestedS/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (present(nestedState)) &
      nestedState = nestedS
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Advertise - Advertise a single Field in a State
! !INTERFACE:
  ! call using generic interface: NUOPC_Advertise
  subroutine NUOPC_AdvertiseField(state, StandardName, Units, &
    LongName, ShortName, name, TransferOfferField, SharePolicyField, &
    TransferOfferGeomObject, SharePolicyGeomObject, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(*),     intent(in)            :: StandardName
    character(*),     intent(in),  optional :: Units
    character(*),     intent(in),  optional :: LongName
    character(*),     intent(in),  optional :: ShortName
    character(*),     intent(in),  optional :: name
    character(*),     intent(in),  optional :: TransferOfferField
    character(*),     intent(in),  optional :: SharePolicyField
    character(*),     intent(in),  optional :: TransferOfferGeomObject
    character(*),     intent(in),  optional :: SharePolicyGeomObject
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   \label{NUOPC_AdvertiseField}
!   Advertise a field in a state. This creates an empty field and adds it to
!   {\tt state}. The "StandardName", "Units", "LongName", "ShortName", and 
!   "TransferOfferGeomObject" attributes of the field are set according to the
!   provided input..
!
!   The call checks the provided information against the NUOPC Field Dictionary
!   to ensure correctness. Defaults are set according to the NUOPC Field 
!   Dictionary.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object through which the field is advertised.
!   \item[StandardName]
!     The "StandardName" attribute of the advertised field. Must be a 
!     StandardName found in the  NUOPC Field Dictionary.
!   \item[{[Units]}]
!     The "Units" attribute of the advertised field. Must be convertible to the
!     canonical units specified in the NUOPC Field Dictionary for the specified
!     StandardName. (Currently this is restricted to be identical to the 
!     canonical untis specified in the NUOPC Field Dictionary.)
!     If omitted, the default is to use the canonical units associated with
!     the StandardName in the NUOPC Field Dictionary.
!   \item[{[LongName]}]
!     The "LongName" attribute of the advertised field. NUOPC does not restrict
!     the value of this attribute.
!     If omitted, the default is to use the StandardName.
!   \item[{[ShortName]}]
!     The "ShortName" attribute of the advertised field. NUOPC does not restrict
!     the value of this attribute.
!     If omitted, the default is to use the StandardName.
!   \item[{[name]}]
!     The actual name of the advertised field by which it is accessed in the
!     state object. NUOPC does not restrict the value of this variable.
!     If omitted, the default is to use the value of the ShortName.
!   \item[{[TransferOfferField]}]
!     The "TransferOfferField" attribute of the advertised field. NUOPC 
!     controls the vocabulary of this attribute. Valid options are 
!     "will provide", "can provide", "cannot provide".
!     If omitted, the default is "will provide".
!   \item[{[SharePolicyField]}]
!     The "SharePolicyField" attribute of the advertised field. NUOPC 
!     controls the vocabulary of this attribute. Valid options are 
!     "share", and "not share".
!     If omitted, the default is "not share".
!   \item[{[TransferOfferGeomObject]}]
!     The "TransferOfferGeomObject" attribute of the advertised field. NUOPC 
!     controls the vocabulary of this attribute. Valid options are 
!     "will provide", "can provide", "cannot provide".
!     If omitted, the default is equal to {\tt TransferOfferField}.
!   \item[{[SharePolicyGeomObject]}]
!     The "SharePolicyGeomObject" attribute of the advertised field. NUOPC 
!     controls the vocabulary of this attribute. Valid options are 
!     "share", and "not share".
!     If omitted, the default is equal to {\tt SharePolicyField}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)        :: field
    character(ESMF_MAXSTR)  :: tempString
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    field = ESMF_FieldEmptyCreate(name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call NUOPC_InitAttributes(field, StandardName=StandardName, &
      Units=Units, LongName=LongName, ShortName=ShortName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (.not.present(name)) then
      ! name was not provided -> default to using ShortName
      call NUOPC_GetAttribute(field, name="ShortName", value=tempString, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_FieldSet(field, name=trim(tempString), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (present(TransferOfferField)) then
      if (trim(TransferOfferField)=="will provide") then
        call NUOPC_SetAttribute(field, name="TransferOfferField", &
          value="will provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      elseif (trim(TransferOfferField)=="can provide") then
        call NUOPC_SetAttribute(field, name="TransferOfferField", &
          value="can provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      elseif (trim(TransferOfferField)=="cannot provide") then
        call NUOPC_SetAttribute(field, name="TransferOfferField", &
          value="cannot provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="must provide a valid string for TransferOfferField", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif
    if (present(SharePolicyField)) then
      if (trim(SharePolicyField)=="share") then
        call NUOPC_SetAttribute(field, name="SharePolicyField", &
          value="share", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      elseif (trim(SharePolicyField)=="not share") then
        call NUOPC_SetAttribute(field, name="SharePolicyField", &
          value="not share", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="must provide a valid string for SharePolicyField", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif
    if (present(TransferOfferGeomObject)) then
      if (trim(TransferOfferGeomObject)=="will provide") then
        call NUOPC_SetAttribute(field, name="TransferOfferGeomObject", &
          value="will provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      elseif (trim(TransferOfferGeomObject)=="can provide") then
        call NUOPC_SetAttribute(field, name="TransferOfferGeomObject", &
          value="can provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      elseif (trim(TransferOfferGeomObject)=="cannot provide") then
        call NUOPC_SetAttribute(field, name="TransferOfferGeomObject", &
          value="cannot provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="must provide a valid string for TransferOfferGeomObject", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
    else
      ! set default for TransferOfferGeomObject
      call NUOPC_GetAttribute(field, name="TransferOfferField", &
        value=tempString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call NUOPC_SetAttribute(field, name="TransferOfferGeomObject", &
        value=tempString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (present(SharePolicyGeomObject)) then
      if (trim(SharePolicyGeomObject)=="share") then
        call NUOPC_SetAttribute(field, name="SharePolicyGeomObject", &
          value="share", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      elseif (trim(SharePolicyGeomObject)=="not share") then
        call NUOPC_SetAttribute(field, name="SharePolicyGeomObject", &
          value="not share", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="must provide a valid string for SharePolicyGeomObject", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
    else
      ! set default for SharePolicyGeomObject
      call NUOPC_GetAttribute(field, name="SharePolicyField", &
        value=tempString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call NUOPC_SetAttribute(field, name="SharePolicyGeomObject", &
        value=tempString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    call ESMF_StateAdd(state, fieldList=(/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Advertise - Advertise a list of Fields in a State
! !INTERFACE:
  ! call using generic interface: NUOPC_Advertise
  subroutine NUOPC_AdvertiseFields(state, StandardNames, &
    TransferOfferField, SharePolicyField, &
    TransferOfferGeomObject, SharePolicyGeomObject, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(*),     intent(in)            :: StandardNames(:)
    character(*),     intent(in),  optional :: TransferOfferField
    character(*),     intent(in),  optional :: SharePolicyField
    character(*),     intent(in),  optional :: TransferOfferGeomObject
    character(*),     intent(in),  optional :: SharePolicyGeomObject
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   \label{NUOPC_AdvertiseFields}
!   Advertise a list of fields in a state. This creates a list of empty fields
!   and adds it to the {\tt state}. The "StandardName", and 
!   "TransferOfferGeomObject" attributes of all the fields are set according to
!   the provided input. The "Units", "LongName", and "ShortName" attributes for
!   each field are set according to the defaults documented under method 
!   \ref{NUOPC_AdvertiseField}
!
!   The call checks the provided information against the NUOPC Field Dictionary
!   to ensure correctness.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object through which the fields are advertised.
!   \item[StandardNames]
!     A list of "StandardName" attributes of the advertised fields. Must be 
!     StandardNames found in the  NUOPC Field Dictionary.
!   \item[{[TransferOfferField]}]
!     The "TransferOfferField" attribute of the advertised fields. This 
!     setting applies to all the fields advertised in this call. NUOPC 
!     controls the vocabulary of this attribute. Valid options are 
!     "will provide", "can provide", "cannot provide".
!     If omitted, the default is "will provide".
!   \item[{[SharePolicyField]}]
!     The "SharePolicyField" attribute of the advertised fields. This 
!     setting applies to all the fields advertised in this call. NUOPC 
!     controls the vocabulary of this attribute. Valid options are 
!     "share", and "not share".
!     If omitted, the default is "not share".
!   \item[{[TransferOfferGeomObject]}]
!     The "TransferOfferGeomObject" attribute of the advertised fields. This 
!     setting applies to all the fields advertised in this call. NUOPC 
!     controls the vocabulary of this attribute. Valid options are 
!     "will provide", "can provide", "cannot provide".
!     If omitted, the default is equal to {\tt TransferOfferField}.
!   \item[{[SharePolicyGeomObject]}]
!     The "SharePolicyGeomObject" attribute of the advertised fields. This 
!     setting applies to all the fields advertised in this call. NUOPC 
!     controls the vocabulary of this attribute. Valid options are 
!     "share", and "not share".
!     If omitted, the default is equal to {\tt SharePolicyField}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                 :: i
    
    if (present(rc)) rc = ESMF_SUCCESS

    do i=1, size(StandardNames)
      call NUOPC_AdvertiseField(state, StandardName=StandardNames(i), &
        TransferOfferField=TransferOfferField, &
        SharePolicyField=SharePolicyField, &
        TransferOfferGeomObject=TransferOfferGeomObject, &
        SharePolicyGeomObject=SharePolicyGeomObject, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_AdjustClock - Adjust the timestep in a clock
! !INTERFACE:
  subroutine NUOPC_AdjustClock(clock, maxTimestep, rc)
! !ARGUMENTS:
    type(ESMF_Clock)                               :: clock
    type(ESMF_TimeInterval), intent(in),  optional :: maxTimestep
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Adjust the {\tt clock} to have a potentially smaller timestep. The timestep
!   on the incoming {\tt clock} object is compared to the {\tt maxTimestep}, and
!   reset to the smaller of the two.
!
!   The arguments are:
!   \begin{description}
!   \item[clock]
!     The clock to be adjusted.
!   \item[{[maxTimestep]}]
!     Upper bound of the timestep allowed in {\tt clock}. 
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Clock)        :: internalClock
    type(ESMF_TimeInterval) :: incomingTimeStep
    type(ESMF_TimeInterval) :: actualTimeStep
    integer                 :: internalStepCount
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(maxTimestep)) then
    
      ! determine the internal timeStep
      ! The incoming (parent) timeStep must be a multiple of the internal
      ! timeStep. At the same time there is typically a physical/stability limit
      ! for the internal timeStep. The following procedure finds an internal
      ! timeStep that is as close as possible to the provided stability limit, 
      ! while <= that limit. At the same time the incoming timeStep is a multiple
      ! of the internal timeStep.
      call ESMF_ClockGet(clock, timeStep=incomingTimeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    
      internalStepCount = ceiling(incomingTimeStep / maxTimestep)
      actualTimeStep = incomingTimeStep / internalStepCount
    
      call ESMF_ClockSet(clock, timeStep=actualTimeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
      
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CheckSetClock - Check a Clock for compatibility and set its values
! !INTERFACE:
  subroutine NUOPC_CheckSetClock(setClock, checkClock, setStartTimeToCurrent, rc)
! !ARGUMENTS:
    type(ESMF_Clock),        intent(inout)         :: setClock
    type(ESMF_Clock),        intent(in)            :: checkClock
    logical,                 intent(in),  optional :: setStartTimeToCurrent
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Compare {\tt setClock} to {\tt checkClock} to ensure they match in
!   their current fime. Further ensure that the timeStep of {\tt checkClock}
!   is a multiple of the timeStep of {\tt setClock}. If both conditions are 
!   satisfied then the stopTime of the {\tt setClock} is set one 
!   {\tt checkClock} timeStep ahead of the current time. The direction of the
!   clock is considered.
!
!   By default the startTime of the {\tt setClock} is not modified. However, if
!   {\tt setStartTimeToCurrent == .true.} the startTime of {\tt setClock} is set
!   to the currentTime of {\tt checkClock}.
!
!   The arguments are:
!   \begin{description}
!   \item[setClock]
!     The {\tt ESMF\_Clock} object to be checked and set.
!   \item[checkClock]
!     The reference clock object.
!   \item[{[setStartTimeToCurrent]}]
!     If {\tt .true.} then also set the startTime in {\tt setClock} according to
!     the startTime in {\tt checkClock}. The default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)           :: checkCurrTime, currTime, stopTime, startTime
    type(ESMF_TimeInterval)   :: checkTimeStep, timeStep
    type(ESMF_Direction_Flag) :: direction
    type(ESMF_Time)           :: setTime

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ClockGet(checkClock, currTime=checkCurrTime, &
      timeStep=checkTimeStep, direction=direction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_ClockGet(setClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! ensure the current times match between checkClock and setClock
    if (currTime /= checkCurrTime) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="setClock and checkClock do not match in current time!", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! ensure that the check timestep is a multiple of the internal one
    if (ceiling(checkTimeStep/timeStep) /= floor(checkTimeStep/timeStep))&
      then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="checkClock timestep is not multiple of setClock timestep!", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! set the new stopTime of the setClock
    if (direction==ESMF_DIRECTION_FORWARD) then
      stopTime = currTime + checkTimeStep
    else
      stopTime = currTime - checkTimeStep
    endif
    call ESMF_ClockSet(setClock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
   ! conditionally set startTime of the setClock
   if (present(setStartTimeToCurrent)) then
      if (setStartTimeToCurrent) then
        call ESMF_ClockGet(checkClock, currTime=setTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        call ESMF_ClockSet(setClock, startTime=setTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GetAttribute - Get the value of a NUOPC Field Attribute
! !INTERFACE:
  ! call using generic interface: NUOPC_GetAttribute
  subroutine NUOPC_GetAttributeFieldVal(field, name, value, rc)
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
    character(*),     intent(in)            :: name
    character(*),     intent(out)           :: value
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Access the attribute {\tt name} inside of {\tt field} using the
!   convention {\tt NUOPC} and purpose {\tt Instance}. Returns with error if
!   the attribute is not present or not set.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to be queried.
!   \item[name]
!     The name of the queried attribute.
!   \item[value]
!     The value of the queried attribute.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    defaultvalue = "CheckThisDefaultValue"

    call ESMF_AttributeGet(field, name=name, value=value, &
      defaultvalue=defaultvalue, convention="NUOPC", purpose="Instance", &
      attnestflag=ESMF_ATTNEST_ON, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (trim(value) == trim(defaultvalue)) then
      ! attribute not present
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not present",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    else if (len_trim(value) == 0) then
      ! attribute present but not set
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not set",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GetAttribute - Get the typekind of a NUOPC Field Attribute
! !INTERFACE:
  ! call using generic interface: NUOPC_GetAttribute
  subroutine NUOPC_GetAttributeFieldTK(field, name, typekind, rc)
! !ARGUMENTS:
    type(ESMF_Field),         intent(in)            :: field
    character(*),             intent(in)            :: name
    type(ESMF_TypeKind_Flag), intent(out)           :: typekind
    integer,                  intent(out), optional :: rc
! !DESCRIPTION:
!   Query the {\tt typekind} of the attribute {\tt name} inside of {\tt field}
!   using the convention {\tt NUOPC} and purpose {\tt Instance}. Returns with 
!   error if the attribute is not present or not set.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to be queried.
!   \item[name]
!     The name of the queried attribute.
!   \item[typekind]
!     The typekind of the queried attribute.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeGet(field, name=name, typekind=typekind, &
      convention="NUOPC", purpose="Instance", &
      attnestflag=ESMF_ATTNEST_ON, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GetAttribute - Get the value of a NUOPC State Attribute
! !INTERFACE:
  ! call using generic interface: NUOPC_GetAttribute
  subroutine NUOPC_GetAttributeState(state, name, value, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    character(*),     intent(in)            :: name
    character(*),     intent(out)           :: value
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Access the attribute {\tt name} inside of {\tt state} using the
!   convention {\tt NUOPC} and purpose {\tt Instance}. Returns with error if
!   the attribute is not present or not set.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object to be queried.
!   \item[name]
!     The name of the queried attribute.
!   \item[value]
!     The value of the queried attribute.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    defaultvalue = "CheckThisDefaultValue"

    call ESMF_AttributeGet(state, name=name, value=value, &
      defaultvalue=defaultvalue, &
      convention="NUOPC", purpose="Instance", &
      attnestflag=ESMF_ATTNEST_ON, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (trim(value) == trim(defaultvalue)) then
      ! attribute not present
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not present",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    else if (len_trim(value) == 0) then
      ! attribute present but not set
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not set",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GetStateMemberLists - Build lists of information of State members
! !INTERFACE:
  recursive subroutine NUOPC_GetStateMemberLists(state, StandardNameList, &
    ConnectedList, NamespaceList, CplSetList, itemNameList, fieldList, rc)
! !ARGUMENTS:
    type(ESMF_State),       intent(in)            :: state
    character(ESMF_MAXSTR), pointer, optional     :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: ConnectedList(:)
    character(ESMF_MAXSTR), pointer, optional     :: NamespaceList(:)
    character(ESMF_MAXSTR), pointer, optional     :: CplSetList(:)
    character(ESMF_MAXSTR), pointer, optional     :: itemNameList(:)
    type(ESMF_Field),       pointer, optional     :: fieldList(:)
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Construct lists containing the StandardNames, field names, and connected 
!   status of the fields in {\tt state}. Return this information in the
!   list arguments. Recursively parse through nested States.
!
!   All pointer arguments present must enter this method unassociated. On 
!   return, the deallocation of an associated pointer becomes the responsibility
!   of the caller.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object to be queried.
!   \item[{[StandardNameList]}]
!     If present, return a list of the "StandardName" attribute of each member.
!   \item[{[ConnectedList]}]
!     If present, return a list of the "Connected" attribute of each member.
!   \item[{[NamespaceList]}]
!     If present, return a list of the "Namespace" attribute of each member.
!   \item[{[CplSetList]}]
!     If present, return a list of the "CplSet" attribute of each member.
!   \item[{[itemNameList]}]
!     If present, return a list of each member name.
!   \item[{[fieldList]}]
!     If present, return a list of the member fields.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer           :: item, itemCount, fieldCount, stat, i
    type(ESMF_Field)  :: field
    character(ESMF_MAXSTR), allocatable     :: ll_itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: stateitemtypeList(:)
    type(ESMF_State)                        :: nestedState
    character(ESMF_MAXSTR), pointer         :: l_StandardNameList(:)
    character(ESMF_MAXSTR), pointer         :: l_itemNameList(:)
    character(ESMF_MAXSTR), pointer         :: l_ConnectedList(:)
    character(ESMF_MAXSTR), pointer         :: l_NamespaceList(:)
    character(ESMF_MAXSTR), pointer         :: l_CplSetList(:)
    type(ESMF_Field),       pointer         :: l_fieldList(:)
    character(ESMF_MAXSTR)                  :: namespace
    character(ESMF_MAXSTR)                  :: cplSet

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
          
    if (itemCount > 0) then
      allocate(ll_itemNameList(itemCount))
      allocate(stateitemtypeList(itemCount))
      call ESMF_StateGet(state, itemNameList=ll_itemNameList, &
        itemtypeList=stateitemtypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      fieldCount = 0  ! reset
      do item=1, itemCount
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
          fieldCount = fieldCount + 1
        else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
          ! recursively parse the nested state
          nullify(l_StandardNameList)
          call ESMF_StateGet(state, itemName=ll_itemNameList(item), &
            nestedState=nestedState, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          call NUOPC_GetStateMemberLists(nestedState, l_StandardNameList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (associated(l_StandardNameList)) then
            fieldCount = fieldCount + size(l_StandardNameList)
            deallocate(l_StandardNameList)
          endif
        endif
      enddo
      
      if (present(StandardNameList)) then
        if (associated(StandardNameList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="StandardNameList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(StandardNameList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating StandardNameList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif
      
      if (present(itemNameList)) then
        if (associated(itemNameList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="itemNameList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(itemNameList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating itemNameList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif

      if (present(ConnectedList)) then
        if (associated(ConnectedList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="ConnectedList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(ConnectedList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating ConnectedList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif

      if (present(NamespaceList)) then
        if (associated(NamespaceList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="NamespaceList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(NamespaceList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating NamespaceList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif

      if (present(CplSetList)) then
        if (associated(CplSetList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="CplSetList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(CplSetList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating CplSetList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif

      if (present(fieldList)) then
        if (associated(fieldList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="fieldList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(fieldList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating fieldList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif

      fieldCount = 1  ! reset

      do item=1, itemCount
        call NUOPC_GetAttribute(state, name="Namespace", value=namespace, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        call NUOPC_GetAttribute(state, name="CplSet", value=cplSet, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, itemName=ll_itemNameList(item), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (present(StandardNameList)) then
            call NUOPC_GetAttribute(field, name="StandardName", &
              value=StandardNameList(fieldCount), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          endif
          if (present(itemNameList)) then
            itemNameList(fieldCount)=ll_itemNameList(item)
          endif
          if (present(ConnectedList)) then
            call NUOPC_GetAttribute(field, name="Connected", &
              value=ConnectedList(fieldCount), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          endif
          if (present(NamespaceList)) then
            NamespaceList(fieldCount)=trim(namespace)
          endif
          if (present(CplSetList)) then
            CplSetList(fieldCount)=trim(cplSet)
          endif
          if (present(fieldList)) then
            fieldList(fieldCount)=field
          endif
          fieldCount = fieldCount + 1
        else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
          ! recursively parse the nested state
          nullify(l_StandardNameList)
          nullify(l_itemNameList)
          nullify(l_ConnectedList)
          nullify(l_NamespaceList)
          nullify(l_CplSetList)
          nullify(l_fieldList)
          call ESMF_StateGet(state, itemName=ll_itemNameList(item), &
            nestedState=nestedState, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          call NUOPC_GetStateMemberLists(nestedState, &
            StandardNameList=l_StandardNameList, &
            itemNameList=l_itemNameList, &
            ConnectedList=l_ConnectedList, &
            NamespaceList=l_NamespaceList, &
            CplSetList=l_CplSetList, &
            fieldList=l_fieldList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (associated(l_StandardNameList)) then
            do i=1, size(l_StandardNameList)
              if (present(StandardNameList)) then
                StandardNameList(fieldCount) = l_StandardNameList(i)
              endif
              if (present(itemNameList)) then
                itemNameList(fieldCount) = l_itemNameList(i)
              endif
              if (present(ConnectedList)) then
                ConnectedList(fieldCount) = l_ConnectedList(i)
              endif
              if (present(NamespaceList)) then
                if (trim(l_NamespaceList(i)).EQ."__UNSPECIFIED__") then
                  NamespaceList(fieldCount) = trim(namespace)
                else
                  NamespaceList(fieldCount) = trim(namespace)//":"// &
                    trim(l_NamespaceList(i))
                endif
              endif
              if (present(CplSetList)) then
                CplSetList(fieldCount) = l_CplSetList(i)
              endif
              if (present(fieldList)) then
                fieldList(fieldCount) = l_fieldList(i)
              endif
              fieldCount = fieldCount + 1
            enddo
            deallocate(l_StandardNameList)
            deallocate(l_itemNameList)
            deallocate(l_ConnectedList)
            deallocate(l_NamespaceList)
            deallocate(l_CplSetList)
            deallocate(l_fieldList)
          endif
        endif
      enddo
        
      deallocate(ll_itemNameList)
      deallocate(stateitemtypeList)
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_InitAttributes - Initialize the NUOPC Field Attributes
! !INTERFACE:
  ! call using generic interface: NUOPC_InitAttributes
  subroutine NUOPC_InitAttributesField(field, StandardName, Units, LongName, &
    ShortName, Connected, rc)
! !ARGUMENTS:
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: StandardName
    character(*), intent(in),  optional   :: Units
    character(*), intent(in),  optional   :: LongName
    character(*), intent(in),  optional   :: ShortName
    character(*), intent(in),  optional   :: Connected
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Add standard NUOPC Attributes to a Field object. Checks the provided
!   arguments against the NUOPC Field Dictionary. Omitted optional
!   information is filled in using defaults.
!
!   Add the standard NUOPC Field AttPack hierarchy to the Field.
!
!   The highest level in the AttPack hierarchy will have convention="NUOPC" and
!   purpose="Instance".
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to which the Attributes are added.
!   \item[StandardName]
!     The StandardName of the Field. Must be a StandardName found in
!     the  NUOPC Field Dictionary.
!   \item[{[Units]}]
!     The Units of the Field. Must be convertible to the canonical
!     units specified in the NUOPC Field Dictionary for the specified
!     StandardName.
!     If omitted, the default is to use the canonical units associated with
!     the StandardName in the NUOPC Field Dictionary.
!   \item[{[LongName]}]
!     The LongName of the Field. NUOPC does not restrict the value
!     of this variable.
!     If omitted, the default is to use the StandardName.
!   \item[{[ShortName]}]
!     The ShortName of the Field. NUOPC does not restrict the value
!     of this variable.
!     If omitted, the default is to use the StandardName.
!   \item[{[Connected]}]
!     The connection status of the Field. Must be one of the NUOPC supported
!     values: {\tt false} or {\tt true}.
!     If omitted, the default is a connected status of {\tt false}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)            :: attrList(19)
    character(ESMF_MAXSTR)            :: tempString
    logical                           :: accepted
    integer                           :: i
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be added to the Fields
    attrList(1) = "Connected"  ! values: "true" or "false"
    attrList(2) = "TimeStamp"  ! values: list of 9 integers: yy,mm,dd,h,m,s,ms,us,ns
    attrList(3) = "ProducerConnection"! values: "open", "targeted", "connected"
    attrList(4) = "ConsumerConnection"! values: "open", "targeted", "connected"
    attrList(5) = "Updated" ! values: "true" or "false"
    attrList(6) = "TransferOfferGeomObject" ! values: "cannot provide",
                                            !   "can provide", "will provide"
    attrList(7) = "TransferActionGeomObject"! values: "provide", "accept"
    attrList(8) = "SharePolicyGeomObject"   ! values: "share", "not share"
    attrList(9) = "ShareStatusGeomObject"   ! values: "shared", "not shared"
    attrList(10)= "TransferOfferField"      ! values: "cannot provide",
                                            !   "can provide", "will provide"
    attrList(11)= "TransferActionField"     ! values: "provide", "accept"
    attrList(12)= "SharePolicyField"        ! values: "share", "not share"
    attrList(13)= "ShareStatusField"        ! values: "shared", "not shared"
    attrList(14)= "UngriddedLBound"
    attrList(15)= "UngriddedUBound"
    attrList(16)= "GridToFieldMap"
    attrList(17)= "ArbDimCount"
    attrList(18)= "MinIndex"
    attrList(19)= "MaxIndex"
    
    ! add Attribute packages
    call ESMF_AttributeAdd(field, convention="ESG", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeAdd(field, convention="NUOPC", purpose="Instance",   &
      attrList=attrList, nestConvention="ESG", nestPurpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! Attributes don't offer controlled vocabulary checking (yet) -> do it here!
    ! first ensure that NUOPC_FieldDictionary is set up
    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! check that StandardName has an entry in the NUOPC_FieldDictionary
    call ESMF_ContainerGet(NUOPC_FieldDictionary, itemName=trim(StandardName), &
      isPresent=accepted, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (.not.accepted) then
      if (NUOPC_FieldDictionaryAutoAdd) then
        call NUOPC_FieldDictionaryAddEntry(standardName=trim(StandardName), &
          canonicalUnits="unknown - Autogenerated Entry", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=trim(StandardName)//" is not a StandardName in the NUOPC_FieldDictionary!",&
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
    endif
    call ESMF_ContainerGetUDT(NUOPC_FieldDictionary, trim(StandardName), &
      fdEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! set StandardName
    call ESMF_AttributeSet(field, &
      name="StandardName", value=trim(StandardName), &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! set Units
    if (present(Units)) then
      if ((trim(Units))/=trim(fdEntry%wrap%canonicalUnits)) then
        ! not the same as canoncial units
        accepted = .false. ! reset
        ! TODO: implement access to UDUNITS-2 to figure if Units can be 
        ! TODO: converted to the canonicalUnits, if so then o.k.
        if (.not.accepted) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg=Units//" cannot be converted to the canonical units in "// &
              " NUOPC_FieldDictionary for StandardName: "//StandardName,&
              line=__LINE__, file=FILENAME, rcToReturn=rc)
          return  ! bail out
        endif
      endif
      tempString = Units
    else
      tempString = fdEntry%wrap%canonicalUnits  ! default
    endif
    call ESMF_AttributeSet(field, &
      name="Units", value=trim(tempString), &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set LongName
    if (present(LongName)) then
      tempString = trim(LongName)
    else
      tempString = trim(StandardName)   ! default
    endif
    call ESMF_AttributeSet(field, &
      name="LongName", value=trim(tempString), &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set ShortName
    if (present(ShortName)) then
      tempString = trim(ShortName)
    else
      tempString = trim(StandardName)   ! default
    endif
    call ESMF_AttributeSet(field, &
      name="ShortName", value=trim(tempString), &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set Connected
    if (present(Connected)) then
      accepted = .false. ! reset
      do i=1, size(fdEntry%wrap%connectedOptions)
        if ((trim(Connected))==trim(fdEntry%wrap%connectedOptions(i))) then
          accepted = .true.
          exit
        endif
      enddo
      if (.not.accepted) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=Connected//" is not a supported Connected value in the "// &
            " NUOPC_FieldDictionary for StandardName: "//StandardName,&
            line=__LINE__, file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      tempString = Connected
    else
      tempString = fdEntry%wrap%connectedOptions(1)  ! default
    endif
    call ESMF_AttributeSet(field, &
      name="Connected", value=trim(tempString), &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set TimeStamp
    call ESMF_AttributeSet(field, &
      name="TimeStamp", valueList=(/0,0,0,0,0,0,0,0,0/), &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set ProducerConnection
    call ESMF_AttributeSet(field, &
      name="ProducerConnection", value="open", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set ConsumerConnection
    call ESMF_AttributeSet(field, &
      name="ConsumerConnection", value="open", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set Updated
    call ESMF_AttributeSet(field, &
      name="Updated", value="false", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set TransferOfferField
    call ESMF_AttributeSet(field, &
      name="TransferOfferField", value="will provide", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set TransferActionField
    call ESMF_AttributeSet(field, &
      name="TransferActionField", value="provide", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set SharePolicyField
    call ESMF_AttributeSet(field, &
      name="SharePolicyField", value="not share", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set ShareStatusField
    call ESMF_AttributeSet(field, &
      name="ShareStatusField", value="not shared", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set TransferOfferGeomObject
    call ESMF_AttributeSet(field, &
      name="TransferOfferGeomObject", value="_undefined", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set TransferActionGeomObject
    call ESMF_AttributeSet(field, &
      name="TransferActionGeomObject", value="_undefined", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set SharePolicyGeomObject
    call ESMF_AttributeSet(field, &
      name="SharePolicyGeomObject", value="not shared", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set ShareStatusGeomObject
    call ESMF_AttributeSet(field, &
      name="ShareStatusGeomObject", value="not shared", &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_InitAttribute - Initialize the NUOPC State Attributes
! !INTERFACE:
  ! call using generic interface: NUOPC_InitAttributes
  subroutine NUOPC_InitAttributesState(state, rc)
! !ARGUMENTS:
    type(ESMF_state)                      :: state
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Add the standard NUOPC State AttPack hierarchy to the State.
!
!   The highest level in the AttPack hierarchy will have convention="NUOPC" and
!   purpose="Instance".
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)            :: attrList(3)
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be added to the Fields
    attrList(1) = "Namespace"           ! namespace of this State
    attrList(2) = "FieldTransferPolicy" ! indicates to connectors to transfer/mirror fields:
                                        !    one of transferNone, transferAll
    attrList(3) = "CplSet"              ! coupling set identifier of this state
    
    ! add Attribute packages
    call ESMF_AttributeAdd(state, convention="NUOPC", purpose="Instance", &
      attrList=attrList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set Attributes to defaults
    call ESMF_AttributeSet(state, attrList(1), "__UNSPECIFIED__", &
        convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeSet(state, attrList(2), "transferNone", &
        convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeSet(state, attrList(3), "__UNSPECIFIED__", &
        convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsAtTime - Check if a Field is at the given Time
! !INTERFACE:
  ! call using generic interface: NUOPC_IsAtTime
  function NUOPC_IsAtTimeField(field, time, rc)
! !RETURN VALUE:
    logical :: NUOPC_IsAtTimeField
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
    type(ESMF_Time),  intent(in)            :: time
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if {\tt field} has a timestamp attribute
!   that matches {\tt time}. Otherwise returns {\tt .false.}.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to be checked.
!   \item[time]
!     The time to compare against.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)         :: fieldTime
    integer                 :: i, valueList(9)
    type(ESMF_CalKind_Flag) :: calkindflag
#ifdef DEBUG
    character(ESMF_MAXSTR)  :: msgString
#endif

    if (present(rc)) rc = ESMF_SUCCESS
    
    NUOPC_IsAtTimeField = .true. ! initialize
    
    call ESMF_TimeGet(time, calkindflag=calkindflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_AttributeGet(field, &
      name="TimeStamp", valueList=valueList, &
      convention="NUOPC", purpose="Instance", &
      attnestflag=ESMF_ATTNEST_ON, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ValueList(2)==0) then
      ! month value of 0 is indicative of an uninitialized timestamp
      NUOPC_IsAtTimeField = .false.
#ifdef DEBUG
      write (msgString,*) "NUOPC_IsAtTimeField() uninitialized time detected: "
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_WARNING)
      write (msgString,*) "field time:  ", valueList
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_WARNING)
#endif
      return
    else
      call ESMF_TimeSet(fieldTime, &
        yy=valueList(1), mm=ValueList(2), dd=ValueList(3), &
         h=valueList(4),  m=ValueList(5),  s=ValueList(6), &
        ms=valueList(7), us=ValueList(8), ns=ValueList(9), &
        calkindflag=calkindflag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (fieldTime /= time) then
        NUOPC_IsAtTimeField = .false.
#ifdef DEBUG
        write (msgString,*) "NUOPC_IsAtTimeField() time mismatch detected: "
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_WARNING)
        write (msgString,*) "field time:  ", valueList
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_WARNING)
        call ESMF_TimeGet(time, &
          yy=valueList(1), mm=ValueList(2), dd=ValueList(3), &
           h=valueList(4),  m=ValueList(5),  s=ValueList(6), &
          ms=valueList(7), us=ValueList(8), ns=ValueList(9), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        write (msgString,*) "target time: ", valueList
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_WARNING)
#endif
        return
      endif
    endif

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsAtTime - Check if Field(s) in a State are at the given Time
! !INTERFACE:
  ! call using generic interface: NUOPC_IsAtTime
  function NUOPC_IsAtTimeState(state, time, fieldName, count, fieldList, rc)
! !RETURN VALUE:
    logical :: NUOPC_IsAtTimeState
! !ARGUMENTS:
    type(ESMF_State),              intent(in)            :: state
    type(ESMF_Time),               intent(in)            :: time
    character(*),                  intent(in),  optional :: fieldName
    integer,                       intent(out), optional :: count
    type(ESMF_Field), allocatable, intent(out), optional :: fieldList(:)
    integer,                       intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the field(s) in {\tt state} have a timestamp 
!   attribute that matches {\tt time}. Otherwise return {\tt .false.}.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object to be checked.
!   \item[time]
!     The time to compare against.
!   \item[{[fieldName]}]
!     The name of the field in {\tt state} to be checked. If provided, and 
!     the state does not contain a field with {\tt fieldName}, return an 
!     error in {\tt rc}. If not provided, check {\em all} the fields contained
!     in {\tt state} and return {\tt .true.} if all the fields are at the 
!     correct time.
!   \item[{[count]}]
!     If provided, the number of fields that are at {\tt time} are returned. If 
!     {\tt fieldName} is present then {\tt count} cannot be greater than 1.
!   \item[{[fieldList]}]
!     If provided, the fields that are {\em not} at {\tt time} are returned. If 
!     {\tt fieldName} is present then {\tt fieldList} can contain a maximum of
!     1 field.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer       :: itemNameList(:)
    type(ESMF_Field),       pointer       :: allFieldList(:)
    type(ESMF_Field)                      :: field
    logical                               :: isAtTime
    integer                               :: i, j
    character(ESMF_MAXSTR)                :: iString, msgString
    integer, allocatable                  :: fieldIndexList(:)
    
    if (present(rc)) rc = ESMF_SUCCESS
    if (present(count)) count = 0
    
    NUOPC_IsAtTimeState = .false.  ! initialize

    if (present(fieldName)) then
    
      call ESMF_StateGet(state, itemName=fieldName, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      NUOPC_IsAtTimeState = NUOPC_IsAtTime(field, time, rc=rc)
      
      if (NUOPC_IsAtTimeState.and.present(count)) count = 1
    
    else

      NUOPC_IsAtTimeState = .true.  ! initialize
      
      nullify(StandardNameList)
      nullify(itemNameList)
      nullify(allFieldList)

      call NUOPC_GetStateMemberLists(state, StandardNameList=StandardNameList, &
        itemNameList=itemNameList, fieldList=allFieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      if (associated(itemNameList)) then
        if (present(fieldList)) allocate(fieldIndexList(size(itemNameList)))
        j=1
        do i=1, size(itemNameList)
          write (iString, *) i
          write (msgString, *) "Failure in NUOPC_IsAtTimeState() for item "// &
            trim(adjustl(iString))//": "//trim(itemNameList(i))
          field = allFieldList(i)
          isAtTime = NUOPC_IsAtTime(field, time, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=msgString, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (.not.isAtTime) then
            NUOPC_IsAtTimeState = .false.
            ! no need to keep going if first true/false is all that matters
            if (.not.present(count) .and. .not.present(fieldList)) exit
            if (present(fieldList)) then
              fieldIndexList(j)=i ! record the field index
              j=j+1
            endif
          elseif (present(count)) then
            count = count + 1
          endif
        enddo
        if (present(fieldList)) then
          allocate(fieldList(j-1))
          do i=1, j-1
            fieldList(i)=allFieldList(fieldIndexList(i))
          enddo
          deallocate(fieldIndexList)
        endif
      endif
      
      if (associated(StandardNameList)) deallocate(StandardNameList)
      if (associated(itemNameList)) deallocate(itemNameList)
      if (associated(allFieldList)) deallocate(allFieldList)
    endif
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsConnected - Check if a Field is connected
! !INTERFACE:
  ! call using generic interface: NUOPC_IsConnected
  function NUOPC_IsConnectedField(field, rc)
! !RETURN VALUE:
    logical :: NUOPC_IsConnectedField
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt field} is connected.
!   Otherwise return {\tt .false.}.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to be checked.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)            :: connectedValue

    if (present(rc)) rc = ESMF_SUCCESS
    
    NUOPC_IsConnectedField = .false. ! initialize

    call NUOPC_GetAttribute(field, name="Connected", &
      value=connectedValue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    if (connectedValue=="true") then
      NUOPC_IsConnectedField = .true.
    endif

  end function
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsConnected - Check if Field(s) in a State are connected
! !INTERFACE:
  ! call using generic interface: NUOPC_IsConnected
  function NUOPC_IsConnectedState(state, fieldName, count, rc)
! !RETURN VALUE:
    logical :: NUOPC_IsConnectedState
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    character(*),     intent(in),  optional :: fieldName
    integer,          intent(out), optional :: count
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the field(s) in {\tt state} are connected. Otherwise
!   return {\tt .false.}.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object to be checked.
!   \item[{[fieldName]}]
!     The name of the field in {\tt state} to be checked. If provided, and 
!     the state does not contain a field with {\tt fieldName}, return an 
!     error in {\tt rc}. If not provided, check {\em all} the fields contained
!     in {\tt state} and return {\tt .true.} if all the fields are connected.
!   \item[{[count]}]
!     If provided, the number of fields that are connected are returned. If 
!     {\tt fieldName} is present then {\tt count} cannot be greater than 1.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)                  :: field
    character(ESMF_MAXSTR), pointer   :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer   :: ConnectedList(:)
    logical                           :: allConnected
    logical                           :: isConnected
    integer                           :: i

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(count)) count = 0

    NUOPC_IsConnectedState = .false.  ! initialize

    if (present(fieldName)) then
    
      call ESMF_StateGet(state, itemName=fieldName, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      NUOPC_IsConnectedState = NUOPC_IsConnected(field, rc=rc)
    
      if (NUOPC_IsConnectedState.and.present(count)) count = 1

    else
    
      nullify(StandardNameList)
      nullify(ConnectedList)

      call NUOPC_GetStateMemberLists(state, StandardNameList=StandardNameList, &
        ConnectedList=ConnectedList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      allConnected = .true.  ! initialize
      if (associated(ConnectedList)) then
        do i=1, size(ConnectedList)
          isConnected = (ConnectedList(i) == "true")
          if (.not.isConnected) then
            allConnected = .false.
            if (.not.present(count)) exit ! no need to keep going
          elseif (present(count)) then
            count = count + 1
          endif
        enddo
      endif

      if (associated(StandardNameList)) deallocate(StandardNameList)
      if (associated(ConnectedList)) deallocate(ConnectedList)

      NUOPC_IsConnectedState = allConnected
      
    endif

  end function
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsUpdated - Check if a Field is marked as updated
! !INTERFACE:
  ! call using generic interface: NUOPC_IsUpdated
  function NUOPC_IsUpdatedField(field, rc)
! !RETURN VALUE:
    logical :: NUOPC_IsUpdatedField
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the field has its "Updated"
!   attribute set to "true". Otherwise return {\tt .false.}. 
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to be checked.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)                :: value

    NUOPC_IsUpdatedField = .false.  ! initialize

    call ESMF_AttributeGet(field, name="Updated", value=value, &
      convention="NUOPC", purpose="Instance", &
      attnestflag=ESMF_ATTNEST_ON, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (trim(value)=="true") then
      NUOPC_IsUpdatedField = .true. ! toggle
    else
      NUOPC_IsUpdatedField = .false. ! toggle
    endif
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsUpdated - Check if Field(s) in a State are marked as updated
! !INTERFACE:
  ! call using generic interface: NUOPC_IsUpdated
  function NUOPC_IsUpdatedState(state, fieldName, count, rc)
! !RETURN VALUE:
    logical :: NUOPC_IsUpdatedState
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    character(*),     intent(in),  optional :: fieldName
    integer,          intent(out), optional :: count
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the field(s) in {\tt state} have the "Updated"
!   attribute set to "true". Otherwise return {\tt .false.}. 
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object to be checked.
!   \item[{[fieldName]}]
!     The name of the field in {\tt state} to be checked. If provided, and 
!     the state does not contain a field with {\tt fieldName}, return an 
!     error in {\tt rc}. If not provided, check {\em all} the fields contained
!     in {\tt state} and return {\tt .true.} if all the fields are updated.
!   \item[{[count]}]
!     If provided, the number of fields that are updated are returned. If 
!     {\tt fieldName} is present then {\tt count} cannot be greater than 1.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer       :: itemNameList(:)
    type(ESMF_Field),       pointer       :: fieldList(:)
    type(ESMF_Field)                      :: field
    logical                               :: isUpdated
    integer                 :: i
    character(ESMF_MAXSTR)  :: iString, msgString
    
    if (present(rc)) rc = ESMF_SUCCESS
    if (present(count)) count = 0

    NUOPC_IsUpdatedState = .false.  ! initialize
    
    if (present(fieldName)) then
    
      call ESMF_StateGet(state, itemName=fieldName, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      NUOPC_IsUpdatedState = NUOPC_IsUpdated(field, rc=rc)
    
      if (NUOPC_IsUpdatedState.and.present(count)) count = 1

    else
    
      nullify(StandardNameList)
      nullify(itemNameList)
      nullify(fieldList)
      
      if (present(count)) count = 0 ! reset
      
      NUOPC_IsUpdatedState = .true. ! initialize 

      call NUOPC_GetStateMemberLists(state, StandardNameList=StandardNameList, &
        itemNameList=itemNameList, fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
        
      if (associated(itemNameList)) then
        do i=1, size(itemNameList)
          write (iString, *) i
          write (msgString, *) "Failure in NUOPC_IsUpdatedState() for item "// &
            trim(adjustl(iString))//": "//trim(itemNameList(i))
          field=fieldList(i)
          isUpdated = NUOPC_IsUpdated(field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) return  ! bail out
          if (present(count) .and. isUpdated) then
            count = count + 1
          else if (.not.isUpdated) then
            NUOPC_IsUpdatedState = .false. ! toggle
            if (.not.present(count)) exit ! no need to continue looking
          endif
        enddo
      endif
      
      if (associated(StandardNameList)) deallocate(StandardNameList)
      if (associated(itemNameList)) deallocate(itemNameList)
      if (associated(fieldList)) deallocate(fieldList)
      
    endif
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_NoOp - No-Operation attachable method for GridComp
! !INTERFACE:
  subroutine NUOPC_NoOp(gcomp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
! !DESCRIPTION:
!   No-Op method with an interface that matches the
!   requirements for a attachable method for ESMF\_GridComp objects.
!
!   The arguments are:
!   \begin{description}
!   \item[gcomp]
!     The {\tt ESMF\_GridComp} object to which this method is attached.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    rc = ESMF_SUCCESS
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Realize - Realize previously advertised Fields inside a State on a single Grid with internal allocation
! !INTERFACE:
  ! call using generic interface: NUOPC_Realize
  subroutine NUOPC_RealizeCompleteG(state, grid, fieldName, typekind, selection,&
    dataFillScheme, rc)
! !ARGUMENTS:
    type(ESMF_State)                                :: state
    type(ESMF_Grid),          intent(in)            :: grid
    character(*),             intent(in),  optional :: fieldName
    type(ESMF_TypeKind_Flag), intent(in),  optional :: typekind
    character(len=*),         intent(in),  optional :: selection
    character(len=*),         intent(in),  optional :: dataFillScheme    
    integer,                  intent(out), optional :: rc
! !DESCRIPTION:
!   \label{NUOPC_RealizeComplete}
!
!   Realize or remove fields inside of {\tt state} according to {\tt selection}.
!   All of the fields that are realized are created internally on the same 
!   {\tt grid} object, allocating memory for as many field dimensions as there 
!   are grid dimensions.
!
!   The type and kind of the created fields is according to argument 
!   {\tt typekind}.
!
!   Realized fields are filled with data according to the {\tt dataFillScheme}
!   argument.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object in which the fields are realized.
!   \item[grid]
!     The {\tt ESMF\_Grid} object on which to realize the fields.
!   \item[{[fieldName]}]
!     The name of the field in {\tt state} to be realized, or removed, according
!     to {\tt selection}. If provided, and the state does not contain a field
!     with name {\tt fieldName}, return an error in {\tt rc}. If not provided,
!     realize {\em all} the fields contained in {\tt state} according to 
!     {\tt selection}.
!   \item[{[typekind]}]
!     The typekind of the internally created field(s). The valid options are
!     {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!     {\tt ESMF\_TYPEKIND\_R4}, and {\tt ESMF\_TYPEKIND\_R8} (default).
!   \item[{[selection]}]
!     Selection of mode of operation:
!     \begin{itemize}
!     \item {\tt "realize\_all"} (default)
!     \item {\tt "realize\_connected\_remove\_others"}
!     \item {\tt "realize\_connected+provide\_remove\_others"}
!     \end{itemize}
!   \item[{[dataFillScheme]}]
!     Realized fields will be filled according to the selected fill
!     scheme. See \ref{NUOPC_FillField} for fill schemes. Default is to leave
!     the data in realized fields uninitialized.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=80), allocatable  :: fieldNameList(:)
    integer                         :: i, itemCount, k
    type(ESMF_Field)                :: field
    character(len=80)               :: selectionOpt
    type(ESMF_TypeKind_Flag)        :: typekindOpt
    character(len=80)               :: value

    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(fieldName)) then
      ! fieldName provided -> construct a fieldNameList with a single element
      itemCount=1
      allocate(fieldNameList(itemCount))
      fieldNameList(1)=trim(fieldName)
    else
      ! query the entire fieldNameList from state
      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out
      allocate(fieldNameList(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out
    endif
    
    ! optional selection argument
    if (present(selection)) then
      selectionOpt=trim(selection)
    else
      selectionOpt="realize_all"
    endif
    
    ! optional typekind argument
    if (present(typekind)) then
      typekindOpt=typekind
    else
      typekindOpt=ESMF_TYPEKIND_R8
    endif

    k=1 ! initialize
    do i=1, itemCount
      if (trim(selectionOpt)=="realize_all") then
        ! create a Field
        field = ESMF_FieldCreate(grid, typekindOpt, &
          name=fieldNameList(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) &
          return  ! bail out
        ! realize the connected Field using the just created Field
        call NUOPC_Realize(state, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) &
          return  ! bail out
        if (present(dataFillScheme)) then
          ! a data fill scheme was provided -> use it to initialize
          call ESMF_FieldFill(field, dataFillScheme=dataFillScheme, member=k, &
            step=0, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          k=k+1 ! increment the member counter
        endif
      else if (trim(selectionOpt)=="realize_connected_remove_others") then
        if (NUOPC_IsConnected(state, fieldName=fieldNameList(i))) then
          ! create a Field
          field = ESMF_FieldCreate(grid, typekindOpt, &
            name=fieldNameList(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          ! realize the connected Field using the just created Field
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          if (present(dataFillScheme)) then
            ! a data fill scheme was provided -> use it to initialize
            call ESMF_FieldFill(field, dataFillScheme=dataFillScheme, member=k, step=0, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME)) &
              return  ! bail out
            k=k+1 ! increment the member counter
          endif
        else
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(i)/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
        endif
      else if (trim(selectionOpt)=="realize_connected+provide_remove_others") then
        if (NUOPC_IsConnected(state, fieldName=fieldNameList(i))) then
          call ESMF_StateGet(state, itemName=fieldNameList(i), field=field, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
#if 0
          call NUOPC_GetAttribute(field, name="TransferActionField", &
            value=value, rc=rc)
          call ESMF_LogWrite(trim(fieldNameList(i))//":*** "//trim(value)// &
            " ***: TransferActionField", ESMF_LOGMSG_INFO, rc=rc)
          call NUOPC_GetAttribute(field, name="ShareStatusField", &
            value=value, rc=rc)
          call ESMF_LogWrite(trim(fieldNameList(i))//":*** "//trim(value)// &
            " ***: ShareStatusField", ESMF_LOGMSG_INFO, rc=rc)
          call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
            value=value, rc=rc)
          call ESMF_LogWrite(trim(fieldNameList(i))//":*** "//trim(value)// &
            " ***: TransferActionGeomObject", ESMF_LOGMSG_INFO, rc=rc)
          call NUOPC_GetAttribute(field, name="ShareStatusGeomObject", &
            value=value, rc=rc)
          call ESMF_LogWrite(trim(fieldNameList(i))//":*** "//trim(value)// &
            " ***: ShareStatusGeomObject:", ESMF_LOGMSG_INFO, rc=rc)
#endif
          call NUOPC_GetAttribute(field, name="TransferActionField", &
            value=value, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          if (trim(value)=="provide") then
            ! create a Field
            field = ESMF_FieldCreate(grid, typekindOpt, &
              name=fieldNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME)) &
              return  ! bail out
            ! realize the connected Field using the just created Field
            call NUOPC_Realize(state, field=field, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME)) &
              return  ! bail out
            if (present(dataFillScheme)) then
              ! a data fill scheme was provided -> use it to initialize
              call ESMF_FieldFill(field, dataFillScheme=dataFillScheme, member=k, step=0, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME)) &
                return  ! bail out
              k=k+1 ! increment the member counter
            endif
          endif
        else
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(i)/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Unknown selection requested.", &
          line=__LINE__, file=FILENAME, &
          rcToReturn=rc)
        return ! bail out
      endif
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Realize - Realize previously advertised Fields inside a State on a single LocStream with internal allocation
! !INTERFACE:
  ! call using generic interface: NUOPC_Realize
  subroutine NUOPC_RealizeCompleteLS(state, locstream, fieldName, typekind, selection,&
    dataFillScheme, rc)
! !ARGUMENTS:
    type(ESMF_State)                                :: state
    type(ESMF_LocStream),     intent(in)            :: locstream
    character(*),             intent(in),  optional :: fieldName
    type(ESMF_TypeKind_Flag), intent(in),  optional :: typekind
    character(len=*),         intent(in),  optional :: selection
    character(len=*),         intent(in),  optional :: dataFillScheme    
    integer,                  intent(out), optional :: rc
! !DESCRIPTION:
!   \label{NUOPC_RealizeComplete}
!
!   Realize or remove fields inside of {\tt state} according to {\tt selection}.
!   All of the fields that are realized are created internally on the same 
!   {\tt locstream} object, allocating memory accordingly.
!
!   The type and kind of the created fields is according to argument 
!   {\tt typekind}.
!
!   Realized fields are filled with data according to the {\tt dataFillScheme}
!   argument.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object in which the fields are realized.
!   \item[locstream]
!     The {\tt ESMF\_LocStream} object on which to realize the fields.
!   \item[{[fieldName]}]
!     The name of the field in {\tt state} to be realized, or removed, according
!     to {\tt selection}. If provided, and the state does not contain a field
!     with name {\tt fieldName}, return an error in {\tt rc}. If not provided,
!     realize {\em all} the fields contained in {\tt state} according to 
!     {\tt selection}.
!   \item[{[typekind]}]
!     The typekind of the internally created field(s). The valid options are
!     {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!     {\tt ESMF\_TYPEKIND\_R4}, and {\tt ESMF\_TYPEKIND\_R8} (default).
!   \item[{[selection]}]
!     Selection of mode of operation:
!     \begin{itemize}
!     \item {\tt "realize\_all"} (default)
!     \item {\tt "realize\_connected\_remove\_others"}
!     \end{itemize}
!   \item[{[dataFillScheme]}]
!     Realized fields will be filled according to the selected fill
!     scheme. See \ref{NUOPC_FillField} for fill schemes. Default is to leave
!     the data in realized fields uninitialized.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=80), allocatable  :: fieldNameList(:)
    integer                         :: i, itemCount, k
    type(ESMF_Field)                :: field
    character(len=80)               :: selectionOpt
    type(ESMF_TypeKind_Flag)        :: typekindOpt

    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(fieldName)) then
      ! fieldName provided -> construct a fieldNameList with a single element
      itemCount=1
      allocate(fieldNameList(itemCount))
      fieldNameList(1)=trim(fieldName)
    else
      ! query the entire fieldNameList from state
      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out
      allocate(fieldNameList(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out
    endif
    
    ! optional selection argument
    if (present(selection)) then
      selectionOpt=trim(selection)
    else
      selectionOpt="realize_all"
    endif
    
    ! optional typekind argument
    if (present(typekind)) then
      typekindOpt=typekind
    else
      typekindOpt=ESMF_TYPEKIND_R8
    endif

    k=1 ! initialize
    do i=1, itemCount
      if (trim(selectionOpt)=="realize_all") then
        ! create a Field
        field = ESMF_FieldCreate(locstream, typekindOpt, &
          name=fieldNameList(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) &
          return  ! bail out
        ! realize the connected Field using the just created Field
        call NUOPC_Realize(state, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) &
          return  ! bail out
        if (present(dataFillScheme)) then
          ! a data fill scheme was provided -> use it to initialize
          call ESMF_FieldFill(field, dataFillScheme=dataFillScheme, member=k, &
            step=0, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          k=k+1 ! increment the member counter
        endif
      else if (trim(selectionOpt)=="realize_connected_remove_others") then
        if (NUOPC_IsConnected(state, fieldName=fieldNameList(i))) then
          ! create a Field
          field = ESMF_FieldCreate(locstream, typekindOpt, &
            name=fieldNameList(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          ! realize the connected Field using the just created Field
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          if (present(dataFillScheme)) then
            ! a data fill scheme was provided -> use it to initialize
            call ESMF_FieldFill(field, dataFillScheme=dataFillScheme, member=k, step=0, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME)) &
              return  ! bail out
            k=k+1 ! increment the member counter
          endif
        else
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(i)/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Unknown selection requested.", &
          line=__LINE__, file=FILENAME, &
          rcToReturn=rc)
        return ! bail out
      endif
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Realize - Realize previously advertised Fields inside a State on a single Mesh with internal allocation
! !INTERFACE:
  ! call using generic interface: NUOPC_Realize
  subroutine NUOPC_RealizeCompleteM(state, mesh, fieldName, typekind, selection,&
    dataFillScheme, rc)
! !ARGUMENTS:
    type(ESMF_State)                                :: state
    type(ESMF_Mesh),          intent(in)            :: mesh
    character(*),             intent(in),  optional :: fieldName
    type(ESMF_TypeKind_Flag), intent(in),  optional :: typekind
    character(len=*),         intent(in),  optional :: selection
    character(len=*),         intent(in),  optional :: dataFillScheme    
    integer,                  intent(out), optional :: rc
! !DESCRIPTION:
!   \label{NUOPC_RealizeComplete}
!
!   Realize or remove fields inside of {\tt state} according to {\tt selection}.
!   All of the fields that are realized are created internally on the same 
!   {\tt mesh} object, allocating memory accordingly.
!
!   The type and kind of the created fields is according to argument 
!   {\tt typekind}.
!
!   Realized fields are filled with data according to the {\tt dataFillScheme}
!   argument.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object in which the fields are realized.
!   \item[mesh]
!     The {\tt ESMF\_Mesh} object on which to realize the fields.
!   \item[{[fieldName]}]
!     The name of the field in {\tt state} to be realized, or removed, according
!     to {\tt selection}. If provided, and the state does not contain a field
!     with name {\tt fieldName}, return an error in {\tt rc}. If not provided,
!     realize {\em all} the fields contained in {\tt state} according to 
!     {\tt selection}.
!   \item[{[typekind]}]
!     The typekind of the internally created field(s). The valid options are
!     {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!     {\tt ESMF\_TYPEKIND\_R4}, and {\tt ESMF\_TYPEKIND\_R8} (default).
!   \item[{[selection]}]
!     Selection of mode of operation:
!     \begin{itemize}
!     \item {\tt "realize\_all"} (default)
!     \item {\tt "realize\_connected\_remove\_others"}
!     \end{itemize}
!   \item[{[dataFillScheme]}]
!     Realized fields will be filled according to the selected fill
!     scheme. See \ref{NUOPC_FillField} for fill schemes. Default is to leave
!     the data in realized fields uninitialized.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=80), allocatable  :: fieldNameList(:)
    integer                         :: i, itemCount, k
    type(ESMF_Field)                :: field
    character(len=80)               :: selectionOpt
    type(ESMF_TypeKind_Flag)        :: typekindOpt

    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(fieldName)) then
      ! fieldName provided -> construct a fieldNameList with a single element
      itemCount=1
      allocate(fieldNameList(itemCount))
      fieldNameList(1)=trim(fieldName)
    else
      ! query the entire fieldNameList from state
      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out
      allocate(fieldNameList(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out
    endif
    
    ! optional selection argument
    if (present(selection)) then
      selectionOpt=trim(selection)
    else
      selectionOpt="realize_all"
    endif
    
    ! optional typekind argument
    if (present(typekind)) then
      typekindOpt=typekind
    else
      typekindOpt=ESMF_TYPEKIND_R8
    endif

    k=1 ! initialize
    do i=1, itemCount
      if (trim(selectionOpt)=="realize_all") then
        ! create a Field
        field = ESMF_FieldCreate(mesh, typekindOpt, &
          name=fieldNameList(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) &
          return  ! bail out
        ! realize the connected Field using the just created Field
        call NUOPC_Realize(state, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) &
          return  ! bail out
        if (present(dataFillScheme)) then
          ! a data fill scheme was provided -> use it to initialize
          call ESMF_FieldFill(field, dataFillScheme=dataFillScheme, member=k, &
            step=0, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          k=k+1 ! increment the member counter
        endif
      else if (trim(selectionOpt)=="realize_connected_remove_others") then
        if (NUOPC_IsConnected(state, fieldName=fieldNameList(i))) then
          ! create a Field
          field = ESMF_FieldCreate(mesh, typekindOpt, &
            name=fieldNameList(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          ! realize the connected Field using the just created Field
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
          if (present(dataFillScheme)) then
            ! a data fill scheme was provided -> use it to initialize
            call ESMF_FieldFill(field, dataFillScheme=dataFillScheme, member=k, step=0, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME)) &
              return  ! bail out
            k=k+1 ! increment the member counter
          endif
        else
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(i)/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) &
            return  ! bail out
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Unknown selection requested.", &
          line=__LINE__, file=FILENAME, &
          rcToReturn=rc)
        return ! bail out
      endif
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Realize - Realize a previously advertised Field in a State
! !INTERFACE:
  ! call using generic interface: NUOPC_Realize
  subroutine NUOPC_RealizeField(state, field, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    type(ESMF_Field), intent(in)            :: field
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   \label{NUOPC_RealizeField}
!
!   Realize a previously advertised field in {\tt state} by replacing the
!   advertised field with {\tt field} of the same name.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object in which the fields are realized.
!   \item[field]
!     The new field to put in place of the previously advertised (empty) field.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)        :: advertisedField
    character(ESMF_MAXSTR)  :: name
    character(ESMF_MAXSTR)  :: StandardName
    character(ESMF_MAXSTR)  :: Units
    character(ESMF_MAXSTR)  :: LongName
    character(ESMF_MAXSTR)  :: ShortName
    integer                 :: i
    integer, parameter      :: attrCount=12
    character(ESMF_MAXSTR)  :: attrList(attrCount)
    character(ESMF_MAXSTR)  :: tempString
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be copied
    attrList(1) = "Connected"
    attrList(2) = "ProducerConnection"
    attrList(3) = "ConsumerConnection"
    attrList(4) = "Updated"
    attrList(5) = "TransferOfferField"
    attrList(6) = "TransferActionField"
    attrList(7) = "SharePolicyField"
    attrList(8) = "ShareStatusField"
    attrList(9) = "TransferOfferGeomObject"
    attrList(10) = "TransferActionGeomObject"
    attrList(11) = "SharePolicyGeomObject"
    attrList(12) = "ShareStatusGeomObject"
    
    ! Obtain the advertised Field
    
    call ESMF_FieldGet(field, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call ESMF_StateGet(state, itemName=name, field=advertisedField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! Obtain basic attributes from the advertised Field
      
    call NUOPC_GetAttribute(advertisedField, name="StandardName", &
      value=StandardName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_GetAttribute(advertisedField, name="Units", &
      value=Units, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_GetAttribute(advertisedField, name="LongName", &
      value=LongName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_GetAttribute(advertisedField, name="ShortName", &
      value=ShortName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! Add the Field attributes to the realizing Field and set basic values
    
    call NUOPC_InitAttributes(field, StandardName=StandardName,&
      Units=Units, LongName=LongName, ShortName=ShortName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! Loop over the list of Attributes and transfer between Fields
    
    do i=1, attrCount
      
      call NUOPC_GetAttribute(advertisedField, name=trim(attrList(i)), &
        value=tempString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

      call NUOPC_SetAttribute(field, name=trim(attrList(i)), &
        value=trim(tempString), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

    enddo
    
    ! Finally replace the advertised Field with the realizing Field
      
    call ESMF_StateReplace(state, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_Reconcile - Reconcile a State
! !INTERFACE:
  subroutine NUOPC_Reconcile(state, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Reconcile the {\tt state}, including attribute handling as required by 
!   the NUOPC Layer. This call is typically made during the initialization 
!   of a Connector.
!
!   This call should rarely be needed in user written code. It is used 
!   by the generic Connector.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object to reconcile.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateReconcile(state, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_SetAttribute - Set the value of a NUOPC Field Attribute
! !INTERFACE:
  ! call using generic interface: NUOPC_SetAttribute
  subroutine NUOPC_SetAttributeField(field, name, value, rc)
! !ARGUMENTS:
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: name
    character(*), intent(in)              :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the attribute {\tt name} inside of {\tt field} using the
!   convention {\tt NUOPC} and purpose {\tt Instance}.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object on which to set the attribute.
!   \item[name]
!     The name of the set attribute.
!   \item[value]
!     The value of the set attribute.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(field, name=name, value=value, &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_SetAttribute - Set the value of a NUOPC State Attribute
! !INTERFACE:
  ! call using generic interface: NUOPC_SetAttribute
  subroutine NUOPC_SetAttributeState(state, name, value, rc)
! !ARGUMENTS:
    type(ESMF_State)                      :: state
    character(*), intent(in)              :: name
    character(*), intent(in)              :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the attribute {\tt name} inside of {\tt state} using the
!   convention {\tt NUOPC} and purpose {\tt Instance}.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object on which to set the attribute.
!   \item[name]
!     The name of the set attribute.
!   \item[value]
!     The value of the set attribute.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(state, name=name, value=value, &
      convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_UpdateTimestamp - Update the TimeStamp on all the Fields across PETs
! !INTERFACE:
  ! call using generic interface: NUOPC_UpdateTimestamp
  subroutine NUOPC_UpdateFieldList(fieldList, rootPet, rc)
! !ARGUMENTS:
    type(ESMF_Field), pointer               :: fieldList(:)
    integer,          intent(in)            :: rootPet
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Update the "TimeStamp" attribute for all the fields on all the PETs in the
!   current VM to the "TimeStamp" attribute held by the field instance on the 
!   {\tt rootPet}.
!
!   This call should rarely be needed in user written code. It is used 
!   by the generic Connector.
!
!   The arguments are:
!   \begin{description}
!   \item[fieldList]
!     List of {\tt ESMF\_Field} objects to be handled.
!   \item[rootPet]
!     Root PET from where to propagate the "TimeStamp" across all other PETs.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)                      :: field
    integer                 :: i, localPet, valueList(9)
    type(ESMF_VM)           :: vm
    
    real(ESMF_KIND_R8)        :: timeBase, time0, time
    character(ESMF_MAXSTR)    :: msgString
    type(ESMF_AttPack)        :: attPack
    
!gjtdebug character(ESMF_MAXSTR)  :: tempString1, msgString

    if (present(rc)) rc = ESMF_SUCCESS
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(timeBase)
    time0=timeBase
#endif

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 01 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 02 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 03 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    if (associated(fieldList)) then
      do i=1, size(fieldList)
        field=fieldList(i)
#ifdef PROFILE_DETAILS_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04a time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
        call ESMF_AttributeGet(field, &
          name="TimeStamp", valueList=valueList, &
          convention="NUOPC", purpose="Instance", &
          attnestflag=ESMF_ATTNEST_ON, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out

#ifdef PROFILE_DETAILS_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04b time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    
!print *, "NUOPC_UpdateFieldList BEFORE: ", valueList

        call ESMF_VMBroadcast(vm, bcstData=valueList, count=size(valueList), &
          rootPet=rootPet, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        
#ifdef PROFILE_DETAILS_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04c time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    
!print *, "NUOPC_UpdateFieldList AFTER:  ", valueList
        
        if (localPet /= rootPet) then
        
          call ESMF_AttributeSet(field, &
            name="TimeStamp", valueList=valueList, &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

!gjtdebug call ESMF_FieldGet(field, name=tempString1)        
!gjtdebug write (msgString, *) "updating to broadcasted TimeStamp:", trim(tempString1), field%ftypep%base
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
!gjtdebug write (msgString, *) valueList
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

        endif
        
#ifdef PROFILE_DETAILS_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04d time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    
      enddo
    endif
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 05 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_UpdateTimestamp - Propagate the TimeStamp from src to dst Fields
! !INTERFACE:
  ! call using generic interface: NUOPC_UpdateTimestamp
  subroutine NUOPC_UpdateAcrossFieldLists(srcFieldList, dstFieldList, rc)
! !ARGUMENTS:
    type(ESMF_Field), pointer               :: srcFieldList(:)
    type(ESMF_Field), pointer               :: dstFieldList(:)
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Update the "TimeStamp" attribute on each field in {\tt dstFieldList} 
!   according to the corresponding (by position) field in the
!   {\tt srcFieldList}. The number of elements in {\tt dstFieldList} and
!   {\tt srcFieldList} must be equal. The update is carried out locally on 
!   each PET.
!
!   This call should rarely be needed in user written code. It is used 
!   by the generic Connector.
!
!   The arguments are:
!   \begin{description}
!   \item[srcFieldList]
!     List of {\tt ESMF\_Field} objects providing the valid "TimeStamp"
!     attribute.
!   \item[dstFieldList]
!     List of {\tt ESMF\_Field} objects that will receive the updated
!     "TimeStamp" attribute.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)              :: srcField, dstField
    integer                       :: i, valueList(9), srcCount, dstCount
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (.not.associated(srcFieldList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="must be associated",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    srcCount=size(srcFieldList)
    if (.not.associated(dstFieldList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="must be associated",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    dstCount=size(dstFieldList)
    if (srcCount /= dstCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="count mismatch",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    do i=1, srcCount    
      srcField = srcFieldList(i)
      dstField = dstFieldList(i)
      call ESMF_AttributeGet(srcField, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="Instance", &
        attnestflag=ESMF_ATTNEST_ON, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
!gjtdebug call ESMF_FieldGet(srcField, name=tempString1)        
!gjtdebug call ESMF_FieldGet(dstField, name=tempString2)        
!gjtdebug write (msgString, *) "updating TimeStamp:", trim(tempString1), " -> ", trim(tempString2), srcField%ftypep%base
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
!gjtdebug write (msgString, *) valueList
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        
      call ESMF_AttributeSet(dstField, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="Instance", attnestflag=ESMF_ATTNEST_ON, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    enddo

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_UpdateTimestamp - Update the TimeStamp on all the Fields in a FieldBundle
! !INTERFACE:
  ! call using generic interface: NUOPC_UpdateTimestamp
  subroutine NUOPC_FieldBundleUpdateTime(srcFields, dstFields, rc)
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)            :: srcFields
    type(ESMF_FieldBundle), intent(inout)         :: dstFields
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Update the "TimeStamp" attribute on all the fields in the {\tt dstFields}
!   FieldBundle to be the same as in the {\tt srcFields} FieldBundle. The number
!   of elements in both FieldBundles must be equal. The update is carried out 
!   locally on each PET.
!
!   This call should rarely be needed in user written code. It is used 
!   by the generic Connector.
!
!   The arguments are:
!   \begin{description}
!   \item[srcFields]
!     FieldBundle providing the valid "TimeStamp" attributes.
!   \item[dstFields]
!     FieldBundle that will receive the updated "TimeStamp" attributes.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field), pointer     :: srcFieldList(:)
    type(ESMF_Field), pointer     :: dstFieldList(:)
    integer                       :: srcCount, dstCount
    
!gjtdebug character(ESMF_MAXSTR)  :: tempString1, tempString2
!gjtdebug character(5*ESMF_MAXSTR):: msgString
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (srcCount /= dstCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="count mismatch",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    allocate(srcFieldList(srcCount))
    call ESMF_FieldBundleGet(srcFields, fieldList=srcFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    allocate(dstFieldList(dstCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call NUOPC_UpdateTimestamp(srcFieldList, dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    deallocate(srcFieldList, dstFieldList)

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_UpdateTimestamp - Update the TimeStamp on all the Fields in a State
! !INTERFACE:
  ! call using generic interface: NUOPC_UpdateTimestamp
  subroutine NUOPC_StateUpdateTimestamp(state, rootPet, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    integer,          intent(in)            :: rootPet
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Update the "TimeStamp" attribute for all the fields on all the PETs in the
!   current VM to the "TimeStamp" attribute held by the field instance on the 
!   {\tt rootPet}.
!
!   This call should rarely be needed in user written code. It is used 
!   by the generic Connector.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object holding the fields.
!   \item[rootPet]
!     Root PET from where to propagate the "TimeStamp" across all other PETs.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field),       pointer       :: fieldList(:)
    type(ESMF_Field)                      :: field
    integer                 :: i, localPet, valueList(9)
    type(ESMF_VM)           :: vm
    
    real(ESMF_KIND_R8)        :: timeBase, time0, time
    character(ESMF_MAXSTR)    :: msgString

!gjtdebug character(ESMF_MAXSTR)  :: tempString1, msgString

    if (present(rc)) rc = ESMF_SUCCESS
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(timeBase)
    time0=timeBase
#endif

    nullify(fieldList)

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 01 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    call NUOPC_GetStateMemberLists(state, fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 02 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 03 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    if (associated(fieldList)) then
      do i=1, size(fieldList)
        field=fieldList(i)
        call ESMF_AttributeGet(field, &
          name="TimeStamp", valueList=valueList, &
          convention="NUOPC", purpose="Instance", &
          attnestflag=ESMF_ATTNEST_ON, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out

!print *, "NUOPC_StateUpdateTimestamp BEFORE: ", valueList

        call ESMF_VMBroadcast(vm, bcstData=valueList, count=size(valueList), &
          rootPet=rootPet, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        
!print *, "NUOPC_StateUpdateTimestamp AFTER:  ", valueList
        
        if (localPet /= rootPet) then
        
          call ESMF_AttributeSet(field, &
            name="TimeStamp", valueList=valueList, &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

!gjtdebug call ESMF_FieldGet(field, name=tempString1)        
!gjtdebug write (msgString, *) "updating to broadcasted TimeStamp:", trim(tempString1), field%ftypep%base
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
!gjtdebug write (msgString, *) valueList
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

        endif
        
      enddo
    endif
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 04 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    if (associated(fieldList)) deallocate(fieldList)
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 05 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_UpdateTimestamp - Set the TimeStamp on all the Fields in a State
! !INTERFACE:
  ! call using generic interface: NUOPC_UpdateTimestamp
  subroutine NUOPC_StateSetTimestamp(state, clock, selective, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    type(ESMF_Clock), intent(in)            :: clock
    logical,          intent(in),  optional :: selective
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Set the "TimeStamp" attribute according to {\tt clock} on all the fields in
!   {\tt state}. Depending on {\tt selective}, all or only some fields may be
!   updated.
!
!   This call should rarely be needed in user written code. It is used 
!   by the generic Connector.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object holding the fields.
!   \item[clock]
!     The {\tt ESMF\_Clock} object defining the TimeStamp by its current time.
!   \item[{[selective]}]
!     If {\tt .true.}, then only set the "TimeStamp" attributes on those fields
!     for which the "Updated" attribute is equal to "true". Otherwise set the
!     "TimeStamp" attribute on all the fields. Default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: StandardNameList(:)
    character(ESMF_MAXSTR), pointer       :: itemNameList(:)
    type(ESMF_Field),       pointer       :: fieldList(:)
    character(ESMF_MAXSTR)                :: value
    type(ESMF_Field)                      :: field
    type(ESMF_Time)         :: time
    integer                 :: yy, mm, dd, h, m, s, ms, us, ns
    integer                 :: i
    logical                 :: selected
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, us=us, &
      ns=ns, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  
    nullify(StandardNameList)
    nullify(itemNameList)
    nullify(fieldList)
  
    call NUOPC_GetStateMemberLists(state, StandardNameList=StandardNameList, &
      itemNameList=itemNameList, fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    if (associated(itemNameList)) then
      do i=1, size(itemNameList)
        field=fieldList(i)
        if (present(selective)) then
          if (selective) then
            call ESMF_AttributeGet(field, &
              name="Updated", value=value, &
              convention="NUOPC", purpose="Instance", &
              attnestflag=ESMF_ATTNEST_ON, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            if (trim(value)=="true") then
              selected=.true.
            else
              selected = .false.
            endif
          else
            selected=.true.
          endif
        else
          selected=.true.
        endif
        if (selected) then
          call ESMF_AttributeSet(field, &
            name="TimeStamp", valueList=(/yy,mm,dd,h,m,s,ms,us,ns/), &
            convention="NUOPC", purpose="Instance", &
            attnestflag=ESMF_ATTNEST_ON, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      enddo
    endif
    
    if (associated(StandardNameList)) deallocate(StandardNameList)
    if (associated(itemNameList)) deallocate(itemNameList)
    if (associated(fieldList)) deallocate(fieldList)
    
  end subroutine
  !-----------------------------------------------------------------------------
  
end module
