! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2014, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Comp.F90"
!==============================================================================

module NUOPC_Comp

  use ESMF
  use NUOPC_Base

  implicit none
  
  private
  
  ! public module interfaces
  public NUOPC_CompAreServicesSet
  public NUOPC_CompAttributeAdd
  public NUOPC_CompAttributeGet
  public NUOPC_CompAttributeSet
  public NUOPC_CompCheckSetClock
  public NUOPC_CompDerive
  public NUOPC_CompFilterPhaseMap
  public NUOPC_CompSearchPhaseMap
  public NUOPC_CompSetClock
  public NUOPC_CompSetEntryPoint
  public NUOPC_CompSetInternalEntryPoint
  public NUOPC_CompSetServices
  public NUOPC_CompSpecialize
  
  ! interface blocks
  interface NUOPC_CompAreServicesSet
    module procedure NUOPC_GridCompAreServicesSet
    module procedure NUOPC_CplCompAreServicesSet
  end interface
  !---------------------------------------------
  interface NUOPC_CompAttributeAdd
    module procedure NUOPC_GridCompAttributeAdd
    module procedure NUOPC_CplCompAttributeAdd
  end interface
  !---------------------------------------------
  interface NUOPC_CompAttributeGet
    module procedure NUOPC_GridCompAttributeGet
    module procedure NUOPC_CplCompAttributeGet
    module procedure NUOPC_CplCompAttributeCplLGet
  end interface
  !---------------------------------------------
  interface NUOPC_CompAttributeSet
    module procedure NUOPC_GridCompAttributeSetS
    module procedure NUOPC_CplCompAttributeSetS
    module procedure NUOPC_GridCompAttributeSetI
    module procedure NUOPC_CplCompAttributeSetI
    module procedure NUOPC_GridCompAttributeSetSL
    module procedure NUOPC_CplCompAttributeSetSL
  end interface
  !---------------------------------------------
  interface NUOPC_CompCheckSetClock
    module procedure NUOPC_GridCompCheckSetClock
  end interface
  !---------------------------------------------
  interface NUOPC_CompDerive
    module procedure NUOPC_GridCompDerive
    module procedure NUOPC_CplCompDerive
  end interface
  !---------------------------------------------
  interface NUOPC_CompFilterPhaseMap
    module procedure NUOPC_GridCompFilterPhaseMap
    module procedure NUOPC_CplCompFilterPhaseMap
  end interface
  !---------------------------------------------
  interface NUOPC_CompSearchPhaseMap
    module procedure NUOPC_GridCompSearchPhaseMap
    module procedure NUOPC_CplCompSearchPhaseMap
  end interface
  !---------------------------------------------
  interface NUOPC_CompSetClock
    module procedure NUOPC_GridCompSetClock
  end interface
  !---------------------------------------------
  interface NUOPC_CompSetEntryPoint
    module procedure NUOPC_GridCompSetEntryPoint
    module procedure NUOPC_CplCompSetEntryPoint
  end interface
  !---------------------------------------------
  interface NUOPC_CompSetInternalEntryPoint
    module procedure NUOPC_GridCompSetIntEntryPoint
  end interface
  !---------------------------------------------
  interface NUOPC_CompSetServices
    module procedure NUOPC_GridCompSetServices
  end interface
  !---------------------------------------------
  interface NUOPC_CompSpecialize
    module procedure NUOPC_GridCompSpecialize
    module procedure NUOPC_CplCompSpecialize
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAreServicesSet - Check if SetServices was called
! !INTERFACE:
  ! Private name; call using NUOPC_CompAreServicesSet() 
  function NUOPC_GridCompAreServicesSet(comp, rc)
! !RETURN VALUE:
    logical :: NUOPC_GridCompAreServicesSet
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: comp
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if SetServices has been called for {\tt comp}. 
!   Otherwise returns {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Pointer)      :: vm_info
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! early bail out condition if incoming component is not valid
    if (.not.associated(comp%compp)) then
      NUOPC_GridCompAreServicesSet = .false.
      return
    endif
    
    ! get the vm_info
    call ESMF_CompGet(comp%compp, vm_info=vm_info, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    if (vm_info == ESMF_NULL_POINTER) then
      NUOPC_GridCompAreServicesSet = .false.
    else
      NUOPC_GridCompAreServicesSet = .true.
    endif
      
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAreServicesSet - Check if SetServices was called
! !INTERFACE:
  ! Private name; call using NUOPC_CompAreServicesSet() 
  function NUOPC_CplCompAreServicesSet(comp, rc)
! !RETURN VALUE:
    logical :: NUOPC_CplCompAreServicesSet
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: comp
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if SetServices has been called for {\tt comp}.
!   Otherwise returns {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Pointer)      :: vm_info
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! early bail out condition if incoming component is not valid
    if (.not.associated(comp%compp)) then
      NUOPC_CplCompAreServicesSet = .false.
      return
    endif

    ! get the vm_info
    call ESMF_CompGet(comp%compp, vm_info=vm_info, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    if (vm_info == ESMF_NULL_POINTER) then
      NUOPC_CplCompAreServicesSet = .false.
    else
      NUOPC_CplCompAreServicesSet = .true.
    endif
      
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAttributeAdd - Add the NUOPC GridComp Attributes
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeAdd() 
  subroutine NUOPC_GridCompAttributeAdd(comp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                   :: comp
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Adds standard NUOPC Attributes to a Gridded Component.
!
!   This adds the standard NUOPC GridComp Attribute package: convention="NUOPC",
!   purpose="General" to the Gridded Component. The NUOPC GridComp Attribute
!   package extends the CIM Component Attribute package: convention="CIM 1.5",
!   purpose="ModelComp".
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)            :: attrList(10)
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be added to the Fields
    attrList(1) = "Verbosity"           ! control verbosity
    attrList(2) = "InitializePhaseMap"  ! list of strings to map str to phase #
    attrList(3) = "InternalInitializePhaseMap"  ! list of strings to map str to phase #
    attrList(4) = "RunPhaseMap"         ! list of strings to map str to phase #
    attrList(5) = "FinalizePhaseMap"    ! list of strings to map str to phase #
    attrList(6) = "NestingGeneration" ! values: integer starting 0 for parent
    attrList(7) = "Nestling"  ! values: integer starting 0 for first nestling
    attrList(8) = "InitializeDataComplete"  ! values: strings "false"/"true"
    attrList(9) = "InitializeDataProgress"  ! values: strings "false"/"true"
    attrList(10)= "CompLabel"   ! label by which this component was added
    
    ! add Attribute packages
    call ESMF_AttributeAdd(comp, convention="CIM 1.5", &
      purpose="ModelComp", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeAdd(comp, convention="NUOPC", purpose="General",   &
      attrList=attrList, nestConvention="CIM 1.5", &
      nestPurpose="ModelComp", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set Attributes to defaults
    call NUOPC_CompAttributeSet(comp, &
      name="Verbosity", value="low", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call NUOPC_CompAttributeSet(comp, &
      name="NestingGeneration", value=0, &        ! default to parent level
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call NUOPC_CompAttributeSet(comp, &
      name="Nestling", value=0, &                 ! default to first nestling
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call NUOPC_CompAttributeSet(comp, &
      name="InitializeDataComplete", value="false", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call NUOPC_CompAttributeSet(comp, &
      name="InitializeDataProgress", value="false", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAttributeAdd - Add the NUOPC CplComp Attributes
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeAdd() 
  subroutine NUOPC_CplCompAttributeAdd(comp, rc)
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: comp
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Adds standard NUOPC Attributes to a Coupler Component. Checks the provided
!   importState and exportState arguments for matching Fields and adds the list
!   as "CplList" Attribute.
!
!   This adds the standard NUOPC Coupler Attribute package: convention="NUOPC", 
!   purpose="General" to the Field. The NUOPC Coupler Attribute package extends
!   the ESG Component Attribute package: convention="ESG", purpose="General".
!
!   The arguments are:
!   \begin{description}
!   \item[comp]
!     The {\tt ESMF\_CplComp} object to which the Attributes are added.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: attrList(6)

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! Set up a customized list of Attributes to be added to the CplComp
    attrList(1) = "Verbosity"           ! control verbosity
    attrList(2) = "InitializePhaseMap"  ! list of strings to map str to phase #
    attrList(3) = "RunPhaseMap"         ! list of strings to map str to phase #
    attrList(4) = "FinalizePhaseMap"    ! list of strings to map str to phase #
    attrList(5) = "CplList"
    attrList(6) = "CompLabel"  ! label by which this component was added
    
    ! add Attribute packages
    call ESMF_AttributeAdd(comp, convention="ESG", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeAdd(comp, convention="NUOPC", purpose="General",   &
      attrList=attrList, nestConvention="ESG", nestPurpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
          
    ! set Attributes to defaults
    call NUOPC_CompAttributeSet(comp, &
      name="Verbosity", value="low", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAttributeGet - Get a NUOPC GridComp Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeGet() 
  subroutine NUOPC_GridCompAttributeGet(comp, name, value, rc)
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: comp
    character(*),        intent(in)            :: name
    character(*),        intent(out)           :: value
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Access the Attribute {\tt name} inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}. Returns with error if
!   the Attribute is not present or not set.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    defaultvalue = "CheckThisDefaultValue"

    call ESMF_AttributeGet(comp, name=name, value=value, &
      defaultvalue=defaultvalue, convention="NUOPC", purpose="General", &
      rc=rc)
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
! !IROUTINE: NUOPC_CompAttributeGet - Get a NUOPC CplComp Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeGet() 
  subroutine NUOPC_CplCompAttributeGet(comp, name, value, rc)
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: comp
    character(*),        intent(in)            :: name
    character(*),        intent(out)           :: value
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Access the Attribute {\tt name} inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}. Returns with error if
!   the Attribute is not present or not set.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    defaultvalue = "CheckThisDefaultValue"

    call ESMF_AttributeGet(comp, name=name, value=value, &
      defaultvalue=defaultvalue, convention="NUOPC", purpose="General", &
      rc=rc)
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
! !IROUTINE: NUOPC_CompAttributeGet - Get a NUOPC CplComp Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeGet() 
  subroutine NUOPC_CplCompAttributeCplLGet(comp, cplList, cplListSize, rc)
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: comp
    character(*),       intent(out), optional :: cplList(:)
    integer,            intent(out), optional :: cplListSize
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Access the "CplList" Attribute inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}. Returns with error if
!   the Attribute is not present or not set.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    if (present(cplList)) then
      call ESMF_AttributeGet(comp, name="CplList", valueList=cplList, &
        itemCount=cplListSize, convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      call ESMF_AttributeGet(comp, name="CplList", &
        itemCount=cplListSize, convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAttributeSet - Set a NUOPC GridComp Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeSet() 
  subroutine NUOPC_GridCompAttributeSetS(comp, name, value, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                   :: comp
    character(*), intent(in)              :: name
    character(*), intent(in)              :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the Attribute {\tt name} inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}.
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(comp, name=name, value=value, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAttributeSet - Set a NUOPC CplComp Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeSet() 
  subroutine NUOPC_CplCompAttributeSetS(comp, name, value, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                    :: comp
    character(*), intent(in)              :: name
    character(*), intent(in)              :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the Attribute {\tt name} inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}.
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(comp, name=name, value=value, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAttributeSet - Set a NUOPC GridComp Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeSet() 
  subroutine NUOPC_GridCompAttributeSetI(comp, name, value, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                   :: comp
    character(*), intent(in)              :: name
    integer,      intent(in)              :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the Attribute {\tt name} inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}.
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(comp, name=name, value=value, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAttributeSet - Set a NUOPC CplComp Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeSet() 
  subroutine NUOPC_CplCompAttributeSetI(comp, name, value, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                    :: comp
    character(*), intent(in)              :: name
    integer,      intent(in)              :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the Attribute {\tt name} inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}.
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(comp, name=name, value=value, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------


! !IROUTINE: NUOPC_CompAttributeSet - Set a NUOPC GridComp List Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeSet() 
  subroutine NUOPC_GridCompAttributeSetSL(comp, name, valueList, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                   :: comp
    character(*), intent(in)              :: name
    character(*), intent(in)              :: valueList(:)
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the Attribute {\tt name} inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}.
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(comp, name=name, valueList=valueList, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompAttributeSet - Set a NUOPC CplComp List Attribute
! !INTERFACE:
  ! Private name; call using NUOPC_CompAttributeSet() 
  subroutine NUOPC_CplCompAttributeSetSL(comp, name, valueList, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                    :: comp
    character(*), intent(in)              :: name
    character(*), intent(in)              :: valueList(:)
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the Attribute {\tt name} inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}.
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(comp, name=name, valueList=valueList, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompCheckSetClock - Check Clock compatibility and set stopTime
! !INTERFACE:
  ! Private name; call using NUOPC_CompCheckSetClock() 
  subroutine NUOPC_GridCompCheckSetClock(comp, externalClock, rc)
! !ARGUMENTS:
    type(ESMF_GridComp),     intent(inout)         :: comp
    type(ESMF_Clock),        intent(in)            :: externalClock
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Compares {\tt externalClock} to the Component internal Clock to make sure
!   they match in their current Time. Further ensures that the external Clock's
!   timeStep is a multiple of the internal Clock's timeStep. If both
!   these condition are satisfied then the stopTime of the internal Clock is
!   set to be reachable in one timeStep of the external Clock, taking into
!   account the direction of the Clock.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Clock)        :: internalClock

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(comp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call NUOPC_ClockCheckSetClock(setClock=internalClock, &
      checkClock=externalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompDerive - Derive a GridComp from a generic component
! !INTERFACE:
  ! Private name; call using NUOPC_CompDerive() 
  subroutine NUOPC_GridCompDerive(comp, genericSetServicesRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: comp
    interface
      subroutine genericSetServicesRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Derive a GridComp (i.e. Model, Mediator, or Driver) from a generic component.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)    :: name

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! call into the generic SetServices routine
    call genericSetServicesRoutine(comp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompDerive - Derive a CplComp from a generic component
! !INTERFACE:
  ! Private name; call using NUOPC_CompDerive() 
  subroutine NUOPC_CplCompDerive(comp, genericSetServicesRoutine, rc)
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: comp
    interface
      subroutine genericSetServicesRoutine(cplcomp, rc)
        use ESMF
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Derive a CplComp (i.e. Connector) from a generic component.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)    :: name

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_CplCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! call into the generic SetServices routine
    call genericSetServicesRoutine(comp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompFilterPhaseMap - Filter the Phase Map of a GridComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompFilterPhaseMap()
  subroutine NUOPC_GridCompFilterPhaseMap(comp, methodflag, acceptStringList, &
    rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                           :: comp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    character(len=*),       intent(in)            :: acceptStringList(:)
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Filter all PhaseMap entries in a GridComp (i.e. Model, Mediator, or Driver)
! that do {\em not} match any entry in the {\tt acceptStringList}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i, ii, iii
    integer                   :: itemCount, stat
    integer                   :: acceptStringCount
    character(ESMF_MAXSTR)    :: name
    character(len=40)         :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)
    character(len=NUOPC_PhaseMapStringLength), pointer :: newPhases(:)

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    ! determine how many phaseLabels are contained in the incoming list
    acceptStringCount = size(acceptStringList)
    
    ! query the already existing phaseMap enties
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(phases(itemCount), newPhases(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, &
      file=trim(name)//":"//FILENAME)) return  ! bail out
    if (itemCount > 0) then
      call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! filter all entries that do not match entries in acceptStringList
    iii=0 ! reset
    do i=1, itemCount
      ! see if there is a match with an entry in the acceptStringList
      do ii=1, acceptStringCount
        if (index(phases(i),trim(acceptStringList(ii))) > 0 ) exit
      enddo
      if (ii <= acceptStringCount) then
        ! found a match -> preserve the phaseMap entry
        iii = iii+1
        newPhases(iii) = trim(phases(i))
      endif
    enddo
    
    ! insert a dummy entry in case no entries are left
    if (iii==0) then
      iii=1
      newPhases(1) = "IPDvDummy" ! something obvious
    endif
    
    ! set the filtered phase map as the Attribute
    call NUOPC_CompAttributeSet(comp, name=trim(attributeName), &
      valueList=newPhases(1:iii), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! clean-up
    deallocate(phases, newPhases)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompFilterPhaseMap - Filter the Phase Map of a CplComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompFilterPhaseMap()
  subroutine NUOPC_CplCompFilterPhaseMap(comp, methodflag, acceptStringList, &
    rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: comp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    character(len=*),       intent(in)            :: acceptStringList(:)
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Filter all PhaseMap entries in a CplComp (i.e. Connector)
! that do {\em not} match any entry in the {\tt acceptStringList}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i, ii, iii
    integer                   :: itemCount, stat
    integer                   :: acceptStringCount
    character(ESMF_MAXSTR)    :: name
    character(len=40)         :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)
    character(len=NUOPC_PhaseMapStringLength), pointer :: newPhases(:)

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    ! determine how many phaseLabels are contained in the incoming list
    acceptStringCount = size(acceptStringList)
    
    ! query the already existing phaseMap enties
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(phases(itemCount), newPhases(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, &
      file=trim(name)//":"//FILENAME)) return  ! bail out
    if (itemCount > 0) then
      call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! filter all entries that do not match entries in acceptStringList
    iii=0 ! reset
    do i=1, itemCount
      ! see if there is a match with an entry in the acceptStringList
      do ii=1, acceptStringCount
        if (index(phases(i),trim(acceptStringList(ii))) > 0 ) exit
      enddo
      if (ii <= acceptStringCount) then
        ! found a match -> preserve the phaseMap entry
        iii = iii+1
        newPhases(iii) = trim(phases(i))
      endif
    enddo
    
    ! insert a dummy entry in case no entries are left
    if (iii==0) then
      iii=1
      newPhases(1) = "IPDvDummy" ! something obvious
    endif
    
    ! set the filtered phase map as the Attribute
    call NUOPC_CompAttributeSet(comp, name=trim(attributeName), &
      valueList=newPhases(1:iii), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! clean-up
    deallocate(phases, newPhases)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSearchPhaseMap - Search the Phase Map of a GridComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompSearchPhaseMap()
  subroutine NUOPC_GridCompSearchPhaseMap(comp, methodflag, phaseLabel, &
    phaseIndex, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                           :: comp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    character(len=*),       intent(in),  optional :: phaseLabel
    integer,                intent(out)           :: phaseIndex
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Search all PhaseMap entries in a GridComp (i.e. Model, Mediator, or Driver)
! to see if {\tt phaseLabel} is found. Return the associated ESMF
! {\tt phaseIndex}, or {\tt -1} if not found. If {\tt phaseLabel} is not
! specified, set {\tt phaseIndex} to the first entry in the PhaseMap, or 
! {\tt -1} if there are no entries.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i
    integer                   :: itemCount, stat, ind, max
    character(ESMF_MAXSTR)    :: name
    character(len=40)         :: attributeName
    logical                   :: phaseFlag
    character(len=NUOPC_PhaseMapStringLength), pointer  :: phases(:)
    character(len=NUOPC_PhaseMapStringLength)           :: tempString

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    phaseIndex = -1             ! initialize to invalid
    phaseFlag  = .false.        ! initialize
    
    ! access phaseMap info
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! search the phaseMap
    if (itemCount > 0) then
      allocate(phases(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of temporary data structure.", &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (present(phaseLabel)) then
        do i=1, itemCount
          if (index(phases(i),trim(phaseLabel//"=")) > 0) exit
        enddo
        if (i <= itemCount) then
          ! phaseLabel was found
          phaseFlag = .true.
          tempString = trim(phases(i))
        endif
      else
        phaseFlag = .true.
        tempString = trim(phases(1))  ! by default select the first map entry
      endif
      if (phaseFlag) then
        ind = index(tempString, "=")
        max = len(tempString)
        read (tempString(ind+1:max), "(i4)") phaseIndex ! obtain phase index
      endif
      ! clean-up
      deallocate(phases)
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSearchPhaseMap - Search the Phase Map of a CplComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompSearchPhaseMap()
  subroutine NUOPC_CplCompSearchPhaseMap(comp, methodflag, phaseLabel, &
    phaseIndex, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: comp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    character(len=*),       intent(in),  optional :: phaseLabel
    integer,                intent(out)           :: phaseIndex
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Search all PhaseMap entries in a CplComp (i.e. Connector)
! to see if {\tt phaseLabel} is found. Return the associated ESMF
! {\tt phaseIndex}, or {\tt -1} if not found. If {\tt phaseLabel} is not
! specified, set {\tt phaseIndex} to the first entry in the PhaseMap, or 
! {\tt -1} if there are no entries.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i
    integer                   :: itemCount, stat, ind, max
    character(ESMF_MAXSTR)    :: name
    character(len=40)         :: attributeName
    logical                   :: phaseFlag
    character(len=NUOPC_PhaseMapStringLength), pointer  :: phases(:)
    character(len=NUOPC_PhaseMapStringLength)           :: tempString

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    phaseIndex = -1             ! initialize to invalid
    phaseFlag  = .false.        ! initialize
    
    ! access phaseMap info
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! search the phaseMap
    if (itemCount > 0) then
      allocate(phases(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of temporary data structure.", &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (present(phaseLabel)) then
        do i=1, itemCount
          if (index(phases(i),trim(phaseLabel//"=")) > 0) exit
        enddo
        if (i <= itemCount) then
          ! phaseLabel was found
          phaseFlag = .true.
          tempString = trim(phases(i))
        endif
      else
        phaseFlag = .true.
        tempString = trim(phases(1))  ! by default select the first map entry
      endif
      if (phaseFlag) then
        ind = index(tempString, "=")
        max = len(tempString)
        read (tempString(ind+1:max), "(i4)") phaseIndex ! obtain phase index
      endif
      ! clean-up
      deallocate(phases)
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSetClock - Initialize and set the internal Clock of a GridComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompSetClock()
  subroutine NUOPC_GridCompSetClock(comp, externalClock, stabilityTimeStep, rc)
! !ARGUMENTS:
    type(ESMF_GridComp),     intent(inout)         :: comp
    type(ESMF_Clock),        intent(in)            :: externalClock
    type(ESMF_TimeInterval), intent(in),  optional :: stabilityTimeStep
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Sets the Component internal Clock as a copy of {\tt externalClock}, but
!   with a timeStep that is less than or equal to the stabilityTimeStep.
!   At the same time ensures that the timeStep of the external Clock is
!   a multiple of the internal Clock's timeStep. If the stabilityTimeStep
!   argument is not provided then the internal Clock will simply be set
!   as a copy of the externalClock.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Clock)        :: internalClock

    if (present(rc)) rc = ESMF_SUCCESS
    
    internalClock = NUOPC_ClockInitialize(externalClock, stabilityTimeStep, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_GridCompSet(comp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSetEntryPoint - Set entry point for a GridComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSetEntryPoint()
  subroutine NUOPC_GridCompSetEntryPoint(comp, methodflag, phaseLabelList, &
    userRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: comp
    type(ESMF_Method_Flag), intent(in)      :: methodflag
    character(len=*),       intent(in)      :: phaseLabelList(:)
    interface
      subroutine userRoutine(gridcomp, importState, exportState, clock, rc)
        use ESMF_CompMod
        use ESMF_StateMod
        use ESMF_ClockMod
        implicit none
        type(ESMF_GridComp)         :: gridcomp     ! must not be optional
        type(ESMF_State)            :: importState  ! must not be optional
        type(ESMF_State)            :: exportState  ! must not be optional
        type(ESMF_Clock)            :: clock        ! must not be optional
        integer, intent(out)        :: rc           ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set an entry point for a GridComp (i.e. Model, Mediator, or Driver). Publish
! the new entry point in the correct {\tt PhaseMap} component attribute.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i, ii, iii
    character(ESMF_MAXSTR)    :: name
    integer                   :: phase, itemCount, phaseLabelCount, stat
    character(len=8)          :: phaseString
    character(len=40)         :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine next available phase index    
    call ESMF_GridCompGetEPPhaseCount(comp, methodflag, &
      phaseCount=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    phase = phase + 1

    ! set the entry point with this phase index
    call ESMF_GridCompSetEntryPoint(comp, methodflag, userRoutine=userRoutine, &
      phase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
!print *, "NUOPC_GridCompSetEntryPoint: phaseLabelList:", &
!phaseLabelList, "     phase:", phase

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    ! determine how many phaseLabels are contained in the incoming list
    phaseLabelCount = size(phaseLabelList)
    
    ! query the already existing phaseMap enties
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(phases(itemCount+phaseLabelCount), stat=stat) ! space to add more
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (itemCount > 0) then
      call ESMF_AttributeGet(comp, name=trim(attributeName), &
        valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    ! add the new entries to the phaseMap
    write(phaseString, "(I6)") phase
    iii=0 ! initialize
    do i=1, phaseLabelCount
      ! see if this same phaseLabel has already been used before
      do ii=1, itemCount
        if (index(phases(ii),trim(phaseLabelList(i))) > 0 ) exit
      enddo
      if (ii <= itemCount) then
        ! overwrite an existing entry with the same phaseLabel
        phases(ii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      else
        ! add a new entry for the phaseLabel at the end of the list
        iii = iii+1
        phases(itemCount+iii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      endif
    enddo
    
    ! set the new phaseMap in the Attribute
    call NUOPC_CompAttributeSet(comp, name=trim(attributeName), &
      valueList=phases(1:itemCount+iii), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! clean-up
    deallocate(phases)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSetEntryPoint - Set entry point for a CplComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSetEntryPoint()
  subroutine NUOPC_CplCompSetEntryPoint(comp, methodflag, phaseLabelList, &
    userRoutine, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: comp
    type(ESMF_Method_Flag), intent(in)      :: methodflag
    character(len=*),       intent(in)      :: phaseLabelList(:)
    interface
      subroutine userRoutine(cplcomp, importState, exportState, clock, rc)
        use ESMF_CompMod
        use ESMF_StateMod
        use ESMF_ClockMod
        implicit none
        type(ESMF_CplComp)          :: cplcomp      ! must not be optional
        type(ESMF_State)            :: importState  ! must not be optional
        type(ESMF_State)            :: exportState  ! must not be optional
        type(ESMF_Clock)            :: clock        ! must not be optional
        integer, intent(out)        :: rc           ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set an entry point for a CplComp (i.e. Connector). Publish
! the new entry point in the correct {\tt PhaseMap} component attribute.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i, ii, iii
    character(ESMF_MAXSTR)    :: name
    integer                   :: phase, itemCount, phaseLabelCount, stat
    character(len=8)          :: phaseString
    character(len=40)         :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine next available phase index    
    call ESMF_CplCompGetEPPhaseCount(comp, methodflag, &
      phaseCount=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    phase = phase + 1

    ! set the entry point with this phase index
    call ESMF_CplCompSetEntryPoint(comp, methodflag, userRoutine=userRoutine, &
      phase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
!print *, "NUOPC_CplCompSetEntryPoint: phaseLabelList:", &
!phaseLabelList, "     phase:", phase

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    ! determine how many phaseLabels are contained in the incoming list
    phaseLabelCount = size(phaseLabelList)
    
    ! query the already existing phaseMap enties
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(phases(itemCount+phaseLabelCount), stat=stat) ! space to add more
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (itemCount > 0) then
      call ESMF_AttributeGet(comp, name=trim(attributeName), &
        valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    ! add the new entries to the phaseMap
    write(phaseString, "(I6)") phase
    iii=0 ! initialize
    do i=1, phaseLabelCount
      ! see if this same phaseLabel has already been used before
      do ii=1, itemCount
        if (index(phases(ii),trim(phaseLabelList(i))) > 0 ) exit
      enddo
      if (ii <= itemCount) then
        ! overwrite an existing entry with the same phaseLabel
        phases(ii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      else
        ! add a new entry for the phaseLabel at the end of the list
        iii = iii+1
        phases(itemCount+iii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      endif
    enddo
    
    ! set the new phaseMap in the Attribute
    call NUOPC_CompAttributeSet(comp, name=trim(attributeName), &
      valueList=phases(1:itemCount+iii), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! clean-up
    deallocate(phases)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSetInternalEntryPoint - Set internal entry point for a GridComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSetInternalEntryPoint()
  subroutine NUOPC_GridCompSetIntEntryPoint(comp, methodflag, phaseLabelList, &
    userRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: comp
    type(ESMF_Method_Flag), intent(in)      :: methodflag
    character(len=*),       intent(in)      :: phaseLabelList(:)
    interface
      subroutine userRoutine(gridcomp, importState, exportState, clock, rc)
        use ESMF_CompMod
        use ESMF_StateMod
        use ESMF_ClockMod
        implicit none
        type(ESMF_GridComp)         :: gridcomp     ! must not be optional
        type(ESMF_State)            :: importState  ! must not be optional
        type(ESMF_State)            :: exportState  ! must not be optional
        type(ESMF_Clock)            :: clock        ! must not be optional
        integer, intent(out)        :: rc           ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set the internal entry point for a GridComp (i.e. Driver). Only Drivers 
! utilize internal entry points.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i, ii, iii
    character(ESMF_MAXSTR)    :: name
    integer                   :: phase, itemCount, phaseLabelCount, stat
    character(len=8)          :: phaseString
    character(len=40)         :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine next available phase index    
    call ESMF_GridCompGetEPPhaseCount(comp, methodflag, &
      phaseCount=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    phase = phase + 1

    ! set the entry point with this phase index
    call ESMF_GridCompSetEntryPoint(comp, methodflag, userRoutine=userRoutine, &
      phase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
!print *, "NUOPC_GridCompSetEntryPoint: phaseLabelList:", &
!phaseLabelList, "     phase:", phase

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InternalInitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "InternalRunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "InternalFinalizePhaseMap"
    endif
    
    ! determine how many phaseLabels are contained in the incoming list
    phaseLabelCount = size(phaseLabelList)
    
    ! query the already existing phaseMap enties
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(phases(itemCount+phaseLabelCount), stat=stat) ! space to add more
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (itemCount > 0) then
      call ESMF_AttributeGet(comp, name=trim(attributeName), &
        valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
    ! add the new entries to the phaseMap
    write(phaseString, "(I6)") phase
    iii=0 ! initialize
    do i=1, phaseLabelCount
      ! see if this same phaseLabel has already been used before
      do ii=1, itemCount
        if (index(phases(ii),trim(phaseLabelList(i))) > 0 ) exit
      enddo
      if (ii <= itemCount) then
        ! overwrite an existing entry with the same phaseLabel
        phases(ii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      else
        ! add a new entry for the phaseLabel at the end of the list
        iii = iii+1
        phases(itemCount+iii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      endif
    enddo
    
    ! set the new phaseMap in the Attribute
    call NUOPC_CompAttributeSet(comp, name=trim(attributeName), &
      valueList=phases(1:itemCount+iii), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! clean-up
    deallocate(phases)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSetServices - Try to find and call SetServices in a shared object
! !INTERFACE:
  ! Private name; call using NUOPC_CompSetServices()
  recursive subroutine NUOPC_GridCompSetServices(comp, sharedObj, userRc, rc)
! !ARGUMENTS:
    type(ESMF_GridComp),     intent(inout)         :: comp
    character(len=*),        intent(in),  optional :: sharedObj
    integer,                 intent(out), optional :: userRc
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Try to find a routine called "SetServices" in the sharedObj and execute it
!   to set the component's services. An attempt is made to find a routine that
!   is close in name to "SetServices", allowing compiler name mangeling, i.e.
!   upper and lower case, as well as trailing underscores.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    logical           :: userRoutineFound

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! attempt to find something called SetServices, allowing variations
    ! caused by compiler name mangeling
    
    call ESMF_GridCompSetServices(comp, userRoutine="setservices", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="setservices_", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="setservices__", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SETSERVICES", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SETSERVICES_", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SETSERVICES__", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SetServices", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SetServices_", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully

    call ESMF_GridCompSetServices(comp, userRoutine="SetServices__", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully

    ! getting down to here means that none of the attempts were successful
    if (present(sharedObj)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Could not find a matching SetServices routine in "//trim(sharedObj),&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Could not find a matching SetServices routine in the executable.", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
    endif
      
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSpecialize - Specialize a derived GridComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSpecialize()
  subroutine NUOPC_GridCompSpecialize(comp, specLabel, specPhaseLabel, &
    specRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: comp
    character(len=*), intent(in)            :: specLabel
    character(len=*), intent(in),  optional :: specPhaseLabel
    interface
      subroutine specRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Specialize a derived GridComp (i.e. Model, Mediator, or Driver). If
! {\tt specPhaseLabel} is specified, the specialization only applies to
! the associated phase. Otherwise the specialization applies to all phases.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)    :: name
    integer                   :: phaseIndex

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    if (present(specPhaseLabel)) then
      ! Figure out the phase index
      call NUOPC_CompSearchPhaseMap(comp, methodflag=ESMF_METHOD_RUN, &
        phaseLabel=specPhaseLabel, phaseIndex=phaseIndex, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (phaseIndex < 0) then
        ! specPhaseLabel was not found
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="specPhaseLabel could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      ! add the method under the specific phase index
      call ESMF_MethodAdd(comp, label=specLabel, index=phaseIndex, &
        userRoutine=specRoutine, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    else
      ! add the method under under no specific phase index
      call ESMF_MethodAdd(comp, label=specLabel, &
        userRoutine=specRoutine, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSpecialize - Specialize a derived CplComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSpecialize()
  subroutine NUOPC_CplCompSpecialize(comp, specLabel, specPhaseLabel, &
    specRoutine, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: comp
    character(len=*), intent(in)            :: specLabel
    character(len=*), intent(in),  optional :: specPhaseLabel
    interface
      subroutine specRoutine(cplcomp, rc)
        use ESMF
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Specialize a derived CplComp (i.e. Connector). If
! {\tt specPhaseLabel} is specified, the specialization only applies to
! the associated phase. Otherwise the specialization applies to all phases.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)    :: name
    integer                   :: phaseIndex

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    if (present(specPhaseLabel)) then
      ! Figure out the phase index
      call NUOPC_CompSearchPhaseMap(comp, methodflag=ESMF_METHOD_RUN, &
        phaseLabel=specPhaseLabel, phaseIndex=phaseIndex, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (phaseIndex < 0) then
        ! specPhaseLabel was not found
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="specPhaseLabel could not be identified.", &
          line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      ! add the method under the specific phase index
      call ESMF_MethodAdd(comp, label=specLabel, index=phaseIndex, &
        userRoutine=specRoutine, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    else
      ! add the method under under no specific phase index
      call ESMF_MethodAdd(comp, label=specLabel, &
        userRoutine=specRoutine, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
