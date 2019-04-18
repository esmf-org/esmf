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
#define ESMF_FILENAME "ESMF_Comp.F90"
!==============================================================================
!
! ESMF Component module
module ESMF_CompMod
!
!==============================================================================
! A blank line to keep protex happy because there are no public entry
! points in this file, only internal ones.
!BOP

!EOP
!
! This file contains the Component class definition and all Component
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_CompMod - Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Component} class and associated functions and subroutines.  
!
!
! !USES: 
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_BaseMod
  use ESMF_VMMod
  use ESMF_ConfigMod
  use ESMF_CalendarMod
  use ESMF_ClockMod
  use ESMF_GridMod
  use ESMF_MeshMod
  use ESMF_LocStreamMod
  use ESMF_XGridMod
  use ESMF_StateTypesMod
  use ESMF_StateMod
  use ESMF_InitMacrosMod
  use ESMF_IOUtilMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
! ! ESMF_CompType_Flag
!
  type ESMF_CompType_Flag
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    private
    integer :: ctype
  end type

  type(ESMF_CompType_Flag), parameter :: &
    ESMF_COMPTYPE_GRID = ESMF_CompType_Flag(1), &
    ESMF_COMPTYPE_CPL  = ESMF_CompType_Flag(2), &
    ESMF_COMPTYPE_SCI  = ESMF_CompType_Flag(3)

!------------------------------------------------------------------------------
! ! ESMF Method Type
!
  type ESMF_Method_Flag
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    private
    integer :: method
  end type

  type(ESMF_Method_Flag), parameter :: &
    ESMF_METHOD_NONE            = ESMF_Method_Flag(0), &
    ESMF_METHOD_INITIALIZE      = ESMF_Method_Flag(1), &
    ESMF_METHOD_RUN             = ESMF_Method_Flag(2), &
    ESMF_METHOD_FINALIZE        = ESMF_Method_Flag(3), &
    ESMF_METHOD_WRITERESTART    = ESMF_Method_Flag(4), &
    ESMF_METHOD_READRESTART     = ESMF_Method_Flag(5), &
    ESMF_METHOD_SERVICELOOP     = ESMF_Method_Flag(6), &
    ESMF_METHOD_INITIALIZEIC    = ESMF_Method_Flag(7), &
    ESMF_METHOD_RUNIC           = ESMF_Method_Flag(8), &
    ESMF_METHOD_FINALIZEIC      = ESMF_Method_Flag(9), &
    ESMF_METHOD_WRITERESTARTIC  = ESMF_Method_Flag(10), &
    ESMF_METHOD_READRESTARTIC   = ESMF_Method_Flag(11), &
    ESMF_METHOD_SERVICELOOPIC   = ESMF_Method_Flag(12), &
    ESMF_METHOD_SETVM           = ESMF_Method_Flag(13), &
    ESMF_METHOD_SETSERVICES     = ESMF_Method_Flag(14), &
    ESMF_METHOD_WAIT            = ESMF_Method_Flag(15)
    
!------------------------------------------------------------------------------
! ! ESMF_CompStatus
!
  type ESMF_CompStatus
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
!    private
    logical :: configIsPresent
    logical :: clockIsPresent
    logical :: gridIsPresent
    logical :: meshIsPresent
    logical :: locstreamIsPresent
    logical :: xgridIsPresent
    logical :: configFileIsPresent
    logical :: vmIsPresent
    logical :: isIsPresent
    logical :: esIsPresent
    ESMF_INIT_DECLARE
  end type

  type(ESMF_CompStatus), parameter :: &
    ESMF_COMPSTATUS_ALL_PRESENT = ESMF_CompStatus(&
      .true., & ! configIsPresent
      .true., & ! clockIsPresent
      .true., & ! gridIsPresent
      .true., & ! meshIsPresent
      .true., & ! locstreamIsPresent
      .true., & ! xgridIsPresent
      .true., & ! configFileIsPresent
      .true., & ! vmIsPresent
      .true., & ! isIsPresent
      .true., & ! esIsPresent
      ESMF_INIT_DEFINED), &
    ESMF_COMPSTATUS_ALL_NOTPRESENT = ESMF_CompStatus(&
      .false., & ! configIsPresent
      .false., & ! clockIsPresent
      .false., & ! gridIsPresent
      .false., & ! meshIsPresent
      .false., & ! locstreamIsPresent
      .false., & ! xgridIsPresent
      .false., & ! configFileIsPresent
      .false., & ! vmIsPresent
      .false., & ! isIsPresent
      .false., & ! esIsPresent
      ESMF_INIT_DEFINED)

!------------------------------------------------------------------------------
! ! ESMF_CompTunnel

  type ESMF_CompTunnel
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    !private
    type(ESMF_Pointer) :: this
    ! only use internally -> no init macro!
  end type
     
!------------------------------------------------------------------------------
! ! wrapper for Component objects going across F90/C++ boundary
  type ESMF_CWrap
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
#endif
    !private
    type(ESMF_CompClass), pointer :: compp
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_CompClass
!
! ! Component internal class data.

  type ESMF_CompClass
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
#endif
    !private
    type(ESMF_Pointer)  :: ftable           ! C++ ftable pointer - MUST BE FIRST
    type(ESMF_Base)     :: base             ! base class
    type(ESMF_MethodTable)  :: methodTable  ! attachable methods
    type(ESMF_CompType_Flag):: compType     ! component type
    
    type(ESMF_CompTunnel)   :: compTunnel   ! in case this is a dual component
    
    type(ESMF_Config)   :: config           ! configuration object
    type(ESMF_Clock)    :: clock            ! private component clock
    
    type(ESMF_Grid),      allocatable  :: gridList(:)     ! associated grids
    type(ESMF_Mesh),      allocatable  :: meshList(:)     ! associated meshes
    type(ESMF_LocStream), allocatable  :: locstreamList(:)! associated locstream
    type(ESMF_XGrid),     allocatable  :: xgridList(:)    ! associated xgrids

    character(len=ESMF_MAXPATHLEN) :: configFile! resource filename
    character(len=ESMF_MAXPATHLEN) :: dirPath   ! relative dirname, app only

    type(ESMF_CWrap)    :: compw            ! to satisfy the C interface
    type(ESMF_VM)       :: vm               ! component VM
    type(ESMF_VM)       :: vm_parent        ! reference to the parent VM

    integer             :: npetlist         ! number of PETs in petlist
    integer, pointer    :: petlist(:)       ! list of usable parent PETs 
    
    type(ESMF_VMPlan)   :: vmplan           ! reference to VMPlan
    type(ESMF_Pointer)  :: vm_info          ! holding pointer to info
    type(ESMF_Pointer)  :: vm_cargo         ! holding pointer to cargo
    integer             :: vm_recursionCount  ! keep track of recursion level

    type(ESMF_State)    :: is, es   ! hold state args refs for thread-safety
    type(ESMF_Clock)    :: argclock ! hold clock arg ref for thread-safety
    
    logical  :: iAmParticipant     ! .false. : PET does not participate
                                   ! .true.  : PET participates in comp

    logical             :: vm_released      ! flag whether vm is running
    real(ESMF_KIND_R8)  :: startTime        ! startTime used for timeouts

    type(ESMF_Context_Flag)   :: contextflag      ! contextflag
    type(ESMF_CompStatus)     :: compStatus       ! isPresent bits
    
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_CplComp
!
! ! Cplcomp wrapper

  type ESMF_CplComp
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
#endif
    !private
    type(ESMF_CompClass), pointer :: compp
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_GridComp
!
! ! GridComp wrapper

  type ESMF_GridComp
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
#endif
    !private
    type(ESMF_CompClass), pointer :: compp
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_SciComp
!
! ! SciComp wrapper

  type ESMF_SciComp
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
#endif
    !private
    type(ESMF_CompClass), pointer :: compp
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_GridComp, ESMF_CplComp, ESMF_SciComp

  public ESMF_Method_Flag, ESMF_METHOD_NONE
  public ESMF_METHOD_INITIALIZE, ESMF_METHOD_RUN, ESMF_METHOD_FINALIZE
  public ESMF_METHOD_WRITERESTART, ESMF_METHOD_READRESTART
  public ESMF_METHOD_SERVICELOOP
  public ESMF_METHOD_INITIALIZEIC, ESMF_METHOD_RUNIC, ESMF_METHOD_FINALIZEIC
  public ESMF_METHOD_WRITERESTARTIC, ESMF_METHOD_READRESTARTIC
  public ESMF_METHOD_SERVICELOOPIC
  public ESMF_METHOD_SETVM, ESMF_METHOD_SETSERVICES, ESMF_METHOD_WAIT
  
  ! These have to be public so other component types can use them, but 
  ! are not intended to be used outside the Framework code.
  public ESMF_CompClass, ESMF_CWrap
  public ESMF_CompType_Flag
  public ESMF_COMPTYPE_GRID, ESMF_COMPTYPE_CPL, ESMF_COMPTYPE_SCI
  public ESMF_CompStatus
  public ESMF_COMPSTATUS_ALL_PRESENT, ESMF_COMPSTATUS_ALL_NOTPRESENT

  public ESMF_CompTunnel

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
  ! These have to be public so other component types can call them,
  ! but they are not intended to be used outside the Framework code.
  public ESMF_CompClassGetInit 
  public ESMF_CompClassSetInitCreated
  public ESMF_CompClassValidate

  public operator(==), operator(/=)

  public ESMF_CompConstruct
  public ESMF_CompDestruct
  public ESMF_CompExecute
  public ESMF_CompGet
  public ESMF_CompIsPetLocal
  public ESMF_CompIsDualConnected
  public ESMF_CompPrint
  public ESMF_CompSet
  public ESMF_CompSetVMMaxPEs
  public ESMF_CompSetVMMaxThreads
  public ESMF_CompSetVMMinThreads
  public ESMF_CompValidate
  public ESMF_CompWait

  public ESMF_CWrapSetInitCreated
  
  public ESMF_CompStatusGet

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'
!------------------------------------------------------------------------------

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
  interface operator (==)
    module procedure ESMF_meeq
    module procedure ESMF_cteq
  end interface
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  interface operator (/=)
    module procedure ESMF_mene
    module procedure ESMF_ctne
  end interface
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------

!  integer, parameter :: ESMF_DEFAULT_TIMEOUT = 3600
  integer, parameter :: ESMF_DEFAULT_TIMEOUT = 300 ! Temporary

!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompClassGetInit"
!BOPI
! !IROUTINE: ESMF_CompClassGetInit - Internal access routine for init code
!
! !INTERFACE:
  recursive function ESMF_CompClassGetInit(cc) result (CompClassGetInit)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: CompClassGetInit   
!
! !ARGUMENTS:
    type(ESMF_CompClass), intent(in), optional :: cc
!
! !DESCRIPTION:
! Access deep object init code.
!
! The arguments are:
! \begin{description}
! \item[cc]
!   CompClass object.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if(present(cc)) then
      CompClassGetInit = ESMF_INIT_GET(cc)
    else
      CompClassGetInit = ESMF_INIT_CREATED
    endif
  end function ESMF_CompClassGetInit
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompClassSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_CompClassSetInitCreated - Set CompClass init code to "CREATED"

! !INTERFACE:
  recursive subroutine ESMF_CompClassSetInitCreated(cc, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), intent(inout)           :: cc
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
! Set init code in CompClass object to "CREATED".
!
! The arguments are:
! \begin{description}
! \item[cc] 
!   Specified {\tt ESMF\_CompClass} object.
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(cc)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CompClassSetInitCreated
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompClassValidate()"
!BOPI
! !IROUTINE: ESMF_CompClassValidate - Validate CompClass internals

! !INTERFACE:
  subroutine ESMF_CompClassValidate(cc, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), intent(in)              :: cc
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
! Validates that the {\tt CompClass} is internally consistent.
! The method returns an error code if problems are found.  
!
! The arguments are:
! \begin{description}
! \item[cc] 
!   Specified {\tt ESMF\_CompClass} object.
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume not implemented until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, cc, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    !todo: call c_ESMC_CompClassValidate(cc, localrc)
    
    ! Use LogErr to handle return code (ESMF_SUCCESS for Validate)
    ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
    !   ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_CompClassValidate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! function to compare two ESMF_Method derived types to see if they're the same 

  function ESMF_meeq(me1, me2)
    logical ESMF_meeq
    type(ESMF_Method_Flag), intent(in) :: me1, me2
    ESMF_meeq = (me1%method == me2%method)    
  end function

  function ESMF_mene(me1, me2)
    logical ESMF_mene
    type(ESMF_Method_Flag), intent(in) :: me1, me2
    ESMF_mene = (me1%method /= me2%method)
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! function to compare two ESMF_CompType_Flags to see if they're the same 

  function ESMF_cteq(ct1, ct2)
    logical ESMF_cteq
    type(ESMF_CompType_Flag), intent(in) :: ct1, ct2
    ESMF_cteq = (ct1%ctype == ct2%ctype)    
  end function

  function ESMF_ctne(ct1, ct2)
    logical ESMF_ctne
    type(ESMF_CompType_Flag), intent(in) :: ct1, ct2
    ESMF_ctne = (ct1%ctype /= ct2%ctype)
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompConstruct"
!BOPI
! !IROUTINE: ESMF_CompConstruct - Internal routine to fill in a comp struct

! !INTERFACE:
  recursive subroutine ESMF_CompConstruct(compp, compType, name, &
    dirPath, configFile, config, clock, petlist, contextflag, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass),     pointer               :: compp
    type(ESMF_CompType_Flag), intent(in)            :: compType
    character(len=*),         intent(in),  optional :: name
    character(len=*),         intent(in),  optional :: dirPath
    character(len=*),         intent(in),  optional :: configFile
    type(ESMF_Config),        intent(in),  optional :: config
    type(ESMF_Clock),         intent(in),  optional :: clock
    integer,                  intent(in),  optional :: petlist(:)
    type(ESMF_Context_Flag),  intent(in),  optional :: contextflag
    integer,                  intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Take a new component datatype and fill in the contents.
!
!  The arguments are:
!  \begin{description}
!   \item[compp]
!    Component internal structure to be filled in.
!   \item[compType]
!    Component type, where type is {\tt ESMF\_GRIDCOMP} or {\tt ESMF\_CPLCOMP}.
!   \item[{[name]}]
!    Component name.
!   \item[{[dirPath]}]
!    Directory where component-specfic configuration or data files
!    are located.
!   \item[{[configFile]}]
!    File containing configuration information, either absolute filename
!    or relative to {\tt dirPath}.
!   \item[{[config]}]
!    Already created {\tt config} object.
!   \item[{[clock]}]
!    Private {\tt clock} for this {\tt Component}.
!   \item[{[petlist]}]
!    List of {\tt PET}s for this component. The default is to use all PETs.
!   \item[{[contextflag]}]
!    Specify the component's VM context. The default context is
!    {\tt ESMF\_CONTEXT\_OWN\_VM}. See section \ref{const:contextflag} for a
!    complete list of options.
!   \item[{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: npets, mypet, i, petCount
    integer, pointer :: petlist_loc(:)
    character(len=ESMF_MAXPATHLEN) :: fullpath    ! config file + dirPath
    character(len=ESMF_MAXSTR)     :: msgbuf
    type(ESMF_VM):: vm

    ! Assume not implemented until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Set values for the derived type members
    compp%ftable = ESMF_NULL_POINTER
    compp%base%this = ESMF_NULL_POINTER
    compp%compType = compType
    compp%compTunnel%this = ESMF_NULL_POINTER
    compp%configFile = "uninitialized"
    compp%dirPath = "uninitialized"
    compp%npetlist = 0
    nullify(compp%compw%compp)
    nullify(compp%petlist)
    compp%vm_info = ESMF_NULL_POINTER
    compp%vm_cargo = ESMF_NULL_POINTER
    compp%vm_recursionCount = 0
    nullify(compp%is%statep)
    nullify(compp%es%statep)
    compp%vm_released = .false.
    compp%contextflag = ESMF_CONTEXT_OWN_VM
    
    compp%compStatus = ESMF_COMPSTATUS_ALL_NOTPRESENT

    ! parent VM
    call ESMF_VMGetCurrent(vm=compp%vm_parent, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! own VM
    call ESMF_VMSetThis(compp%vm, ESMF_NULL_POINTER, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_VMSetInitCreated(compp%vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! for config files, store a directory path and subsequent opens can
    ! be relative to this or absolute.
    if (present(dirPath)) then
      compp%dirPath = dirPath
    else
      compp%dirPath = "."
    endif

    ! config handling
    if (present(config)) then
      compp%config = config
      compp%compStatus%configIsPresent = .true.
      if (present(configFile)) then
        ! a config object gets priority over a name if both are specified.
        call ESMF_LogWrite("Ignoring configFile because config object given.", &
          ESMF_LOGMSG_WARNING)
      endif
    else if (present(configFile)) then
      ! name of a specific config file.  open it and store the config object.
      compp%configFile = configFile
      compp%compStatus%configFileIsPresent = .true.
      compp%config = ESMF_ConfigCreate(rc=localrc)
      compp%compStatus%configIsPresent = .true.
      call ESMF_ConfigLoadFile(compp%config, configFile, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
        ! try again with the dirPath concatinated on front
        fullpath = trim(compp%dirPath) // '/' // trim(configFile)
        call ESMF_ConfigLoadFile(compp%config, fullpath, rc=localrc)
        ! TODO: construct a msg string and then call something here.
        ! if (ESMF_LogFoundError(status, msgstr, rc)) return
        if (localrc /= ESMF_SUCCESS) then
          write(msgbuf, *) &
            "ERROR: loading config file, unable to open either", &
            " name = ", trim(configFile), " or name = ", trim(fullpath)
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg=msgbuf, &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      endif
    endif

    ! clock
    if (present(clock)) then
      compp%clock = clock
      compp%compStatus%clockIsPresent = .true.
    endif

    ! petlist
    if (present(petlist)) then
      compp%npetlist = size(petlist)
      allocate(petlist_loc(compp%npetlist), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="local petlist", &
        ESMF_CONTEXT, rcToReturn=rc)) return 
      compp%petlist => petlist_loc
      compp%petlist = petlist     ! copy contents of petlist
    else
      compp%npetlist = 0
      allocate(compp%petlist(1), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="local petlist", &
        ESMF_CONTEXT, rcToReturn=rc)) return 
    endif

    ! check for consistency between contextflag and petlist
    call ESMF_VMGet(vm=compp%vm_parent, localPet=mypet, petCount=npets, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(contextflag)) then
      if (contextflag==ESMF_CONTEXT_PARENT_VM) then
        if ((compp%npetlist .gt. 0) .and. (compp%npetlist .lt. npets)) then
          ! conflict between contextflag and petlist -> bail out
          deallocate(compp%petlist) ! local garbage collection for bail-on-error
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg="Conflict between contextflag and petlist arguments", &
            ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
      compp%contextflag = contextflag
    else
      compp%contextflag = ESMF_CONTEXT_OWN_VM    ! default
    endif
    
    ! check for conflict between petlist and current VM petCount
    if (compp%npetlist .gt. 0) then
      call ESMF_VMGetCurrent(vm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      ! see if pets in the petlist are negative or if they exist
      do i=1, compp%npetlist
        if ((compp%petlist(i) .ge. petCount) .or. (compp%petlist(i) .lt. 0)) then
          deallocate(compp%petlist) ! local garbage collection for bail-on-error
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg="Conflict between petlist and global pet count", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      enddo
    endif

    ! initialize base class, including component name
    call ESMF_BaseCreate(compp%base, "Component", name, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set the participation flag
    compp%iAmParticipant = .false.  ! reset
    if (compp%npetlist .gt. 0) then
      ! see if mypet is in the petlist
      do i=1, compp%npetlist
        if (compp%petlist(i) == mypet) compp%iAmParticipant = .true.  ! set
      enddo
    else
      ! no petlist -> all PETs participate
      compp%iAmParticipant = .true. ! set
    endif

    ! instantiate a default VMPlan
    call ESMF_VMPlanConstruct(vmplan=compp%vmplan, vm=compp%vm_parent, &
      npetlist=compp%npetlist, petlist=compp%petlist, &
      contextflag=compp%contextflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
                              
    ! initialize the remaining VM members in compp
    compp%vm_info = ESMF_NULL_POINTER
    compp%vm_cargo = ESMF_NULL_POINTER
    compp%vm_released = .false.
                              
    ! Create an empty subroutine/internal state table.
    call c_ESMC_FTableCreate(compp%ftable, localrc) 
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! create methodTable object
    call c_ESMC_MethodTableCreate(compp%methodTable, localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(compp)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompConstruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompDestruct"
!BOPI
! !IROUTINE: ESMF_CompDestruct - Release resources associated with a Component

! !INTERFACE:
  recursive subroutine ESMF_CompDestruct(compp, interCompComm, fullShutdown, &
    timeout, timeoutFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    logical,              intent(in),  optional :: interCompComm
    logical,              intent(in),  optional :: fullShutdown
    integer,              intent(in),  optional :: timeout
    logical,              intent(out), optional :: timeoutFlag
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_Component}, releasing the resources associated
!   with the object.
!
!   The arguments are:
!   \begin{description}
!   \item[compp]
!     Component internal structure to be freed.
!   \item[{[interCompComm]}]
!     Participate in inter-component wrap up communication. May require that
!     this call be not collective! Default is {\tt .true.}.
!   \item[{[fullShutdown]}]
!     Fully shut down everything, including the component's VM. Depending on
!     the MPI implementation this may make this call collective.
!     Default is {\tt .true.}.
!   \item[{[timeout]}]
!     The maximum period in seconds that this call will wait for any
!     communication with the actual component, before returning with a timeout
!     condition. The default is 3600, i.e. 1 hour.
!   \item[{[timeoutFlag]}]
!     Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!     If {\tt timeoutFlag} was not provided a timeout condition will lead to
!     an {\tt rc \\= ESMF\_SUCCESS}, otherwise the return value of
!     {\tt timeoutFlag} is the indicator whether timeout was reached or not.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    type(ESMF_Status) :: baseStatus
    integer :: timeoutArg
    logical :: interCompCommArg
    logical :: fullShutdownArg
    
    ! Assume not implemented until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    
    ! Set defaults
    if (present(timeoutFlag)) then
      timeoutFlag = .false. ! initialize in any case
    endif
    interCompCommArg = .true.
    if (present(interCompComm)) interCompCommArg = interCompComm
    fullShutdownArg = .true.
    if (present(fullShutdown)) fullShutdownArg = fullShutdown

    ! Now deal with garbage collection
    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus == ESMF_STATUS_READY) then
    
      ! dual component must terminate the service loop of the actual component
      if (interCompCommArg .and. &
        (compp%compTunnel%this /= ESMF_NULL_POINTER)) then
        ! this is indeed a dual component with an open component tunnel
        timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h timeout !!!!!!!!!!!
        if (present(timeout)) timeoutArg = timeout
        call ESMF_CompExecute(compp, method=ESMF_METHOD_NONE, &
          timeout=timeoutArg, rc=localrc) ! disregard userRc - invalid here!
        if (present(timeoutFlag)) then
          timeoutFlag = .false. ! initialize
          if ((localrc==ESMF_RC_TIMEOUT).or.(localrc==ESMC_RC_TIMEOUT)) then
            timeoutFlag = .true.      ! indicate timeout through flag argument
            localrc = ESMF_SUCCESS    ! do not raise error condition on user level
          endif
        endif
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
          
        ! call the tunnel destructor
        call c_ESMC_CompTunnelDestroy(compp%compTunnel, localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    
      if (fullShutdownArg) then

        if (compp%vm_info /= ESMF_NULL_POINTER) then
          ! shut down this component's VM
          call ESMF_VMShutdown(vm=compp%vm_parent, vmplan=compp%vmplan, &
            vm_info=compp%vm_info, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! destruct the VMPlan
        call ESMF_VMPlanDestruct(vmplan=compp%vmplan, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! deallocate space held for petlist
        deallocate(compp%petlist, stat=localrc)
        if (ESMF_LogFoundDeallocError(localrc, msg="local petlist", &
          ESMF_CONTEXT, rcToReturn=rc)) return 

        ! call C++ to release function and data pointer tables.
        call c_ESMC_FTableDestroy(compp%ftable, localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! Release attributes on config
        if(compp%configFile /= "uninitialized" ) then !TODO use is present here
          call ESMF_ConfigDestroy(compp%config, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! destroy the methodTable object
        call c_ESMC_MethodTableDestroy(compp%methodTable, localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
      endif

    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompDestruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompExecute"
!BOPI
! !IROUTINE: ESMF_CompExecute -- Call into registered component method

! !INTERFACE:
  recursive subroutine ESMF_CompExecute(compp, method, &
    importState, exportState, clock, syncflag, phase, port, timeout, userRc, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CompClass),    pointer                 :: compp
    type(ESMF_Method_Flag),  intent(in)              :: method
    type(ESMF_State),        intent(inout), optional :: importState
    type(ESMF_State),        intent(inout), optional :: exportState
    type(ESMF_Clock),        intent(in),    optional :: clock
    type(ESMF_Sync_Flag),    intent(in),    optional :: syncflag
    integer,                 intent(in),    optional :: phase
    integer,                 intent(in),    optional :: port
    integer,                 intent(in),    optional :: timeout
    integer,                 intent(out),   optional :: userRc
    integer,                 intent(out),   optional :: rc
!
! !DESCRIPTION:
! Component Execute method used by GridComp and CplComp for:
!   * Initialize,
!   * Run,
!   * Finalize,
!   * ReadRestart,
!   * WriteRestart.
!
! Call into the associated user code for a component's method.
!
! The arguments are:
! \begin{description}
!
! \item[compp]
!   Component to call Initialization routine for.
! \item[method]
!   One of the ESMF Component methods. See section \ref{const:methods} 
!   for a complete list of valid methods.
! \item[{[importState]}]  
!   Import data for component method.
! \item[{[exportState]}]  
!   Export data for component method.
! \item[{[clock]}]  
!   External clock for passing in time information.
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS.
! \item[{[phase]}]
!   The phase of a multi-phase method. Default is 1.
! \item[{[port]}]
!   Port number. Only used for ESMF\_METHOD\_SERVICELOOP.
! \item[{[timeout]}]
!   Time out in seconds.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: localUserRc  ! return code from user code
    type(ESMF_Sync_Flag)    :: blocking     ! local blocking flag
    type(ESMF_VM)           :: vm           ! VM for current context
    integer                 :: phaseArg
    integer                 :: portArg
    integer                 :: timeoutArg
    real(ESMF_KIND_R8)      :: usedTime
        
    ! dummys that will provide initializer values if args are not present
    type(ESMF_State)        :: dummyis, dummyes
    type(ESMF_Clock)        :: dummyclock
    type(ESMF_Status)       :: baseStatus

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        
    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus /= ESMF_STATUS_READY) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif
    
    ! check if this is a supported combination of conditions
    if (compp%vm_released.and. &
      (method/=ESMF_METHOD_WAIT).and. &
      (method/=ESMF_METHOD_NONE)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="cannot call this method while the component is executing", &
        ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! set the default mode to ESMF_SYNC_VASBLOCKING
    if (present(syncflag)) then
      blocking = syncflag
    else
      blocking = ESMF_SYNC_VASBLOCKING
    endif
    
    ! supply default objects if unspecified by the caller
    if (present(importState)) then
      compp%is = importState
      compp%compStatus%isIsPresent = .true.
    else
      ! use dummy variable
      compp%is = dummyis
    endif

    if (present(exportState)) then
      compp%es = exportState
      compp%compStatus%esIsPresent = .true.
    else
      ! use dummy variable
      compp%es = dummyes
    endif

    ! and something for clocks?
    if (present(clock)) then
      compp%argclock = clock
    else
      ! use dummy variable -> set to null pointer since this is deep C++ impl.
      call ESMF_ClockSetThis(dummyclock, ESMF_NULL_POINTER, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      compp%argclock = dummyclock
    endif

    ! set phase and port number
    phaseArg = 1 ! default phase
    if (present(phase)) phaseArg = phase
    
    if ((method==ESMF_METHOD_SERVICELOOP) .or. &
      (method==ESMF_METHOD_SERVICELOOPIC)) then
      ! deal with special phase/port argument combination
      if (phaseArg /= 1) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Phase must be 1 for ServiceLoop() call.", &
          ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
      if (present(port)) then
        if (port < 1024 .or. port > 65535) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="The 'port' argument is outside valid range [1024, 65535]", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
        portArg = port    ! valid port number
      else
        portArg = -1      ! indicate that no port was specified
      endif
    else
      ! all other component methods have regular phase arguments
      if (present(port)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Port is only allowed for ServiceLoop() call.", &
          ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
      portArg = -1        ! indicate that no port was specified
    endif
    
    ! Timeout argument
    timeoutArg = 0; ! default timeout to flag issue if it is really used later
    if (present(timeout)) then
      if (timeout < 0) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="The 'timeout' argument must be positive", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      timeoutArg = timeout    ! valid timeout
    endif

    ! Wrap comp so it's passed to C++ correctly.
    compp%compw%compp => compp
    ESMF_INIT_SET_CREATED(compp%compw)

    ! Set up the arguments
    if (compp%iAmParticipant) then
      ! only call this on PETs that participate
      call c_ESMC_FTableSetStateArgs(compp%ftable, method, phaseArg, &
        compp%compw, compp%is, compp%es, compp%argclock, compp%compTunnel, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    localUserRc = ESMF_SUCCESS  ! initialize to success
    ! pass back the initialized value of userRc, just in case of a bail out
    if (present(userRc)) userRc = localUserRc
    
    ! All of the participating PETs must call in, but also non-participating
    ! PETs that hold a valid VM and show up here enter the callback mechanism.
    if (compp%iAmParticipant .or. compp%compStatus%vmIsPresent) then
      ! store the start time
      call ESMF_VMWtime(compp%startTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! callback into user code
!print *, "ESMF_CompExecute(), calling c_ESMC_FTableCallEntryPointVM(): timeoutArg=",timeoutArg
      call c_ESMC_FTableCallEntryPointVM(compp%compw, compp%vm_parent, &
        compp%vmplan, compp%vm_info, compp%vm_cargo, compp%ftable, method, &
        phaseArg, portArg, timeoutArg, compp%vm_recursionCount, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! For threaded VMs (single- or multi-threaded) the child VM will 
      ! now be running concurrently with the parent VM.
      ! Also for component tunnels, the actual component will now be executing
      ! concurrently with the dual component that came in to this call.

      ! wait for blocking modes
      if (blocking == ESMF_SYNC_VASBLOCKING .or. blocking == ESMF_SYNC_BLOCKING) then
        ! wait for all child PETs that run in this parent's PET VAS to finish
        ! determine how long the component has been released already
        call ESMF_VMWTime(usedTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        usedTime = usedTime - compp%startTime
        ! allow remaining time for timeout, but at least 1 second to wrap up
        timeoutArg = max(timeoutArg - int(usedTime), 1)
!print *, "ESMF_CompExecute(), calling c_ESMC_CompWait(): usedTime=",usedTime,"timeoutArg=",timeoutArg
        call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
          compp%vm_cargo, timeoutArg, compp%vm_recursionCount, localUserRc, &
          localrc)
        ! localUserRc - return code of registered user callback method
        ! localrc     - return code of ESMF internal callback stack
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif
      
    ! sync PETs according to blocking mode
    if (blocking == ESMF_SYNC_NONBLOCKING) then
      compp%vm_released = .true.
    else
      compp%vm_released = .false.       ! indicate child VM has been caught
      ! for ESMF_SYNC_BLOCKING _all_ parent PETs will be synced on exit
      if (blocking == ESMF_SYNC_BLOCKING) then
        ! the current context _is_ the parent context...
        call ESMF_VMGetCurrent(vm=vm, rc=localrc)  ! determine current VM
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMBarrier(vm=vm, rc=localrc) ! barrier across parent VM
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompExecute
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompGet"
!BOPI
! !IROUTINE: ESMF_CompGet -- Query a component for various information
!
! !INTERFACE:
  recursive subroutine ESMF_CompGet(compp, name, vm, vm_parent, vmplan, &
    vm_info, contextflag, grid, gridList, mesh, meshList, locstream, &
    locstreamList, xgrid, xgridList, importState, exportState, clock, dirPath, &
    configFile, config, compType, currentMethod, currentPhase, timeout, &
    localPet, petCount, petList, compStatus, compTunnel, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass),     pointer               :: compp
    character(len=*),         intent(out), optional :: name
    type(ESMF_VM),            intent(out), optional :: vm
    type(ESMF_VM),            intent(out), optional :: vm_parent
    type(ESMF_VMPlan),        intent(out), optional :: vmplan
    type(ESMF_Pointer),       intent(out), optional :: vm_info
    type(ESMF_Context_Flag),  intent(out), optional :: contextflag
    type(ESMF_Grid),          intent(out), optional :: grid
    type(ESMF_Grid), allocatable, intent(out), optional :: gridList(:)
    type(ESMF_Mesh),          intent(out), optional :: mesh
    type(ESMF_Mesh), allocatable, intent(out), optional :: meshList(:)
    type(ESMF_LocStream),     intent(out), optional :: locstream
    type(ESMF_LocStream), allocatable, intent(out), optional :: locstreamList(:)
    type(ESMF_XGrid),         intent(out), optional :: xgrid
    type(ESMF_XGrid), allocatable, intent(out), optional :: xgridList(:)
    type(ESMF_State),         intent(out), optional :: importState
    type(ESMF_State),         intent(out), optional :: exportState
    type(ESMF_Clock),         intent(out), optional :: clock
    character(len=*),         intent(out), optional :: dirPath
    character(len=*),         intent(out), optional :: configFile
    type(ESMF_Config),        intent(out), optional :: config
    type(ESMF_CompType_Flag), intent(out), optional :: compType
    type(ESMF_Method_Flag),   intent(out), optional :: currentMethod
    integer,                  intent(out), optional :: currentPhase
    integer,                  intent(out), optional :: timeout
    integer,                  intent(out), optional :: localPet
    integer,                  intent(out), optional :: petCount
    integer,                  pointer,     optional :: petList(:)
    type(ESMF_CompStatus),    intent(out), optional :: compStatus
    type(ESMF_CompTunnel),    intent(out), optional :: compTunnel
    integer,                  intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns information about the component.
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc, stat      ! local return code
    type(ESMF_Status)       :: baseStatus
    type(ESMF_Method_Flag)  :: currentMethodArg
    integer                 :: currentPhaseArg
    integer                 :: timeoutArg

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        
    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus /= ESMF_STATUS_READY) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="uninitialized or destroyed Component object.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail out
    endif

    ! access grid
    if (present(grid)) then
      if (.not.compp%compStatus%gridIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested Grid object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      grid = compp%gridList(1)  ! return first element
    endif
    if (present(gridList)) then
      if (allocated(gridList)) deallocate(gridList) ! deallocate incoming
      allocate(gridList(size(compp%gridList)))      ! allocate to correct size
      gridList(:) = compp%gridList(:)               ! copy entries
    endif

    ! access mesh
    if (present(mesh)) then
      if (.not.compp%compStatus%meshIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested Mesh object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      mesh = compp%meshList(1)  ! return first element
    endif
    if (present(meshList)) then
      if (allocated(meshList)) deallocate(meshList) ! deallocate incoming
      allocate(meshList(size(compp%meshList)))      ! allocate to correct size
      meshList(:) = compp%meshList(:)               ! copy entries
    endif

    ! access locstream
    if (present(locstream)) then
      if (.not.compp%compStatus%locstreamIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested locstream object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      locstream = compp%locstreamList(1)  ! return first element
    endif
    if (present(locstreamList)) then
      if (allocated(locstreamList)) deallocate(locstreamList) ! deallocate incoming
      allocate(locstreamList(size(compp%locstreamList)))      ! allocate to correct size
      locstreamList(:) = compp%locstreamList(:)               ! copy entries
    endif

    ! access xgrid
    if (present(xgrid)) then
      if (.not.compp%compStatus%xgridIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested xgrid object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      xgrid = compp%xgridList(1)  ! return first element
    endif
    if (present(xgridList)) then
      if (allocated(xgridList)) deallocate(xgridList) ! deallocate incoming
      allocate(xgridList(size(compp%xgridList)))      ! allocate to correct size
      xgridList(:) = compp%xgridList(:)               ! copy entries
    endif

    ! access config
    if (present(config)) then
      if (.not.compp%compStatus%configIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested Config object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      config = compp%config
    endif

    ! access name
    if (present(name)) then
      call ESMF_GetName(compp%base, name, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! access compType
    if (present(compType)) then
      compType = compp%compType
    endif

    ! access vm
    if (present(vm)) then
      if (.not.compp%compStatus%vmIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested VM object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      vm = compp%vm
    endif

    ! access vm_parent
    if (present(vm_parent)) then
      vm_parent = compp%vm_parent
    endif

    ! access vmplan
    if (present(vmplan)) then
      vmplan = compp%vmplan
    endif

    ! access vm_info
    if (present(vm_info)) then
      vm_info = compp%vm_info
    endif

    ! access contextflag
    if (present(contextflag)) then
      contextflag = compp%contextflag
    endif

    ! access importState
    if (present(importState)) then
      if (.not.compp%compStatus%isIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested importState object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      importState = compp%is
    endif

    ! access exportState
    if (present(exportState)) then
      if (.not.compp%compStatus%esIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested exportState object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      exportState = compp%es
    endif

    ! access clock
    if (present(clock)) then
      if (.not.compp%compStatus%clockIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested Clock object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      clock = compp%clock
    endif

    ! access dirPath
    if (present(dirPath)) then
      dirPath = compp%dirPath
    endif

    ! access configFile
    if (present(configFile)) then
      if (.not.compp%compStatus%configFileIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="requested configFile object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      configFile = compp%configFile
    endif

    ! access currentMethod, currentPhase, timeout
    if (present(currentMethod) &
      .or. present(currentPhase) &
      .or. present(timeout)) then
      call c_ESMC_CompGet(compp%vm_cargo, currentMethodArg, currentPhaseArg, &
        timeoutArg, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(currentMethod)) then
      currentMethod = currentMethodArg
    endif
    if (present(currentPhase)) then
      currentPhase = currentPhaseArg
    endif
    if (present(timeout)) then
      timeout = timeoutArg
    endif

    ! access localPet
    if (present(localPet)) then
      if (.not.compp%compStatus%vmIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="VM object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      call ESMF_VMGet(compp%vm, localPet=localPet, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! access petCount
    if (present(petCount)) then
      if (.not.compp%compStatus%vmIsPresent) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="VM object is not present.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
      call ESMF_VMGet(compp%vm, petCount=petCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! access petList
    if (present(petList)) then
      if (associated(petList)) then
        if (size(petList) /= compp%npetlist) then
          call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
            msg="- size of provided petList argument does not match.", &
            ESMF_CONTEXT, rcToReturn=rc)
          return  ! bail out
        endif
      else
        allocate(petlist(compp%npetlist), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg="local petlist", &
          ESMF_CONTEXT, rcToReturn=rc)) return 
      endif
      petList = compp%petList ! copy the petList content
    endif

    ! access compStatus
    if (present(compStatus)) then
      compStatus = compp%compStatus
    endif

    ! access compTunnel
    if (present(compTunnel)) then
      compTunnel = compp%compTunnel
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompIsPetLocal"
!BOPI
! !IROUTINE: ESMF_CompIsPetLocal -- Inquire if this component is to execute on the calling PET.
!
! !INTERFACE:
  recursive function ESMF_CompIsPetLocal(compp, rc)
!
! !RETURN VALUE:
    logical :: ESMF_CompIsPetLocal
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    integer,              intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Inquire if this component is to execute on the calling PET.
!
!  The return value is {\tt .true.} if the component is to execute on the 
!  calling PET, {\tt .false.} otherwise.
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Status)       :: baseStatus

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        
    ! Initialize output in case of error
    ESMF_CompIsPetLocal = .false.

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus /= ESMF_STATUS_READY) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    
    ESMF_CompIsPetLocal = compp%iAmParticipant
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_CompIsPetLocal
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompIsDualConnected"
!BOPI
! !IROUTINE: ESMF_CompIsDualConnected -- Inquire if this component a connected dual component
!
! !INTERFACE:
  recursive function ESMF_CompIsDualConnected(compp, rc)
!
! !RETURN VALUE:
    logical :: ESMF_CompIsDualConnected
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    integer,              intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Inquire if this component is a connected dual component.
!
!  The return value is {\tt .true.} if the component is a connected dual
!  component, {\tt .false.} otherwise.
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Status)       :: baseStatus

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        
    ! Initialize output in case of error
    ESMF_CompIsDualConnected = .false.

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus /= ESMF_STATUS_READY) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    
    ESMF_CompIsDualConnected = (compp%compTunnel%this /= ESMF_NULL_POINTER)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_CompIsDualConnected
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompPrint"
!BOPI
! !IROUTINE:  ESMF_CompPrint -- Print the contents of a Component
!
! !INTERFACE:
  recursive subroutine ESMF_CompPrint(compp, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    integer,              intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Routine to print information about a component. \\
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!     may be used on unit 6 to get coherent output.  \\
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code
    character(len=6)            :: defaultopts
    character(len=ESMF_MAXSTR)  :: cname
    type(ESMF_Status)           :: baseStatus

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    defaultopts = "brief"

    if (.not.associated(compp)) then
      !nsc  call ESMF_LogWrite("Invalid or uninitialized Component",  &
      !nsc                      ESMF_LOGMSG_INFO)
      write (ESMF_UtilIOStdout,*)  "Invalid or uninitialized Component"
      return
    endif


    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus /= ESMF_STATUS_READY) then
      !nsc  call ESMF_LogWrite("Invalid or uninitialized Component",  &
      !nsc                      ESMF_LOGMSG_INFO)
      write (ESMF_UtilIOStdout,*)  "Invalid or uninitialized Component"
      return
    endif

    call ESMF_GetName(compp%base, cname, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
       
    write (ESMF_UtilIOStdout,*) " Component name = ", trim(cname)
    
    ! TODO: add more info here

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompPrint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompSet"
!BOPI
! !IROUTINE: ESMF_CompSet -- Query a component for various information
!
! !INTERFACE:
  recursive subroutine ESMF_CompSet(compp, name, vm, vm_info, grid, gridList, &
    mesh, meshList, locstream, locstreamList, xgrid, xgridList, clock, &
    dirPath, configFile, config, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass),    pointer               :: compp
    character(len=*),        intent(in),  optional :: name
    type(ESMF_VM),           intent(in),  optional :: vm
    type(ESMF_Pointer),      intent(in),  optional :: vm_info
    type(ESMF_Grid),         intent(in),  optional :: grid
    type(ESMF_Grid),         intent(in),  optional :: gridList(:)
    type(ESMF_Mesh),         intent(in),  optional :: mesh
    type(ESMF_Mesh),         intent(in),  optional :: meshList(:)
    type(ESMF_LocStream),    intent(in),  optional :: locstream
    type(ESMF_LocStream),    intent(in),  optional :: locstreamList(:)
    type(ESMF_XGrid),        intent(in),  optional :: xgrid
    type(ESMF_XGrid),        intent(in),  optional :: xgridList(:)
    type(ESMF_Clock),        intent(in),  optional :: clock
    character(len=*),        intent(in),  optional :: dirPath
    character(len=*),        intent(in),  optional :: configFile
    type(ESMF_Config),       intent(in),  optional :: config
    integer,                 intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Sets or resets information about the component.  When the caller
!      only wants to set a single value specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc      ! local return code
    type(ESMF_Status)               :: baseStatus
    character(len=ESMF_MAXPATHLEN)  :: fullpath     ! config file + dirPath
    character(len=ESMF_MAXSTR)      :: msgbuf

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus /= ESMF_STATUS_READY) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(name)) then
      call ESMF_SetName(compp%base, name, "Component", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(vm)) then
      compp%vm = vm
      compp%compStatus%vmIsPresent = .true.
    endif

    if (present(vm_info)) then
      compp%vm_info = vm_info
    endif

    if (present(grid).and.present(gridList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'grid' and 'gridList' arguments are mutually exclusive.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    else if (present(grid)) then
      if (allocated(compp%gridList)) deallocate(compp%gridList)
      allocate(compp%gridList(1))
      compp%gridList(1) = grid
      compp%compStatus%gridIsPresent = .true.
    else if (present(gridList)) then
      if (allocated(compp%gridList)) deallocate(compp%gridList)
      allocate(compp%gridList(size(gridList)))
      compp%gridList = gridList
      if (size(gridList)>0) then
        compp%compStatus%gridIsPresent = .true.
      else
        compp%compStatus%gridIsPresent = .false.
      endif
    endif

    if (present(mesh).and.present(meshList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'mesh' and 'meshList' arguments are mutually exclusive.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    else if (present(mesh)) then
      if (allocated(compp%meshList)) deallocate(compp%meshList)
      allocate(compp%meshList(1))
      compp%meshList(1) = mesh
      compp%compStatus%meshIsPresent = .true.
    else if (present(meshList)) then
      if (allocated(compp%meshList)) deallocate(compp%meshList)
      allocate(compp%meshList(size(meshList)))
      compp%meshList = meshList
      if (size(meshList)>0) then
        compp%compStatus%meshIsPresent = .true.
      else
        compp%compStatus%meshIsPresent = .false.
      endif
    endif

    if (present(locstream).and.present(locstreamList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'locstream' and 'locstreamList' arguments are mutually exclusive.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    else if (present(locstream)) then
      if (allocated(compp%locstreamList)) deallocate(compp%locstreamList)
      allocate(compp%locstreamList(1))
      compp%locstreamList(1) = locstream
      compp%compStatus%locstreamIsPresent = .true.
    else if (present(locstreamList)) then
      if (allocated(compp%locstreamList)) deallocate(compp%locstreamList)
      allocate(compp%locstreamList(size(locstreamList)))
      compp%locstreamList = locstreamList
      if (size(locstreamList)>0) then
        compp%compStatus%locstreamIsPresent = .true.
      else
        compp%compStatus%locstreamIsPresent = .false.
      endif
    endif

    if (present(xgrid).and.present(xgridList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'xgrid' and 'xgridList' arguments are mutually exclusive.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    else if (present(xgrid)) then
      if (allocated(compp%xgridList)) deallocate(compp%xgridList)
      allocate(compp%xgridList(1))
      compp%xgridList(1) = xgrid
      compp%compStatus%xgridIsPresent = .true.
    else if (present(xgridList)) then
      if (allocated(compp%xgridList)) deallocate(compp%xgridList)
      allocate(compp%xgridList(size(xgridList)))
      compp%xgridList = xgridList
      if (size(xgridList)>0) then
        compp%compStatus%xgridIsPresent = .true.
      else
        compp%compStatus%xgridIsPresent = .false.
      endif
    endif

    if (present(clock)) then
      compp%clock = clock
      compp%compStatus%clockIsPresent = .true.
    endif

    if (present(dirPath)) then
      compp%dirPath = dirPath
    endif

    ! config handling
    if (present(config)) then
      compp%config = config
      compp%compStatus%configIsPresent = .true.
      if (present(configFile)) then
        ! a config object gets priority over a name if both are specified.
        call ESMF_LogWrite("Ignoring configFile because config object given.", &
          ESMF_LOGMSG_WARNING)
      endif
    else if (present(configFile)) then
      ! name of a specific config file.  open it and store the config object.
      compp%configFile = configFile
      compp%compStatus%configFileIsPresent = .true.
      compp%config = ESMF_ConfigCreate(rc=localrc)
      compp%compStatus%configIsPresent = .true.
      call ESMF_ConfigLoadFile(compp%config, configFile, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
        ! try again with the dirPath concatinated on front
        fullpath = trim(compp%dirPath) // '/' // trim(configFile)
        call ESMF_ConfigLoadFile(compp%config, fullpath, rc=localrc)
        ! TODO: construct a msg string and then call something here.
        ! if (ESMF_LogFoundError(status, msgstr, rc)) return
        if (localrc /= ESMF_SUCCESS) then
          write(msgbuf, *) &
            "ERROR: loading config file, unable to open either", &
            " name = ", trim(configFile), " or name = ", trim(fullpath)
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg=msgbuf, &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      endif
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompSet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompSetVMMaxPEs"
!BOPI
! !IROUTINE: ESMF_CompSetVMMaxPEs - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompSetVMMaxPEs(compp, max, pref_intra_process, &
    pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    integer,              intent(in),  optional :: max
    integer,              intent(in),  optional :: pref_intra_process
    integer,              intent(in),  optional :: pref_intra_ssi
    integer,              intent(in),  optional :: pref_inter_ssi
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[max]}] 
!          Maximum number of PEs per PET
!     \item[{[pref\_intra\_process]}] 
!          Intra process communication preference
!     \item[{[pref\_intra\_ssi]}] 
!          Intra SSI communication preference
!     \item[{[pref\_inter\_ssi]}] 
!          Inter process communication preference
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    ! ensure that this is not a child_in_parent_vm plan
    if (compp%contextflag == ESMF_CONTEXT_PARENT_VM) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="CompSetVM() calls are incompatible with CHILD_IN_PARENT_VM component", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    
    ! ensure that this component's VM wasn't already created
    if (compp%vm_info /= ESMF_NULL_POINTER) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="CompSetVM() calls cannot be called on components with existing VM", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call CompClass method
    call ESMF_VMPlanMaxPEs(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompSetVMMaxPEs
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompSetVMMaxThreads"
!BOPI
! !IROUTINE: ESMF_CompSetVMMaxThreads - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompSetVMMaxThreads(compp, max, pref_intra_process, &
    pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    integer,              intent(in),  optional :: max
    integer,              intent(in),  optional :: pref_intra_process
    integer,              intent(in),  optional :: pref_intra_ssi
    integer,              intent(in),  optional :: pref_inter_ssi
    integer,              intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[max]}] 
!          Maximum threading level
!     \item[{[pref\_intra\_process]}] 
!          Intra process communication preference
!     \item[{[pref\_intra\_ssi]}] 
!          Intra SSI communication preference
!     \item[{[pref\_inter\_ssi]}] 
!          Inter process communication preference
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    ! ensure that this is not a child_in_parent_vm plan
    if (compp%contextflag == ESMF_CONTEXT_PARENT_VM) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="CompSetVM() calls are incompatible with CHILD_IN_PARENT_VM component", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    
    ! ensure that this component's VM wasn't already created
    if (compp%vm_info /= ESMF_NULL_POINTER) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="CompSetVM() calls cannot be called on components with existing VM", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call CompClass method
    call ESMF_VMPlanMaxThreads(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompSetVMMaxThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompSetVMMinThreads"
!BOPI
! !IROUTINE: ESMF_CompSetVMMinThreads - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompSetVMMinThreads(compp, max, pref_intra_process, &
    pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    integer,              intent(in),  optional :: max
    integer,              intent(in),  optional :: pref_intra_process
    integer,              intent(in),  optional :: pref_intra_ssi
    integer,              intent(in),  optional :: pref_inter_ssi
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
!     Print VM internals
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[max]}] 
!          Maximum number of PEs per PET
!     \item[{[pref\_intra\_process]}] 
!          Intra process communication preference
!     \item[{[pref\_intra\_ssi]}] 
!          Intra SSI communication preference
!     \item[{[pref\_inter\_ssi]}] 
!          Inter process communication preference
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    ! ensure that this is not a child_in_parent_vm plan
    if (compp%contextflag == ESMF_CONTEXT_PARENT_VM) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="CompSetVM() calls are incompatible with CHILD_IN_PARENT_VM component", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    
    ! ensure that this component's VM wasn't already created
    if (compp%vm_info /= ESMF_NULL_POINTER) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="CompSetVM() calls cannot be called on components with existing VM", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call CompClass method
    call ESMF_VMPlanMinThreads(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompSetVMMinThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompValidate"
!BOPI
! !IROUTINE: ESMF_CompValidate -- Ensure the Component internal data is valid.
!
! !INTERFACE:
  recursive subroutine ESMF_CompValidate(compp, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    integer,              intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Status)       :: baseStatus

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus /= ESMF_STATUS_READY) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Unini/destroyed comp", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! TODO: add code here

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompValidate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompWait"
!BOPI
! !IROUTINE: ESMF_CompWait - Wait for component to return

! !INTERFACE:
  subroutine ESMF_CompWait(compp, syncflag, timeout, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    type(ESMF_Sync_Flag), intent(in),  optional :: syncflag
    integer,              intent(in),  optional :: timeout
    integer,              intent(out), optional :: userRc
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
! Wait for component to return
!
! The arguments are:
! \begin{description}
! \item[compp] 
!   component object
! \item[{[syncflag]}]
!   The blocking behavior determines exactly what this call waits for. The
!   default is {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs across each VAS.
!   See section \ref{const:sync} for a list of valid blocking options.
! \item[{[timeout]}]
!   The maximum period in seconds the actual component is allowed to execute
!   a previously invoked component method before it must communicate back to
!   the dual component. If the actual component does not communicate back in
!   the specified time, a timeout condition is raised on the dual side (this
!   side). The default is 3600, i.e. 1 hour.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: localUserRc  ! return code from user code
    type(ESMF_Sync_Flag)    :: blocking     ! local blocking flag
    type(ESMF_VM)           :: vm           ! VM for current context
    type(ESMF_Status)       :: baseStatus
    integer                 :: timeoutArg
    real(ESMF_KIND_R8)      :: usedTime
    
    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check input
    if (.not.associated(compp)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, baseStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (baseStatus /= ESMF_STATUS_READY) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! set the default mode to ESMF_SYNC_VASBLOCKING
    if (present(syncflag)) then
      blocking = syncflag
    else
      blocking = ESMF_SYNC_VASBLOCKING
    endif

    localUserRc = ESMF_SUCCESS  ! initialize to success
    
    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout
    
    ! check if the child VM, i.e. the VM of this component, is currently marked
    ! as running...
    if (compp%vm_released) then
      ! check if the calling PET has a present VM (i.e. was SetServices called)
      if (compp%compStatus%vmIsPresent) then
        ! wait for all child PETs that run in this parent's PET VAS to finish
        ! determine how long the component has been released already
        call ESMF_VMWTime(usedTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        usedTime = usedTime - compp%startTime
        ! allow remaining time for timeout, but at least 1 second to wrap up
        timeoutArg = max(timeoutArg - int(usedTime), 1)
!print *, "ESMF_CompWait(), calling c_ESMC_CompWait(): usedTime=",usedTime,"timeoutArg=",timeoutArg
        call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
          compp%vm_cargo, timeoutArg, compp%vm_recursionCount, localUserRc, &
          localrc)
        ! localUserRc - return code of registered user callback method
        ! localrc     - return code of ESMF internal callback stack
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      ! reset the released flag
      compp%vm_released = .false.       ! indicate child VM has been caught
      ! for ESMF_SYNC_BLOCKING _all_ parent PETs will be synced on exit
      if (blocking == ESMF_SYNC_BLOCKING) then
        ! the current context _is_ the parent context...
        call ESMF_VMGetCurrent(vm=vm, rc=localrc)  ! determine current VM
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMBarrier(vm=vm, rc=localrc) ! barrier across parent VM
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompWait
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CWrapSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_CWrapSetInitCreated - Set CWrap init code to "CREATED"

! !INTERFACE:
  recursive subroutine ESMF_CWrapSetInitCreated(cw, rc)
!
! !ARGUMENTS:
    type(ESMF_CWrap), intent(inout)           :: cw
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in CWrap object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[cw] 
!          Specified {\tt ESMF\_CWrap} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(cw)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CWrapSetInitCreated
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompStatusGetInit"
!BOPI
! !IROUTINE: ESMF_CompStatusGetInit - Internal access routine for init code
!
! !INTERFACE:
  recursive function ESMF_CompStatusGetInit(compStatus) &
    result (CompStatusGetInit)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: CompStatusGetInit
!
! !ARGUMENTS:
    type(ESMF_CompStatus), intent(in), optional :: compStatus
!
! !DESCRIPTION:
!   Access init code.
!
!   The arguments are:
!   \begin{description}
!   \item [connection]
!     CompStatus object.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(compStatus)) then
      CompStatusGetInit = ESMF_INIT_GET(compStatus)
    else
      CompStatusGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_CompStatusGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompStatusGet"
!BOPI
! !IROUTINE: ESMF_CompStatusGet -- Access the status bits
!
! !INTERFACE:
  recursive subroutine ESMF_CompStatusGet(compStatus, clockIsPresent, &
    configIsPresent, configFileIsPresent, vmIsPresent, isIsPresent, &
    esIsPresent, gridIsPresent, meshIsPresent, locstreamIsPresent, &
    xgridIsPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_CompStatus), intent(in)            :: compStatus
    logical,               intent(out), optional :: clockIsPresent
    logical,               intent(out), optional :: configIsPresent
    logical,               intent(out), optional :: configFileIsPresent
    logical,               intent(out), optional :: vmIsPresent
    logical,               intent(out), optional :: isIsPresent
    logical,               intent(out), optional :: esIsPresent
    logical,               intent(out), optional :: gridIsPresent
    logical,               intent(out), optional :: meshIsPresent
    logical,               intent(out), optional :: locstreamIsPresent
    logical,               intent(out), optional :: xgridIsPresent
    integer,               intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns information about the component status bits.
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    
    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ESMF_INIT_CHECK_SHALLOW(ESMF_CompStatusGetInit, compStatus, rc)
    
    if (present(clockIsPresent)) then
      clockIsPresent = compStatus%clockIsPresent
    endif

    if (present(configIsPresent)) then
      configIsPresent = compStatus%configIsPresent
    endif

    if (present(configFileIsPresent)) then
      configFileIsPresent = compStatus%configFileIsPresent
    endif

    if (present(vmIsPresent)) then
      vmIsPresent = compStatus%vmIsPresent
    endif

    if (present(isIsPresent)) then
      isIsPresent = compStatus%isIsPresent
    endif

    if (present(esIsPresent)) then
      esIsPresent = compStatus%esIsPresent
    endif

    if (present(gridIsPresent)) then
      gridIsPresent = compStatus%gridIsPresent
    endif

    if (present(meshIsPresent)) then
      meshIsPresent = compStatus%meshIsPresent
    endif

    if (present(locstreamIsPresent)) then
      locstreamIsPresent = compStatus%locstreamIsPresent
    endif

    if (present(xgridIsPresent)) then
      xgridIsPresent = compStatus%xgridIsPresent
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CompStatusGet
!------------------------------------------------------------------------------

end module ESMF_CompMod
