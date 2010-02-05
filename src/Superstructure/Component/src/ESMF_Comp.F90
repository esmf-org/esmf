! $Id: ESMF_Comp.F90,v 1.196.2.1 2010/02/05 20:04:07 svasquez Exp $
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
  use ESMF_IOSpecMod
  use ESMF_VMMod
  use ESMF_ConfigMod
  use ESMF_CalendarMod
  use ESMF_ClockMod
  use ESMF_GridMod
  use ESMF_StateTypesMod
  use ESMF_StateMod
  use ESMF_InitMacrosMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
! ! ESMF_CompType
!
  type ESMF_CompType
    sequence
    private
    integer :: ctype
  end type

  type(ESMF_CompType), parameter :: &
    ESMF_COMPTYPE_GRID = ESMF_CompType(1), &
    ESMF_COMPTYPE_CPL  = ESMF_CompType(2)

!------------------------------------------------------------------------------
! ! ESMF_GridCompType
!
! ! Model type: Atmosphere, Land, Ocean, SeaIce, River runoff.
!
  type ESMF_GridCompType
    sequence
    private
    integer :: gridcomptype
  end type

  type(ESMF_GridCompType), parameter :: &
    ESMF_ATM    = ESMF_GridCompType(1), &
    ESMF_LAND   = ESMF_GridCompType(2), &
    ESMF_OCEAN  = ESMF_GridCompType(3), &
    ESMF_SEAICE = ESMF_GridCompType(4), &
    ESMF_RIVER  = ESMF_GridCompType(5), &
    ESMF_OTHER  = ESMF_GridCompType(6)

!------------------------------------------------------------------------------
! ! ESMF Method Type
!
  type ESMF_Method
    sequence
    private
    integer :: method
  end type

  type(ESMF_Method), parameter :: &
    ESMF_SETNONE          = ESMF_Method(0), &
    ESMF_SETINIT          = ESMF_Method(1), &
    ESMF_SETRUN           = ESMF_Method(2), &
    ESMF_SETFINAL         = ESMF_Method(3), &
    ESMF_SETWRITERESTART  = ESMF_Method(4), &
    ESMF_SETREADRESTART   = ESMF_Method(5), &
    ESMF_SETVM            = ESMF_Method(6), &
    ESMF_SETSERVICES      = ESMF_Method(7)
    
!------------------------------------------------------------------------------
! ! ESMF Phase number
  integer, parameter :: ESMF_SINGLEPHASE = 1    ! deprecated!!!!

!------------------------------------------------------------------------------
! ! wrapper for Component objects going across F90/C++ boundary
  type ESMF_CWrap
#ifndef ESMF_SEQUENCE_BUG
    sequence
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
    sequence
#endif
    !private
    type(ESMF_Pointer)  :: this             ! C++ ftable pointer - MUST BE FIRST
    type(ESMF_Base)     :: base             ! base class
    type(ESMF_CompType) :: ctype            ! component type
    type(ESMF_Config)   :: config           ! configuration object
    type(ESMF_Clock)    :: clock            ! private component clock
    type(ESMF_Grid)     :: grid             ! default grid, gcomp only
    type(ESMF_GridCompType) :: gridcomptype ! model type, gcomp only

    character(len=ESMF_MAXSTR) :: configFile! resource filename
    character(len=ESMF_MAXSTR) :: dirPath   ! relative dirname, app only

    type(ESMF_CWrap)    :: compw            ! to satisfy the C interface
    type(ESMF_VM)       :: vm               ! component VM
    type(ESMF_VM)       :: vm_parent        ! reference to the parent VM

    integer             :: npetlist         ! number of PETs in petlist
    integer, pointer    :: petlist(:)       ! list of usable parent PETs 
    
    type(ESMF_VMPlan)   :: vmplan           ! reference to VMPlan
    type(ESMF_Pointer)  :: vm_info          ! holding pointer to info
    type(ESMF_Pointer)  :: vm_cargo         ! holding pointer to cargo

    type(ESMF_State)    :: is, es   ! hold state args refs for thread-safety
    type(ESMF_Clock)    :: argclock ! hold clock arg ref for thread-safety
    
    logical  :: iAmParticipant     ! .false. : PET does not participate
                                   ! .true.  : PET participates in comp

    logical             :: vm_released      ! flag whether vm is running

    type(ESMF_ContextFlag) :: contextflag   ! contextflag
    
    type(ESMF_Method)   :: currentMethod    ! current method component is in
    integer             :: currentPhase     ! current phase of method
    
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_CplComp
!
! ! Cplcomp wrapper

  type ESMF_CplComp
#ifndef ESMF_SEQUENCE_BUG
    sequence
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
    sequence
#endif
    !private
    type(ESMF_CompClass), pointer :: compp
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_GridCompType, ESMF_ATM, ESMF_LAND, ESMF_OCEAN, &
    ESMF_SEAICE, ESMF_RIVER, ESMF_OTHER
  public ESMF_Method, ESMF_SETNONE, ESMF_SETINIT, ESMF_SETRUN, ESMF_SETFINAL
  public ESMF_SETWRITERESTART, ESMF_SETREADRESTART
  public ESMF_SETVM, ESMF_SETSERVICES
  
  public ESMF_SINGLEPHASE     ! deprecated!!!!
      
  ! These have to be public so other component types can use them, but 
  ! are not intended to be used outside the Framework code.
  public ESMF_CompClass, ESMF_CWrap
  public ESMF_CompType
  public ESMF_COMPTYPE_GRID, ESMF_COMPTYPE_CPL 
  public ESMF_CplComp, ESMF_GridComp

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
  ! These have to be public so other component types can call them,
  ! but they are not intended to be used outside the Framework code.
  public ESMF_CompClassGetInit 
  public ESMF_CompClassSetInitCreated
  public ESMF_CompClassValidate

  public operator(.eq.), operator(.ne.)

  public ESMF_CompConstruct
  public ESMF_CompDestruct
  public ESMF_CompExecute
  public ESMF_CompGet
  public ESMF_CompIsPetLocal
  public ESMF_CompPrint
  public ESMF_CompSet
  public ESMF_CompSetVMMaxPEs
  public ESMF_CompSetVMMaxThreads
  public ESMF_CompSetVMMinThreads
  public ESMF_CompValidate
  public ESMF_CompWait

  public ESMF_CWrapSetInitCreated

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Comp.F90,v 1.196.2.1 2010/02/05 20:04:07 svasquez Exp $'
!------------------------------------------------------------------------------

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
  interface operator (.eq.)
    module procedure ESMF_meeq
    module procedure ESMF_cteq
    module procedure ESMF_mteq
  end interface
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  interface operator (.ne.)
    module procedure ESMF_mene
    module procedure ESMF_ctne
    module procedure ESMF_mtne
  end interface
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
  function ESMF_CompClassGetInit(cc) 
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_CompClassGetInit   
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
      ESMF_CompClassGetInit = ESMF_INIT_GET(cc)
    else
      ESMF_CompClassGetInit = ESMF_INIT_CREATED
    endif
  end function ESMF_CompClassGetInit
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompClassSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_CompClassSetInitCreated - Set CompClass init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_CompClassSetInitCreated(cc, rc)
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
    ! if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    type(ESMF_Method), intent(in) :: me1, me2
    ESMF_meeq = (me1%method .eq. me2%method)    
  end function

  function ESMF_mene(me1, me2)
    logical ESMF_mene
    type(ESMF_Method), intent(in) :: me1, me2
    ESMF_mene = (me1%method .ne. me2%method)
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! function to compare two ESMF_CompTypes to see if they're the same 

  function ESMF_cteq(ct1, ct2)
    logical ESMF_cteq
    type(ESMF_CompType), intent(in) :: ct1, ct2
    ESMF_cteq = (ct1%ctype .eq. ct2%ctype)    
  end function

  function ESMF_ctne(ct1, ct2)
    logical ESMF_ctne
    type(ESMF_CompType), intent(in) :: ct1, ct2
    ESMF_ctne = (ct1%ctype .ne. ct2%ctype)
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! function to compare two ESMF_GridCompTypes to see if they're the same

  function ESMF_mteq(mt1, mt2)
    logical ESMF_mteq
    type(ESMF_GridCompType), intent(in) :: mt1, mt2
    ESMF_mteq = (mt1%gridcomptype .eq. mt2%gridcomptype)
  end function

  function ESMF_mtne(mt1, mt2)
    logical ESMF_mtne
    type(ESMF_GridCompType), intent(in) :: mt1, mt2
    ESMF_mtne = (mt1%gridcomptype .ne. mt2%gridcomptype)
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
  recursive subroutine ESMF_CompConstruct(compp, ctype, name, gridcomptype, &
    dirPath, configFile, config, grid, clock, petlist, contextflag, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass),    pointer               :: compp
    type(ESMF_CompType),     intent(in)            :: ctype
    character(len=*),        intent(in),  optional :: name
    type(ESMF_GridCompType), intent(in),  optional :: gridcomptype 
    character(len=*),        intent(in),  optional :: dirPath
    character(len=*),        intent(in),  optional :: configFile
    type(ESMF_Config),       intent(in),  optional :: config
    type(ESMF_Grid),         intent(in),  optional :: grid
    type(ESMF_Clock),        intent(in),  optional :: clock
    integer,                 intent(in),  optional :: petlist(:)
    type(ESMF_ContextFlag),  intent(in),  optional :: contextflag
    integer,                 intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Take a new component datatype and fill in the contents.
!
!  The arguments are:
!  \begin{description}
!   \item[compp]
!    Component internal structure to be filled in.
!   \item[ctype]
!    Component type, where type is {\tt ESMF\_GRIDCOMP} or {\tt ESMF\_CPLCOMP}.
!   \item[{[name]}]
!    Component name.
!   \item[{[gridcomptype]}]
!    Component Model Type, where model includes {\tt ESMF\_ATM, ESMF\_LAND,
!    ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER}.  
!   \item[{[dirPath]}]
!    Directory where component-specfic configuration or data files
!    are located.
!   \item[{[configFile]}]
!    File containing configuration information, either absolute filename
!    or relative to {\tt dirPath}.
!   \item[{[config]}]
!    Already created {\tt config} object.
!   \item[{[grid]}]
!    Default {\tt grid} for a Gridded {\tt Component}.
!   \item[{[clock]}]
!    Private {\tt clock} for this {\tt Component}.
!   \item[{[petlist]}]
!    List of {\tt PET}s for this component. The default is to use all PETs.
!   \item[{[contextflag]}]
!    Specify the component's VM context. The default context is
!    {\tt ESMF\_CHILD\_IN\_NEW\_VM}. See section \ref{opt:contextflag} for a
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
    character(len=ESMF_MAXSTR) :: fullpath    ! config file + dirPath
    character(len=ESMF_MAXSTR) :: msgbuf
    type(ESMF_VM):: vm

    ! Assume not implemented until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Set values for the derived type members
    compp%this = ESMF_NULL_POINTER
    compp%base%this = ESMF_NULL_POINTER
    compp%ctype = ctype
    compp%configFile = "uninitialized"
    compp%dirPath = "uninitialized"
    compp%grid%this = ESMF_NULL_POINTER        
    compp%npetlist = 0
    nullify(compp%compw%compp)
    nullify(compp%petlist)
    compp%vm_info = ESMF_NULL_POINTER
    compp%vm_cargo = ESMF_NULL_POINTER
    nullify(compp%is%statep)
    nullify(compp%es%statep)
    compp%vm_released = .FALSE.
    compp%contextflag = ESMF_CHILD_IN_NEW_VM

    ! parent VM
    call ESMF_VMGetCurrent(vm=compp%vm_parent, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
 
    ! for Gridded Components, the model type it represents
    if (present(gridcomptype)) then
      compp%gridcomptype = gridcomptype
    else
      compp%gridcomptype = ESMF_OTHER
    endif

    ! for config files, store a directory path and subsequent opens can
    ! be relative to this or absolute.
    if (present(dirPath)) then
      compp%dirPath = dirPath
    else
      compp%dirPath = "."
    endif

    ! sort out what happens if both a already created config object and
    ! a config filename are given.  the current rules are:  
    if (present(configFile) .and. present(config)) then
      ! a config object gets priority over a name if both are specified.
      call ESMF_LogWrite("Warning: only 1 of Config object or filename should be given.", &
        ESMF_LOG_WARNING)
      call ESMF_LogWrite("Using Config object; ignoring Config filename.", &
        ESMF_LOG_WARNING)
      compp%config = config
    else if (present(configFile)) then
      ! name of a specific config file.  open it and store the config object.
      compp%configFile = configFile
      compp%config = ESMF_ConfigCreate(rc=localrc)
      call ESMF_ConfigLoadFile(compp%config, configFile, rc=localrc)
      if (localrc .ne. ESMF_SUCCESS) then
        ! try again with the dirPath concatinated on front
        fullpath = trim(compp%dirPath) // '/' // trim(configFile)
        call ESMF_ConfigLoadFile(compp%config, fullpath, rc=localrc)
        ! TODO: construct a msg string and then call something here.
        ! if (ESMF_LogMsgFoundError(status, msgstr, rc)) return
        if (localrc .ne. ESMF_SUCCESS) then
          write(msgbuf, *) &
            "ERROR: loading config file, unable to open either", &
            " name = ", trim(configFile), " or name = ", trim(fullpath)
          call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
            msgbuf, &
            ESMF_CONTEXT, rc)
          return
        endif
      endif
    else if (present(config)) then
      ! store already opened config object
      compp%config = config
    else
      ! need a way to set config to 0/invalid
      ! compp%config = ?
    endif

    ! default grid for a Gridded Component
    if (present(grid)) then
      compp%grid = grid
    else
      ! TODO: do we need an "empty grid" object?
    endif

    ! default clock
    if (present(clock)) then
      compp%clock = clock
    else
      !! TODO: Fix this to work
      !!compp%clock = ESMF_NULL_POINTER
    endif

    ! petlist
    if (present(petlist)) then
      compp%npetlist = size(petlist)
      allocate(petlist_loc(compp%npetlist), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "local petlist", &
        ESMF_CONTEXT, rc)) return 
      compp%petlist => petlist_loc
      compp%petlist = petlist     ! copy contents of petlist
    else
      compp%npetlist = 0
      allocate(compp%petlist(0), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "local petlist", &
        ESMF_CONTEXT, rc)) return 
    endif


    ! check for consistency between contextflag and petlist
    call ESMF_VMGet(vm=compp%vm_parent, localPet=mypet, petCount=npets, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    if (present(contextflag)) then
      if (contextflag==ESMF_CHILD_IN_PARENT_VM) then
        if ((compp%npetlist .gt. 0) .and. (compp%npetlist .lt. npets)) then
          ! conflict between contextflag and petlist -> bail out
          deallocate(compp%petlist) ! local garbage collection for bail-on-error
          call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
            "Conflict between contextflag and petlist arguments", &
            ESMF_CONTEXT, rc) 
          return
        endif
      endif
      compp%contextflag = contextflag
    else
      compp%contextflag = ESMF_CHILD_IN_NEW_VM    ! default
    endif
    
    ! check for conflict between petlist and current VM petCount
    if (compp%npetlist .gt. 0) then
      call ESMF_VMGetCurrent(vm, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
      call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
      ! see if pets in the petlist are negative or if they exist
      do i=1, compp%npetlist
        if ((compp%petlist(i) .ge. petCount) .or. (compp%petlist(i) .lt. 0)) then
          deallocate(compp%petlist) ! local garbage collection for bail-on-error
          call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
            "Conflict between petlist and global pet count", &
            ESMF_CONTEXT, rc)
          return
        endif
      enddo
    endif

    ! initialize base class, including component name
    call ESMF_BaseCreate(compp%base, "Component", name, 0, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

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
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
                              
    ! initialize the remaining VM members in compp
    compp%vm_info = ESMF_NULL_POINTER
    compp%vm_cargo = ESMF_NULL_POINTER
    compp%vm_released = .false.
                              
    ! Create an empty subroutine/internal state table.
    call c_ESMC_FTableCreate(compp%this, localrc) 
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! current method/phase
    compp%currentMethod = ESMF_SETNONE
    compp%currentPhase  = 0

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
! !IROUTINE: ESMF_CompDestruct - Internal routine for freeing resources

! !INTERFACE:
  subroutine ESMF_CompDestruct(compp, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Component}.
!
!     The arguments are:
!     \begin{description}
!     \item[compp]
!      Component internal structure to be freed.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    type(ESMF_Status) :: status

    ! Assume not implemented until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    call ESMF_BaseGetStatus(compp%base, status, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (status .eq. ESMF_STATUS_READY) then
    
      if (compp%vm_info /= ESMF_NULL_POINTER) then
        ! shut down this component's VM
        call ESMF_VMShutdown(vm=compp%vm_parent, vmplan=compp%vmplan, &
          vm_info=compp%vm_info, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
      endif

      ! destruct the VMPlan
      call ESMF_VMPlanDestruct(vmplan=compp%vmplan, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

      ! deallocate space held for petlist
      deallocate(compp%petlist, stat=localrc)
      if (ESMF_LogMsgFoundDeallocError(localrc, "local petlist", &
        ESMF_CONTEXT, rc)) return 

      ! call C++ to release function and data pointer tables.
      call c_ESMC_FTableDestroy(compp%this, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

      ! Release attributes on config
      if(compp%configFile .ne. "uninitialized" ) then
        call ESMF_ConfigDestroy(compp%config, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
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
    importState, exportState, clock, phase, blockingflag, userRc, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CompClass),    pointer                 :: compp
    type(ESMF_Method),       intent(in)              :: method
    type(ESMF_State),        intent(inout), optional :: importState
    type(ESMF_State),        intent(inout), optional :: exportState
    type(ESMF_Clock),        intent(in),    optional :: clock
    integer,                 intent(in),    optional :: phase
    type(ESMF_BlockingFlag), intent(in),    optional :: blockingflag
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
!   One of the ESMF Component methods. See section \ref{opt:methods} 
!   for a complete list of valid methods.
! \item[{[importState]}]  
!   Import data for component method.
! \item[{[exportState]}]  
!   Export data for component method.
! \item[{[clock]}]  
!   External clock for passing in time information.
! \item[{[phase]}]
!   If multiple-phase methods, which phase number this is. Default is 1.
! \item[{[blockingflag]}]
!   Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS.
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
    type(ESMF_BlockingFlag) :: blocking     ! local blocking flag
    type(ESMF_VM)           :: vm           ! VM for current context
    integer                 :: phaseArg
        
    ! dummys that will provide initializer values if args are not present
    type(ESMF_State)        :: dummyis, dummyes
    type(ESMF_Clock)        :: dummyclock
    type(ESMF_Status)       :: status

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        
    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, status, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (status .ne. ESMF_STATUS_READY) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rc) 
      return
    endif

    ! set the default mode to ESMF_VASBLOCKING
    if (present(blockingflag)) then
      blocking = blockingflag
    else
      blocking = ESMF_VASBLOCKING
    endif
    
    ! supply default objects if unspecified by the caller
    if (present(importState)) then
      compp%is = importState
    else
      ! use dummy variable
      compp%is = dummyis
    endif

    if (present(exportState)) then
      compp%es = exportState
    else
      ! use dummy variable
      compp%es = dummyes
    endif

    ! and something for clocks?
    if (present(clock)) then
      compp%argclock = clock
    else
      ! use dummy variable
      compp%argclock = dummyclock
    endif

    ! phase
    phaseArg = 1 !default
    if (present(phase)) phaseArg = phase

    ! Wrap comp so it's passed to C++ correctly.
    compp%compw%compp => compp
    ESMF_INIT_SET_CREATED(compp%compw)

    ! Set up the arguments
    if (compp%iAmParticipant) then
      ! only need to call this on PETs that participate
      call c_ESMC_FTableSetStateArgs(compp%this, method, phaseArg, &
        compp%compw, compp%is, compp%es, compp%argclock, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    endif
    
    ! set the current method/phase to keep track inside Component for query
    compp%currentMethod = method
    compp%currentPhase  = phaseArg
          
    ! callback into user code
    call c_ESMC_FTableCallEntryPointVM(compp%vm_parent, compp%vmplan, &
      compp%vm_info, compp%vm_cargo, compp%this, method, phaseArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
      
    ! for threaded VMs (single- or multi-threaded) the child VM will 
    ! now be running concurrently with the parent VM. This is indicated
    ! by the following flag:  
    compp%vm_released = .true.

    ! sync PETs according to blocking mode
    if (blocking == ESMF_VASBLOCKING .or. blocking == ESMF_BLOCKING) then
      ! wait for all child PETs that run in this parent's PET VAS to finish
      call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
        compp%vm_cargo, localUserRc, localrc)
      ! localUserRc - return code of registered user callback method
      ! localrc     - return code of ESMF internal callback stack
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      compp%vm_released = .false.       ! indicate child VM has been caught
      ! for ESMF_BLOCKING _all_ parent PETs will be synced on exit
      if (blocking == ESMF_BLOCKING) then
        ! the current context _is_ the parent context...
        call ESMF_VMGetCurrent(vm=vm, rc=localrc)  ! determine current VM
        if (ESMF_LogMsgFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
        call ESMF_VMBarrier(vm=vm, rc=localrc) ! barrier across parent VM
        if (ESMF_LogMsgFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
      endif
      ! reset current method/phase
      compp%currentMethod = ESMF_SETNONE
      compp%currentPhase  = 0
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
    vm_info, contextflag, gridcomptype, grid, clock, dirPath, configFile, &
    config, ctype, currentMethod, currentPhase, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass),    pointer               :: compp
    character(len=*),        intent(out), optional :: name
    type(ESMF_VM),           intent(out), optional :: vm
    type(ESMF_VM),           intent(out), optional :: vm_parent
    type(ESMF_VMPlan),       intent(out), optional :: vmplan
    type(ESMF_Pointer),      intent(out), optional :: vm_info
    type(ESMF_ContextFlag),  intent(out), optional :: contextflag
    type(ESMF_GridCompType), intent(out), optional :: gridcomptype 
    type(ESMF_Grid),         intent(out), optional :: grid
    type(ESMF_Clock),        intent(out), optional :: clock
    character(len=*),        intent(out), optional :: dirPath
    character(len=*),        intent(out), optional :: configFile
    type(ESMF_Config),       intent(out), optional :: config
    type(ESMF_CompType),     intent(out), optional :: ctype
    type(ESMF_Method),       intent(out), optional :: currentMethod
    integer,                 intent(out), optional :: currentPhase
    integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns information about the component.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Status)       :: status

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        
    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, status, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (status .ne. ESMF_STATUS_READY) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    if (present(name)) then
      call ESMF_GetName(compp%base, name, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    endif

    if (present(ctype)) then
      ctype = compp%ctype
    endif

    if (present(vm)) then
      vm = compp%vm
    endif

    if (present(vm_parent)) then
      vm_parent = compp%vm_parent
    endif

    if (present(vmplan)) then
      vmplan = compp%vmplan
    endif

    if (present(vm_info)) then
      vm_info = compp%vm_info
    endif

    if (present(contextflag)) then
      contextflag = compp%contextflag
    endif

    if (present(gridcomptype)) then
      gridcomptype = compp%gridcomptype
    endif

    if (present(grid)) then
      grid = compp%grid
    endif

    if (present(clock)) then
      clock = compp%clock
    endif

    if (present(dirPath)) then
      dirPath = compp%dirPath
    endif

    if (present(configFile)) then
      configFile = compp%configFile
    endif

    if (present(config)) then
      config = compp%config
    endif

    if (present(currentMethod)) then
      currentMethod = compp%currentMethod
    endif

    if (present(currentPhase)) then
      currentPhase = compp%currentPhase
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
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Status)       :: status

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        
    ! Initialize output in case of error
    ESMF_CompIsPetLocal = .false.

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, status, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (status .ne. ESMF_STATUS_READY) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rc)
      return
    endif
    
    ESMF_CompIsPetLocal = compp%iAmParticipant
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_CompIsPetLocal
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompPrint"
!BOPI
! !IROUTINE:  ESMF_CompPrint -- Print the contents of a Component
!
! !INTERFACE:
  recursive subroutine ESMF_CompPrint(compp, options, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    character(len = *),   intent(in),  optional :: options
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
    type(ESMF_Status)           :: status

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    defaultopts = "brief"

    ! Parse options and decide what to print
    if(present(options)) then
      ! TODO:  decide what to print
    endif

    if (.not.associated(compp)) then
      !nsc  call ESMF_LogWrite("Invalid or uninitialized Component",  &
      !nsc                      ESMF_LOG_INFO)
      write (*,*)  "Invalid or uninitialized Component"
      return
    endif


    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, status, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (status .ne. ESMF_STATUS_READY) then
      !nsc  call ESMF_LogWrite("Invalid or uninitialized Component",  &
      !nsc                      ESMF_LOG_INFO)
      write (*,*)  "Invalid or uninitialized Component"
      return
    endif

    call ESMF_GetName(compp%base, cname, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
       
    write (*,*) " Component name = ", trim(cname)
    
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
  recursive subroutine ESMF_CompSet(compp, name, vm, vm_info, gridcomptype, &
    grid, clock, dirPath, configFile, config, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass),    pointer               :: compp
    character(len=*),        intent(in),  optional :: name
    type(ESMF_VM),           intent(in),  optional :: vm
    type(ESMF_Pointer),      intent(in),  optional :: vm_info
    type(ESMF_GridCompType), intent(in),  optional :: gridcomptype 
    type(ESMF_Grid),         intent(in),  optional :: grid
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
    integer                 :: localrc      ! local return code
    type(ESMF_Status)       :: status

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, status, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (status .ne. ESMF_STATUS_READY) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    if (present(name)) then
      call ESMF_SetName(compp%base, name, "Component", rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(vm)) then
      compp%vm = vm
    endif

    if (present(vm_info)) then
      compp%vm_info = vm_info
    endif

    if (present(gridcomptype)) then
      compp%gridcomptype = gridcomptype
    endif

    if (present(grid)) then
      compp%grid = grid
    endif

    if (present(clock)) then
      compp%clock = clock
    endif

    if (present(dirPath)) then
      compp%dirPath = dirPath
    endif

    if (present(configFile)) then
      compp%configFile = configFile
    endif

    if (present(config)) then
      compp%config = config
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
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    ! ensure that this is not a child_in_parent_vm plan
    if (compp%contextflag == ESMF_CHILD_IN_PARENT_VM) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
        "CompSetVM() calls are incompatible with CHILD_IN_PARENT_VM component", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! call CompClass method
    call ESMF_VMPlanMaxPEs(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

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
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    ! ensure that this is not a child_in_parent_vm plan
    if (compp%contextflag == ESMF_CHILD_IN_PARENT_VM) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
        "CompSetVM() calls are incompatible with CHILD_IN_PARENT_VM component", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! call CompClass method
    call ESMF_VMPlanMaxThreads(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

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
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    ! ensure that this is not a child_in_parent_vm plan
    if (compp%contextflag == ESMF_CHILD_IN_PARENT_VM) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
        "CompSetVM() calls are incompatible with CHILD_IN_PARENT_VM component", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! call CompClass method
    call ESMF_VMPlanMinThreads(compp%vmplan, compp%vm_parent, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      compp%npetlist, compp%petlist, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

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
  recursive subroutine ESMF_CompValidate(compp, options, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer               :: compp
    character(len=*),     intent(in),  optional :: options
    integer,              intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Status)       :: status

    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Test incoming compp object
    if (.not.associated(compp)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Not a valid pointer to ESMF Component object", &
        ESMF_CONTEXT, rc)
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, status, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (status .ne. ESMF_STATUS_READY) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Unini/destroyed comp", &
        ESMF_CONTEXT, rc)
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
  subroutine ESMF_CompWait(compp, blockingflag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass),    pointer               :: compp
    type(ESMF_BlockingFlag), intent(in),  optional :: blockingflag
    integer,                 intent(out), optional :: userRc
    integer,                 intent(out), optional :: rc
!
! !DESCRIPTION:
! Wait for component to return
!
! The arguments are:
! \begin{description}
! \item[compp] 
!   component object
! \item[{[blockingflag]}]
!   The blocking behavior determines exactly what this call waits for. The
!   default is {\tt ESMF\_VASBLOCKING} which blocks PETs across each VAS.
!   See section \ref{opt:blockingflag} for a list of valid blocking options.
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
    type(ESMF_BlockingFlag) :: blocking     ! local blocking flag
    type(ESMF_VM)           :: vm           ! VM for current context
    type(ESMF_Status)       :: status

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check input
    if (.not.associated(compp)) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rc) 
      return
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    call ESMF_BaseGetStatus(compp%base, status, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (status .ne. ESMF_STATUS_READY) then
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "uninitialized or destroyed Component object", &
        ESMF_CONTEXT, rc) 
      return
    endif

    ! check if the child VM, i.e. the VM of this component, is currently marked
    ! as running...
    if (compp%vm_released) then
      ! wait for all child PETs that run in this parent's PET VAS to finish
      call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
        compp%vm_cargo, localUserRc, localrc)
      ! localUserRc - return code of registered user callback method
      ! localrc     - return code of ESMF internal callback stack
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      compp%vm_released = .false.       ! indicate child VM has been caught
      ! set the default mode to ESMF_VASBLOCKING
      if (present(blockingflag)) then
        blocking = blockingflag
      else
        blocking = ESMF_VASBLOCKING
      endif
      ! for ESMF_BLOCKING _all_ parent PETs will be synced on exit
      if (blocking == ESMF_BLOCKING) then
        ! the current context _is_ the parent context...
        call ESMF_VMGetCurrent(vm=vm, rc=localrc)  ! determine current VM
        if (ESMF_LogMsgFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
        call ESMF_VMBarrier(vm=vm, rc=localrc) ! barrier across parent VM
        if (ESMF_LogMsgFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
      endif
    endif

    ! reset current method/phase
    compp%currentMethod = ESMF_SETNONE
    compp%currentPhase  = 0

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
  subroutine ESMF_CWrapSetInitCreated(cw, rc)
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

end module ESMF_CompMod
