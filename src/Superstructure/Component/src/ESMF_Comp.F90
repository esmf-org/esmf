! $Id: ESMF_Comp.F90,v 1.165.2.4 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Comp.F90"
!
!     ESMF Component module
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
!------------------------------------------------------------------------------
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
!     ! ESMF_CompType
!
      type ESMF_CompType
      sequence
      private
        integer :: ctype
      end type

      type(ESMF_CompType), parameter :: ESMF_COMPTYPE_GRID = ESMF_CompType(1), &
                                        ESMF_COMPTYPE_CPL = ESMF_CompType(2)

!------------------------------------------------------------------------------
!     ! ESMF_GridCompType
!
!     ! Model type: Atmosphere, Land, Ocean, SeaIce, River runoff.
!
      type ESMF_GridCompType
      sequence
      private
        integer :: gridcomptype
      end type

      type(ESMF_GridCompType), parameter :: &
                  ESMF_ATM = ESMF_GridCompType(1), &
                  ESMF_LAND = ESMF_GridCompType(2), &
                  ESMF_OCEAN = ESMF_GridCompType(3), &
                  ESMF_SEAICE = ESMF_GridCompType(4), &
                  ESMF_RIVER = ESMF_GridCompType(5), &
                  ESMF_OTHER = ESMF_GridCompType(6)

!------------------------------------------------------------------------------
!     ! ESMF Entry Point Names

      character(len=20), parameter :: ESMF_SETINIT         = "ESMF_Initialize"
      character(len=20), parameter :: ESMF_SETRUN          = "ESMF_Run"
      character(len=20), parameter :: ESMF_SETFINAL        = "ESMF_Finalize"
      character(len=20), parameter :: ESMF_SETWRITERESTART = "ESMF_WriteRestart"
      character(len=20), parameter :: ESMF_SETREADRESTART  = "ESMF_ReadRestart"
 
!------------------------------------------------------------------------------
!     ! ESMF Phase number
      integer, parameter :: ESMF_SINGLEPHASE = 0

!------------------------------------------------------------------------------
!     ! wrapper for Component objects going across F90/C++ boundary
      type ESMF_CWrap
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
#ifndef ESMF_NO_INITIALIZERS
         type(ESMF_CompClass), pointer :: compp => NULL()
#else
         type(ESMF_CompClass), pointer :: compp
#endif
         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_CompClass
!
!     ! Component internal class data.

      type ESMF_CompClass
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      private
         type(ESMF_Pointer) :: this       ! C++ ftable pointer - MUST BE FIRST
         type(ESMF_Base) :: base                  ! base class
#ifndef ESMF_NO_INITIALIZERS
         type(ESMF_Status) :: compstatus = ESMF_STATUS_INVALID  ! object ok?
#else
         type(ESMF_Status) :: compstatus          ! valid object or not?
#endif
         type(ESMF_CompType) :: ctype             ! component type
         type(ESMF_Config) :: config              ! configuration object
         type(ESMF_Clock) :: clock                ! private component clock


         character(len=ESMF_MAXSTR) :: configFile ! resource filename
         character(len=ESMF_MAXSTR) :: dirPath    ! relative dirname, app only
         type(ESMF_Grid) :: grid                  ! default grid, gcomp only
#ifdef ESMF_IS_64BIT_MACHINE

         integer            :: npetlist           ! number of PETs in petlist
#endif

         type(ESMF_GridCompType) :: gridcomptype  ! model type, gcomp only

         type(ESMF_CompClass), pointer :: parent  ! pointer to parent comp
         type(ESMF_CWrap)   :: compw              ! to satisfy the C interface
         type(ESMF_VM)      :: vm                 ! component VM
         type(ESMF_VM)      :: vm_parent          ! reference to the parent VM

         integer, pointer   :: petlist(:)         ! list of usable parent PETs 
         
         type(ESMF_VMPlan)  :: vmplan             ! reference to VMPlan
         type(ESMF_Pointer) :: vm_info            ! holding pointer to info
         type(ESMF_Pointer) :: vm_cargo           ! holding pointer to cargo

         type(ESMF_State)   :: is, es   ! hold state args refs for thread-safety
         type(ESMF_Clock)   :: argclock ! hold clock arg ref for thread-safety
#ifdef ESMF_IS_32BIT_MACHINE

         integer            :: npetlist           ! number of PETs in petlist
#endif
         logical  :: iAmParticipant     ! .false. : PET does not participate
                                        ! .true.  : PET participates in comp

         logical :: multiphaseinit                ! multiple init, run, final
         integer :: initphasecount                ! max inits, for error check
         logical :: multiphaserun                 ! multiple init, run, final
         integer :: runphasecount                 ! max runs, for error check
         logical :: multiphasefinal               ! multiple init, run, final
         integer :: finalphasecount               ! max finals, for error check
         logical            :: vm_released        ! flag whether vm is running

         integer            :: status

! gjt - I added this new member at the end as to not disturb the above order
         type(ESMF_ContextFlag) :: contextflag    ! contextflag

         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_CplComp
!
!     ! Cplcomp wrapper

      type ESMF_CplComp
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
#ifndef ESMF_NO_INITIALIZERS
         type(ESMF_CompClass), pointer :: compp => NULL()
#else
         type(ESMF_CompClass), pointer :: compp
#endif
         ESMF_INIT_DECLARE
      end type


!------------------------------------------------------------------------------
!     ! ESMF_GridComp
!
!     ! GridComp wrapper

      type ESMF_GridComp
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
#ifndef ESMF_NO_INITIALIZERS
         type(ESMF_CompClass), pointer :: compp => NULL()
#else
         type(ESMF_CompClass), pointer :: compp
#endif
         ESMF_INIT_DECLARE
      end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Config   ! TODO: move to its own file 
      public ESMF_GridCompType, ESMF_ATM, ESMF_LAND, ESMF_OCEAN, &
                             ESMF_SEAICE, ESMF_RIVER, ESMF_OTHER
      public ESMF_SETINIT, ESMF_SETRUN, ESMF_SETFINAL, ESMF_SINGLEPHASE
      
      ! These have to be public so other component types can use them, but 
      !  are not intended to be used outside the Framework code.

      public ESMF_CompClass, ESMF_CWrap
      public ESMF_CompType
      public ESMF_COMPTYPE_GRID, ESMF_COMPTYPE_CPL 
      public ESMF_CplComp, ESMF_GridComp

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:


      ! These have to be public so other component types can call them,
      !  but they are not intended to be used outside the Framework code.

      public ESMF_CompClassGetInit 
      public ESMF_CompClassValidate
      public ESMF_CompClassSetInitCreated

      public ESMF_CompConstruct, ESMF_CompDestruct
      public ESMF_CompExecute
      public ESMF_CompWriteRestart, ESMF_CompReadRestart
      public ESMF_CompGet, ESMF_CompSet
      public ESMF_CompIsPetLocal

      public ESMF_CompValidate, ESMF_CompPrint

      public ESMF_CompSetVMMaxThreads
      public ESMF_CompSetVMMinThreads
      public ESMF_CompSetVMMaxPEs
      public ESMF_CompWait

      public ESMF_CWrapSetInitCreated
!EOPI

      public operator(.eq.), operator(.ne.)

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Comp.F90,v 1.165.2.4 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare     
!  them as if they were simple integers.

interface operator (.eq.)
 module procedure ESMF_cteq
 module procedure ESMF_mteq
end interface

interface operator (.ne.)
 module procedure ESMF_ctne
 module procedure ESMF_mtne
end interface


!==============================================================================

      contains

!==============================================================================


! -------------------------- ESMF-public method -------------------------------
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
!      Validates that the {\tt CompClass} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[cc] 
!          Specified {\tt ESMF\_CompClass} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
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



! -------------------------- ESMF-public method -------------------------------
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
!      Set init code in CompClass object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[cc] 
!          Specified {\tt ESMF\_CompClass} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [cc]
!           CompClass object.
!     \end{description}
!
!EOPI

    if (present(cc)) then
      ESMF_CompClassGetInit = ESMF_INIT_GET(cc)
    else
      ESMF_CompClassGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_CompClassGetInit
!------------------------------------------------------------------------------


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
!
! This section includes Component Create/Destroy, Construct/Destruct methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompConstruct"
!BOPI
! !IROUTINE: ESMF_CompConstruct - Internal routine to fill in a comp struct

! !INTERFACE:
      recursive subroutine ESMF_CompConstruct(compp, ctype, name, gridcomptype, &
                          dirPath, configFile, config, grid, clock, parent, &
                          vm, petlist, contextflag, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type (ESMF_CompType), intent(in) :: ctype
      character(len=*), intent(in), optional :: name
      type(ESMF_GridCompType), intent(in), optional :: gridcomptype 
      character(len=*), intent(in), optional :: dirPath
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Config), intent(in), optional :: config
      type(ESMF_Grid), intent(in), optional :: grid
      type(ESMF_Clock), intent(in), optional :: clock
      type(ESMF_CompClass), pointer, optional :: parent
      type(ESMF_VM), intent(in), optional :: vm
      integer,       intent(in), optional :: petlist(:)
      type(ESMF_ContextFlag), intent(in), optional :: contextflag
      integer, intent(out), optional :: rc 
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
!   \item[{[parent]}]
!    Parent component - if specified, inherit from here.
!   \item[{[vm]}]
!    Parent virtual machine.
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
        ! Local vars
        integer :: status                            ! local error status
        integer :: allocstatus                       ! alloc/dealloc status
        logical :: rcpresent                         ! did user specify rc?
        character(len=ESMF_MAXSTR) :: fullpath       ! config file + dirPath
        character(len=ESMF_MAXSTR) :: msgbuf
        integer, pointer :: petlist_loc(:)
        integer :: npets, mypet, i

        ! Initialize return code; assume failure until success is certain
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif


        ! Fill in values
        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                    "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif


        ! Check init status of arguments
        ! (a non-present pointer argument seems to cause an error on some systems)
!!        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, parent, rc)


        ! component type
        compp%ctype = ctype

        ! set initial values for the derived type members, so if they
        ! do not get set later they have known initial values.
        compp%this = ESMF_NULL_POINTER
        compp%base%this = ESMF_NULL_POINTER
        compp%compstatus = ESMF_STATUS_INVALID 
        compp%ctype = ESMF_COMPTYPE_GRID
        ! type(ESMF_Config) compp%config              ! configuration object
        ! type(ESMF_Clock) compp%clock                ! private component clock
        compp%configFile = "uninitialized"
        compp%dirPath = "uninitialized"
	compp%grid%this = ESMF_NULL_POINTER        
	compp%npetlist = 0
        ! type(ESMF_GridCompType) compp%gridcomptype
        nullify(compp%parent)
        ! type(ESMF_CWrap)   compp%compw
        ! type(ESMF_VM)      compp%vm
        ! type(ESMF_VM)      compp%vm_parent
        nullify(compp%petlist)
        ! type(ESMF_VMPlan)  compp%vmplan
        compp%vm_info = ESMF_NULL_POINTER
        compp%vm_cargo = ESMF_NULL_POINTER
        nullify(compp%is%statep)
        nullify(compp%es%statep)
        compp%multiphaseinit = .FALSE.
        compp%initphasecount = 0
        compp%multiphaserun = .FALSE.
        compp%runphasecount = 0 
        compp%multiphasefinal = .FALSE.
        compp%finalphasecount = 0 
        compp%vm_released = .FALSE.

        compp%status = 0

        compp%contextflag = ESMF_CHILD_IN_NEW_VM

        ! now continue with rest of normal initialization.

        ! initialize base class, including component name
        call ESMF_BaseCreate(compp%base, "Component", name, 0, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! parent VM
        if (present(vm)) then
          compp%vm_parent = vm
        else
          ! if parent comp was specified then use its VM, else use current VM.
          if (present(parent)) then
            compp%vm_parent = parent%vm
          else
            call ESMF_VMGetCurrent(vm=compp%vm_parent, rc=status)
            if (ESMF_LogMsgFoundError(status, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rc)) return
          endif
        endif

        ! for Gridded Components, the model type it represents
        if (present(gridcomptype)) then
          compp%gridcomptype = gridcomptype
        else
          compp%gridcomptype = ESMF_OTHER
        endif

        ! for config files, store a directory path and subsequent opens can
        !  be relative to this or absolute.
        if (present(dirPath)) then
          compp%dirPath = dirPath
        else
          compp%dirPath = "."
        endif

        ! sort out what happens if both a already created config object and
        ! a config filename are given.  the current rules are:  a config object
        ! gets priority over a name if both are specified.
        if (present(configFile) .and. present(config)) then
            call ESMF_LogWrite("Warning: only 1 of Config object or filename should be given.", &
                 ESMF_LOG_WARNING)

            call ESMF_LogWrite("Using Config object; ignoring Config filename.", &
                 ESMF_LOG_WARNING)
            compp%config = config

        ! name of a specific config file.  open it and store the config object.
        else if (present(configFile)) then
          compp%configFile = configFile
          compp%config = ESMF_ConfigCreate(status)
          call ESMF_ConfigLoadFile(compp%config, configFile, rc=status)
          if (status .ne. ESMF_SUCCESS) then
              ! try again with the dirPath concatinated on front
              fullpath = trim(dirPath) // '/' // trim(configFile)
              call ESMF_ConfigLoadFile(compp%config, fullpath, rc=status)
              ! TODO: construct a msg string and then call something here.
              ! if (ESMF_LogMsgFoundError(status, msgstr, rc)) return
              if (status .ne. ESMF_SUCCESS) then
                call ESMF_BaseDestroy(compp%base)
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
          allocate(petlist_loc(compp%npetlist), stat=allocstatus)
          if (ESMF_LogMsgFoundAllocError(allocstatus, "local petlist", &
            ESMF_CONTEXT, rc)) return 
          compp%petlist => petlist_loc
          compp%petlist = petlist     ! copy contents of petlist
        else
          compp%npetlist = 0
          allocate(compp%petlist(0), stat=allocstatus)
          if (ESMF_LogMsgFoundAllocError(allocstatus, "local petlist", &
            ESMF_CONTEXT, rc)) return 
        endif

        ! check for consistency between contextflag and petlist
        call ESMF_VMGet(vm=compp%vm_parent, localPet=mypet, petCount=npets, &
          rc=status)
        if (ESMF_LogMsgFoundError(status, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
        if (present(contextflag)) then
          if (contextflag==ESMF_CHILD_IN_PARENT_VM) then
            if ((compp%npetlist .gt. 0) .and. (compp%npetlist .lt. npets)) then
              ! conflict between contextflag and petlist -> bail out
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
          contextflag=compp%contextflag, rc=status)
        if (ESMF_LogMsgFoundError(status, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
                                  
        ! initialize the remaining VM members in compp
        compp%vm_info = ESMF_NULL_POINTER
        compp%vm_cargo = ESMF_NULL_POINTER
        compp%vm_released = .false.
                                  
        ! Create an empty subroutine/internal state table.
        call c_ESMC_FTableCreate(compp%this, status) 
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
   
        compp%compstatus = ESMF_STATUS_READY

        ! Set init code
        ESMF_INIT_SET_CREATED(compp)

        ! Set return values
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompConstruct


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompDestruct"
!BOPI
! !IROUTINE: ESMF_CompDestruct - Internal routine for freeing resources

! !INTERFACE:
      subroutine ESMF_CompDestruct(compp, rc)
!
! !ARGUMENTS:
      type(ESMF_CompClass), pointer :: compp
      integer, intent(out), optional :: rc
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

        ! local vars
        integer :: status                       ! local error status
        integer :: allocstatus                  ! alloc/dealloc status
        logical :: rcpresent                    ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif


        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                    "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif


        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)


        if (compp%vm_info /= ESMF_NULL_POINTER) then
          ! shut down this component's VM
          call ESMF_VMShutdown(vm=compp%vm_parent, vmplan=compp%vmplan, &
            vm_info=compp%vm_info, rc=status)
          if (ESMF_LogMsgFoundError(status, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        endif

        ! destruct the VMPlan
        call ESMF_VMPlanDestruct(vmplan=compp%vmplan, rc=status)
        if (ESMF_LogMsgFoundError(status, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return

        ! deallocate space held for petlist
        deallocate(compp%petlist, stat=allocstatus)
        if (ESMF_LogMsgFoundAllocError(allocstatus, "local petlist", &
          ESMF_CONTEXT, rc)) return 

        ! mark obj invalid
        compp%compstatus = ESMF_STATUS_INVALID

        ! call C++ to release function and data pointer tables.
        call c_ESMC_FTableDestroy(compp%this, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Release attributes and other things on base class
        call ESMF_BaseDestroy(compp%base, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        
        ! Release attributes on config
        if(compp%configFile .ne. "uninitialized" ) then
        call ESMF_ConfigDestroy(compp%config, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        ! Set init code
        ESMF_INIT_SET_DELETED(compp)

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompDestruct



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Component Execute method used by GridComp and 
! CplComp for:
!   * Initialize,
!   * Run,
!   * Finalize.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompExecute"
!BOPI
! !IROUTINE: ESMF_CompExecute -- Call into registered component method

! !INTERFACE:
      recursive subroutine ESMF_CompExecute(compp, importState, exportState, &
        clock, methodtype, phase, blockingFlag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer                           :: compp
      type (ESMF_State),              intent(inout),  optional :: importState
      type (ESMF_State),              intent(inout),  optional :: exportState
      type (ESMF_Clock),              intent(in),     optional :: clock
      character(len=*),               intent(in),     optional :: methodtype
      integer,                        intent(in),     optional :: phase
      type (ESMF_BlockingFlag),       intent(in),     optional :: blockingFlag
      integer,                        intent(out),    optional :: rc 
!
! !DESCRIPTION:
!  Call into the associated user code for a component's method.
!
!  The arguments are:
!  \begin{description}
!
!   \item[compp]
!    Component to call Initialization routine for.
!   \item[{[importState]}]  
!    Import data for component method.
!   \item[{[exportState]}]  
!    Export data for component method.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[methodtype]}]
!    One of the method types: ESMF\_SETINIT, ESMF\_SETRUN, ESMF\_SETFINAL
!   \item[{[phase]}]  
!    If multiple-phase methods, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[blockingFlag]}]
!    Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!    for a list of valid blocking options. Default option is
!    {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!    across each VAS.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


        ! local vars
        integer                 :: status       ! local error status
        logical                 :: rcpresent    ! did user specify rc?
        integer                 :: callrc       ! return code from user code
        type(ESMF_BlockingFlag) :: blocking     ! local blocking flag
        type(ESMF_VM)           :: vm           ! VM for current context
        
        ! dummys that will provide initializer values if args are not present
        type(ESMF_State)        :: dummyis, dummyes
        type(ESMF_Clock)        :: dummyclock

        ! Initialize return code; assume failure until success is certain
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif
        

        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                    "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc) 
            return
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)



        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                    "uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc) 
            return
        endif

        ! set the default mode to ESMF_VASBLOCKING
        if (present(blockingFlag)) then
          blocking = blockingFlag
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

        ! Wrap comp so it's passed to C++ correctly.
        compp%compw%compp => compp
        ESMF_INIT_SET_CREATED(compp%compw)

        ! Set up the arguments
        call c_ESMC_FTableSetStateArgs(compp%this, methodtype, phase, &
          compp%compw, compp%is, compp%es, compp%argclock, status)
        if (ESMF_LogMsgFoundError(status, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
          
        ! callback into user code
        call c_ESMC_FTableCallEntryPointVM(compp%vm_parent, compp%vmplan, &
          compp%vm_info, compp%vm_cargo, compp%this, methodtype, phase, &
          status)
        if (ESMF_LogMsgFoundError(status, &
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
            compp%vm_cargo, callrc, status)
          ! callrc - return code of registered user callback method
          ! status - return code of ESMF internal callback stack
          if (ESMF_LogMsgFoundError(status, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
          compp%vm_released = .false.       ! indicate child VM has been caught
          ! for ESMF_BLOCKING _all_ parent PETs will be synced on exit
          if (blocking == ESMF_BLOCKING) then
            ! the current context _is_ the parent context...
            call ESMF_VMGetCurrent(vm=vm, rc=status)  ! determine current VM
            if (ESMF_LogMsgFoundError(status, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rc)) return
            call ESMF_VMBarrier(vm=vm, rc=status) ! barrier across parent VM
            if (ESMF_LogMsgFoundError(status, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rc)) return
          endif
          ! TODO: we need to be able to return two return codes here,
          ! "callrc" for the registered user callback method and "status"
          ! for to indicate ESMF internal issues. For now, if we haven't
          ! bailed out down to this point because of ESMF internal error
          ! codes in status the status variable will be set to the "callrc"
          ! code that the user method returned.
          status = callrc
        endif

        ! TODO: Since the current interface does not support returning two
        ! separate return codes things are inconsistent here. In the 
        ! blocking cases status holds the return code of the registered
        ! user method, while in the non-blocking case status holds the
        ! ESMF internal return code of calling into 
        ! c_ESMC_FTableCallEntryPointVM() [which has been error checked above
        ! already!].
        
        ! TODO: not sure we want to log an error for user return codes. Are
        ! users required to abide to the ESMF error code convention? The least
        ! restrictive thing to do is to just pass the user return code through
        ! to the parent component and have the user code interpret what it 
        ! means.
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! Return success
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_CompExecute


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Component WriteRestart and ReadRestart methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompWriteRestart"
!BOPI
! !IROUTINE: ESMF_CompWriteRestart -- Call the Component's internal save routine

! !INTERFACE:
      recursive subroutine ESMF_CompWriteRestart(compp, iospec, clock, &
                                                 phase, blockingFlag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type(ESMF_IOSpec), intent(in), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingFlag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user checkpoint code for a component.
!    
!  The arguments are:
!  \begin{description}
!   \item[compp]
!    Component to call WriteRestart routine for.
!   \item[{[iospec]}]  
!     Controls for how the component's data will be written.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!     If multiple-phase checkpoint, which phase number this is.
!     Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[blockingFlag]}]  
!    Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!    for a list of valid blocking options. Default option is
!    {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!    across each VAS.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        character(ESMF_MAXSTR) :: cname
        type (ESMF_BlockingFlag) :: blocking

        ! WriteRestart return code; assume failure until success is certain
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                    "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc) 
            return
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)


        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc) 
            return
        endif

        call ESMF_GetName(compp%base, cname, status)

        ! set the default mode to ESMF_VASBLOCKING
        if (present(blockingFlag)) then
          blocking = blockingFlag
        else
          blocking = ESMF_VASBLOCKING
        endif

        ! TODO: add rest of default handling here.

        ! Wrap comp so it's passed to C++ correctly.
        compp%compw%compp => compp

        ! Set up the arguments before the call     
        call c_ESMC_FTableSetIOArgs(compp%this, ESMF_SETWRITERESTART, phase, &
                                    compp%compw, iospec, clock, status)

        ! Call user-defined run routine
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETWRITERESTART, &
                                                            phase, status)

        ! set error code but fall thru and clean up states
        if (ESMF_LogMsgFoundError(status, &
                                  "Component writerestart error", &
                                  ESMF_CONTEXT, rc)) continue
        ! fall thru intentionally

        ! Set return values
        if (rcpresent) rc = status

        end subroutine ESMF_CompWriteRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompReadRestart"
!BOPI
! !IROUTINE: ESMF_CompReadRestart -- Call the Component's internal restart routine

! !INTERFACE:
      recursive subroutine ESMF_CompReadRestart(compp, iospec, clock, phase, &
                                                blockingFlag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type(ESMF_IOSpec), intent(in), optional :: iospec
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingFlag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user internal restart code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[compp]
!    Component to call ReadRestart routine for.
!   \item[{[iospec]}]  
!    Controls for how the component's data will be read back.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[phase]}]  
!     If multiple-phase restore, which phase number this is.
!     Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[blockingFlag]}]  
!    Use {\tt ESMF\_BLOCKING} (default) or {\tt ESMF\_NONBLOCKING}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        character(ESMF_MAXSTR) :: cname
        type (ESMF_BlockingFlag) :: blocking

        ! ReadRestart return code; assume failure until success is certain
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif


        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc) 
            return
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)



        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)  
            return
        endif

        call ESMF_GetName(compp%base, cname, status)

        ! set the default mode to ESMF_VASBLOCKING
        if (present(blockingFlag)) then
          blocking = blockingFlag
        else
          blocking = ESMF_VASBLOCKING
        endif

        ! TODO: put in rest of default argument handling here

        ! Wrap comp so it's passed to C++ correctly.
        compp%compw%compp => compp

        ! Set up the arguments before the call     
        call c_ESMC_FTableSetIOArgs(compp%this, ESMF_SETREADRESTART, phase, &
                                    compp%compw, iospec, clock, status)

        ! Call user-defined run routine
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETREADRESTART, &
                                                           phase, status)

        ! set error code but fall thru and clean up states
        if (ESMF_LogMsgFoundError(status, &
                                  "Component readrestart error", &
                                  ESMF_CONTEXT, rc)) continue
        ! fall thru intentionally

        ! Set return values
        if (rcpresent) rc = status


        end subroutine ESMF_CompReadRestart



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query/Set information from/in the component.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompGet"
!BOPI
! !IROUTINE: ESMF_CompGet -- Query a component for various information
!
! !INTERFACE:
      recursive subroutine ESMF_CompGet(compp, name, vm, vm_parent, vmplan, &
        vm_info, contextflag, gridcomptype, grid, clock, dirPath, configFile, &
        config, ctype, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character(len=*), intent(out), optional :: name
      type(ESMF_VM), intent(out), optional :: vm
      type(ESMF_VM), intent(out), optional :: vm_parent
      type(ESMF_VMPlan), intent(out), optional :: vmplan
      type(ESMF_Pointer), intent(out), optional :: vm_info
      type(ESMF_ContextFlag), intent(out), optional :: contextflag
      type(ESMF_GridCompType), intent(out), optional :: gridcomptype 
      type(ESMF_Grid), intent(out), optional :: grid
      type(ESMF_Clock), intent(out), optional :: clock
      character(len=*), intent(out), optional :: dirPath
      character(len=*), intent(out), optional :: configFile
      type(ESMF_Config), intent(out), optional :: config
      type(ESMF_CompType), intent(out), optional :: ctype
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the component.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOPI
        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif


        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)


        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif

        if (present(name)) then
          call ESMF_GetName(compp%base, name, status)
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


        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompSet"
!BOPI
! !IROUTINE: ESMF_CompSet -- Query a component for various information
!
! !INTERFACE:
      recursive subroutine ESMF_CompSet(compp, name, vm, vm_info, gridcomptype,&
        grid, clock, dirPath, configFile, config, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character(len=*), intent(in), optional :: name
      type(ESMF_VM), intent(in), optional :: vm
      type(ESMF_Pointer), intent(in), optional :: vm_info
      type(ESMF_GridCompType), intent(in), optional :: gridcomptype 
      type(ESMF_Grid), intent(in), optional :: grid
      type(ESMF_Clock), intent(in), optional :: clock
      character(len=*), intent(in), optional :: dirPath
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Config), intent(in), optional :: config
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Sets or resets information about the component.  When the caller
!      only wants to set a single value specify the argument by name.
!      All the arguments after the component input are optional 
!      to facilitate this.
!
!EOPI
        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif


        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)


        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif

        if (present(name)) then
          call ESMF_SetName(compp%base, name, "Component", status)
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


        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompSet

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
      type (ESMF_CompClass), pointer :: compp
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Inquire if this component is to execute on the calling PET.
!
!  The return value is {\tt .true.} if the component is to execute on the 
!  calling PET, {\tt .false.} otherwise.
!    
!
!EOPI
        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif

        ! Initialize output in case of error
        ESMF_CompIsPetLocal = .false.

        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)


        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Unini/destroyed comp", &
                                     ESMF_CONTEXT, rc)
            return
        endif
        
        ESMF_CompIsPetLocal = compp%iAmParticipant

 
        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_CompIsPetLocal

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Components
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompWrite"
!BOPI
! !IROUTINE: ESMF_CompWrite - Write a Component to disk
!
! !INTERFACE:
      recursive subroutine ESMF_CompWrite(compp, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_CompClass) :: compp
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOPI

!
! TODO: code goes here
!
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

        end subroutine ESMF_CompWrite


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompRead"
!BOPI
! !IROUTINE: ESMF_CompRead - Read a Component from disk
!
! !INTERFACE:
      function ESMF_CompRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_CompClass) :: ESMF_CompRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!
!EOPI

!
! TODO: code goes here
!
        type (ESMF_CompClass) :: a


!       this is just to stop compiler warnings
        a%gridcomptype = ESMF_OTHER

        ESMF_CompRead = a 

        ESMF_INIT_SET_CREATED(ESMF_CompRead)
 
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
        end function ESMF_CompRead


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
      type (ESMF_CompClass), pointer :: compp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to ensure a Component is valid.
!
!EOPI

!
! TODO: code goes here
!
       character (len=6) :: defaultopts
       integer :: status                       ! local error status
       logical :: rcpresent                    ! did user specify rc?

       ! Initialize return code; assume failure until success is certain
       status = ESMF_RC_NOT_IMPL
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_RC_NOT_IMPL
       endif

        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif


        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

       if (compp%compstatus .ne. ESMF_STATUS_READY) then
           call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                    "Unini/destroyed comp", &
                                    ESMF_CONTEXT, rc)
            return
       endif

       defaultopts = "brief"

       ! Make sure object is internally consistent
       if(present(options)) then
           ! validate object at the requested level
       else
           ! do default validation
       endif

       ! TODO: add code here

       ! Use LogErr to handle return code (ESMF_SUCCESS for *Validate)
       ! if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
       !    ESMF_CONTEXT, rcToReturn=rc)) return

       ! set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CompValidate

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
      type (ESMF_CompClass), pointer :: compp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Routine to print information about a component.
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!EOPI

       integer :: status                       ! local error status
       logical :: rcpresent                    ! did user specify rc?
       character (len=6) :: defaultopts
       character (len=ESMF_MAXSTR) :: cname
       !character (len=ESMF_MAXSTR) :: msgbuf

       ! Initialize return code; assume failure until success is certain
       status = ESMF_RC_NOT_IMPL
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_RC_NOT_IMPL
       endif

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

       if (compp%compstatus .ne. ESMF_STATUS_READY) then
          !nsc  call ESMF_LogWrite("Invalid or uninitialized Component",  &
          !nsc                      ESMF_LOG_INFO)
          write (*,*)  "Invalid or uninitialized Component"
          return
       endif

       call ESMF_GetName(compp%base, cname, status)
       if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
     !jw  write (msgbuf,*) " Component name = ", trim(cname)
     !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
       write (*,*) " Component name = ", trim(cname)
       
       ! TODO: add more info here

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_CompPrint


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompSetVMMaxThreads"
!BOPI
! !IROUTINE: ESMF_CompSetVMMaxThreads - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompSetVMMaxThreads(compp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer                        :: compp
    integer,                       intent(in),  optional :: max
    integer,                       intent(in),  optional :: pref_intra_process
    integer,                       intent(in),  optional :: pref_intra_ssi
    integer,                       intent(in),  optional :: pref_inter_ssi
    integer,                       intent(out), optional :: rc           
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

    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_RC_NOT_IMPL
    endif

        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
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
      compp%npetlist, compp%petlist, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompSetVMMaxThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompSetVMMinThreads"
!BOPI
! !IROUTINE: ESMF_CompSetVMMinThreads - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompSetVMMinThreads(compp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer                        :: compp
    integer,                       intent(in),  optional :: max
    integer,                       intent(in),  optional :: pref_intra_process
    integer,                       intent(in),  optional :: pref_intra_ssi
    integer,                       intent(in),  optional :: pref_inter_ssi
    integer,                       intent(out), optional :: rc
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

    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_RC_NOT_IMPL
    endif


        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
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
      compp%npetlist, compp%petlist, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompSetVMMinThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompSetVMMaxPEs"
!BOPI
! !IROUTINE: ESMF_CompSetVMMaxPEs - Define a VM for this Component

! !INTERFACE:
  subroutine ESMF_CompSetVMMaxPEs(compp, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CompClass), pointer                        :: compp
    integer,                       intent(in),  optional :: max
    integer,                       intent(in),  optional :: pref_intra_process
    integer,                       intent(in),  optional :: pref_intra_ssi
    integer,                       intent(in),  optional :: pref_inter_ssi
    integer,                       intent(out), optional :: rc
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

    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_RC_NOT_IMPL
    endif


        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
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
      compp%npetlist, compp%petlist, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompSetVMMaxPEs
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompWait"
!BOPI
! !IROUTINE: ESMF_CompWait - Wait for component to return

! !INTERFACE:
  subroutine ESMF_CompWait(compp, blockingFlag, rc)
!
! !ARGUMENTS:
    type (ESMF_CompClass), pointer                         :: compp
    type (ESMF_BlockingFlag),       intent(in),   optional :: blockingFlag
    integer,                        intent(out),  optional :: rc
!
! !DESCRIPTION:
!     Wait for component to return
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[blockingFlag]}]
!       The blocking behavior determines exactly what this call waits for. The
!       default is {\tt ESMF\_VASBLOCKING} which blocks PETs across each VAS.
!       See section \ref{opt:blockingflag} for a list of valid blocking options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer                 :: status       ! local error status
    logical                 :: rcpresent    ! did user specify rc?
    integer                 :: callrc       ! return code from user code
    type(ESMF_BlockingFlag) :: blocking     ! local blocking flag
    type(ESMF_VM)           :: vm           ! VM for current context

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_RC_NOT_IMPL
    endif


        if (.not.associated(compp)) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                     "Uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)
            return
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_CompClassGetInit, compp, rc)

    if (compp%compstatus .ne. ESMF_STATUS_READY) then
        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                 "uninitialized or destroyed component", &
                                 ESMF_CONTEXT, rc)
            return
    endif

    ! check if the child VM, i.e. the VM of this component, is currently marked
    ! as running...
    if (compp%vm_released) then
      ! wait for all child PETs that run in this parent's PET VAS to finish
      call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
                           compp%vm_cargo, callrc, status)
      ! callrc - return code of registered user callback method
      ! status - return code of ESMF internal callback stack
      if (ESMF_LogMsgFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      compp%vm_released = .false.       ! indicate child VM has been caught
      ! set the default mode to ESMF_VASBLOCKING
      if (present(blockingFlag)) then
        blocking = blockingFlag
      else
        blocking = ESMF_VASBLOCKING
      endif
      ! for ESMF_BLOCKING _all_ parent PETs will be synced on exit
      if (blocking == ESMF_BLOCKING) then
        ! the current context _is_ the parent context...
        call ESMF_VMGetCurrent(vm=vm, rc=status)  ! determine current VM
        if (ESMF_LogMsgFoundError(status, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
        call ESMF_VMBarrier(vm=vm, rc=status) ! barrier across parent VM
        if (ESMF_LogMsgFoundError(status, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return
      endif
      ! TODO: we need to be able to return two return codes here,
      ! "callrc" for the registered user callback method and "status"
      ! for to indicate ESMF internal issues. For now, if we haven't
      ! bailed out down to this point because of ESMF internal error
      ! codes in status the status variable will be set to the "callrc"
      ! code that the user method returned.
      status = callrc
    else
      ! if the component's VM was not marked as running there is no sense in
      ! waiting for it to finish. Still it is o.k. to issue a wait in this case.
      status = ESMF_SUCCESS
    endif

    ! TODO: Since the current interface does not support returning two
    ! separate return codes things are inconsistent here. In the 
    ! case where the component's VM was marked released on entering CompWait
    ! the status will hold the return code of the registered
    ! user method, while in the opposite case status holds the
    ! ESMF internal return code.

    ! TODO: not sure we want to log an error for user return codes. Are
    ! users required to abide by the ESMF error code convention? The least
    ! restrictive thing to do is to just pass the user return code through
    ! to the parent component and have the user code interpret what it 
    ! means.
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CompWait
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
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
