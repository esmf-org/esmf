! $Id: ESMF_Comp.F90,v 1.112 2004/11/01 23:39:38 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
      use ESMF_BaseTypesMod
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_VMMod
      use ESMF_ConfigMod
      use ESMF_CalendarMod
      use ESMF_ClockMod
      use ESMF_GridTypesMod
      use ESMF_StateTypesMod
      use ESMF_StateMod
      
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
         logical :: multiphaseinit                ! multiple init, run, final
         integer :: initphasecount                ! max inits, for error check
         logical :: multiphaserun                 ! multiple init, run, final
         integer :: runphasecount                 ! max runs, for error check
         logical :: multiphasefinal               ! multiple init, run, final
         integer :: finalphasecount               ! max finals, for error check
         character(len=ESMF_MAXSTR) :: configFile ! resource filename
         character(len=ESMF_MAXSTR) :: dirPath    ! relative dirname, app only
         type(ESMF_Grid) :: grid                  ! default grid, gcomp only
         type(ESMF_GridCompType) :: gridcomptype  ! model type, gcomp only
         type(ESMF_CompClass), pointer :: parent  ! pointer to parent comp
         type(ESMF_CWrap)   :: compw              ! to satisfy the C interface
         type(ESMF_VM)      :: vm                 ! component VM
         type(ESMF_VM)      :: vm_parent          ! reference to the parent VM
         integer            :: npetlist           ! number of PETs in petlist
         integer, pointer   :: petlist(:)         ! list of usble parent PETs 
         type(ESMF_VMPlan)  :: vmplan             ! reference to VMPlan
         type(ESMF_Pointer) :: vm_info            ! holding pointer to info
         type(ESMF_Pointer) :: vm_cargo           ! holding pointer to cargo
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
      end type


!------------------------------------------------------------------------------
!     ! ESMF_GridComp
!
!     ! Gridcomp wrapper

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

      public ESMF_CompConstruct, ESMF_CompDestruct
      public ESMF_CompInitialize, ESMF_CompRun, ESMF_CompFinalize
      public ESMF_CompWriteRestart, ESMF_CompReadRestart
      public ESMF_CompGet, ESMF_CompSet 

      public ESMF_CompValidate, ESMF_CompPrint

      public ESMF_CompSetVMMaxThreads
      public ESMF_CompSetVMMinThreads
      public ESMF_CompSetVMMaxPEs
      public ESMF_CompWait

!EOPI

      public operator(.eq.), operator(.ne.)

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Comp.F90,v 1.112 2004/11/01 23:39:38 nscollins Exp $'
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
      subroutine ESMF_CompConstruct(compp, ctype, name, gridcomptype, &
                          dirPath, configFile, config, grid, clock, parent, &
                          vm, petlist, rc)
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
!    List of {\tt PET}s for this component.
!   \item[{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
        ! Local vars
        integer :: status                            ! local error status
        logical :: rcpresent                         ! did user specify rc?
        character(len=ESMF_MAXSTR) :: fullpath       ! config file + dirPath
        character(len=ESMF_MAXSTR) :: msgbuf

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Fill in values
        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        ! component type
        compp%ctype = ctype

        ! initialize base class, including component name
        call ESMF_BaseCreate(compp%base, "Component", name, 0, status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! parent VM
        if (present(vm)) then
          compp%vm_parent = vm
        else
          ! if parent specified, use their vm, else use global.
          if (present(parent)) then
            compp%vm_parent = parent%vm
          else
            call ESMF_VMGetGlobal(compp%vm_parent)
          endif
        endif
      
        ! for gridded components, the model type it represents
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
          ! TODO: rationalize return codes from config with ESMF codes
          if (status .ne. 0) then
              ! try again with the dirPath concatinated on front
              fullpath = trim(dirPath) // '/' // trim(configFile)
              call ESMF_ConfigLoadFile(compp%config, fullpath, rc=status)
              ! TODO: construct a msg string and then call something here.
              ! if (ESMF_LogMsgFoundError(status, msgstr, rc)) return
              if (status .ne. 0) then
	        call ESMF_BaseDestroy(compp%base)
                write(msgbuf, *) &
                  "ERROR: loading config file, unable to open either", &
                  " name = ", trim(configFile), " or name = ", trim(fullpath)
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  msgbuf, &
                                  ESMF_CONTEXT, rc)) return
              endif
          endif
        else if (present(config)) then
          ! store already opened config object
          compp%config = config
        else
          ! need a way to set config to 0/invalid
          ! compp%config = ?
        endif

        ! default grid for a gridded component
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
          allocate(compp%petlist(size(petlist)))
          compp%petlist = petlist
        else
          compp%npetlist = 0
          allocate(compp%petlist(0))
        endif
      
        ! instantiate a default VMPlan
        call ESMF_VMPlanConstruct(compp%vmplan, compp%vm_parent, &
                                  compp%npetlist, compp%petlist)
                                  
        ! Create an empty subroutine/internal state table.
        call c_ESMC_FTableCreate(compp%this, status) 
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
   
        compp%compstatus = ESMF_STATUS_READY

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
        logical :: rcpresent                    ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        ! mark obj invalid
        compp%compstatus = ESMF_STATUS_INVALID

        ! call C++ to release function and data pointer tables.
        call c_ESMC_FTableDestroy(compp%this, status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Release attributes and other things on base class
        call ESMF_BaseDestroy(compp%base, status)
        ! if (ESMF_LogPassFoundError(status, rc)) return
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        
        ! Deallocate space held for petlist
        deallocate(compp%petlist)

        ! Shut down this component's VM
        call ESMF_VMShutdown(compp%vm_parent, compp%vmplan, compp%vm_info)

        ! destruct the VMPlan
        call ESMF_VMPlanDestruct(compp%vmplan)

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_CompDestruct



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Component Init, Run, and Finalize methods
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompInitialize"
!BOPI
! !IROUTINE: ESMF_CompInitialize -- Call the Component's init routine

! !INTERFACE:
      recursive subroutine ESMF_CompInitialize(compp, importState, &
                                 exportState, clock, phase, blockingFlag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingFlag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for a component.
!
!  The arguments are:
!  \begin{description}
!
!   \item[compp]
!    Component to call Initialization routine for.
!   \item[{[importState]}]  
!    Import data for initialization.
!   \item[{[exportState]}]  
!    Export data for initialization.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[phase]}]  
!    If multiple-phase init, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
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
        type(ESMF_State) :: is, es
        logical :: isdel, esdel
        integer :: dummy
        type(ESMF_BlockingFlag):: blocking
        integer :: callrc

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif
        
        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        ! set the default mode to ESMF_BLOCKING
        if (present(blockingFlag)) then
          blocking = blockingFlag
        else
          blocking = ESMF_BLOCKING
        endif

        ! supply default objects if unspecified by the caller
        if (present(importState)) then
          is = importState
          isdel = .FALSE.
        else
          ! create an empty state
          is = ESMF_StateCreate(rc=rc)
          isdel = .TRUE.
        endif

        if (present(exportState)) then
          es = exportState
          esdel = .FALSE.
        else
          ! create an empty state
          es = ESMF_StateCreate(rc=rc)
          esdel = .TRUE.
        endif

        ! and something for clocks?
        if (present(clock)) then
            ! all is well
        else
            call ESMF_LogWrite("Component Initialize called without a clock", &
                               ESMF_LOG_WARNING)
        endif

        call ESMF_GetName(compp%base, cname, status)

        ! Wrap comp so it's passed to C++ correctly.
        compp%compw%compp => compp

        ! Set up the arguments, then make the call
        call c_ESMC_FTableSetStateArgs(compp%this, ESMF_SETINIT, phase, &
                                       compp%compw, is, es, clock, status)
          
        call c_ESMC_FTableCallEntryPointVM(compp%vm_parent, compp%vmplan, &
          compp%vm_info, compp%vm_cargo, compp%this, ESMF_SETINIT, phase, &
          status)
        if (blocking == ESMF_BLOCKING) then
          call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
            compp%vm_cargo, callrc, status)
          status = callrc
        endif
#if 0
        ! Old entry point, pre-VM
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETINIT, phase, &
          status)
#endif

        ! set error code but fall thru and clean up states
        if (ESMF_LogMsgFoundError(status, &
                                  "Component initialization error", &
                                  ESMF_CONTEXT, rc)) continue
        ! fall thru intentionally

        ! if we created dummy states, delete them here.
        if (isdel) call ESMF_StateDestroy(is, rc=dummy)
        if (esdel) call ESMF_StateDestroy(es, rc=dummy)
      
        ! Set return values
        if (rcpresent) rc = status

        end subroutine ESMF_CompInitialize


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
!    Use {\tt ESMF\_BLOCKING} (default) or {\tt ESMF\_NONBLOCKING}.
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
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        call ESMF_GetName(compp%base, cname, status)

        ! set the default mode to ESMF_BLOCKING
        if (present(blockingFlag)) then
          blocking = blockingFlag
        else
          blocking = ESMF_BLOCKING
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
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        call ESMF_GetName(compp%base, cname, status)

        ! set the default mode to ESMF_BLOCKING
        if (present(blockingFlag)) then
          blocking = blockingFlag
        else
          blocking = ESMF_BLOCKING
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompFinalize"
!BOPI
! !IROUTINE: ESMF_CompFinalize -- Call the Component's finalize routine

! !INTERFACE:
      recursive subroutine ESMF_CompFinalize(compp, importState, &
                                  exportState, clock, phase, blockingFlag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingFlag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user finalize code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!   \item[compp]
!    Component to call Finalize routine for.
!   \item[{[importState]}]  
!    Import data for finalize.
!   \item[{[exportState]}]  
!    Export data for finalize.
!   \item[{[clock]}]  
!    External clock for passing in time information.
!   \item[{[phase]}]  
!    If multiple-phase finalize, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHAS} for non-multiples.
!   \item[{[blockingFlag]}]  
!    Use {\tt ESMF\_BLOCKING} (default) or {\tt ESMF\_NONBLOCKING}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOPI

        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        character(ESMF_MAXSTR) :: cname
        type(ESMF_State) :: is, es
        logical :: isdel, esdel
        integer :: dummy
        type(ESMF_BlockingFlag):: blocking
        integer :: callrc

        ! Finalize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        ! set the default mode to ESMF_BLOCKING
        if (present(blockingFlag)) then
          blocking = blockingFlag
        else
          blocking = ESMF_BLOCKING
        endif

        ! supply default objects if unspecified by the caller
        if (present(importState)) then
          is = importState
          isdel = .FALSE.
        else
          ! create an empty state
          is = ESMF_StateCreate(rc=rc)
          isdel = .TRUE.
        endif

        if (present(exportState)) then
          es = exportState
          esdel = .FALSE.
        else
          ! create an empty state
          es = ESMF_StateCreate(rc=rc)
          esdel = .TRUE.
        endif

        ! and something for clocks?
        if (present(clock)) then
            ! all is well
        else
            call ESMF_LogWrite("Component Initialize called without a clock", &
                               ESMF_LOG_WARNING)
        endif

        call ESMF_GetName(compp%base, cname, status)

        ! Wrap comp so it's passed to C++ correctly.
        compp%compw%compp => compp

        ! Set up the arguments before the call     
        call c_ESMC_FTableSetStateArgs(compp%this, ESMF_SETFINAL, phase, &
                       compp%compw, importState, exportState, clock, status)
        
        call c_ESMC_FTableCallEntryPointVM(compp%vm_parent, compp%vmplan, &
          compp%vm_info, compp%vm_cargo, compp%this, ESMF_SETFINAL, phase, &
          status)
        if (blocking == ESMF_BLOCKING) then
          call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
            compp%vm_cargo, callrc, status)
          status = callrc
        endif
#if 0
        ! old pre-VM interface
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETFINAL, phase, &
          status)
#endif

        ! set error code but fall thru and clean up states
        if (ESMF_LogMsgFoundError(status, &
                                  "Component finalize error", &
                                  ESMF_CONTEXT, rc)) continue
        ! fall thru intentionally

        ! if we created dummy states, delete them here.
        if (isdel) call ESMF_StateDestroy(is, rc=dummy)
        if (esdel) call ESMF_StateDestroy(es, rc=dummy)
      
        ! Set return values
        if (rcpresent) rc = status

        end subroutine ESMF_CompFinalize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CompRun"
!BOPI
! !IROUTINE: ESMF_CompRun -- Call the Component's run routine

! !INTERFACE:
      recursive subroutine ESMF_CompRun(compp, importState, &
                                    exportState, clock, phase, blockingFlag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(in), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingFlag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user run code for a component.
!
!    
!  The arguments are:
!  \begin{description}
!
!   \item[compp]
!    Component to call Run routine for.
!   \item[{[importState]}]  
!     Import data for run.
!   \item[{[exportState]}]  
!     Export data for run.
!   \item[{[clock]}]  
!     External clock for passing in time information.
!   \item[{[phase]}]  
!    If multiple-phase run, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[blockingFlag]}]  
!    Use {\tt ESMF\_BLOCKING} (default) or {\tt ESMF\_NONBLOCKING}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOPI

        ! local vars
        integer :: status                       ! local error status
        logical :: rcpresent                    ! did user specify rc?
        character(ESMF_MAXSTR) :: cname
        type(ESMF_State) :: is, es
        logical :: isdel, esdel
        integer :: dummy
        type(ESMF_BlockingFlag):: blocking
        integer :: callrc

        ! Run return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        ! set the default mode to ESMF_BLOCKING
        if (present(blockingFlag)) then
          blocking = blockingFlag
        else
          blocking = ESMF_BLOCKING
        endif

        ! handle creating defaults if not specified by the user
        if (present(importState)) then
          is = importState
          isdel = .FALSE.
        else
          ! create an empty state
          is = ESMF_StateCreate(rc=rc)
          isdel = .TRUE.
        endif

        if (present(exportState)) then
          es = exportState
          esdel = .FALSE.
        else
          ! create an empty state
          es = ESMF_StateCreate(rc=rc)
          esdel = .TRUE.
        endif

        ! and something for clocks?
        if (present(clock)) then
            ! all is well
        else
            call ESMF_LogWrite("Component Initialize called without a clock", &
                               ESMF_LOG_WARNING)
        endif

        call ESMF_GetName(compp%base, cname, status)

        ! Wrap comp so it's passed to C++ correctly.
        compp%compw%compp => compp

        ! Set up the arguments before the call     
        call c_ESMC_FTableSetStateArgs(compp%this, ESMF_SETRUN, phase, &
                       compp%compw, importState, exportState, clock, status)
        
        call c_ESMC_FTableCallEntryPointVM(compp%vm_parent, compp%vmplan, &
          compp%vm_info, compp%vm_cargo, compp%this, ESMF_SETRUN, phase, status)
        if (blocking == ESMF_BLOCKING) then
          call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
            compp%vm_cargo, callrc, status)
          status = callrc
        endif
#if 0
        ! old pre-VM name
        call c_ESMC_FTableCallEntryPoint(compp%this, ESMF_SETRUN, phase, &
          status)
#endif

        ! set error code but fall thru and clean up states
        if (ESMF_LogMsgFoundError(status, &
                                  "Component run error", &
                                  ESMF_CONTEXT, rc)) continue
        ! fall thru intentionally

        ! if we created dummy states, delete them here.
        if (isdel) call ESMF_StateDestroy(is, rc=dummy)
        if (esdel) call ESMF_StateDestroy(es, rc=dummy)
      
        ! Set return values
        if (rcpresent) rc = status

        end subroutine ESMF_CompRun



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
      subroutine ESMF_CompGet(compp, name, vm, vm_parent, vmplan, vm_info, &
                              gridcomptype, grid, clock, dirPath, configFile, &
                              config, ctype, rc)
!
! !ARGUMENTS:
      type (ESMF_CompClass), pointer :: compp
      character(len=*), intent(out), optional :: name
      type(ESMF_VM), intent(out), optional :: vm
      type(ESMF_VM), intent(out), optional :: vm_parent
      type(ESMF_VMPlan), intent(out), optional :: vmplan
      type(ESMF_Pointer), intent(out), optional :: vm_info
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
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
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
      subroutine ESMF_CompSet(compp, name, vm, vm_info, gridcomptype, grid, &
                              clock, dirPath, configFile, config, rc)
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
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

        if (compp%compstatus .ne. ESMF_STATUS_READY) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
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
        if (present(rc)) rc = ESMF_FAILURE
 
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
 
        if (present(rc)) rc = ESMF_FAILURE
 
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
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

       if (compp%compstatus .ne. ESMF_STATUS_READY) then
           if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                     "uninitialized or destroyed component", &
                                     ESMF_CONTEXT, rc)) return
       endif

       defaultopts = "brief"

       ! Make sure object is internally consistent
       if(present(options)) then
           ! validate object at the requested level
       else
           ! do default validation
       endif

       ! TODO: add code here

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
!      Routine to print information about a component.
!
!EOPI

       integer :: status                       ! local error status
       logical :: rcpresent                    ! did user specify rc?
       character (len=6) :: defaultopts
       character (len=ESMF_MAXSTR) :: cname
       !character (len=ESMF_MAXSTR) :: msgbuf

       ! Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
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

       if (compp%compstatus .ne. ESMF_STATUS_READY) then
          !nsc  call ESMF_LogWrite("Invalid or uninitialized Component",  &
          !nsc                      ESMF_LOG_INFO)
          write (*,*)  "Invalid or uninitialized Component"
          return
       endif

       call ESMF_GetName(compp%base, cname, status)
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
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
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
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
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
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
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
  subroutine ESMF_CompWait(compp, rc)
!
! !ARGUMENTS:
    type (ESMF_CompClass), pointer ::             compp
    integer, intent(out), optional ::             rc           
!
! !DESCRIPTION:
!     Wait for component to return
!
!     The arguments are:
!     \begin{description}
!     \item[compp] 
!          component object
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: status                     ! local error status
    logical :: rcpresent
    integer :: callrc

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

        if (.not.associated(compp)) then
            if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "Uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
        endif

    if (compp%compstatus .ne. ESMF_STATUS_READY) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                  "uninitialized or destroyed component", &
                                  ESMF_CONTEXT, rc)) return
    endif

    ! call into C++ 
    call c_ESMC_CompWait(compp%vm_parent, compp%vmplan, compp%vm_info, &
                         compp%vm_cargo, callrc, status)
    ! TODO: what is the relationship between callrc and status and rc
    if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (rcpresent) rc = callrc
 
  end subroutine ESMF_CompWait
!------------------------------------------------------------------------------

end module ESMF_CompMod
