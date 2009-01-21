! $Id: ESMF_GridComp.F90,v 1.94.2.4 2009/01/21 21:25:24 cdeluca Exp $
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
!
#define ESMF_FILENAME "ESMF_GridComp.F90"
!     ESMF Gridded Component module
      module ESMF_GridCompMod
!
!==============================================================================
!
! This file contains the Gridded Component class definition and all 
!  Gridded Component class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_GridCompMod - Gridded Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt ESMF\_GridComp} class and associated functions and subroutines.  
!
!
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_VMMod
      use ESMF_LogErrMod
      use ESMF_ConfigMod
      use ESMF_ClockMod
      use ESMF_ClockTypeMod
      use ESMF_GridMod
      use ESMF_StateTypesMod
      use ESMF_StateMod
      use ESMF_CompMod
      use ESMF_InitMacrosMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private


!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_GridCompCreate
      public ESMF_GridCompDestroy

      public ESMF_GridCompGet
      public ESMF_GridCompSet
 
      public ESMF_GridCompGetInit
      public ESMF_GridCompValidate
      public ESMF_GridCompPrint
 
      ! These do argument processing and then call the user-provided routines.
      public ESMF_GridCompInitialize
      public ESMF_GridCompRun
      public ESMF_GridCompFinalize

      ! Other routines the user might request to setup.
      public ESMF_GridCompWriteRestart
      public ESMF_GridCompReadRestart
      !public ESMF_GridCompWrite
      !public ESMF_GridCompRead

      ! Procedures for VM-enabled mode      
      public ESMF_GridCompSetVMMaxThreads
      public ESMF_GridCompSetVMMinThreads
      public ESMF_GridCompSetVMMaxPEs
      ! Return from user-provided routines
      public ESMF_GridCompWait
      
      ! function to simplify user code pet-conditionals
      public ESMF_GridCompIsPetLocal
     
      ! interface blocks for ESMF routines that are implemented in C
      public :: ESMF_GridCompSetEntryPoint
      public :: ESMF_GridCompSetServices

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_GridComp.F90,v 1.94.2.4 2009/01/21 21:25:24 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_GridCompCreate - Create a Gridded Component
!
! !INTERFACE:
      interface ESMF_GridCompCreate

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_GridCompCreate
        
! !DESCRIPTION:
!     This interface provides an entry point for methods that create an
!     {\tt ESMF\_GridComp}.  The difference is whether an already
!     created configuration object is passed in, or a filename of a new
!     config file which needs to be opened.  Also whether the resources are
!     to default, to be specified explicitly, or inherited from a parent
!     component.
!

!EOPI
      end interface

!==============================================================================
      interface
          subroutine services(comp, rc)
            use ESMF_CompMod
            type(ESMF_CompClass) :: comp 
            integer :: rc
          end subroutine
      end interface

!==============================================================================
      interface
!BOPI
! !IROUTINE: ESMF_GridCompSetEntryPoint
!
! !INTERFACE:
        subroutine ESMF_GridCompSetEntryPoint (comp, subroutineType, subroutineName, phase, rc)

! !ARGUMENTS:
	  use ESMF_CompMod
	  implicit none
	  type(ESMF_GridComp) :: comp
	  character(*), intent(in) :: subroutineType
	  interface
            subroutine subroutineName (comp, importState, exportState, clock, rc)
        	use ESMF_CompMod
        	use ESMF_StateMod
        	use ESMF_ClockMod
        	implicit none
        	type(ESMF_GridComp) :: comp
        	type(ESMF_State) :: importState, exportState
        	type(ESMF_Clock) :: clock
        	integer, intent(out) :: rc
            end subroutine
	  end interface
	  integer, intent(in) :: phase
	  integer, intent(out) :: rc

! !DESCRIPTION:
!     Registers a user-supplied initialization, run, or finalize call-back
!     routine for a grid component.
!EOPI
	end subroutine
      end interface

      interface
!BOPI
! !IROUTINE: ESMF_GridCompSetServices
!
! !INTERFACE:
        subroutine ESMF_GridCompSetServices (comp, subroutineName, rc)

! !ARGUMENTS:
          use ESMF_CompMod
          implicit none
          type(ESMF_GridComp), intent(inout) :: comp
          interface
            subroutine subroutineName (comp, rc)
              use ESMF_CompMod
              implicit none
              type(ESMF_GridComp) :: comp
              integer, intent(out) :: rc
            end subroutine
          end interface
          integer, intent(out) :: rc
! !DESCRIPTION:
!     Registers a user-supplied subroutine whose purpose is to then
!     register initialization, run, and finalize call-back routines
!     for a grid component.
!EOPI
        end subroutine
      end interface

!==============================================================================

      contains

!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompCreate"
!BOP
! !IROUTINE: ESMF_GridCompCreate - Create a Gridded Component
!
! !INTERFACE:
      recursive function ESMF_GridCompCreate(name, gridcomptype, grid, &
        config, configFile, clock, petList, contextflag, parentVm, rc)
!
! !RETURN VALUE:
      type(ESMF_GridComp) :: ESMF_GridCompCreate
!
! !ARGUMENTS:
      !external :: services
      character(len=*),        intent(in),    optional :: name
      type(ESMF_GridCompType), intent(in),    optional :: gridcomptype 
      type(ESMF_Grid),         intent(inout),    optional :: grid
      type(ESMF_Config),       intent(inout),    optional :: config
      character(len=*),        intent(in),    optional :: configFile
      type(ESMF_Clock),        intent(inout), optional :: clock
      integer,                 intent(in),    optional :: petList(:)
      type(ESMF_ContextFlag),  intent(in),    optional :: contextflag
      type(ESMF_VM),           intent(inout),    optional :: parentVm
      integer,                 intent(out),   optional :: rc 
!
! !DESCRIPTION:
!  Create an {\tt ESMF\_GridComp} object.
!
!  The return value is the new {\tt ESMF\_GridComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[name]}]
!    Name of the newly-created {\tt ESMF\_GridComp}.  This name can be altered
!    from within the {\tt ESMF\_GridComp} code once the initialization routine
!    is called.
!   \item[{[gridcomptype]}]
!    {\tt ESMF\_GridComp} model type, where model includes 
!    {\tt ESMF\_ATM, ESMF\_LAND, ESMF\_OCEAN, ESMF\_SEAICE, ESMF\_RIVER}.  
!    Note that this has no meaning to the framework, it is an
!    annotation for user code to query.
!   \item[{[grid]}]
!    Default {\tt ESMF\_Grid} associated with this {\tt gridcomp}.
!   \item[{[config]}]
!    An already-created {\tt ESMF\_Config} configuration object
!    from which the new component
!    can read in namelist-type information to set parameters for this run.
!    If both are specified, this object takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    The filename of an {\tt ESMF\_Config} format file.  
!    If specified, this file is opened an {\tt ESMF\_Config} configuration
!    object is created for the file, and attached to the new component.  
!    The user can call {\tt ESMF\_GridCompGet()} to get and use the object.
!    If both are specified, the {\tt config} object takes priority
!    over this one.
!   \item[{[clock]}]
!    Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!    queried and updated by the new {\tt ESMF\_GridComp} as it chooses.
!    This should
!    not be the parent component clock, which should be maintained and passed
!    down to the initialize/run/finalize routines separately.
!   \item[{[petList]}]
!    List of parent {\tt PET}s given to the created child component by the
!    parent component. If {\tt petList} is not specified all of the
!    parent {\tt PET}s will be given to the child component. The order of
!    PETs in {\tt petList} determines how the child local PETs refer back to
!    the parent PETs.
!   \item[{[contextflag]}]
!    Specify the component's VM context. The default context is
!    {\tt ESMF\_CHILD\_IN\_NEW\_VM}. See section \ref{opt:contextflag} for a
!    complete list of valid flags.
!   \item[{[parentVm]}]
!    {\tt ESMF\_VM} object for the current component. This will become the
!    parent {\tt ESMF\_VM} for the newly created {\tt ESMF\_GridComp} object.
!    By default the current VM is determined automatically.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: localrc                               ! local error status

        ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVm,rc)

        ! Initialize the pointer to null.
        nullify(ESMF_GridCompCreate%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ! Allocate a new comp class
        allocate(compclass, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "compclass", &
                                       ESMF_CONTEXT, rc)) return
   
        ! Call construction method to initialize gridcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_GRID, name, &
                                gridcomptype=gridcomptype, &
                                configFile=configFile, &
                                config=config, grid=grid, clock=clock, &
                                vm=parentVm, petList=petList, &
                                contextflag=contextflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_GridCompCreate%compp => compclass

        ESMF_INIT_SET_CREATED(ESMF_GridCompCreate)
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_GridCompCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompDestroy"
!BOP
! !IROUTINE: ESMF_GridCompDestroy - Release resources for a GridComp
!
! !INTERFACE:
      subroutine ESMF_GridCompDestroy(gridcomp, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_GridComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridcomp]
!       Release all resources associated with this {\tt ESMF\_GridComp}
!       and mark the object as invalid.  It is an error to pass this
!       object into any other routines after being destroyed.
!     \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        ! local vars
        integer :: localrc                       ! local error status

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

        ! Check to see if already destroyed
        if (.not.associated(gridcomp%compp)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                            "GridComp not initialized or already destroyed", &
                             ESMF_CONTEXT, rc)) return
        endif

        ! call Destruct to release resources
        call ESMF_CompDestruct(gridcomp%compp, localrc)
        ! if (ESMF_LogPassFoundError(localrc, rc)) return
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Deallocate the gridcomp struct itself
        deallocate(gridcomp%compp, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "compclass dealloc", &
                                       ESMF_CONTEXT, rc)) return

        nullify(gridcomp%compp)
 
        ESMF_INIT_SET_DELETED(gridcomp)
        ! Set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_GridCompDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompGetInit"
!BOPI
! !IROUTINE:  ESMF_GridCompGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_GridCompGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_GridComp), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_GridCompGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt GridComp}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_GridComp} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_GridCompGetInit = ESMF_INIT_GET(d)
       else
         ESMF_GridCompGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_GridCompGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompFinalize"
!BOP
! !IROUTINE: ESMF_GridCompFinalize - Call the GridComp's finalize routine
!
! !INTERFACE:
      recursive subroutine ESMF_GridCompFinalize(gridcomp, importState, &
                                   exportState, clock, phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_GridComp)                              :: gridcomp
      type (ESMF_State),        intent(inout), optional :: importState
      type (ESMF_State),        intent(inout), optional :: exportState
      type (ESMF_Clock),        intent(inout),    optional :: clock
      integer,                  intent(in),    optional :: phase
      type (ESMF_BlockingFlag), intent(in),    optional :: blockingflag
      integer,                  intent(out),   optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user-supplied finalization code for 
!  an {\tt ESMF\_GridComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    The {\tt ESMF\_GridComp} to call finalize routine for.
!   \item[{[importState]}]  
!    {\tt ESMF\_State} containing import data. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    importState argument in the user code cannot be optional. 
!   \item[{[exportState]}]  
!    {\tt ESMF\_State} containing export data. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    exportState argument in the user code cannot be optional. 
!   \item[{[clock]}]  
!    External {\tt ESMF\_Clock} for passing in time information.  
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    clock argument in the user code cannot be optional. 
!   \item[{[phase]}]  
!      Component providers must document whether their each of their
!      routines are {\em single-phase} or {\em multi-phase}.
!      Single-phase routines require only one invocation to complete
!      their work.
!      Multi-phase routines provide multiple subroutines to accomplish
!      the work, accomodating components which must complete part of their
!      work, return to the caller and allow other processing to occur,
!      and then continue the original operation.
!      For single-phase child components this argument is optional, but
!      if specified it must be {\tt ESMF\_SINGLEPHASE}.
!      For multiple-phase child components, this is the integer phase
!      number to be invoked.
!   \item[{[blockingflag]}]  
!    Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!    for a list of valid blocking options. Default option is
!    {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!    across each VAS but does not synchronize PETs that run in different VASs.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                        ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)

        call ESMF_CompExecute(gridcomp%compp, importState, exportState, &
          clock=clock, methodtype=ESMF_SETFINAL, phase=phase, &
          blockingflag=blockingflag, rc=localrc)
        ! Use LogErr to handle return code
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_GridCompFinalize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompGet"
!BOP
! !IROUTINE: ESMF_GridCompGet - Query a GridComp for information
!
! !INTERFACE:
      subroutine ESMF_GridCompGet(gridcomp, name, gridcomptype, &
        grid, config, configFile, clock, vm, contextflag, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp),     intent(inout)            :: gridcomp
      character(len=*),        intent(out), optional :: name
      type(ESMF_GridCompType), intent(out), optional :: gridcomptype 
      type(ESMF_Grid),         intent(out), optional :: grid
      type(ESMF_Config),       intent(out), optional :: config
      character(len=*),        intent(out), optional :: configFile
      type(ESMF_Clock),        intent(out), optional :: clock
      type(ESMF_VM),           intent(out), optional :: vm
      type(ESMF_ContextFlag),  intent(out), optional :: contextflag
      integer,                 intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Returns information about an {\tt ESMF\_GridComp}.
!  For queries where the caller
!  only wants a single value, specify the argument by name.
!  All the arguments after the {\tt gridcomp} argument are optional
!  to facilitate this.
!   
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    {\tt ESMF\_GridComp} object to query.
!   \item[{[name]}]
!    Return the name of the {\tt ESMF\_GridComp}.
!   \item[{[gridcomptype]}]
!    Return the model type of this {\tt ESMF\_GridComp}.
!   \item[{[grid]}]
!    Return the {\tt ESMF\_Grid} associated with this {\tt ESMF\_GridComp}.
!   \item[{[config]}]
!    Return the {\tt ESMF\_Config} object for this {\tt ESMF\_GridComp}.
!   \item[{[configFile]}]
!    Return the configuration filename for this {\tt ESMF\_GridComp}.
!   \item[{[clock]}]
!    Return the private clock for this {\tt ESMF\_GridComp}.
!   \item[{[vm]}]
!    Return the {\tt ESMF\_VM} for this {\tt ESMF\_GridComp}.
!   \item[{[contextflag]}]
!    Return the {\tt ESMF\_ContextFlag} for this {\tt ESMF\_GridComp}.
!    See section \ref{opt:contextflag} for a complete list of valid flags.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                        ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

        call ESMF_CompGet(gridcomp%compp, name, vm=vm, contextflag=contextflag,&
                          gridcomptype=gridcomptype, grid=grid, clock=clock, &
                          configFile=configFile, config=config, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_GridCompGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompInitialize"
!BOP
! !IROUTINE: ESMF_GridCompInitialize - Call the GridComp's initialize routine

! !INTERFACE:
      recursive subroutine ESMF_GridCompInitialize(gridcomp, importState, &
                                  exportState, clock, phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_GridComp)                              :: gridcomp
      type (ESMF_State),        intent(inout), optional :: importState
      type (ESMF_State),        intent(inout), optional :: exportState
      type (ESMF_Clock),        intent(inout), optional :: clock
      integer,                  intent(in),    optional :: phase
      type (ESMF_BlockingFlag), intent(in),    optional :: blockingflag
      integer,                  intent(out),   optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for a gridcomp.
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    {\tt ESMF\_GridComp} to call initialize routine for.
!   \item[{[importState]}]  
!    {\tt ESMF\_State} containing import data for coupling. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    importState argument in the user code cannot be optional. 
!   \item[{[exportState]}]  
!    {\tt ESMF\_State} containing export data for coupling. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    exportState argument in the user code cannot be optional. 
!   \item[{[clock]}]  
!    External {\tt ESMF\_Clock} for passing in time information.  
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    clock argument in the user code cannot be optional. 
!   \item[{[phase]}]
!    Component providers must document whether their each of their
!    routines are {\em single-phase} or {\em multi-phase}.
!    Single-phase routines require only one invocation to complete
!    their work.
!    Multi-phase routines provide multiple subroutines to accomplish
!    the work, accomodating components which must complete part of their
!    work, return to the caller and allow other processing to occur,
!    and then continue the original operation.
!    For single-phase child components this argument is optional, but
!    if specified it must be {\tt ESMF\_SINGLEPHASE}.
!    For multiple-phase child components, this is the integer phase
!    number to be invoked.
!   \item[{[blockingflag]}]
!    Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!    for a list of valid blocking options. Default option is
!    {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!    across each VAS but does not synchronize PETs that run in different VASs.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                        ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)

        call ESMF_CompExecute(gridcomp%compp, importState, exportState, &
          clock=clock, methodtype=ESMF_SETINIT, phase=phase, &
          blockingflag=blockingflag, rc=localrc)
        ! Use LogErr to handle return code
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_GridCompInitialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompPrint"
!BOP
! !IROUTINE:  ESMF_GridCompPrint - Print the contents of a GridComp
!
! !INTERFACE:
      subroutine ESMF_GridCompPrint(gridcomp, options, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gridcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Prints information about an {\tt ESMF\_GridComp} to {\tt stdout}.
!
!  Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!  On some platforms/compilers there is a potential issue with interleaving
!  Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!  the expected order.  If this occurs, it is recommended to use the
!  standard Fortran call {\tt flush(6)} as a workaround until this issue
!  is fixed in a future release. 
!
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    {\tt ESMF\_GridComp} to print.
!   \item[{[options]}]
!    Print options are not yet supported.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
       integer :: localrc                        ! local return code

       ! Assume failure until success
       if (present(rc)) rc = ESMF_RC_NOT_IMPL
       localrc = ESMF_RC_NOT_IMPL

       ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)


     !jw  call ESMF_LogWrite("Gridded Component:", ESMF_LOG_INFO)
       print *, "Gridded Component:"
       call ESMF_CompPrint(gridcomp%compp, options, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (present(rc)) rc = ESMF_SUCCESS
       end subroutine ESMF_GridCompPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompReadRestart"
!BOPI
! !IROUTINE: ESMF_GridCompReadRestart - Call the GridComp's restore routine
!
! !INTERFACE:
      recursive subroutine ESMF_GridCompReadRestart(gridcomp, iospec, clock, &
                                                    phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_GridComp), intent(inout) :: gridcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(inout), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user restore code for a {\tt gridcomp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    {\tt ESMF\_GridComp} object to call readrestart routine for.
!   \item[{[iospec]}]
!    {\tt ESMF\_IOSpec} object which describes I/O options.
!   \item[{[clock]}]
!    External {\tt ESMF\_Clock} for passing in time information.
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations.
!    {\tt ESMF\_State} containing export data for coupling.
!   \item[{[phase]}]
!    Component providers must document whether their each of their
!    routines are {\em single-phase} or {\em multi-phase}.  
!    Single-phase routines require only one invocation to complete
!    their work.  
!    Multi-phase routines provide multiple subroutines to accomplish
!    the work, accomodating components which must complete part of their
!    work, return to the caller and allow other processing to occur,
!    and then continue the original operation.
!    For single-phase child components this argument is optional, but  
!    if specified it must be {\tt ESMF\_SINGLEPHASE}.
!    For multiple-phase child components, this is the integer phase  
!    number to be invoked.
!    If multiple-phase restore, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[blockingflag]}]
!    Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!    for a list of valid blocking options. Default option is
!    {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!    across each VAS but does not synchronize PETs that run in different VASs.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                        ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

	! Change BOPI to BOP when implemented.
        call ESMF_CompReadRestart(gridcomp%compp, iospec, clock, phase, &
                                  blockingflag, rc=localrc)
        ! Use LogErr to handle return code
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_GridCompReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompRun"
!BOP
! !IROUTINE: ESMF_GridCompRun - Call the GridComp's run routine
!
! !INTERFACE:
      recursive subroutine ESMF_GridCompRun(gridcomp, importState, exportState,&
                                            clock, phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_GridComp)                              :: gridcomp
      type (ESMF_State),        intent(inout), optional :: importState
      type (ESMF_State),        intent(inout), optional :: exportState
      type (ESMF_Clock),        intent(inout),    optional :: clock
      integer,                  intent(in),    optional :: phase
      type (ESMF_BlockingFlag), intent(in),    optional :: blockingflag
      integer,                  intent(out),   optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user run code for an {\tt ESMF\_GridComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    {\tt ESMF\_GridComp} to call run routine for.
!   \item[{[importState]}]  
!    {\tt ESMF\_State} containing import data. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    importState argument in the user code cannot be optional. 
!   \item[{[exportState]}]  
!    {\tt ESMF\_State} containing export data. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    exportState argument in the user code cannot be optional. 
!   \item[{[clock]}]  
!    External {\tt ESMF\_Clock} for passing in time information.  
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations. If not present, a dummy
!    argument will be passed to the user-supplied routine.  The 
!    clock argument in the user code cannot be optional. 
!   \item[{[phase]}]   
!    Component providers must document whether their each of their
!    routines are {\em single-phase} or {\em multi-phase}.    
!    Single-phase routines require only one invocation to complete
!    their work.    
!    Multi-phase routines provide multiple subroutines to accomplish
!    the work, accomodating components which must complete part of their
!    work, return to the caller and allow other processing to occur,
!    and then continue the original operation.
!    For single-phase child components this argument is optional, but     
!    if specified it must be {\tt ESMF\_SINGLEPHASE}.
!    For multiple-phase child components, this is the integer phase  
!    number to be invoked.
!    If multiple-phase restore, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!   \item[{[blockingflag]}]  
!    Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!    for a list of valid blocking options. Default option is
!    {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!    across each VAS but does not synchronize PETs that run in different VASs.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                        ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)

        call ESMF_CompExecute(gridcomp%compp, importState, exportState, &
          clock=clock, methodtype=ESMF_SETRUN, phase=phase, &
          blockingflag=blockingflag, rc=localrc)
        ! Use LogErr to handle return code
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_GridCompRun

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSet"
!BOP
! !IROUTINE: ESMF_GridCompSet - Set or reset information about the GridComp
!
! !INTERFACE:
      subroutine ESMF_GridCompSet(gridcomp, name, gridcomptype, grid, &
                                  config, configFile, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp),     intent(inout)         :: gridcomp
      character(len=*),        intent(in),  optional :: name
      type(ESMF_GridCompType), intent(in),  optional :: gridcomptype 
      type(ESMF_Grid),        intent(inout),  optional :: grid
      type(ESMF_Config),       intent(inout),  optional :: config
      character(len=*),        intent(in),  optional :: configFile
      type(ESMF_Clock),        intent(inout),  optional :: clock
      integer,                 intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Sets or resets information about an {\tt ESMF\_GridComp}.
!  The caller can set individual values by specifying
!  the arguments by name.
!  All the arguments except {\tt gridcomp} are optional    
!  to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    {\tt ESMF\_GridComp} to change.
!   \item[{[name]}]
!    Set the name of the {\tt ESMF\_GridComp}.
!   \item[{[gridcomptype]}]
!    Set the model type for this {\tt ESMF\_GridComp}.
!   \item[{[grid]}]
!    Set the {\tt ESMF\_Grid} associated with the {\tt ESMF\_GridComp}.
!   \item[{[config]}]
!    Set the configuration information for the {\tt ESMF\_GridComp} from
!    this already created {\tt ESMF\_Config} object.   
!    If specified, takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    Set the configuration filename for this {\tt ESMF\_GridComp}.
!    An {\tt ESMF\_Config} object will be created for this file
!    and attached to the {\tt ESMF\_GridComp}.  Superceeded by {\tt config}
!    if both are specified.
!   \item[{[clock]}]
!    Set the private clock for this {\tt ESMF\_GridComp}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                        ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

        call ESMF_CompSet(gridcomp%compp, name, &
                          gridcomptype=gridcomptype, grid=grid, clock=clock, &
                          configFile=configFile, config=config, rc=localrc)
        ! Use LogErr to handle return code
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_GridCompSet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetVMMaxThreads"
!BOPI
! !IROUTINE: ESMF_GridCompSetVMMaxThreads - Define a VM for this GridComp
!
! !INTERFACE:
  subroutine ESMF_GridCompSetVMMaxThreads(gridcomp, max, &
                     pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)            :: gridcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_GridComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridcomp] 
!      {\tt ESMF\_GridComp} to set the {\tt ESMF\_VM} for.
!     \item[{[max]}] 
!      Maximum threading level.
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference.
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference.
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference.
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: localrc                     ! local error status

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call CompClass method
    call ESMF_CompSetVMMaxThreads(gridcomp%compp, max, &
                 pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompSetVMMaxThreads

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetVMMinThreads"
!BOPI
! !IROUTINE: ESMF_GridCompSetVMMinThreads - Define a VM for this GridComp
!
! !INTERFACE:
  subroutine ESMF_GridCompSetVMMinThreads(gridcomp, max, &
                    pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)            :: gridcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_GridComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridcomp] 
!      {\tt ESMF\_GridComp} to set the {\tt ESMF\_VM} for.
!     \item[{[max]}] 
!      Maximum number of PEs per PET.
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference.
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference.
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference.
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: localrc                     ! local error status

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call CompClass method
    call ESMF_CompSetVMMinThreads(gridcomp%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompSetVMMinThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetVMMaxPEs"
!BOPI
! !IROUTINE: ESMF_GridCompSetVMMaxPEs - Define a VM for this GridComp
!
! !INTERFACE:
  subroutine ESMF_GridCompSetVMMaxPEs(gridcomp, max, &
                       pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)            :: gridcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_GridComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridcomp] 
!      {\tt ESMF\_GridComp} to set the {\tt ESMF\_VM} for.
!     \item[{[max]}] 
!      Maximum number of PEs per PET.
!     \item[{[pref\_intra\_process]}] 
!      Intra process communication preference.
!     \item[{[pref\_intra\_ssi]}] 
!      Intra SSI communication preference.
!     \item[{[pref\_inter\_ssi]}] 
!      Inter process communication preference.
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: localrc                     ! local error status

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call CompClass method
    call ESMF_CompSetVMMaxPEs(gridcomp%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompSetVMMaxPEs
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompValidate"
!BOP
! !IROUTINE: ESMF_GridCompValidate - Check validity of a GridComp
!
! !INTERFACE:
      subroutine ESMF_GridCompValidate(gridcomp, options, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gridcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!   Currently all this method does is to check that the 
!   {\tt gridcomp} exists.
!
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    {\tt ESMF\_GridComp} to validate.
!   \item[{[options]}]  
!    Validation options are not yet supported.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
      integer :: localrc                        ! local return code

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_CompValidate(gridcomp%compp, options, localrc)
      ! Use LogErr to handle return code
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

      ! TODO: also need to validate grid if it's associated here

      ! Check Init Status
      ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

      ! If all checks pass return success
      if (present(rc)) rc = ESMF_SUCCESS
 
      end subroutine ESMF_GridCompValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompWriteRestart"
!BOPI
! !IROUTINE: ESMF_GridCompWriteRestart - Call the GridComp's checkpoint routine
!
! !INTERFACE:
      recursive subroutine ESMF_GridCompWriteRestart(gridcomp, iospec, clock, &
                                                     phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_GridComp), intent(inout) :: gridcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(inout), optional :: clock
      integer, intent(in), optional :: phase
      type(ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user checkpoint code for an {\tt ESMF\_GridComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp]
!    {\tt ESMF\_GridComp} to call writerestart routine for.
!   \item[{[iospec]}]  
!    {\tt ESMF\_IOSpec} object which describes I/O options.
!   \item[{[clock]}]
!    External {\tt ESMF\_Clock} for passing in time information.
!    This is generally the parent component's clock, and will be treated
!    as read-only by the child component.  The child component can maintain
!    a private clock for its own internal time computations.
!   \item[{[phase]}]
!    Component providers must document whether their each of their
!    routines are {\em single-phase} or {\em multi-phase}.
!    Single-phase routines require only one invocation to complete
!    their work.
!    Multi-phase routines provide multiple subroutines to accomplish
!    the work, accomodating components which must complete part of their
!    work, return to the caller and allow other processing to occur,
!    and then continue the original operation.
!    For single-phase child components this argument is optional, but
!    if specified it must be {\tt ESMF\_SINGLEPHASE}.
!    For multiple-phase child components, this is the integer phase
!    number to be invoked.
!   \item[{[blockingflag]}]
!    Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!    for a list of valid blocking options. Default option is
!    {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!    across each VAS but does not synchronize PETs that run in different VASs.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                        ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

	!Change BOPI to BOP when implemented.
        call ESMF_CompWriteRestart(gridcomp%compp, iospec, clock, phase, &
                                   blockingflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_GridCompWriteRestart


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompWait"
!BOP
! !IROUTINE: ESMF_GridCompWait - Wait for a GridComp to return
!
! !INTERFACE:
  subroutine ESMF_GridCompWait(gridcomp, blockingFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)              :: gridcomp
    type (ESMF_BlockingFlag), intent(in), optional  :: blockingFlag
    integer,             intent(out), optional      :: rc           
!
! !DESCRIPTION:
!     When executing asychronously, wait for an {\tt ESMF\_GridComp} to return.
!
!     The arguments are:
!     \begin{description}
!     \item[gridcomp] 
!      {\tt ESMF\_GridComp} to wait for.
!     \item[{[blockingFlag]}]
!    Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!    for a list of valid blocking options. Default option is
!    {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!    across each VAS but does not synchronize PETs that run in different VASs.
!     \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

    integer :: localrc                     ! local error status

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call CompClass method
    call ESMF_CompWait(gridcomp%compp, blockingFlag, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_GridCompWait
!------------------------------------------------------------------------------


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompIsPetLocal"
!BOP
! !IROUTINE: ESMF_GridCompIsPetLocal - Inquire if this component is to execute on the calling PET.
!
! !INTERFACE:
      recursive function ESMF_GridCompIsPetLocal(gridcomp, rc)
!
! !RETURN VALUE:
      logical :: ESMF_GridCompIsPetLocal
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, intent(out), optional  :: rc 
!
! !DESCRIPTION:
!  Inquire if this {\tt ESMF\_GridComp} object is to execute on the calling PET.
!
!  The return value is {\tt .true.} if the component is to execute on the 
!  calling PET, {\tt .false.} otherwise.
!    
!  The arguments are:
!  \begin{description}
!   \item[gridcomp] 
!    {\tt ESMF\_GridComp} queried.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

    integer :: localrc                     ! local error status
    logical :: localresult

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ! Initialize output value in case of error
    ESMF_GridCompIsPetLocal = .false.

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call CompClass method
    localresult = ESMF_CompIsPetLocal(gridcomp%compp, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
    
    ESMF_GridCompIsPetLocal = localresult
    
  end function ESMF_GridCompIsPetLocal
    
!------------------------------------------------------------------------------

end module ESMF_GridCompMod

