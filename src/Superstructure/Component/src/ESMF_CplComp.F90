! $Id: ESMF_CplComp.F90,v 1.81.2.4 2009/01/21 21:25:24 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_CplComp.F90"
!
!     ESMF Coupler Cplcomp module
      module ESMF_CplCompMod
!
!==============================================================================
!
! This file contains the Coupler Cplcomp class definition and all 
!   Coupler Cplcomp class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_CplCompMod - Coupler Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt ESMF\_CplComp} class and associated functions and subroutines.  
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
! !PUBLIC TYPES:
      public ESMF_CplCompCreate
      public ESMF_CplCompDestroy

      public ESMF_CplCompGet
      public ESMF_CplCompSet
 
      public ESMF_CplCompValidate
      public ESMF_CplCompGetInit
      public ESMF_CplCompPrint
 
      ! These do argument processing and then call the user-provided routines.
      public ESMF_CplCompInitialize
      public ESMF_CplCompRun
      public ESMF_CplCompFinalize

      ! Other routines the user might request to setup.
      public ESMF_CplCompWriteRestart
      public ESMF_CplCompReadRestart
      !public ESMF_CplCompWrite
      !public ESMF_CplCompRead

      ! Procedures for VM-enabled mode      
      public ESMF_CplCompSetVMMaxThreads
      public ESMF_CplCompSetVMMinThreads
      public ESMF_CplCompSetVMMaxPEs
      ! Return from user-provided routines
      public ESMF_CplCompWait

      ! function to simplify user code pet-conditionals
      public ESMF_CplCompIsPetLocal

      !public operator(.eq.), operator(.ne.), assignment(=)
     
      ! interface blocks for ESMF routines that are implemented in C
      public :: ESMF_CplCompSetEntryPoint
      public :: ESMF_CplCompSetServices

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_CplComp.F90,v 1.81.2.4 2009/01/21 21:25:24 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_CplCompCreate - Create a Coupler Component
!
! !INTERFACE:
      interface ESMF_CplCompCreate

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_CplCompCreate

! !DESCRIPTION:
!     This interface provides an entry point for methods that create an 
!     {\tt ESMF\_CplComp}.  The various varieties allow the resources
!     to default, to be specified explicitly, or inherited from a parent
!     component.
!

!EOPI
      end interface

!==============================================================================

      interface
!BOPI
! !IROUTINE: ESMF_CplCompSetEntryPoint
!
! !INTERFACE:
        subroutine ESMF_CplCompSetEntryPoint (comp, subroutineType, subroutineName, phase, rc)

! !ARGUMENTS:
	  use ESMF_CompMod
	  implicit none
	  type(ESMF_CplComp) :: comp
	  character(*), intent(in) :: subroutineType
	  interface
            subroutine subroutineName (comp, importState, exportState, clock, rc)
        	use ESMF_CompMod
        	use ESMF_StateMod
        	use ESMF_ClockMod
        	implicit none
        	type(ESMF_CplComp) :: comp
        	type(ESMF_State) :: importState, exportState
        	type(ESMF_Clock) :: clock
        	integer, intent(out) :: rc
            end subroutine
	  end interface
	  integer, intent(in) :: phase
	  integer, intent(out) :: rc

! !DESCRIPTION:
!     Registers a user-supplied initialization, run, or finalize call-back
!     routine for a coupler component.
!EOPI
	end subroutine
      end interface

      interface
!BOPI
! !IROUTINE: ESMF_CplCompSetServices
!
! !INTERFACE:
        subroutine ESMF_CplCompSetServices (comp, subroutineName, rc)

! !ARGUMENTS:
          use ESMF_CompMod
          implicit none
          type(ESMF_CplComp), intent(inout) :: comp
          interface
            subroutine subroutineName (comp, rc)
              use ESMF_CompMod
              implicit none
              type(ESMF_CplComp) :: comp
              integer, intent(out) :: rc
            end subroutine
          end interface
          integer, intent(out) :: rc
! !DESCRIPTION:
!     Registers a user-supplied subroutine whose purpose is to then
!     register initialization, run, and finalize call-back routines
!     for a coupler component.
!EOPI
        end subroutine
      end interface

!------------------------------------------------------------------------------
! out for now.
!      interface assignment(=)
!        module procedure ESMF_cpas
!      end interface

!==============================================================================

      contains

!==============================================================================

!subroutine ESMF_cpas(lval, rval)
! type(ESMF_CompClass), intent(out) :: lval
! type(ESMF_CplClass), intent(in) :: rval
!
! compp = rval%compp
!end subroutine


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreate"
!BOP
! !IROUTINE: ESMF_CplCompCreate - Create a Coupler Component
!
! !INTERFACE:
      recursive function ESMF_CplCompCreate(name, config, configFile, &
                                    clock, petList, contextflag, parentVm, rc)
!
! !RETURN VALUE:
      type(ESMF_CplComp) :: ESMF_CplCompCreate
!
! !ARGUMENTS:
      character(len=*),       intent(in),  optional :: name
      type(ESMF_Config),      intent(inout),  optional :: config
      character(len=*),       intent(in),  optional :: configFile
      type(ESMF_Clock),       intent(inout),  optional :: clock
      integer,                intent(in),  optional :: petList(:)
      type(ESMF_ContextFlag), intent(in),  optional :: contextflag
      type(ESMF_VM),          intent(inout),  optional :: parentVm
      integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create an {\tt ESMF\_CplComp} object.
!
!  The return value is the new {\tt ESMF\_CplComp}.
!    
!  The arguments are:
!  \begin{description}
!   \item[{[name]}]
!    Name of the newly-created {\tt ESMF\_CplComp}.  This name can be altered 
!    from within the {\tt ESMF\_CplComp} code once the initialization routine
!    is called.
!   \item[{[config]}]
!    An already-created {\tt ESMF\_Config} configuration object 
!    from which the new component
!    can read in namelist-type information to set parameters for this run.
!    If both are specified, this object takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    The filename of an {\tt ESMF\_Config} format file.  
!    If specified, this file is opened, an {\tt ESMF\_Config} configuration
!    object is created for the file, and attached to the new component.  
!    The user can call {\tt ESMF\_CplCompGet()} to get and use the object.
!    If both are specified, the {\tt config} object takes priority 
!    over this one.
!   \item[{[clock]}]
!    Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!    queried and updated by the new {\tt ESMF\_CplComp} as it chooses.  
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
!    parent {\tt ESMF\_VM} for the newly created {\tt ESMF\_CplComp} object.
!    By default the current VM is determined automatically.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

        ! local vars
        type (ESMF_CompClass), pointer :: compclass      ! generic comp
        integer :: localrc                                ! local error localrc

        ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVm,rc)

        ! Initialize the pointer to null.
        nullify(ESMF_CplCompCreate%compp)
        nullify(compclass)

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ! Allocate a new comp class
        allocate(compclass, stat=localrc)
	if (ESMF_LogMsgFoundAllocError(localrc, "Component class", &
                                       ESMF_CONTEXT, rc)) return
   
        ! Call construction method to initialize cplcomp internals
        call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
                                configFile=configFile, config=config, &
                                clock=clock, vm=parentVm, petList=petList, &
                                contextflag=contextflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

        ! Set return values
        ESMF_CplCompCreate%compp => compclass

        ESMF_INIT_SET_CREATED(ESMF_CplCompCreate)
        if (present(rc)) rc = ESMF_SUCCESS

        end function ESMF_CplCompCreate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompDestroy"
!BOP
! !IROUTINE: ESMF_CplCompDestroy - Release resources for a CplComp

! !INTERFACE:
      subroutine ESMF_CplCompDestroy(cplcomp, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: cplcomp
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp]
!       Release all resources associated with this {\tt ESMF\_CplComp}
!       and mark the object as invalid.  It is an error to pass this
!       object into any other routines after being destroyed.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

        ! local vars
        integer :: localrc                       ! local error localrc

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

        ! Check to see if already destroyed
        if (.not.associated(cplcomp%compp)) then  
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                             "CplComp not initialized or already destroyed", &
                              ESMF_CONTEXT, rc)) return
        endif

        ! call Destruct to release resources
        call ESMF_CompDestruct(cplcomp%compp, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

        ! Deallocate the cplcomp struct itself
        deallocate(cplcomp%compp, stat=localrc)
	if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating Component class", &
                                       ESMF_CONTEXT, rc)) return
        nullify(cplcomp%compp)
 
        ESMF_INIT_SET_DELETED(cplcomp)
        ! Set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_CplCompDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompGetInit"
!BOPI
! !IROUTINE:  ESMF_CplCompGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_CplCompGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_CplComp), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_CplCompGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_CplComp} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_CplCompGetInit = ESMF_INIT_GET(d)
       else
         ESMF_CplCompGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_CplCompGetInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompFinalize"
!BOP
! !IROUTINE: ESMF_CplCompFinalize - Call the CplComp's finalize routine
!
! !INTERFACE:
    recursive subroutine ESMF_CplCompFinalize(cplcomp, importState, &
                                  exportState, clock, phase, blockingflag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(inout), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user-supplied finalization routine for 
!  an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    The {\tt ESMF\_CplComp} to call finalize routine for.
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
        integer :: localrc                       ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)

        call ESMF_CompExecute(cplcomp%compp, importState, exportState, &
          clock=clock, methodtype=ESMF_SETFINAL, phase=phase, &
          blockingflag=blockingflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_CplCompFinalize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompGet"
!BOP
! !IROUTINE: ESMF_CplCompGet - Query a CplComp for information
!
! !INTERFACE:
      subroutine ESMF_CplCompGet(cplcomp, name, config, configFile, clock, &
        vm, contextflag, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp),     intent(inout)            :: cplcomp
      character(len=*),       intent(out), optional :: name
      type(ESMF_Config),      intent(out), optional :: config
      character(len=*),       intent(out), optional :: configFile
      type(ESMF_Clock),       intent(out), optional :: clock
      type(ESMF_VM),          intent(out), optional :: vm
      type(ESMF_ContextFlag), intent(out), optional :: contextflag
      integer,                intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Returns information about an {\tt ESMF\_CplComp}.
!  For queries where the caller
!  only wants a single value, specify the argument by name.
!  All the arguments after {\tt cplcomp} argument are optional 
!  to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to query.
!   \item[{[name]}]
!    Return the name of the {\tt ESMF\_CplComp}.
!   \item[{[config]}]
!    Return the {\tt ESMF\_Config} object for this {\tt ESMF\_CplComp}.
!   \item[{[configFile]}]
!    Return the configuration filename for this {\tt ESMF\_CplComp}.
!   \item[{[clock]}]
!    Return the private clock for this {\tt ESMF\_CplComp}.
!   \item[{[vm]}]
!    Return the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!   \item[{[contextflag]}]
!    Return the {\tt ESMF\_ContextFlag} for this {\tt ESMF\_CplComp}.
!    See section \ref{opt:contextflag} for a complete list of valid flags.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                  ! local return code

        ! Assume failure until success
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

        call ESMF_CompGet(cplcomp%compp, name, vm=vm, contextflag=contextflag, &
          clock=clock, configFile=configFile, config=config, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_CplCompGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompInitialize"
!BOP
! !IROUTINE: ESMF_CplCompInitialize - Call the CplComp's initialize routine
!
! !INTERFACE:
      recursive subroutine ESMF_CplCompInitialize(cplcomp, importState, &
                                   exportState, clock, phase, blockingflag, rc)
!
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(inout), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user initialization code for an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to call initialize routine for.
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

        ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)

        call ESMF_CompExecute(cplcomp%compp, importState, exportState, &
          clock=clock, methodtype=ESMF_SETINIT, phase=phase, &
          blockingflag=blockingflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_CplCompInitialize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompPrint"
!BOP
! !IROUTINE:  ESMF_CplCompPrint - Print the contents of a CplComp
!
! !INTERFACE:
      subroutine ESMF_CplCompPrint(cplcomp, options, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: cplcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Prints information about an {\tt ESMF\_CplComp} to {\tt stdout}.
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
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to print.
!   \item[{[options]}]
!    Print options are not yet supported.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
       integer :: localrc              ! local return code

       ! Assume failure until success
       if (present(rc)) rc = ESMF_RC_NOT_IMPL
       localrc = ESMF_RC_NOT_IMPL

       ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

     !jw  call ESMF_LogWrite("Coupler Component:", ESMF_LOG_INFO)
       print *, "Coupler Component:"
       call ESMF_CompPrint(cplcomp%compp, options, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Return success
       if (present(rc)) rc = ESMF_SUCCESS
       end subroutine ESMF_CplCompPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompReadRestart"
!BOPI
! !IROUTINE: ESMF_CplCompReadRestart -- Call the CplComp's restore routine
!
! !INTERFACE:
     recursive subroutine ESMF_CplCompReadRestart(cplcomp, iospec, clock, &
                                                  phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_CplComp), intent(inout) :: cplcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(inout), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user restore code for an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to call readrestart routine for.
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
        integer :: localrc                  ! local return code

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

	! Changed BOP to BOPI until implemented.
        call ESMF_CompReadRestart(cplcomp%compp, iospec, clock, phase, &
                                  blockingflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_CplCompReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompRun"
!BOP
! !IROUTINE: ESMF_CplCompRun - Call the CplComp's run routine
!
! !INTERFACE:
    recursive subroutine ESMF_CplCompRun(cplcomp, importState, exportState, &
                                         clock, phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_CplComp) :: cplcomp
      type (ESMF_State), intent(inout), optional :: importState
      type (ESMF_State), intent(inout), optional :: exportState
      type (ESMF_Clock), intent(inout), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user run code for an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to call run routine for.
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
!    If multiple-phase restore, which phase number this is.
!    Pass in 0 or {\tt ESMF\_SINGLEPHASE} for non-multiples.
!    External clock for passing in time information.
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
        integer :: localrc                     ! local return code

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)

        call ESMF_CompExecute(cplcomp%compp, importState, exportState, &
          clock=clock, methodtype=ESMF_SETRUN, phase=phase, &
          blockingflag=blockingflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! Return success
        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_CplCompRun


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSet"
!BOP
! !IROUTINE: ESMF_CplCompSet - Set or reset information about the CplComp
!
! !INTERFACE:
      subroutine ESMF_CplCompSet(cplcomp, name, config, configFile, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: cplcomp
      character(len=*), intent(in), optional :: name
      type(ESMF_Config), intent(inout), optional :: config
      character(len=*), intent(in), optional :: configFile
      type(ESMF_Clock), intent(inout), optional :: clock
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!  Sets or resets information about an {\tt ESMF\_CplComp}.
!  The caller can set individual values by specifying
!  the arguments by name.
!  All the arguments except {\tt cplcomp} are optional 
!  to facilitate this.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to change.
!   \item[{[name]}]
!    Set the name of the {\tt ESMF\_CplComp}.
!   \item[{[config]}]
!    Set the configuration information for the {\tt ESMF\_CplComp} from
!    this already created {\tt ESMF\_Config} object.   
!    If specified, takes priority over {\tt configFile}.
!   \item[{[configFile]}]
!    Set the configuration filename for this {\tt ESMF\_CplComp}.
!    An {\tt ESMF\_Config} object will be created for this file
!    and attached to the {\tt ESMF\_CplComp}.  Superceeded by {\tt config}
!    if both are specified.
!   \item[{[clock]}]
!    Set the private clock for this {\tt ESMF\_CplComp}.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        integer :: localrc                    ! local return code

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

        call ESMF_CompSet(cplcomp%compp, name, clock=clock, &
                          configFile=configFile, config=config, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_CplCompSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMaxThreads"
!BOPI
! !IROUTINE: ESMF_CplCompSetVMMaxThreads - Define a VM for this CplComp
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMaxThreads(cplcomp, max, &
                     pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)            :: cplcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
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

    integer :: localrc                     ! local error localrc

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call CompClass method
    call ESMF_CompSetVMMaxThreads(cplcomp%compp, max, &
                   pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompSetVMMaxThreads

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMinThreads"
!BOPI
! !IROUTINE: ESMF_CplCompSetVMMinThreads - Define a VM for this CplComp
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMinThreads(cplcomp, max, &
                        pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
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

    integer :: localrc                     ! local error localrc

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call CompClass method
    call ESMF_CompSetVMMinThreads(cplcomp%compp, max, &
                  pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompSetVMMinThreads
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMaxPEs"
!BOPI
! !IROUTINE: ESMF_CplCompSetVMMaxPEs - Define a VM for this CplComp
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMaxPEs(cplcomp, max, &
                       pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
    integer,             intent(in),  optional :: max
    integer,             intent(in),  optional :: pref_intra_process
    integer,             intent(in),  optional :: pref_intra_ssi
    integer,             intent(in),  optional :: pref_inter_ssi
    integer,             intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
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

    integer :: localrc                     ! local error localrc

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call CompClass method
    call ESMF_CompSetVMMaxPEs(cplcomp%compp, max, &
                   pref_intra_process, pref_intra_ssi, pref_inter_ssi, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompSetVMMaxPEs
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompValidate"
!BOP
! !IROUTINE: ESMF_CplCompValidate -- Ensure the CplComp is internally consistent
!
! !INTERFACE:
      subroutine ESMF_CplCompValidate(cplcomp, options, rc)
!
! !ARGUMENTS:
      type(ESMF_CplComp) :: cplcomp
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!   Currently all this method does is to check that the 
!   {\tt cplcomp} exists.
!
!  The arguments are:
!  \begin{description}
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to validate.
!   \item[{[options]}]
!    Validation options are not yet supported.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      call ESMF_CompValidate(cplcomp%compp, options, rc)
 
      ! Check Init Status
      ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

      ! If all checks pass return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_CplCompValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompWriteRestart"
!BOPI
! !IROUTINE: ESMF_CplCompWriteRestart -- Call the CplComp's checkpoint routine

! !INTERFACE:
    recursive subroutine ESMF_CplCompWriteRestart(cplcomp, iospec, clock, &
                                                  phase, blockingflag, rc)
!
! !ARGUMENTS:
      type (ESMF_CplComp), intent(inout) :: cplcomp
      type (ESMF_IOSpec), intent(inout), optional :: iospec
      type (ESMF_Clock), intent(inout), optional :: clock
      integer, intent(in), optional :: phase
      type (ESMF_BlockingFlag), intent(in), optional :: blockingflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Call the associated user checkpoint code for an {\tt ESMF\_CplComp}.
!    
!  The arguments are: 
!  \begin{description} 
!   \item[cplcomp]
!    {\tt ESMF\_CplComp} to call writerestart routine for.
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

        ! Initialize return code; assume routine not implemented
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

        ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
        ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

	! Changed BOP to BOPI until implemented.
        call ESMF_CompWriteRestart(cplcomp%compp, iospec, clock, phase, &
                                   blockingflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

        ! Return success
        if (present(rc)) rc = ESMF_SUCCESS
        end subroutine ESMF_CplCompWriteRestart



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompWait"
!BOP
! !IROUTINE: ESMF_CplCompWait - Wait for a CplComp to return
!
! !INTERFACE:
  subroutine ESMF_CplCompWait(cplcomp, blockingFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)               :: cplcomp
    type (ESMF_BlockingFlag), intent(in), optional  :: blockingFlag
    integer,            intent(out), optional       :: rc           
!
! !DESCRIPTION:
!     When executing asychronously, wait for an {\tt ESMF\_CplComp} to return.
!
!     The arguments are:
!     \begin{description}
!     \item[cplcomp] 
!      {\tt ESMF\_CplComp} to wait for.
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

    integer :: localrc                     ! local error localrc

    ! Initialize return code; assume failure until success is certain       
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call CompClass method
    call ESMF_CompWait(cplcomp%compp, blockingFlag, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                       ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_CplCompWait
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompIsPetLocal"
!BOP
! !IROUTINE: ESMF_CplCompIsPetLocal - Inquire if this component is to execute on the calling PET.
!
! !INTERFACE:
      recursive function ESMF_CplCompIsPetLocal(cplcomp, rc)
!
! !RETURN VALUE:
      logical :: ESMF_CplCompIsPetLocal
!
! !ARGUMENTS:
      type(ESMF_CplComp), intent(inout) :: cplcomp
      integer, intent(out), optional  :: rc 
!
! !DESCRIPTION:
!  Inquire if this {\tt ESMF\_CplComp} object is to execute on the calling PET.
!
!  The return value is {\tt .true.} if the component is to execute on the 
!  calling PET, {\tt .false.} otherwise.
!    
!  The arguments are:
!  \begin{description}
!   \item[cplcomp] 
!    {\tt ESMF\_CplComp} queried.
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
    ESMF_CplCompIsPetLocal = .false.

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call CompClass method
    localresult = ESMF_CompIsPetLocal(cplcomp%compp, localrc)
    ! if (ESMF_LogPassFoundError(localrc, rc)) return
    if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
    
    ESMF_CplCompIsPetLocal = localresult
    
  end function ESMF_CplCompIsPetLocal
    
!------------------------------------------------------------------------------



end module ESMF_CplCompMod

