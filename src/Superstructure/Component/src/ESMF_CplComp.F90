! $Id: ESMF_CplComp.F90,v 1.117.2.1 2010/02/05 20:04:11 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_CplComp.F90"
!==============================================================================
!
! ESMF Coupler Cplcomp module
module ESMF_CplCompMod
!
!==============================================================================
!
! This file contains the Coupler Cplcomp class definition and all 
!   Coupler Cplcomp class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
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
  use ESMF_IOSpecMod
  use ESMF_VMMod
  use ESMF_ConfigMod
  use ESMF_ClockTypeMod
  use ESMF_ClockMod
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

! - ESMF-public methods:
  public ESMF_CplCompCreate
  public ESMF_CplCompDestroy
  public ESMF_CplCompFinalize
  public ESMF_CplCompGet
  public ESMF_CplCompInitialize
  public ESMF_CplCompIsPetLocal
  public ESMF_CplCompPrint
  public ESMF_CplCompReadRestart
  public ESMF_CplCompRun
  public ESMF_CplCompSet
  public ESMF_CplCompSetEntryPoint
  public ESMF_CplCompSetServices
  public ESMF_CplCompSetVM
  public ESMF_CplCompSetVMMaxPEs
  public ESMF_CplCompSetVMMaxThreads
  public ESMF_CplCompSetVMMinThreads
  public ESMF_CplCompValidate
  public ESMF_CplCompWait
  public ESMF_CplCompWriteRestart

! - ESMF-internal methods:
  public ESMF_CplCompGetInit

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_CplComp.F90,v 1.117.2.1 2010/02/05 20:04:11 svasquez Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
  interface ESMF_CplCompSetServices
    module procedure ESMF_CplCompSetServices
    module procedure ESMF_CplCompSetServicesShObj
  end interface
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  interface ESMF_CplCompSetVM
    module procedure ESMF_CplCompSetVM
    module procedure ESMF_CplCompSetVMShObj
  end interface
!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreate"
!BOP
! !IROUTINE: ESMF_CplCompCreate - Create a Coupler Component
!
! !INTERFACE:
  recursive function ESMF_CplCompCreate(name, config, configFile, clock, &
    petList, contextflag, rc)
!
! !RETURN VALUE:
    type(ESMF_CplComp) :: ESMF_CplCompCreate
!
! !ARGUMENTS:
    character(len=*),       intent(in),     optional :: name
    type(ESMF_Config),      intent(inout),  optional :: config
    character(len=*),       intent(in),     optional :: configFile
    type(ESMF_Clock),       intent(inout),  optional :: clock
    integer,                intent(in),     optional :: petList(:)
    type(ESMF_ContextFlag), intent(in),     optional :: contextflag
    integer,                intent(out),    optional :: rc
!
! !DESCRIPTION:
! This interface creates an {\tt ESMF\_CplComp} object. By default, a
! separate VM context will be created for each component.  This implies
! creating a new MPI communicator and allocating additional memory to
! manage the VM resources. When running on a large number of processors,
! creating a separate VM for each component could be both time and memory
! inefficient.  If the application is sequential, i.e., each component is
! running on all the PETs of the global VM, it will be more efficient to use
! the global VM instead of creating a new one.  This can be done by setting
! {\tt contextflag} to ESMF\_CHILD\_IN\_PARENT\_VM.
!
! The return value is the new {\tt ESMF\_CplComp}.
!    
! The arguments are:
! \begin{description}
! \item[{[name]}]
!   Name of the newly-created {\tt ESMF\_CplComp}.  This name can be altered 
!   from within the {\tt ESMF\_CplComp} code once the initialization routine
!   is called.
! \item[{[config]}]
!   An already-created {\tt ESMF\_Config} configuration object 
!   from which the new component
!   can read in namelist-type information to set parameters for this run.
!   If both are specified, this object takes priority over {\tt configFile}.
! \item[{[configFile]}]
!   The filename of an {\tt ESMF\_Config} format file.  
!   If specified, this file is opened, an {\tt ESMF\_Config} configuration
!   object is created for the file, and attached to the new component.  
!   The user can call {\tt ESMF\_CplCompGet()} to get and use the object.
!   If both are specified, the {\tt config} object takes priority 
!   over this one.
! \item[{[clock]}]
!   Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!   queried and updated by the new {\tt ESMF\_CplComp} as it chooses.  
!   This should
!   not be the parent component clock, which should be maintained and passed
!   down to the initialize/run/finalize routines separately.
! \item[{[petList]}]
!   List of parent {\tt PET}s given to the created child component by the
!   parent component. If {\tt petList} is not specified all of the
!   parent {\tt PET}s will be given to the child component. The order of
!   PETs in {\tt petList} determines how the child local PETs refer back to
!   the parent PETs.
! \item[{[contextflag]}]
!   Specify the component's VM context. The default context is
!   {\tt ESMF\_CHILD\_IN\_NEW\_VM}. See section \ref{opt:contextflag} for a
!   complete list of valid flags.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    type(ESMF_CompClass), pointer :: compclass        ! generic comp
    type(ESMF_CplComp)            :: cplcomp
    integer :: localrc                                ! local error localrc

    ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    ! Initialize the pointer to null.
    nullify(ESMF_CplCompCreate%compp)
    nullify(compclass)

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Allocate a new comp class
    allocate(compclass, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Component class", &
      ESMF_CONTEXT, rc)) return
   
    ! call Comp method
    call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
      configFile=configFile, config=config, clock=clock, petList=petList, &
      contextflag=contextflag, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) then
      deallocate(compclass)
      return
    endif

    cplcomp%compp => compclass
    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(cplcomp, ESMF_ID_COMPONENT%objectID)
      
    ! Set return values
    ESMF_CplCompCreate%compp => compclass
    
    ESMF_INIT_SET_CREATED(ESMF_CplCompCreate)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_CplCompCreate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompDestroy"
!BOP
! !IROUTINE: ESMF_CplCompDestroy - Release resources for a CplComp

! !INTERFACE:
  subroutine ESMF_CplCompDestroy(cplcomp, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)             :: cplcomp
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Releases all resources associated with this {\tt ESMF\_CplComp}.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Release all resources associated with this {\tt ESMF\_CplComp}
!   and mark the object as invalid.  It is an error to pass this
!   object into any other routines after being destroyed.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error localrc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! Check to see if already destroyed
    if (.not.associated(cplcomp%compp)) then  
      if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
        "CplComp not initialized or already destroyed", &
        ESMF_CONTEXT, rc)) return
    endif

    ! call Comp method
    call ESMF_CompDestruct(cplcomp%compp, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! mark object invalid
    call ESMF_BaseSetStatus(cplcomp%compp%base, ESMF_STATUS_INVALID, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ESMF_INIT_SET_DELETED(cplcomp)
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CplCompDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompFinalize"
!BOP
! !IROUTINE: ESMF_CplCompFinalize - Call the CplComp's finalize routine
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompFinalize(cplcomp, importState, exportState, &
    clock, phase, blockingflag, userRc, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CplComp)                               :: cplcomp
    type(ESMF_State),        intent(inout), optional :: importState
    type(ESMF_State),        intent(inout), optional :: exportState
    type(ESMF_Clock),        intent(inout), optional :: clock
    integer,                 intent(in),    optional :: phase
    type(ESMF_BlockingFlag), intent(in),    optional :: blockingflag
    integer,                 intent(out),   optional :: userRc
    integer,                 intent(out),   optional :: rc
!
! !DESCRIPTION:
! Call the associated user-supplied finalization routine for 
! an {\tt ESMF\_CplComp}.
!   
! The arguments are: 
! \begin{description} 
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} to call finalize routine for.
! \item[{[importState]}]  
!   {\tt ESMF\_State} containing import data for coupling. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   importState argument in the user code cannot be optional. 
! \item[{[exportState]}]  
!   {\tt ESMF\_State} containing export data for coupling. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   exportState argument in the user code cannot be optional. 
! \item[{[clock]}]  
!   External {\tt ESMF\_Clock} for passing in time information.  
!   This is generally the parent component's clock, and will be treated
!   as read-only by the child component.  The child component can maintain
!   a private clock for its own internal time computations. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   clock argument in the user code cannot be optional. 
! \item[{[phase]}]  
!   Component providers must document whether their each of their
!   routines are {\em single-phase} or {\em multi-phase}.  
!   Single-phase routines require only one invocation to complete
!   their work.  
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accomodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase 
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[blockingflag]}]
!   Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    call ESMF_CompExecute(cplcomp%compp, method=ESMF_SETFINAL, &
      importState=importState, exportState=exportState, clock=clock, &
      phase=phase, blockingflag=blockingflag, userRc=userRc, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompFinalize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompGet"
!BOP
! !IROUTINE: ESMF_CplCompGet - Query a CplComp for information
!
! !INTERFACE:
  subroutine ESMF_CplCompGet(cplcomp, name, config, configFile, clock, vm, &
    contextflag, currentMethod, currentPhase, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),     intent(inout)         :: cplcomp
    character(len=*),       intent(out), optional :: name
    type(ESMF_Config),      intent(out), optional :: config
    character(len=*),       intent(out), optional :: configFile
    type(ESMF_Clock),       intent(out), optional :: clock
    type(ESMF_VM),          intent(out), optional :: vm
    type(ESMF_ContextFlag), intent(out), optional :: contextflag
    type(ESMF_Method),      intent(out), optional :: currentMethod
    integer,                intent(out), optional :: currentPhase
    integer,                intent(out), optional :: rc

!
! !DESCRIPTION:
! Returns information about an {\tt ESMF\_CplComp}.
! For queries where the caller
! only wants a single value, specify the argument by name.
! All the arguments after {\tt cplcomp} argument are optional 
! to facilitate this.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to query.
! \item[{[name]}]
!   Return the name of the {\tt ESMF\_CplComp}.
! \item[{[config]}]
!   Return the {\tt ESMF\_Config} object for this {\tt ESMF\_CplComp}.
! \item[{[configFile]}]
!   Return the configuration filename for this {\tt ESMF\_CplComp}.
! \item[{[clock]}]
!   Return the private clock for this {\tt ESMF\_CplComp}.
! \item[{[vm]}]
!   Return the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
! \item[{[contextflag]}]
!   Return the {\tt ESMF\_ContextFlag} for this {\tt ESMF\_CplComp}.
!   See section \ref{opt:contextflag} for a complete list of valid flags.
! \item[{[currentMethod]}]
!   Return the current {\tt ESMF\_Method} of the {\tt ESMF\_CplComp} execution.
!   See section \ref{opt:method}  for a complete list of valid options.
! \item[{[currentPhase]}]
!   Return the current {\tt phase} of the {\tt ESMF\_CplComp} execution.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                  ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompGet(cplcomp%compp, name, vm=vm, contextflag=contextflag, &
      clock=clock, configFile=configFile, config=config, &
      currentMethod=currentMethod, currentPhase=currentPhase, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompGetInternalState - Get private data block pointer
!
! !INTERFACE:
! subroutine ESMF_CplCompGetInternalState(cplcomp, dataPointer, rc)
!
! !ARGUMENTS:
!   type(ESMF_CplComp), intent(inout) :: cplcomp
!   type(any), pointer                :: dataPointer
!   integer,            intent(out)   :: rc
!
! !DESCRIPTION:
! Available to be called by an {\tt ESMF\_CplComp} at any time after 
! {\tt ESMF\_CplCompSetInternalState} has been called.
! Since init, run, and finalize must be separate subroutines, data that
! they need to share in common can either be module global data, or can
! be allocated in a private data block and the address of that block
! can be registered with the framework and retrieved by this call.
! When running multiple instantiations of an {\tt ESMF\_CplComp}, 
! for example during ensemble runs, 
! it may be simpler to maintain private data specific to 
! each run with private data blocks.  A corresponding 
! {\tt ESMF\_CplCompSetInternalState} call sets the data pointer to 
! this block, and this call retrieves the data pointer.   
! Note that the {\tt dataPointer} argument needs to be a derived type
! which contains only a pointer of the type of the data block defined
! by the user.  When making this call the pointer needs to be unassociated.
! When the call returns, the pointer will now reference the original
! data block which was set during the previous call to
! {\tt ESMF\_CplCompSetInternalState}.
!   
! Only the {\em last} data block set via
! {\tt ESMF\_CplCompSetInternalState} will be accessible.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   An {\tt ESMF\_CplComp} object.
! \item[dataPointer]
!   A derived type, containing only an unassociated pointer 
!   to the private data block.
!   The framework will fill in the pointer. When this call returns, the
!   pointer is set to the same address set during the last
!   {\tt ESMF\_CplCompSetInternalState} call.
!   This level of indirection is needed to reliably set and retrieve 
!   the data block no matter which architecture or compiler is used.  
! \item[rc] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   Note: unlike most other ESMF routines, this argument is not optional
!   because of implementation considerations.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompInitialize"
!BOP
! !IROUTINE: ESMF_CplCompInitialize - Call the CplComp's initialize routine
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompInitialize(cplcomp, importState, &
    exportState, clock, phase, blockingflag, userRc, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CplComp)                               :: cplcomp
    type(ESMF_State),        intent(inout), optional :: importState
    type(ESMF_State),        intent(inout), optional :: exportState
    type(ESMF_Clock),        intent(inout), optional :: clock
    integer,                 intent(in),    optional :: phase
    type(ESMF_BlockingFlag), intent(in),    optional :: blockingflag
    integer,                 intent(out),   optional :: userRc
    integer,                 intent(out),   optional :: rc
!
! !DESCRIPTION:
! Call the associated user initialization code for a CplComp.
!    
! The arguments are: 
! \begin{description} 
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to call initialize routine for.
! \item[{[importState]}]  
!   {\tt ESMF\_State} containing import data for coupling. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   importState argument in the user code cannot be optional. 
! \item[{[exportState]}]  
!   {\tt ESMF\_State} containing export data for coupling. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   exportState argument in the user code cannot be optional. 
! \item[{[clock]}]  
!   External {\tt ESMF\_Clock} for passing in time information.  
!   This is generally the parent component's clock, and will be treated
!   as read-only by the child component.  The child component can maintain
!   a private clock for its own internal time computations. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   clock argument in the user code cannot be optional. 
! \item[{[phase]}] 
!   Component providers must document whether their each of their
!   routines are {\em single-phase} or {\em multi-phase}.  
!   Single-phase routines require only one invocation to complete
!   their work.  
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accomodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase 
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[blockingflag]}]
!   Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    call ESMF_CompExecute(cplcomp%compp, method=ESMF_SETINIT, &
      importState=importState, exportState=exportState, clock=clock, &
      phase=phase, blockingflag=blockingflag, userRc=userRc, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompInitialize
!------------------------------------------------------------------------------


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
    type(ESMF_CplComp), intent(inout)         :: cplcomp
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
! Inquire if this {\tt ESMF\_CplComp} object is to execute on the calling PET.
!
! The return value is {\tt .true.} if the component is to execute on the 
! calling PET, {\tt .false.} otherwise.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp] 
!   {\tt ESMF\_CplComp} queried.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                     ! local error status
    logical :: localresult

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
        localrc = ESMF_RC_NOT_IMPL

    ! Initialize output value in case of error
    ESMF_CplCompIsPetLocal = .false.

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    localresult = ESMF_CompIsPetLocal(cplcomp%compp, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ESMF_CplCompIsPetLocal = localresult

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_CplCompIsPetLocal
!------------------------------------------------------------------------------


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
    type(ESMF_CplComp)                        :: cplcomp
    character(len = *), intent(in),  optional :: options
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
! Prints information about an {\tt ESMF\_CplComp} to {\tt stdout}. \\
!
! Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
! On some platforms/compilers there is a potential issue with interleaving
! Fortran and C++ output to {\tt stdout} such that it doesn't appear in
! the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
! may be used on unit 6 to get coherent output.  \\
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to print.
! \item[{[options]}]
!   Print options are not yet supported.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc              ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    print *, "Coupler Component:"
    ! call Comp method
    call ESMF_CompPrint(cplcomp%compp, options, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompPrint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompReadRestart"
!BOP
! !IROUTINE: ESMF_CplCompReadRestart -- Call the CplComp's read restart routine
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompReadRestart(cplcomp, importState, &
    exportState, clock, phase, blockingflag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                               :: cplcomp
    type(ESMF_State),        intent(inout), optional :: importState
    type(ESMF_State),        intent(inout), optional :: exportState
    type(ESMF_Clock),        intent(inout), optional :: clock
    integer,                 intent(in),    optional :: phase
    type(ESMF_BlockingFlag), intent(in),    optional :: blockingflag
    integer,                 intent(out),   optional :: userRc
    integer,                 intent(out),   optional :: rc
!
! !DESCRIPTION:
! Call the associated user read restart code for an {\tt ESMF\_CplComp}.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to call run routine for.
! \item[{[importState]}]  
!   {\tt ESMF\_State} containing import data. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   importState argument in the user code cannot be optional. 
! \item[{[exportState]}]  
!   {\tt ESMF\_State} containing export data. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   exportState argument in the user code cannot be optional. 
! \item[{[clock]}]  
!   External {\tt ESMF\_Clock} for passing in time information.  
!   This is generally the parent component's clock, and will be treated
!   as read-only by the child component.  The child component can maintain
!   a private clock for its own internal time computations. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   clock argument in the user code cannot be optional. 
! \item[{[phase]}]   
!   Component providers must document whether their each of their
!   routines are {\em single-phase} or {\em multi-phase}.    
!   Single-phase routines require only one invocation to complete
!   their work.    
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accomodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase  
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[blockingflag]}]  
!   Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                  ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    call ESMF_CompExecute(cplcomp%compp, method=ESMF_SETREADRESTART, &
      importState=importState, exportState=exportState, clock=clock, &
      phase=phase, blockingflag=blockingflag, userRc=userRc, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompReadRestart
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompRun"
!BOP
! !IROUTINE: ESMF_CplCompRun - Call the CplComp's run routine
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompRun(cplcomp, importState, exportState, &
    clock, phase, blockingflag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                               :: cplcomp
    type(ESMF_State),        intent(inout), optional :: importState
    type(ESMF_State),        intent(inout), optional :: exportState
    type(ESMF_Clock),        intent(inout), optional :: clock
    integer,                 intent(in),    optional :: phase
    type(ESMF_BlockingFlag), intent(in),    optional :: blockingflag
    integer,                 intent(out),   optional :: userRc
    integer,                 intent(out),   optional :: rc
!
! !DESCRIPTION:
! Call the associated user run code for an {\tt ESMF\_CplComp}.
!    
! The arguments are: 
! \begin{description} 
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to call run routine for.
! \item[{[importState]}]  
!   {\tt ESMF\_State} containing import data for coupling. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   importState argument in the user code cannot be optional. 
! \item[{[exportState]}]  
!   {\tt ESMF\_State} containing export data for coupling. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   exportState argument in the user code cannot be optional. 
! \item[{[clock]}]  
!   External {\tt ESMF\_Clock} for passing in time information.  
!   This is generally the parent component's clock, and will be treated
!   as read-only by the child component.  The child component can maintain
!   a private clock for its own internal time computations. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   clock argument in the user code cannot be optional. 
! \item[{[phase]}]  
!   Component providers must document whether their each of their
!   routines are {\em single-phase} or {\em multi-phase}.  
!   Single-phase routines require only one invocation to complete
!   their work.  
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accomodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase 
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[blockingflag]}]
!   Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                     ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,importState,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,exportState,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    call ESMF_CompExecute(cplcomp%compp, method=ESMF_SETRUN, &
      importState=importState, exportState=exportState, clock=clock, &
      phase=phase, blockingflag=blockingflag, userRc=userRc, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompRun
!------------------------------------------------------------------------------


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
    type(ESMF_CplComp), intent(inout)           :: cplcomp
    character(len=*),   intent(in),    optional :: name
    type(ESMF_Config),  intent(inout), optional :: config
    character(len=*),   intent(in),    optional :: configFile
    type(ESMF_Clock),   intent(inout), optional :: clock
    integer,            intent(out),   optional :: rc

!
! !DESCRIPTION:
! Sets or resets information about an {\tt ESMF\_CplComp}.
! The caller can set individual values by specifying
! the arguments by name.
! All the arguments except {\tt cplcomp} are optional 
! to facilitate this.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to change.
! \item[{[name]}]
!   Set the name of the {\tt ESMF\_CplComp}.
! \item[{[config]}]
!   Set the configuration information for the {\tt ESMF\_CplComp} from
!   this already created {\tt ESMF\_Config} object.   
!   If specified, takes priority over {\tt configFile}.
! \item[{[configFile]}]
!   Set the configuration filename for this {\tt ESMF\_CplComp}.
!   An {\tt ESMF\_Config} object will be created for this file
!   and attached to the {\tt ESMF\_CplComp}.  Superceeded by {\tt config}
!   if both are specified.
! \item[{[clock]}]
!   Set the private clock for this {\tt ESMF\_CplComp}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                    ! local return code

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    ! call Comp method
    call ESMF_CompSet(cplcomp%compp, name, clock=clock, configFile=configFile, &
      config=config, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetEntryPoint"
!BOP
! !IROUTINE: ESMF_CplCompSetEntryPoint - Set user routine as entry point for standard Component method
!
! !INTERFACE:
  subroutine ESMF_CplCompSetEntryPoint(cplcomp, method, userRoutine, phase, rc)

! !ARGUMENTS:
    type(ESMF_CplComp), intent (in) :: cplcomp
    type(ESMF_Method),  intent(in)  :: method
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
    integer, intent(in),  optional  :: phase
    integer, intent(out), optional  :: rc 
!
! !DESCRIPTION:
! Registers a user-supplied {\tt userRoutine} as the entry point for one of the
! predefined Component {\tt method}s. After this call the {\tt userRoutine}
! becomes accessible via the standard Component method API.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   An {\tt ESMF\_CplComp} object.
! \item[method]
!   One of a set of predefined Component methods - e.g. {\tt ESMF\_SETINIT}, 
!   {\tt ESMF\_SETRUN}, {\tt ESMF\_SETFINAL}. See section \ref{opt:method} 
!   for a complete list of valid method options.
! \item[userRoutine]
!   The user-supplied subroutine to be associated for this {\tt method}.
!   This subroutine does not have to be public.
! \item[{[phase]}] 
!   The {\tt phase} number for multi-phase methods. For single phase 
!   methods the {\tt phase} argument can be omitted. The default setting
!   is 1.
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown above for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
! must not be declared as optional, and the types, intent and order must match.
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: phaseArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
  
    phaseArg = 1   ! default
    if (present(phase)) phaseArg = phase
  
    call c_ESMC_SetEntryPoint(cplcomp, method, userRoutine, phaseArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSetInternalState - Set private data block pointer
!
! !INTERFACE:
! subroutine ESMF_CplCompSetInternalState(cplcomp, dataPointer, rc)
!
! !ARGUMENTS:
!   type(ESMF_CplComp), intent(inout) :: cplcomp
!   type(any), pointer                :: dataPointer
!   integer,            intent(out)   :: rc
!
! !DESCRIPTION:
! Available to be called by an {\tt ESMF\_CplComp} at any time, but 
! expected to be
! most useful when called during the registration process, or initialization.
! Since init, run, and finalize must be separate subroutines data that
! they need to share in common can either be module global data, or can
! be allocated in a private data block and the address of that block
! can be registered with the framework and retrieved by subsequent calls.
! When running multiple instantiations of an {\tt ESMF\_CplComp}, 
! for example during
! ensemble runs, it may be simpler to maintain private data specific to 
! each run with private data blocks.  A corresponding 
! {\tt ESMF\_CplCompGetInternalState} call retrieves the data pointer.
!   
! Only the {\em last} data block set via
! {\tt ESMF\_CplCompSetInternalState} will be accessible.
!
! The arguments are:
! \begin{description}
! \item[cplcomp] 
!   An {\tt ESMF\_CplComp} object.
! \item[dataPointer]
!   A pointer to the private data block, wrapped in a derived type which
!   contains only a pointer to the block.  This level of indirection is
!   needed to reliably set and retrieve the data block no matter which
!   architecture or compiler is used.  
! \item[rc] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   Note: unlike most other ESMF routines, this argument is not optional
!   because of implementation considerations.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetServices"
!BOP
! !IROUTINE: ESMF_CplCompSetServices - Call user routine to register CplComp methods
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompSetServices(cplcomp, userRoutine, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)             :: cplcomp
    interface
      subroutine userRoutine(cplcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer, intent(out), optional :: userRc
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Call into user provided {\tt userRoutine} which is responsible for
! for setting Component's Initialize(), Run() and Finalize() services.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[userRoutine]
!   Routine to be called.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown above for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
! must not be declared as optional, and the types, intent and order must match.
!
! The {\tt userRoutine}, when called by the framework, must make successive calls to
! {\tt ESMF\_CplCompSetEntryPoint()} to preset callback routines for standard
! Component Initialize(), Run() and Finalize() methods.
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
  
    ! set the current method to keep track inside Component for query
    cplcomp%compp%currentMethod = ESMF_SETSERVICES

    call c_ESMC_SetServices(cplcomp, userRoutine, localUserRc, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! reset the current method to keep track inside Component for query
    cplcomp%compp%currentMethod = ESMF_SETNONE

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetServicesShObj"
!BOP
! !IROUTINE: ESMF_CplCompSetServices - Call user routine, located in shared object, to register CplComp methods
!
! !INTERFACE:
  ! Private name; call using ESMF_CplCompSetServices()
  recursive subroutine ESMF_CplCompSetServicesShObj(cplcomp, userRoutine, &
    sharedObj, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
    character(len=*),    intent(in)            :: userRoutine
    character(len=*),    intent(in),  optional :: sharedObj
    integer,             intent(out), optional :: userRc
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Call into user provided routine which is responsible for setting
! Component's Initialize(), Run() and Finalize() services. The named
! {\tt userRoutine} must exist in the shared object file specified in the
! {\tt sharedObj} argument. All of the platform specific details about 
! dynamic linking and loading apply.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[userRoutine]
!   Name of routine to be called.
! \item[{[sharedObj]}]
!   Name of shared object that contains {\tt userRoutine}. If the
!   {\tt sharedObj} argument is not provided the executable itself will be
!   searched for {\tt userRoutine}.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown for {\tt userRoutine} below. Arguments must not be declared
! as optional, and the types, intent and order must match.
!
! !INTERFACE:
!   interface
!     subroutine userRoutine(cplcomp, rc)
!       type(ESMF_CplComp)   :: cplcomp    ! must not be optional
!       integer, intent(out) :: rc         ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The {\tt userRoutine}, when called by the framework, must make successive
! calls to {\tt ESMF\_CplCompSetEntryPoint()} to preset callback routines for
! standard Component Initialize(), Run() and Finalize() methods.
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc
    character(len=0) :: emptyString

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
  
    ! set the current method to keep track inside Component for query
    cplcomp%compp%currentMethod = ESMF_SETSERVICES

    if (present(sharedObj)) then
      call c_ESMC_SetServicesShObj(cplcomp, userRoutine, sharedObj, &
        localUserRc, localrc)
    else
      call c_ESMC_SetServicesShObj(cplcomp, userRoutine, emptyString, &
        localUserRc, localrc)
    endif
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! reset the current method to keep track inside Component for query
    cplcomp%compp%currentMethod = ESMF_SETNONE

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVM"
!BOP
! !IROUTINE: ESMF_CplCompSetVM - Call user routine to set CplComp VM properies
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompSetVM(cplcomp, userRoutine, userRc, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)             :: cplcomp
    interface
      subroutine userRoutine(cplcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer, intent(out), optional :: userRc
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Optionally call into user provided {\tt userRoutine} which is responsible for
! for setting Component's VM properties. 
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[userRoutine]
!   Routine to be called.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown above for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
! must not be declared as optional, and the types, intent and order must match.
!
! The subroutine, when called by the framework, is expected to use any of the
! {\tt ESMF\_CplCompSetVMxxx()} methods to set the properties of the VM
! associated with the Coupler Component.
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
  
    ! set the current method to keep track inside Component for query
    cplcomp%compp%currentMethod = ESMF_SETVM

    call c_ESMC_SetVM(cplcomp, userRoutine, localUserRc, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! reset the current method to keep track inside Component for query
    cplcomp%compp%currentMethod = ESMF_SETNONE

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMShObj"
!BOP
! !IROUTINE: ESMF_CplCompSetVM - Set CplComp VM properties in routine located in shared object
! !INTERFACE:
  ! Private name; call using ESMF_CplCompSetVM()
  recursive subroutine ESMF_CplCompSetVMShObj(cplcomp, userRoutine, sharedObj, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
    character(len=*),    intent(in)            :: userRoutine
    character(len=*),    intent(in),  optional :: sharedObj
    integer,             intent(out), optional :: userRc
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Optionally call into user provided {\tt userRoutine} which is responsible for
! for setting Component's VM properties. The named {\tt userRoutine} must exist
! in the shared object file specified in the {\tt sharedObj} argument. All of
! the platform specific details about dynamic linking and loading apply.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[userRoutine]
!   Routine to be called.
! \item[{[sharedObj]}]
!   Name of shared object that contains {\tt userRoutine}. If the 
!   {\tt sharedObj} argument is not provided the executable itself will be
!   searched for {\tt userRoutine}.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! The Component writer must supply a subroutine with the exact interface 
! shown for {\tt userRoutine} below. Arguments must not be declared
! as optional, and the types, intent and order must match.
!
! !INTERFACE:
!   interface
!     subroutine userRoutine(cplcomp, rc)
!       type(ESMF_CplComp)   :: cplcomp     ! must not be optional
!       integer, intent(out) :: rc          ! must not be optional
!     end subroutine
!   end interface
!
! !DESCRIPTION:
! The subroutine, when called by the framework, is expected to use any of the
! {\tt ESMF\_CplCompSetVMxxx()} methods to set the properties of the VM
! associated with the Coupler Component.
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc
    character(len=0) :: emptyString

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
  
    ! set the current method to keep track inside Component for query
    cplcomp%compp%currentMethod = ESMF_SETVM

    if (present(sharedObj)) then
      call c_ESMC_SetVMShObj(cplcomp, userRoutine, sharedObj, localUserRc, &
        localrc)
    else
      call c_ESMC_SetVMShObj(cplcomp, userRoutine, emptyString, localUserRc, &
        localrc)
    endif
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! reset the current method to keep track inside Component for query
    cplcomp%compp%currentMethod = ESMF_SETNONE

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMaxPEs"
!BOP
! !IROUTINE: ESMF_CplCompSetVMMaxPEs - Set VM for Coupler Component to associate max PEs with PETs.
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMaxPEs(cplcomp, max, pref_intra_process, &
    pref_intra_ssi, pref_inter_ssi, rc)
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
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!   Attempts to associate {\tt max} PEs with each PET. Only PEs that are 
!   located on the same single system image can be associated with the same PET.
!   Within this constraint the call tries to get as close as possible to the
!   number specified by {\tt max}.
!
!   The typical use of {\tt ESMF\_CplCompSetVMMaxPEs()} is to allocate
!   multiple PEs per PET in a Component for user-level threading, e.g. OpenMP.
!
! The arguments are:
! \begin{description}
! \item[cplcomp] 
!   {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
! \item[{[max]}] 
!   Maximum number of PEs per PET. Default is peCount.
! \item[{[pref\_intra\_process]}] 
!   Intra process communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[pref\_intra\_ssi]}] 
!   Intra SSI communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[pref\_inter\_ssi]}] 
!   Inter process communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                     ! local error localrc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompSetVMMaxPEs(cplcomp%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetVMMaxPEs
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMaxThreads"
!BOP
! !IROUTINE: ESMF_CplCompSetVMMaxThreads - Set VM for Gridded Component with multi-threaded PETs.
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMaxThreads(cplcomp, max, pref_intra_process, &
    pref_intra_ssi, pref_inter_ssi, rc)
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
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!   Attempts to provide {\tt max} threaded PETs in each VAS. Only as many
!   threaded PETs as there are PEs located on the same single system image
!   can be associated with the same VAS. Within this constraint the call
!   tries to get as close as possible to the number specified by {\tt max}.
!
!   The typical use of {\tt ESMF\_CplCompSetVMMaxThreads()} is to run a 
!   Component multi-threaded with a groups of PETs that execute within the
!   same virtual address space.
!
! The arguments are:
! \begin{description}
! \item[cplcomp] 
!   {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
! \item[{[max]}] 
!   Maximum threading level.
! \item[{[pref\_intra\_process]}] 
!   Intra process communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[pref\_intra\_ssi]}] 
!   Intra SSI communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[pref\_inter\_ssi]}] 
!   Inter process communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                     ! local error localrc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompSetVMMaxThreads(cplcomp%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetVMMaxThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMinThreads"
!BOP
! !IROUTINE: ESMF_CplCompSetVMMinThreads - Set VM for Coupler Component with reduced threading level.
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMinThreads(cplcomp, max, pref_intra_process, &
    pref_intra_ssi, pref_inter_ssi, rc)
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
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!   Reduces the number of threaded PETs in each VAS. The {\tt max} argument
!   may be specified to limit the maximum number of PEs that a single PET 
!   may be associated with.
!
!   The typical use of {\tt ESMF\_CplCompSetVMMinThreads()} is to run a 
!   Component across a set of single-threaded PETs.
!
! The arguments are:
! \begin{description}
! \item[cplcomp] 
!   {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
! \item[{[max]}] 
!   Maximum number of PEs per PET. Default is peCount.
! \item[{[pref\_intra\_process]}] 
!   Intra process communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[pref\_intra\_ssi]}] 
!   Intra SSI communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[pref\_inter\_ssi]}] 
!   Inter process communication preference.
!   {\em Currently options not documented. Use default.}
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                     ! local error localrc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompSetVMMinThreads(cplcomp%compp, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetVMMinThreads
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
    type(ESMF_CplComp)                        :: cplcomp
    character(len = *), intent(in),  optional :: options
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
! Currently all this method does is to check that the {\tt cplcomp} exists.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to validate.
! \item[{[options]}]
!   Validation options are not yet supported.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompValidate(cplcomp%compp, options, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompValidate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompWait"
!BOP
! !IROUTINE: ESMF_CplCompWait - Wait for a CplComp to return
!
! !INTERFACE:
  subroutine ESMF_CplCompWait(cplcomp, blockingflag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),      intent(inout)         :: cplcomp
    type(ESMF_BlockingFlag), intent(in),  optional :: blockingflag
    integer,                 intent(out), optional :: userRc
    integer,                 intent(out), optional :: rc
!
! !DESCRIPTION:
! When executing asychronously, wait for an {\tt ESMF\_CplComp} to return.
!
! The arguments are:
! \begin{description}
! \item[cplcomp] 
!   {\tt ESMF\_CplComp} to wait for.
! \item[{[blockingflag]}]
!   Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}] 
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                     ! local error localrc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompWait(cplcomp%compp, blockingflag=blockingflag, &
      userRc=userRc, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompWait
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompWriteRestart"
!BOP
! !IROUTINE: ESMF_CplCompWriteRestart -- Call the CplComp's write restart routine

! !INTERFACE:
  recursive subroutine ESMF_CplCompWriteRestart(cplcomp, importState, &
    exportState, clock, phase, blockingflag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),      intent(inout)           :: cplcomp
    type(ESMF_State),        intent(inout), optional :: importState
    type(ESMF_State),        intent(inout), optional :: exportState
    type(ESMF_Clock),        intent(inout), optional :: clock
    integer,                 intent(in),    optional :: phase
    type(ESMF_BlockingFlag), intent(in),    optional :: blockingflag
    integer,                 intent(out),   optional :: userRc
    integer,                 intent(out),   optional :: rc
!
! !DESCRIPTION:
! Call the associated user write restart code for an {\tt ESMF\_CplComp}.
!    
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to call run routine for.
! \item[{[importState]}]  
!   {\tt ESMF\_State} containing import data. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   importState argument in the user code cannot be optional. 
! \item[{[exportState]}]  
!   {\tt ESMF\_State} containing export data. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   exportState argument in the user code cannot be optional. 
! \item[{[clock]}]  
!   External {\tt ESMF\_Clock} for passing in time information.  
!   This is generally the parent component's clock, and will be treated
!   as read-only by the child component.  The child component can maintain
!   a private clock for its own internal time computations. If not present, a dummy
!   argument will be passed to the user-supplied routine.  The 
!   clock argument in the user code cannot be optional. 
! \item[{[phase]}]   
!   Component providers must document whether their each of their
!   routines are {\em single-phase} or {\em multi-phase}.    
!   Single-phase routines require only one invocation to complete
!   their work.    
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accomodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase  
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[blockingflag]}]  
!   Blocking behavior of this method call. See section \ref{opt:blockingflag} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    call ESMF_CompExecute(cplcomp%compp, method=ESMF_SETWRITERESTART, &
      importState=importState, exportState=exportState, clock=clock, &
      phase=phase, blockingflag=blockingflag, userRc=userRc, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompWriteRestart
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompGetInit"
!BOPI
! !IROUTINE:  ESMF_CplCompGetInit - Get initialization status.

! !INTERFACE:
  function ESMF_CplCompGetInit(d)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_CplCompGetInit
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in), optional :: d
!
! !DESCRIPTION:
! Get the initialization status of the Deep class {\tt CplComp}.
!
! The arguments are:
! \begin{description}
! \item[d]
!   {\tt ESMF\_CplComp} from which to retreive status.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(d)) then
      ESMF_CplCompGetInit = ESMF_INIT_GET(d)
    else
      ESMF_CplCompGetInit = ESMF_INIT_CREATED
    endif
  end function ESMF_CplCompGetInit
!------------------------------------------------------------------------------

end module ESMF_CplCompMod
