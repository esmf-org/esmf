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
  use ESMF_BaseMod
  use ESMF_VMMod
  use ESMF_ConfigMod
  use ESMF_ClockTypeMod
  use ESMF_ClockMod
  use ESMF_StateTypesMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_InitMacrosMod
  use ESMF_IOUtilMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public operator(==)
  public operator(/=)

  public ESMF_CplCompCreate
  public ESMF_CplCompDestroy
  public ESMF_CplCompFinalize
  public ESMF_CplCompFinalizeAct
  public ESMF_CplCompGet
  public ESMF_CplCompGetEPPhaseCount
  public ESMF_CplCompInitialize
  public ESMF_CplCompInitializeAct
  public ESMF_CplCompIsCreated
  public ESMF_CplCompIsPetLocal
  public ESMF_CplCompPrint
  public ESMF_CplCompReadRestart
  public ESMF_CplCompRun
  public ESMF_CplCompRunAct
  public ESMF_CplCompServiceLoop
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
    '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
  interface ESMF_CplCompSetServices
    module procedure ESMF_CplCompSetServices
    module procedure ESMF_CplCompSetServicesShObj
    module procedure ESMF_CplCompSetServicesComp
    module procedure ESMF_CplCompSetServicesSock
  end interface
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  interface ESMF_CplCompSetVM
    module procedure ESMF_CplCompSetVM
    module procedure ESMF_CplCompSetVMShObj
  end interface
!------------------------------------------------------------------------------

!===============================================================================
! CplCompOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_CplCompAssignment(=) - CplComp assignment
!
! !INTERFACE:
!   interface assignment(=)
!   cplcomp1 = cplcomp2
!
! !ARGUMENTS:
!   type(ESMF_CplComp) :: cplcomp1
!   type(ESMF_CplComp) :: cplcomp2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Assign cplcomp1 as an alias to the same ESMF CplComp object in memory
!   as cplcomp2. If cplcomp2 is invalid, then cplcomp1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[cplcomp1]
!     The {\tt ESMF\_CplComp} object on the left hand side of the assignment.
!   \item[cplcomp2]
!     The {\tt ESMF\_CplComp} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_CplCompOperator(==) - CplComp equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (cplcomp1 == cplcomp2) then ... endif
!             OR
!   result = (cplcomp1 == cplcomp2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_CplComp), intent(in) :: cplcomp1
!   type(ESMF_CplComp), intent(in) :: cplcomp2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether cplcomp1 and cplcomp2 are valid aliases to the same ESMF
!   CplComp object in memory. For a more general comparison of two ESMF CplComps,
!   going beyond the simple alias test, the ESMF\_CplCompMatch() function (not yet
!   implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[cplcomp1]
!     The {\tt ESMF\_CplComp} object on the left hand side of the equality
!     operation.
!   \item[cplcomp2]
!     The {\tt ESMF\_CplComp} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_CplCompEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_CplCompOperator(/=) - CplComp not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (cplcomp1 /= cplcomp2) then ... endif
!             OR
!   result = (cplcomp1 /= cplcomp2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_CplComp), intent(in) :: cplcomp1
!   type(ESMF_CplComp), intent(in) :: cplcomp2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether cplcomp1 and cplcomp2 are {\it not} valid aliases to the
!   same ESMF CplComp object in memory. For a more general comparison of two ESMF
!   CplComps, going beyond the simple alias test, the ESMF\_CplCompMatch() function
!   (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[cplcomp1]
!     The {\tt ESMF\_CplComp} object on the left hand side of the non-equality
!     operation.
!   \item[cplcomp2]
!     The {\tt ESMF\_CplComp} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_CplCompNE

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


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompEQ()"
!BOPI
! !IROUTINE:  ESMF_CplCompEQ - Compare two CplComps for equality
!
! !INTERFACE:
  function ESMF_CplCompEQ(cplcomp1, cplcomp2)
!
! !RETURN VALUE:
    logical :: ESMF_CplCompEQ

! !ARGUMENTS:
    type(ESMF_CplComp), intent(in) :: cplcomp1
    type(ESMF_CplComp), intent(in) :: cplcomp2

! !DESCRIPTION:
!   Test if both {\tt cplcomp1} and {\tt cplcomp2} alias the same ESMF CplComp
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE ccinit1, ccinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    ccinit1 = ESMF_CplCompGetInit(cplcomp1)
    ccinit2 = ESMF_CplCompGetInit(cplcomp2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (ccinit1 .eq. ESMF_INIT_CREATED .and. &
      ccinit2 .eq. ESMF_INIT_CREATED) then
      ESMF_CplCompEQ = associated(cplcomp1%compp,cplcomp2%compp)
    else
      ESMF_CplCompEQ = ESMF_FALSE
    endif

  end function ESMF_CplCompEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompNE()"
!BOPI
! !IROUTINE:  ESMF_CplCompNE - Compare two CplComps for non-equality
!
! !INTERFACE:
  function ESMF_CplCompNE(cplcomp1, cplcomp2)
!
! !RETURN VALUE:
    logical :: ESMF_CplCompNE

! !ARGUMENTS:
    type(ESMF_CplComp), intent(in) :: cplcomp1
    type(ESMF_CplComp), intent(in) :: cplcomp2

! !DESCRIPTION:
!   Test if both {\tt cplcomp1} and {\tt cplcomp2} alias the same ESMF CplComp
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE ccinit1, ccinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ESMF_CplCompNE = .not.ESMF_CplCompEQ(cplcomp1, cplcomp2)

  end function ESMF_CplCompNE
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompCreate"
!BOP
! !IROUTINE: ESMF_CplCompCreate - Create a CplComp
!
! !INTERFACE:
  recursive function ESMF_CplCompCreate(keywordEnforcer, config, configFile, &
    clock, petList, contextflag, name, rc)
!
! !RETURN VALUE:
    type(ESMF_CplComp) :: ESMF_CplCompCreate
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Config),       intent(in),  optional :: config
    character(len=*),        intent(in),  optional :: configFile
    type(ESMF_Clock),        intent(in),  optional :: clock
    integer,                 intent(in),  optional :: petList(:)
    type(ESMF_Context_Flag), intent(in),  optional :: contextflag
    character(len=*),        intent(in),  optional :: name
    integer,                 intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
! {\tt contextflag} to ESMF\_CONTEXT\_PARENT\_VM.
!
! The return value is the new {\tt ESMF\_CplComp}.
!
! The arguments are:
! \begin{description}
! \item[{[config]}]
!   An already-created {\tt ESMF\_Config} object to be attached to the newly
!   created component.
!   If both {\tt config} and {\tt configFile} arguments are specified,
!   {\tt config} takes priority.
! \item[{[configFile]}]
!   The filename of an {\tt ESMF\_Config} format file.
!   If specified, a new {\tt ESMF\_Config} object is created and attached to the
!   newly created component. The {\tt configFile} file is opened and associated
!   with the new config object.
!   If both {\tt config} and {\tt configFile} arguments are specified,
!   {\tt config} takes priority.
! \item[{[clock]}]
!   \begin{sloppypar}
!   Component-specific {\tt ESMF\_Clock}.  This clock is available to be
!   queried and updated by the new {\tt ESMF\_CplComp} as it chooses.
!   This should
!   not be the parent component clock, which should be maintained and passed
!   down to the initialize/run/finalize routines separately.
!   \end{sloppypar}
! \item[{[petList]}]
!   List of parent {\tt PET}s given to the created child component by the
!   parent component. If {\tt petList} is not specified all of the
!   parent {\tt PET}s will be given to the child component. The order of
!   PETs in {\tt petList} determines how the child local PETs refer back to
!   the parent PETs.
! \item[{[contextflag]}]
!   Specify the component's VM context. The default context is
!   {\tt ESMF\_CONTEXT\_OWN\_VM}. See section \ref{const:contextflag} for a
!   complete list of valid flags.
! \item[{[name]}]
!   Name of the newly-created {\tt ESMF\_CplComp}.  This name can be altered
!   from within the {\tt ESMF\_CplComp} code once the initialization routine
!   is called.
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
    if (ESMF_LogFoundAllocError(localrc, msg="Component class", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call Comp method
    call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_CPL, name, &
      configFile=configFile, config=config, clock=clock, petList=petList, &
      contextflag=contextflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) then
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
! !IROUTINE: ESMF_CplCompDestroy - Release resources associated with a CplComp

! !INTERFACE:
  recursive subroutine ESMF_CplCompDestroy(cplcomp, keywordEnforcer, &
    timeout, timeoutFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)          :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),   optional :: timeout
    logical,            intent(out),  optional :: timeoutFlag
    integer,            intent(out),  optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.3.0] Added argument {\tt timeout}.
!              Added argument {\tt timeoutFlag}.
!              The new arguments provide access to the fault-tolerant component
!              features.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! Destroys an {\tt ESMF\_CplComp}, releasing the resources associated
! with the object.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Release all resources associated with this {\tt ESMF\_CplComp}
!   and mark the object as invalid.  It is an error to pass this
!   object into any other routines after being destroyed.
! \item[{[timeout]}]
!   The maximum period in seconds that this call will wait in communications
!   with the actual component, before returning with a timeout condition.
!   The default is 3600, i.e. 1 hour. The {\tt timeout} argument is only
!   supported for connected dual components.
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
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
      if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
        msg="CplComp not initialized or already destroyed", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(cplcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call Comp method
    call ESMF_CompDestruct(cplcomp%compp, timeout=timeout, &
      timeoutFlag=timeoutFlag, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! mark object invalid
    call ESMF_BaseSetStatus(cplcomp%compp%base, ESMF_STATUS_INVALID, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
  recursive subroutine ESMF_CplCompFinalize(cplcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(in),    optional :: timeout
    logical,              intent(out),   optional :: timeoutFlag
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.3.0] Added argument {\tt timeout}.
!              Added argument {\tt timeoutFlag}.
!              The new arguments provide access to the fault-tolerant component
!              features.
! \end{description}
! \end{itemize}
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
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync}
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[phase]}]
!   Component providers must document whether each of their
!   routines are {\em single-phase} or {\em multi-phase}.
!   Single-phase routines require only one invocation to complete
!   their work.
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accommodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[timeout]}]
!   The maximum period in seconds that this call will wait in communications
!   with the actual component, before returning with a timeout condition.
!   The default is 3600, i.e. 1 hour. The {\tt timeout} argument is only
!   supported for connected dual components.
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local return code
    integer :: timeoutArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(cplcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_FINALIZEIC, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, timeout=timeoutArg, &
      userRc=userRc, rc=localrc)
    ! conditionally filter out the RC_TIMEOUT and return success
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

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompFinalize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompFinalizeAct"
!BOPI
! !IROUTINE: ESMF_CplCompFinalizeAct - Call the CplComp's finalize routine
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompFinalizeAct(cplcomp, importState, exportState, &
    clock, syncflag, phase, userRc, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !DESCRIPTION:
! Same as {\tt ESMF\_CplCompFinalize} but no redirection through the
! Interface Component method, instead directly call into the actual method.
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                       ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_FINALIZE, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, userRc=userRc, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompFinalizeAct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompGet"
!BOP
! !IROUTINE: ESMF_CplCompGet - Get CplComp information
!
! !INTERFACE:
  subroutine ESMF_CplCompGet(cplcomp, keywordEnforcer, configIsPresent, config, &
    configFileIsPresent, configFile, clockIsPresent, clock, localPet, &
    petCount, contextflag, currentMethod, currentPhase, vmIsPresent, &
    vm, name, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),      intent(in)            :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                 intent(out), optional :: configIsPresent
    type(ESMF_Config),       intent(out), optional :: config
    logical,                 intent(out), optional :: configFileIsPresent
    character(len=*),        intent(out), optional :: configFile
    logical,                 intent(out), optional :: clockIsPresent
    type(ESMF_Clock),        intent(out), optional :: clock
    integer,                 intent(out), optional :: localPet
    integer,                 intent(out), optional :: petCount
    type(ESMF_Context_Flag), intent(out), optional :: contextflag
    type(ESMF_Method_Flag),  intent(out), optional :: currentMethod
    integer,                 intent(out), optional :: currentPhase
    logical,                 intent(out), optional :: vmIsPresent
    type(ESMF_VM),           intent(out), optional :: vm
    character(len=*),        intent(out), optional :: name
    integer,                 intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Get information about an {\tt ESMF\_CplComp} object.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} object being queried.
! \item[{[configIsPresent]}]
!   {\tt .true.} if {\tt config} was set in CplComp object,
!   {\tt .false.} otherwise.
! \item[{[config]}]
!   Return the associated Config.
!   It is an error to query for the Config if none is associated with
!   the CplComp. If unsure, get {\tt configIsPresent} first to determine
!   the status.
! \item[{[configFileIsPresent]}]
!   {\tt .true.} if {\tt configFile} was set in CplComp object,
!   {\tt .false.} otherwise.
! \item[{[configFile]}]
!   Return the associated configuration filename.
!   It is an error to query for the configuration filename if none is associated with
!   the CplComp. If unsure, get {\tt configFileIsPresent} first to determine
!   the status.
! \item[{[clockIsPresent]}]
!   {\tt .true.} if {\tt clock} was set in CplComp object,
!   {\tt .false.} otherwise.
! \item[{[clock]}]
!   Return the associated Clock.
!   It is an error to query for the Clock if none is associated with
!   the CplComp. If unsure, get {\tt clockIsPresent} first to determine
!   the status.
! \item[{[localPet]}]
!   Return the local PET id within the {\tt ESMF\_CplComp} object.
! \item[{[petCount]}]
!   Return the number of PETs in the the {\tt ESMF\_CplComp} object.
! \item[{[contextflag]}]
!   Return the {\tt ESMF\_Context\_Flag} for this {\tt ESMF\_CplComp}.
!   See section \ref{const:contextflag} for a complete list of valid flags.
! \item[{[currentMethod]}]
!   Return the current {\tt ESMF\_Method\_Flag} of the {\tt ESMF\_CplComp} execution.
!   See section \ref{const:method}  for a complete list of valid options.
! \item[{[currentPhase]}]
!   Return the current {\tt phase} of the {\tt ESMF\_CplComp} execution.
! \item[{[vmIsPresent]}]
!   {\tt .true.} if {\tt vm} was set in CplComp object,
!   {\tt .false.} otherwise.
! \item[{[vm]}]
!   Return the associated VM.
!   It is an error to query for the VM if none is associated with
!   the CplComp. If unsure, get {\tt vmIsPresent} first to determine
!   the status.
! \item[{[name]}]
!   Return the name of the {\tt ESMF\_CplComp}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_CompStatus) :: compStatus

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompGet(cplcomp%compp, name=name, vm=vm, contextflag=contextflag,&
      clock=clock, configFile=configFile, config=config, &
      currentMethod=currentMethod, currentPhase=currentPhase, &
      localPet=localPet, petCount=petCount, compStatus=compStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call Comp method
    call ESMF_CompStatusGet(compStatus, &
      clockIsPresent = clockIsPresent, &
      configIsPresent = configIsPresent, &
      configFileIsPresent = configFileIsPresent, &
      vmIsPresent = vmIsPresent, &
      rc = localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompGetEPPhaseCount"
!BOPI
! !IROUTINE: ESMF_CplCompGetEPPhaseCount - Get number of phases of an entry point
!
! !INTERFACE:
  subroutine ESMF_CplCompGetEPPhaseCount(cplcomp, methodflag, phaseCount, &
    phaseZeroFlag, rc)

! !ARGUMENTS:
    type(ESMF_CplComp),     intent(in)            :: cplcomp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    integer,                intent(out)           :: phaseCount
    logical,                intent(out), optional :: phaseZeroFlag
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
! Get phaseCount
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   An {\tt ESMF\_CplComp} object.
! \item[methodflag]
!   \begin{sloppypar}
!   One of a set of predefined Component methods - e.g.
!   {\tt ESMF\_METHOD\_INITIALIZE}, {\tt ESMF\_METHOD\_RUN},
!   {\tt ESMF\_METHOD\_FINALIZE}. See section \ref{const:method}
!   for a complete list of valid method options.
!   \end{sloppypar}
! \item[phaseCount]
!   The number of phases for {\tt methodflag}. The method has 1..phaseCount phases.
! \item[phaseZeroFlag]
!   Return .true. if a "zero" phase was registered for {\tt methodflag}. Otherwise
!   return .false..
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    type(ESMF_Logical)::  phaseZeroFlagHelp

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    call c_ESMC_GetEntryPointPhaseCount(cplcomp, methodflag, phaseCount, &
      phaseZeroFlagHelp, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! translate ESMF_Logical -> logical
    if (present(phaseZeroFlag)) then
      phaseZeroFlag = phaseZeroFlagHelp
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompGetEPPhaseCount
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompGetInternalState - Get private data block pointer
!
! !INTERFACE:
! subroutine ESMF_CplCompGetInternalState(cplcomp, wrappedDataPointer, rc)
!
! !ARGUMENTS:
!   type(ESMF_CplComp)              :: cplcomp
!   type(wrapper)                   :: wrappedDataPointer
!   integer,            intent(out) :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
! Note that the {\tt wrappedDataPointer} argument needs to be a derived type
! which contains only a pointer of the type of the data block defined
! by the user.  When making this call the pointer needs to be unassociated.
! When the call returns, the pointer will now reference the original
! data block which was set during the previous call to
! {\tt ESMF\_CplCompSetInternalState}.
!
! Only the {\em last} data block set via
! {\tt ESMF\_CplCompSetInternalState} will be accessible.
!
! CAUTION: This method does not have an explicit Fortran interface. Do not
! specify argument keywords when calling this method!
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   An {\tt ESMF\_CplComp} object.
! \item[wrappedDataPointer]
!   A derived type (wrapper), containing only an unassociated pointer
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
  recursive subroutine ESMF_CplCompInitialize(cplcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(in),    optional :: timeout
    logical,              intent(out),   optional :: timeoutFlag
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.3.0] Added argument {\tt timeout}.
!              Added argument {\tt timeoutFlag}.
!              The new arguments provide access to the fault-tolerant component
!              features.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! Call the associated user initialization routine for
! an {\tt ESMF\_CplComp}.
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
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync}
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[phase]}]
!   Component providers must document whether each of their
!   routines are {\em single-phase} or {\em multi-phase}.
!   Single-phase routines require only one invocation to complete
!   their work.
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accommodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[timeout]}]
!   The maximum period in seconds that this call will wait in communications
!   with the actual component, before returning with a timeout condition.
!   The default is 3600, i.e. 1 hour. The {\tt timeout} argument is only
!   supported for connected dual components.
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: timeoutArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(cplcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_INITIALIZEIC, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, timeout=timeoutArg, &
      userRc=userRc, rc=localrc)
    ! conditionally filter out the RC_TIMEOUT and return success
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

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompInitialize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompInitializeAct"
!BOPI
! !IROUTINE: ESMF_CplCompInitializeAct - Call the CplComp's initialize routine
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompInitializeAct(cplcomp, importState, &
    exportState, clock, syncflag, phase, userRc, rc)
!
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !DESCRIPTION:
! Same as {\tt ESMF\_CplCompInitialize} but no redirection through the
! Interface Component method, instead directly call into the actual method.
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_INITIALIZE, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, userRc=userRc, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompInitializeAct
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompIsCreated()"
!BOP
! !IROUTINE: ESMF_CplCompIsCreated - Check whether a CplComp object has been created

! !INTERFACE:
  function ESMF_CplCompIsCreated(cplcomp, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_CplCompIsCreated
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt cplcomp} has been created. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[cplcomp]
!     {\tt ESMF\_CplComp} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ESMF_CplCompIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_CplCompGetInit(cplcomp)==ESMF_INIT_CREATED) &
      ESMF_CplCompIsCreated = .true.
  end function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompIsPetLocal"
!BOP
! !IROUTINE: ESMF_CplCompIsPetLocal - Inquire if this CplComp is to execute on the calling PET
!
! !INTERFACE:
  recursive function ESMF_CplCompIsPetLocal(cplcomp, keywordEnforcer, rc)
!
! !RETURN VALUE:
    logical :: ESMF_CplCompIsPetLocal
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_CplCompIsPetLocal = localresult

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_CplCompIsPetLocal
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompPrint"
!BOP
! !IROUTINE:  ESMF_CplCompPrint - Print CplComp information
!
! !INTERFACE:
  subroutine ESMF_CplCompPrint(cplcomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Prints information about an {\tt ESMF\_CplComp} to {\tt stdout}. \\
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to print.
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

    write (ESMF_UtilIOStdout,*) "Coupler Component:"
    ! call Comp method
    call ESMF_CompPrint(cplcomp%compp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
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
  recursive subroutine ESMF_CplCompReadRestart(cplcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(in),    optional :: timeout
    logical,              intent(out),   optional :: timeoutFlag
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.3.0] Added argument {\tt timeout}.
!              Added argument {\tt timeoutFlag}.
!              The new arguments provide access to the fault-tolerant component
!              features.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! Call the associated user read restart routine for
! an {\tt ESMF\_CplComp}.
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
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync}
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[phase]}]
!   Component providers must document whether each of their
!   routines are {\em single-phase} or {\em multi-phase}.
!   Single-phase routines require only one invocation to complete
!   their work.
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accommodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[timeout]}]
!   The maximum period in seconds that this call will wait in communications
!   with the actual component, before returning with a timeout condition.
!   The default is 3600, i.e. 1 hour. The {\tt timeout} argument is only
!   supported for connected dual components.
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                  ! local return code
    integer :: timeoutArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(cplcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_READRESTART, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, timeout=timeoutArg, &
      userRc=userRc, rc=localrc)
    ! conditionally filter out the RC_TIMEOUT and return success
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
  recursive subroutine ESMF_CplCompRun(cplcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(in),    optional :: timeout
    logical,              intent(out),   optional :: timeoutFlag
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.3.0] Added argument {\tt timeout}.
!              Added argument {\tt timeoutFlag}.
!              The new arguments provide access to the fault-tolerant component
!              features.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! Call the associated user run routine for
! an {\tt ESMF\_CplComp}.
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
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync}
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[phase]}]
!   Component providers must document whether each of their
!   routines are {\em single-phase} or {\em multi-phase}.
!   Single-phase routines require only one invocation to complete
!   their work.
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accommodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[timeout]}]
!   The maximum period in seconds that this call will wait in communications
!   with the actual component, before returning with a timeout condition.
!   The default is 3600, i.e. 1 hour. The {\tt timeout} argument is only
!   supported for connected dual components.
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                     ! local return code
    integer :: timeoutArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(cplcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_RUNIC, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, timeout=timeoutArg, &
      userRc=userRc, rc=localrc)
    ! conditionally filter out the RC_TIMEOUT and return success
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

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompRun
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompRunAct"
!BOPI
! !IROUTINE: ESMF_CplCompRunAct - Call the CplComp's run routine
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompRunAct(cplcomp, importState, exportState, &
    clock, syncflag, phase, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !DESCRIPTION:
! Same as {\tt ESMF\_CplCompRun} but no redirection through the
! Interface Component method, instead directly call into the actual method.
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                     ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_RUN, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, userRc=userRc, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompRunAct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompServiceLoop"
!BOP
! !IROUTINE: ESMF_CplCompServiceLoop - Call the CplComp's service loop routine

! !INTERFACE:
  recursive subroutine ESMF_CplCompServiceLoop(cplcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, port, timeout, timeoutFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: port
    integer,              intent(in),    optional :: timeout
    logical,              intent(out),   optional :: timeoutFlag
    integer,              intent(out),   optional :: rc
!
! !DESCRIPTION:
! Call the ServiceLoop routine for an {\tt ESMF\_CplComp}.
! This tries to establish a "component tunnel" between the {\em actual}
! Component (calling this routine) and a {\tt dual} Component connecting to it
! through a matching SetServices call.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to call service loop routine for.
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
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync}
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[port]}]
!   In case a port number is provided, the "component tunnel" is established
!   using sockets. The actual component side, i.e. the side that calls into
!   {\tt ESMF\_CplCompServiceLoop()}, starts to listen on the specified port
!   as the server. The valid port range is [1024, 65535].
!   In case the {\tt port} argument is {\em not} specified, the "component
!   tunnel" is established within the same executable using local communication
!   methods (e.g. MPI).
! \item[{[timeout]}]
!   The maximum period in seconds that this call will wait for communications
!   with the dual component, before returning with a timeout condition.
!   The default is 3600, i.e. 1 hour.
!   (NOTE: Currently this option is only available for socket based component
!   tunnels.)
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: localrc2                       ! local return code
    integer :: timeoutArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)

    if (.not.present(port).and.(present(timeout))) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Currently the 'timeout' argument requires the 'port' argument", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail out
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_SERVICELOOP, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, port=port, timeout=timeoutArg, userRc=localrc2, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! ESMF_METHOD_SERVICELOOP is a framework internal method, therefore
    ! the code returned in userRc is a framework internal return code and must
    ! be treated as such. However, the treatment of RC_TIMEOUT depends
    ! on the presence/absence of timeoutFlag.
    if (present(timeoutFlag)) then
      timeoutFlag = .false. ! initialize
      if ((localrc2==ESMF_RC_TIMEOUT).or.(localrc2==ESMC_RC_TIMEOUT)) then
        timeoutFlag = .true.      ! indicate timeout through flag argument
        localrc2 = ESMF_SUCCESS   ! do not raise error condition on user level
      endif
    endif
    if (ESMF_LogFoundError(localrc2, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompServiceLoop
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSet"
!BOP
! !IROUTINE: ESMF_CplCompSet - Set or reset information about the CplComp
!
! !INTERFACE:
  subroutine ESMF_CplCompSet(cplcomp, keywordEnforcer, config, configFile, &
    clock, name, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Config),  intent(in),  optional :: config
    character(len=*),   intent(in),  optional :: configFile
    type(ESMF_Clock),   intent(in),  optional :: clock
    character(len=*),   intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Sets or resets information about an {\tt ESMF\_CplComp}.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to change.
! \item[{[name]}]
!   Set the name of the {\tt ESMF\_CplComp}.
! \item[{[config]}]
!   An already-created {\tt ESMF\_Config} object to be attached to the
!   component.
!   If both {\tt config} and {\tt configFile} arguments are specified,
!   {\tt config} takes priority.
! \item[{[configFile]}]
!   The filename of an {\tt ESMF\_Config} format file.
!   If specified, a new {\tt ESMF\_Config} object is created and attached to the
!   component. The {\tt configFile} file is opened and associated
!   with the new config object.
!   If both {\tt config} and {\tt configFile} arguments are specified,
!   {\tt config} takes priority.
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
    if (ESMF_LogFoundError(localrc, &
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
  recursive subroutine ESMF_CplCompSetEntryPoint(cplcomp, methodflag, &
    userRoutine, keywordEnforcer, phase, rc)

! !ARGUMENTS:
    type(ESMF_CplComp),     intent(inout)         :: cplcomp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
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
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: phase
    integer,                intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Registers a user-supplied {\tt userRoutine} as the entry point for one of the
! predefined Component {\tt methodflag}s. After this call the {\tt userRoutine}
! becomes accessible via the standard Component method API.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   An {\tt ESMF\_CplComp} object.
! \item[methodflag]
!   \begin{sloppypar}
!   One of a set of predefined Component methods - e.g.
!   {\tt ESMF\_METHOD\_INITIALIZE}, {\tt ESMF\_METHOD\_RUN},
!   {\tt ESMF\_METHOD\_FINALIZE}. See section \ref{const:method}
!   for a complete list of valid method options.
!   \end{sloppypar}
! \item[userRoutine]
!   The user-supplied subroutine to be associated for this {\tt methodflag}.
!   The Component writer must supply a subroutine with the exact interface
!   shown above for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!   must not be declared as optional, and the types, intent and order must match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
! \item[{[phase]}]
!   The {\tt phase} number for multi-phase methods. For single phase
!   methods the {\tt phase} argument can be omitted. The default setting
!   is 1.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
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

    call c_ESMC_SetEntryPoint(cplcomp, methodflag, userRoutine, phaseArg, &
      localrc)
!TODO: back in once thread-safe    if (ESMF_LogFoundError(localrc, &
!TODO: back in once thread-safe      ESMF_ERR_PASSTHRU, &
!TODO: back in once thread-safe      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetEntryPoint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CplCompSetInternalState - Set private data block pointer
!
! !INTERFACE:
! subroutine ESMF_CplCompSetInternalState(cplcomp, wrappedDataPointer, rc)
!
! !ARGUMENTS:
!   type(ESMF_CplComp)              :: cplcomp
!   type(wrapper)                   :: wrappedDataPointer
!   integer,            intent(out) :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
! CAUTION: This method does not have an explicit Fortran interface. Do not
! specify argument keywords when calling this method!
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   An {\tt ESMF\_CplComp} object.
! \item[wrappedDataPointer]
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
  recursive subroutine ESMF_CplCompSetServices(cplcomp, userRoutine, &
     keywordEnforcer, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: cplcomp
    interface
      subroutine userRoutine(cplcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: userRc
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! \label{CplComp:SetServices}
! Call into user provided {\tt userRoutine} which is responsible
! for setting Component's Initialize(), Run(), and Finalize() services.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[userRoutine]
!  The Component writer must supply a subroutine with the exact interface
!  shown above for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!  must not be declared as optional, and the types, intent and order must match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
!
!  \begin{sloppypar}
!  The {\tt userRoutine}, when called by the framework, must make successive calls to
!  {\tt ESMF\_CplCompSetEntryPoint()} to preset callback routines for standard
!  Component Initialize(), Run(), and Finalize() methods.
!  \end{sloppypar}
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    call c_ESMC_SetServices(cplcomp, userRoutine, localUserRc, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! now indicate that this Component has a VM associated
    cplcomp%compp%compStatus%vmIsPresent = .true.

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetServices
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetServicesShObj"
!BOP
! !IROUTINE: ESMF_CplCompSetServices - Call user routine through name lookup, to register CplComp methods
!
! !INTERFACE:
  ! Private name; call using ESMF_CplCompSetServices()
  recursive subroutine ESMF_CplCompSetServicesShObj(cplcomp, userRoutine, &
    keywordEnforcer, sharedObj, userRoutineFound, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
    character(len=*),    intent(in)            :: userRoutine
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),    intent(in),  optional :: sharedObj
    logical,             intent(out), optional :: userRoutineFound
    integer,             intent(out), optional :: userRc
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.3.0r] Added argument {\tt userRoutineFound}.
!              The new argument provides a way to test availability without
!              causing error conditions.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! \label{CplComp:SetServicesShObj}
! Call into a user provided routine which is responsible for setting
! Component's Initialize(), Run(), and Finalize() services. The named
! {\tt userRoutine} must exist in the executable, or in the shared object
! specified by {\tt sharedObj}. In the latter case all of the platform
! specific details about dynamic linking and loading apply.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[userRoutine]
!   Name of routine to be called, specified as a character string.
!   The Component writer must supply a subroutine with the exact interface
!   shown for {\tt userRoutine} below. Arguments must not be declared
!   as optional, and the types, intent and order must match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
!
!   !INTERFACE:
!     interface
!       subroutine userRoutine(cplcomp, rc)
!         type(ESMF_CplComp)   :: cplcomp    ! must not be optional
!         integer, intent(out) :: rc         ! must not be optional
!       end subroutine
!     end interface
!
!   !DESCRIPTION:
!   \begin{sloppypar}
!   The {\tt userRoutine}, when called by the framework, must make successive
!   calls to {\tt ESMF\_CplCompSetEntryPoint()} to preset callback routines for
!   standard Component Initialize(), Run(), and Finalize() methods.
!   \end{sloppypar}
! \item[{[sharedObj]}]
!   Name of shared object that contains {\tt userRoutine}. If the
!   {\tt sharedObj} argument is not provided the executable itself will be
!   searched for {\tt userRoutine}.
! \item[{[userRoutineFound]}]
!   Report back whether the specified {\tt userRoutine} was found and executed,
!   or was not available. If this argument is present, not finding the
!   {\tt userRoutine} will not result in returning an error in {\tt rc}.
!   The default is to return an error if the {\tt userRoutine} cannot be found.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc
    character(len=0)    :: emptyString
    type(ESMF_Logical)  :: userRoutineFoundHelp
    logical             :: userRoutineFoundHelpHelp

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    if (present(sharedObj)) then
      call c_ESMC_SetServicesShObj(cplcomp, userRoutine, sharedObj, &
        userRoutineFoundHelp, localUserRc, localrc)
    else
      call c_ESMC_SetServicesShObj(cplcomp, userRoutine, emptyString, &
        userRoutineFoundHelp, localUserRc, localrc)
    endif
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! translate ESMF_Logical -> logical
    userRoutineFoundHelpHelp = userRoutineFoundHelp

    ! report back
    if (present(userRoutineFound)) userRoutineFound = userRoutineFoundHelpHelp

    if (userRoutineFoundHelpHelp) then
      ! routine found and executed -> indicate this Component has VM associated
      cplcomp%compp%compStatus%vmIsPresent = .true.
    else
      ! routine not found
      if (.not.present(userRoutineFound)) then
        ! an error condition that needs to be reported back
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="userRoutine was not found", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetServicesShObj
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetServicesComp"
!BOP
! !IROUTINE: ESMF_CplCompSetServices - Set to serve as Dual Component for an Actual Component
!
! !INTERFACE:
  ! Private name; call using ESMF_CplCompSetServices()
  recursive subroutine ESMF_CplCompSetServicesComp(cplcomp, &
    actualCplcomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: cplcomp
    type(ESMF_CplComp), intent(in)            :: actualCplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
! Set the services of a Coupler Component to serve a "dual" Component for an
! "actual" Component. The component tunnel is VM based.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Dual Coupler Component.
! \item[actualCplcomp]
!   Actual Coupler Component.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local error status
    integer, pointer :: actualCompPetList(:)
    integer :: actualCompRootPet, i

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, actualCplcomp, rc)

    ! access the petList of the actualCplcomp and find the lowest PET
    ! -> this is going to be the rendezvous PET for the component tunnel setup
    nullify(actualCompPetList)
    ! call Comp method
    call ESMF_CompGet(actualCplcomp%compp, petList=actualCompPetList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    actualCompRootPet = actualCompPetList(1)  ! prime the search variable
    do i=2, size(actualCompPetList)
      if (actualCompPetList(i) < actualCompRootPet) &
        actualCompRootPet = actualCompPetList(i)
    enddo
    deallocate(actualCompPetList)

    call c_ESMC_SetServicesComp(cplcomp, cplcomp%compp%compTunnel, &
      actualCplcomp, actualCompRootPet, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! now indicate that this Component has a VM associated
    cplcomp%compp%compStatus%vmIsPresent = .true.

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetServicesComp
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetServicesSock"
!BOP
! !IROUTINE: ESMF_CplCompSetServices - Set to serve as Dual Component for an Actual Component through sockets
!
! !INTERFACE:
  ! Private name; call using ESMF_CplCompSetServices()
  recursive subroutine ESMF_CplCompSetServicesSock(cplcomp, port, &
    keywordEnforcer, server, timeout, timeoutFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: cplcomp
    integer,            intent(in)            :: port
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),   intent(in),  optional :: server
    integer,            intent(in),  optional :: timeout
    logical,            intent(out), optional :: timeoutFlag
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
! Set the services of a Coupler Component to serve a "dual" Component for an
! "actual" Component. The component tunnel is socket based.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Dual Coupler Component.
! \item[port]
!   Port number under which the actual component is being served. The valid
!   port range is [1024, 65535].
! \item[{[server]}]
!   Server name where the actual component is being served. The default, i.e.
!   if the {\tt server} argument was not provided, is {\tt localhost}.
! \item[{[timeout]}]
!   The maximum period in seconds that this call will wait in communications
!   with the actual component, before returning with a timeout condition.
!   The default is 3600, i.e. 1 hour.
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local error status
    integer :: timeoutArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    if (present(server)) then
      call c_ESMC_SetServicesSock(cplcomp, cplcomp%compp%compTunnel, &
        port, server, timeoutArg, localrc)
    else
      call c_ESMC_SetServicesSock(cplcomp, cplcomp%compp%compTunnel, &
        port, "localhost", timeoutArg, localrc)
    endif
    ! conditionally filter out the RC_TIMEOUT and return success
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

    ! now indicate that this Component has a VM associated
    cplcomp%compp%compStatus%vmIsPresent = .true.

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetServicesSock
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVM"
!BOP
! !IROUTINE: ESMF_CplCompSetVM - Call user routine to set CplComp VM properties
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompSetVM(cplcomp, userRoutine, &
    keywordEnforcer, userRc, rc)
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: cplcomp
    interface
      subroutine userRoutine(cplcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: userRc
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Optionally call into user provided {\tt userRoutine} which is responsible
! for setting Component's VM properties.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[userRoutine]
!   The Component writer must supply a subroutine with the exact interface
!   shown above for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!   must not be declared as optional, and the types, intent and order must match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
!
!   The subroutine, when called by the framework, is expected to use any of the
!   {\tt ESMF\_CplCompSetVMxxx()} methods to set the properties of the VM
!   associated with the Coupler Component.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    call c_ESMC_SetVM(cplcomp, userRoutine, localUserRc, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetVM
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMShObj"
!BOP
! !IROUTINE: ESMF_CplCompSetVM - Call user routine through name lookup, to set CplComp VM properties
! !INTERFACE:
  ! Private name; call using ESMF_CplCompSetVM()
  recursive subroutine ESMF_CplCompSetVMShObj(cplcomp, userRoutine, &
    keywordEnforcer, sharedObj, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
    character(len=*),    intent(in)            :: userRoutine
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),    intent(in),  optional :: sharedObj
    integer,             intent(out), optional :: userRc
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Optionally call into user provided {\tt userRoutine} which is responsible
! for setting Component's VM properties. The named
! {\tt userRoutine} must exist in the executable, or in the shared object
! specified by {\tt sharedObj}. In the latter case all of the platform
! specific details about dynamic linking and loading apply.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   Coupler Component.
! \item[userRoutine]
!   Routine to be called, specified as a character string.
!   The Component writer must supply a subroutine with the exact interface
!   shown for {\tt userRoutine} below. Arguments must not be declared
!   as optional, and the types, intent and order must match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
!
!   !INTERFACE:
!     interface
!       subroutine userRoutine(cplcomp, rc)
!         type(ESMF_CplComp)   :: cplcomp     ! must not be optional
!         integer, intent(out) :: rc          ! must not be optional
!       end subroutine
!     end interface
!
!   !DESCRIPTION:
!   The subroutine, when called by the framework, is expected to use any of the
!   {\tt ESMF\_CplCompSetVMxxx()} methods to set the properties of the VM
!   associated with the Coupler Component.
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
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc
    character(len=0) :: emptyString

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    if (present(sharedObj)) then
      call c_ESMC_SetVMShObj(cplcomp, userRoutine, sharedObj, localUserRc, &
        localrc)
    else
      call c_ESMC_SetVMShObj(cplcomp, userRoutine, emptyString, localUserRc, &
        localrc)
    endif
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetVMShObj
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMaxPEs"
!BOP
! !IROUTINE: ESMF_CplCompSetVMMaxPEs - Associate PEs with PETs in CplComp VM
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMaxPEs(cplcomp, keywordEnforcer, &
    maxPeCountPerPet, prefIntraProcess, prefIntraSsi, prefInterSsi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in),  optional :: maxPeCountPerPet
    integer,             intent(in),  optional :: prefIntraProcess
    integer,             intent(in),  optional :: prefIntraSsi
    integer,             intent(in),  optional :: prefInterSsi
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!   Attempts to associate up to {\tt maxPeCountPerPet} PEs with each PET. Only
!   PEs that are located on the same single system image (SSI) can be associated
!   with the same PET. Within this constraint the call tries to get as close as
!   possible to the number specified by {\tt maxPeCountPerPet}.
!
!   The other constraint to this call is that the number of PEs is preserved.
!   This means that the child Component in the end is associated with as many
!   PEs as the parent Component provided to the child. The number of child PETs
!   however is adjusted according to the above rule.
!
!   The typical use of {\tt ESMF\_CplCompSetVMMaxPEs()} is to allocate
!   multiple PEs per PET in a Component for user-level threading, e.g. OpenMP.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
! \item[{[maxPeCountPerPet]}]
!   Maximum number of PEs on each PET.
!   Default for each SSI is the local number of PEs.
! \item[{[prefIntraProcess]}]
!   Communication preference within a single process.
!   {\em Currently options not documented. Use default.}
! \item[{[prefIntraSsi]}]
!   Communication preference within a single system image (SSI).
!   {\em Currently options not documented. Use default.}
! \item[{[prefInterSsi]}]
!   Communication preference between different single system images (SSIs).
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
    call ESMF_CompSetVMMaxPEs(cplcomp%compp, maxPeCountPerPet, &
      prefIntraProcess, prefIntraSsi, prefInterSsi, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetVMMaxPEs
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMaxThreads"
!BOP
! !IROUTINE: ESMF_CplCompSetVMMaxThreads - Set multi-threaded PETs in CplComp VM
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMaxThreads(cplcomp, keywordEnforcer, &
    maxPetCountPerVas, prefIntraProcess, prefIntraSsi, prefInterSsi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in),  optional :: maxPetCountPerVas
    integer,             intent(in),  optional :: prefIntraProcess
    integer,             intent(in),  optional :: prefIntraSsi
    integer,             intent(in),  optional :: prefInterSsi
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!   Attempts to provide {\tt maxPetCountPerVas} threaded PETs in each
!   virtual address space (VAS). Only as many threaded PETs as there are PEs
!   located on the single system image (SSI) can be associated with the VAS.
!   Within this constraint the call tries to get as close as possible to the
!   number specified by {\tt maxPetCountPerVas}.
!
!   The other constraint to this call is that the number of PETs is preserved.
!   This means that the child Component in the end is associated with as many
!   PETs as the parent Component provided to the child. The threading level of
!   the child PETs however is adjusted according to the above rule.
!
!   The typical use of {\tt ESMF\_CplCompSetVMMaxThreads()} is to run a
!   Component multi-threaded with groups of PETs executing within a common
!   virtual address space.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
! \item[{[maxPetCountPerVas]}]
!   Maximum number of threaded PETs in each virtual address space (VAS).
!   Default for each SSI is the local number of PEs.
! \item[{[prefIntraProcess]}]
!   Communication preference within a single process.
!   {\em Currently options not documented. Use default.}
! \item[{[prefIntraSsi]}]
!   Communication preference within a single system image (SSI).
!   {\em Currently options not documented. Use default.}
! \item[{[prefInterSsi]}]
!   Communication preference between different single system images (SSIs).
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
    call ESMF_CompSetVMMaxThreads(cplcomp%compp, maxPetCountPerVas, &
      prefIntraProcess, prefIntraSsi, prefInterSsi, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_CplCompSetVMMaxThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompSetVMMinThreads"
!BOP
! !IROUTINE: ESMF_CplCompSetVMMinThreads - Set a reduced threading level in CplComp VM
!
! !INTERFACE:
  subroutine ESMF_CplCompSetVMMinThreads(cplcomp, keywordEnforcer, &
    maxPeCountPerPet, prefIntraProcess, prefIntraSsi, prefInterSsi, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(inout)         :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in),  optional :: maxPeCountPerPet
    integer,             intent(in),  optional :: prefIntraProcess
    integer,             intent(in),  optional :: prefIntraSsi
    integer,             intent(in),  optional :: prefInterSsi
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_CplComp}.
!   Reduces the number of threaded PETs in each VAS. The {\tt max} argument
!   may be specified to limit the maximum number of PEs that a single PET
!   can be associated with.
!
!   Several constraints apply: 1) the number of PEs cannot change, 2) PEs
!   cannot migrate between single system images (SSIs), 3) the number of PETs
!   cannot increase, only decrease, 4) PETs cannot migrate between virtual
!   address spaces (VASs), nor can VASs migrate between SSIs.
!
!   The typical use of {\tt ESMF\_CplCompSetVMMinThreads()} is to run a
!   Component across a set of single-threaded PETs.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to set the {\tt ESMF\_VM} for.
! \item[{[maxPeCountPerPet]}]
!   Maximum number of PEs on each PET.
!   Default for each SSI is the local number of PEs.
! \item[{[prefIntraProcess]}]
!   Communication preference within a single process.
!   {\em Currently options not documented. Use default.}
! \item[{[prefIntraSsi]}]
!   Communication preference within a single system image (SSI).
!   {\em Currently options not documented. Use default.}
! \item[{[prefInterSsi]}]
!   Communication preference between different single system images (SSIs).
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
    call ESMF_CompSetVMMinThreads(cplcomp%compp, maxPeCountPerPet, &
      prefIntraProcess, prefIntraSsi, prefInterSsi, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
  subroutine ESMF_CplCompValidate(cplcomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Currently all this method does is to check that the {\tt cplcomp}
! was created.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to validate.
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
    call ESMF_CompValidate(cplcomp%compp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
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
  subroutine ESMF_CplCompWait(cplcomp, keywordEnforcer, syncflag, &
    timeout, timeoutFlag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)         :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag), intent(in),  optional :: syncflag
    integer,              intent(in),  optional :: timeout
    logical,              intent(out), optional :: timeoutFlag
    integer,              intent(out), optional :: userRc
    integer,              intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.3.0] Added argument {\tt timeout}.
!              Added argument {\tt timeoutFlag}.
!              The new arguments provide access to the fault-tolerant component
!              features.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! When executing asynchronously, wait for an {\tt ESMF\_CplComp} to return.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   {\tt ESMF\_CplComp} to wait for.
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync}
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[timeout]}]
!   The maximum period in seconds the actual component is allowed to execute
!   a previously invoked component method before it must communicate back to
!   the dual component. If the actual component does not communicate back in
!   the specified time, a timeout condition is raised on the dual side (this
!   side). The default is 3600, i.e. 1 hour. The {\tt timeout} argument is only
!   supported for connected dual components.
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
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

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(cplcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call Comp method
    call ESMF_CompWait(cplcomp%compp, syncflag=syncflag, timeout=timeout, &
      userRc=userRc, rc=localrc)
    ! conditionally filter out the RC_TIMEOUT and return success
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
  recursive subroutine ESMF_CplCompWriteRestart(cplcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),   intent(inout)           :: cplcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(in),    optional :: timeout
    logical,              intent(out),   optional :: timeoutFlag
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[5.3.0] Added argument {\tt timeout}.
!              Added argument {\tt timeoutFlag}.
!              The new arguments provide access to the fault-tolerant component
!              features.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! Call the associated user write restart routine for
! an {\tt ESMF\_CplComp}.
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
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync}
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[phase]}]
!   Component providers must document whether each of their
!   routines are {\em single-phase} or {\em multi-phase}.
!   Single-phase routines require only one invocation to complete
!   their work.
!   Multi-phase routines provide multiple subroutines to accomplish
!   the work, accommodating components which must complete part of their
!   work, return to the caller and allow other processing to occur,
!   and then continue the original operation.
!   For multiple-phase child components, this is the integer phase
!   number to be invoked.
!   For single-phase child components this argument is optional. The default is
!   1.
! \item[{[timeout]}]
!   The maximum period in seconds that this call will wait in communications
!   with the actual component, before returning with a timeout condition.
!   The default is 3600, i.e. 1 hour. The {\tt timeout} argument is only
!   supported for connected dual components.
! \item[{[timeoutFlag]}]
!   Returns {\tt .true.} if the timeout was reached, {\tt .false.} otherwise.
!   If {\tt timeoutFlag} was {\em not} provided, a timeout condition will lead
!   to a return code of {\tt rc \textbackslash = ESMF\_SUCCESS}. Otherwise the
!   return value of {\tt timeoutFlag} is the sole indicator of a timeout
!   condition.
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: timeoutArg

    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit,cplcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(cplcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(cplcomp%compp, method=ESMF_METHOD_WRITERESTART, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, timeout=timeoutArg, &
      userRc=userRc, rc=localrc)
    ! conditionally filter out the RC_TIMEOUT and return success
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
!   {\tt ESMF\_CplComp} from which to retrieve status.
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
