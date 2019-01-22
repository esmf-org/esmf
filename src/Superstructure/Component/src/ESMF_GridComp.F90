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
#define ESMF_FILENAME "ESMF_GridComp.F90"
!==============================================================================
!
! ESMF Gridded Component module
module ESMF_GridCompMod
!
!==============================================================================
!
! This file contains the Gridded Component class definition and all
!  Gridded Component class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
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
  use ESMF_VMMod
  use ESMF_ConfigMod
  use ESMF_ClockTypeMod
  use ESMF_ClockMod
  use ESMF_StateTypesMod
  use ESMF_StateMod
  use ESMF_GridMod
  use ESMF_MeshMod
  use ESMF_LocStreamMod
  use ESMF_XGridMod
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

  public ESMF_GridCompCreate
  public ESMF_GridCompDestroy
  public ESMF_GridCompFinalize
  public ESMF_GridCompFinalizeAct
  public ESMF_GridCompGet
  public ESMF_GridCompGetEPPhaseCount
  public ESMF_GridCompInitialize
  public ESMF_GridCompInitializeAct
  public ESMF_GridCompIsCreated
  public ESMF_GridCompIsPetLocal
  public ESMF_GridCompPrint
  public ESMF_GridCompReadRestart
  public ESMF_GridCompRun
  public ESMF_GridCompRunAct
  public ESMF_GridCompServiceLoop
  public ESMF_GridCompSet
  public ESMF_GridCompSetEntryPoint
  public ESMF_GridCompSetServices
  public ESMF_GridCompSetVM
  public ESMF_GridCompSetVMMaxPEs
  public ESMF_GridCompSetVMMaxThreads
  public ESMF_GridCompSetVMMinThreads
  public ESMF_GridCompValidate
  public ESMF_GridCompWait
  public ESMF_GridCompWriteRestart

! - ESMF-internal methods:
  public ESMF_GridCompGetInit

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
  interface ESMF_GridCompSetServices
    module procedure ESMF_GridCompSetServices
    module procedure ESMF_GridCompSetServicesShObj
    module procedure ESMF_GridCompSetServicesComp
    module procedure ESMF_GridCompSetServicesSock
  end interface
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
  interface ESMF_GridCompSetVM
    module procedure ESMF_GridCompSetVM
    module procedure ESMF_GridCompSetVMShObj
  end interface
!------------------------------------------------------------------------------

!===============================================================================
! GridCompOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_GridCompAssignment(=) - GridComp assignment
!
! !INTERFACE:
!   interface assignment(=)
!   gridcomp1 = gridcomp2
!
! !ARGUMENTS:
!   type(ESMF_GridComp) :: gridcomp1
!   type(ESMF_GridComp) :: gridcomp2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Assign gridcomp1 as an alias to the same ESMF GridComp object in memory
!   as gridcomp2. If gridcomp2 is invalid, then gridcomp1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[gridcomp1]
!     The {\tt ESMF\_GridComp} object on the left hand side of the assignment.
!   \item[gridcomp2]
!     The {\tt ESMF\_GridComp} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_GridCompOperator(==) - GridComp equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (gridcomp1 == gridcomp2) then ... endif
!             OR
!   result = (gridcomp1 == gridcomp2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_GridComp), intent(in) :: gridcomp1
!   type(ESMF_GridComp), intent(in) :: gridcomp2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether gridcomp1 and gridcomp2 are valid aliases to the same ESMF
!   GridComp object in memory. For a more general comparison of two ESMF GridComps,
!   going beyond the simple alias test, the ESMF\_GridCompMatch() function (not yet
!   implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[gridcomp1]
!     The {\tt ESMF\_GridComp} object on the left hand side of the equality
!     operation.
!   \item[gridcomp2]
!     The {\tt ESMF\_GridComp} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_GridCompEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_GridCompOperator(/=) - GridComp not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (gridcomp1 /= gridcomp2) then ... endif
!             OR
!   result = (gridcomp1 /= gridcomp2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_GridComp), intent(in) :: gridcomp1
!   type(ESMF_GridComp), intent(in) :: gridcomp2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether gridcomp1 and gridcomp2 are {\it not} valid aliases to the
!   same ESMF GridComp object in memory. For a more general comparison of two ESMF
!   GridComps, going beyond the simple alias test, the ESMF\_GridCompMatch() function
!   (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[gridcomp1]
!     The {\tt ESMF\_GridComp} object on the left hand side of the non-equality
!     operation.
!   \item[gridcomp2]
!     The {\tt ESMF\_GridComp} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_GridCompNE

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
#define ESMF_METHOD "ESMF_GridCompEQ()"
!BOPI
! !IROUTINE:  ESMF_GridCompEQ - Compare two GridComps for equality
!
! !INTERFACE:
  function ESMF_GridCompEQ(gridcomp1, gridcomp2)
!
! !RETURN VALUE:
    logical :: ESMF_GridCompEQ

! !ARGUMENTS:
    type(ESMF_GridComp), intent(in) :: gridcomp1
    type(ESMF_GridComp), intent(in) :: gridcomp2

!
! !DESCRIPTION:
!   Test if both {\tt gridcomp1} and {\tt gridcomp2} alias the same ESMF GridComp
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE gcinit1, gcinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    gcinit1 = ESMF_GridCompGetInit(gridcomp1)
    gcinit2 = ESMF_GridCompGetInit(gridcomp2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (gcinit1 .eq. ESMF_INIT_CREATED .and. &
      gcinit2 .eq. ESMF_INIT_CREATED) then
      ESMF_GridCompEQ = associated(gridcomp1%compp,gridcomp2%compp)
    else
      ESMF_GridCompEQ = ESMF_FALSE
    endif

  end function ESMF_GridCompEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompNE()"
!BOPI
! !IROUTINE:  ESMF_GridCompNE - Compare two GridComps for non-equality
!
! !INTERFACE:
  function ESMF_GridCompNE(gridcomp1, gridcomp2)
!
! !RETURN VALUE:
    logical :: ESMF_GridCompNE

! !ARGUMENTS:
    type(ESMF_GridComp), intent(in) :: gridcomp1
    type(ESMF_GridComp), intent(in) :: gridcomp2

!
! !DESCRIPTION:
!   Test if both {\tt gridcomp1} and {\tt gridcomp2} alias the same ESMF GridComp
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE gcinit1, gcinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ESMF_GridCompNE = .not.ESMF_GridCompEQ(gridcomp1, gridcomp2)

  end function ESMF_GridCompNE
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompCreate"
!BOP
! !IROUTINE: ESMF_GridCompCreate - Create a GridComp
!
! !INTERFACE:
  recursive function ESMF_GridCompCreate(keywordEnforcer, grid, gridList, &
    mesh, meshList, locstream, locstreamList, xgrid, xgridList, &
    config, configFile, clock, petList, contextflag, name, rc)
!
! !RETURN VALUE:
    type(ESMF_GridComp) :: ESMF_GridCompCreate
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Grid),         intent(in),    optional :: grid
    type(ESMF_Grid),         intent(in),    optional :: gridList(:)
    type(ESMF_Mesh),         intent(in),    optional :: mesh
    type(ESMF_Mesh),         intent(in),    optional :: meshList(:)
    type(ESMF_LocStream),    intent(in),    optional :: locstream
    type(ESMF_LocStream),    intent(in),    optional :: locstreamList(:)
    type(ESMF_XGrid),        intent(in),    optional :: xgrid
    type(ESMF_XGrid),        intent(in),    optional :: xgridList(:)
    type(ESMF_Config),       intent(in),    optional :: config
    character(len=*),        intent(in),    optional :: configFile
    type(ESMF_Clock),        intent(in),    optional :: clock
    integer,                 intent(in),    optional :: petList(:)
    type(ESMF_Context_Flag), intent(in),    optional :: contextflag
    character(len=*),        intent(in),    optional :: name
    integer,                 intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \begin{sloppypar}
! \item[7.1.0r] Added arguments {\tt gridList}, {\tt mesh}, {\tt meshList},
!   {\tt locstream}, {\tt locstreamList}, {\tt xgrid}, and {\tt xgridList}.
!   These arguments add support for holding references to multiple geom objects,
!   either of the same type, or different type, in the same
!   {\tt ESMF\_GridComp} object.
! \end{sloppypar}
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! This interface creates an {\tt ESMF\_GridComp} object. By default, a
! separate VM context will be created for each component.  This implies
! creating a new MPI communicator and allocating additional memory to
! manage the VM resources. When running on a large number of processors,
! creating a separate VM for each component could be both time and memory
! inefficient.  If the application is sequential, i.e., each component is
! running on all the PETs of the global VM, it will be more efficient to use
! the global VM instead of creating a new one.  This can be done by setting
! {\tt contextflag} to ESMF\_CONTEXT\_PARENT\_VM.
!
! The return value is the new {\tt ESMF\_GridComp}.
!
! The arguments are:
! \begin{description}
! \item[{[grid]}]
!   Associate an {\tt ESMF\_Grid} object with the newly created component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt grid} object.
!   The {\tt grid} argument is mutually exclusive with the {\tt gridList}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt grid} nor {\tt gridList} are provided,
!   no {\tt ESMF\_Grid} objects are associated with the component.
! \item[{[gridList]}]
!   Associate a list of {\tt ESMF\_Grid} objects with the newly created
!   component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt gridList} object.
!   The {\tt gridList} argument is mutually exclusive with the {\tt grid}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt grid} nor {\tt gridList} are provided,
!   no {\tt ESMF\_Grid} objects are associated with the component.
! \item[{[mesh]}]
!   Associate an {\tt ESMF\_Mesh} object with the newly created component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt mesh} object.
!   The {\tt mesh} argument is mutually exclusive with the {\tt meshList}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt mesh} nor {\tt meshList} are provided,
!   no {\tt ESMF\_Mesh} objects are associated with the component.
! \item[{[meshList]}]
!   Associate a list of {\tt ESMF\_Mesh} objects with the newly created
!   component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt meshList} object.
!   The {\tt meshList} argument is mutually exclusive with the {\tt mesh}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt mesh} nor {\tt meshList} are provided,
!   no {\tt ESMF\_Mesh} objects are associated with the component.
! \item[{[locstream]}]
!   Associate an {\tt ESMF\_LocStream} object with the newly created component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt locstream} object.
!   The {\tt locstream} argument is mutually exclusive with the
!   {\tt locstreamList}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt locstream} nor {\tt locstreamList} are
!   provided, no {\tt ESMF\_LocStream} objects are associated with the
!   component.
! \item[{[locstreamList]}]
!   Associate a list of {\tt ESMF\_LocStream} objects with the newly created
!   component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt locstreamList} object.
!   The {\tt locstreamList} argument is mutually exclusive with the
!   {\tt locstream}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt locstream} nor {\tt locstreamList} are
!   provided, no {\tt ESMF\_LocStream} objects are associated with the
!   component.
! \item[{[xgrid]}]
!   Associate an {\tt ESMF\_XGrid} object with the newly created component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt xgrid} object.
!   The {\tt xgrid} argument is mutually exclusive with the {\tt xgridList}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt xgrid} nor {\tt xgridList} are provided,
!   no {\tt ESMF\_XGrid} objects are associated with the component.
! \item[{[xgridList]}]
!   Associate a list of {\tt ESMF\_XGrid} objects with the newly created
!   component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt xgridList} object.
!   The {\tt xgridList} argument is mutually exclusive with the {\tt xgrid}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt xgrid} nor {\tt xgridList} are provided,
!   no {\tt ESMF\_XGrid} objects are associated with the component.
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
!   queried and updated by the new {\tt ESMF\_GridComp} as it chooses.
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
!   Name of the newly-created {\tt ESMF\_GridComp}.  This name can be altered
!   from within the {\tt ESMF\_GridComp} code once the initialization routine
!   is called.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    type(ESMF_CompClass), pointer :: compclass       ! generic comp
    type(ESMF_GridComp)           :: gcomp
    integer :: localrc                               ! local error status

    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    ! Initialize the pointer to null.
    nullify(ESMF_GridCompCreate%compp)
    nullify(compclass)

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Allocate a new comp class
    allocate(compclass, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="compclass", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call Comp method
    call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_GRID, name, &
      configFile=configFile, config=config, clock=clock, petList=petList, &
      contextflag=contextflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) then
      deallocate(compclass)
      return
    endif

    gcomp%compp => compclass
    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(gcomp, ESMF_ID_COMPONENT%objectID)

    ! Set return values
    ESMF_GridCompCreate%compp => compclass

    ESMF_INIT_SET_CREATED(ESMF_GridCompCreate)

    ! deal with geom object arguments
    call ESMF_GridCompSet(ESMF_GridCompCreate, grid=grid, gridList=gridList, &
      mesh=mesh, meshList=meshList, locstream=locstream, &
      locstreamList=locstreamList, xgrid=xgrid, xgridList=xgridList, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_GridCompCreate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompDestroy"
!BOP
! !IROUTINE: ESMF_GridCompDestroy - Release resources associated with a GridComp
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompDestroy(gridcomp, keywordEnforcer, &
    timeout, timeoutFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)          :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in),   optional :: timeout
    logical,             intent(out),  optional :: timeoutFlag
    integer,             intent(out),  optional :: rc
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
! Destroys an {\tt ESMF\_GridComp}, releasing the resources associated
! with the object.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   Release all resources associated with this {\tt ESMF\_GridComp}
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
    integer :: localrc                       ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! Check to see if already destroyed
    if (.not.associated(gridcomp%compp)) then
      if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
        msg="GridComp not initialized or already destroyed", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(gridcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call Comp method
    call ESMF_CompDestruct(gridcomp%compp, timeout=timeout, &
      timeoutFlag=timeoutFlag, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! mark object invalid
    call ESMF_BaseSetStatus(gridcomp%compp%base, ESMF_STATUS_INVALID, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_INIT_SET_DELETED(gridcomp)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_GridCompDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompFinalize"
!BOP
! !IROUTINE: ESMF_GridCompFinalize - Call the GridComp's finalize routine
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompFinalize(gridcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
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
! an {\tt ESMF\_GridComp}.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   The {\tt ESMF\_GridComp} to call finalize routine for.
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
!   For single-phase child components this argument is optional. The default
!   is 1.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(gridcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_FINALIZEIC, &
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
  end subroutine ESMF_GridCompFinalize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompFinalizeAct"
!BOPI
! !IROUTINE: ESMF_GridCompFinalizeAct - Call the GridComp's finalize routine
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompFinalizeAct(gridcomp, importState, &
    exportState, clock, syncflag, phase, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !DESCRIPTION:
! Same as {\tt ESMF\_GridCompFinalize} but no redirection through the
! Interface Component method, instead directly call into the actual method.
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_FINALIZE, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, userRc=userRc, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompFinalizeAct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompGet"
!BOP
! !IROUTINE: ESMF_GridCompGet - Get GridComp information
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompGet(gridcomp, keywordEnforcer, &
    gridIsPresent, grid, gridList, meshIsPresent, mesh, meshList, &
    locstreamIsPresent, locstream, locstreamList, xgridIsPresent, &
    xgrid, xgridList, importStateIsPresent, importState, &
    exportStateIsPresent, exportState, configIsPresent, config, &
    configFileIsPresent, configFile, clockIsPresent, clock, localPet, &
    petCount, contextflag, currentMethod, currentPhase, comptype, &
    vmIsPresent, vm, name, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),           intent(in)            :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                       intent(out), optional :: gridIsPresent
    type(ESMF_Grid),               intent(out), optional :: grid
    type(ESMF_Grid), allocatable,  intent(out), optional :: gridList(:)
    logical,                       intent(out), optional :: meshIsPresent
    type(ESMF_Mesh),               intent(out), optional :: mesh
    type(ESMF_Mesh), allocatable,  intent(out), optional :: meshList(:)
    logical,                       intent(out), optional :: locstreamIsPresent
    type(ESMF_LocStream),          intent(out), optional :: locstream
    type(ESMF_LocStream), allocatable, intent(out), optional :: locstreamList(:)
    logical,                       intent(out), optional :: xgridIsPresent
    type(ESMF_XGrid),              intent(out), optional :: xgrid
    type(ESMF_XGrid), allocatable, intent(out), optional :: xgridList(:)
    logical,                       intent(out), optional :: importStateIsPresent
    type(ESMF_State),              intent(out), optional :: importState
    logical,                       intent(out), optional :: exportStateIsPresent
    type(ESMF_State),              intent(out), optional :: exportState
    logical,                       intent(out), optional :: configIsPresent
    type(ESMF_Config),             intent(out), optional :: config
    logical,                       intent(out), optional :: configFileIsPresent
    character(len=*),              intent(out), optional :: configFile
    logical,                       intent(out), optional :: clockIsPresent
    type(ESMF_Clock),              intent(out), optional :: clock
    integer,                       intent(out), optional :: localPet
    integer,                       intent(out), optional :: petCount
    type(ESMF_Context_Flag),       intent(out), optional :: contextflag
    type(ESMF_Method_Flag),        intent(out), optional :: currentMethod
    integer,                       intent(out), optional :: currentPhase
    type(ESMF_CompType_Flag),      intent(out), optional :: comptype
    logical,                       intent(out), optional :: vmIsPresent
    type(ESMF_VM),                 intent(out), optional :: vm
    character(len=*),              intent(out), optional :: name
    integer,                       intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \begin{sloppypar}
! \item[7.1.0r] Added arguments {\tt gridList}, {\tt meshIsPresent}, {\tt mesh},
!   {\tt meshList}, {\tt locstreamIsPresent}, {\tt locstream},
!   {\tt locstreamList}, {\tt xgridIsPresent}, {\tt xgrid}, and {\tt xgridList}.
!   These arguments add support for accessing references to multiple geom objects,
!   either of the same type, or different type, associated with the same
!   {\tt ESMF\_GridComp} object.
! \end{sloppypar}
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! Get information about an {\tt ESMF\_GridComp} object.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   The {\tt ESMF\_GridComp} object being queried.
! \item[{[gridIsPresent]}]
!   Set to {\tt .true.} if at least one {\tt ESMF\_Grid} object is
!   associated with the {\tt gridcomp} component.
!   Set to {\tt .false.} otherwise.
! \item[{[grid]}]
!   Return the {\tt ESMF\_Grid} object associated with the {\tt gridcomp}
!   component. If multiple {\tt ESMF\_Grid} objects are associated, return the
!   first in the list.
!   It is an error to query for {\tt grid} if no {\tt ESMF\_Grid} object is
!   associated with the {\tt gridcomp} component.
!   If unsure, query for {\tt gridIsPresent} first, or use the {\tt gridList}
!   variant.
! \item[{[gridList]}]
!   Return a list of all {\tt ESMF\_Grid} objects associated with the
!   {\tt gridcomp} component. The size of the returned {\tt gridList}
!   corresponds to the number of {\tt ESMF\_Grid} objects associated.
!   If no {\tt ESMF\_Grid} object is associated with the {\tt gridcomp}
!   component, the size of the returned {\tt gridList} is zero.
! \item[{[meshIsPresent]}]
!   Set to {\tt .true.} if at least one {\tt ESMF\_Mesh} object is
!   associated with the {\tt gridcomp} component.
!   Set to {\tt .false.} otherwise.
! \item[{[mesh]}]
!   Return the {\tt ESMF\_Mesh} object associated with the {\tt gridcomp}
!   component. If multiple {\tt ESMF\_Mesh} objects are associated, return the
!   first in the list.
!   It is an error to query for {\tt mesh} if no {\tt ESMF\_Mesh} object is
!   associated with the {\tt gridcomp} component.
!   If unsure, query for {\tt meshIsPresent} first, or use the {\tt meshList}
!   variant.
! \item[{[meshList]}]
!   Return a list of all {\tt ESMF\_Mesh} objects associated with the
!   {\tt gridcomp} component. The size of the returned {\tt meshList}
!   corresponds to the number of {\tt ESMF\_Mesh} objects associated.
!   If no {\tt ESMF\_Mesh} object is associated with the {\tt gridcomp}
!   component, the size of the returned {\tt meshList} is zero.
! \item[{[locstreamIsPresent]}]
!   Set to {\tt .true.} if at least one {\tt ESMF\_LocStream} object is
!   associated with the {\tt gridcomp} component.
!   Set to {\tt .false.} otherwise.
! \item[{[locstream]}]
! \begin{sloppypar}
!   Return the {\tt ESMF\_LocStream} object associated with the {\tt gridcomp}
!   component. If multiple {\tt ESMF\_LocStream} objects are associated, return
!   the first in the list.
!   It is an error to query for {\tt locstream} if no {\tt ESMF\_Grid} object is
!   associated with the {\tt gridcomp} component.
!   If unsure, query for {\tt locstreamIsPresent} first, or use the
!   {\tt locstreamList} variant.
! \end{sloppypar}
! \item[{[locstreamList]}]
!   Return a list of all {\tt ESMF\_LocStream} objects associated with the
!   {\tt gridcomp} component. The size of the returned {\tt locstreamList}
!   corresponds to the number of {\tt ESMF\_LocStream} objects associated.
!   If no {\tt ESMF\_LocStream} object is associated with the {\tt gridcomp}
!   component, the size of the returned {\tt locstreamList} is zero.
! \item[{[xgridIsPresent]}]
!   Set to {\tt .true.} if at least one {\tt ESMF\_XGrid} object is
!   associated with the {\tt gridcomp} component.
!   Set to {\tt .false.} otherwise.
! \item[{[xgrid]}]
!   Return the {\tt ESMF\_XGrid} object associated with the {\tt gridcomp}
!   component. If multiple {\tt ESMF\_XGrid} objects are associated, return the
!   first in the list.
!   It is an error to query for {\tt xgrid} if no {\tt ESMF\_XGrid} object is
!   associated with the {\tt gridcomp} component.
!   If unsure, query for {\tt xgridIsPresent} first, or use the {\tt xgridList}
!   variant.
! \item[{[xgridList]}]
!   Return a list of all {\tt ESMF\_XGrid} objects associated with the
!   {\tt gridcomp} component. The size of the returned {\tt xgridList}
!   corresponds to the number of {\tt ESMF\_XGrid} objects associated.
!   If no {\tt ESMF\_XGrid} object is associated with the {\tt gridcomp}
!   component, the size of the returned {\tt xgridList} is zero.
! \item[{[importStateIsPresent]}]
!   {\tt .true.} if {\tt importState} was set in GridComp object,
!   {\tt .false.} otherwise.
! \item[{[importState]}]
!   Return the associated import State.
!   It is an error to query for the import State if none is associated with
!   the GridComp. If unsure, get {\tt importStateIsPresent} first to determine
!   the status.
! \item[{[exportStateIsPresent]}]
!   {\tt .true.} if {\tt exportState} was set in GridComp object,
!   {\tt .false.} otherwise.
! \item[{[exportState]}]
!   Return the associated export State.
!   It is an error to query for the export State if none is associated with
!   the GridComp. If unsure, get {\tt exportStateIsPresent} first to determine
!   the status.
! \item[{[configIsPresent]}]
!   {\tt .true.} if {\tt config} was set in GridComp object,
!   {\tt .false.} otherwise.
! \item[{[config]}]
!   Return the associated Config.
!   It is an error to query for the Config if none is associated with
!   the GridComp. If unsure, get {\tt configIsPresent} first to determine
!   the status.
! \item[{[configFileIsPresent]}]
!   {\tt .true.} if {\tt configFile} was set in GridComp object,
!   {\tt .false.} otherwise.
! \item[{[configFile]}]
!   Return the associated configuration filename.
!   It is an error to query for the configuration filename if none is associated with
!   the GridComp. If unsure, get {\tt configFileIsPresent} first to determine
!   the status.
! \item[{[clockIsPresent]}]
!   {\tt .true.} if {\tt clock} was set in GridComp object,
!   {\tt .false.} otherwise.
! \item[{[clock]}]
!   Return the associated Clock.
!   It is an error to query for the Clock if none is associated with
!   the GridComp. If unsure, get {\tt clockIsPresent} first to determine
!   the status.
! \item[{[localPet]}]
!   Return the local PET id within the {\tt ESMF\_GridComp} object.
! \item[{[petCount]}]
!   Return the number of PETs in the the {\tt ESMF\_GridComp} object.
! \item[{[contextflag]}]
!   Return the {\tt ESMF\_Context\_Flag} for this {\tt ESMF\_GridComp}.
!   See section \ref{const:contextflag} for a complete list of valid flags.
! \item[{[currentMethod]}]
!   Return the current {\tt ESMF\_Method\_Flag} of the {\tt ESMF\_GridComp}
!   execution. See section \ref{const:method}  for a complete list of valid
!   options.
! \item[{[currentPhase]}]
!   Return the current {\tt phase} of the {\tt ESMF\_GridComp} execution.
! \item[{[comptype]}]
!   Return the Component type.
!   See section \ref{const:comptype} for a complete list of valid flags.
! \item[{[vmIsPresent]}]
!   {\tt .true.} if {\tt vm} was set in GridComp object,
!   {\tt .false.} otherwise.
! \item[{[vm]}]
!   Return the associated VM.
!   It is an error to query for the VM if none is associated with
!   the GridComp. If unsure, get {\tt vmIsPresent} first to determine
!   the status.
! \item[{[name]}]
!   Return the name of the {\tt ESMF\_GridComp}.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    call ESMF_CompGet(gridcomp%compp, name=name, vm=vm, contextflag=contextflag,&
      grid=grid, gridList=gridList, mesh=mesh, meshList=meshList, &
      locstream=locstream, locstreamList=locstreamList, &
      xgrid=xgrid, xgridList=xgridList, &
      importState=importState, exportState=exportState, clock=clock,&
      configFile=configFile, config=config, currentMethod=currentMethod, &
      currentPhase=currentPhase, localPet=localPet, petCount=petCount, &
      comptype=comptype, compStatus=compStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call Comp method
    call ESMF_CompStatusGet(compStatus, &
      clockIsPresent = clockIsPresent, &
      configIsPresent = configIsPresent, &
      configFileIsPresent = configFileIsPresent, &
      vmIsPresent = vmIsPresent, &
      isIsPresent = importStateIsPresent, &
      esIsPresent = exportStateIsPresent, &
      gridIsPresent = gridIsPresent, &
      meshIsPresent = meshIsPresent, &
      locstreamIsPresent = locstreamIsPresent, &
      xgridIsPresent = xgridIsPresent, &
      rc = localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompGetEPPhaseCount"
!BOPI
! !IROUTINE: ESMF_GridCompGetEPPhaseCount - Get number of phases of an entry point
!
! !INTERFACE:
  subroutine ESMF_GridCompGetEPPhaseCount(gridcomp, methodflag, phaseCount, &
    phaseZeroFlag, rc)

! !ARGUMENTS:
    type(ESMF_GridComp),    intent(in)            :: gridcomp
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
! \item[gridcomp]
!   An {\tt ESMF\_GridComp} object.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gridcomp, rc)

    call c_ESMC_GetEntryPointPhaseCount(gridcomp, methodflag, phaseCount, &
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
  end subroutine ESMF_GridCompGetEPPhaseCount
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompGetInternalState - Get private data block pointer
!
! !INTERFACE:
! subroutine ESMF_GridCompGetInternalState(gridcomp, wrappedDataPointer, rc)
!
! !ARGUMENTS:
!   type(ESMF_GridComp)             :: gridcomp
!   type(wrapper)                   :: wrappedDataPointer
!   integer,            intent(out) :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Available to be called by an {\tt ESMF\_GridComp} at any time after
! {\tt ESMF\_GridCompSetInternalState} has been called.
! Since init, run, and finalize must be separate subroutines, data that
! they need to share in common can either be module global data, or can
! be allocated in a private data block and the address of that block
! can be registered with the framework and retrieved by this call.
! When running multiple instantiations of an {\tt ESMF\_GridComp},
! for example during ensemble runs,
! it may be simpler to maintain private data specific to
! each run with private data blocks.  A corresponding
! {\tt ESMF\_GridCompSetInternalState} call sets the data pointer to
! this block, and this call retrieves the data pointer.
! Note that the {\tt wrappedDataPointer} argument needs to be a derived type
! which contains only a pointer of the type of the data block defined
! by the user.  When making this call the pointer needs to be unassociated.
! When the call returns, the pointer will now reference the original
! data block which was set during the previous call to
! {\tt ESMF\_GridCompSetInternalState}.
!
! Only the {\em last} data block set via
! {\tt ESMF\_GridCompSetInternalState} will be accessible.
!
! CAUTION: This method does not have an explicit Fortran interface. Do not
! specify argument keywords when calling this method!
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   An {\tt ESMF\_GridComp} object.
! \item[wrappedDataPointer]
!   A derived type (wrapper), containing only an unassociated pointer
!   to the private data block.
!   The framework will fill in the pointer. When this call returns, the
!   pointer is set to the same address set during the last
!   {\tt ESMF\_GridCompSetInternalState} call.
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
#define ESMF_METHOD "ESMF_GridCompInitialize"
!BOP
! !IROUTINE: ESMF_GridCompInitialize - Call the GridComp's initialize routine

! !INTERFACE:
  recursive subroutine ESMF_GridCompInitialize(gridcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
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
! an {\tt ESMF\_GridComp}.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to call initialize routine for.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(gridcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_INITIALIZEIC, &
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
  end subroutine ESMF_GridCompInitialize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompInitializeAct"
!BOPI
! !IROUTINE: ESMF_GridCompInitializeAct - Call the GridComp's initialize routine

! !INTERFACE:
  recursive subroutine ESMF_GridCompInitializeAct(gridcomp, importState, &
    exportState, clock, syncflag, phase, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !DESCRIPTION:
! Same as {\tt ESMF\_GridCompInitialize} but no redirection through the
! Interface Component method, instead directly call into the actual method.
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_INITIALIZE, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, userRc=userRc, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompInitializeAct
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompIsCreated()"
!BOP
! !IROUTINE: ESMF_GridCompIsCreated - Check whether a GridComp object has been created

! !INTERFACE:
  function ESMF_GridCompIsCreated(gridcomp, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_GridCompIsCreated
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt gridcomp} has been created. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[gridcomp]
!     {\tt ESMF\_GridComp} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ESMF_GridCompIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_GridCompGetInit(gridcomp)==ESMF_INIT_CREATED) &
      ESMF_GridCompIsCreated = .true.
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompIsPetLocal"
!BOP
! !IROUTINE: ESMF_GridCompIsPetLocal - Inquire if this GridComp is to execute on the calling PET
!
! !INTERFACE:
  recursive function ESMF_GridCompIsPetLocal(gridcomp, keywordEnforcer, rc)
!
! !RETURN VALUE:
    logical :: ESMF_GridCompIsPetLocal
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Inquire if this {\tt ESMF\_GridComp} object is to execute on the calling PET.
!
! The return value is {\tt .true.} if the component is to execute on the
! calling PET, {\tt .false.} otherwise.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} queried.
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
    ESMF_GridCompIsPetLocal = .false.

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    localresult = ESMF_CompIsPetLocal(gridcomp%compp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_GridCompIsPetLocal = localresult

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_GridCompIsPetLocal
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompPrint"
!BOP
! !IROUTINE:  ESMF_GridCompPrint - Print GridComp information
!
! !INTERFACE:
  subroutine ESMF_GridCompPrint(gridcomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Prints information about an {\tt ESMF\_GridComp} to {\tt stdout}. \\
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to print.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    write (ESMF_UtilIOStdout,*) "Gridded Component:"
    ! call Comp method
    call ESMF_CompPrint(gridcomp%compp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompPrint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompReadRestart"
!BOP
! !IROUTINE: ESMF_GridCompReadRestart - Call the GridComp's read restart routine
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompReadRestart(gridcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
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
! an {\tt ESMF\_GridComp}.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to call run routine for.
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

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(gridcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_READRESTART, &
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
  end subroutine ESMF_GridCompReadRestart
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompRun"
!BOP
! !IROUTINE: ESMF_GridCompRun - Call the GridComp's run routine
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompRun(gridcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
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
! an {\tt ESMF\_GridComp}.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to call run routine for.
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

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(gridcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_RUNIC, &
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
  end subroutine ESMF_GridCompRun
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompRunAct"
!BOPI
! !IROUTINE: ESMF_GridCompRunAct - Call the GridComp's run routine
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompRunAct(gridcomp, importState, exportState,&
    clock, syncflag, phase, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
    type(ESMF_State),     intent(inout), optional :: importState
    type(ESMF_State),     intent(inout), optional :: exportState
    type(ESMF_Clock),     intent(inout), optional :: clock
    type(ESMF_Sync_Flag), intent(in),    optional :: syncflag
    integer,              intent(in),    optional :: phase
    integer,              intent(out),   optional :: userRc
    integer,              intent(out),   optional :: rc
!
! !DESCRIPTION:
! Same as {\tt ESMF\_GridCompRun} but no redirection through the
! Interface Component method, instead directly call into the actual method.
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_RUN, &
      importState=importState, exportState=exportState, clock=clock, &
      syncflag=syncflag, phase=phase, userRc=userRc, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompRunAct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompServiceLoop"
!BOP
! !IROUTINE: ESMF_GridCompServiceLoop - Call the GridComp's service loop routine

! !INTERFACE:
  recursive subroutine ESMF_GridCompServiceLoop(gridcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, port, timeout, timeoutFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
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
! Call the ServiceLoop routine for an {\tt ESMF\_GridComp}.
! This tries to establish a "component tunnel" between the {\em actual}
! Component (calling this routine) and a {\tt dual} Component connecting to it
! through a matching SetServices call.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to call service loop routine for.
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
!   {\tt ESMF\_GridCompServiceLoop()}, starts to listen on the specified port
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    if (.not.present(port).and.(present(timeout))) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Currently the 'timeout' argument requires the 'port' argument", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail out
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_SERVICELOOP, &
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
  end subroutine ESMF_GridCompServiceLoop
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSet"
!BOP
! !IROUTINE: ESMF_GridCompSet - Set or reset information about the GridComp
!
! !INTERFACE:
  subroutine ESMF_GridCompSet(gridcomp, keywordEnforcer, grid, gridList, &
    mesh, meshList, locstream, locstreamList, xgrid, xgridList, &
    config, configFile, clock, name, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),    intent(inout)         :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Grid),        intent(in),  optional :: grid
    type(ESMF_Grid),        intent(in),  optional :: gridList(:)
    type(ESMF_Mesh),        intent(in),  optional :: mesh
    type(ESMF_Mesh),        intent(in),  optional :: meshList(:)
    type(ESMF_LocStream),   intent(in),  optional :: locstream
    type(ESMF_LocStream),   intent(in),  optional :: locstreamList(:)
    type(ESMF_XGrid),       intent(in),  optional :: xgrid
    type(ESMF_XGrid),       intent(in),  optional :: xgridList(:)
    type(ESMF_Config),      intent(in),  optional :: config
    character(len=*),       intent(in),  optional :: configFile
    type(ESMF_Clock),       intent(in),  optional :: clock
    character(len=*),       intent(in),  optional :: name
    integer,                intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \begin{sloppypar}
! \item[7.1.0r] Added arguments {\tt gridList}, {\tt mesh}, {\tt meshList},
!   {\tt locstream}, {\tt locstreamList}, {\tt xgrid}, and {\tt xgridList}.
!   These arguments add support for holding references to multiple geom objects,
!   either of the same type, or different type, in the same
!   {\tt ESMF\_GridComp} object.
! \end{sloppypar}
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
! Sets or resets information about an {\tt ESMF\_GridComp}.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to change.
! \item[{[grid]}]
!   Associate an {\tt ESMF\_Grid} object with the {\tt gridcomp} component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt grid} object.
!   The {\tt grid} argument is mutually exclusive with the {\tt gridList}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt grid} nor {\tt gridList} are provided,
!   the {\tt ESMF\_Grid} association of the incoming {\tt gridcomp}
!   component remains unchanged.
! \item[{[gridList]}]
!   Associate a list of {\tt ESMF\_Grid} objects with the {\tt gridcomp}
!   component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt gridList} object.
!   The {\tt gridList} argument is mutually exclusive with the {\tt grid}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt grid} nor {\tt gridList} are provided,
!   the {\tt ESMF\_Grid} association of the incoming {\tt gridcomp}
!   component remains unchanged.
! \item[{[mesh]}]
!   Associate an {\tt ESMF\_Mesh} object with the {\tt gridcomp} component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt mesh} object.
!   The {\tt mesh} argument is mutually exclusive with the {\tt meshList}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt mesh} nor {\tt meshList} are provided,
!   the {\tt ESMF\_Mesh} association of the incoming {\tt gridcomp}
!   component remains unchanged.
! \item[{[meshList]}]
!   Associate a list of {\tt ESMF\_Mesh} objects with the {\tt gridcomp}
!   component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt meshList} object.
!   The {\tt meshList} argument is mutually exclusive with the {\tt mesh}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt mesh} nor {\tt meshList} are provided,
!   the {\tt ESMF\_Mesh} association of the incoming {\tt gridcomp}
!   component remains unchanged.
! \item[{[locstream]}]
!   Associate an {\tt ESMF\_LocStream} object with the {\tt gridcomp} component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt locstream} object.
!   The {\tt locstream} argument is mutually exclusive with the
!   {\tt locstreamList}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt locstream} nor {\tt locstreamList} are
!   provided, the {\tt ESMF\_LocStream} association of the incoming
!   {\tt gridcomp} component remains unchanged.
! \item[{[locstreamList]}]
!   Associate a list of {\tt ESMF\_LocStream} objects with the {\tt gridcomp}
!   component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt locstreamList} object.
!   The {\tt locstreamList} argument is mutually exclusive with the
!   {\tt locstream}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt locstream} nor {\tt locstreamList} are
!   provided, the {\tt ESMF\_LocStream} association of the incoming
!   {\tt gridcomp} component remains unchanged.
! \item[{[xgrid]}]
!   Associate an {\tt ESMF\_XGrid} object with the {\tt gridcomp} component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt xgrid} object.
!   The {\tt xgrid} argument is mutually exclusive with the {\tt xgridList}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt xgrid} nor {\tt xgridList} are provided,
!   the {\tt ESMF\_XGrid} association of the incoming {\tt gridcomp}
!   component remains unchanged.
! \item[{[xgridList]}]
!   Associate a list of {\tt ESMF\_XGrid} objects with the {\tt gridcomp}
!   component.
!   This is simply a convenience feature for the user. The ESMF library code
!   does not access the {\tt xgridList} object.
!   The {\tt xgridList} argument is mutually exclusive with the {\tt xgrid}
!   argument. If both arguments are provided, the routine will fail, and an
!   error is returned in {\tt rc}.
!   By default, i.e. if neither {\tt xgrid} nor {\tt xgridList} are provided,
!   the {\tt ESMF\_XGrid} association of the incoming {\tt gridcomp}
!   component remains unchanged.
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
!   Set the private clock for this {\tt ESMF\_GridComp}.
! \item[{[name]}]
!   Set the name of the {\tt ESMF\_GridComp}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                         :: localrc           ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    ! call Comp method
    call ESMF_CompSet(gridcomp%compp, name=name, &
      grid=grid, gridList=gridList, mesh=mesh, meshList=meshList, &
      locstream=locstream, locstreamList=locstreamList, xgrid=xgrid, &
      xgridList=xgridList, clock=clock, configFile=configFile, config=config, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetEntryPoint"
!BOP
! !IROUTINE: ESMF_GridCompSetEntryPoint - Set user routine as entry point for standard GridComp method
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompSetEntryPoint(gridcomp, methodflag, &
    userRoutine, keywordEnforcer, phase, rc)

! !ARGUMENTS:
    type(ESMF_GridComp),    intent(inout)         :: gridcomp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
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
! \item[gridcomp]
!   An {\tt ESMF\_GridComp} object.
! \item[methodflag]
!   \begin{sloppypar}
!   One of a set of predefined Component methods - e.g.
!   {\tt ESMF\_METHOD\_INITIALIZE}, {\tt ESMF\_METHOD\_RUN},
!   {\tt ESMF\_METHOD\_FINALIZE}. See section \ref{const:method}
!   for a complete list of valid method options.
!   \end{sloppypar}
! \item[userRoutine]
!   The user-supplied subroutine to be associated for this Component
!   {\tt method}.  Argument types, intent and order must match
!   the interface signature, and must not have the {\tt optional} attribute.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gridcomp, rc)

    phaseArg = 1   ! default
    if (present(phase)) phaseArg = phase

    call c_ESMC_SetEntryPoint(gridcomp, methodflag, userRoutine, phaseArg, &
      localrc)
!TODO: back in once thread-safe    if (ESMF_LogFoundError(localrc, &
!TODO: back in once thread-safe      ESMF_ERR_PASSTHRU, &
!TODO: back in once thread-safe      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetEntryPoint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCompSetInternalState - Set private data block pointer
!
! !INTERFACE:
! subroutine ESMF_GridCompSetInternalState(gridcomp, wrappedDataPointer, rc)
!
! !ARGUMENTS:
!   type(ESMF_GridComp)             :: gridcomp
!   type(wrapper)                   :: wrappedDataPointer
!   integer,            intent(out) :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Available to be called by an {\tt ESMF\_GridComp} at any time, but
! expected to be
! most useful when called during the registration process, or initialization.
! Since init, run, and finalize must be separate subroutines, data that
! they need to share in common can either be module global data, or can
! be allocated in a private data block and the address of that block
! can be registered with the framework and retrieved by subsequent calls.
! When running multiple instantiations of an {\tt ESMF\_GridComp},
! for example during
! ensemble runs, it may be simpler to maintain private data specific to
! each run with private data blocks.  A corresponding
! {\tt ESMF\_GridCompGetInternalState} call retrieves the data pointer.
!
! Only the {\em last} data block set via
! {\tt ESMF\_GridCompSetInternalState} will be accessible.
!
! CAUTION: This method does not have an explicit Fortran interface. Do not
! specify argument keywords when calling this method!
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   An {\tt ESMF\_GridComp} object.
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
#define ESMF_METHOD "ESMF_GridCompSetServices"
!BOP
! !IROUTINE: ESMF_GridCompSetServices - Call user routine to register GridComp methods
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompSetServices(gridcomp, &
    userRoutine, keywordEnforcer, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
    interface
      subroutine userRoutine(gridcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: userRc
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! \label{GridComp:SetServices}
! Call into user provided {\tt userRoutine} which is responsible for
! setting Component's Initialize(), Run(), and Finalize() services.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   Gridded Component.
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
!   \begin{sloppypar}
!   The {\tt userRoutine}, when called by the framework, must make successive calls
!   to {\tt ESMF\_GridCompSetEntryPoint()} to preset callback routines for
!   standard Component Initialize(), Run(), and Finalize() methods.
!   \end{sloppypar}
! \item[{[userRc]}]
!   Return code set by {\tt userRoutine} before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gridcomp, rc)

    call c_ESMC_SetServices(gridcomp, userRoutine, localUserRc, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! now indicate that this Component has a VM associated
    gridcomp%compp%compStatus%vmIsPresent = .true.

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetServices
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetServicesShObj"
!BOP
! !IROUTINE: ESMF_GridCompSetServices - Call user routine through name lookup, to register GridComp methods
!
! !INTERFACE:
  ! Private name; call using ESMF_GridCompSetServices()
  recursive subroutine ESMF_GridCompSetServicesShObj(gridcomp, userRoutine, &
    keywordEnforcer, sharedObj, userRoutineFound, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
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
! \label{GridComp:SetServicesShObj}
! Call into a user provided routine which is responsible for setting
! Component's Initialize(), Run(), and Finalize() services. The named
! {\tt userRoutine} must exist in the executable, or in the shared object
! specified by {\tt sharedObj}. In the latter case all of the platform
! specific details about dynamic linking and loading apply.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   Gridded Component.
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
!       subroutine userRoutine(gridcomp, rc)
!         type(ESMF_GridComp)  :: gridcomp   ! must not be optional
!         integer, intent(out) :: rc         ! must not be optional
!       end subroutine
!     end interface
!
!   !DESCRIPTION:
!   \begin{sloppypar}
!   The {\tt userRoutine}, when called by the framework, must make successive calls
!   to {\tt ESMF\_GridCompSetEntryPoint()} to preset callback routines for
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gridcomp, rc)

    if (present(sharedObj)) then
      call c_ESMC_SetServicesShObj(gridcomp, userRoutine, sharedObj, &
        userRoutineFoundHelp, localUserRc, localrc)
    else
      call c_ESMC_SetServicesShObj(gridcomp, userRoutine, emptyString, &
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
      gridcomp%compp%compStatus%vmIsPresent = .true.
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
  end subroutine ESMF_GridCompSetServicesShObj
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetServicesComp"
!BOP
! !IROUTINE: ESMF_GridCompSetServices - Set to serve as Dual Component for an Actual Component
!
! !INTERFACE:
  ! Private name; call using ESMF_GridCompSetServices()
  recursive subroutine ESMF_GridCompSetServicesComp(gridcomp, &
    actualGridcomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
    type(ESMF_GridComp), intent(in)            :: actualGridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Set the services of a Gridded Component to serve a "dual" Component for an
! "actual" Component. The component tunnel is VM based.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   Dual Gridded Component.
! \item[actualGridcomp]
!   Actual Gridded Component.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gridcomp, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, actualGridcomp, rc)

    ! access the petList of the actualGridcomp and find the lowest PET
    ! -> this is going to be the rendezvous PET for the component tunnel setup
    nullify(actualCompPetList)
    ! call Comp method
    call ESMF_CompGet(actualGridcomp%compp, petList=actualCompPetList, &
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

    call c_ESMC_SetServicesComp(gridcomp, gridcomp%compp%compTunnel, &
      actualGridcomp, actualCompRootPet, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! now indicate that this Component has a VM associated
    gridcomp%compp%compStatus%vmIsPresent = .true.

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetServicesComp
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetServicesSock"
!BOP
! !IROUTINE: ESMF_GridCompSetServices - Set to serve as Dual Component for an Actual Component through sockets
!
! !INTERFACE:
  ! Private name; call using ESMF_GridCompSetServices()
  recursive subroutine ESMF_GridCompSetServicesSock(gridcomp, port, &
    keywordEnforcer, server, timeout, timeoutFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
    integer,             intent(in)            :: port
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),    intent(in),  optional :: server
    integer,             intent(in),  optional :: timeout
    logical,             intent(out), optional :: timeoutFlag
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Set the services of a Gridded Component to serve a "dual" Component for an
! "actual" Component. The component tunnel is socket based.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   Dual Gridded Component.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gridcomp, rc)

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    if (present(server)) then
      call c_ESMC_SetServicesSock(gridcomp, gridcomp%compp%compTunnel, &
        port, server, timeoutArg, localrc)
    else
      call c_ESMC_SetServicesSock(gridcomp, gridcomp%compp%compTunnel, &
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
    gridcomp%compp%compStatus%vmIsPresent = .true.

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetServicesSock
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetVM"
!BOP
! !IROUTINE: ESMF_GridCompSetVM - Call user routine to set GridComp VM properties
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompSetVM(gridcomp, userRoutine, keywordEnforcer, &
    userRc, rc)
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
    interface
      subroutine userRoutine(gridcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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
! for setting Component's VM properties.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   Gridded Component.
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
!   {\tt ESMF\_GridCompSetVMxxx()} methods to set the properties of the VM
!   associated with the Gridded Component.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gridcomp, rc)

    call c_ESMC_SetVM(gridcomp, userRoutine, localUserRc, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetVM
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetVMShObj"
!BOP
! !IROUTINE: ESMF_GridCompSetVM - Call user routine through name lookup, to set GridComp VM properties
!
! !INTERFACE:
  ! Private name; call using ESMF_GridCompSetVM()
  recursive subroutine ESMF_GridCompSetVMShObj(gridcomp, userRoutine, &
    keywordEnforcer, sharedObj, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
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
! \item[gridcomp]
!   Gridded Component.
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
!       subroutine userRoutine(gridcomp, rc)
!         type(ESMF_GridComp)  :: gridcomp    ! must not be optional
!         integer, intent(out) :: rc          ! must not be optional
!       end subroutine
!     end interface
!
!   !DESCRIPTION:
!   The subroutine, when called by the framework, is expected to use any of the
!   {\tt ESMF\_GridCompSetVMxxx()} methods to set the properties of the VM
!   associated with the Gridded Component.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gridcomp, rc)

    if (present(sharedObj)) then
      call c_ESMC_SetVMShObj(gridcomp, userRoutine, sharedObj, localUserRc, &
        localrc)
    else
      call c_ESMC_SetVMShObj(gridcomp, userRoutine, emptyString, localUserRc, &
        localrc)
    endif
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetVMShObj
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetVMMaxPEs"
!BOP
! !IROUTINE: ESMF_GridCompSetVMMaxPEs - Associate PEs with PETs in GridComp VM
!
! !INTERFACE:
  subroutine ESMF_GridCompSetVMMaxPEs(gridcomp, keywordEnforcer, &
    maxPeCountPerPet, prefIntraProcess, prefIntraSsi, prefInterSsi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in),  optional :: maxPeCountPerPet
    integer,             intent(in),  optional :: prefIntraProcess
    integer,             intent(in),  optional :: prefIntraSsi
    integer,             intent(in),  optional :: prefInterSsi
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_GridComp}.
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
!   The typical use of {\tt ESMF\_GridCompSetVMMaxPEs()} is to allocate
!   multiple PEs per PET in a Component for user-level threading, e.g. OpenMP.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to set the {\tt ESMF\_VM} for.
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
    integer :: localrc                     ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    call ESMF_CompSetVMMaxPEs(gridcomp%compp, maxPeCountPerPet, &
      prefIntraProcess, prefIntraSsi, prefInterSsi, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetVMMaxPEs
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetVMMaxThreads"
!BOP
! !IROUTINE: ESMF_GridCompSetVMMaxThreads - Set multi-threaded PETs in GridComp VM
!
! !INTERFACE:
  subroutine ESMF_GridCompSetVMMaxThreads(gridcomp, keywordEnforcer, &
    maxPetCountPerVas, prefIntraProcess, prefIntraSsi, prefInterSsi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in),  optional :: maxPetCountPerVas
    integer,             intent(in),  optional :: prefIntraProcess
    integer,             intent(in),  optional :: prefIntraSsi
    integer,             intent(in),  optional :: prefInterSsi
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_GridComp}.
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
!   The typical use of {\tt ESMF\_GridCompSetVMMaxThreads()} is to run a
!   Component multi-threaded with groups of PETs executing within a common
!   virtual address space.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to set the {\tt ESMF\_VM} for.
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
    integer :: localrc                     ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    call ESMF_CompSetVMMaxThreads(gridcomp%compp, maxPetCountPerVas, &
      prefIntraProcess, prefIntraSsi, prefInterSsi, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetVMMaxThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompSetVMMinThreads"
!BOP
! !IROUTINE: ESMF_GridCompSetVMMinThreads - Set a reduced threading level in GridComp VM
!
! !INTERFACE:
  subroutine ESMF_GridCompSetVMMinThreads(gridcomp, keywordEnforcer, &
    maxPeCountPerPet, prefIntraProcess, prefIntraSsi, prefInterSsi, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)         :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in),  optional :: maxPeCountPerPet
    integer,             intent(in),  optional :: prefIntraProcess
    integer,             intent(in),  optional :: prefIntraSsi
    integer,             intent(in),  optional :: prefInterSsi
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set characteristics of the {\tt ESMF\_VM} for this {\tt ESMF\_GridComp}.
!   Reduces the number of threaded PETs in each VAS. The {\tt max} argument
!   may be specified to limit the maximum number of PEs that a single PET
!   can be associated with.
!
!   Several constraints apply: 1) the number of PEs cannot change, 2) PEs
!   cannot migrate between single system images (SSIs), 3) the number of PETs
!   cannot increase, only decrease, 4) PETs cannot migrate between virtual
!   address spaces (VASs), nor can VASs migrate between SSIs.
!
!   The typical use of {\tt ESMF\_GridCompSetVMMinThreads()} is to run a
!   Component across a set of single-threaded PETs.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to set the {\tt ESMF\_VM} for.
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
    integer :: localrc                     ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    call ESMF_CompSetVMMinThreads(gridcomp%compp, maxPeCountPerPet, &
      prefIntraProcess, prefIntraSsi, prefInterSsi, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompSetVMMinThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompValidate"
!BOP
! !IROUTINE: ESMF_GridCompValidate - Check validity of a GridComp
!
! !INTERFACE:
  subroutine ESMF_GridCompValidate(gridcomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gridcomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Currently all this method does is to check that the {\tt gridcomp}
! was created.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to validate.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! call Comp method
    call ESMF_CompValidate(gridcomp%compp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_GridCompValidate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompWait"
!BOP
! !IROUTINE: ESMF_GridCompWait - Wait for a GridComp to return
!
! !INTERFACE:
  subroutine ESMF_GridCompWait(gridcomp, keywordEnforcer, syncflag, &
    timeout, timeoutFlag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)         :: gridcomp
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
! When executing asynchronously, wait for an {\tt ESMF\_GridComp} to return.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to wait for.
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
    integer :: localrc                     ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(gridcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call Comp method
    call ESMF_CompWait(gridcomp%compp, syncflag=syncflag, timeout=timeout, &
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
  end subroutine ESMF_GridCompWait
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompWriteRestart"
!BOP
! !IROUTINE: ESMF_GridCompWriteRestart - Call the GridComp's write restart routine
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompWriteRestart(gridcomp, keywordEnforcer, &
    importState, exportState, clock, syncflag, phase, timeout, timeoutFlag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),  intent(inout)           :: gridcomp
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
! an {\tt ESMF\_GridComp}.
!
! The arguments are:
! \begin{description}
! \item[gridcomp]
!   {\tt ESMF\_GridComp} to call run routine for.
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

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit,gridcomp,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(gridcomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    timeoutArg = ESMF_DEFAULT_TIMEOUT ! default 1h
    if (present(timeout)) timeoutArg = timeout

    ! call Comp method
    call ESMF_CompExecute(gridcomp%compp, method=ESMF_METHOD_WRITERESTART, &
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
  end subroutine ESMF_GridCompWriteRestart
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompGetInit"
!BOPI
! !IROUTINE:  ESMF_GridCompGetInit - Get initialization status.

! !INTERFACE:
  recursive function ESMF_GridCompGetInit(d) result (GridCompGetInit)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: GridCompGetInit
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in), optional :: d
!
! !DESCRIPTION:
! Get the initialization status of the Deep class {\tt GridComp}.
!
! The arguments are:
! \begin{description}
! \item[d]
!   {\tt ESMF\_GridComp} from which to retrieve status.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(d)) then
      GridCompGetInit = ESMF_INIT_GET(d)
    else
      GridCompGetInit = ESMF_INIT_CREATED
    endif
  end function ESMF_GridCompGetInit
!------------------------------------------------------------------------------

end module ESMF_GridCompMod
