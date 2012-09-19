! $Id: ESMF_SciComp.F90,v 1.2 2012/09/19 20:35:46 ksaint Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_SciComp.F90"
!==============================================================================
!
! ESMF Science Component module
module ESMF_SciCompMod
!
!==============================================================================
!
! This file contains the Science Component class definition and all 
!  Science Component class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_SciCompMod - Science Component class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt ESMF\_SciComp} class and associated functions and subroutines.  
!
!
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_BaseMod
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

  public ESMF_SciCompCreate
  public ESMF_SciCompDestroy
  public ESMF_SciCompGet
  public ESMF_SciCompPrint
  public ESMF_SciCompSet
  public ESMF_SciCompValidate
  public ESMF_SciCompWait
  
! - ESMF-internal methods:
  public ESMF_SciCompGetInit

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_SciComp.F90,v 1.2 2012/09/19 20:35:46 ksaint Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!===============================================================================
! SciCompOperator() interfaces
!===============================================================================


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_SciCompOperator(==) - SciComp equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (scicomp1 == scicomp2) then ... endif
!             OR
!   result = (scicomp1 == scicomp2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_SciComp), intent(in) :: scicomp1
!   type(ESMF_SciComp), intent(in) :: scicomp2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether scicomp1 and scicomp2 are valid aliases to the same ESMF
!   SciComp object in memory. For a more general comparison of two ESMF 
!   SciComps, going beyond the simple alias test, the ESMF\_SciCompMatch() 
!   function (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[scicomp1]
!     The {\tt ESMF\_SciComp} object on the left hand side of the equality
!     operation.
!   \item[scicomp2]
!     The {\tt ESMF\_SciComp} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_SciCompEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_SciCompOperator(/=) - SciComp not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (scicomp1 /= scicomp2) then ... endif
!             OR
!   result = (scicomp1 /= scicomp2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_SciComp), intent(in) :: scicomp1
!   type(ESMF_SciComp), intent(in) :: scicomp2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether scicomp1 and scicomp2 are {\it not} valid aliases to the
!   same ESMF SciComp object in memory. For a more general comparison of two 
!   ESMF SciComps, going beyond the simple alias test, the ESMF\_SciCompMatch()
!   function (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[scicomp1]
!     The {\tt ESMF\_SciComp} object on the left hand side of the non-equality
!     operation.
!   \item[scicomp2]
!     The {\tt ESMF\_SciComp} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_SciCompNE

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
#define ESMF_METHOD "ESMF_SciCompEQ()"
!BOPI
! !IROUTINE:  ESMF_SciCompEQ - Compare two SciComps for equality
!
! !INTERFACE:
  function ESMF_SciCompEQ(scicomp1, scicomp2)
! 
! !RETURN VALUE:
    logical :: ESMF_SciCompEQ

! !ARGUMENTS:
    type(ESMF_SciComp), intent(in) :: scicomp1
    type(ESMF_SciComp), intent(in) :: scicomp2

!
! !DESCRIPTION:
!   Test if both {\tt scicomp1} and {\tt scicomp2} alias the same ESMF SciComp 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE scinit1, scinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    scinit1 = ESMF_SciCompGetInit(scicomp1)
    scinit2 = ESMF_SciCompGetInit(scicomp2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (scinit1 .eq. ESMF_INIT_CREATED .and. &
      scinit2 .eq. ESMF_INIT_CREATED) then
      ESMF_SciCompEQ = associated(scicomp1%compp,scicomp2%compp)
    else
      ESMF_SciCompEQ = ESMF_FALSE
    endif

  end function ESMF_SciCompEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SciCompNE()"
!BOPI
! !IROUTINE:  ESMF_SciCompNE - Compare two SciComps for non-equality
!
! !INTERFACE:
  function ESMF_SciCompNE(scicomp1, scicomp2)
! 
! !RETURN VALUE:
    logical :: ESMF_SciCompNE

! !ARGUMENTS:
    type(ESMF_SciComp), intent(in) :: scicomp1
    type(ESMF_SciComp), intent(in) :: scicomp2

!
! !DESCRIPTION:
!   Test if both {\tt scicomp1} and {\tt scicomp2} alias the same ESMF SciComp 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE scinit1, scinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).
    
    ESMF_SciCompNE = .not.ESMF_SciCompEQ(scicomp1, scicomp2)

  end function ESMF_SciCompNE
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SciCompCreate"
!BOP
! !IROUTINE: ESMF_SciCompCreate - Create a SciComp
!
! !INTERFACE:
  recursive function ESMF_SciCompCreate(keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_SciComp) :: ESMF_SciCompCreate
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),        intent(in),    optional :: name
    integer,                 intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! This interface creates an {\tt ESMF\_SciComp} object. By default, a
! separate VM context will be created for each component.  This implies
! creating a new MPI communicator and allocating additional memory to
! manage the VM resources. When running on a large number of processors,
! creating a separate VM for each component could be both time and memory
! inefficient.  If the application is sequential, i.e., each component is
! running on all the PETs of the global VM, it will be more efficient to use
! the global VM instead of creating a new one.  This can be done by setting
! {\tt contextflag} to ESMF\_CONTEXT\_PARENT\_VM.
!
! The return value is the new {\tt ESMF\_SciComp}.
!   
! The arguments are:
! \begin{description}
! \item[{[name]}]
!   Name of the newly-created {\tt ESMF\_SciComp}.  This name can be altered
!   from within the {\tt ESMF\_SciComp} code once the initialization routine
!   is called.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    type(ESMF_CompClass), pointer :: compclass       ! generic comp
    type(ESMF_SciComp)            :: scomp
    integer                       :: localrc         ! local error status

    ! Initialize the pointer to null.
    nullify(ESMF_SciCompCreate%compp)
    nullify(compclass)

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Allocate a new comp class
    allocate(compclass, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="compclass", &
      ESMF_CONTEXT, rcTOReturn=rc)) return
      
    ! call Comp method
    call ESMF_CompConstruct(compclass, ESMF_COMPTYPE_SCI, name, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcTOReturn=rc)) then
      deallocate(compclass)
      return
    endif

    scomp%compp => compclass
    ! Add reference to this object into ESMF garbage collection table
    call c_ESMC_VMAddFObject(scomp, ESMF_ID_COMPONENT%objectID)
      
    ! Set return values
    ESMF_SciCompCreate%compp => compclass
    
    ESMF_INIT_SET_CREATED(ESMF_SciCompCreate)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_SciCompCreate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SciCompDestroy"
!BOP
! !IROUTINE: ESMF_SciCompDestroy - Release resources associated with a SciComp
!
! !INTERFACE:
  subroutine ESMF_SciCompDestroy(scicomp, keywordEnforcer, &
    timeout, timeoutFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp), intent(inout)           :: scicomp
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
! Destroys an {\tt ESMF\_SciComp}, releasing the resources associated
! with the object.
!
! The arguments are:
! \begin{description}
! \item[scicomp]
!   Release all resources associated with this {\tt ESMF\_SciComp}
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

    ESMF_INIT_CHECK_DEEP(ESMF_SciCompGetInit,scicomp,rc)

    ! Check to see if already destroyed
    if (.not.associated(scicomp%compp)) then
      if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
        msg="SciComp not initialized or already destroyed", &
        ESMF_CONTEXT, rcTOReturn=rc)) return
    endif

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(scicomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcTOReturn=rc)
      return
    endif

    ! call Comp method
    call ESMF_CompDestruct(scicomp%compp, timeout=timeout, &
      timeoutFlag=timeoutFlag, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcTOReturn=rc)) return

    ! mark object invalid
    call ESMF_BaseSetStatus(scicomp%compp%base, ESMF_STATUS_INVALID, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcTOReturn=rc)) return

    ESMF_INIT_SET_DELETED(scicomp)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_SciCompDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SciCompGet"
!BOP
! !IROUTINE: ESMF_SciCompGet - Get SciComp information
!
! !INTERFACE:
  subroutine ESMF_SciCompGet(scicomp, keywordEnforcer, comptype, name, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp),       intent(in)            :: scicomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_CompType_Flag), intent(out), optional :: comptype
    character(len=*),         intent(out), optional :: name
    integer,                  intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Get information about an {\tt ESMF\_SciComp} object.
!  
! The arguments are:
! \begin{description}
! \item[scicomp]
!   The {\tt ESMF\_SciComp} object being queried.
! \item[{[comptype]}]
!   Return the Component type: {\tt ESMF\_COMPTYPE\_GRID}, 
!   {\tt ESMF\_COMPTYPE\_CPL} or {\tt ESMF\_COMPTYPE\_SCI}.
! \item[{[name]}]
!   Return the name of the {\tt ESMF\_SciComp}.
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

    ESMF_INIT_CHECK_DEEP(ESMF_SciCompGetInit,scicomp,rc)

    ! call Comp method
    call ESMF_CompGet(scicomp%compp, name=name, &
      comptype=comptype, compStatus=compStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call Comp method
    call ESMF_CompStatusGet(compStatus, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_SciCompGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_SciCompGetInternalState - Get private data block pointer
!
! !INTERFACE:
! subroutine ESMF_SciCompGetInternalState(scicomp, wrappedDataPointer, rc)
!
! !ARGUMENTS:
!   type(ESMF_SciComp)              :: scicomp
!   type(wrapper)                   :: wrappedDataPointer
!   integer,            intent(out) :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Available to be called by an {\tt ESMF\_SciComp} at any time after 
! {\tt ESMF\_SciCompSetInternalState} has been called.
! When running multiple instantiations of an {\tt ESMF\_SciComp}, 
! for example during ensemble runs, 
! it may be simpler to maintain private data specific to 
! each run with private data blocks.  A corresponding 
! {\tt ESMF\_SciCompSetInternalState} call sets the data pointer to 
! this block, and this call retrieves the data pointer.
! Note that the {\tt wrappedDataPointer} argument needs to be a derived type
! which contains only a pointer of the type of the data block defined
! by the user.  When making this call the pointer needs to be unassociated.
! When the call returns, the pointer will now reference the original
! data block which was set during the previous call to
! {\tt ESMF\_SciCompSetInternalState}.
!
! Only the {\em last} data block set via
! {\tt ESMF\_SciCompSetInternalState} will be accessible.
!
! CAUTION: This method does not have an explicit Fortran interface. Do not 
! specify argument keywords when calling this method!
!   
! The arguments are:
! \begin{description}
! \item[scicomp]
!   An {\tt ESMF\_SciComp} object.
! \item[wrappedDataPointer]
!   A derived type (wrapper), containing only an unassociated pointer 
!   to the private data block.
!   The framework will fill in the pointer. When this call returns, the
!   pointer is set to the same address set during the last
!   {\tt ESMF\_SciCompSetInternalState} call.
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
#define ESMF_METHOD "ESMF_SciCompPrint"
!BOP
! !IROUTINE:  ESMF_SciCompPrint - Print SciComp information
!
! !INTERFACE:
  subroutine ESMF_SciCompPrint(scicomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp), intent(in)             :: scicomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Prints information about an {\tt ESMF\_SciComp} to {\tt stdout}. \\
!
! The arguments are:
! \begin{description}
! \item[scicomp]
!   {\tt ESMF\_SciComp} to print.
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

    ESMF_INIT_CHECK_DEEP(ESMF_SciCompGetInit,scicomp,rc)

    write (ESMF_UtilIOStdout,*) "Science Component:"
    ! call Comp method
    call ESMF_CompPrint(scicomp%compp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_SciCompPrint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SciCompSet"
!BOP
! !IROUTINE: ESMF_SciCompSet - Set or reset information about the SciComp
!
! !INTERFACE:
  subroutine ESMF_SciCompSet(scicomp, keywordEnforcer, name, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp), intent(inout)          :: scicomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),    intent(in),  optional :: name
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Sets or resets information about an {\tt ESMF\_SciComp}.
!
! The arguments are:
! \begin{description}
! \item[scicomp]
!   {\tt ESMF\_SciComp} to change.
! \item[{[name]}]
!   Set the name of the {\tt ESMF\_SciComp}.
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

    ESMF_INIT_CHECK_DEEP(ESMF_SciCompGetInit,scicomp,rc)

    ! call Comp method
    call ESMF_CompSet(scicomp%compp, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_SciCompSet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_SciCompSetInternalState - Set private data block pointer
!
! !INTERFACE:
! subroutine ESMF_SciCompSetInternalState(scicomp, wrappedDataPointer, rc)
!
! !ARGUMENTS:
!   type(ESMF_SciComp)              :: scicomp
!   type(wrapper)                   :: wrappedDataPointer
!   integer,            intent(out) :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Available to be called by an {\tt ESMF\_SciComp} at any time.
! When running multiple instantiations of an {\tt ESMF\_SciComp}, 
! for example during
! ensemble runs, it may be simpler to maintain private data specific to 
! each run with private data blocks.  A corresponding 
! {\tt ESMF\_SciCompGetInternalState} call retrieves the data pointer.
!   
! Only the {\em last} data block set via
! {\tt ESMF\_SciCompSetInternalState} will be accessible.
!
! CAUTION: This method does not have an explicit Fortran interface. Do not 
! specify argument keywords when calling this method!
!   
! The arguments are:
! \begin{description}
! \item[scicomp]
!   An {\tt ESMF\_SciComp} object.
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
#define ESMF_METHOD "ESMF_SciCompValidate"
!BOP
! !IROUTINE: ESMF_SciCompValidate - Check validity of a SciComp
!
! !INTERFACE:
  subroutine ESMF_SciCompValidate(scicomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp), intent(in)             :: scicomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Currently all this method does is to check that the {\tt scicomp}
! was created.
!
! The arguments are:
! \begin{description}
! \item[scicomp]
!   {\tt ESMF\_SciComp} to validate.
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

    ESMF_INIT_CHECK_DEEP(ESMF_SciCompGetInit,scicomp,rc)

    ! call Comp method
    call ESMF_CompValidate(scicomp%compp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_SciCompValidate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SciCompWait"
!BOP
! !IROUTINE: ESMF_SciCompWait - Wait for a SciComp to return
!
! !INTERFACE:
  subroutine ESMF_SciCompWait(scicomp, keywordEnforcer, syncflag, &
    timeout, timeoutFlag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp),  intent(inout)          :: scicomp
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
! When executing asychronously, wait for an {\tt ESMF\_SciComp} to return.
!
! The arguments are:
! \begin{description}
! \item[scicomp] 
!   {\tt ESMF\_SciComp} to wait for.
! \item[{[syncflag]}]
!   Blocking behavior of this method call. See section \ref{const:sync} 
!   for a list of valid blocking options. Default option is
!   {\tt ESMF\_SYNC\_VASBLOCKING} which blocks PETs and their spawned off threads 
!   across each VAS but does not synchronize PETs that run in different VASs.
! \item[{[timeout]}]
!   The maximum period in seconds the actual component is allowed to execute
!   a previously envoked component method before it must communicate back to
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

    ESMF_INIT_CHECK_DEEP(ESMF_SciCompGetInit,scicomp,rc)

    ! check consistency between timeout argument and component argument
    if (present(timeout).and. &
      .not.ESMF_CompIsDualConnected(scicomp%compp, rc=localrc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="'timeout' argument is only allowed for connected dual components",&
        ESMF_CONTEXT, rcTOReturn=rc)
      return
    endif

    ! call Comp method
    call ESMF_CompWait(scicomp%compp, syncflag=syncflag, timeout=timeout, &
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
      ESMF_CONTEXT, rcTOReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_SciCompWait
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SciCompGetInit"
!BOPI
! !IROUTINE:  ESMF_SciCompGetInit - Get initialization status.

! !INTERFACE:
  function ESMF_SciCompGetInit(d)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_SciCompGetInit
!
! !ARGUMENTS:
    type(ESMF_SciComp), intent(in), optional :: d
!
! !DESCRIPTION:
! Get the initialization status of the Deep class {\tt SciComp}.
!
! The arguments are:
! \begin{description}
! \item[d]
!   {\tt ESMF\_SciComp} from which to retreive status.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(d)) then
      ESMF_SciCompGetInit = ESMF_INIT_GET(d)
    else
      ESMF_SciCompGetInit = ESMF_INIT_CREATED
    endif
  end function ESMF_SciCompGetInit
!------------------------------------------------------------------------------

end module ESMF_SciCompMod
