! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
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
  public ESMF_SciCompIsCreated
  public ESMF_SciCompPrint
  public ESMF_SciCompSet
  public ESMF_SciCompValidate
  
! - ESMF-internal methods:
  public ESMF_SciCompGetInit

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

!===============================================================================
! SciCompOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_SciCompAssignment(=) - SciComp assignment
!
! !INTERFACE:
!   interface assignment(=)
!   scicomp1 = scicomp2
!
! !ARGUMENTS:
!   type(ESMF_SciComp) :: scicomp1
!   type(ESMF_SciComp) :: scicomp2
!
! !DESCRIPTION:
!   Assign scicomp1 as an alias to the same ESMF SciComp object in memory
!   as scicomp2. If scicomp2 is invalid, then scicomp1 will be equally 
!   invalid after the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[scicomp1]
!     The {\tt ESMF\_SciComp} object on the left hand side of the assignment.
!   \item[scicomp2]
!     The {\tt ESMF\_SciComp} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


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
! !DESCRIPTION:
! This interface creates an {\tt ESMF\_SciComp} object. 
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
  subroutine ESMF_SciCompDestroy(scicomp, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp), intent(inout)           :: scicomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out),  optional :: rc
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

    ! call Comp method
    call ESMF_CompDestruct(scicomp%compp, rc=localrc)
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
  subroutine ESMF_SciCompGet(scicomp, keywordEnforcer, name, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp),       intent(in)            :: scicomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),         intent(out), optional :: name
    integer,                  intent(out), optional :: rc
!
! !DESCRIPTION:
! Get information about an {\tt ESMF\_SciComp} object.
!  
! The arguments are:
! \begin{description}
! \item[scicomp]
!   The {\tt ESMF\_SciComp} object being queried.
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

    ESMF_INIT_CHECK_DEEP(ESMF_SciCompGetInit,scicomp,localrc)

    ! call Comp method
    call ESMF_CompGet(scicomp%compp, name=name, compStatus=compStatus, &
       rc=localrc)
print *, "Name: ", name
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=localrc)) return

    ! call Comp method
    call ESMF_CompStatusGet(compStatus, rc = localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=localrc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_SciCompGet
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SciCompIsCreated()"
!BOP
! !IROUTINE: ESMF_SciCompIsCreated - Check whether a SciComp object has been created

! !INTERFACE:
  function ESMF_SciCompIsCreated(scicomp, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_SciCompIsCreated
!
! !ARGUMENTS:
    type(ESMF_SciComp), intent(in)            :: scicomp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt scicomp} has been created. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[scicomp]
!     {\tt ESMF\_SciComp} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ESMF_SciCompIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_SciCompGetInit(scicomp)==ESMF_INIT_CREATED) &
      ESMF_SciCompIsCreated = .true.
  end function
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
!   {\tt ESMF\_SciComp} from which to retrieve status.
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
